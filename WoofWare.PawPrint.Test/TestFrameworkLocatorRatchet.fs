namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Text.RegularExpressions
open FsUnitTyped
open NUnit.Framework

/// A test that finds its own framework to run a guest on escapes `FrameworkUnderTest`: it runs on
/// whatever that finds, whichever runtime was selected, and no check notices. A test that finds the
/// host's CoreLib file for itself hides that it reads the host's image rather than the selected
/// runtime's. So the members that locate a framework may be named only in `FrameworkUnderTest.fs`,
/// the members that locate an assembly's file only in `HostImage.fs`, and either elsewhere only
/// where an allowance below says why the use there is not doing the owning module's job.
///
/// The scan is over the text of every file the test project compiles, embedded into the test
/// assembly by the project file, comments included. It sees names, not values: it cannot see a
/// path spelled as a literal, or a member reached through reflection by a string.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestFrameworkLocatorRatchet =

    /// A member that locates something on disk, and the one file whose job that is.
    type private Locator =
        {
            /// How a violation names the member.
            Name : string
            Pattern : Regex
            Owner : string
            /// What a violation tells its author to do instead.
            Remedy : string
        }

    let private frameworkLocator (name : string) : Locator =
        {
            Name = name
            Pattern = Regex (Regex.Escape name)
            Owner = "FrameworkUnderTest.fs"
            Remedy = "run guests along FrameworkUnderTest.runtimeDirs ()"
        }

    /// A property that reports an assembly's or a module's file. Any assembly's, since the ratchet
    /// cannot tell which assembly an expression names.
    let private fileLocator (name : string) : Locator =
        {
            Name = name
            Pattern = Regex (@"\." + name + @"\b")
            Owner = "HostImage.fs"
            Remedy =
                "read the test host's CoreLib through HostImage, or the selected runtime's through FrameworkUnderTest.coreLibPath ()"
        }

    let private locators : Locator list =
        [
            frameworkLocator "SelectForDll"
            frameworkLocator "GetRuntimeDirectory"
            fileLocator "Location"
            fileLocator "FullyQualifiedName"
        ]

    type private Allowance =
        {
            File : string
            Locator : string
            /// Exactly how many times `File` names `Locator`, so a second use there is not waved
            /// through by the first one's reason.
            Count : int
            Reason : string
        }

    let private allowances : Allowance list =
        [
            {
                File = "Roslyn.fs"
                Locator = "GetRuntimeDirectory"
                Count = 1
                Reason =
                    "guests compile against the test host's framework, the oldest supported runtime, so that one image runs on every runtime under test"
            }
            {
                File = "TestInterfaceDispatchMap.fs"
                Locator = "GetRuntimeDirectory"
                Count = 1
                Reason =
                    "compiles its corpus against the test host's framework, as Roslyn.fs does, and never runs it on a framework"
            }
            {
                File = "FrameworkUnderTest.fs"
                Locator = "Location"
                Count = 1
                Reason = "locates the test assembly, whose runtimeconfig names the test host's framework"
            }
            {
                File = "TestAssemblyProbe.fs"
                Locator = "Location"
                Count = 1
                Reason = "locates WoofWare.PawPrint.Logging, the small image the probe loads"
            }
            {
                File = "TestDispatchesVirtually.fs"
                Locator = "Location"
                Count = 1
                Reason = "locates FSharp.Core, the corpus it sweeps"
            }
            {
                File = "TestFieldIdAgreement.fs"
                Locator = "Location"
                Count = 1
                Reason = "locates FSharp.Core, which the corpus references"
            }
            {
                File = "TestFieldReferenceResolution.fs"
                Locator = "Location"
                Count = 2
                Reason =
                    "reads the framework assembly host reflection loaded, after checking it is the framework under test's, since host reflection is the oracle"
            }
            {
                File = "TestFSharpPureCases.fs"
                Locator = "Location"
                Count = 1
                Reason = "locates the test assembly's directory, which holds the F# guests"
            }
            {
                File = "TestImpureCases.fs"
                Locator = "Location"
                Count = 1
                Reason = "a comment on what a guest's Assembly.Location reports"
            }
            {
                File = "TestInterfaceDispatchMap.fs"
                Locator = "Location"
                Count = 1
                Reason = "a Roslyn diagnostic's source location, not an assembly's"
            }
            {
                File = "TestMethodReferenceResolution.fs"
                Locator = "Location"
                Count = 2
                Reason =
                    "reads the framework assembly host reflection loaded, after checking it is the framework under test's, since host reflection is the oracle"
            }
            {
                File = "TestNestedClassifier.fs"
                Locator = "Location"
                Count = 1
                Reason = "locates the test assembly, whose nesting it compares with host reflection"
            }
            {
                File = "TestNoMissedInterpolation.fs"
                Locator = "Location"
                Count = 2
                Reason = "locates WoofWare.PawPrint and WoofWare.PawPrint.Domain, whose IL it scans"
            }
            {
                File = "TestPublishedXmlDocumentation.fs"
                Locator = "Location"
                Count = 1
                Reason =
                    "locates the test assembly's directory, beside which MSBuild copies each referenced project's documentation file"
            }
            {
                File = "TestSequencePoints.fs"
                Locator = "Location"
                Count = 1
                Reason = "locates WoofWare.PawPrint.Domain, whose sequence points it reads"
            }
            {
                File = "TestStackShape.fs"
                Locator = "Location"
                Count = 2
                Reason = "locates the test assembly, whose method bodies it checks"
            }
            {
                File = "TestTypeDefnRendering.fs"
                Locator = "Location"
                Count = 1
                Reason = "locates the test assembly, whose types it renders"
            }
        ]

    /// Every way `sources` (file name, text) departs from the rule: a file other than a locator's
    /// owner (or this one) naming the locator other than exactly as often as an allowance says, and
    /// an allowance for a file that is not among `sources`.
    let private violations (allowances : Allowance list) (sources : (string * string) list) : string list =
        let allowed =
            allowances
            |> List.map (fun allowance -> (allowance.File, allowance.Locator), allowance.Count)
            |> Map.ofList

        let files = sources |> List.map fst |> Set.ofList

        let unknownAllowances =
            allowances
            |> List.filter (fun allowance -> not (files.Contains allowance.File))
            |> List.map (fun allowance ->
                $"an allowance names %s{allowance.File}, which the test project does not compile"
            )

        let miscounts =
            [
                for file, text in sources do
                    for locator in locators do
                        if file <> locator.Owner && file <> "TestFrameworkLocatorRatchet.fs" then
                            let found = locator.Pattern.Matches(text).Count

                            let expected = allowed |> Map.tryFind (file, locator.Name) |> Option.defaultValue 0

                            if found <> expected then
                                yield
                                    $"%s{file} names %s{locator.Name} %d{found} time(s), where %d{expected} are allowed: %s{locator.Remedy}, or give the use an allowance saying why it is not doing %s{locator.Owner}'s job"
            ]

        unknownAllowances @ miscounts

    /// The prefix the project file gives each embedded source's resource name.
    [<Literal>]
    let private ResourcePrefix = "PawPrintTestSource/"

    let private compiledSources () : (string * string) list =
        let assembly = typeof<Allowance>.Assembly

        [
            for resource in assembly.GetManifestResourceNames () do
                if resource.StartsWith (ResourcePrefix, StringComparison.Ordinal) then
                    use stream = assembly.GetManifestResourceStream resource
                    use reader = new StreamReader (stream)
                    yield resource.Substring ResourcePrefix.Length, reader.ReadToEnd ()
        ]

    [<Test>]
    let ``only FrameworkUnderTest locates a framework, and only HostImage an assembly's file`` () : unit =
        let sources = compiledSources ()

        // Guards against the embedding matching nothing, which would pass vacuously.
        sources.Length |> shouldBeGreaterThan 200

        let names = sources |> List.map fst |> Set.ofList

        for locator in locators do
            names.Contains locator.Owner |> shouldEqual true

        violations allowances sources |> shouldBeEmpty

    /// A copy of the real sources with `planted` appended to `file`.
    let private plantedIn (file : string) (planted : string) : (string * string) list =
        let sources = compiledSources ()
        sources |> List.map fst |> shouldContain file

        sources
        |> List.map (fun (name, text) ->
            if name = file then
                name, text + "\n" + planted
            else
                name, text
        )

    /// The locators `violations` reports in `file`, one name per violation.
    let private reportedIn (file : string) (violations : string list) : string list =
        let prefix = file + " names "

        violations
        |> List.map (fun violation ->
            if not (violation.StartsWith (prefix, StringComparison.Ordinal)) then
                failwith $"expected a violation in %s{file}, got: %s{violation}"

            violation.Substring(prefix.Length).Split(' ').[0]
        )

    [<TestCase("DotnetRuntime.SelectForDll assy.Location |> ImmutableArray.CreateRange", "SelectForDll Location")>]
    [<TestCase("yield! SelectForDll (typeof<RunResult>.Assembly.Location)", "SelectForDll Location")>]
    [<TestCase("WoofWare.DotnetRuntimeLocator.DotnetRuntime.SelectForDll", "SelectForDll")>]
    [<TestCase("Runtime.InteropServices.RuntimeEnvironment.GetRuntimeDirectory ()", "GetRuntimeDirectory")>]
    [<TestCase("let f = RuntimeEnvironment.GetRuntimeDirectory", "GetRuntimeDirectory")>]
    [<TestCase("// the dirs come from SelectForDll", "SelectForDll")>]
    [<TestCase("let p = typeof<obj>.Assembly.Location", "Location")>]
    [<TestCase("File.ReadAllBytes typeof<int>.Assembly.Location", "Location")>]
    [<TestCase("Path.GetDirectoryName typeof<System.Object>.Assembly.Location", "Location")>]
    [<TestCase("(typeof<obj>.Assembly).Location", "Location")>]
    [<TestCase("let corelib = typeof<obj>.Assembly in corelib.Location", "Location")>]
    [<TestCase("Assembly.GetAssembly(typeof<obj>).Location", "Location")>]
    [<TestCase("typeof<obj>.Module.FullyQualifiedName", "FullyQualifiedName")>]
    [<TestCase("// PawPrint reads typeof<obj>.Assembly.Location", "Location")>]
    let ``a planted locator is caught`` (planted : string, expected : string) : unit =
        violations allowances (plantedIn "TestPureCases.fs" planted)
        |> reportedIn "TestPureCases.fs"
        |> shouldEqual (expected.Split ' ' |> List.ofArray)

    [<Test>]
    let ``a name that only contains a locator is not caught`` () : unit =
        let planted =
            "let a = StorageLocation.Heap\nlet b = x.Locations\nlet c = y.LocationOf\nlet d = HostImage.coreLibPath"

        violations allowances (plantedIn "TestPureCases.fs" planted) |> shouldBeEmpty

    [<Test>]
    let ``each owner may name only its own locators`` () : unit =
        violations allowances (plantedIn "HostImage.fs" "SelectForDll ()")
        |> reportedIn "HostImage.fs"
        |> shouldEqual [ "SelectForDll" ]

        violations allowances (plantedIn "FrameworkUnderTest.fs" "typeof<obj>.Assembly.Location")
        |> List.exactlyOne
        |> shouldContainText "FrameworkUnderTest.fs names Location 2 time(s), where 1 are allowed"

        violations allowances (plantedIn "HostImage.fs" "typeof<int>.Assembly.Location")
        |> shouldBeEmpty

    [<Test>]
    let ``a second locator in an allowed file is caught`` () : unit =
        violations allowances (plantedIn "Roslyn.fs" "RuntimeEnvironment.GetRuntimeDirectory ()")
        |> List.exactlyOne
        |> shouldContainText "Roslyn.fs names GetRuntimeDirectory 2 time(s), where 1 are allowed"

        violations allowances (plantedIn "TestStackShape.fs" "typeof<obj>.Assembly.Location")
        |> List.exactlyOne
        |> shouldContainText "TestStackShape.fs names Location 3 time(s), where 2 are allowed"

    [<Test>]
    let ``an allowance that no longer matches is caught`` () : unit =
        let sources =
            compiledSources ()
            |> List.map (fun (name, text) ->
                if name = "Roslyn.fs" then
                    name, text.Replace ("GetRuntimeDirectory", "SomewhereElse")
                else
                    name, text
            )

        violations allowances sources
        |> List.exactlyOne
        |> shouldContainText "Roslyn.fs names GetRuntimeDirectory 0 time(s), where 1 are allowed"

    [<Test>]
    let ``an allowance for a file that is not compiled is caught`` () : unit =
        let stale =
            {
                File = "NoSuchFile.fs"
                Locator = "SelectForDll"
                Count = 1
                Reason = "planted"
            }

        violations (stale :: allowances) (compiledSources ())
        |> List.exactlyOne
        |> shouldContainText "NoSuchFile.fs, which the test project does not compile"

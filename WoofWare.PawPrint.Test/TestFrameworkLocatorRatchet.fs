namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Text.RegularExpressions
open FsUnitTyped
open NUnit.Framework

/// A test that finds its own framework to run a guest on escapes `FrameworkUnderTest`: it runs on
/// whatever that finds, whichever runtime was selected, and no check notices. So the members that
/// locate a framework may be named only in `FrameworkUnderTest.fs`, or where an allowance below
/// says why a use there is not choosing the framework under test.
///
/// The scan is over the text of every file the test project compiles, embedded into the test
/// assembly by the project file, comments included.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestFrameworkLocatorRatchet =

    /// Names of members that locate a .NET framework on disk.
    let private locators : string list = [ "SelectForDll" ; "GetRuntimeDirectory" ]

    /// Files that may name the locators freely: the module whose job they are, and this file.
    let private exempt : Set<string> =
        Set.ofList [ "FrameworkUnderTest.fs" ; "TestFrameworkLocatorRatchet.fs" ]

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
        ]

    /// Every way `sources` (file name, text) departs from the rule: an unexempted file naming a
    /// locator other than exactly as often as an allowance says, and an allowance for a file that
    /// is not among `sources`.
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
                    if not (exempt.Contains file) then
                        for locator in locators do
                            let found = Regex.Matches(text, Regex.Escape locator).Count

                            let expected = allowed |> Map.tryFind (file, locator) |> Option.defaultValue 0

                            if found <> expected then
                                yield
                                    $"%s{file} names %s{locator} %d{found} time(s), where %d{expected} are allowed: run guests along FrameworkUnderTest.runtimeDirs (), or give the use an allowance saying why it is not choosing the framework under test"
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
    let ``only FrameworkUnderTest locates a framework`` () : unit =
        let sources = compiledSources ()

        // Guards against the embedding matching nothing, which would pass vacuously.
        sources.Length |> shouldBeGreaterThan 200

        let names = sources |> List.map fst |> Set.ofList

        for file in exempt do
            names.Contains file |> shouldEqual true

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

    [<TestCase "DotnetRuntime.SelectForDll assy.Location |> ImmutableArray.CreateRange">]
    [<TestCase "yield! SelectForDll (typeof<RunResult>.Assembly.Location)">]
    [<TestCase "WoofWare.DotnetRuntimeLocator.DotnetRuntime.SelectForDll">]
    [<TestCase "Runtime.InteropServices.RuntimeEnvironment.GetRuntimeDirectory ()">]
    [<TestCase "let f = RuntimeEnvironment.GetRuntimeDirectory">]
    [<TestCase "// the dirs come from SelectForDll">]
    let ``a planted locator is caught`` (planted : string) : unit =
        violations allowances (plantedIn "TestPureCases.fs" planted)
        |> List.exactlyOne
        |> shouldContainText "TestPureCases.fs names"

    [<Test>]
    let ``a second locator in an allowed file is caught`` () : unit =
        violations allowances (plantedIn "Roslyn.fs" "RuntimeEnvironment.GetRuntimeDirectory ()")
        |> List.exactlyOne
        |> shouldContainText "Roslyn.fs names GetRuntimeDirectory 2 time(s), where 1 are allowed"

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

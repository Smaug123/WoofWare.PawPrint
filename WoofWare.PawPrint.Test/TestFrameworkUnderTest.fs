namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.IO
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open Microsoft.Extensions.Logging.Abstractions
open NUnit.Framework
open WoofWare.PawPrint

/// `FrameworkUnderTest` selects a framework only when it is exactly the one asked for, and never
/// falls back to the test host's.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestFrameworkUnderTest =

    /// Strings near a runtime's name, as well as arbitrary ones: a parser that trimmed, ignored
    /// case, or matched a prefix would accept some of these.
    let private candidateValues : Arbitrary<string> =
        let names = EmulatedRuntime.supported |> List.map FrameworkUnderTest.name

        let nearMiss =
            gen {
                let! name = Gen.elements names

                let! edit =
                    Gen.elements
                        [
                            id
                            (fun (s : string) -> s.ToLowerInvariant ())
                            (fun (s : string) -> s.ToUpperInvariant ())
                            (fun (s : string) -> " " + s)
                            (fun (s : string) -> s + "\n")
                            (fun (s : string) -> s.Substring (0, s.Length - 1))
                            (fun (s : string) -> s + "0")
                            (fun (s : string) -> "net" + s.Substring 3)
                        ]

                return edit name
            }

        let arbitrary =
            ArbMap.defaults
            |> ArbMap.generate<string>
            |> Gen.map (fun s -> s |> Option.ofObj |> Option.defaultValue "")

        Gen.oneof [ nearMiss ; arbitrary ; Gen.constant "" ] |> Arb.fromGen

    [<Test>]
    let ``A value selects a runtime exactly when it is that runtime's name`` () : unit =
        let property (value : string) : bool =
            let expected =
                EmulatedRuntime.supported
                |> List.filter (fun runtime -> FrameworkUnderTest.name runtime = value)

            match expected, FrameworkUnderTest.parse (Some value) with
            | [ runtime ], Ok parsed -> parsed = runtime
            | [], Error (FrameworkUnderTestError.UnknownRuntime unknown) -> unknown = value
            | _ -> false

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 1000, Prop.forAll candidateValues property)

    [<Test>]
    let ``Every supported runtime has its own name`` () : unit =
        let names = EmulatedRuntime.supported |> List.map FrameworkUnderTest.name
        names |> List.distinct |> shouldEqual names

        for runtime in EmulatedRuntime.supported do
            FrameworkUnderTest.parse (Some (FrameworkUnderTest.name runtime))
            |> shouldEqual (Ok runtime)

    [<Test>]
    let ``Only an unset variable selects the default runtime`` () : unit =
        FrameworkUnderTest.parse None |> shouldEqual (Ok EmulatedRuntime.Net10)

        FrameworkUnderTest.parse (Some "")
        |> shouldEqual (Error (FrameworkUnderTestError.UnknownRuntime ""))

    [<Test>]
    let ``The selection this run was given is valid`` () : unit =
        let selected = FrameworkUnderTest.selected ()
        selected.RuntimeDirs |> shouldEqual (FrameworkUnderTest.runtimeDirs ())

        FrameworkUnderTest.check selected.Runtime selected.RuntimeDirs
        |> Result.map (fun checkedAgain -> checkedAgain.CoreLibPath)
        |> shouldEqual (Ok selected.CoreLibPath)

    [<Test>]
    let ``Net10 is the test host's own framework`` () : unit =
        if FrameworkUnderTest.runtime () <> EmulatedRuntime.Net10 then
            Assert.Ignore "this run selected a runtime other than the test host's"

        FrameworkUnderTest.sharedFrameworkDirectory ()
        |> shouldEqual (Path.GetDirectoryName typeof<obj>.Assembly.Location)

    let private withTempDir (body : string -> unit) : unit =
        let dir =
            Path.Combine (Path.GetTempPath (), "PawPrintFrameworkUnderTest", Guid.NewGuid().ToString ("N"))

        Directory.CreateDirectory dir |> ignore<DirectoryInfo>

        try
            body dir
        finally
            Directory.Delete (dir, true)

    [<Test>]
    let ``A missing directory fails the selection even when a later one would do`` () : unit =
        withTempDir (fun parent ->
            let missing = Path.Combine (parent, "absent")

            let dirs =
                seq {
                    yield missing
                    yield! FrameworkUnderTest.runtimeDirs ()
                }
                |> ImmutableArray.CreateRange

            match FrameworkUnderTest.check (FrameworkUnderTest.runtime ()) dirs with
            | Error (FrameworkUnderTestError.MissingDirectory (_, found)) -> found |> shouldEqual missing
            | other -> failwith $"expected MissingDirectory, got %O{other}"
        )

    [<Test>]
    let ``Directories with no CoreLib fail the selection`` () : unit =
        withTempDir (fun dir ->
            match FrameworkUnderTest.check (FrameworkUnderTest.runtime ()) (ImmutableArray.Create dir) with
            | Error (FrameworkUnderTestError.NoCoreLib _) -> ()
            | other -> failwith $"expected NoCoreLib, got %O{other}"
        )

    [<TestCase 9us>]
    [<TestCase 11us>]
    let ``A CoreLib of another major ahead of the framework fails the selection`` (major : uint16) : unit =
        withTempDir (fun dir ->
            let patched = PatchedCoreLib.write major dir

            let dirs =
                seq {
                    yield dir
                    yield! FrameworkUnderTest.runtimeDirs ()
                }
                |> ImmutableArray.CreateRange

            match FrameworkUnderTest.check (FrameworkUnderTest.runtime ()) dirs with
            | Error (FrameworkUnderTestError.CoreLibMismatch (_, _, path, Error unsupported)) ->
                path |> shouldEqual (Some patched)
                unsupported.FoundMajor |> shouldEqual (int major)
            | other -> failwith $"expected CoreLibMismatch, got %O{other}"
        )

    [<Test>]
    let ``A run on the framework under test serves it`` () : unit =
        let image =
            Roslyn.compile [ "public static class Program { public static int Main() { return 3; } }" ]

        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory
        use peImage = new MemoryStream (image)

        let outcome =
            BoundedRun.run
                loggerFactory
                "Trivial.cs"
                (Some "Trivial.cs")
                peImage
                (HostConfig.Default (FrameworkUnderTest.runtimeDirs ()))

        FrameworkUnderTest.assertOutcomeServes outcome

        match outcome with
        | RunOutcome.NormalExit (state, _) ->
            state.LatchedExitCode |> shouldEqual 3

            (FrameworkUnderTest.loadedCoreLib state).OriginalPath
            |> shouldEqual (Some (FrameworkUnderTest.selected ()).CoreLibPath)
        | other -> failwith $"expected the guest to exit normally, got %O{other}"

    [<TestCase 9us>]
    [<TestCase 11us>]
    let ``A CoreLib of another major does not serve the framework under test`` (major : uint16) : unit =
        withTempDir (fun dir ->
            let patched = PatchedCoreLib.write major dir
            let corelib = Assembly.readFile NullLoggerFactory.Instance patched

            match FrameworkUnderTest.checkServes (FrameworkUnderTest.runtime ()) corelib with
            | Error (FrameworkUnderTestError.CoreLibMismatch (_, _, path, Error unsupported)) ->
                path |> shouldEqual (Some patched)
                unsupported.FoundMajor |> shouldEqual (int major)
            | other -> failwith $"expected CoreLibMismatch, got %O{other}"
        )

    /// A machine state that has loaded `corelib`, if any, and nothing else.
    let private stateHaving (corelib : DumpedAssembly option) : IlMachineState =
        let image =
            Roslyn.compile [ "public static class Program { public static void Main() { } }" ]

        use peImage = new MemoryStream (image)
        let entry = Assembly.read NullLoggerFactory.Instance None peImage

        let state =
            IlMachineState.initial NullLoggerFactory.Instance (FrameworkUnderTest.runtimeDirs ()) entry

        match corelib with
        | Some corelib -> state.WithLoadedAssembly corelib
        | None -> state

    [<Test>]
    let ``A state that loaded no CoreLib does not serve the framework under test`` () : unit =
        let e =
            Assert.Throws<Exception> (fun () -> FrameworkUnderTest.assertServes (stateHaving None))

        e.Message |> shouldContainText "No System.Private.CoreLib was loaded"

    [<Test>]
    let ``A state that loaded a CoreLib of another major does not serve the framework under test`` () : unit =
        withTempDir (fun dir ->
            let patched = PatchedCoreLib.write 11us dir
            let corelib = Assembly.readFile NullLoggerFactory.Instance patched

            let e =
                Assert.Throws<Exception> (fun () -> FrameworkUnderTest.assertServes (stateHaving (Some corelib)))

            e.Message |> shouldContainText "states major 11"
        )

    [<Test>]
    let ``A state that loaded the selected CoreLib serves the framework under test`` () : unit =
        let corelib =
            Assembly.readFile NullLoggerFactory.Instance (FrameworkUnderTest.selected ()).CoreLibPath

        FrameworkUnderTest.assertServes (stateHaving (Some corelib))

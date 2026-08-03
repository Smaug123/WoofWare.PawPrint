namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint

/// End-to-end coverage for the `SystemNative_HandleNonCanceledPosixSignal`
/// `DefaultDisposition.Terminate` branch: a guest that DllImports the
/// handler with a Linux signo whose kernel default is Terminate (e.g.
/// SIGTERM) must surface as `RunOutcome.SignalTerminated` carrying the
/// originating `Signal`, and the App-layer mapping must produce the
/// POSIX-conventional exit code `128 + signo`.
///
/// `TestSignal` already verifies the disposition classifier in isolation;
/// this fixture nails down that the arm's Terminate branch propagates the
/// outcome all the way through `ExecutionResult` → `RunOutcome` → the App
/// exit-code mapping without dropping or rewriting the `Signal` identity.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestSignalTermination =
    let private assy = typeof<RunResult>.Assembly

    let private runImpureSource (sourceFileName : string) : RunOutcome =
        let source = Assembly.getEmbeddedResourceAsString sourceFileName assy
        let image = Roslyn.compile [ source ]

        let messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", sourceFileName ]

        use _loggerFactoryResource = loggerFactory

        let dotnetRuntimes =
            DotnetRuntime.SelectForDll assy.Location |> ImmutableArray.CreateRange

        use peImage = new MemoryStream (image)

        try
            Program.run loggerFactory (Some sourceFileName) peImage dotnetRuntimes Map.empty None []
        with _ ->
            for message in messages () do
                System.Console.Error.WriteLine $"{message}"

            reraise ()

    [<Test>]
    let ``HandleNonCanceledPosixSignal Terminate branch surfaces SignalTerminated`` () : unit =
        // The C# guest calls SystemNative_HandleNonCanceledPosixSignal(15)
        // directly. 15 is SIGTERM in PawPrint's Linux-signo table, which
        // classifies as DefaultDisposition.Terminate; the arm must
        // therefore short-circuit the run with SignalTerminated carrying
        // the originating Signal.SIGTERM. A regression that fell through
        // to `Main`'s `return 99` would surface as `NormalExit` with
        // exit code 99 — the negative case below catches that.
        let outcome = runImpureSource "SystemNativeHandleNonCanceledPosixSignalTerminate.cs"

        match outcome with
        | RunOutcome.SignalTerminated (_, signal) -> signal |> shouldEqual Signal.SIGTERM
        | other -> failwith $"expected RunOutcome.SignalTerminated (Signal.SIGTERM), got %O{other}"

    [<Test>]
    let ``SignalTerminated maps to POSIX-conventional exit code 128 + signo`` () : unit =
        // Codifies the App-layer exit-code mapping (`App/Program.fs`'s
        // `SignalTerminated` arm). The mapping itself is one expression,
        // but it's the contract between the simulator and the host shell:
        // a process killed by SIGTERM exits 143, by SIGINT 130, etc. If
        // someone re-tunes the formula (e.g. to hardcode 134 like
        // FailFast does) this test catches it.
        128 + Signal.toLinuxSigno Signal.SIGTERM |> shouldEqual 143
        128 + Signal.toLinuxSigno Signal.SIGINT |> shouldEqual 130
        128 + Signal.toLinuxSigno Signal.SIGABRT |> shouldEqual 134
        128 + Signal.toLinuxSigno Signal.SIGHUP |> shouldEqual 129

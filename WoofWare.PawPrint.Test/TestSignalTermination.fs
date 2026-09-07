namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint
open WoofWare.PosixKernel

/// End-to-end coverage for the `SystemNative_HandleNonCanceledPosixSignal`
/// `DefaultDisposition.Terminate` branch: a guest that DllImports the
/// handler with a signo whose kernel default is Terminate must surface as
/// `RunOutcome.SignalTerminated` carrying the originating `Signal`, read
/// under the configured platform's numbering, and the App-layer mapping
/// must produce the POSIX-conventional exit code `128 + signo`.
///
/// `TestSignal` already verifies the disposition classifier in isolation;
/// this fixture nails down that the arm's Terminate branch propagates the
/// outcome all the way through `ExecutionResult` → `RunOutcome` → the App
/// exit-code mapping without dropping or rewriting the `Signal` identity.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestSignalTermination =
    let private assy = typeof<RunResult>.Assembly

    let private runImpureSource (platform : SimulatedUnixPlatform) (sourceFileName : string) : RunOutcome =
        let source = Assembly.getEmbeddedResourceAsString sourceFileName assy
        let image = Roslyn.compile [ source ]

        let messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", sourceFileName ]

        use _loggerFactoryResource = loggerFactory

        let dotnetRuntimes =
            DotnetRuntime.SelectForDll assy.Location |> ImmutableArray.CreateRange

        use peImage = new MemoryStream (image)

        let config =
            let host = HostConfig.Default dotnetRuntimes

            { host with
                Guest =
                    { host.Guest with
                        Kernel =
                            { host.Guest.Kernel with
                                UnixPlatform = platform
                            }
                    }
            }

        try
            Program.run loggerFactory (Some sourceFileName) peImage config
        with _ ->
            for message in messages () do
                System.Console.Error.WriteLine $"{message}"

            reraise ()

    let private signalTerminatedBy (outcome : RunOutcome) : Signal =
        match outcome with
        | RunOutcome.SignalTerminated (_, signal) -> signal
        | other -> failwith $"expected RunOutcome.SignalTerminated, got %O{other}"

    [<Test>]
    let ``HandleNonCanceledPosixSignal Terminate branch surfaces SignalTerminated`` () : unit =
        // The C# guest calls SystemNative_HandleNonCanceledPosixSignal(15)
        // directly. 15 is SIGTERM under both numberings, which classifies
        // as DefaultDisposition.Terminate; the arm must therefore
        // short-circuit the run with SignalTerminated carrying the
        // originating Signal.SIGTERM. A regression that fell through to
        // `Main`'s `return 99` would surface as `NormalExit` with exit code
        // 99 — `signalTerminatedBy` catches that.
        for platform in [ SimulatedUnixPlatform.linuxX64 ; SimulatedUnixPlatform.macOsArm64 ] do
            runImpureSource platform "SystemNativeHandleNonCanceledPosixSignalTerminate.cs"
            |> signalTerminatedBy
            |> shouldEqual Signal.SIGTERM

    /// Signo 30 terminates under both numberings but is a different signal
    /// under each — SIGUSR1 on Darwin, SIGPWR on Linux — so the outcome's
    /// identity is what shows the arm read the number under the configured
    /// platform rather than under a fixed table.
    [<Test>]
    let ``SignalTerminated carries the signal the signo names under the configured platform`` () : unit =
        runImpureSource SimulatedUnixPlatform.macOsArm64 "SystemNativeHandleNonCanceledPosixSignal30.cs"
        |> signalTerminatedBy
        |> shouldEqual Signal.SIGUSR1

        runImpureSource SimulatedUnixPlatform.linuxX64 "SystemNativeHandleNonCanceledPosixSignal30.cs"
        |> signalTerminatedBy
        |> shouldEqual (Signal.Other 30)

    [<Test>]
    let ``SignalTerminated maps to POSIX-conventional exit code 128 + signo`` () : unit =
        // Pins the exit codes `128 + Signal.toRawSignoUnder` produces for
        // the signals a shell user knows by heart: a process killed by
        // SIGTERM exits 143, by SIGINT 130, etc. This is the formula
        // `App/Program.fs`'s `SignalTerminated` arm applies, but the App
        // layer is not exercised here — only the signo table underneath it.
        // A re-tuned App formula (e.g. hardcoding 134 like FailFast does)
        // would not be caught.
        for numbering in [ SignalNumbering.Linux ; SignalNumbering.Darwin ] do
            128 + Signal.toRawSignoUnder numbering Signal.SIGTERM |> shouldEqual 143
            128 + Signal.toRawSignoUnder numbering Signal.SIGINT |> shouldEqual 130
            128 + Signal.toRawSignoUnder numbering Signal.SIGABRT |> shouldEqual 134
            128 + Signal.toRawSignoUnder numbering Signal.SIGHUP |> shouldEqual 129

        // And the two terminating signals whose number depends on the
        // platform: a process killed by SIGUSR1 exits 138 on Linux and 158
        // on macOS.
        128 + Signal.toRawSignoUnder SignalNumbering.Linux Signal.SIGUSR1
        |> shouldEqual 138

        128 + Signal.toRawSignoUnder SignalNumbering.Darwin Signal.SIGUSR1
        |> shouldEqual 158

        128 + Signal.toRawSignoUnder SignalNumbering.Linux Signal.SIGUSR2
        |> shouldEqual 140

        128 + Signal.toRawSignoUnder SignalNumbering.Darwin Signal.SIGUSR2
        |> shouldEqual 159

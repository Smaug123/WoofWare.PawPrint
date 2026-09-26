namespace WoofWare.PawPrint.Test

open System.IO
open FsUnitTyped
open NUnit.Framework
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

        let dotnetRuntimes = FrameworkUnderTest.runtimeDirs ()

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

    /// Sends itself `args[0]` with libc's kill(2), or, given a second
    /// argument, hands it to the shim's non-cancelled handling instead.
    let private selfSignalGuest : string =
        """
using System;
using System.Runtime.InteropServices;

class Program
{
    [DllImport("libc", EntryPoint = "kill", SetLastError = true)]
    static extern int Kill(int pid, int sig);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_HandleNonCanceledPosixSignal")]
    static extern void HandleNonCanceled(int signalCode);

    static int Main(string[] args)
    {
        int signo = int.Parse(args[0]);
        if (args.Length > 1) HandleNonCanceled(signo);
        else Kill(Environment.ProcessId, signo);
        return 99;
    }
}
"""

    let private runSelfSignal
        (platform : SimulatedUnixPlatform)
        (coreDumps : CoreDumps)
        (argv : string list)
        : RunOutcome
        =
        let image = Roslyn.compile [ selfSignalGuest ]

        let _messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "case", String.concat " " argv ]

        use _loggerFactoryResource = loggerFactory
        let dotnetRuntimes = FrameworkUnderTest.runtimeDirs ()
        use peImage = new MemoryStream (image)
        let host = HostConfig.Default dotnetRuntimes

        Program.run
            loggerFactory
            None
            peImage
            { host with
                Guest =
                    { host.Guest with
                        Kernel =
                            { host.Guest.Kernel with
                                UnixPlatform = platform
                                CoreDumps = coreDumps
                            }
                        Argv = argv
                    }
            }

    let private signalTerminatedBy (outcome : RunOutcome) : Signal =
        match outcome with
        | RunOutcome.SignalTerminated (_, signal, _) -> signal
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

    [<Test>]
    let ``a death by a signal that dumps core reports the dump exactly when the process writes dumps`` () : unit =
        // SIGQUIT (3 under both numberings) dumps core by default; SIGTERM does
        // not. Both routes to the death, libc's kill(2) and the shim's
        // re-raise, read the process's setting.
        for platform in [ SimulatedUnixPlatform.linuxX64 ; SimulatedUnixPlatform.macOsArm64 ] do
            for route in [ [] ; [ "shim" ] ] do
                for coreDumps in [ CoreDumps.Suppressed ; CoreDumps.Written ] do
                    for signo, signal, dumpsCore in [ 3, Signal.SIGQUIT, true ; 15, Signal.SIGTERM, false ] do
                        match runSelfSignal platform coreDumps (string<int> signo :: route) with
                        | RunOutcome.SignalTerminated (_, killedBy, coreDumped) ->
                            (platform, route, coreDumps, killedBy, coreDumped)
                            |> shouldEqual (
                                platform,
                                route,
                                coreDumps,
                                signal,
                                dumpsCore && coreDumps = CoreDumps.Written
                            )
                        | other -> failwith $"%O{platform} %A{route} signo %d{signo}: expected a death, got %O{other}"

    [<Test>]
    let ``the shim re-raises SIGKILL though sigaction refuses to restore it`` () : unit =
        // The restore fails with EINVAL, unchecked, and kill(2) goes ahead.
        for platform in [ SimulatedUnixPlatform.linuxX64 ; SimulatedUnixPlatform.macOsArm64 ] do
            match runSelfSignal platform CoreDumps.Written [ "9" ; "shim" ] with
            | RunOutcome.SignalTerminated (_, signal, coreDumped) ->
                (signal, coreDumped) |> shouldEqual (Signal.Other 9, false)
            | other -> failwith $"%O{platform}: expected death by SIGKILL, got %O{other}"

    [<Test>]
    let ``the shim's re-raise of Linux's 33 is refused, because glibc's handler would run`` () : unit =
        let exn =
            Assert.Catch<exn> (fun () ->
                runSelfSignal SimulatedUnixPlatform.linuxX64 CoreDumps.Suppressed [ "33" ; "shim" ]
                |> ignore<RunOutcome>
            )

        exn.Message |> shouldContainText "native code PawPrint does not model"

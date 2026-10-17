namespace WoofWare.PawPrint.Test

open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PosixKernel

/// A guest's own libc `raise(3)`, from the main thread and from another, under
/// each flavour: what is answered, and what is refused because a real CoreCLR
/// process would do something the model cannot express. The answers both
/// flavours agree on are compared against the real runtime by the
/// `LibcRaise*` and `PosixSignalRaise*` guests.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
// Runs guests under the interpreter; see AGENTS.md for why that makes it
// `Explicit`, and CI selects it by category.
[<Category("Guest")>]
[<Explicit>]
module TestLibcRaise =

    /// Raises `args[1]` from the main thread (`args[0]` is `main`) or from a
    /// thread of its own (`worker`), with a PosixSignalRegistration for SIGTERM
    /// that cancels it if `args[2]` is `registered`. Exits 42 if `raise`
    /// answered 0, and 50 + errno if it answered -1.
    let private guest : string =
        """
using System;
using System.Runtime.InteropServices;
using System.Threading;

class Program
{
    [DllImport("libc", EntryPoint = "raise", SetLastError = true)]
    static extern int Raise(int sig);

    static int RaiseOnce(int signo)
    {
        if (Raise(signo) == 0) return 42;
        return 50 + Marshal.GetLastPInvokeError();
    }

    static int Main(string[] args)
    {
        int signo = int.Parse(args[1]);
        using var registration =
            args[2] == "registered"
                ? PosixSignalRegistration.Create(PosixSignal.SIGTERM, context => context.Cancel = true)
                : null;

        if (args[0] == "main") return RaiseOnce(signo);

        int result = -1;
        var worker = new Thread(() => result = RaiseOnce(signo));
        worker.Start();
        worker.Join();
        return result;
    }
}
"""

    let private run (platform : SimulatedUnixPlatform) (argv : string list) : RunOutcome =
        let description = $"""raise(%s{String.concat " " argv})"""

        let _messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "case", description ]

        use _loggerFactoryResource = loggerFactory
        let dotnetRuntimes = FrameworkUnderTest.runtimeDirs ()
        use peImage = new MemoryStream (Roslyn.compile [ guest ])

        BoundedRun.run
            loggerFactory
            description
            None
            peImage
            { HostConfig.Default dotnetRuntimes with
                Guest =
                    { GuestConfig.Default dotnetRuntimes with
                        Kernel =
                            { KernelConfig.Default with
                                UnixPlatform = platform
                            }
                        Argv = argv
                    }
            }
        |> ExpectRun.ended

    let private everyPlatform : SimulatedUnixPlatform list =
        [
            SimulatedUnixPlatform.linuxX64
            SimulatedUnixPlatform.linuxArm64
            SimulatedUnixPlatform.macOsArm64
        ]

    let private exitsWith (expected : int) (platform : SimulatedUnixPlatform) (argv : string list) : unit =
        match run platform argv with
        | RunOutcome.NormalExit (state, _, _) ->
            (platform, argv, state.LatchedExitCode)
            |> shouldEqual (platform, argv, expected)
        | other -> failwith $"%O{platform} %A{argv}: expected an exit with %d{expected}, got %O{other}"

    let private refused (platform : SimulatedUnixPlatform) (argv : string list) (reason : string) : unit =
        let exn = Assert.Catch<exn> (fun () -> run platform argv |> ignore<RunOutcome>)
        exn.Message |> shouldContainText "is not modelled"
        exn.Message |> shouldContainText reason

    [<Test>]
    let ``a signal the process discards is answered from either thread under either flavour`` () : unit =
        // 28 is SIGWINCH on both, discarded by default; 13 is SIGPIPE, which
        // the runtime ignores from startup.
        for platform in everyPlatform do
            for thread in [ "main" ; "worker" ] do
                for signo in [ 0 ; 28 ; 13 ] do
                    exitsWith 42 platform [ thread ; string<int> signo ; "none" ]

    [<Test>]
    let ``a number raise cannot send is EINVAL from either thread under either flavour`` () : unit =
        // 32 and 33 are signals on Linux, which glibc keeps for itself, and
        // past Darwin's last.
        for platform in everyPlatform do
            for thread in [ "main" ; "worker" ] do
                for signo in [ -1 ; 32 ; 33 ; 65 ] do
                    exitsWith (50 + 22) platform [ thread ; string<int> signo ; "none" ]

    [<Test>]
    let ``SIGTERM at its default kills the process from either thread`` () : unit =
        for platform in everyPlatform do
            for thread in [ "main" ; "worker" ] do
                match run platform [ thread ; "15" ; "none" ] with
                | RunOutcome.SignalTerminated (_, signal, _) -> signal |> shouldEqual Signal.SIGTERM
                | other -> failwith $"%O{platform} %s{thread}: expected death by SIGTERM, got %O{other}"

    [<Test>]
    let ``a registered signal raised on the main thread runs its handler`` () : unit =
        for platform in everyPlatform do
            exitsWith 42 platform [ "main" ; "15" ; "registered" ]

    [<Test>]
    let ``a caught signal raised on another thread is refused, being left pending there`` () : unit =
        // System.Native's handler, and the runtime's hardware-fault handler for
        // SIGILL (4 on both).
        for platform in everyPlatform do
            refused platform [ "worker" ; "15" ; "registered" ] "which is not the main thread"
            refused platform [ "worker" ; "4" ; "none" ] "which is not the main thread"

    [<Test>]
    let ``a hardware-fault signal raised on the main thread is survived once`` () : unit =
        // 4 is SIGILL on both, whose handler restores the default and returns.
        for platform in everyPlatform do
            exitsWith 42 platform [ "main" ; "4" ; "none" ]

    [<Test>]
    let ``a signal is read under the configured flavour's numbering`` () : unit =
        // 20 is SIGTSTP on Linux, which would stop the process, and SIGCHLD on
        // Darwin, which is discarded.
        exitsWith 42 SimulatedUnixPlatform.macOsArm64 [ "main" ; "20" ; "none" ]

        let exn =
            Assert.Catch<exn> (fun () ->
                run SimulatedUnixPlatform.linuxX64 [ "main" ; "20" ; "none" ]
                |> ignore<RunOutcome>
            )

        exn.Message |> shouldContainText "stopped process"

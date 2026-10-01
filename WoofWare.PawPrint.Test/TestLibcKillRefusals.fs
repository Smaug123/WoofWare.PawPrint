namespace WoofWare.PawPrint.Test

open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PosixKernel

/// The refusals of `NativeLibc.screenSelfSignal`, reached from a guest's own
/// libc `kill(2)` under each flavour: the signal is read under the configured
/// platform's numbering, and a signal the model cannot answer for ends the
/// run rather than being answered wrongly.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
// Runs guests under the interpreter; see AGENTS.md for why that makes it
// `Explicit`, and CI selects it by category.
[<Category("Guest")>]
[<Explicit>]
module TestLibcKillRefusals =

    let private guest : string =
        """
using System;
using System.Runtime.InteropServices;

class Program
{
    [DllImport("libc", EntryPoint = "kill", SetLastError = true)]
    static extern int Kill(int pid, int sig);

    static int Main(string[] args)
    {
        if (Kill(Environment.ProcessId, int.Parse(args[0])) != 0) return 1;
        return 42;
    }
}
"""

    /// Registers SIGINFO (29 on Darwin) at the shim without initialising its
    /// signal handling first, sends it, and then runs the shim's non-cancelled
    /// handling for SIGINFO.
    let private queuedThenRestoredGuest : string =
        """
using System;
using System.Runtime.InteropServices;

class Program
{
    [DllImport("libc", EntryPoint = "kill", SetLastError = true)]
    static extern int Kill(int pid, int sig);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_EnablePosixSignalHandling")]
    static extern int Enable(int signalCode);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_HandleNonCanceledPosixSignal")]
    static extern void HandleNonCanceled(int signalCode);

    static int Main(string[] args)
    {
        if (Enable(29) != 1) return 1;
        if (Kill(Environment.ProcessId, 29) != 0) return 2;
        HandleNonCanceled(29);
        return 42;
    }
}
"""

    /// Sends SIGWINCH, whose handler waits, and then a stop signal
    /// (`args[0]`) and SIGCONT (`args[1]`), both with handlers registered.
    let private queuedStopThenContinueGuest : string =
        """
using System;
using System.Runtime.InteropServices;
using System.Threading;

class Program
{
    [DllImport("libc", EntryPoint = "kill", SetLastError = true)]
    static extern int Kill(int pid, int sig);

    static int Main(string[] args)
    {
        int pid = Environment.ProcessId;
        using var release = new ManualResetEventSlim(false);
        using var busy = PosixSignalRegistration.Create(PosixSignal.SIGWINCH, _ => release.Wait());
        using var stop = PosixSignalRegistration.Create(PosixSignal.SIGTSTP, context => context.Cancel = true);
        using var cont = PosixSignalRegistration.Create(PosixSignal.SIGCONT, context => context.Cancel = true);

        if (Kill(pid, 28) != 0) return 1;
        if (Kill(pid, int.Parse(args[0])) != 0) return 2;
        if (Kill(pid, int.Parse(args[1])) != 0) return 3;

        release.Set();
        return 42;
    }
}
"""

    /// Sends SIGWINCH, whose handler waits, and then SIGCHLD (`args[0]`),
    /// registered, which is then unregistered.
    let private queuedThenUnregisteredGuest : string =
        """
using System;
using System.Runtime.InteropServices;
using System.Threading;

class Program
{
    [DllImport("libc", EntryPoint = "kill", SetLastError = true)]
    static extern int Kill(int pid, int sig);

    static int Main(string[] args)
    {
        int pid = Environment.ProcessId;
        using var release = new ManualResetEventSlim(false);
        using var busy = PosixSignalRegistration.Create(PosixSignal.SIGWINCH, _ => release.Wait());
        var child = PosixSignalRegistration.Create(PosixSignal.SIGCHLD, _ => { });

        if (Kill(pid, 28) != 0) return 1;
        if (Kill(pid, int.Parse(args[0])) != 0) return 2;
        child.Dispose();

        release.Set();
        return 42;
    }
}
"""

    /// Sends SIGWINCH, whose handler waits, and then two registered signals,
    /// `args[0]` and then `args[1]`.
    let private queuedThenAnotherGuest : string =
        """
using System;
using System.Runtime.InteropServices;
using System.Threading;

class Program
{
    [DllImport("libc", EntryPoint = "kill", SetLastError = true)]
    static extern int Kill(int pid, int sig);

    static int Main(string[] args)
    {
        int pid = Environment.ProcessId;
        using var release = new ManualResetEventSlim(false);
        using var busy = PosixSignalRegistration.Create(PosixSignal.SIGWINCH, _ => release.Wait());
        using var term = PosixSignalRegistration.Create(PosixSignal.SIGTERM, context => context.Cancel = true);
        using var interrupt = PosixSignalRegistration.Create(PosixSignal.SIGINT, context => context.Cancel = true);

        if (Kill(pid, 28) != 0) return 1;
        if (Kill(pid, int.Parse(args[0])) != 0) return 2;
        if (Kill(pid, int.Parse(args[1])) != 0) return 3;

        release.Set();
        return 42;
    }
}
"""

    let private runSourceWith (source : string) (platform : SimulatedUnixPlatform) (argv : string list) : RunOutcome =
        let arguments = String.concat " " argv
        let description = $"kill(self, %s{arguments})"

        let _messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "case", description ]

        use _loggerFactoryResource = loggerFactory
        let dotnetRuntimes = FrameworkUnderTest.runtimeDirs ()
        use peImage = new MemoryStream (Roslyn.compile [ source ])

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

    let private runSource (source : string) (platform : SimulatedUnixPlatform) (signo : int) : RunOutcome =
        runSourceWith source platform [ string<int> signo ]

    let private run (platform : SimulatedUnixPlatform) (signo : int) : RunOutcome = runSource guest platform signo

    let private refused (platform : SimulatedUnixPlatform) (signo : int) (reason : string) : unit =
        let exn = Assert.Catch<exn> (fun () -> run platform signo |> ignore<RunOutcome>)
        exn.Message |> shouldContainText "is not modelled"
        exn.Message |> shouldContainText reason

    [<Test>]
    let ``SIGPIPE, which the runtime ignores from startup, is discarded under either flavour`` () : unit =
        for platform in [ SimulatedUnixPlatform.linuxX64 ; SimulatedUnixPlatform.macOsArm64 ] do
            match run platform 13 with
            | RunOutcome.NormalExit (state, _, _) -> state.LatchedExitCode |> shouldEqual 42
            | other -> failwith $"expected kill(self, SIGPIPE) to be answered and the guest to exit 42, got %O{other}"

    [<Test>]
    let ``30 is Darwin's SIGUSR1, the runtime's activation signal, and is refused there`` () : unit =
        refused SimulatedUnixPlatform.macOsArm64 30 "runs a handler of CoreCLR's PAL for SIGUSR1"

    [<Test>]
    let ``11 is SIGSEGV, which the runtime catches, and is refused under either flavour`` () : unit =
        for platform in [ SimulatedUnixPlatform.linuxX64 ; SimulatedUnixPlatform.macOsArm64 ] do
            refused platform 11 "runs a handler of CoreCLR's PAL"

    [<Test>]
    let ``33 is glibc's SIGSETXID, which glibc catches, and is refused under Linux`` () : unit =
        refused SimulatedUnixPlatform.linuxX64 33 "runs glibc's own SIGSETXID handler"

    [<Test>]
    let ``30 is Linux's SIGPWR, which terminates a real process, and PawPrint's`` () : unit =
        match run SimulatedUnixPlatform.linuxX64 30 with
        | RunOutcome.SignalTerminated (_, signal, _) -> signal |> shouldEqual (Signal.Other 30)
        | other -> failwith $"expected termination by signal 30, got %O{other}"

    [<Test>]
    let ``SIGCONT with no handler is refused`` () : unit =
        refused SimulatedUnixPlatform.linuxX64 18 "has no stopped state"

    [<Test>]
    let ``a signal System.Native catches before its signal handling is initialised is refused`` () : unit =
        // A hand-rolled `SystemNative_EnablePosixSignalHandling` installs the
        // shim's handler with no pipe to write to: the real handler writes to
        // descriptor -1 and aborts.
        let exn =
            Assert.Catch<exn> (fun () ->
                runSource queuedThenRestoredGuest SimulatedUnixPlatform.macOsArm64 29
                |> ignore<RunOutcome>
            )

        exn.Message |> shouldContainText "never initialised"

    let private exitsWith42 (outcome : RunOutcome) : unit =
        match outcome with
        | RunOutcome.NormalExit (state, _, _) -> state.LatchedExitCode |> shouldEqual 42
        | other -> failwith $"expected the guest to exit 42, got %O{other}"

    [<Test>]
    let ``SIGCONT sent after a stop signal the native handler has already taken is answered`` () : unit =
        // The stop signal is in System.Native's pipe by the time SIGCONT is
        // generated, so the kernel's discard of pending stop signals does not
        // reach it, and both handlers run, as on the real runtime. SIGTSTP and
        // SIGCONT are 20 and 18 under Linux's numbering.
        runSourceWith queuedStopThenContinueGuest SimulatedUnixPlatform.linuxX64 [ "20" ; "18" ]
        |> exitsWith42

    [<Test>]
    let ``unregistering a signal the native handler has already taken is answered`` () : unit =
        // The SIGCHLD is in System.Native's pipe; whether the dispatcher reads
        // it before or after the registration goes, the process carries on:
        // the callback or the loop's non-cancelled handling, which for SIGCHLD
        // does nothing. SIGCHLD is 17 under Linux's numbering.
        runSourceWith queuedThenUnregisteredGuest SimulatedUnixPlatform.linuxX64 [ "17" ]
        |> exitsWith42

    [<Test>]
    let ``two signals sent while the dispatcher is busy are answered in either order`` () : unit =
        // The dispatcher reads them in the order the native handler wrote
        // them, whichever order the kernel would take them in.
        runSourceWith queuedThenAnotherGuest SimulatedUnixPlatform.linuxX64 [ "15" ; "2" ]
        |> exitsWith42

        runSourceWith queuedThenAnotherGuest SimulatedUnixPlatform.linuxX64 [ "2" ; "15" ]
        |> exitsWith42

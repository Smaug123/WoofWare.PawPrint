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

    /// Registers SIGINFO (29 on Darwin) at the shim with nothing to dispatch
    /// it, so a SIGINFO sent to the process stays queued, and then runs the
    /// shim's non-cancelled handling for SIGINFO, which restores the kernel's
    /// default of discarding it.
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

    let private runSource (source : string) (platform : SimulatedUnixPlatform) (signo : int) : RunOutcome =
        let description = $"kill(self, %d{signo})"

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
                        Argv = [ string<int> signo ]
                    }
            }

    let private run (platform : SimulatedUnixPlatform) (signo : int) : RunOutcome = runSource guest platform signo

    let private refused (platform : SimulatedUnixPlatform) (signo : int) (reason : string) : unit =
        let exn = Assert.Catch<exn> (fun () -> run platform signo |> ignore<RunOutcome>)
        exn.Message |> shouldContainText "is not modelled"
        exn.Message |> shouldContainText reason

    [<Test>]
    let ``SIGPIPE, which the runtime ignores from startup, is refused under Linux`` () : unit =
        refused SimulatedUnixPlatform.linuxX64 13 "catches or ignores SIGPIPE from startup"

    [<Test>]
    let ``30 is Darwin's SIGUSR1, the runtime's activation signal, and is refused there`` () : unit =
        refused SimulatedUnixPlatform.macOsArm64 30 "catches or ignores SIGUSR1 from startup"

    [<Test>]
    let ``30 is Linux's SIGPWR, which terminates a real process, and PawPrint's`` () : unit =
        match run SimulatedUnixPlatform.linuxX64 30 with
        | RunOutcome.SignalTerminated (_, signal) -> signal |> shouldEqual (Signal.Other 30)
        | other -> failwith $"expected termination by signal 30, got %O{other}"

    [<Test>]
    let ``SIGCONT with no handler is refused`` () : unit =
        refused SimulatedUnixPlatform.linuxX64 18 "has no stopped state"

    [<Test>]
    let ``restoring the default of a signal with an instance still queued is refused`` () : unit =
        // The real shim has already written the queued instance to its pipe,
        // and keeps the registration bit that sends it to the callback; the
        // model would discard it as ignored.
        let exn =
            Assert.Catch<exn> (fun () ->
                runSource queuedThenRestoredGuest SimulatedUnixPlatform.macOsArm64 29
                |> ignore<RunOutcome>
            )

        exn.Message |> shouldContainText "SystemNative_HandleNonCanceledPosixSignal"
        exn.Message |> shouldContainText "still queued"

namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PawPrint.Test.LinuxCoreLibFlavour
open WoofWare.PosixKernel

/// .NET 11's `SystemNative_LowLevelFutex_*` entry points, each one `futex(2)` call, answered
/// through the kernel's `UnixFutex`.
///
/// The guests hand-roll the P/Invokes, which reaches the handlers on any runtime and host, and
/// read errno through `SetLastError`. The real `libSystem.Native` exports these entry points
/// only from .NET 11, and makes the syscall only on Linux, so a guest is compared with the real
/// runtime only when .NET 11 on a Linux host is under test, which CI's Linux leg is.
///
/// Driven by stepping rather than by exit code alone, because an exit code cannot tell a wait
/// that parked from one that was answered at once: a test whose point is a park says so, and
/// fails if none happened.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestLowLevelFutex =

    /// The P/Invokes and errno numbers every guest here shares, under Linux's numbering, which is
    /// the numbering of the kernel the guests run on (`KernelConfig.Default`) and of the host
    /// they are compared on.
    let private prelude : string =
        """
using System;
using System.Runtime.InteropServices;
using System.Threading;

static unsafe class Futex
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_LowLevelFutex_WaitOnAddress", SetLastError = true)]
    public static extern void Wait(int* address, int comparand);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_LowLevelFutex_WaitOnAddressTimeout", SetLastError = true)]
    public static extern int WaitTimeout(int* address, int comparand, int timeoutMilliseconds);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_LowLevelFutex_WakeByAddressSingle", SetLastError = true)]
    public static extern void Wake(int* address);

    [DllImport("libc", EntryPoint = "kill", SetLastError = true)]
    public static extern int Kill(int pid, int sig);

    public const int EINTR = 4;
    public const int EAGAIN = 11;
    public const int EFAULT = 14;
    public const int EINVAL = 22;
    public const int ETIMEDOUT = 110;
    public const int SIGTERM = 15;

    // A futex word in its own cache line, as `LowLevelThreadBlocker` allocates it.
    public static int* NewWord()
    {
        int* word = (int*)NativeMemory.AlignedAlloc(64, 64);
        *word = 0;
        return word;
    }
}
"""

    /// How a stepped run ended, and whether any thread was ever parked in a futex wait.
    type private Journey =
        {
            Outcome : RunOutcome
            SawFutexPark : bool
        }

    let private futexParked (state : IlMachineState) : bool =
        state.ThreadState
        |> Map.exists (fun tid ts ->
            ts.Status = ThreadStatus.BlockedInSyscall
            && (
                match UnixThreadState.parkedIn (EmulatedKernel.taskOf tid state.Kernel.Tasks) with
                | Some (ParkedSyscall.Futex _) -> true
                | _ -> false
            )
        )

    let private journey (name : string) (dotnetRuntimes : ImmutableArray<string>) (image : byte[]) : Journey =
        let _messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", name ]

        use _loggerFactoryResource = loggerFactory
        let logger = loggerFactory.CreateLogger "TestLowLevelFutex"
        use peImage = new MemoryStream (image)

        let prepared =
            match Program.prepare loggerFactory (Some name) peImage (HostConfig.Default dotnetRuntimes) with
            | Program.ProgramStartResult.CompletedBeforeMain outcome ->
                failwith $"guest completed before Main: %O{outcome}"
            | Program.ProgramStartResult.Ready prepared -> prepared

        let rec loop (prepared : Program.PreparedProgram) (steps : int64) (sawPark : bool) : Journey =
            if steps > BoundedRun.defaultMaxSteps then
                failwith $"%s{name} ran past %d{BoundedRun.defaultMaxSteps} steps without terminating"

            let sawPark = sawPark || futexParked prepared.State

            match Program.stepPrepared loggerFactory logger prepared with
            | Program.ProgramStepOutcome.Completed outcome ->
                {
                    Outcome = outcome
                    SawFutexPark = sawPark
                }
            | Program.ProgramStepOutcome.Deadlocked (_, stuck) -> failwith $"%s{name} deadlocked: %s{stuck}"
            | Program.ProgramStepOutcome.WorkerTerminated (prepared, _)
            | Program.ProgramStepOutcome.InstructionStepped (prepared, _, _, _) -> loop prepared (steps + 1L) sawPark

        loop prepared 0L false

    /// Run `source` under PawPrint, comparing it with the real runtime where that can make the
    /// same call (see the fixture's comment), and return how PawPrint's run went. Fails unless
    /// it exits with code 0, which every guest here uses for "every check passed".
    let private run (name : string) (source : string) : Journey =
        let image = Roslyn.compile [ prelude + source ]

        let comparesHere =
            FrameworkUnderTest.runtime () = EmulatedRuntime.Net11
            && HostPlatform.flavour () = Some SimulatedUnixFlavour.Linux
            && OraclePolicy.hostShapeCanCompare System.BitConverter.IsLittleEndian System.IntPtr.Size

        let realResult, journey =
            if comparesHere then
                let real, journey =
                    DifferentialOracle.alongsideInterpreted
                        (fun () -> RealRuntime.executeWithRealRuntime [||] image)
                        (fun () -> journey name (FrameworkUnderTest.runtimeDirs ()) image)

                Some real, journey
            else
                None, journey name (FrameworkUnderTest.runtimeDirs ()) image

        match realResult with
        | Some real -> DifferentialOracle.compareOutcomes name 0 false real (RunEnd.Ended journey.Outcome)
        | None -> ()

        match journey.Outcome with
        | RunOutcome.NormalExit (state, _, _) -> state.LatchedExitCode |> shouldEqual 0
        | other -> failwith $"%s{name}: expected a normal exit, got %O{other}"

        journey

    [<Test>]
    let ``Each call answers as Linux's futex does where no thread waits`` () : unit =
        let source =
            """
unsafe class Program
{
    static int Main()
    {
        int* word = Futex.NewWord();
        int* misaligned = (int*)((byte*)word + 1);

        // A word that does not hold the comparand: EAGAIN, which is not a timeout.
        if (Futex.WaitTimeout(word, 1, 1000) != 1) return 1;
        if (Marshal.GetLastPInvokeError() != Futex.EAGAIN) return 2;
        Futex.Wait(word, 1);
        if (Marshal.GetLastPInvokeError() != Futex.EAGAIN) return 3;

        // Nobody wakes the waiter: a timeout.
        if (Futex.WaitTimeout(word, 0, 20) != 0) return 4;
        if (Marshal.GetLastPInvokeError() != Futex.ETIMEDOUT) return 5;

        // A misaligned word, and no word at all.
        if (Futex.WaitTimeout(misaligned, 0, 20) != 1) return 6;
        if (Marshal.GetLastPInvokeError() != Futex.EINVAL) return 7;
        if (Futex.WaitTimeout(null, 0, 20) != 1) return 8;
        if (Marshal.GetLastPInvokeError() != Futex.EFAULT) return 9;

        // A wake with no waiter succeeds, even with no memory behind it; a misaligned one fails.
        Futex.Wake(word);
        if (Marshal.GetLastPInvokeError() != 0) return 10;
        Futex.Wake(null);
        if (Marshal.GetLastPInvokeError() != 0) return 11;
        Futex.Wake(misaligned);
        if (Marshal.GetLastPInvokeError() != Futex.EINVAL) return 12;

        NativeMemory.AlignedFree(word);
        return 0;
    }
}
"""

        let journey = run "LowLevelFutexAnswers.cs" source
        journey.SawFutexPark |> shouldEqual true

    [<Test>]
    let ``A wake takes a waiter another thread parked`` () : unit =
        let source =
            """
unsafe class Program
{
    static int Main()
    {
        int* word = Futex.NewWord();
        IntPtr address = (IntPtr)word;
        int untimed = -1;
        int timed = -1;

        // Each waiter loops as LowLevelThreadBlocker does, so a wake that comes first, or a
        // spurious one, only sends it round again.
        Thread untimedWaiter = new Thread(() =>
        {
            int* w = (int*)address;
            while (Volatile.Read(ref *w) == 0) Futex.Wait(w, 0);
            untimed = Volatile.Read(ref *w);
        });
        Thread timedWaiter = new Thread(() =>
        {
            int* w = (int*)address;
            int answer = 1;
            while (Volatile.Read(ref *w) == 0 && answer == 1) answer = Futex.WaitTimeout(w, 0, 60000);
            timed = answer;
        });
        untimedWaiter.Start();
        timedWaiter.Start();
        Thread.Sleep(100);

        Volatile.Write(ref *word, 1);
        Futex.Wake(word);
        Futex.Wake(word);
        untimedWaiter.Join();
        timedWaiter.Join();
        if (untimed != 1) return 1;
        if (timed != 1) return 2;
        return 0;
    }
}
"""

        let journey = run "LowLevelFutexWaitWake.cs" source

        journey.SawFutexPark |> shouldEqual true

    [<Test>]
    let ``A signal ends a timed wait with EINTR and restarts an untimed one, which reads its word again`` () : unit =
        // System.Native installs its handler with SA_RESTART, under which Linux restarts an
        // untimed futex wait and fails a timed one with EINTR. The restarted wait reads the word
        // again, so a word another thread changed meanwhile, with no wake, answers EAGAIN.
        let source =
            """
unsafe class Program
{
    static int Handled;

    static Thread Signal(Action before)
    {
        Thread t = new Thread(() =>
        {
            Thread.Sleep(100);
            before();
            Futex.Kill(Environment.ProcessId, Futex.SIGTERM);
        });
        t.Start();
        return t;
    }

    static int Main()
    {
        var registration = PosixSignalRegistration.Create(
            PosixSignal.SIGTERM,
            context =>
            {
                context.Cancel = true;
                Interlocked.Increment(ref Handled);
            });

        int* word = Futex.NewWord();
        IntPtr address = (IntPtr)word;

        Thread signaller = Signal(() => { });
        int answer = Futex.WaitTimeout(word, 0, 60000);
        int error = Marshal.GetLastPInvokeError();
        signaller.Join();
        if (answer != 1) return 1;
        if (error != Futex.EINTR) return 2;

        signaller = Signal(() => Volatile.Write(ref *(int*)address, 1));
        Futex.Wait(word, 0);
        error = Marshal.GetLastPInvokeError();
        signaller.Join();
        if (error != Futex.EAGAIN) return 3;

        GC.KeepAlive(registration);
        return 0;
    }
}
"""

        let journey = run "LowLevelFutexSignal.cs" source
        journey.SawFutexPark |> shouldEqual true

    [<Test>]
    let ``A futex word outside native-heap memory is refused`` () : unit =
        let source =
            """
unsafe class Program
{
    static int Main()
    {
        int[] words = new int[1];
        fixed (int* word = words)
        {
            Futex.Wake(word);
        }

        return 0;
    }
}
"""

        let image = Roslyn.compile [ prelude + source ]

        let exn =
            Assert.Catch (fun () ->
                journey "LowLevelFutexManagedWord.cs" (FrameworkUnderTest.runtimeDirs ()) image
                |> ignore
            )

        exn.Message
        |> shouldContainText "SystemNative_LowLevelFutex_WakeByAddressSingle"

        exn.Message |> shouldContainText "native-heap memory"

    /// The path real guest code takes: on the Linux CoreLib, a thread-pool worker with nothing
    /// to do blocks in `LowLevelLifoSemaphore`, through `LowLevelThreadBlocker`, and the next
    /// work item wakes it.
    [<Test>]
    let ``The Linux CoreLib's thread pool blocks and wakes its workers through the futex`` () : unit =
        ParkedOn.check
            [ EmulatedRuntime.Net10 ]
            "Only .NET 11's CoreLib blocks through LowLevelThreadBlocker, and so through futex; .NET 10's blocks on LowLevelMonitor. Nothing to un-park: the test is about .NET 11."

        let frameworkDir = FrameworkUnderTest.requireLinuxFrameworkDirectory ()

        let source =
            """
using System;
using System.Threading;

class Program
{
    static int Main()
    {
        int total = 0;
        for (int round = 1; round <= 3; round++)
        {
            using var done = new ManualResetEventSlim(false);
            int r = round;
            ThreadPool.QueueUserWorkItem(_ => { Interlocked.Add(ref total, r); done.Set(); });
            done.Wait();
            // Long enough for the worker to run out of work and block.
            Thread.Sleep(100);
        }

        return total == 6 ? 0 : 1;
    }
}
"""

        let journey =
            journey "LowLevelFutexThreadPool.cs" (runtimeDirsPreferringLinux frameworkDir) (Roslyn.compile [ source ])

        match journey.Outcome with
        | RunOutcome.NormalExit (state, _, _) ->
            state.LatchedExitCode |> shouldEqual 0
            loadedCorelibPath state |> shouldEqual (corelibPath frameworkDir)
        | other -> failwith $"expected a normal exit, got %O{other}"

        journey.SawFutexPark |> shouldEqual true

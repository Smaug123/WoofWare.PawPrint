namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint
open WoofWare.PosixKernel

/// Two threads parked in *different* blocking syscalls at the same time.
///
/// `ThreadStatus.BlockedInSyscall` says only that a thread is asleep in one, so which syscall a
/// waiter is in — and therefore which sweep owns it — is a question only the task's
/// `ParkedSyscall` record answers. Every other park fixture has one kind of waiter, under which
/// "select the parks I own" and "take every parked thread" agree; this is the workload that tells
/// them apart, and it is what makes `Scheduler`'s two wake helpers' kind guards more than
/// decoration.
///
/// Driven by stepping, because the interesting half is invisible to the guest: a lock waiter woken
/// by the *socket* sweep would be woken for no reason and simply park again, and a socket waiter
/// woken by the lock sweep likewise. Only the states the driver passes through record the
/// excursion — and, as it happens, the wake guards turn both into a loud failure first.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestSyscallPark =

    let private assy = System.Reflection.Assembly.GetExecutingAssembly ()

    /// One regular file, for the lock waiter to contend on.
    let private seed : KernelConfig =
        { KernelConfig.Default with
            FileSystem =
                Map.ofList
                    [
                        DirectoryEntryName.parseOrFail "test seed" "f",
                        SeedEntry.file (System.Text.Encoding.UTF8.GetBytes "hello" |> ImmutableArray.CreateRange)
                    ]
        }

    /// The entry thread takes an exclusive lock, starts one thread that blocks for it and one that
    /// waits on an event port nothing will ever make ready, and then releases. Only the lock waiter
    /// may wake.
    ///
    /// The second sleep is what makes the observation deterministic: after the release the entry
    /// thread is blocked again, so a spuriously woken port waiter is the only other runnable thread
    /// and the scheduler must pick it.
    let private source : string =
        """
using System;
using System.Runtime.InteropServices;
using System.Threading;

class LockAndPortWaiters
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Open", SetLastError = true)]
    static extern unsafe IntPtr Open(byte* path, int flags, int mode);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_FLock", SetLastError = true)]
    static extern int FLock(IntPtr fd, int operation);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_CreateSocketEventPort")]
    static extern unsafe int CreateSocketEventPort(IntPtr* port);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_WaitForSocketEvents")]
    static extern unsafe int WaitForSocketEvents(IntPtr port, byte* buffer, int* count);

    const int LOCK_EX = 2;
    const int LOCK_NB = 4;
    const int LOCK_UN = 8;

    static unsafe IntPtr OpenF()
    {
        byte* path = stackalloc byte[2];
        path[0] = (byte)'f';
        path[1] = 0;
        return Open(path, 0, 0);
    }

    static int LockResult = -1;

    static unsafe void LockWaiter()
    {
        IntPtr b = OpenF();
        // No LOCK_NB, and the entry thread holds the lock: this parks.
        LockResult = FLock(b, LOCK_EX);
    }

    static unsafe void PortWaiter()
    {
        IntPtr port;
        if (CreateSocketEventPort(&port) != 0) return;
        byte* buffer = stackalloc byte[32];
        int count = 1;
        // Nothing is registered with this port, so nothing can ever make it ready.
        WaitForSocketEvents(port, buffer, &count);
    }

    static unsafe int Main(string[] args)
    {
        IntPtr a = OpenF();
        if (FLock(a, LOCK_EX | LOCK_NB) != 0) return 1;

        new Thread(LockWaiter) { IsBackground = true }.Start();
        new Thread(PortWaiter) { IsBackground = true }.Start();
        Thread.Sleep(100);

        if (FLock(a, LOCK_UN) != 0) return 2;
        Thread.Sleep(100);

        return LockResult == 0 ? 5 : 6;
    }
}
"""

    /// Every thread parked in a syscall, with the kind of park its task records.
    let private parked (state : IlMachineState) : (ThreadId * ParkedSyscall) list =
        state.ThreadState
        |> Map.toList
        |> List.choose (fun (tid, ts) ->
            match ts.Status with
            | ThreadStatus.BlockedInSyscall ->
                UnixTaskTable.parkedFor tid state.Kernel.Tasks |> Option.map (fun p -> tid, p)
            | _ -> None
        )

    let private isPortWait (parked : ParkedSyscall) : bool =
        match parked with
        | ParkedSyscall.SocketWait _ -> true
        | ParkedSyscall.Flock _ -> false

    /// The exit code a terminated guest left on its terminating thread's eval stack.
    let private exitCodeOf (outcome : RunOutcome) : int =
        let terminalState, terminatingThread =
            match outcome with
            | RunOutcome.NormalExit (state, thread) -> state, thread
            | RunOutcome.ProcessExit (state, thread) -> state, thread
            | other -> failwith $"expected the guest to terminate cleanly, got %O{other}"

        match terminalState.ThreadState.[terminatingThread].MethodState.EvaluationStack.Values with
        | [] -> failwith "expected the guest to return a value, but it returned void"
        | EvalStackValue.Int32 (Int32Source.Verbatim i) :: _ -> i
        | ret :: _ -> failwith $"expected the guest to return an int, but it returned %O{ret}"

    [<Test>]
    let ``a release wakes the lock waiter and leaves the port waiter alone`` () : unit =
        let image = Roslyn.compile [ source ]

        let _messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", "LockAndPortWaiters.cs" ]

        use _loggerFactoryResource = loggerFactory
        let logger = loggerFactory.CreateLogger "TestSyscallPark"

        let dotnetRuntimes =
            DotnetRuntime.SelectForDll assy.Location |> ImmutableArray.CreateRange

        use peImage = new MemoryStream (image)

        let config =
            { HostConfig.Default dotnetRuntimes with
                Guest =
                    { GuestConfig.Default dotnetRuntimes with
                        Kernel = seed
                    }
            }

        let prepared =
            match Program.prepare loggerFactory (Some "LockAndPortWaiters.cs") peImage config with
            | Program.ProgramStartResult.CompletedBeforeMain outcome ->
                failwith $"guest completed before Main: %O{outcome}"
            | Program.ProgramStartResult.Ready prepared -> prepared

        let maxSteps = 20_000_000L

        // `port` is `Some` once the port waiter has been seen parked; it is consulted *before*
        // each step, so the step in which it parked is not itself counted as a run after parking.
        let rec loop
            (prepared : Program.PreparedProgram)
            (steps : int64)
            (port : ThreadId option)
            (bothParked : bool)
            (ranAfterParking : ThreadId list)
            : RunOutcome * bool * bool * ThreadId list
            =
            if steps > maxSteps then
                failwith $"guest did not terminate within %d{maxSteps} steps"

            let parkedNow = parked prepared.State

            let port =
                match port with
                | Some _ -> port
                | None ->
                    parkedNow
                    |> List.tryPick (fun (tid, p) -> if isPortWait p then Some tid else None)

            let bothParked =
                bothParked
                || (parkedNow |> List.exists (fun (_, p) -> isPortWait p)
                    && parkedNow |> List.exists (fun (_, p) -> not (isPortWait p)))

            let ranAfter (ran : ThreadId) : ThreadId list =
                match port with
                | Some p when p = ran -> ran :: ranAfterParking
                | _ -> ranAfterParking

            match Program.stepPrepared loggerFactory logger prepared with
            | Program.ProgramStepOutcome.Completed outcome -> outcome, port.IsSome, bothParked, ranAfterParking
            | Program.ProgramStepOutcome.Deadlocked (_, stuck) ->
                failwith $"guest deadlocked rather than completing. Stuck: %s{stuck}"
            | Program.ProgramStepOutcome.WorkerTerminated (prepared, _) ->
                loop prepared (steps + 1L) port bothParked ranAfterParking
            | Program.ProgramStepOutcome.InstructionStepped (prepared, ran, _, _) ->
                loop prepared (steps + 1L) port bothParked (ranAfter ran)

        let outcome, portParked, bothParked, ranAfterParking =
            loop prepared 0L None false []

        // The lock waiter's acquisition completed, which is the sweep having woken the right
        // thread rather than no thread.
        exitCodeOf outcome |> shouldEqual 5

        // Vacuity: the two-park state the fixture exists for really did occur. Without this a
        // guest whose waiters never overlapped would satisfy everything below trivially.
        portParked |> shouldEqual true
        bothParked |> shouldEqual true

        // And the port waiter never ran again: no sweep woke it, and neither did the release that
        // was not its business.
        ranAfterParking |> shouldEqual []

namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint
open WoofWare.PosixKernel

/// A blocking `flock` that cannot be granted parks the calling thread, and a release wakes it.
///
/// The library decided the park in stage 9a: `UnixSystem.flock` answers `WouldBlock` carrying the
/// lock being waited for. This fixture is about the half PawPrint owns — a thread status, a
/// readiness sweep, and a re-entrant native frame — and about the one thing neither half can state
/// alone, which is that a parked call ever finishes.
///
/// Driven by stepping rather than by exit code, because an exit code cannot tell a park that
/// worked from a park that never happened: if the second thread reaches its `flock` after the
/// first has released, the acquisition succeeds uncontended and the guest exits 0 having covered
/// nothing.
///
/// CoreLib cannot reach any of this — `SafeFileHandle.Init` always sets `LOCK_NB` — so the guests
/// hand-roll the P/Invoke, as `TestFlockBlocking`'s do.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestFlockPark =

    let private assy = System.Reflection.Assembly.GetExecutingAssembly ()

    /// One regular file, which is all these guests open.
    let private seed : KernelConfig =
        { KernelConfig.Default with
            FileSystem =
                Map.ofList
                    [
                        FileName.parseOrFail "test seed" "f",
                        SeedEntry.file (System.Text.Encoding.UTF8.GetBytes "hello" |> ImmutableArray.CreateRange)
                    ]
        }

    let private guest (body : string) : string =
        $"""
using System;
using System.Runtime.InteropServices;
using System.Threading;

class Program
{{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Open", SetLastError = true)]
    static extern unsafe IntPtr Open(byte* path, int flags, int mode);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_FLock", SetLastError = true)]
    static extern int FLock(IntPtr fd, int operation);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Close", SetLastError = true)]
    static extern int Close(IntPtr fd);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Dup", SetLastError = true)]
    static extern IntPtr Dup(IntPtr fd);

    const int LOCK_SH = 1;
    const int LOCK_EX = 2;
    const int LOCK_NB = 4;
    const int LOCK_UN = 8;

    static unsafe IntPtr OpenF()
    {{
        byte* path = stackalloc byte[2];
        path[0] = (byte)'f';
        path[1] = 0;
        return Open(path, 0, 0);
    }}

    static unsafe int Main(string[] args)
    {{
{body}
    }}
}}
"""

    let private prepared (name : string) (source : string) : Program.PreparedProgram =
        let image = Roslyn.compile [ source ]

        let _messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", name ]

        let logger = loggerFactory.CreateLogger "TestFlockPark"
        ignore<Microsoft.Extensions.Logging.ILogger> logger

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

        match Program.prepare loggerFactory (Some name) peImage config with
        | Program.ProgramStartResult.CompletedBeforeMain outcome -> failwith $"guest completed before Main: %O{outcome}"
        | Program.ProgramStartResult.Ready prepared -> prepared

    /// How many threads are parked in an `flock`.
    ///
    /// The status says only that a thread is parked in *some* syscall, so the record is what
    /// makes this count locks rather than parks. These guests reach no other parking syscall,
    /// but a counter that would silently include one is not what any assertion below means.
    let private parkedCount (state : IlMachineState) : int =
        state.ThreadState
        |> Map.toSeq
        |> Seq.filter (fun (tid, ts) ->
            ts.Status = ThreadStatus.BlockedInSyscall
            && match UnixTaskTable.parkedFor tid state.Kernel.Tasks with
               | Some (ParkedSyscall.Flock _) -> true
               | Some (ParkedSyscall.SocketWait _)
               | None -> false
        )
        |> Seq.length

    /// Whether any thread is parked in an `flock`.
    let private someoneParked (state : IlMachineState) : bool = parkedCount state > 0

    /// The outcome of stepping a guest, together with whether an `flock` park was ever observed
    /// along the way and the last state in which one was.
    type private Journey =
        {
            Outcome : RunOutcome option
            /// The state at the first moment a thread was parked in an `flock`, if that ever
            /// happened. This is what a park test asserts against: once the run has moved on, the
            /// park is gone and nothing about the final state records that it occurred.
            FirstPark : IlMachineState option
            /// The most threads ever parked on locks at once. A multi-waiter test that never got
            /// two threads parked together would pass vacuously without this.
            MostParkedAtOnce : int
            /// Whether some single step took two or more threads out of the lock park together.
            ///
            /// This is what tells wake-all apart from wake-one. The two differ only in *which*
            /// waiter wins a released lock, which is unobservable to a guest — so the observation
            /// has to be made here, on the states the driver passes through.
            SawSimultaneousWake : bool
            Stuck : string option
        }

    /// Step to termination or deadlock, noticing whether anything parked on the way.
    let private journey (name : string) (source : string) : Journey =
        let _messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", name ]

        use _loggerFactoryResource = loggerFactory
        let logger = loggerFactory.CreateLogger "TestFlockPark"

        // Bounded rather than unbounded: an implementation that "parks" by leaving the thread
        // Runnable and re-entering the handler would otherwise spin here for ever instead of
        // failing.
        let maxSteps = 20_000_000L

        let rec loop
            (prepared : Program.PreparedProgram)
            (steps : int64)
            (firstPark : IlMachineState option)
            (most : int)
            (sawWake : bool)
            =
            if steps > maxSteps then
                failwith $"guest ran past %d{maxSteps} steps without terminating or deadlocking"

            let before = parkedCount prepared.State

            let firstPark =
                match firstPark with
                | Some _ -> firstPark
                | None -> if before > 0 then Some prepared.State else None

            let most = max most before

            let finish (state : IlMachineState) (outcome : RunOutcome option) (stuck : string option) : Journey =
                {
                    Outcome = outcome
                    FirstPark =
                        match firstPark with
                        | Some _ -> firstPark
                        | None -> if someoneParked state then Some state else None
                    MostParkedAtOnce = max most (parkedCount state)
                    SawSimultaneousWake = sawWake
                    Stuck = stuck
                }

            // Two or more leaving the lock park in one step is the wake-all rule firing; one
            // leaving is a thread being scheduled, which says nothing.
            let woke (after : IlMachineState) : bool =
                sawWake || (before - parkedCount after >= 2)

            match Program.stepPrepared loggerFactory logger prepared with
            | Program.ProgramStepOutcome.Completed outcome ->
                match outcome with
                | RunOutcome.NormalExit (state, _)
                | RunOutcome.ProcessExit (state, _) -> finish state (Some outcome) None
                | _ -> finish prepared.State (Some outcome) None
            | Program.ProgramStepOutcome.Deadlocked (prepared, stuck) -> finish prepared.State None (Some stuck)
            | Program.ProgramStepOutcome.WorkerTerminated (prepared, _) ->
                loop prepared (steps + 1L) firstPark most (woke prepared.State)
            | Program.ProgramStepOutcome.InstructionStepped (prepared, _, _, _) ->
                loop prepared (steps + 1L) firstPark most (woke prepared.State)

        loop (prepared name source) 0L None 0 false

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

    let private completed (journey : Journey) : int =
        match journey.Outcome with
        | Some outcome -> exitCodeOf outcome
        | None ->
            let stuck = journey.Stuck |> Option.defaultValue "(no description)"
            failwith $"expected the guest to complete; it deadlocked instead. Stuck: %s{stuck}"

    let private parkedState (journey : Journey) : IlMachineState =
        match journey.FirstPark with
        | Some state -> state
        | None ->
            failwith
                "no thread ever parked in an flock, so this test covered nothing: the contention it set up did not happen."

    [<Test>]
    let ``a contended blocking lock parks, and the holder's release completes it`` () : unit =
        // The whole mechanism end to end: park, sweep, wake, re-entry, grant.
        //
        // The worker takes the lock and sleeps; the main thread's blocking acquire therefore
        // cannot be granted when it is made, and can only complete because the release woke it.
        let source =
            guest
                """
        IntPtr a = OpenF();
        if ((long)a < 0) return 1;

        var ready = new ManualResetEventSlim(false);
        var holder = new Thread(() =>
        {
            IntPtr b = OpenF();
            if (FLock(b, LOCK_EX | LOCK_NB) != 0) { Environment.Exit(2); }
            ready.Set();
            Thread.Sleep(50);
            if (FLock(b, LOCK_UN) != 0) { Environment.Exit(3); }
        });
        holder.Start();
        ready.Wait();

        // No LOCK_NB: this must park until the worker releases.
        if (FLock(a, LOCK_EX) != 0) return 4;

        // The lock really is held now, so a third description cannot take it...
        IntPtr c = OpenF();
        if (FLock(c, LOCK_EX | LOCK_NB) != -1) return 5;
        // ...and specifically the *exclusive* lock that was asked for, which only a
        // shared request can tell apart: an exclusive holder refuses it and a shared
        // one would grant it.
        if (FLock(c, LOCK_SH | LOCK_NB) != -1) return 6;

        if (FLock(a, LOCK_UN) != 0) return 7;
        return 0;
"""

        let journey = journey "FlockParkAndWake.cs" source

        // Both halves are load-bearing. Without the park assertion, a run in which the worker
        // released before the main thread ever asked would pass having contended with nothing;
        // without the exit code, a park that never woke would too.
        parkedState journey |> ignore<IlMachineState>
        completed journey |> shouldEqual 0

    [<Test>]
    let ``a release wakes every waiter, not one chosen by this interpreter`` () : unit =
        // The decision this sweep takes *against* its socket neighbour, which refuses to wake
        // several waiters at all: epoll's wait queue is exclusive, so a real edge wakes exactly
        // one by park order, and PawPrint keeps no state to reproduce that. `flock` has no
        // exclusive handoff, and which waiter wins is unobservable to any userspace program — so
        // waking them all and letting the scheduler pick declines to invent a winner, where
        // waking the lowest-numbered one would invent one.
        //
        // A guest cannot see the difference, which is the whole point; so the observation is made
        // on the states the driver passes through.
        let source =
            guest
                """
        IntPtr a = OpenF();
        if (FLock(a, LOCK_EX | LOCK_NB) != 0) return 1;

        var arrived = new CountdownEvent(2);
        var failed = 0;

        ThreadStart contend = () =>
        {
            IntPtr mine = OpenF();
            arrived.Signal();
            // Blocking: both workers park here, and both are woken by the release below.
            if (FLock(mine, LOCK_EX) != 0) { Interlocked.Exchange(ref failed, 1); return; }
            if (FLock(mine, LOCK_UN) != 0) { Interlocked.Exchange(ref failed, 2); return; }
        };

        var first = new Thread(contend);
        var second = new Thread(contend);
        first.Start();
        second.Start();

        // Both workers have opened their descriptions and are about to park. The sleep gives
        // them time to actually reach the syscall before the lock frees.
        arrived.Wait();
        Thread.Sleep(50);

        if (FLock(a, LOCK_UN) != 0) return 3;

        first.Join();
        second.Join();
        return failed;
"""

        let journey = journey "FlockParkTwoWaiters.cs" source

        // Vacuity guards first: without two threads parked at once there is nothing for a
        // wake-all rule to be right about.
        journey.MostParkedAtOnce |> shouldBeGreaterThan 1

        journey.SawSimultaneousWake |> shouldEqual true

        completed journey |> shouldEqual 0

    [<Test>]
    let ``the wake finishes the call on the description, not on the descriptor`` () : unit =
        // A parked call is finished against the open file description it parked on, never by
        // re-issuing it with the descriptor number the guest passed. Numbers are handed out
        // lowest-free, so the number this call parked through is closed and immediately reused
        // for a different file while the call sleeps; a resume that trusted it would lock the
        // wrong object.
        let source =
            guest
                """
        IntPtr a = OpenF();
        if ((long)a < 0) return 1;
        // `alias` keeps a's description alive when a is closed.
        IntPtr alias = Dup(a);
        if ((long)alias < 0) return 2;

        var ready = new ManualResetEventSlim(false);
        var parked = new ManualResetEventSlim(false);
        var holder = new Thread(() =>
        {
            IntPtr b = OpenF();
            if (FLock(b, LOCK_EX | LOCK_NB) != 0) { Environment.Exit(3); }
            ready.Set();
            // Close `a` and reopen, so its number now names something else entirely, while
            // the main thread is parked on the description `alias` still holds.
            parked.Wait();
            if (Close(a) != 0) { Environment.Exit(4); }
            IntPtr reused = OpenF();
            if ((long)reused != (long)a) { Environment.Exit(5); }
            if (FLock(b, LOCK_UN) != 0) { Environment.Exit(6); }
        });
        holder.Start();
        ready.Wait();

        // Signal *before* parking: the worker's close must land while this call sleeps, and
        // this thread cannot signal once it is parked.
        parked.Set();
        if (FLock(a, LOCK_EX) != 0) return 7;

        // Which description ended up holding the lock is the whole question, and the
        // return code cannot answer it: a resume that re-issued through the number `a`
        // used to be would have locked `reused`, and would also have returned 0.
        //
        // Releasing through `alias` releases the description this call parked on and
        // nothing else. If that is where the lock is, the file is now free; if the
        // resume put it on `reused` instead, `alias` released nothing and it is not.
        if (FLock(alias, LOCK_UN) != 0) return 8;
        IntPtr witness = OpenF();
        if (FLock(witness, LOCK_EX | LOCK_NB) != 0) return 9;
        return 0;
"""

        let journey = journey "FlockParkDescriptorReused.cs" source

        parkedState journey |> ignore<IlMachineState>
        completed journey |> shouldEqual 0

    [<Test>]
    let ``a release by closing the holder's descriptor wakes a waiter`` () : unit =
        // A lock is released by more than the obvious call: dropping the holder's last
        // descriptor drops its lock too, and nothing about `close` knows somebody is waiting.
        // The sweep is what makes that work, and it is why waking is a sweep rather than
        // something each releasing syscall pushes.
        let source =
            guest
                """
        IntPtr a = OpenF();
        if ((long)a < 0) return 1;

        var ready = new ManualResetEventSlim(false);
        var holder = new Thread(() =>
        {
            IntPtr b = OpenF();
            if (FLock(b, LOCK_EX | LOCK_NB) != 0) { Environment.Exit(2); }
            ready.Set();
            Thread.Sleep(50);
            // No LOCK_UN anywhere: the close is the release.
            if (Close(b) != 0) { Environment.Exit(3); }
        });
        holder.Start();
        ready.Wait();

        if (FLock(a, LOCK_EX) != 0) return 4;
        return 0;
"""

        let journey = journey "FlockParkClosedRelease.cs" source

        parkedState journey |> ignore<IlMachineState>
        completed journey |> shouldEqual 0

    [<Test>]
    let ``a parked conversion is holding nothing while it waits`` () : unit =
        // The claim stage 9a's outcome exists to carry: `flock` removes the caller's old lock
        // before it establishes the new one, so a conversion that has to wait is already holding
        // nothing by the time it sleeps.
        //
        // Single-threaded and self-deadlocking, which is exactly what a real kernel would do
        // here — and what makes it a deterministic observer, where a fresh contended acquire
        // could not observe the advance at all: a requester holding nothing has nothing to drop.
        let source =
            guest
                """
        IntPtr a = OpenF();
        IntPtr b = OpenF();
        if (FLock(a, LOCK_SH | LOCK_NB) != 0) return 1;
        if (FLock(b, LOCK_SH | LOCK_NB) != 0) return 2;
        // Converting b to exclusive cannot be granted while a holds shared, and without
        // LOCK_NB it parks -- for ever, since this is the only thread.
        if (FLock(b, LOCK_EX) != 0) return 3;
        return 4;
"""

        let journey = journey "FlockParkedConversion.cs" source

        match journey.Outcome with
        | None -> ()
        | Some outcome ->
            failwith $"expected the guest to deadlock on its own conversion, but it completed: %O{outcome}"

        let state = parkedState journey

        // `b`'s description holds nothing: the conversion dropped its shared lock before
        // discovering it could not have the exclusive one.
        let registry = (EmulatedKernel.unix state.Kernel).Process.FileDescriptors

        match FileDescriptorRegistry.tryFind 4 registry with
        | Some description -> description.Flock |> shouldEqual None
        | None -> failwith "expected fd 4 to still be open"

        // ...and `a` still holds its shared lock, so the drop was the conversion's own rather
        // than everything being cleared.
        match FileDescriptorRegistry.tryFind 3 registry with
        | Some description -> description.Flock |> shouldEqual (Some FlockMode.Shared)
        | None -> failwith "expected fd 3 to still be open"

    [<Test>]
    let ``the stuck-thread report names the lock a parked thread waits for`` () : unit =
        // `ThreadStatus.BlockedInSyscall` carries no payload, so the report has to reach into the
        // kernel's park record for it. Without that, a person debugging a stuck guest is told
        // only that some thread is parked on some lock.
        let source =
            guest
                """
        IntPtr a = OpenF();
        IntPtr b = OpenF();
        if (FLock(a, LOCK_EX | LOCK_NB) != 0) return 1;
        if (FLock(b, LOCK_SH) != 0) return 2;
        return 3;
"""

        let journey = journey "FlockParkReport.cs" source

        match journey.Stuck with
        | None -> failwith "expected the guest to deadlock"
        | Some stuck ->
            stuck |> shouldContainText "BlockedInSyscall"
            // *Which* syscall, which the status no longer says either.
            stuck |> shouldContainText "for a lock on"
            // The mode, and the description rather than the descriptor number.
            stuck |> shouldContainText "Shared"
            stuck |> shouldContainText "open file description"

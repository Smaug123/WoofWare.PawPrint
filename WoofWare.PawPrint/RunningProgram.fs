namespace WoofWare.PawPrint

open Microsoft.Extensions.Logging
open WoofWare.PosixKernel

/// What `Main` returns, which decides whether its return writes the latched exit code:
/// CoreCLR's `RunMain` copies an `int Main`'s return value to `IlMachineState.LatchedExitCode`
/// the moment `Main` returns, and a `void Main` leaves the latch as the guest left it.
[<RequireQualifiedAccess>]
type internal MainReturn =
    /// The exit code is whatever the guest last wrote to `Environment.ExitCode`, 0 if nothing did.
    | Void
    /// The return value becomes the latched exit code as `Main` returns, overwriting any
    /// earlier `Environment.ExitCode` write and overwritten by any later one.
    | Int32

/// Where a program's startup has got to: which call it is pumping on the entry thread, and
/// what it does when that call returns.
///
/// Startup runs guest code up to three times before `Main`, and the runs are not
/// interchangeable. They are in the order CoreCLR runs them: the AppContext seed is
/// `CorHost2::CreateAppDomainWithManager`, and the command line is
/// `CorHost2::ExecuteAssembly` calling `SetCommandLineArgs` immediately before
/// `ExecuteMainMethod` — which is what triggers the entry type's `.cctor`. Both deadlines
/// bite: BCL feature switches latch into `static readonly` fields on first read, and a
/// `.cctor` may call `Environment.GetCommandLineArgs` itself.
///
/// Modelled as a DU carrying each phase's own data so the phases cannot drift apart —
/// there is no way to be initialising classes without `Main`'s arguments in hand, nor to
/// be pumping a call without knowing what to do when it returns. Each pumped phase names
/// its successor rather than always yielding to class initialisation, so inserting or
/// skipping one is a local change.
///
/// The phase transitions are closures. They capture concretization results (a concretized
/// `Main`, the entry type's handle) whose inspectable form would be no more use to a caller
/// than the functions that consume them, and hoisting them to module scope would mean
/// threading ten parameters through for no gain in reasoning. What a caller *can* see —
/// the machine state, and which outcome a step produced — is data.
[<RequireQualifiedAccess>]
type internal StartupPhase =
    /// Pumping `AppContext.Setup`; `onReturn` puts the next phase's call on the entry thread.
    | SeedingAppContext of onReturn : (IlMachineState -> IlMachineState * StartupPhase)
    /// Pumping `Environment.InitializeCommandLineArgs`, whose return value is the array
    /// `Main` must receive; `onReturn` puts the next phase's call on the entry thread.
    | InitialisingCommandLine of onReturn : (IlMachineState -> IlMachineState * StartupPhase)
    /// Pumping class initialisers, the entry type's included, with `Main`'s arguments
    /// already in hand: `installMain` puts `Main` on the entry thread once they have run, and
    /// `returns` is what `Main` returns.
    | InitialisingClasses of installMain : (IlMachineState -> IlMachineState) * returns : MainReturn

/// What the entry thread is running, which decides what its bottom frame returning means.
[<RequireQualifiedAccess>]
type internal EntryFrameKind =
    /// One of the calls startup pumps to completion: the AppContext seed, the command-line
    /// initialiser, a class initialiser. Its return ends the phase, and `phase` says what the
    /// entry thread runs next.
    | StartupCall of phase : StartupPhase
    /// `Main`. Its return latches the exit code if `Main` returns one, and does not end the
    /// run: the entry thread goes to `ThreadStatus.WaitingForForegroundThreads` (and
    /// background), and the run ends with `NormalExit` once `shutdownSignalled` — CoreCLR's
    /// `ThreadStore::WaitForOtherThreads`, which `RunMainPost` blocks in after `Main`.
    ///
    /// `shutdownSignalled` is CoreCLR's `m_TerminationEvent`, a manual-reset event that
    /// `CheckForEEShutdown` sets at the first moment, once `Main` is running, at which no
    /// foreground thread is alive, and which nothing resets. It is set *before* `Main`
    /// returns if `Main` makes itself background while it is the only foreground thread —
    /// and then `WaitForOtherThreads` returns at once whatever `Main` started afterwards
    /// (measured on real .NET 10: `Main` goes background, starts a foreground worker that
    /// sleeps for ever, returns 3; the process exits 3). So it is carried as state rather
    /// than recomputed from the thread table when `Main` returns.
    ///
    /// CoreCLR arms this (`g_fWeControlLifetime`) in `RunMainPre`, which is also before
    /// the entry type's class initialiser runs; PawPrint arms it only once `Main` itself is
    /// installed, so a `.cctor` that sends the entry thread background and starts a
    /// foreground worker is waited for here where real .NET would abandon it.
    | Main of returns : MainReturn * shutdownSignalled : bool

/// One PawPrint process the driver (`MultiProgram`) runs: its interpreter state, and what
/// its scheduler carries from one tick to the next.
type internal RunningProgram =
    {
        /// The process's interpreter state. Its kernel holds the process's view of the
        /// machine, which is the machine as it stands only while the program is checked out
        /// of its driver.
        State : IlMachineState
        BaseClassTypes : BaseClassTypes<DumpedAssembly>
        EntryThread : ThreadId
        EntryFrame : EntryFrameKind
        LastRan : ThreadId
    }

/// What one program's step did, once its scheduler had picked a thread to run.
[<Struct>]
[<RequireQualifiedAccess>]
type internal ProgramTick =
    /// `ranThread` retired an instruction, and `effect` is the step's `StepEffect`, forwarded
    /// verbatim from `ExecutionResult.Stepped`.
    | InstructionStepped of
        program : RunningProgram *
        ranThread : ThreadId *
        whatWeDid : WhatWeDid *
        effect : StepEffect
    /// A worker thread's last frame returned.
    | WorkerTerminated of afterTermination : RunningProgram * terminatingThread : ThreadId
    /// The entry thread's `EntryFrameKind.StartupCall` returned, which ends a phase of startup
    /// rather than the process: `advanced` is the program in its next phase, or running `Main`
    /// if the phase that ended was the last.
    | StartupCallReturned of advanced : RunningProgram
    /// The process ended, as `outcome` says, and `ended` is the kernel's account of the end,
    /// which the driver takes to the machine the process ran on.
    | Ended of outcome : RunOutcome * ended : EndedProcess<ThreadId, NativeSignalHandler>

/// The operations on one program that the driver (`MultiProgram`) runs each tick: those that
/// stay with a program, as opposed to the machine-wide ones the driver runs itself. The other
/// per-program phase is `SignalDispatch.poll`.
///
/// Every one of them but `stepDecided` writes nothing through the program's view of the machine
/// (`EmulatedKernel.System`), and reads of it only the program's own process, which nothing but
/// the program's own steps changes: so the driver can run them on a program that is not checked
/// out, whose view of the machine is stale. A Debug build asserts the first half of that of each
/// operation that answers a state, by reference.
[<RequireQualifiedAccess>]
module internal RunningProgram =
    /// Fail loudly, naming `operation`, if `after`'s view of the machine is not `before`'s by
    /// reference: the operation wrote through the view, which on a program not checked out of
    /// its driver would be a write through a stale copy of the machine. Debug builds only.
    let inline private assertWroteNoView
        (operation : string)
        (before : IlMachineState)
        (after : IlMachineState)
        : unit
        =
#if DEBUG
        if not (obj.ReferenceEquals (before.Kernel.System, after.Kernel.System)) then
            failwith
                $"RunningProgram.%s{operation}: wrote the program's view of the machine, which the driver runs it without checking the program out for (this is an interpreter bug)."
#else
        ()
#endif

    /// Apply the spurious-wakeup strategies at `tick`, the driver's global tick. For the
    /// default (`Disabled`) strategy each application is a fold over the identity. The two
    /// layers (LowLevel and SyncBlock) are independent waiters on disjoint primitive types, so
    /// the order between them at a given tick is unobservable; LowLevel is applied first.
    let applySpuriousWakeups (tick : int64) (before : IlMachineState) : IlMachineState =
        let state =
            LowLevelMonitor.applySpuriousWakeups before.Kernel.SpuriousWakeup tick before

        let state =
            SyncBlockMonitor.applySpuriousWakeups state.Kernel.SyncBlockSpuriousWakeup tick state

        assertWroteNoView "applySpuriousWakeups" before state
        state

    /// Discriminator for a fired wait deadline: which subsystem owns the
    /// parked thread, and therefore which `fireTimeout` implementation
    /// must be called to wake it.
    ///
    /// Each subsystem reaches its waiters through a different queue and
    /// each has its own contract for how the optimistic park-time eval
    /// stack push is rewritten on timeout.
    [<RequireQualifiedAccess>]
    type private FiredDeadline =
        | WaitHandle of handle : WaitHandleId
        | MonitorWait of monitor : LowLevelMonitorId
        | SyncBlockWait of lockObject : ManagedHeapAddress
        /// `Monitor.TryEnter(obj, ms)` slowpath park on a contended
        /// SyncBlock with a finite positive timeout. The waiter sits on
        /// the SyncBlock's `AcquireQueue` (not its `WaitQueue`), and
        /// `SyncBlockMonitor.fireAcquireTimeout` dequeues + rewrites the
        /// optimistic `Int32 1` push to `Int32 0`.
        | SyncBlockAcquire of lockObject : ManagedHeapAddress
        /// `Thread.Join(int)` with a positive finite timeout. Carries no
        /// payload because there is no per-primitive wait queue to
        /// reference: the joiner is identified by the outer `(tid, kind)`
        /// pair, and `Scheduler.fireJoinTimeout` reads its state directly
        /// from the thread's status.
        | JoinTimeout
        /// `Thread.Sleep(int)` with a positive finite timeout. Carries no
        /// payload because sleep has no per-primitive wait queue and no
        /// optimistic eval-stack push to rewrite (Sleep returns `void`).
        /// `Scheduler.fireSleepTimeout` reads state directly from the
        /// thread's status and flips it back to `Runnable`.
        | SleepTimeout
        /// `WaitHandle.WaitAny` / `WaitAll` with a positive finite timeout.
        /// Carries no payload because the waiter sits on *several* queues at
        /// once, so no single primitive identifies it;
        /// `WaitHandle.fireMultipleTimeout` reads the handle list from the
        /// thread's status and dequeues it from every one of them.
        | WaitHandlesTimeout

    /// Project a thread status into its finite-timeout deadline against
    /// the virtual clock, if any. Threads with no deadline (Runnable,
    /// non-timed blocks, infinite waits) return `None`; threads parked
    /// with a finite timeout return `Some (kind, absoluteVirtualClockTicks)`.
    /// The `kind` is what tells the deadline-firing path which
    /// subsystem's fire function to invoke.
    let private waitDeadline (status : ThreadStatus) : (FiredDeadline * int64) option =
        match status with
        | ThreadStatus.BlockedOnWaitHandle (handle, Some deadline, _) ->
            Some (FiredDeadline.WaitHandle handle, deadline)
        | ThreadStatus.BlockedOnMonitorWait (monitor, Some deadline) ->
            Some (FiredDeadline.MonitorWait monitor, deadline)
        | ThreadStatus.BlockedOnSyncBlockWait (lockObject, Some deadline) ->
            Some (FiredDeadline.SyncBlockWait lockObject, deadline)
        | ThreadStatus.BlockedOnSyncBlockAcquire (lockObject, Some deadline) ->
            Some (FiredDeadline.SyncBlockAcquire lockObject, deadline)
        | ThreadStatus.BlockedOnJoin (_, Some deadline) -> Some (FiredDeadline.JoinTimeout, deadline)
        | ThreadStatus.BlockedOnSleep (Some deadline) -> Some (FiredDeadline.SleepTimeout, deadline)
        | ThreadStatus.BlockedOnWaitHandles (_, _, Some deadline) -> Some (FiredDeadline.WaitHandlesTimeout, deadline)
        | ThreadStatus.BlockedOnWaitHandles (_, _, None)
        | ThreadStatus.BlockedOnWaitHandle (_, None, _)
        | ThreadStatus.BlockedOnMonitorWait (_, None)
        | ThreadStatus.BlockedOnSyncBlockWait (_, None)
        | ThreadStatus.BlockedOnSyncBlockAcquire (_, None)
        | ThreadStatus.BlockedOnJoin (_, None)
        | ThreadStatus.BlockedOnSleep None
        // A call parked in the kernel keeps its deadline in its wake condition, where
        // `syscallDeadlines` reads it, and it fires through the driver's syscall wakes, which
        // ask the kernel. There is deliberately no `FiredDeadline` case for one.
        | ThreadStatus.BlockedInSyscall
        | ThreadStatus.Runnable
        | ThreadStatus.NotStarted _
        | ThreadStatus.BlockedOnClassInit _
        | ThreadStatus.BlockedOnMonitorAcquire _
        | ThreadStatus.Terminated
        | ThreadStatus.WaitingForForegroundThreads
        | ThreadStatus.Parked -> None

    /// Fire a timeout wake for every blocked-with-deadline thread whose
    /// deadline is `<= now`, a reading of the virtual clock in its ticks.
    ///
    /// `now` is an argument rather than read from the state's kernel because
    /// the clock is the machine's, and a process's view of a machine other
    /// processes share holds a stale copy of it once another has moved it on.
    /// Nothing here reads the kernel's POSIX half, so a state whose view is
    /// stale in that way may be passed with the machine's clock. Each fire routes
    /// through a per-subsystem fire function (WaitHandle dequeues from
    /// the handle's wait queue and rewrites `WAIT_OBJECT_0 → WAIT_TIMEOUT`;
    /// LowLevelMonitor moves the waiter from `WaitQueue` to `AcquireQueue`
    /// — granting ownership directly if the monitor is unowned — and
    /// rewrites `Int32 1 → Int32 0`; `SyncBlockMonitor.fireWaitTimeout`
    /// does the same against the managed-heap object's SyncBlock,
    /// preserving the snapshot reentrancy depth carried in `WaitQueue`;
    /// `SyncBlockMonitor.fireAcquireTimeout` dequeues a slowpath
    /// `TryEnter(obj, ms)` waiter from the SyncBlock's `AcquireQueue`
    /// and rewrites `Int32 1 → Int32 0` without changing ownership).
    ///
    /// Fire order matters for `LowLevelMonitor` and `SyncBlockMonitor`
    /// wait-timeout fires. When two waiters on the same primitive expire
    /// in the same tick, the fire grants ownership to whichever fires
    /// first against the unowned primitive, so the head of the owning
    /// primitive's queue fires first, matching the FIFO contract enforced
    /// everywhere else in the state machines (release, signalRelease,
    /// pulse/pulseAll, applySpuriousWakeups AlwaysAll).
    let fireExpiredDeadlines (now : int64) (state : IlMachineState) : IlMachineState =
        // `Map.foldBack` visits keys in descending order, so the resulting list is sorted by
        // thread ID.
        let expired =
            (state.ThreadState, [])
            ||> Map.foldBack (fun tid ts acc ->
                match waitDeadline ts.Status with
                | Some (kind, deadline) when deadline <= now -> (tid, kind) :: acc
                | _ -> acc
            )

        let monitorQueuePosition (LowLevelMonitorId mid as monitorId : LowLevelMonitorId) (thread : ThreadId) : int =
            let monitor = Map.find monitorId state.Kernel.LowLevelMonitors

            match List.tryFindIndex (fun t -> t = thread) monitor.WaitQueue with
            | Some i -> i
            | None ->
                failwith
                    $"fireExpiredDeadlines: thread %O{thread} has BlockedOnMonitorWait status against monitor #%i{mid} but is not in its WaitQueue %A{monitor.WaitQueue}; structural invariant violated."

        let syncBlockWaitQueuePosition (addr : ManagedHeapAddress) (thread : ThreadId) : int =
            let block = IlMachineState.getSyncBlock addr state

            match List.tryFindIndex (fun (t, _) -> t = thread) block.WaitQueue with
            | Some i -> i
            | None ->
                failwith
                    $"fireExpiredDeadlines: thread %O{thread} has BlockedOnSyncBlockWait status against object %O{addr} but is not in its WaitQueue %A{block.WaitQueue}; structural invariant violated."

        let syncBlockAcquireQueuePosition (addr : ManagedHeapAddress) (thread : ThreadId) : int =
            let block = IlMachineState.getSyncBlock addr state

            match block.Lock with
            | SyncBlockLock.Free ->
                failwith
                    $"fireExpiredDeadlines: thread %O{thread} has BlockedOnSyncBlockAcquire status against object %O{addr} but its SyncBlock is Free; structural invariant violated (acquire queue only exists when Held)."
            | SyncBlockLock.Held locked ->
                match List.tryFindIndex (fun (t, _) -> t = thread) locked.AcquireQueue with
                | Some i -> i
                | None ->
                    failwith
                        $"fireExpiredDeadlines: thread %O{thread} has BlockedOnSyncBlockAcquire status against object %O{addr} but is not in its AcquireQueue %A{locked.AcquireQueue}; structural invariant violated."

        // Iterating `state.ThreadState` (a Map keyed on ThreadId) would let a
        // later-parked waiter with a smaller thread id steal the lock from the
        // FIFO head, so entries are sorted by their position in the owning
        // primitive's `WaitQueue` (or `AcquireQueue`, for acquire-timeouts).
        // Cross-primitive ordering is irrelevant — each fire touches a
        // disjoint primitive/thread — so `WaitHandle` entries are ordered
        // last and by ThreadId, which is deterministic; order is
        // unobservable for that subsystem.
        // Queue positions are computed against the input state, before any
        // fires mutate `WaitQueue`s.
        //
        // Sort key: LowLevelMonitor entries first (group=0), then
        // SyncBlock wait entries (group=1), then SyncBlock acquire
        // entries (group=2), then WaitHandle entries (group=3), then
        // Join entries (group=4), then Sleep entries (group=5). Within
        // each subsystem-group, entries are keyed first by their
        // primitive id (so distinct primitives are ordered
        // deterministically but independently) and then by FIFO position
        // in the primitive's queue (so the head of any contested
        // primitive fires before its successors). For WaitHandle, queue
        // order is unobservable for timeout fires, so ThreadId is used
        // as a stable deterministic break. Join and Sleep have no
        // per-primitive queue (the "primitive" is the target thread's
        // status for Join, and the virtual clock itself for Sleep), so
        // ThreadId is the only deterministic break.
        let sortKey ((tid, kind) : ThreadId * FiredDeadline) : int * int * int =
            match kind with
            | FiredDeadline.MonitorWait monitorId ->
                let (LowLevelMonitorId mid) = monitorId
                0, mid, monitorQueuePosition monitorId tid
            | FiredDeadline.SyncBlockWait addr ->
                let (ManagedHeapAddress aid) = addr
                1, aid, syncBlockWaitQueuePosition addr tid
            | FiredDeadline.SyncBlockAcquire addr ->
                let (ManagedHeapAddress aid) = addr
                2, aid, syncBlockAcquireQueuePosition addr tid
            | FiredDeadline.WaitHandle handleId ->
                let (WaitHandleId hid) = handleId
                let (ThreadId t) = tid
                3, hid, t
            | FiredDeadline.JoinTimeout ->
                let (ThreadId t) = tid
                4, t, 0
            | FiredDeadline.SleepTimeout ->
                let (ThreadId t) = tid
                5, t, 0
            | FiredDeadline.WaitHandlesTimeout ->
                let (ThreadId t) = tid
                6, t, 0

        let expired = expired |> List.sortBy sortKey

        let fired =
            expired
            |> List.fold
                (fun s (tid, kind) ->
                    match kind with
                    | FiredDeadline.WaitHandle handleId -> WaitHandle.fireTimeout tid handleId s
                    | FiredDeadline.WaitHandlesTimeout -> WaitHandle.fireMultipleTimeout tid s
                    | FiredDeadline.MonitorWait monitorId -> LowLevelMonitor.fireTimeout tid monitorId s
                    | FiredDeadline.SyncBlockWait addr -> SyncBlockMonitor.fireWaitTimeout tid addr s
                    | FiredDeadline.SyncBlockAcquire addr -> SyncBlockMonitor.fireAcquireTimeout tid addr s
                    | FiredDeadline.JoinTimeout -> Scheduler.fireJoinTimeout tid s
                    | FiredDeadline.SleepTimeout -> Scheduler.fireSleepTimeout tid s
                )
                state

        assertWroteNoView "fireExpiredDeadlines" state fired
        fired

    /// Every deadline a thread parked in a syscall is waiting for, as the first
    /// tick of the virtual clock at or after it.
    let private syscallDeadlines (state : IlMachineState) : int64 list =
        match Scheduler.syscallWaiters state with
        | [] -> []
        | asleep ->
            UnixWait.deadlines (Set.ofList asleep) state.Kernel.System
            |> List.map ClockPal.firstTickAtOrAfter

    /// Every finite wait deadline the program has outstanding, in no particular order
    /// and with duplicates where two threads are parked on the same instant:
    /// the runtime-level waits' own deadlines, and those of the calls parked in
    /// the kernel. The candidate set `ClockJitterStrategy.EagerDeadlines` draws
    /// from, which is why it is the whole collection and not just the minimum.
    ///
    /// The kernel's deadlines are read from the program's own parks
    /// (`UnixWait.deadlines`), which are its process's and so current in its view
    /// whether or not the program is checked out.
    let pendingDeadlines (state : IlMachineState) : int64 list =
        let threadDeadlines =
            state.ThreadState
            |> Map.toList
            |> List.choose (fun (_, ts) -> waitDeadline ts.Status |> Option.map snd)

        threadDeadlines @ syscallDeadlines state

    /// Flip to `Runnable` each of `woken`, threads of this program's state that the machine's
    /// syscall wakes (`SimulatedMachine.wakes`) answered: what each was asleep for in the
    /// kernel has happened.
    ///
    /// Every park is re-entrant — the native frame stays and the caller's
    /// program counter still names the call — so waking is exactly a flip to
    /// `Runnable`, and the re-entered handler finishes the call from the caller's
    /// own frame.
    ///
    /// The signal dispatcher, asleep in its read of the signal pipe, is asked
    /// about with the rest (see `Scheduler.asleepInSyscall`) but never flipped:
    /// `SignalDispatch.poll` finishes its read at the next tick.
    let wakeFromSyscalls (woken : ThreadId list) (before : IlMachineState) : IlMachineState =
        let state =
            (before, woken)
            ||> List.fold (fun s tid ->
                match (Map.find tid s.ThreadState).Status with
                | ThreadStatus.Parked -> s
                | _ -> Scheduler.wakeFromSyscall tid s
            )

        assertWroteNoView "wakeFromSyscalls" before state
        state

    let private logStepOutcome
        (logger : ILogger)
        (state : IlMachineState)
        (thread : ThreadId)
        (whatWeDid : WhatWeDid)
        : unit
        =
        // Called once per interpreted IL instruction. `ActiveAssembly` is a by-name lookup over
        // the loaded assemblies, and the parameterised `LogTrace` overload boxes its argument
        // into an `obj[]` before the level is consulted, so both stay behind the check.
        if not (logger.IsEnabled LogLevel.Trace) then
            ()
        else

        match whatWeDid with
        | WhatWeDid.Executed ->
            logger.LogTrace (
                "Executed one step; active assembly: {ActiveAssembly}",
                state.ActiveAssembly(thread).Name.Name
            )
        | WhatWeDid.VoluntaryYield _ ->
            logger.LogTrace (
                "Executed one step (voluntary yield requested); active assembly: {ActiveAssembly}",
                state.ActiveAssembly(thread).Name.Name
            )
        | WhatWeDid.Aborted fatal ->
            logger.LogTrace (
                "Step aborted the process ({FatalErrorCode}): {FatalErrorMessage}",
                fatal.Code,
                (fatal.Message |> Option.defaultValue "<no message>")
            )
        | WhatWeDid.UnhandledException exn ->
            logger.LogTrace (
                "Step ended the thread with an unhandled exception at {ExceptionObject}",
                exn.ExceptionObject
            )
        | WhatWeDid.SuspendedForClassInit ->
            logger.LogTrace "Suspended execution of current method for class initialisation."
        | WhatWeDid.SuspendedForManagedCall ->
            logger.LogTrace "Suspended execution of native handler for a managed call continuation."
        | WhatWeDid.BlockedOnClassInit _ -> logger.LogTrace "Unable to execute because class has not yet initialised."
        | WhatWeDid.ThrowingTypeInitializationException ->
            logger.LogTrace "TypeInitializationException dispatched due to failed .cctor."

    /// The run's end when a signal killed the process: `ended` is the kernel's answer to
    /// the signal, and `state` the machine as it stood when the signal was sent.
    let signalTerminated (state : IlMachineState) (ended : EndedProcess<ThreadId, NativeSignalHandler>) : RunOutcome =
        match ended.Termination with
        | ProcessTermination.Signaled (signal, coreDumped) -> RunOutcome.SignalTerminated (state, signal, coreDumped)
        | ProcessTermination.Exited _ as other ->
            failwith
                $"Program: a signal was reported to have killed the process, but the kernel ended it by %O{other} (this is an interpreter bug)."

    /// How a startup call that was pumped to completion ended, as the tail of a sentence
    /// naming what was being run: "Seeding AppContext <this>."
    ///
    /// By case rather than with `%O`: every `RunOutcome` carries an `IlMachineState`, so
    /// structural formatting would render the entire heap into the exception message.
    let private describeStartupOutcome (outcome : RunOutcome) : string =
        match outcome with
        | RunOutcome.NormalExit _ -> "exited normally"
        | RunOutcome.ProcessExit (_, thread, _) -> $"called Environment.Exit on %O{thread}"
        | RunOutcome.Aborted (_, thread, fatal, _) ->
            let message = fatal.Message |> Option.defaultValue "<no message>"
            $"aborted on %O{thread} with %O{fatal.Code}: %s{message}"
        | RunOutcome.SignalTerminated (_, signal, coreDumped) ->
            if coreDumped then
                $"was terminated by signal %O{signal} (core dumped)"
            else
                $"was terminated by signal %O{signal}"
        | RunOutcome.GuestUnhandledException (finalState, thread, exn, _) ->
            $"threw an unhandled exception on %O{thread}:\n%s{UnhandledExceptionReport.describe finalState exn}"

    /// How `program`'s run ended, given that its process ended as `outcome` says while
    /// `program`'s entry thread was running what `program.EntryFrame` says.
    ///
    /// Fails loudly if the process ended while startup was seeding AppContext or installing the
    /// command line. Neither `AppContext.Setup` nor `InitializeCommandLineArgs` can legitimately
    /// exit, fail fast or throw: each allocates and copies strings out of buffers PawPrint
    /// itself just wrote. Anything else means a `.cctor` dragged in by that work misbehaved,
    /// and the run is not one a host should be handed as the guest's.
    let runEnd (program : RunningProgram) (outcome : RunOutcome) : RunEnd =
        match program.EntryFrame with
        | EntryFrameKind.StartupCall (StartupPhase.InitialisingCommandLine _) ->
            failwith $"Installing the guest's command line %s{describeStartupOutcome outcome}."
        | EntryFrameKind.StartupCall (StartupPhase.SeedingAppContext _) ->
            failwith $"Seeding AppContext %s{describeStartupOutcome outcome}."
        // The entry thread's `.cctor` raised, or a worker spawned during cctor pumping exited,
        // failed fast, or took a terminating signal: the CLR tears the process down, and the
        // guest-level diagnostic is the run's end, before `Main` ever ran.
        | EntryFrameKind.StartupCall (StartupPhase.InitialisingClasses _)
        | EntryFrameKind.Main _ -> RunEnd.Ended outcome

    /// True iff the process waits for `thread` before it can exit: what CoreCLR's
    /// `ThreadStore::OtherThreadsComplete` counts, a foreground thread that has been started and
    /// has not finished.
    let private holdsProcessOpen (thread : ThreadState) : bool =
        not thread.IsBackground && ThreadStatus.keepsProcessAlive thread.Status

    /// How the process ends when the runtime aborts it on `thread`: CoreCLR's `PROCAbort`
    /// ends in `abort()`, which the kernel answers with a death by SIGABRT.
    let private abortTermination
        (thread : ThreadId)
        (state : IlMachineState)
        : EndedProcess<ThreadId, NativeSignalHandler>
        =
        EmulatedKernel.abort thread state.Kernel

    /// `RunMain`'s `SetLatchedExitCode(*piRetVal)`: the moment an `int Main` returns, its return
    /// value — which its `ret` left as the only value on the entry thread's eval stack — becomes
    /// the latched exit code. A `void Main` latches nothing.
    ///
    /// The eval stack is exactly what the signature says by the time this runs:
    /// `returnStackFrame` has already refused, as invalid CIL, a `Main` that returned with any
    /// other number of values on it.
    let private latchMainReturnValue
        (returns : MainReturn)
        (entry : ThreadId)
        (state : IlMachineState)
        : IlMachineState
        =
        match returns, state.ThreadState.[entry].MethodState.EvaluationStack.Values with
        | MainReturn.Void, [] -> state
        | MainReturn.Int32, [ EvalStackValue.Int32 (Int32Source.Verbatim code) ] ->
            { state with
                LatchedExitCode = code
            }
        | MainReturn.Int32, [ other ] ->
            failwith
                $"an int Main returned %O{other}, which is not a verbatim int32; PawPrint cannot report it as an exit code"
        | MainReturn.Void, stack
        | MainReturn.Int32, stack ->
            failwith
                $"logic error: Main (%O{returns}) returned with %d{List.length stack} values on its eval stack, which returnStackFrame should have refused as invalid CIL"

    /// Finish a tick that did not end the run by itself: CoreCLR's `CheckForEEShutdown`, then
    /// the exit `WaitForOtherThreads` is waiting for.
    ///
    /// `CheckForEEShutdown` runs whenever a component of `OtherThreadsComplete` changes — a
    /// thread dying, a thread flipping to background — and latches `shutdownSignalled` if no
    /// foreground thread is alive. It is run here after every continuing outcome rather than
    /// only where those changes are known to happen, so that no handler that makes one can
    /// forget it; the latch only ever goes one way, so checking more often than CoreCLR does
    /// changes nothing. While `Main` is running and the entry thread is itself foreground, it
    /// holds the process open on its own and the tick costs one map lookup, no scan.
    ///
    /// Once the entry thread is `WaitingForForegroundThreads` and the latch is set, the run
    /// ends with `NormalExit`; otherwise `continuing` builds the tick's outcome from the
    /// program with the latch brought up to date — which is `program` itself unless the latch
    /// has just flipped, so the usual tick allocates nothing here. Inlined for the same reason
    /// `MultiProgram.annotating` is: this runs once per interpreted instruction.
    let inline private afterStep
        (program : RunningProgram)
        ([<InlineIfLambda>] continuing : RunningProgram -> ProgramTick)
        : ProgramTick
        =
        match program.EntryFrame with
        | EntryFrameKind.StartupCall _ -> continuing program
        | EntryFrameKind.Main (returns, shutdownSignalled) ->

        let entry = program.State.ThreadState.[program.EntryThread]

        let nowSignalled =
            shutdownSignalled
            || (not (holdsProcessOpen entry)
                && not (program.State.ThreadState |> Map.exists (fun _ ts -> holdsProcessOpen ts)))

        match entry.Status with
        | ThreadStatus.WaitingForForegroundThreads when nowSignalled ->
            // The host passes the latched exit code to `exit`, which ends in `exit_group`.
            let ended =
                EmulatedKernel.exitGroup program.EntryThread program.State.LatchedExitCode program.State.Kernel

            ProgramTick.Ended (RunOutcome.NormalExit (program.State, program.EntryThread, ended.Termination), ended)
        | _ ->
            // The latch goes one way, so this rebuilds the program at most once per run; every
            // other tick hands `program` on as it is.
            if nowSignalled = shutdownSignalled then
                continuing program
            else
                continuing
                    { program with
                        EntryFrame = EntryFrameKind.Main (returns, nowSignalled)
                    }

    /// The program's half of a scheduler tick: ask the policy which thread runs next, run it,
    /// and fold the outcome back into the thread states. `program` must already have been
    /// through the driver's preamble (`MultiProgram.advance`), which leaves it with a Runnable
    /// thread; running this against an inter-tick value would consult the policy about a
    /// Runnable set that a deadline or a spurious wake was about to change.
    let stepDecided (loggerFactory : ILoggerFactory) (logger : ILogger) (program : RunningProgram) : ProgramTick =
        let scheduledState, scheduledChoice =
            Scheduler.chooseNext program.LastRan program.State

        // Adopt the scheduler-updated state before stepping so that any RNG
        // advancement the policy performed is reflected in the run-forward
        // state — otherwise replaying the same seed would diverge on the
        // first stochastic decision.
        let program =
            { program with
                State = scheduledState
            }

        match scheduledChoice with
        | None ->
            failwith
                "RunningProgram.stepDecided: the scheduler found no Runnable thread, but the driver chose this program because it had one (this is an interpreter bug)."
        | Some nextThread ->
            // `nextThread` has now retired a step, and that is true of *every* outcome below —
            // including the ones that do not look like ordinary progress: a thread's final
            // `Ret` arrives as `Terminated`, and the entry thread's synthetic `onlyRet` frame
            // arrives as a `NormalExit` that the pre-`Main` pump then continues past. So the
            // per-step scheduler bookkeeping that holds regardless of outcome is applied here,
            // once, before we look at which outcome we got.
            //
            // Doing it here rather than in the individual arms is what makes it hard to get
            // wrong: `mapState` is exhaustive over `ExecutionResult`, so a new outcome cannot
            // quietly skip it. Outcome-*specific* consequences still belong in the arms, via
            // `Scheduler.onStepOutcome` and `Scheduler.onThreadTerminated`.
            let stepResult =
                AbstractMachine.executeOneStep loggerFactory program.BaseClassTypes program.State nextThread
                |> ExecutionResult.mapState (Scheduler.dischargeYieldDebts nextThread)

            match stepResult with
            | ExecutionResult.Terminated (state, terminatingThread) ->
                if terminatingThread = program.EntryThread then
                    match program.EntryFrame with
                    | EntryFrameKind.StartupCall phase ->
                        // The pumped call is done, which ends a phase of startup rather than the
                        // process, whatever the other threads are doing. The entry thread is
                        // deliberately not marked Terminated — it is about to get its next frame,
                        // ultimately `Main` — because a worker that joined it during a `.cctor`
                        // must not observe a false end-of-thread and proceed past its Join before
                        // `Main` has started.
                        match phase with
                        | StartupPhase.SeedingAppContext onReturn
                        | StartupPhase.InitialisingCommandLine onReturn ->
                            let state, next = onReturn state

                            ProgramTick.StartupCallReturned
                                { program with
                                    State = state
                                    EntryFrame = EntryFrameKind.StartupCall next
                                }
                        | StartupPhase.InitialisingClasses (installMain, returns) ->
                            ProgramTick.StartupCallReturned
                                { program with
                                    State = installMain state
                                    // Nothing can have signalled shutdown yet: the entry thread is
                                    // about to run `Main` as a foreground thread, and only `Main`
                                    // arms the latch.
                                    EntryFrame = EntryFrameKind.Main (returns, false)
                                    LastRan = program.EntryThread
                                }
                    | EntryFrameKind.Main (returns, _) ->
                        // `Main` has returned. Its return value, if it has one, is latched as the
                        // exit code now, and the entry thread keeps its final frame and waits for
                        // the other foreground threads; the run ends below if there are none.
                        logger.LogDebug (
                            "Main returned on {Thread}; the process now waits for its other foreground threads",
                            program.EntryThread
                        )

                        let state =
                            state
                            |> latchMainReturnValue returns program.EntryThread
                            |> Scheduler.onMainReturned program.EntryThread

                        // The `ret` retired a step, reported here as `WhatWeDid.Executed`, so it
                        // gets that outcome's consequences — as the dispatcher's final `ret`
                        // does below.
                        let state = Scheduler.onStepOutcome program.EntryThread WhatWeDid.Executed state

                        let program =
                            { program with
                                State = state
                                LastRan = program.EntryThread
                            }

                        afterStep
                            program
                            (fun program ->
                                ProgramTick.InstructionStepped (
                                    program,
                                    program.EntryThread,
                                    WhatWeDid.Executed,
                                    // `ExecutionResult.Terminated` carries no effect: a `ret`
                                    // performs no I/O of its own.
                                    StepEffect.NoEffect
                                )
                            )
                elif PosixSignalShim.signalThread state.Kernel.PosixSignalShim = Some terminatingThread then
                    // The shim's signal-dispatch thread's handler frame
                    // has returned past its bottom; `Ret` surfaces that as a
                    // `Terminated` outcome because the bottom frame has no
                    // `ReturnState`. Reset the dispatcher to its idle Parked
                    // shape so the next deliverable signal can wake it again,
                    // and let the loop continue: this thread isn't *really*
                    // terminated, the dispatcher is just between handler
                    // invocations.
                    match SignalDispatch.reParkAfterHandler terminatingThread state with
                    | SignalPoll.ProcessKilled (state, ended) ->
                        // The callback reported the signal unhandled, and the loop's
                        // `SystemNative_HandleNonCanceledPosixSignal` re-raised it at a
                        // default that kills the process.
                        ProgramTick.Ended (signalTerminated state ended, ended)
                    | SignalPoll.Continues state ->

                    // The dispatcher retired a step and this branch reports it as
                    // `WhatWeDid.Executed`, so give it that outcome's consequences — waking
                    // anything parked BlockedOnClassInit behind it. (The yield-debt half of
                    // the bookkeeping has already happened, in the discharge above.)
                    let state = Scheduler.onStepOutcome terminatingThread WhatWeDid.Executed state

                    let program =
                        { program with
                            State = state
                            LastRan = terminatingThread
                        }

                    afterStep
                        program
                        (fun program ->
                            ProgramTick.InstructionStepped (
                                program,
                                terminatingThread,
                                WhatWeDid.Executed,
                                // The signal dispatcher's handler frame returning past its
                                // bottom arrives as `ExecutionResult.Terminated`, which carries
                                // no effect: the step performed no I/O of its own.
                                StepEffect.NoEffect
                            )
                        )
                else
                    let state = Scheduler.onThreadTerminated terminatingThread state

                    let program =
                        { program with
                            State = state
                            LastRan = terminatingThread
                        }

                    afterStep program (fun program -> ProgramTick.WorkerTerminated (program, terminatingThread))
            | ExecutionResult.ProcessExit (state, exitingThread) ->
                // `Environment.Exit` passes the latched exit code to `exit`, which ends in
                // `exit_group`.
                let ended =
                    EmulatedKernel.exitGroup exitingThread state.LatchedExitCode state.Kernel

                ProgramTick.Ended (RunOutcome.ProcessExit (state, exitingThread, ended.Termination), ended)
            | ExecutionResult.Aborted (state, abortingThread, message) ->
                let ended = abortTermination abortingThread state
                ProgramTick.Ended (RunOutcome.Aborted (state, abortingThread, message, ended.Termination), ended)
            | ExecutionResult.SignalTerminated (state, ended) -> ProgramTick.Ended (signalTerminated state ended, ended)
            | ExecutionResult.UnhandledException (state, terminatingThread, exn) ->
                let ended = abortTermination terminatingThread state

                ProgramTick.Ended (
                    RunOutcome.GuestUnhandledException (state, terminatingThread, exn, ended.Termination),
                    ended
                )
            | ExecutionResult.Stepped (state, whatWeDid, effect) ->
                logStepOutcome logger state nextThread whatWeDid

                let state = Scheduler.onStepOutcome nextThread whatWeDid state

                let program =
                    { program with
                        State = state
                        LastRan = nextThread
                    }

                afterStep
                    program
                    (fun program -> ProgramTick.InstructionStepped (program, nextThread, whatWeDid, effect))

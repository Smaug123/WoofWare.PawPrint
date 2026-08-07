namespace WoofWare.PawPrint

open System
open System.Collections.Immutable
open System.IO
open System.Reflection.Metadata
open Microsoft.Extensions.Logging

[<RequireQualifiedAccess>]
module Program =
    /// Returns the pointer to the resulting array on the heap.
    let allocateArgs
        (loggerFactory : ILoggerFactory)
        (args : string list)
        (corelib : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        : ManagedHeapAddress * IlMachineState
        =
        let state, stringType =
            DumpedAssembly.typeInfoToTypeDefn' corelib state._LoadedAssemblies corelib.String
            |> IlMachineState.concretizeType
                loggerFactory
                corelib
                state
                corelib.Corelib.Name
                ImmutableArray.Empty
                ImmutableArray.Empty

        let argsAllocations, state =
            (state, args)
            ||> Seq.mapFold (fun state arg ->
                IlMachineRuntimeMetadata.allocateManagedString loggerFactory corelib arg state
            )

        let stringArrayType = ConcreteTypeHandle.OneDimArrayZero stringType

        let arrayAllocation, state =
            IlMachineState.allocateArray stringArrayType (fun () -> CliType.ObjectRef None) args.Length state

        let state =
            ((state, 0), argsAllocations)
            ||> Seq.fold (fun (state, i) arg ->
                let state =
                    IlMachineState.setArrayValue arrayAllocation (CliType.ofManagedObject arg) i state

                state, i + 1
            )
            |> fst

        arrayAllocation, state

    type PreparedProgram =
        {
            State : IlMachineState
            BaseClassTypes : BaseClassTypes<DumpedAssembly>
            EntryThread : ThreadId
            LastRan : ThreadId
        }

    type ProgramStartResult =
        | Ready of PreparedProgram
        | CompletedBeforeMain of RunOutcome

    type ProgramStepOutcome =
        | InstructionStepped of PreparedProgram * ranThread : ThreadId * whatWeDid : WhatWeDid
        | WorkerTerminated of PreparedProgram * terminatingThread : ThreadId
        | Completed of RunOutcome
        | Deadlocked of PreparedProgram * stuckThreads : string

    let private deadlockDescription (state : IlMachineState) : string =
        state.ThreadState
        |> Map.toSeq
        |> Seq.filter (fun (_, ts) -> ts.Status <> ThreadStatus.Terminated)
        |> Seq.map (fun (ThreadId i, ts) -> $"thread {i} in state {ts.Status}")
        |> String.concat "; "

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
    /// with a finite timeout return `Some (kind, absoluteVirtualClockMs)`.
    /// The `kind` is what tells the deadline-firing path which
    /// subsystem's fire function to invoke.
    let private waitDeadline (status : ThreadStatus) : (FiredDeadline * int64) option =
        match status with
        | ThreadStatus.BlockedOnWaitHandle (handle, Some deadline) -> Some (FiredDeadline.WaitHandle handle, deadline)
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
        | ThreadStatus.BlockedOnWaitHandle (_, None)
        | ThreadStatus.BlockedOnMonitorWait (_, None)
        | ThreadStatus.BlockedOnSyncBlockWait (_, None)
        | ThreadStatus.BlockedOnSyncBlockAcquire (_, None)
        | ThreadStatus.BlockedOnJoin (_, None)
        | ThreadStatus.BlockedOnSleep None
        | ThreadStatus.Runnable
        | ThreadStatus.NotStarted
        | ThreadStatus.BlockedOnClassInit _
        | ThreadStatus.BlockedOnMonitorAcquire _
        | ThreadStatus.Terminated
        | ThreadStatus.Parked -> None

    /// Fire a timeout wake for every blocked-with-deadline thread whose
    /// deadline is `<= state.Kernel.VirtualClockMs`. Each fire routes
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
    /// first against the unowned primitive — so iterating
    /// `state.ThreadState` (a Map keyed on ThreadId) would let a
    /// later-parked waiter with a smaller thread id steal the lock from
    /// the FIFO head. We sort entries by their position in the owning
    /// primitive's `WaitQueue` (or `AcquireQueue`, for acquire-timeouts)
    /// so that the head fires first, matching the FIFO contract enforced
    /// everywhere else in the state machines (release, signalRelease,
    /// pulse/pulseAll, applySpuriousWakeups AlwaysAll).
    ///
    /// Cross-primitive ordering is irrelevant — each fire touches a
    /// disjoint primitive/thread — so `WaitHandle` entries are ordered
    /// last and by ThreadId, which is deterministic and matches the
    /// pre-fix behaviour for the subsystem where order is unobservable.
    /// Queue positions are computed against the input state, before any
    /// fires mutate `WaitQueue`s.
    let private fireExpiredDeadlines (state : IlMachineState) : IlMachineState =
        let now = state.Kernel.VirtualClockMs

        let expired =
            state.ThreadState
            |> Map.toSeq
            |> Seq.choose (fun (tid, ts) ->
                match waitDeadline ts.Status with
                | Some (kind, deadline) when deadline <= now -> Some (tid, kind)
                | _ -> None
            )
            |> Seq.toList

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

    /// The minimum wait deadline among currently-blocked threads, or
    /// `None` if no thread is parked with a finite timeout. Used by the
    /// driver loop's jump-to-deadline fallback: if no thread is Runnable
    /// but at least one has a finite-timeout wait outstanding, advance
    /// `VirtualClockMs` to the nearest such deadline so the wait can
    /// resolve on the next pass.
    ///
    /// The clock-jump must not bump `StepCounter` — the spurious-wakeup
    /// schedules are keyed on `StepCounter`, and a jump-driven tick is
    /// deliberately *not* a real scheduler tick. Keeping the two clocks
    /// separate is exactly why `VirtualClockMs` is its own field rather
    /// than derived from `StepCounter`.
    let private nextDeadline (state : IlMachineState) : int64 option =
        state.ThreadState
        |> Map.toSeq
        |> Seq.choose (fun (_, ts) -> waitDeadline ts.Status |> Option.map snd)
        |> Seq.fold
            (fun acc d ->
                match acc with
                | None -> Some d
                | Some a -> Some (min a d)
            )
            None

    let private logStepOutcome
        (logger : ILogger)
        (state : IlMachineState)
        (thread : ThreadId)
        (whatWeDid : WhatWeDid)
        : unit
        =
        match whatWeDid with
        | WhatWeDid.Executed ->
            logger.LogTrace (
                "Executed one step; active assembly: {ActiveAssembly}",
                state.ActiveAssembly(thread).Name.Name
            )
        | WhatWeDid.VoluntaryYield ->
            logger.LogTrace (
                "Executed one step (voluntary yield requested); active assembly: {ActiveAssembly}",
                state.ActiveAssembly(thread).Name.Name
            )
        | WhatWeDid.SuspendedForClassInit ->
            logger.LogTrace "Suspended execution of current method for class initialisation."
        | WhatWeDid.SuspendedForManagedCall ->
            logger.LogTrace "Suspended execution of native handler for a managed call continuation."
        | WhatWeDid.BlockedOnClassInit _ -> logger.LogTrace "Unable to execute because class has not yet initialised."
        | WhatWeDid.ThrowingTypeInitializationException ->
            logger.LogTrace "TypeInitializationException dispatched due to failed .cctor."

    let stepPrepared
        (loggerFactory : ILoggerFactory)
        (logger : ILogger)
        (prepared : PreparedProgram)
        : ProgramStepOutcome
        =
        // The stepper reports NormalExit as soon as `EntryThread` Terminates, regardless
        // of whether other threads are still Runnable or Blocked. This matches both
        // use sites:
        //   * Pre-Main cctor pump: the synthetic onlyRet frame has returned, which
        //     means class initialisation is done. The entry thread isn't actually
        //     finished — Program.run is about to resurrect it with the real Main
        //     frame — so we deliberately do NOT mark it Terminated, because doing so
        //     would let a worker that joined the entry thread during a .cctor observe
        //     a false end-of-thread and proceed past the Join before Main has started.
        //   * Post-Main pump: when Main returns, we report NormalExit immediately
        //     rather than waiting for foreground threads. The test comparison oracles
        //     in WoofWare.PawPrint.Test just invoke `assy.EntryPoint.Invoke` via
        //     reflection, which also returns as soon as Main returns without waiting
        //     for foreground workers, so matching that behaviour keeps PawPrint and
        //     the oracle aligned. Environment.Exit from a worker still propagates as
        //     ProcessExit (handled below) before Main has a chance to return.

        // Apply the spurious-wakeup strategies at the current tick, then
        // advance the counter so the next iteration sees a fresh tick.
        // For the default (`Disabled`) strategy each application is a fold
        // over the identity and a single integer add — bit-for-bit
        // identical to pre-feature behaviour. The two layers (LowLevel and
        // SyncBlock) are independent waiters on disjoint primitive types,
        // so the order between them at a given tick is not load-bearing;
        // we apply LowLevel first for parity with the pre-SyncBlock
        // codepath.
        let state =
            LowLevelMonitor.applySpuriousWakeups
                prepared.State.Kernel.SpuriousWakeup
                prepared.State.Kernel.StepCounter
                prepared.State

        let state =
            SyncBlockMonitor.applySpuriousWakeups state.Kernel.SyncBlockSpuriousWakeup state.Kernel.StepCounter state

        let prepared =
            { prepared with
                State =
                    state.MapKernel (fun kernel ->
                        { kernel with
                            StepCounter = kernel.StepCounter + 1L
                            // One wall-clock millisecond per scheduler
                            // tick — see `EmulatedKernel.VirtualClockMs`
                            // for why the rate is "very slow computer"
                            // by realism standards but bit-for-bit
                            // deterministic. Bumping in lock-step with
                            // `StepCounter` keeps both clocks pure
                            // functions of "how many scheduler ticks
                            // have elapsed", which is what tests rely
                            // on when driving the strategies without a
                            // real driver.
                            VirtualClockMs = kernel.VirtualClockMs + 1L
                        }
                    )
            }

        // After advancing `VirtualClockMs`, fire any wait deadlines that
        // are now in the past. This runs every tick (not just on
        // deadlock) so a timeout against a thread holding a release lock
        // can still expire while other threads make progress: e.g.
        // thread A is parked with a 50 ms timeout on a semaphore, and
        // thread B is busy computing something else — A's deadline still
        // fires when the clock reaches it, even though B keeps the
        // scheduler from ever stalling.
        let prepared =
            { prepared with
                State = fireExpiredDeadlines prepared.State
            }

        // Drive the signal-dispatcher state machine before the scheduler
        // picks its next thread. If a pending signal is deliverable and
        // the dispatcher is currently Parked, this flips it to Runnable
        // and installs a handler-invocation bottom frame; the scheduler
        // then picks the dispatcher up on the same tick. If nothing is
        // deliverable, this is a no-op and the next tick re-polls.
        let prepared =
            { prepared with
                State = SignalDispatch.trySpawnHandler prepared.BaseClassTypes prepared.State
            }

        // Jump-to-deadline fallback: if no thread is Runnable but at
        // least one is parked with a finite-timeout wait outstanding,
        // advance `VirtualClockMs` to the nearest pending deadline and
        // fire it. This is what keeps a guest like `WaitOne(50)` against
        // an unsignalled handle from deadlocking — without the jump, the
        // clock would advance 1 ms per tick *only when there's something
        // to step*, but there is nothing to step.
        //
        // The fallback loops because a single fire may not make any
        // thread Runnable: `LowLevelMonitor.fireTimeout` moves a waiter
        // out of `WaitQueue`, but if the monitor is still owned by a
        // separate thread (which itself may be parked on a *later*
        // deadline), the waiter becomes `BlockedOnMonitorAcquire` rather
        // than `Runnable`. Stopping after one jump in that shape would
        // declare deadlock even though the owner's later finite wait can
        // still resolve and release the monitor. Each iteration either
        // produces a Runnable thread (terminating the loop) or strictly
        // advances `VirtualClockMs` to the next outstanding deadline; the
        // set of finite-deadline threads is finite and monotonically
        // shrinks (no fire creates a new deadline), so the loop
        // terminates.
        //
        // Only `VirtualClockMs` is advanced (not `StepCounter`), so the
        // spurious-wakeup schedule is untouched. A jump-driven wake is
        // not a scheduler tick — it is the resolution of a timeout that
        // would otherwise be invisible.
        let rec advanceUntilRunnableOrQuiescent (state : IlMachineState) : IlMachineState =
            // Use the policy-independent existence check rather than calling
            // `chooseNext` and discarding its returned state: a stochastic
            // policy would otherwise advance its RNG once per deadline-jump
            // probe, perturbing the scheduling stream without ever observing
            // the result.
            if Scheduler.hasAnyRunnable state then
                state
            else
                match nextDeadline state with
                | None -> state
                | Some target ->
                    let state =
                        state.MapKernel (fun kernel ->
                            { kernel with
                                VirtualClockMs = max kernel.VirtualClockMs target
                            }
                        )

                    advanceUntilRunnableOrQuiescent (fireExpiredDeadlines state)

        let prepared =
            { prepared with
                State = advanceUntilRunnableOrQuiescent prepared.State
            }

        let scheduledState, scheduledChoice =
            Scheduler.chooseNext prepared.LastRan prepared.State

        // Adopt the scheduler-updated state before stepping so that any RNG
        // advancement the policy performed is reflected in the run-forward
        // state — otherwise replaying the same seed would diverge on the
        // first stochastic decision.
        let prepared =
            { prepared with
                State = scheduledState
            }

        match scheduledChoice with
        | None ->
            // No Runnable threads and the entry thread didn't hit its ret. Every
            // remaining thread is blocked, so progress is impossible.
            ProgramStepOutcome.Deadlocked (prepared, deadlockDescription prepared.State)
        | Some nextThread ->
            match AbstractMachine.executeOneStep loggerFactory prepared.BaseClassTypes prepared.State nextThread with
            | ExecutionResult.Terminated (state, terminatingThread) ->
                if terminatingThread = prepared.EntryThread then
                    ProgramStepOutcome.Completed (RunOutcome.NormalExit (state, prepared.EntryThread))
                elif SignalState.signalThread state.Kernel.Signals = Some terminatingThread then
                    // The kernel-owned signal-dispatch thread's handler frame
                    // has returned past its bottom; `Ret` surfaces that as a
                    // `Terminated` outcome because the bottom frame has no
                    // `ReturnState`. Reset the dispatcher to its idle Parked
                    // shape so the next deliverable signal can wake it again,
                    // and let the loop continue: this thread isn't *really*
                    // terminated, the dispatcher is just between handler
                    // invocations.
                    let state = SignalDispatch.reParkAfterHandler terminatingThread state

                    ProgramStepOutcome.InstructionStepped (
                        { prepared with
                            State = state
                            LastRan = terminatingThread
                        },
                        terminatingThread,
                        WhatWeDid.Executed
                    )
                else
                    let state = Scheduler.onThreadTerminated terminatingThread state

                    ProgramStepOutcome.WorkerTerminated (
                        { prepared with
                            State = state
                            LastRan = terminatingThread
                        },
                        terminatingThread
                    )
            | ExecutionResult.ProcessExit (state, exitingThread) ->
                ProgramStepOutcome.Completed (RunOutcome.ProcessExit (state, exitingThread))
            | ExecutionResult.FailFast (state, abortingThread, message) ->
                ProgramStepOutcome.Completed (RunOutcome.FailFast (state, abortingThread, message))
            | ExecutionResult.SignalTerminated (state, signal) ->
                ProgramStepOutcome.Completed (RunOutcome.SignalTerminated (state, signal))
            | ExecutionResult.UnhandledException (state, terminatingThread, exn) ->
                ProgramStepOutcome.Completed (RunOutcome.GuestUnhandledException (state, terminatingThread, exn))
            | ExecutionResult.Stepped (state, whatWeDid, _) ->
                logStepOutcome logger state nextThread whatWeDid

                let state = Scheduler.onStepOutcome nextThread whatWeDid state

                ProgramStepOutcome.InstructionStepped (
                    { prepared with
                        State = state
                        LastRan = nextThread
                    },
                    nextThread,
                    whatWeDid
                )

    let rec pumpPrepared (loggerFactory : ILoggerFactory) (logger : ILogger) (prepared : PreparedProgram) : RunOutcome =
        match stepPrepared loggerFactory logger prepared with
        | ProgramStepOutcome.Completed outcome -> outcome
        | ProgramStepOutcome.Deadlocked (_, stuck) ->
            failwith $"Deadlock: no runnable threads and entry thread has not terminated. Stuck: {stuck}"
        | ProgramStepOutcome.InstructionStepped (prepared, _, _)
        | ProgramStepOutcome.WorkerTerminated (prepared, _) -> pumpPrepared loggerFactory logger prepared

    let internal pumpToReturn
        (loggerFactory : ILoggerFactory)
        (logger : ILogger)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (entryThread : ThreadId)
        (state : IlMachineState)
        : RunOutcome
        =
        let prepared =
            {
                State = state
                BaseClassTypes = baseClassTypes
                EntryThread = entryThread
                LastRan = entryThread
            }

        pumpPrepared loggerFactory logger prepared

    /// Reads the guest assembly and performs the one-time setup needed before Main is ready to schedule.
    ///
    /// `kernelConfig` carries the host's choices for the simulated process's kernel and is
    /// applied here rather than by the caller afterwards, because this function pumps the entry
    /// type's `.cctor` and CoreLib latches some of these values during static initialisation
    /// (notably `Environment.ProcessorCount`). `KernelConfig.Default` is the no-preference
    /// choice. Its `Environment` is overlaid on top of `EmulatedKernel.defaultEnvironment`, so
    /// callers that supply no overlay still get the seeded
    /// `DOTNET_SYSTEM_GLOBALIZATION_INVARIANT=1` default, and keys the caller does set win over
    /// it — that's how the CLI lets the host process override the seed if it really needs to.
    ///
    /// `pctSeed = Some s` selects the PCT scheduling policy seeded with `s`; `None` keeps the
    /// default round-robin policy. Applied before any cctor frame is pushed so the very first
    /// `chooseNext` decision is policy-correct — `IlMachineState.initial` defaults the field
    /// to `RoundRobin`, and `withPctSeed` simply overwrites it.
    let prepare
        (loggerFactory : ILoggerFactory)
        (originalPath : string option)
        (fileStream : Stream)
        (hostConfig : HostConfig)
        : ProgramStartResult
        =
        let logger = loggerFactory.CreateLogger "Program"
        let dotnetRuntimeDirs = hostConfig.DotnetRuntimeDirs
        let kernelConfig = hostConfig.Kernel
        let pctSeed = hostConfig.PctSeed
        let argv = hostConfig.Argv

        let dumped = Assembly.read loggerFactory originalPath fileStream

        let entryPoint =
            match dumped.MainMethod with
            | None -> failwith "No entry point in input DLL"
            | Some d -> d

        let mainMethodFromMetadata = dumped.Methods.[entryPoint]

        if mainMethodFromMetadata.Signature.GenericParameterCount > 0 then
            failwith "Refusing to execute generic main method"

        let mainTakesStringArrayArg =
            match mainMethodFromMetadata.Signature.ParameterTypes |> Seq.toList with
            | [] -> false
            | [ TypeDefn.OneDimensionalArrayLowerBoundZero (TypeDefn.PrimitiveType PrimitiveType.String) ] -> true
            | _ ->
                failwith
                    "Main method must take no parameters or a single string[]; other signatures not yet implemented"

        match mainMethodFromMetadata.Signature.ReturnType with
        | MethodReturnType.Returns (TypeDefn.PrimitiveType PrimitiveType.Int32) -> ()
        | _ -> failwith "Main method must return int32; other types not currently supported"

        let state =
            IlMachineState.initial loggerFactory dotnetRuntimeDirs dumped
            |> fun s -> s.MapKernel (KernelConfig.applyTo kernelConfig)
            |> fun s ->
                match pctSeed with
                | None -> s
                | Some seed -> IlMachineState.withPctSeed seed s

        // Find the core library by traversing the type hierarchy of the main method's declaring type
        // until we reach System.Object
        let rec handleBaseTypeInfo
            (state : IlMachineState)
            (baseTypeInfo : BaseTypeInfo)
            (currentAssembly : DumpedAssembly)
            (continueWithGeneric :
                IlMachineState
                    -> TypeInfo<GenericParamFromMetadata, TypeDefn>
                    -> DumpedAssembly
                    -> IlMachineState * BaseClassTypes<DumpedAssembly> option)
            (continueWithResolved :
                IlMachineState
                    -> TypeInfo<TypeDefn, TypeDefn>
                    -> DumpedAssembly
                    -> IlMachineState * BaseClassTypes<DumpedAssembly> option)
            : IlMachineState * BaseClassTypes<DumpedAssembly> option
            =
            match baseTypeInfo with
            | BaseTypeInfo.TypeRef typeRefHandle ->
                // Look up the TypeRef from the handle
                let typeRef = currentAssembly.TypeRefs.[typeRefHandle]

                let rec go state =
                    // Resolve the type reference to find which assembly it's in
                    match
                        Assembly.resolveTypeRef state._LoadedAssemblies currentAssembly ImmutableArray.Empty typeRef
                    with
                    | TypeResolutionResult.FirstLoadAssy assyRef ->
                        // Need to load this assembly first
                        let handle, definedIn = assyRef.Handle

                        let state, _, _ =
                            IlMachineState.loadAssembly loggerFactory state._LoadedAssemblies.[definedIn] handle state

                        go state
                    | TypeResolutionResult.Resolved (resolvedAssembly, _, resolvedType) ->
                        continueWithResolved state resolvedType resolvedAssembly

                go state
            | BaseTypeInfo.TypeDef typeDefHandle ->
                // Base type is in the same assembly
                let baseType = currentAssembly.TypeDefs.[typeDefHandle]
                continueWithGeneric state baseType currentAssembly
            | BaseTypeInfo.TypeSpec _ -> failwith "Type specs not yet supported in base type traversal"

        let rec findCoreLibraryAssemblyFromGeneric
            (state : IlMachineState)
            (currentType : TypeInfo<GenericParamFromMetadata, TypeDefn>)
            (currentAssembly : DumpedAssembly)
            =
            match currentType.BaseType with
            | None ->
                // We've reached the root (System.Object), so this assembly contains the core library
                let baseTypes = Corelib.getBaseTypes currentAssembly
                state, Some baseTypes
            | Some baseTypeInfo ->
                handleBaseTypeInfo
                    state
                    baseTypeInfo
                    currentAssembly
                    findCoreLibraryAssemblyFromGeneric
                    findCoreLibraryAssemblyFromResolved

        and findCoreLibraryAssemblyFromResolved
            (state : IlMachineState)
            (currentType : TypeInfo<TypeDefn, TypeDefn>)
            (currentAssembly : DumpedAssembly)
            =
            match currentType.BaseType with
            | None ->
                // We've reached the root (System.Object), so this assembly contains the core library
                let baseTypes = Corelib.getBaseTypes currentAssembly
                state, Some baseTypes
            | Some baseTypeInfo ->
                handleBaseTypeInfo
                    state
                    baseTypeInfo
                    currentAssembly
                    findCoreLibraryAssemblyFromGeneric
                    findCoreLibraryAssemblyFromResolved

        /// The frame the entry thread runs during startup: the entry point's *signature* with
        /// its body replaced by a bare `ret`. Pushing cctors underneath it and pumping until
        /// it returns is how `prepare` drives static initialisation without entering Main.
        ///
        /// Rebuildable rather than built once, because seeding AppContext also has to pump
        /// the entry thread to completion, which consumes this frame; the seed then puts a
        /// fresh one back for the cctor pump that follows.
        let buildStartupFrame
            (baseTypes : BaseClassTypes<DumpedAssembly>)
            (state : IlMachineState)
            : IlMachineState * MethodState
            =
            // Use the original method from metadata, but convert FakeUnit to TypeDefn
            let rawMainMethod =
                mainMethodFromMetadata
                |> MethodInfo.mapTypeGenerics (fun (i, _) -> TypeDefn.GenericTypeParameter i.SequenceNumber)

            let state, concretizedMainMethod, _ =
                ExecutionConcretization.concretizeMethodWithTypeGenerics
                    loggerFactory
                    baseTypes
                    ImmutableArray.Empty // No type generics for main method's declaring type
                    { rawMainMethod with
                        Body = MethodBody.Il (MethodInstructions.onlyRet ())
                    }
                    None
                    dumped.Name
                    ImmutableArray.Empty
                    state

            // Create the method state with the concretized method.
            // The body has been replaced with onlyRet, so these are placeholders whose
            // length must match the method's parameter count.
            let placeholderArgs =
                if mainTakesStringArrayArg then
                    ImmutableArray.CreateRange [ CliType.ObjectRef None ]
                else
                    ImmutableArray.Empty

            match
                MethodState.Empty
                    state.ConcreteTypes
                    baseTypes
                    state._LoadedAssemblies
                    dumped
                    concretizedMainMethod
                    ImmutableArray.Empty
                    placeholderArgs
                    None
            with
            | Ok concretizedMeth -> state, concretizedMeth
            | Error _ -> failwith "Unexpected failure creating method state with concretized method"

        let rec computeState (baseClassTypes : BaseClassTypes<DumpedAssembly> option) (state : IlMachineState) =
            match baseClassTypes with
            | Some baseTypes ->
                // We already have base class types, can directly create the concretized method
                let state, concretizedMeth = buildStartupFrame baseTypes state
                IlMachineState.addThread concretizedMeth state, Some baseTypes
            | None ->
                // We need to discover the core library by traversing the type hierarchy
                let mainMethodType =
                    dumped.TypeDefs.[mainMethodFromMetadata.DeclaringType.Definition.Get]

                let state, baseTypes =
                    findCoreLibraryAssemblyFromGeneric state mainMethodType dumped

                computeState baseTypes state

        let (state, mainThread), baseClassTypes = state |> computeState None

        let baseClassTypes =
            match baseClassTypes with
            | Some c -> c
            | None -> failwith "Expected base class types to be available at this point"

        // Now that we have base class types, concretize the main method for use in the rest of the function
        let state, concretizedMainMethod, mainTypeHandle =
            let rawMainMethod =
                mainMethodFromMetadata
                |> MethodInfo.mapTypeGenerics (fun (i, _) -> TypeDefn.GenericTypeParameter i.SequenceNumber)

            ExecutionConcretization.concretizeMethodWithTypeGenerics
                loggerFactory
                baseClassTypes
                ImmutableArray.Empty // No type generics for main method's declaring type
                rawMainMethod
                None
                dumped.Name
                ImmutableArray.Empty
                state

        let state =
            { state with
                ConcreteTypes = Corelib.concretizeAll state._LoadedAssemblies baseClassTypes state.ConcreteTypes
            }

        // Seed AppContext before anything else runs. On CoreCLR this happens in
        // `CorHost2::CreateAppDomainWithManager`, before any managed code at all; the deadline
        // that actually bites is that BCL feature switches latch on first read into a
        // `static readonly` (`EventSource.IsSupported` is the motivating one), so seeding has
        // to precede the entry type's cctor pump below, not merely precede Main.
        //
        // This runs the entry thread to completion, which consumes its startup frame; a fresh
        // one goes back afterwards so the cctor pump that follows is unaffected.
        let state =
            match AppContextSeed.prepareCall loggerFactory baseClassTypes hostConfig.AppContext state with
            | None -> state
            | Some (state, setupFrame) ->
                logger.LogInformation "Seeding AppContext from the host's configuration properties"

                let threadState =
                    state.ThreadState.[mainThread]
                    |> ThreadState.replaceFrames setupFrame
                    |> fun threadState ->
                        { threadState with
                            Status = ThreadStatus.Runnable
                        }

                let state =
                    { state with
                        ThreadState = state.ThreadState |> Map.add mainThread threadState
                    }

                let state =
                    match pumpToReturn loggerFactory logger baseClassTypes mainThread state with
                    | RunOutcome.NormalExit (state, _) -> state
                    | outcome ->
                        // Nothing in `AppContext.Setup` can legitimately exit, fail fast or
                        // throw: it allocates a Dictionary and copies strings out of buffers
                        // we ourselves just wrote. Anything else means a cctor dragged in by
                        // that work misbehaved, and pressing on would run Main against a
                        // half-seeded AppContext.
                        //
                        // Describe the outcome by case rather than with `%O`: every
                        // `RunOutcome` carries an `IlMachineState`, so structural formatting
                        // would render the entire heap into the exception message.
                        let described =
                            match outcome with
                            | RunOutcome.NormalExit _ -> "returned normally" // unreachable, matched above
                            | RunOutcome.ProcessExit (_, thread) -> $"called Environment.Exit on %O{thread}"
                            | RunOutcome.FailFast (_, thread, message) ->
                                let message = message |> Option.defaultValue "<no message>"
                                $"called Environment.FailFast on %O{thread}: %s{message}"
                            | RunOutcome.SignalTerminated (_, signal) -> $"was terminated by signal %O{signal}"
                            | RunOutcome.GuestUnhandledException (_, thread, exn) ->
                                $"threw an unhandled exception on %O{thread}: %O{exn.ExceptionObject}"

                        failwith $"Seeding AppContext %s{described}. Properties being seeded: %O{hostConfig.AppContext}"

                let state, startupFrame = buildStartupFrame baseClassTypes state

                let threadState =
                    state.ThreadState.[mainThread]
                    |> ThreadState.replaceFrames startupFrame
                    |> fun threadState ->
                        { threadState with
                            Status = ThreadStatus.Runnable
                        }

                { state with
                    ThreadState = state.ThreadState |> Map.add mainThread threadState
                }

        let rec loadInitialState (state : IlMachineState) =
            match
                state
                |> IlMachineStateExecution.loadClass loggerFactory baseClassTypes mainTypeHandle mainThread
            with
            | StateLoadResult.NothingToDo ilMachineState -> ilMachineState
            | StateLoadResult.FirstLoadThis ilMachineState -> loadInitialState ilMachineState
            | StateLoadResult.ThrowingTypeInitializationException _ ->
                failwith "TypeInitializationException during initial class load of entry point type"
            | StateLoadResult.Blocked _ ->
                // Unreachable at startup: only the entry thread exists, so no other thread can
                // be mid-cctor on the entry type. Listing the case explicitly keeps the match
                // exhaustive and pins the invariant for future readers.
                failwith
                    "logic error: initial loadClass for entry point cannot block on another thread (no other threads exist yet)"

        let state = loadInitialState state

        let mainArgs, state =
            if mainTakesStringArrayArg then
                let arrayAllocation, state = allocateArgs loggerFactory argv baseClassTypes state
                ImmutableArray.Create (CliType.ofManagedObject arrayAllocation), state
            else
                ImmutableArray.Empty, state

        // We might be in the middle of class construction. Pump the static constructors to completion.
        // We haven't yet entered the main method!

        match pumpToReturn loggerFactory logger baseClassTypes mainThread state with
        | RunOutcome.GuestUnhandledException _ as outcome ->
            // Either the entry thread's .cctor raised an unhandled exception, or a worker
            // spawned during cctor pumping did. In both cases the CLR would terminate the
            // process; propagate rather than collapsing to a host failwith that would
            // mask the guest-level diagnostic.
            ProgramStartResult.CompletedBeforeMain outcome
        | RunOutcome.ProcessExit _ as outcome ->
            // A worker started during cctor pumping called Environment.Exit; the process
            // has torn down. Propagate rather than pressing on into Main.
            ProgramStartResult.CompletedBeforeMain outcome
        | RunOutcome.FailFast _ as outcome ->
            // A worker started during cctor pumping called Environment.FailFast; the
            // process has aborted. Propagate rather than pressing on into Main.
            ProgramStartResult.CompletedBeforeMain outcome
        | RunOutcome.SignalTerminated _ as outcome ->
            // A non-cancelled signal handler reached the kernel-default
            // Terminate disposition during cctor pumping. Same shape as
            // ProcessExit: the simulated process is gone, so propagate
            // rather than pressing on into Main.
            ProgramStartResult.CompletedBeforeMain outcome
        | RunOutcome.NormalExit (state, _) ->

        logger.LogInformation "Main method class now initialised"

        // Now that BCL initialisation has taken place and the user-code classes are constructed,
        // overwrite the main thread completely using the already-concretized method. The entry
        // thread Terminated during the cctor pump (its onlyRet body hit `ret`); we're resurrecting
        // it to run Main, so restore Status to Runnable before the scheduler is asked to pick again.
        let methodState =
            match
                MethodState.Empty
                    state.ConcreteTypes
                    baseClassTypes
                    state._LoadedAssemblies
                    dumped
                    concretizedMainMethod
                    ImmutableArray.Empty
                    mainArgs
                    None
            with
            | Ok s -> s
            | Error _ -> failwith "TODO: I'd be surprised if this could ever happen in a valid program"

        let threadState =
            state.ThreadState.[mainThread]
            |> ThreadState.replaceFrames methodState
            |> fun threadState ->
                { threadState with
                    Status = ThreadStatus.Runnable
                }

        let state, init =
            { state with
                ThreadState = state.ThreadState |> Map.add mainThread threadState
            }
            |> IlMachineStateExecution.ensureTypeInitialised loggerFactory baseClassTypes mainThread mainTypeHandle

        match init with
        | WhatWeDid.SuspendedForClassInit -> failwith "TODO: suspended for class init"
        | WhatWeDid.SuspendedForManagedCall ->
            failwith "logic error: ensureTypeInitialised cannot suspend for an arbitrary managed call"
        | WhatWeDid.BlockedOnClassInit _ -> failwith "logic error: surely this thread can't be blocked on class init"
        | WhatWeDid.ThrowingTypeInitializationException ->
            failwith "TypeInitializationException during entry point type initialisation"
        | WhatWeDid.VoluntaryYield ->
            // ensureTypeInitialised drives cctor execution, which has no path to a
            // yield primitive: voluntary yields are produced by native handlers like
            // `ThreadNative_YieldThread`, never by a synthetic cctor step. If this
            // arm ever fires, the cctor pipeline has acquired a producer we didn't
            // anticipate, and the entry-point sequencer needs to decide explicitly
            // whether to honour the yield before running Main.
            failwith "logic error: ensureTypeInitialised cannot produce a VoluntaryYield"
        | WhatWeDid.Executed -> ()

        ProgramStartResult.Ready
            {
                State = state
                BaseClassTypes = baseClassTypes
                EntryThread = mainThread
                LastRan = mainThread
            }

    /// Returns the outcome of the program run: normal exit or unhandled guest exception.
    ///
    /// `pctSeed` flows through to `prepare`: `Some s` selects PCT with seed `s`,
    /// `None` keeps the default round-robin scheduler. See `prepare` for the
    /// timing contract (applied before the first cctor frame is pushed).
    let run
        (loggerFactory : ILoggerFactory)
        (originalPath : string option)
        (fileStream : Stream)
        (hostConfig : HostConfig)
        : RunOutcome
        =
        let logger = loggerFactory.CreateLogger "Program"

        match prepare loggerFactory originalPath fileStream hostConfig with
        | ProgramStartResult.CompletedBeforeMain outcome -> outcome
        | ProgramStartResult.Ready prepared -> pumpPrepared loggerFactory logger prepared

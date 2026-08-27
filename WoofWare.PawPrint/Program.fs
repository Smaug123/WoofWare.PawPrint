namespace WoofWare.PawPrint

open System
open System.Collections.Immutable
open System.IO
open System.Reflection.Metadata
open Microsoft.Extensions.Logging
open WoofWare.PosixKernel

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
                corelib.Corelib.DefinitionFullName
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
        /// `effect` is the step's `StepEffect`, forwarded verbatim from
        /// `ExecutionResult.Stepped`. It is what makes a *streaming* driver
        /// possible: `StepEffect.WroteToFd` carries exactly the bytes this step
        /// appended to `EmulatedKernel.OutputLog`, so a driver can write them to
        /// a real stream as they are produced instead of waiting for a
        /// `RunOutcome` and draining the log. A run that never produces a
        /// `RunOutcome` (a livelocked guest, a guest killed from outside,
        /// `Deadlocked`) has no end-of-run drain to reach, so without streaming
        /// its output is lost entirely.
        ///
        /// Steps that terminate the run do not carry an effect: those outcomes
        /// are `Completed`, and their `RunOutcome` carries the final state whose
        /// `OutputLog` is authoritative. A driver that streams should still drain
        /// any log entries beyond what it has written when the run ends, because
        /// writes performed *before* the driver's own loop starts (a `.cctor`
        /// that prints, pumped inside `prepare`) never pass through here.
        | InstructionStepped of PreparedProgram * ranThread : ThreadId * whatWeDid : WhatWeDid * effect : StepEffect
        | WorkerTerminated of PreparedProgram * terminatingThread : ThreadId
        | Completed of RunOutcome
        | Deadlocked of PreparedProgram * stuckThreads : string

    /// Where a `Startup` has got to, together with whatever that phase needs to hand on.
    ///
    /// Startup runs guest code twice before `Main`, and the two runs are not interchangeable:
    /// the AppContext seed must finish before the entry type's `.cctor` starts, because BCL
    /// feature switches latch into `static readonly` fields on first read. Modelled as a DU
    /// carrying each phase's own data so the pair cannot drift apart — there is no way to be
    /// initialising classes without having allocated argv, nor to be seeding without knowing
    /// what to do when the seed returns.
    type private StartupPhase =
        /// Pumping `AppContext.Setup`. `onReturn` reinstates the startup frame the seed
        /// consumed, loads the entry class, and allocates argv.
        | SeedingAppContext of onReturn : (IlMachineState -> IlMachineState * ImmutableArray<CliType>)
        /// Pumping class initialisers, the entry type's included, with argv already allocated.
        | InitialisingClasses of mainArgs : ImmutableArray<CliType>

    /// Startup in progress. Holds the machine state as a `PreparedProgram`, so the same
    /// `stepPrepared` drives startup as drives `Main`, plus what remains to be done at each
    /// phase boundary.
    ///
    /// This exists so a driver can *step* startup rather than having it run to completion
    /// behind a single call. Guest code runs here — a static initialiser may print, block, or
    /// wedge — and a driver that cannot see those steps cannot stream their output or report
    /// where startup got stuck.
    ///
    /// The phase transitions are closures. They capture concretization results (a concretized
    /// `Main`, the entry type's handle) whose inspectable form would be no more use to a caller
    /// than the functions that consume them, and hoisting them to module scope would mean
    /// threading ten parameters through for no gain in reasoning. What a caller *can* see —
    /// the machine state, and which outcome a step produced — is data.
    type Startup =
        private
            {
                Prepared : PreparedProgram
                Phase : StartupPhase
                /// Installs the `Main` frame once class initialisation has returned.
                InstallMain : IlMachineState -> ImmutableArray<CliType> -> ProgramStartResult
            }

        /// The machine state as it currently stands. A driver streaming guest output reads
        /// `Kernel.OutputLog` from here when startup ends without a `ProgramStartResult`.
        member this.State : IlMachineState = this.Prepared.State

    /// The result of stepping startup once. Mirrors `ProgramStepOutcome`, and for the same
    /// reason carries the step's `StepEffect`: a driver consumes it to stream guest writes as
    /// they happen, which is the whole point of startup being steppable.
    [<RequireQualifiedAccess>]
    type StartupStepOutcome =
        | Stepped of Startup * ranThread : ThreadId * whatWeDid : WhatWeDid * effect : StepEffect
        | WorkerTerminated of Startup * terminatingThread : ThreadId
        /// The entry thread's frame returned and startup moved to its next phase. No guest
        /// instruction retired, so there is no effect to report.
        | PhaseAdvanced of Startup
        | Completed of ProgramStartResult
        | Deadlocked of Startup * stuckThreads : string

    /// Where each live thread is, for the deadlock reports below and for every host that
    /// consumes a `Deadlocked` outcome. The status alone does not locate a guest — every thread
    /// blocked on a monitor looks alike — so this names the frame each thread is in and, where
    /// the guest was built with debug information, the source line it is on.
    let private deadlockDescription (state : IlMachineState) : string = GuestLocation.describe state

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
        // `BlockedOnSocketEvents` has no deadline to project: `WaitForSocketEvents` takes no
        // timeout, so the clock can never wake such a thread. There is deliberately no
        // `FiredDeadline` case for it, which is what makes that unrepresentable rather than
        // merely unwritten.
        | ThreadStatus.BlockedOnSocketEvents _
        | ThreadStatus.Runnable
        | ThreadStatus.NotStarted
        | ThreadStatus.BlockedOnClassInit _
        | ThreadStatus.BlockedOnMonitorAcquire _
        | ThreadStatus.Terminated
        | ThreadStatus.Parked -> None

    /// Fire a timeout wake for every blocked-with-deadline thread whose
    /// deadline is `<= state.Kernel.VirtualClockTicks`. Each fire routes
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
    let private fireExpiredDeadlines (state : IlMachineState) : IlMachineState =
        let now = state.Kernel.VirtualClockTicks

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

    /// Wake every thread parked in `SystemNative_WaitForSocketEvents` whose
    /// port would deliver at least one event right now. The park is
    /// re-entrant — the native frame stays and the caller's program counter
    /// still names the call — so waking is exactly a flip to `Runnable`; the
    /// re-entered handler asks `EmulatedKernel.deliverSocketEvents`, whose
    /// walk is the same one `hasDeliverableSocketEvents` consulted here, so
    /// the woken thread cannot find the port empty unless another thread
    /// drained it first — in which case the handler parks it again.
    ///
    /// Runs every tick beside `fireExpiredDeadlines` rather than being
    /// pushed by the producing syscalls: a sweep asks the same question of
    /// the same state each time, so a new producer cannot forget to wake
    /// anyone — where a push from each producer would fail silently, as a
    /// deadlock, on the first one that did.
    let private fireSocketReadiness (state : IlMachineState) : IlMachineState =
        // Runs on every tick of every workload, so the no-waiter case must
        // cost no allocation: a fold that accumulates only matches, rather
        // than materialising the thread map.
        let waiters =
            (state.ThreadState, [])
            ||> Map.foldBack (fun tid ts acc ->
                match ts.Status with
                | ThreadStatus.BlockedOnSocketEvents port -> (tid, port) :: acc
                | _ -> acc
            )

        match waiters with
        | [] -> state
        | waiters ->

        let deliverable =
            waiters
            |> List.filter (fun (_, port) -> EmulatedKernel.hasDeliverableSocketEvents port state.Kernel)

        // An edge arriving with several threads parked on one port is
        // unmodelled: `ep_poll` adds each waiter to the port's wait queue
        // *exclusively*, so a real event wakes one of them — in an order
        // PawPrint keeps no state to reproduce (the queue is park-order) and
        // has not measured. No managed caller can reach this
        // (`SocketAsyncEngine` dedicates one thread per port), so refuse
        // loudly rather than wake every waiter and let the scheduler invent
        // the winner.
        for port, sharing in deliverable |> List.groupBy snd do
            if List.length sharing > 1 then
                let tids = sharing |> List.map (fun (tid, _) -> $"%O{tid}") |> String.concat ", "

                failwith
                    $"fireSocketReadiness: threads %s{tids} are all parked in SystemNative_WaitForSocketEvents on port %O{port}, which now has a deliverable event. epoll parks waiters exclusively, so a real kernel wakes exactly one of them, chosen by park order — state PawPrint does not record and semantics it has not measured. Implement the one-wakeup rule before parking several threads on one port."

        (state, deliverable)
        ||> List.fold (fun s (tid, _) -> Scheduler.wakeFromSocketEvents tid s)

    /// The minimum wait deadline among currently-blocked threads, or
    /// `None` if no thread is parked with a finite timeout. Used by the
    /// driver loop's jump-to-deadline fallback: if no thread is Runnable
    /// but at least one has a finite-timeout wait outstanding, advance
    /// `VirtualClockTicks` to the nearest such deadline so the wait can
    /// resolve on the next pass.
    ///
    /// The clock-jump must not bump `StepCounter` — the spurious-wakeup
    /// schedules are keyed on `StepCounter`, and a jump-driven tick is
    /// deliberately *not* a real scheduler tick. Keeping the two clocks
    /// separate is exactly why `VirtualClockTicks` is its own field rather
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

    /// Every finite wait deadline currently outstanding, in no particular order
    /// and with duplicates where two threads are parked on the same instant.
    /// The candidate set `ClockJitterStrategy.EagerDeadlines` draws from, which
    /// is why it is the whole collection and not just the minimum
    /// `nextDeadline` reports.
    let private pendingDeadlines (state : IlMachineState) : int64 list =
        state.ThreadState
        |> Map.toList
        |> List.choose (fun (_, ts) -> waitDeadline ts.Status |> Option.map snd)

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
        | WhatWeDid.SuspendedForClassInit ->
            logger.LogTrace "Suspended execution of current method for class initialisation."
        | WhatWeDid.SuspendedForManagedCall ->
            logger.LogTrace "Suspended execution of native handler for a managed call continuation."
        | WhatWeDid.BlockedOnClassInit _ -> logger.LogTrace "Unable to execute because class has not yet initialised."
        | WhatWeDid.ThrowingTypeInitializationException ->
            logger.LogTrace "TypeInitializationException dispatched due to failed .cctor."

    /// The first half of a scheduler tick: everything that happens before the policy is asked
    /// which thread runs next. Advancing the clocks, firing wait deadlines, letting the signal
    /// dispatcher wake, and jumping the virtual clock forward if nothing is Runnable.
    ///
    /// Split out from `stepPrepared` because *this* is the moment at which "how many threads are
    /// Runnable" becomes the answer the policy will act on. Every phase here can create
    /// contention within the tick — a deadline firing, a spurious wake, the dispatcher becoming
    /// Runnable — so a fork detector that probed the inter-tick state instead would miss forks
    /// and hand a schedule-sweeping harness a prefix that is not actually forced. See
    /// `runToNextFork`.
    ///
    /// Deliberately policy-independent: nothing here reads `state.Scheduling`, and a stochastic
    /// policy's RNG is not advanced by a probe. That
    /// is what makes it safe to run this, look at the result, and then run it again from the
    /// original state on a later resume.
    ///
    /// Not idempotent: it advances `StepCounter` and the virtual clock. Callers hold the
    /// *inter-tick* value if they want to be able to replay the tick.
    let private advanceToDecision (prepared : PreparedProgram) : PreparedProgram =
        // Apply the spurious-wakeup strategies at the current tick, then
        // advance the counter so the next iteration sees a fresh tick.
        // For the default (`Disabled`) strategy each application is a fold
        // over the identity and a single integer add. The two layers
        // (LowLevel and SyncBlock) are independent waiters on disjoint
        // primitive types, so the order between them at a given tick is
        // unobservable; LowLevel is applied first.
        let state =
            LowLevelMonitor.applySpuriousWakeups
                prepared.State.Kernel.SpuriousWakeup
                prepared.State.Kernel.StepCounter
                prepared.State

        let state =
            SyncBlockMonitor.applySpuriousWakeups state.Kernel.SyncBlockSpuriousWakeup state.Kernel.StepCounter state

        // The tick this preamble is running, captured before the counter moves on so that clock
        // jitter is keyed on the same number the two spurious-wakeup strategies just used. All
        // three are fuzz dials a caller scripts by tick, and they would be treacherous to script
        // against each other if "tick N" meant a different moment to each.
        let tick = state.Kernel.StepCounter

        // Threaded as a state rather than rebuilding `prepared` at each stage below: every stage
        // from here to the return touches only `State`, so each rebuilt `PreparedProgram` existed
        // only for the next stage to read `.State` straight back out of it. This function runs
        // once per interpreted instruction, which is what made those wrappers worth removing.

        // `EmulatedKernel.InstructionCostTicks` of virtual time per scheduler
        // tick — see that constant for the rate and why it is what it is.
        // Bumping in lock-step with `StepCounter` keeps both clocks pure
        // functions of "how many scheduler ticks have elapsed", which is what
        // tests rely on when driving the strategies without a real driver.
        //
        // `retireStep` rather than a record-copy piped through
        // `withVirtualClockTicks`: it applies the same validation — so the horizon
        // is still enforced at the writer, which this path cannot realistically
        // reach (it would take ~9.2e12 retired instructions) but which should not
        // hold only by coincidence — while costing one copy of a 31-field record
        // instead of two. This runs once per interpreted instruction, where the
        // second copy was ~8% of everything the interpreter allocated.
        let state = state.WithKernel (EmulatedKernel.retireStep state.Kernel)

        // Clock jitter: with the configured strategy's blessing, jump the clock
        // onto a deadline some thread is already parked on, so that the timeout
        // fires while other threads still had work left in the window rather
        // than at the instruction count the guest's own arithmetic implies. Off
        // by default, in which case this is one match on a DU case.
        //
        // Applied after the ordinary advance and before the expiry pass below,
        // so a jitter-reached deadline fires in the very same pass as one that
        // came due on its own: the rest of the tick cannot tell the two apart,
        // which is the point.
        //
        // `StepCounter` is deliberately not bumped, exactly as the
        // jump-to-deadline fallback below does not bump it: the jitter schedule
        // and the spurious-wakeup schedules are both keyed on that counter, and
        // a jump is the resolution of a timeout rather than a retired step.
        let state =
            match state.Kernel.ClockJitter with
            // Taken before `pendingDeadlines`, which walks every thread and
            // allocates: F# evaluates arguments eagerly, so passing it to
            // `chooseJump` unconditionally would charge that walk to every tick
            // of every run, in exchange for an answer that is `None` by
            // definition. Only the disabled case is special-cased here — any
            // future variant falls through to the full decision below rather
            // than silently inheriting a fast path meant for "switched off".
            | ClockJitterStrategy.Disabled -> state
            | strategy ->

            match ClockJitter.chooseJump strategy tick state.Kernel.VirtualClockTicks (pendingDeadlines state) with
            | None -> state
            // Through the validating setter, which is what faults if a guest's
            // own timeout arithmetic has run the clock off the representable
            // range; `chooseJump` guarantees the target is ahead of the clock,
            // so the monotonicity half of that check cannot fire here.
            | Some target -> state.MapKernel (EmulatedKernel.mapMachine (UnixMachineState.withVirtualClockTicks target))

        // After advancing `VirtualClockTicks`, fire any wait deadlines that
        // are now in the past. This runs every tick (not just on
        // deadlock) so a timeout against a thread holding a release lock
        // can still expire while other threads make progress: e.g.
        // thread A is parked with a 50 ms timeout on a semaphore, and
        // thread B is busy computing something else — A's deadline still
        // fires when the clock reaches it, even though B keeps the
        // scheduler from ever stalling.
        let state = fireExpiredDeadlines state

        // Wake any socket-events waiter whose port has become deliverable
        // since it parked. Before the jump-to-deadline fallback below, so a
        // deliverable port is never mistaken for quiescence.
        let state = fireSocketReadiness state

        // Drive the signal-dispatcher state machine before the scheduler
        // picks its next thread. If a pending signal is deliverable and
        // the dispatcher is currently Parked, this flips it to Runnable
        // and installs a handler-invocation bottom frame; the scheduler
        // then picks the dispatcher up on the same tick. If nothing is
        // deliverable, this is a no-op and the next tick re-polls.
        let state = SignalDispatch.trySpawnHandler prepared.BaseClassTypes state

        // Jump-to-deadline fallback: if no thread is Runnable but at
        // least one is parked with a finite-timeout wait outstanding,
        // advance `VirtualClockTicks` to the nearest pending deadline and
        // fire it. This is what keeps a guest like `WaitOne(50)` against
        // an unsignalled handle from deadlocking — without the jump, the
        // clock would advance only when there's something to step, and
        // there is nothing to step.
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
        // advances `VirtualClockTicks` to the next outstanding deadline; the
        // set of finite-deadline threads is finite and monotonically
        // shrinks (no fire creates a new deadline), so the loop
        // terminates.
        //
        // Only `VirtualClockTicks` is advanced (not `StepCounter`), so the
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
                        // The path that *can* reach the horizon: this jumps the clock straight
                        // to a deadline without retiring a step, so a guest looping on
                        // `Thread.Sleep(Int32.MaxValue)` advances it ~2.1e13 ticks per cheap
                        // iteration. `withVirtualClockTicks` faults here, naming the wait that
                        // ran time off the end, instead of letting the addition wrap and hand
                        // some later sleeper a negative deadline that fires immediately.
                        state.MapKernel (
                            EmulatedKernel.mapMachine (
                                UnixMachineState.withVirtualClockTicks (max state.Kernel.VirtualClockTicks target)
                            )
                        )

                    advanceUntilRunnableOrQuiescent (fireExpiredDeadlines state)

        { prepared with
            State = advanceUntilRunnableOrQuiescent state
        }

    /// The second half of a scheduler tick: ask the policy which thread runs next, run it, and
    /// fold the outcome back into the thread states. `prepared` must already have been through
    /// `advanceToDecision`; running this against an inter-tick value would consult the policy
    /// about a Runnable set that a deadline or a spurious wake was about to change.
    let private stepDecided
        (loggerFactory : ILoggerFactory)
        (logger : ILogger)
        (prepared : PreparedProgram)
        : ProgramStepOutcome
        =
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
                AbstractMachine.executeOneStep loggerFactory prepared.BaseClassTypes prepared.State nextThread
                |> ExecutionResult.mapState (Scheduler.dischargeYieldDebts nextThread)

            match stepResult with
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

                    // The dispatcher retired a step and this branch reports it as
                    // `WhatWeDid.Executed`, so give it that outcome's consequences — waking
                    // anything parked BlockedOnClassInit behind it. (The yield-debt half of
                    // the bookkeeping has already happened, in the discharge above.)
                    let state = Scheduler.onStepOutcome terminatingThread WhatWeDid.Executed state

                    ProgramStepOutcome.InstructionStepped (
                        { prepared with
                            State = state
                            LastRan = terminatingThread
                        },
                        terminatingThread,
                        WhatWeDid.Executed,
                        // The signal dispatcher's handler frame returning past its
                        // bottom arrives as `ExecutionResult.Terminated`, which carries
                        // no effect: the step performed no I/O of its own.
                        StepEffect.NoEffect
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
            | ExecutionResult.Aborted (state, abortingThread, message) ->
                ProgramStepOutcome.Completed (RunOutcome.Aborted (state, abortingThread, message))
            | ExecutionResult.SignalTerminated (state, signal) ->
                ProgramStepOutcome.Completed (RunOutcome.SignalTerminated (state, signal))
            | ExecutionResult.UnhandledException (state, terminatingThread, exn) ->
                ProgramStepOutcome.Completed (RunOutcome.GuestUnhandledException (state, terminatingThread, exn))
            | ExecutionResult.Stepped (state, whatWeDid, effect) ->
                logStepOutcome logger state nextThread whatWeDid

                let state = Scheduler.onStepOutcome nextThread whatWeDid state

                ProgramStepOutcome.InstructionStepped (
                    { prepared with
                        State = state
                        LastRan = nextThread
                    },
                    nextThread,
                    whatWeDid,
                    effect
                )

    /// <summary>
    /// Run <paramref name="tick" />, annotating any host failure with where the guest was at
    /// <paramref name="state" />.
    /// </summary>
    /// <remarks>
    /// <para>
    /// PawPrint fails by <c>failwith</c> in some 2,400 places, and almost none of them can name
    /// the guest: the least informative messages of all come from pure helpers inside the opcode
    /// implementations, which have no <c>IlMachineState</c> to consult and should not grow one
    /// just to describe a failure. Annotating at the tick covers all of them at once.
    /// </para>
    /// <para>
    /// A *whole* tick must be inside, not just the instruction. Work that can fail sits on both
    /// sides of it: <c>advanceToDecision</c> applies spurious wakeups and moves the virtual
    /// clock — which faults at its horizon — before the instruction, and
    /// <c>dischargeYieldDebts</c>, <c>onStepOutcome</c> and <c>onThreadTerminated</c> run after.
    /// Those failures are every bit as guest-provoked: <c>onThreadTerminated</c> refusing a
    /// worker that exited still holding a monitor is a diagnostic *about the guest*, and is far
    /// less useful without knowing which guest code let go of it.
    /// </para>
    /// <para>
    /// Hence the invariant this combinator exists to make checkable: <c>advanceToDecision</c> and
    /// <c>stepDecided</c> are both private, and *every* call to either is inside an
    /// <c>annotating</c>. There are three such sites — <c>stepPrepared</c>, and the two in the
    /// fork-prefix sweep — and grepping for the two names finds exactly them. Adding a fourth
    /// call site outside a wrapper is the one way to reintroduce the gap.
    /// </para>
    /// <para>
    /// The state described is the one the tick *started* from. The failure happened partway
    /// through, so there is no consistent later state to report.
    /// </para>
    /// <para>
    /// <c>inline</c> with <c>InlineIfLambda</c> because <c>stepPrepared</c> is per-tick: taking
    /// the body as a first-class function would allocate an <c>FSharpFunc</c> capturing the
    /// logger and the program state on every interpreted instruction — some 20 million of them
    /// in a bounded run — purely to serve a path that normally never fires. Inlined, the caller
    /// keeps the exception region and nothing else.
    /// </para>
    /// </remarks>
    let inline private annotating (state : IlMachineState) ([<InlineIfLambda>] tick : unit -> 'a) : 'a =
        try
            tick ()
        with
        // Already annotated: a nested tick would otherwise repeat the thread summary once per
        // level, and the outermost frame's guest position is the least specific of them.
        | :? GuestFailureException -> reraise ()
        | e ->
            // `TryCreate` is total over both the lookup *and* the message construction, so a
            // failure to annotate reraises the original rather than replacing it.
            match GuestFailureException.TryCreate (e, state) with
            | Some annotated -> raise annotated
            | None -> reraise ()

    /// Advance the machine by one scheduler tick.
    ///
    /// The stepper reports NormalExit as soon as `EntryThread` Terminates, regardless
    /// of whether other threads are still Runnable or Blocked. This matches both
    /// use sites:
    ///   * Pre-Main cctor pump: the synthetic onlyRet frame has returned, which
    ///     means class initialisation is done. The entry thread isn't actually
    ///     finished — Program.run is about to resurrect it with the real Main
    ///     frame — so we deliberately do NOT mark it Terminated, because doing so
    ///     would let a worker that joined the entry thread during a .cctor observe
    ///     a false end-of-thread and proceed past the Join before Main has started.
    ///   * Post-Main pump: when Main returns, we report NormalExit immediately
    ///     rather than waiting for foreground threads. The test comparison oracles
    ///     in WoofWare.PawPrint.Test just invoke `assy.EntryPoint.Invoke` via
    ///     reflection, which also returns as soon as Main returns without waiting
    ///     for foreground workers, so matching that behaviour keeps PawPrint and
    ///     the oracle aligned. Environment.Exit from a worker still propagates as
    ///     ProcessExit (handled below) before Main has a chance to return.
    let stepPrepared
        (loggerFactory : ILoggerFactory)
        (logger : ILogger)
        (prepared : PreparedProgram)
        : ProgramStepOutcome
        =
        annotating prepared.State (fun () -> stepDecided loggerFactory logger (advanceToDecision prepared))

    let rec pumpPrepared (loggerFactory : ILoggerFactory) (logger : ILogger) (prepared : PreparedProgram) : RunOutcome =
        match stepPrepared loggerFactory logger prepared with
        | ProgramStepOutcome.Completed outcome -> outcome
        | ProgramStepOutcome.Deadlocked (_, stuck) ->
            failwith $"Deadlock: no runnable threads and entry thread has not terminated. Stuck: {stuck}"
        | ProgramStepOutcome.InstructionStepped (prepared, _, _, _)
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
    /// `hostConfig.Guest.Kernel` carries the host's choices for the simulated process's kernel and
    /// is applied here rather than by the caller afterwards, because this function pumps the entry
    /// type's `.cctor` and CoreLib latches some of these values during static initialisation
    /// (notably `Environment.ProcessorCount`). `KernelConfig.Default` is the no-preference
    /// choice. Its `Environment` is overlaid on top of `EmulatedKernel.defaultEnvironment`, so
    /// callers that supply no overlay still get the seeded
    /// `DOTNET_SYSTEM_GLOBALIZATION_INVARIANT=1` default, and keys the caller does set win over
    /// it — that's how the CLI lets the host process override the seed if it really needs to.
    ///
    /// `hostConfig.PctSeed = Some s` selects the PCT scheduling policy seeded with `s`; `None` keeps the
    /// default round-robin policy. Applied before any cctor frame is pushed so the very first
    /// `chooseNext` decision is policy-correct — `IlMachineState.initial` defaults the field
    /// to `RoundRobin`, and `withPctSeed` simply overwrites it.
    let beginStartup
        (loggerFactory : ILoggerFactory)
        (originalPath : string option)
        (fileStream : Stream)
        (hostConfig : HostConfig)
        : Startup
        =
        let logger = loggerFactory.CreateLogger "Program"
        let dotnetRuntimeDirs = hostConfig.Guest.DotnetRuntimeDirs
        let kernelConfig = hostConfig.Guest.Kernel
        let pctSeed = hostConfig.PctSeed
        let argv = hostConfig.Guest.Argv

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
                    // Synthesised, not the entry point with its body swapped: the substituted
                    // body is not what `Main`'s MethodDef row describes, so carrying that row's
                    // identity would let anything keyed by it — debug information above all —
                    // describe this frame as though `Main` were running. It is not; `Main` has
                    // not been installed yet.
                    (MethodInfo.Synthesised (
                        { rawMainMethod.Core with
                            Body = MethodBody.Il (MethodInstructions.onlyRet ())
                        },
                        SynthesisedMethod.EntryPointPlaceholder
                    ))
                    None
                    dumped.DefinitionFullName
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
                    dumped.TypeDefs.[mainMethodFromMetadata.RequiredDeclaringType.Definition.Get]

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
                dumped.DefinitionFullName
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
        // The host's properties sit on top of PawPrint's own runtime baseline, which is how
        // "this runtime does not support dynamic code" reaches every guest without each host
        // having to remember to say so. Applied here rather than in `HostConfig.Default` so
        // that a host which builds its `HostConfig` some other way — the App, which replaces
        // `AppContext` wholesale with the guest's `runtimeconfig.json` — cannot drop it.
        let propertiesToSeed =
            AppContextProperties.withRuntimeBaseline hostConfig.Guest.AppContext

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
                // be mid-cctor on the entry type.
                failwith
                    "logic error: initial loadClass for entry point cannot block on another thread (no other threads exist yet)"

        /// Everything between the two guest-code phases: load the entry class and allocate argv.
        /// Runs no guest instructions of its own — `loadClass` only pushes cctor frames, which
        /// the class-initialisation phase then pumps.
        let enterClassInit (state : IlMachineState) : IlMachineState * ImmutableArray<CliType> =
            let state = loadInitialState state

            if mainTakesStringArrayArg then
                let arrayAllocation, state = allocateArgs loggerFactory argv baseClassTypes state
                state, ImmutableArray.Create (CliType.ofManagedObject arrayAllocation)
            else
                state, ImmutableArray.Empty

        let installMain (state : IlMachineState) (mainArgs : ImmutableArray<CliType>) : ProgramStartResult =
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
            | WhatWeDid.Aborted fatal ->
                // Triggered when initialising the entry point's declaring type tears the process
                // down. Startup has no `RunOutcome` to hand back at this point -- it is still
                // assembling the machine -- so the abort cannot be reported as one; surface it
                // rather than installing Main on a state whose process has already died.
                let message = fatal.Message |> Option.defaultValue "<no message>"

                failwith
                    $"TODO: initialising the entry point's declaring type aborted the process (%O{fatal.Code}): %s{message}"
            | WhatWeDid.SuspendedForClassInit -> failwith "TODO: suspended for class init"
            | WhatWeDid.SuspendedForManagedCall ->
                failwith "logic error: ensureTypeInitialised cannot suspend for an arbitrary managed call"
            | WhatWeDid.BlockedOnClassInit _ ->
                failwith "logic error: surely this thread can't be blocked on class init"
            | WhatWeDid.ThrowingTypeInitializationException ->
                failwith "TypeInitializationException during entry point type initialisation"
            | WhatWeDid.VoluntaryYield _ ->
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

        let atPhase (state : IlMachineState) (phase : StartupPhase) : Startup =
            {
                Prepared =
                    {
                        State = state
                        BaseClassTypes = baseClassTypes
                        EntryThread = mainThread
                        LastRan = mainThread
                    }
                Phase = phase
                InstallMain = installMain
            }

        match AppContextSeed.prepareCall loggerFactory baseClassTypes propertiesToSeed state with
        | None ->
            // Nothing to seed, so there is no first phase to pump: go straight to class
            // initialisation. The startup frame `computeState` installed is still in place,
            // never having been consumed.
            let state, mainArgs = enterClassInit state
            atPhase state (StartupPhase.InitialisingClasses mainArgs)
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

            let onSeeded (state : IlMachineState) : IlMachineState * ImmutableArray<CliType> =
                // The seed ran the entry thread to completion, consuming its startup frame; put
                // a fresh one back so the class-initialisation pump that follows is unaffected.
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
                |> enterClassInit

            atPhase state (StartupPhase.SeedingAppContext onSeeded)

    /// Advance startup by one guest instruction, crossing a phase boundary when the entry
    /// thread's current frame returns.
    let stepStartup (loggerFactory : ILoggerFactory) (logger : ILogger) (startup : Startup) : StartupStepOutcome =
        match stepPrepared loggerFactory logger startup.Prepared with
        | ProgramStepOutcome.InstructionStepped (prepared, ran, whatWeDid, effect) ->
            StartupStepOutcome.Stepped (
                { startup with
                    Prepared = prepared
                },
                ran,
                whatWeDid,
                effect
            )
        | ProgramStepOutcome.WorkerTerminated (prepared, terminated) ->
            StartupStepOutcome.WorkerTerminated (
                { startup with
                    Prepared = prepared
                },
                terminated
            )
        | ProgramStepOutcome.Deadlocked (prepared, stuck) ->
            StartupStepOutcome.Deadlocked (
                { startup with
                    Prepared = prepared
                },
                stuck
            )
        | ProgramStepOutcome.Completed outcome ->

        // `stepPrepared` reports `Completed` as soon as the entry thread terminates, which
        // during startup means the frame this phase was pumping has returned rather than that
        // the program is over.
        match startup.Phase, outcome with
        | StartupPhase.SeedingAppContext onReturn, RunOutcome.NormalExit (state, _) ->
            let state, mainArgs = onReturn state

            StartupStepOutcome.PhaseAdvanced
                { startup with
                    Prepared =
                        { startup.Prepared with
                            State = state
                        }
                    Phase = StartupPhase.InitialisingClasses mainArgs
                }
        | StartupPhase.SeedingAppContext _, outcome ->
            // Nothing in `AppContext.Setup` can legitimately exit, fail fast or throw: it
            // allocates a Dictionary and copies strings out of buffers we ourselves just
            // wrote. Anything else means a cctor dragged in by that work misbehaved, and
            // pressing on would run Main against a half-seeded AppContext.
            //
            // Describe the outcome by case rather than with `%O`: every `RunOutcome` carries
            // an `IlMachineState`, so structural formatting would render the entire heap into
            // the exception message.
            let described =
                match outcome with
                | RunOutcome.NormalExit _ -> "returned normally" // unreachable, matched above
                | RunOutcome.ProcessExit (_, thread) -> $"called Environment.Exit on %O{thread}"
                | RunOutcome.Aborted (_, thread, fatal) ->
                    let message = fatal.Message |> Option.defaultValue "<no message>"
                    $"aborted on %O{thread} with %O{fatal.Code}: %s{message}"
                | RunOutcome.SignalTerminated (_, signal) -> $"was terminated by signal %O{signal}"
                | RunOutcome.GuestUnhandledException (_, thread, exn) ->
                    $"threw an unhandled exception on %O{thread}: %O{exn.ExceptionObject}"

            failwith $"Seeding AppContext %s{described}."
        | StartupPhase.InitialisingClasses mainArgs, RunOutcome.NormalExit (state, _) ->
            StartupStepOutcome.Completed (startup.InstallMain state mainArgs)
        | StartupPhase.InitialisingClasses _, RunOutcome.GuestUnhandledException _
        | StartupPhase.InitialisingClasses _, RunOutcome.ProcessExit _
        | StartupPhase.InitialisingClasses _, RunOutcome.Aborted _
        | StartupPhase.InitialisingClasses _, RunOutcome.SignalTerminated _ ->
            // The entry thread's `.cctor` raised, or a worker spawned during cctor pumping
            // exited, failed fast, or took a terminating signal. In every case the CLR would
            // tear the process down; propagate rather than collapsing to a host `failwith`
            // that would mask the guest-level diagnostic, and rather than pressing on into
            // Main.
            StartupStepOutcome.Completed (ProgramStartResult.CompletedBeforeMain outcome)

    /// Reads the guest assembly and performs the one-time setup needed before Main is ready to
    /// schedule, running startup to completion.
    ///
    /// This is `beginStartup` driven by `stepStartup` in a loop. A driver that wants to observe
    /// startup — to stream a static initialiser's output, or to report where startup wedged
    /// rather than throwing out of it — should drive those two directly instead; guest code
    /// runs during startup, and this function gives back nothing until all of it has finished.
    ///
    /// See `beginStartup` for the kernel-config and PCT-seed timing contracts.
    let prepare
        (loggerFactory : ILoggerFactory)
        (originalPath : string option)
        (fileStream : Stream)
        (hostConfig : HostConfig)
        : ProgramStartResult
        =
        let logger = loggerFactory.CreateLogger "Program"

        let rec go (startup : Startup) : ProgramStartResult =
            match stepStartup loggerFactory logger startup with
            | StartupStepOutcome.Completed result -> result
            | StartupStepOutcome.Stepped (startup, _, _, _)
            | StartupStepOutcome.WorkerTerminated (startup, _)
            | StartupStepOutcome.PhaseAdvanced startup -> go startup
            | StartupStepOutcome.Deadlocked (_, stuck) ->
                failwith $"Deadlock during startup: no runnable threads and startup has not finished. Stuck: {stuck}"

        go (beginStartup loggerFactory originalPath fileStream hostConfig)

    /// Returns the outcome of the program run: normal exit or unhandled guest exception.
    ///
    /// `hostConfig.PctSeed` flows through to `prepare`: `Some s` selects PCT with seed `s`,
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

    /// A machine state sitting at a scheduler tick *boundary* whose next decision is contended:
    /// once this tick's preamble has run, more than one thread is Runnable, so which of them runs
    /// is a genuine choice — and it is the first such choice since this snapshot's run began.
    ///
    /// Contention is a property of the state the *policy*
    /// sees, which is not the state held here: a deadline expiring or the signal dispatcher waking
    /// can make a second thread Runnable inside the tick. So `State` may well show only one
    /// Runnable thread, and `Contenders` may name a thread that is blocked in it. Guests reaching
    /// their first fork organically do not show this — there the second thread arrives via the
    /// guest's own `Thread.Start`, which is a retired instruction — but `runToNextFork` from
    /// mid-run does, and a caller inspecting `State` should not expect otherwise.
    ///
    /// Why this is worth having: everything before a fork point is forced, so every scheduling
    /// policy makes the same choices there and — since `Scheduler` only ever mutates policy state
    /// at a contended decision — the policy state is still exactly what it was seeded with. A
    /// harness sweeping many PCT seeds over one guest can therefore compute this prefix *once*,
    /// under `RoundRobin`, and hand each seed a run bit-identical to what it would have produced
    /// from scratch. Measured on the `sourcesConcurrencyBugs` guests, that prefix is 74-94% of a
    /// run's instructions and ~90% of its wall clock.
    ///
    /// The state held is the one from *before* the tick's preamble, not from between the preamble
    /// and the choice: a mid-tick value would be a new kind of resumable
    /// thing, and handing it to the ordinary driver would run the preamble twice — advancing
    /// `StepCounter` twice and shifting the spurious-wakeup schedule. Resuming therefore re-runs
    /// the contended tick's preamble, which is policy-independent (see `advanceToDecision`) and
    /// so reproduces it exactly.
    ///
    /// Construct one only through `runToFirstFork` / `runToNextFork`: the representation is
    /// private because the type's whole value is the claim that the prefix behind it was forced,
    /// and a hand-built one would carry that claim without having earned it.
    type ForkSnapshot =
        private
            {
                Prepared : PreparedProgram
                Contending : ThreadId list
            }

        /// The machine as it stands at the fork point.
        member this.State : IlMachineState = this.Prepared.State

        /// The threads whose contention makes this a fork point: at least two, ascending by
        /// `ThreadId`. Runnable *at the decision point* — i.e. after this tick's preamble — which
        /// is not necessarily the same as Runnable in `State`. Ascending order is the order
        /// `PctState.ensurePriorityFor` samples in, so it is part of what makes a seeded
        /// schedule reproducible.
        member this.Contenders : ThreadId list = this.Contending

    /// How far a run got before it first had a scheduling choice to make.
    [<RequireQualifiedAccess>]
    type PrefixOutcome =
        /// Reached a contended decision. Resume with `resumeFork`, once per seed.
        | ForkedAt of ForkSnapshot
        /// The program ran to completion without ever reaching a contended decision. No policy
        /// had a choice anywhere, so this is the outcome under *every* seed, and a sweep is
        /// answered by this one run. (Its state's `Scheduling` is the `RoundRobin` the prefix ran
        /// under, where a from-scratch `Pct s` run would carry `Pct (ofSeed s)`; nothing
        /// guest-visible depends on the difference, but do not compare that field.)
        | NeverForked of RunOutcome
        /// Every thread blocked before any choice arose. Like `NeverForked`, seed-independent.
        | DeadlockedBeforeFork of stuckThreads : string
        /// A class initialiser started a thread, so the first contended decision happens during
        /// startup rather than in `Main`.
        ///
        /// Detected and refused rather than snapshotted. Snapshotting it is possible — the
        /// detector finds the exact point — but resuming it means handing the caller a
        /// half-finished `Startup` rather than a `PreparedProgram`, so `resumeFork` would have to
        /// return a two-shape value and every caller would have to drive both phases. No guest in
        /// this repository does it, so refuse loudly rather than build the surface. To lift the
        /// restriction, give `ForkSnapshot` a startup arm — nothing else here has to change.
        ///
        /// Carries the contenders rather than a rendered message, so a caller can decide what to
        /// do about the refusal (report it, fall back to per-seed runs).
        | ForkedDuringStartup of contenders : ThreadId list

    /// Guard against a yield retiring at a tick we classified as forced whose *post*-step state is
    /// contended.
    ///
    /// This is the one way a prefix could be seed-dependent despite every decision being forced.
    /// `Scheduler.onStepOutcome` wakes class-init waiters *before* charging the yield debt, so
    /// `chargeYieldDebt` reads contention against a Runnable set that may have grown since the
    /// choice was made. At such a tick a `Pct` policy would toss its honour coin — and could
    /// decline the yield where `RoundRobin` always honours it, which the guest sees directly in
    /// `Thread.Yield()`'s return value. A prefix containing one is not shareable.
    ///
    /// Unreachable today: a thread parked `BlockedOnClassInit` must have executed a step to get
    /// there, and a `.cctor` can only be `InProgress` on another thread, so two threads have
    /// already run and contention has already occurred. But that is a chain of facts about wake
    /// paths rather than a structural property, so check the conclusion and crash rather than
    /// silently emit a snapshot that does not commute.
    let private checkYieldDidNotStraddle (ran : ThreadId) (whatWeDid : WhatWeDid) (after : PreparedProgram) : unit =
        match whatWeDid with
        | WhatWeDid.VoluntaryYield _ ->
            match Scheduler.tryContenders after.State with
            | None -> ()
            | Some contenders ->
                failwith
                    $"Program: thread %O{ran} yielded at a tick whose scheduling decision was forced, but the state after the step is contended (Runnable: %A{contenders}). Scheduler.chargeYieldDebt reads contention after class-init waiters are woken, so a Pct policy would have drawn here — and could have declined the yield where RoundRobin honours it — which means the prefix up to this point is not seed-independent and must not be shared. See Scheduler.onStepOutcome."
        | WhatWeDid.Executed
        | WhatWeDid.Aborted _
        | WhatWeDid.SuspendedForClassInit
        | WhatWeDid.SuspendedForManagedCall
        | WhatWeDid.BlockedOnClassInit _
        | WhatWeDid.ThrowingTypeInitializationException -> ()

    /// Advance `prepared` until the next contended scheduling decision, returning the machine as
    /// it stood at the start of that tick.
    ///
    /// This is the general primitive: from a fresh `Main` it finds the *first* fork point, and
    /// from a mid-run state it finds the next one, which is what a future schedule-space tree
    /// search descends with. What "resume" means differs between those two — see
    /// `IlMachineState.withPctSeed` — but finding the point does not.
    ///
    /// Each *retired* tick's preamble runs exactly once: the probe consumes it and hands the
    /// advanced state straight to the decision half. The fork tick itself is the exception:
    /// its preamble runs here to answer the probe, and again on every resume.
    let rec runToNextFork
        (loggerFactory : ILoggerFactory)
        (logger : ILogger)
        (prepared : PreparedProgram)
        : PrefixOutcome
        =
        let advanced = annotating prepared.State (fun () -> advanceToDecision prepared)

        match Scheduler.tryContenders advanced.State with
        | Some contenders ->
            PrefixOutcome.ForkedAt
                {
                    Prepared = prepared
                    Contending = contenders
                }
        | None ->

        match annotating advanced.State (fun () -> stepDecided loggerFactory logger advanced) with
        | ProgramStepOutcome.Completed outcome -> PrefixOutcome.NeverForked outcome
        | ProgramStepOutcome.Deadlocked (_, stuck) -> PrefixOutcome.DeadlockedBeforeFork stuck
        | ProgramStepOutcome.WorkerTerminated (next, _) -> runToNextFork loggerFactory logger next
        | ProgramStepOutcome.InstructionStepped (next, ran, whatWeDid, _) ->
            checkYieldDidNotStraddle ran whatWeDid next
            runToNextFork loggerFactory logger next

    /// Read the guest assembly and run it — startup and all — up to its first contended
    /// scheduling decision.
    ///
    /// Takes a `GuestConfig` rather than a `HostConfig` precisely so that no seed can be passed:
    /// the prefix is the part of the run every seed shares, and it is computed under the
    /// randomness-free `RoundRobin` policy. `resumeFork` supplies the seed afterwards.
    let runToFirstFork
        (loggerFactory : ILoggerFactory)
        (originalPath : string option)
        (fileStream : Stream)
        (guestConfig : GuestConfig)
        : PrefixOutcome
        =
        let logger = loggerFactory.CreateLogger "Program"

        let hostConfig =
            {
                Guest = guestConfig
                PctSeed = None
            }

        let rec goStartup (startup : Startup) : PrefixOutcome =
            // Probe startup with the same predicate `runToNextFork` uses, so a `.cctor` that
            // starts a thread is reported rather than silently mistaken for a forced prefix. The
            // preamble runs twice per startup tick here, once for the probe and once inside
            // `stepStartup`; that is a handful of map operations against `executeOneStep`, and it
            // is paid once for a whole sweep rather than once per seed.
            let probed =
                annotating startup.Prepared.State (fun () -> advanceToDecision startup.Prepared)

            match Scheduler.tryContenders probed.State with
            | Some contenders -> PrefixOutcome.ForkedDuringStartup contenders
            | None ->

            match stepStartup loggerFactory logger startup with
            | StartupStepOutcome.Completed (ProgramStartResult.Ready prepared) ->
                runToNextFork loggerFactory logger prepared
            | StartupStepOutcome.Completed (ProgramStartResult.CompletedBeforeMain outcome) ->
                PrefixOutcome.NeverForked outcome
            | StartupStepOutcome.Deadlocked (_, stuck) -> PrefixOutcome.DeadlockedBeforeFork stuck
            | StartupStepOutcome.Stepped (startup, ran, whatWeDid, _) ->
                checkYieldDidNotStraddle ran whatWeDid startup.Prepared
                goStartup startup
            | StartupStepOutcome.WorkerTerminated (startup, _)
            | StartupStepOutcome.PhaseAdvanced startup -> goStartup startup

        goStartup (beginStartup loggerFactory originalPath fileStream hostConfig)

    /// Install a scheduling policy on a fork snapshot and hand back an ordinary `PreparedProgram`,
    /// to be driven with `stepPrepared` / `pumpPrepared` like any other.
    ///
    /// For a snapshot from `runToFirstFork`, `pctSeed = Some s` gives a run bit-identical to
    /// `Program.run` with `PctSeed = Some s` over the same image and `GuestConfig`: the prefix was
    /// forced, so the policy state a from-scratch run would hold here is exactly
    /// `PctState.ofSeed s`. See `IlMachineState.withPctSeed`, which spells out why that stops
    /// being true for a mid-run snapshot from `runToNextFork`.
    ///
    /// `None` installs no policy at all — it keeps whatever the snapshot carries. For a
    /// `runToFirstFork` snapshot that is the `RoundRobin` the prefix ran under, so it reproduces
    /// the default run; for a mid-run snapshot it is whatever policy got you there, mid-flight.
    ///
    /// `loggerFactory` rebinds the state's logging sink, which would otherwise still be the
    /// prefix's: every seed resumed from one snapshot would log through the factory the *prefix*
    /// was built with, losing whatever per-run properties the caller attaches. The prefix's own
    /// factory must outlive every resume regardless, because `BaseClassTypes` and the loaded
    /// assemblies were built against it.
    ///
    /// One thing a resumed run does *not* reproduce: `StepEffect`s retired during the prefix. A
    /// driver streaming guest output per step sees only post-fork effects. The final state's
    /// `Kernel.OutputLog` is still complete, because it came through the snapshot.
    let resumeFork
        (loggerFactory : ILoggerFactory)
        (pctSeed : uint64 option)
        (snapshot : ForkSnapshot)
        : PreparedProgram
        =
        let state =
            snapshot.Prepared.State |> IlMachineState.withLoggerFactory loggerFactory

        let state =
            match pctSeed with
            | None -> state
            | Some seed -> IlMachineState.withPctSeed seed state

        { snapshot.Prepared with
            State = state
        }

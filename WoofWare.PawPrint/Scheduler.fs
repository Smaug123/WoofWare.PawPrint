namespace WoofWare.PawPrint

/// The scheduler owns every ThreadStatus transition and the decision of which thread
/// runs next. It is a pure function of `IlMachineState`: the driver loop hands us an
/// outcome from `AbstractMachine.executeOneStep`, we fold that back into the thread
/// states, and we hand back a new state plus the id of the thread to step next.
///
/// Reading this module in isolation should tell you everything about how interleaving
/// works. Intrinsics in `AbstractMachine` that need to change a thread's status (e.g.
/// `Thread.Join` setting the caller to `BlockedOnJoin`) call into here rather than
/// mutating `ThreadStatus` inline, so the set of legal transitions is enumerable in
/// one file.
///
/// The long-term goal, towards Antithesis-style pruning over thread interleaving,
/// is for the scheduling policy to become pluggable — a harness will want to drive
/// `chooseNext` from outside. Keep this module free of logging and of anything that
/// isn't a pure state transformation so that swap is cheap.
[<RequireQualifiedAccess>]
module Scheduler =

    /// Enumerate the Runnable threads in ascending id order. Used by every
    /// policy: the set of candidates is policy-independent, only the choice
    /// among them differs. Kept private so policies stay enumerable here.
    let private runnableThreads (state : IlMachineState) : ThreadId list =
        state.ThreadState
        |> Map.toSeq
        |> Seq.choose (fun (tid, ts) ->
            match ts.Status with
            | ThreadStatus.Runnable -> Some tid
            | _ -> None
        )
        |> Seq.sortBy (fun (ThreadId i) -> i)
        |> Seq.toList

    /// Does any thread currently have status `Runnable`? Used by the
    /// deadline-advance loop in `Program.fs` to decide whether jumping the
    /// virtual clock has made progress; that check is policy-independent
    /// (every scheduler returns `None` from `chooseNext` iff no thread is
    /// Runnable), so callers should reach for this helper instead of
    /// invoking `chooseNext` and discarding its returned state.
    let hasAnyRunnable (state : IlMachineState) : bool =
        not (List.isEmpty (runnableThreads state))

    /// Pick the next thread to run, returning the (possibly-updated) machine
    /// state alongside the choice so that stochastic policies can thread
    /// their RNG state forward. The `RoundRobin` policy is pure in `state`
    /// (the returned state is `=` to the input) and reproduces the legacy
    /// deterministic ordering: among the Runnable threads, prefer the
    /// lowest id strictly greater than `lastRan`; if there isn't one, wrap
    /// to the lowest id overall. The policy is intentionally *not* sticky
    /// — staying on the most-recently-run thread minimises interleaving,
    /// which is the opposite of what a pruning harness wants.
    ///
    /// Returns `None` for the choice iff no thread is Runnable, which the
    /// driver treats as deadlock; the state is still returned so the caller
    /// always handles the same shape regardless of the outcome.
    let chooseNext (lastRan : ThreadId) (state : IlMachineState) : IlMachineState * ThreadId option =
        match state.Scheduling with
        | SchedulerState.RoundRobin ->
            let runnable = runnableThreads state

            let chosen =
                match runnable with
                | [] -> None
                | _ ->
                    let (ThreadId lastRanId) = lastRan

                    runnable
                    |> List.tryFind (fun (ThreadId i) -> i > lastRanId)
                    |> Option.orElse (List.tryHead runnable)

            state, chosen
        | SchedulerState.Pct _ ->
            // The PCT decision logic lands in a follow-up PR. Until then
            // nothing constructs `Pct _` (the default is `RoundRobin`), so
            // reaching this branch indicates a partially-wired harness;
            // fail loud rather than silently fall back to round-robin and
            // mask the mistake.
            failwith
                "Scheduler.chooseNext: PCT scheduling policy is not yet implemented; only RoundRobin is supported in this build."

    /// Set `thread`'s status. Used by the LowLevelMonitor state machine, which
    /// owns the registry-side bookkeeping (queues, owner) but routes every
    /// ThreadStatus flip through here so the scheduler stays the single place
    /// that mutates `ThreadStatus`.
    ///
    /// Not exposed for general use: every external caller should reach for a
    /// purpose-built helper (`blockOnJoin`, `blockOnMonitorAcquire`, etc.) so
    /// that the set of legal transitions stays enumerable in this module. Kept
    /// internal to the assembly so it does become a back door.
    let internal setThreadStatus (thread : ThreadId) (status : ThreadStatus) (state : IlMachineState) : IlMachineState =
        { state with
            ThreadState =
                state.ThreadState
                |> Map.change
                    thread
                    (Option.map (fun s ->
                        { s with
                            Status = status
                        }
                    ))
        }

    /// Transition `blocked` from Runnable to `BlockedOnJoin (target, deadlineMs)`.
    /// Called from the `Thread.Join` intrinsic; exposed here so the set of places
    /// that mutate `ThreadStatus` stays small and auditable.
    ///
    /// `deadlineMs = None` is an infinite wait (`Thread.Join()` /
    /// `Thread.Join(-1)`); `Some ms` is a finite timeout, expressed as the
    /// absolute virtual-clock millisecond at which the wait expires. The
    /// deadline-firing path in `Program.fireExpiredDeadlines` routes such
    /// threads through `fireJoinTimeout` below.
    let blockOnJoin
        (blocked : ThreadId)
        (target : ThreadId)
        (deadlineMs : int64 option)
        (state : IlMachineState)
        : IlMachineState
        =
        { state with
            ThreadState =
                state.ThreadState
                |> Map.change
                    blocked
                    (Option.map (fun s ->
                        { s with
                            Status = ThreadStatus.BlockedOnJoin (target, deadlineMs)
                        }
                    ))
        }

    /// Fire a deadline expiry for a thread parked in
    /// `BlockedOnJoin (_, Some _)`. Mirrors the `fireTimeout` family in
    /// `WaitHandle` / `LowLevelMonitor` / `SyncBlockMonitor`, but Join has
    /// no per-primitive wait queue — the "primitive" is just the target
    /// thread's status — so there is nothing to dequeue. We:
    ///
    ///  1. Pop the optimistic `Int32 1` slot pushed by `executeJoinCore`
    ///     at park time and push `Int32 0` in its place. The QCall
    ///     (`ThreadNative_Join`) returns `int32`; the InternalCall
    ///     (`Thread.Join(int)`) returns `bool`, but `CliType.ofBool`
    ///     normalises both to `EvalStackValue.Int32` on the eval stack,
    ///     so a single rewrite handles both call paths uniformly.
    ///  2. Flip the status back to `Runnable`. The new status carries no
    ///     deadline field, so the deadline is implicitly forgotten —
    ///     exactly the invariant the variant encodes.
    ///
    /// Fails loud if `thread` is not actually parked in `BlockedOnJoin`
    /// with a finite deadline: the only caller is
    /// `Program.fireExpiredDeadlines`, which enumerates the statuses
    /// itself, so a miss would indicate the deadline-firing path was
    /// reached for an untimed waiter — a structural bug worth surfacing
    /// here rather than silently popping a stack slot that doesn't
    /// belong to a Join optimistic-push.
    let fireJoinTimeout (thread : ThreadId) (state : IlMachineState) : IlMachineState =
        match state.ThreadState |> Map.tryFind thread with
        | None -> failwith $"Scheduler.fireJoinTimeout: thread %O{thread} has no ThreadState."
        | Some ts ->
            match ts.Status with
            | ThreadStatus.BlockedOnJoin (_, Some _) -> ()
            | other ->
                failwith
                    $"Scheduler.fireJoinTimeout: thread %O{thread} is not parked in BlockedOnJoin with a finite deadline (status: %O{other}); the scheduler observed a join deadline against a thread the join machinery does not know about."

        let _, state = IlMachineState.popEvalStack thread state
        let state = IlMachineState.pushToEvalStack' (EvalStackValue.Int32 0) thread state
        setThreadStatus thread ThreadStatus.Runnable state

    /// Park `blocked` in `BlockedOnSleep`, transitioning out of `Runnable`.
    /// Mirrors `blockOnJoin` but with no per-primitive wait queue: sleeping
    /// is purely time-driven, the wake comes from `Scheduler.fireSleepTimeout`
    /// when the virtual clock crosses the deadline. `deadlineMs = None` is
    /// an infinite sleep (`Thread.Sleep(-1)` / `Timeout.Infinite`); `Some _`
    /// is a finite timeout. No optimistic eval-stack push is performed
    /// because `Thread.Sleep` returns `void`.
    ///
    /// Caller is responsible for advancing the program counter past the
    /// `Sleep` call site before parking (so the wake resumes after the
    /// call), matching the contract used by every other QCall handler that
    /// blocks.
    let blockOnSleep (blocked : ThreadId) (deadlineMs : int64 option) (state : IlMachineState) : IlMachineState =
        { state with
            ThreadState =
                state.ThreadState
                |> Map.change
                    blocked
                    (Option.map (fun s ->
                        { s with
                            Status = ThreadStatus.BlockedOnSleep deadlineMs
                        }
                    ))
        }

    /// Fire a `Thread.Sleep` timeout: the deadline-firing path has
    /// observed that `thread` is parked in `BlockedOnSleep (Some _)` and
    /// the virtual clock has advanced past its deadline. Flip the status
    /// back to `Runnable` so the scheduler can resume the thread.
    ///
    /// Unlike `fireJoinTimeout` / `WaitHandle.fireTimeout` /
    /// `LowLevelMonitor.fireTimeout` / `SyncBlockMonitor.fireWaitTimeout`,
    /// there is no optimistic-push to rewrite: `Thread.Sleep(int)` returns
    /// `void`, so the call site advanced past itself without leaving an
    /// eval-stack slot behind. The wake therefore only needs to flip the
    /// status.
    ///
    /// Fails loud if `thread` is not actually parked in `BlockedOnSleep`
    /// with a finite deadline: the only caller is
    /// `Program.fireExpiredDeadlines`, which enumerates the statuses
    /// itself, so a miss would indicate the deadline-firing path was
    /// reached for an infinite (or non-sleep) waiter — a structural bug
    /// worth surfacing here.
    let fireSleepTimeout (thread : ThreadId) (state : IlMachineState) : IlMachineState =
        match state.ThreadState |> Map.tryFind thread with
        | None -> failwith $"Scheduler.fireSleepTimeout: thread %O{thread} has no ThreadState."
        | Some ts ->
            match ts.Status with
            | ThreadStatus.BlockedOnSleep (Some _) -> ()
            | other ->
                failwith
                    $"Scheduler.fireSleepTimeout: thread %O{thread} is not parked in BlockedOnSleep with a finite deadline (status: %O{other}); the scheduler observed a sleep deadline against a thread the sleep machinery does not know about."

        setThreadStatus thread ThreadStatus.Runnable state

    /// Record that `terminated` has finished executing its final `ret`.
    /// - Flips its own status to Terminated.
    /// - Wakes every thread that was BlockedOnJoin on it; they proceed past Join.
    /// - Fails loudly if `terminated` was still the `InProgress` initializer of any
    ///   type, because every thread waiting on that init would be stuck on a dead
    ///   blocker — a silent liveness bug. The real CLR wraps the dying cctor in a
    ///   TypeInitializationException; we don't synthesise one yet, so crash clearly.
    /// - Fails loudly if `terminated` was still the Owner of any LowLevelMonitor,
    ///   for the same reason: any thread parked in BlockedOnMonitorAcquire on that
    ///   monitor would be permanently stuck on a dead owner. CoreCLR's
    ///   `LowLevelMonitor` predicates "thread does not die holding the monitor" on
    ///   higher-level discipline (RAII in `LowLevelMonitorHelper`); we mirror that
    ///   contract with a loud failure rather than a silent deadlock.
    /// - Fails loudly if `terminated` was still the Owner of any mutex. Full
    ///   Win32 abandoned-mutex propagation (so the next waiter wakes with
    ///   `WAIT_ABANDONED` and `AbandonedMutexException`) is structural and
    ///   not yet implemented: the wake-time return value is pushed onto
    ///   blocked waiters' eval stacks at park time, so changing it requires
    ///   deferred-return-value materialisation. Until that lands, failing
    ///   loud here is correct-by-detection — a real guest reaching this case
    ///   surfaces as a clean crash rather than a silent permanent ownership
    ///   transfer.
    let onThreadTerminated (terminated : ThreadId) (state : IlMachineState) : IlMachineState =
        let orphanedInits =
            state.TypeInitTable
            |> Seq.choose (fun kvp ->
                match kvp.Value with
                | TypeInitState.InProgress t when t = terminated -> Some kvp.Key
                | _ -> None
            )
            |> Seq.toList

        match orphanedInits with
        | [] -> ()
        | _ ->
            // Waking the waiters wouldn't help: they'd re-observe `InProgress terminated`
            // and re-block on a dead thread, producing either a silent spin (waiters kept
            // Runnable by the scheduler but never unblocking) or a deadlock whose location
            // is far from the actual bug. Fail here so the blame is obvious.
            failwith
                $"Thread {terminated} terminated while still the InProgress initializer of {orphanedInits.Length} type(s); the real CLR would raise TypeInitializationException into every waiter, which we don't yet synthesise."

        let orphanedMonitors =
            state.Kernel.LowLevelMonitors
            |> Map.toSeq
            |> Seq.choose (fun (id, monitor) ->
                match monitor.Owner with
                | Some owner when owner = terminated -> Some id
                | _ -> None
            )
            |> Seq.toList

        match orphanedMonitors with
        | [] -> ()
        | _ ->
            failwith
                $"Thread {terminated} terminated while still owning {orphanedMonitors.Length} LowLevelMonitor(s) (%A{orphanedMonitors}); any thread parked in BlockedOnMonitorAcquire on those monitors would deadlock on a dead owner. The guest must Release before terminating."

        // Same contract for managed SyncBlocks (Monitor.Enter / `lock`): a terminating
        // thread must not still hold any SyncBlock, because any thread parked in
        // BlockedOnSyncBlockAcquire on that object would wait forever for ownership
        // transfer from a dead owner. Mirrors the LowLevelMonitor check above —
        // RAII-style release is the guest's responsibility, and a loud failure is far
        // easier to diagnose than a silent deadlock.
        let orphanedSyncBlocks =
            state.ManagedHeap.NonArrayObjects
            |> Map.toSeq
            |> Seq.choose (fun (addr, obj) ->
                match obj.SyncBlock.Lock with
                | SyncBlockLock.Held locked when locked.LockingThread = terminated -> Some (addr, locked)
                | _ -> None
            )
            |> Seq.toList

        match orphanedSyncBlocks with
        | [] -> ()
        | _ ->
            failwith
                $"Thread {terminated} terminated while still holding {orphanedSyncBlocks.Length} SyncBlock(s) (%A{orphanedSyncBlocks}); any thread parked in BlockedOnSyncBlockAcquire on those objects would deadlock on a dead owner. The guest must Monitor.Exit before terminating."

        // Same contract for Win32-shaped mutexes (Mutex.WaitOne / ReleaseMutex):
        // a terminating thread must not still own any mutex, because abandoned-
        // mutex wake-up propagation is not yet implemented. Real CoreCLR sets
        // a sticky abandoned flag and wakes waiters with WAIT_ABANDONED; we
        // can't (yet) rewrite the wake-time return value of already-blocked
        // waiters because their `WAIT_OBJECT_0` slot is pushed at park time.
        // Failing loud is correct-by-detection until that gap is closed.
        let orphanedMutexes =
            state.Kernel.WaitHandles
            |> Map.toSeq
            |> Seq.choose (fun (id, handle) ->
                match handle with
                | WaitHandleState.Mutex mutex ->
                    match mutex.Ownership with
                    | MutexOwnership.Held (owner, _) when owner = terminated -> Some id
                    | _ -> None
                | WaitHandleState.Semaphore _ -> None
                | WaitHandleState.Event _ -> None
            )
            |> Seq.toList

        match orphanedMutexes with
        | [] -> ()
        | _ ->
            failwith
                $"Thread {terminated} terminated while still owning {orphanedMutexes.Length} mutex(es) (%A{orphanedMutexes}); abandoned-mutex propagation is not yet implemented (the wake-time return value is pushed onto blocked waiters' eval stacks at park time, so producing WAIT_ABANDONED for them requires deferred-return-value materialisation). The guest must ReleaseMutex before terminating until that gap is closed."

        let threadState =
            state.ThreadState
            |> Map.change
                terminated
                (Option.map (fun s ->
                    { s with
                        Status = ThreadStatus.Terminated
                    }
                ))
            |> Map.map (fun _ ts ->
                match ts.Status with
                | ThreadStatus.BlockedOnJoin (target, _) when target = terminated ->
                    // Wake any joiner blocked on the terminating thread, regardless
                    // of whether its wait was infinite or finite. The new Runnable
                    // status carries no deadline, so a still-outstanding deadline
                    // is naturally forgotten — the next `fireExpiredDeadlines`
                    // pass projects `Runnable` to `None` in `waitDeadline` and
                    // moves on. The optimistic `Int32 1` pushed at park time
                    // stays on the eval stack as the Join's return value,
                    // mirroring CoreCLR's contract that Join returns `true`
                    // whenever control flows past it via target termination.
                    { ts with
                        Status = ThreadStatus.Runnable
                    }
                | _ -> ts
            )

        { state with
            ThreadState = threadState
        }

    /// Apply the init outcome of a freshly-spawned worker to its own ThreadStatus.
    /// Called once from `Thread.StartInternal` after `ensureTypeInitialised` has run
    /// on the new thread's declaring type.
    ///
    /// This is deliberately distinct from `onStepOutcome`: the worker has not taken
    /// a step, so the "wake threads blocked on `ran`" logic in `onStepOutcome` is
    /// the wrong semantics here (and vacuous in practice because no thread can yet
    /// be blocked on a just-created ThreadId). Keeping the two entry points separate
    /// means a reader tracing why a status changed lands in the right function.
    let onWorkerSpawned (worker : ThreadId) (initOutcome : WhatWeDid) (state : IlMachineState) : IlMachineState =
        match initOutcome with
        | WhatWeDid.Executed
        | WhatWeDid.VoluntaryYield
        | WhatWeDid.SuspendedForClassInit
        | WhatWeDid.SuspendedForManagedCall
        | WhatWeDid.ThrowingTypeInitializationException ->
            // The worker is free to run: either the type was already initialised
            // (Executed), a cctor frame was pushed on top of the target frame
            // (SuspendedForClassInit — the worker will run the cctor first, then
            // fall into the target method), a managed callee was pushed on top of a
            // native handler frame (SuspendedForManagedCall — the worker will run the
            // callee first, then re-enter the native handler), or the cached
            // TypeInitializationException was dispatched onto the worker's frames
            // (ThrowingTypeInit — the worker will run the exception handler /
            // terminate on its next step). In every case the worker stays Runnable.
            // SuspendedForManagedCall and VoluntaryYield aren't reachable from
            // `ensureTypeInitialised` (which is what feeds this entry point) today,
            // but listing them explicitly keeps the match exhaustive and documents
            // the intended treatment.
            state
        | WhatWeDid.BlockedOnClassInit blocker ->
            // Another thread is mid-init of the worker's declaring type. StartInternal
            // currently fails loud for this case (see the guard before this call), so
            // we shouldn't reach here; keep the branch for completeness and as the
            // obvious extension point when cross-thread class-init synchronisation for
            // workers lands.
            { state with
                ThreadState =
                    state.ThreadState
                    |> Map.change
                        worker
                        (Option.map (fun s ->
                            { s with
                                Status = ThreadStatus.BlockedOnClassInit blocker
                            }
                        ))
            }

    /// Apply the scheduler consequences of a single successful step by `ran`, given
    /// the `WhatWeDid` signal the abstract machine reported. Centralises every
    /// Runnable ↔ BlockedOnClassInit transition so that adding a new signal only
    /// touches this function.
    ///
    /// Note: on `Executed`, we speculatively wake every thread BlockedOnClassInit on
    /// `ran`. They'll re-check their blocker on their next turn and re-block if the
    /// cctor hasn't completed. This is correct but wasteful;
    /// it's cheap to fix once the scheduler owns the
    /// policy, which is only true after this refactor.
    let onStepOutcome (ran : ThreadId) (outcome : WhatWeDid) (state : IlMachineState) : IlMachineState =
        match outcome with
        // VoluntaryYield is identical in its scheduler effect to Executed: the yielder
        // made forward progress, so any thread parked BlockedOnClassInit on `ran` must
        // be woken to re-check its blocker. The hint (that the guest *asked* to yield)
        // is preserved for the driver-loop boundary — `chooseNext`'s current signature
        // doesn't consume the previous outcome, so the round-robin policy is hint-
        // insensitive today, but the variant exists so a future fuzz/pruning policy
        // can branch here without a wider refactor.
        | WhatWeDid.Executed
        | WhatWeDid.VoluntaryYield ->
            let threadState =
                state.ThreadState
                |> Map.map (fun _ ts ->
                    match ts.Status with
                    | ThreadStatus.BlockedOnClassInit blocker when blocker = ran ->
                        { ts with
                            Status = ThreadStatus.Runnable
                        }
                    | _ -> ts
                )

            { state with
                ThreadState = threadState
            }
        | WhatWeDid.SuspendedForClassInit
        | WhatWeDid.SuspendedForManagedCall ->
            // Mid-call work: another frame is now on top of `ran` and will run on its next turn.
            // No scheduler-level transition is required for any thread; in particular, threads
            // BlockedOnClassInit on `ran` stay parked because `ran`'s class init has not finished.
            state
        | WhatWeDid.BlockedOnClassInit blocker ->
            { state with
                ThreadState =
                    state.ThreadState
                    |> Map.change
                        ran
                        (Option.map (fun s ->
                            { s with
                                Status = ThreadStatus.BlockedOnClassInit blocker
                            }
                        ))
            }
        | WhatWeDid.ThrowingTypeInitializationException ->
            // `ran`'s .cctor failed and the type is now in TypeInitState.Failed. Any
            // thread that was parked BlockedOnClassInit behind `ran` must be woken so
            // it can re-enter its call site, hit ensureTypeInitialised, and observe the
            // cached TypeInitializationException. Leaving them blocked would deadlock
            // the program even though the failure is recoverable via a catch.
            let threadState =
                state.ThreadState
                |> Map.map (fun _ ts ->
                    match ts.Status with
                    | ThreadStatus.BlockedOnClassInit blocker when blocker = ran ->
                        { ts with
                            Status = ThreadStatus.Runnable
                        }
                    | _ -> ts
                )

            { state with
                ThreadState = threadState
            }

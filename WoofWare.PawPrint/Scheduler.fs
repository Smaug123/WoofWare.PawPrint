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

    /// Base per-step demotion probability when the imminent op is classified
    /// `AlwaysGuestVisible` (weight 1.0). Lower bands scale this down linearly
    /// via `ContextSwitchPrior.weight`: `RarelyGuestVisible` (0.1) gives 0.1%,
    /// `InterpreterOnly` (0.01) gives 0.01%, `Never` (0.0) gives 0%. The 1% cap
    /// is a calibration choice: high enough that a multi-thousand-step run
    /// reliably exercises interleaving, low enough that bursty native-step
    /// sequences don't churn the schedule on every dispatch. Tuned empirically;
    /// surface as a knob if a future harness needs to fuzz the constant itself.
    let private P_BASE : double = 0.01

    /// Probability that the `Pct` policy honours a guest yield (`Thread.Yield()`,
    /// `Thread.Sleep(0)`) by charging the yielder a `ThreadState.YieldDebt`. On the
    /// complementary draw the yield is declined: nothing happens, exactly as when a real
    /// `SwitchToThread` returns without switching.
    ///
    /// Declines exist for *reachability*, not for frequency. A schedule in which a yield is
    /// not respected is a real execution — it is why `Thread.Yield()` returns a `bool` at all
    /// — so a scheduler that honoured every yield would delete those schedules from the space
    /// a fuzzing harness can explore. But the common real-world decline, "no other thread was
    /// ready", is already modelled exactly and deterministically by the empty-debt branch of
    /// `chargeYieldDebt`; what this constant buys is the *rarer* case where a peer was ready
    /// and the OS declined anyway. Hence a value near 1: at 0.9 the fairness effect engages
    /// within a couple of yields, while a spinner's measured 16-yield `SpinWait` warmup phase
    /// contains at least one decline with probability ~0.81.
    ///
    /// `RoundRobin` does not draw at all — see `chargeYieldDebt`. Like `P_BASE` this is a
    /// hand-calibrated constant rather than a derived one; deriving it from the runnable-set
    /// size buys nothing, since retries are geometric at any constant rate.
    let private P_HONOUR_YIELD : double = 0.9

    /// Enumerate the Runnable threads in ascending id order. Used by every
    /// policy: the set of candidates is policy-independent, only the choice
    /// among them differs. Kept private so policies stay enumerable here.
    /// Runnable threads in ascending `ThreadId` order.
    ///
    /// Written as a fold rather than a `Map.toSeq |> Seq.choose |> Seq.sortBy |> Seq.toList`
    /// pipeline because this runs on every scheduler tick, i.e. once per interpreted IL
    /// instruction, and that pipeline allocates several enumerators plus the sort's scratch
    /// array per call. `Map.foldBack` visits keys in descending order, so consing during the
    /// fold produces the ascending list directly; `ThreadId` is a single-field wrapper over the
    /// `int`, so map-key order *is* the `ThreadId i` order the old `sortBy` asked for.
    let private runnableThreads (state : IlMachineState) : ThreadId list =
        (state.ThreadState, [])
        ||> Map.foldBack (fun tid ts acc ->
            match ts.Status with
            | ThreadStatus.Runnable -> tid :: acc
            | _ -> acc
        )

    /// Is `thread`'s yield debt discharged, given the currently-Runnable set? A debt member
    /// that is no longer Runnable has left the run queue and cannot be waited for, so it stops
    /// counting; this is what makes the debt self-clearing across a park/wake cycle, with no
    /// hook in any wake path.
    ///
    /// The `IsEmpty` test is not merely a fast path for its own sake — it is the common case,
    /// and keeping it reachable is why `onThreadTerminated` prunes rather than relying on this
    /// filter alone. A debt holding a permanently-unrunnable member would answer correctly
    /// here forever while scanning the whole runnable list to do it.
    let private debtDischarged (runnable : ThreadId list) (ts : ThreadState) : bool =
        ts.YieldDebt.IsEmpty
        || not (runnable |> List.exists (fun tid -> ts.YieldDebt |> Set.contains tid))

    /// The Runnable threads the policy may choose among: those whose yield debt is discharged.
    /// Every policy filters through here, so fairness is a property of the schedule space
    /// rather than of any one policy.
    ///
    /// **This is never empty when `runnableThreads` is non-empty**, which is what keeps
    /// `chooseNext`'s deadlock contract (`None` iff nothing is Runnable) intact. Proof: a debt
    /// is only ever charged to a thread during that thread's own step, so any thread holding
    /// one has run at least once; and every live member of that debt was Runnable when the
    /// debt was charged and has not run since (a member that ran was discharged by
    /// `dischargeYieldDebts`). So a thread with live debt necessarily ran more recently than
    /// every live member of its debt. Now take the Runnable thread whose last run is least
    /// recent, counting "never ran" as least recent of all — a never-run thread has an empty
    /// debt by construction, and any other candidate for that position cannot hold live debt,
    /// since its members would have to be Runnable and yet less recent still. Either way that
    /// thread is a candidate.
    ///
    /// The proof leans on the discharge lemma — *if a thread ran, it left every debt* — which
    /// in turn requires every step to reach `onStepOutcome`. Rather than trust that, we check
    /// the conclusion and fail loudly: a silent fallback to the unfiltered set would convert a
    /// structural bug into a subtle fairness anomaly, and this project would rather crash.
    let private candidates (state : IlMachineState) : ThreadId list =
        let runnable = runnableThreads state

        match runnable with
        | [] -> []
        | _ ->

        let eligible =
            runnable
            |> List.filter (fun tid -> debtDischarged runnable (Map.find tid state.ThreadState))

        match eligible with
        | [] ->
            let debts =
                runnable
                |> List.map (fun tid -> tid, (Map.find tid state.ThreadState).YieldDebt)

            failwith
                $"Scheduler.candidates: every Runnable thread has an outstanding yield debt (%A{debts}), which the debt invariant makes impossible — a thread's debt can only name threads that were Runnable when it was charged, and members are discharged as they run. Reaching here means some execution path retired a step without routing its outcome through Scheduler.onStepOutcome, so debts are no longer being discharged."
        | _ -> eligible

    /// Remove `ran` from every outstanding yield debt: it has taken its step, so anyone waiting
    /// to see it run has been satisfied. Called for every step outcome, since every step
    /// discharges, not just yields.
    ///
    /// Short-circuits on the common path the way the `BlockedOnClassInit` wake scan next door
    /// does: this runs once per interpreted IL instruction and almost never has anything to do,
    /// because outstanding debts exist only during yield bursts.
    let private dischargeYieldDebts (ran : ThreadId) (state : IlMachineState) : IlMachineState =
        let anyDebtNames =
            state.ThreadState |> Map.exists (fun _ ts -> ts.YieldDebt |> Set.contains ran)

        if not anyDebtNames then
            state
        else

        { state with
            ThreadState =
                state.ThreadState
                |> Map.map (fun _ ts ->
                    // Guard the rewrite so untouched threads keep their existing record rather
                    // than being reallocated with an identical debt.
                    if ts.YieldDebt |> Set.contains ran then
                        { ts with
                            YieldDebt = ts.YieldDebt |> Set.remove ran
                        }
                    else
                        ts
                )
        }

    /// Does any thread currently have status `Runnable`? Used by the
    /// deadline-advance loop in `Program.fs` to decide whether jumping the
    /// virtual clock has made progress; that check is policy-independent
    /// (every scheduler returns `None` from `chooseNext` iff no thread is
    /// Runnable), so callers should reach for this helper instead of
    /// invoking `chooseNext` and discarding its returned state.
    let hasAnyRunnable (state : IlMachineState) : bool =
        // Deliberately not `runnableThreads`: the caller only asks whether the set is empty, and
        // this is on the per-tick path, so answer without materialising the list at all.
        state.ThreadState
        |> Map.exists (fun _ ts ->
            match ts.Status with
            | ThreadStatus.Runnable -> true
            | _ -> false
        )

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
            let runnable = candidates state

            let chosen =
                match runnable with
                | [] -> None
                | _ ->
                    let (ThreadId lastRanId) = lastRan

                    runnable
                    |> List.tryFind (fun (ThreadId i) -> i > lastRanId)
                    |> Option.orElse (List.tryHead runnable)

            state, chosen
        | SchedulerState.Pct pct ->
            // Priorities are sampled over the *unfiltered* Runnable set, but the argmax runs
            // over the candidates. Keeping the sampling domain unfiltered preserves the
            // documented invariant that the sampling sequence is a function of the seed plus
            // the set of threads ever seen Runnable — if the filter drove it, the RNG stream
            // would depend on yield timing.
            let runnable = runnableThreads state
            let eligible = candidates state

            match runnable with
            | [] ->
                // No Runnable threads — deadlock signal. State is returned
                // unchanged (no RNG advance) so a quiescent probe followed by
                // a wake-up resumes from the same PRNG position as if the probe
                // never happened, keeping replay bit-exact across the boundary.
                state, None
            | _ ->
                // Lazy first-observation insert: any Runnable thread without a
                // priority gets one sampled in ascending-ThreadId order, so the
                // sampling sequence is determined by the seed plus the set of
                // threads that have ever been seen Runnable, not by the order
                // in which they were created.
                let pct = PctState.ensurePriorityFor runnable pct

                // Deterministic argmax over `eligible`. F#'s `List.maxBy` keeps
                // the first element on ties (it uses strict `>`); `eligible`
                // is sorted by ThreadId ascending, so ties resolve to the
                // lowest id — purely for reproducibility, since with `nextDouble`
                // sampling from a 53-bit mantissa a tie is astronomically rare.
                // `Map.find` is total here: `eligible` is a subset of `runnable`,
                // which `ensurePriorityFor` has just covered.
                let argmax (priorities : Map<ThreadId, double>) : ThreadId =
                    eligible |> List.maxBy (fun tid -> Map.find tid priorities)

                let current = argmax pct.Priorities

                // Classify the imminent op of `current` to weight the demotion
                // probability. `None` (the active frame is native — InternalCall,
                // PInvoke, or RuntimeProvided) is treated as AlwaysGuestVisible
                // (weight 1.0): a native step runs as one atomic block from the
                // scheduler's viewpoint and almost always has observable effects,
                // so it's the most interesting interleaving point we can see.
                let weight =
                    match ThreadState.peekNextOp (Map.find current state.ThreadState) with
                    | Some op -> ContextSwitchPrior.weight (ContextSwitchPrior.ofIlOp op)
                    | None -> 1.0

                // Single weighted-Bernoulli draw against `weight * P_BASE`. We
                // always burn one RNG step here, regardless of weight, so
                // `Pct` schedules consume the seed at a predictable rate
                // (one `nextDouble` per `chooseNext` call) and `weight = 0.0`
                // is correctly a no-op without a branch that skips the draw.
                let sample, rng = NonCryptoRandom.nextDouble pct.Rng

                let pct =
                    { pct with
                        Rng = rng
                    }

                if sample < weight * P_BASE then
                    // Demote: resample `current`'s priority and recompute the
                    // argmax. The new priority is uniform-on-[0, 1), so demotion
                    // may yield a higher value than the old one (in which case
                    // `current` wins again) — by design, since the per-step
                    // weight system is a Bernoulli "consider switching here"
                    // signal, not a guaranteed switch. The effective switch
                    // rate is `weight * P_BASE * P(some other thread now has
                    // a higher priority)`, which the PCT statistics naturally
                    // approach asymptotically.
                    let pct = PctState.resamplePriority current pct
                    let chosen = argmax pct.Priorities

                    let state =
                        { state with
                            Scheduling = SchedulerState.Pct pct
                        }

                    state, Some chosen
                else
                    let state =
                        { state with
                            Scheduling = SchedulerState.Pct pct
                        }

                    state, Some current

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

        let state =
            IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim 0)) thread state

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
            state.ManagedHeap.SyncBlocks
            |> Map.toSeq
            |> Seq.choose (fun (addr, syncBlock) ->
                match syncBlock.Lock with
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

        // If a Pct schedule is in effect, drop the terminated thread's
        // priority entry so the next argmax can never see a stale slot.
        // In practice the slot would never be picked (the terminated thread
        // is not Runnable), but keeping the map domain "ever-seen-and-not-
        // terminated" is easier to reason about than "ever-seen", and bounds
        // the map size by the live-thread count rather than the all-time
        // thread count. No-op for `RoundRobin`.
        let scheduling =
            match state.Scheduling with
            | SchedulerState.RoundRobin -> SchedulerState.RoundRobin
            | SchedulerState.Pct pct -> SchedulerState.Pct (PctState.removeThread terminated pct)

        let state =
            { state with
                ThreadState = threadState
                Scheduling = scheduling
            }

        // Discharge `terminated` from every outstanding yield debt. A thread's *final* step is
        // its bottom-frame `Ret`, which the driver surfaces as `ExecutionResult.Terminated` and
        // routes here rather than through `onStepOutcome` — so without this, the one step that
        // most conclusively satisfies "I am waiting to see you run" would be the one step that
        // never discharges anything.
        //
        // Not needed for correctness: `candidates` intersects each debt with the live Runnable
        // set, and a Terminated thread is never in it, so a stale member could not hold anyone
        // out. It is needed for cost. A debt containing a terminated id never becomes empty, so
        // its owner permanently misses the `IsEmpty` fast path in `debtDischarged` and pays a
        // scan of the runnable list on every scheduling decision for the rest of the run —
        // turning candidate selection from O(R) into O(R²) once a few threads have yielded and
        // a peer has exited. Pruning here keeps the fast path reachable.
        dischargeYieldDebts terminated state

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
        | WhatWeDid.VoluntaryYield _
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
    /// Act on a guest yield by `ran`: draw the policy's honour coin and, if it comes up
    /// honoured, charge `ran` a yield debt naming every *other* currently-Runnable thread, so
    /// `candidates` holds it out until they have each taken a step. Returns the new state and
    /// whether a switch away from `ran` is now guaranteed.
    ///
    /// That returned flag is an exact iff, not an approximation, and it is what
    /// `Thread.Yield()` reports to the guest: a non-empty debt excludes `ran` from
    /// `candidates` until its members run, so under *any* policy somebody else runs first;
    /// an empty debt (nobody else was Runnable, or the coin declined) excludes nothing, so
    /// nothing is guaranteed. This is why the debt lives in `ThreadState` rather than the
    /// yield being a transient hint — the guarantee has to be inspectable to be reported.
    ///
    /// `RoundRobin` takes no draw at all and always honours. That is a deliberate asymmetry
    /// rather than `P_HONOUR_YIELD = 1.0` through a shared path: `RoundRobin` is the
    /// reproducible baseline and is documented as consuming no randomness whatsoever, so
    /// introducing a draw there would break that contract for every existing run. `Pct` is the
    /// exploration policy, and "the OS declined to switch" is an exploration feature.
    ///
    /// Under `Pct` the draw is unconditional — burned even when `ran` is the only Runnable
    /// thread and the outcome cannot matter — matching the always-burn Bernoulli in
    /// `chooseNext`, so the seed is consumed at a rate that depends only on the sequence of
    /// yields and not on how many threads happened to be Runnable at each one.
    let private chargeYieldDebt (ran : ThreadId) (state : IlMachineState) : IlMachineState * bool =
        let others = runnableThreads state |> List.filter (fun tid -> tid <> ran)

        let state, honour =
            match state.Scheduling with
            | SchedulerState.RoundRobin -> state, true
            | SchedulerState.Pct pct ->
                let sample, rng = NonCryptoRandom.nextDouble pct.Rng

                let state =
                    { state with
                        Scheduling =
                            SchedulerState.Pct
                                { pct with
                                    Rng = rng
                                }
                    }

                state, sample < P_HONOUR_YIELD

        if not honour || List.isEmpty others then
            state, false
        else

        let state =
            { state with
                ThreadState =
                    state.ThreadState
                    |> Map.change
                        ran
                        (Option.map (fun ts ->
                            { ts with
                                YieldDebt = Set.ofList others
                            }
                        ))
            }

        state, true

    let onStepOutcome (ran : ThreadId) (outcome : WhatWeDid) (state : IlMachineState) : IlMachineState =
        // Every step discharges, whatever else it did: `ran` has taken its turn, so any thread
        // waiting to see it run is satisfied. Must happen for all outcomes, including the
        // blocking ones — a thread that blocks has still run, and `candidates`' non-emptiness
        // proof depends on that being true without exception.
        let state = dischargeYieldDebts ran state

        // A yielder made forward progress just as an ordinary `Executed` step does, so both
        // wake any thread parked BlockedOnClassInit on `ran`. They diverge only afterwards:
        // a yield additionally goes to the back of the run queue.
        let wakeClassInitWaiters (state : IlMachineState) : IlMachineState =
            // This runs on every scheduler tick (once per interpreted IL instruction),
            // and almost never has anything to do, so short-circuit first.
            let anyBlockedOnRan =
                state.ThreadState
                |> Map.exists (fun _ ts ->
                    match ts.Status with
                    | ThreadStatus.BlockedOnClassInit blocker -> blocker = ran
                    | _ -> false
                )

            if not anyBlockedOnRan then
                state
            else

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

        match outcome with
        | WhatWeDid.Executed -> wakeClassInitWaiters state
        | WhatWeDid.VoluntaryYield reportsSwitch ->
            // Wake first, then charge: the run queue the yielder goes to the back of is the
            // one that exists at the end of its step, so a thread this very step unblocked is
            // owed a turn too. If it promptly re-blocks it drops out of the debt anyway.
            let state = wakeClassInitWaiters state
            let state, switched = chargeYieldDebt ran state

            if not reportsSwitch then
                state
            else

            // Optimistic-push-then-rewrite, as `fireJoinTimeout` does for Join's return value:
            // the handler could not know whether the switch would happen (that is the
            // scheduler's decision, taken here), so it left `Interop.BOOL.FALSE` behind for us
            // to correct. Pop-and-repush unconditionally, checking what we find, so that a
            // handler which sets `reportsSwitch` without leaving the slot fails loudly rather
            // than silently corrupting its caller's eval stack.
            let popped, state = IlMachineState.popEvalStack ran state

            match popped with
            | EvalStackValue.Int32 _ -> ()
            | other ->
                failwith
                    $"Scheduler.onStepOutcome: thread %O{ran} reported a VoluntaryYield with reportsSwitch=true, so the yield handler should have left an optimistic Interop.BOOL.FALSE on its eval stack for rewriting, but the top of stack was %O{other}. Either the handler failed to push, or a handler that returns void passed reportsSwitch=true."

            let result = if switched then 1 else 0

            IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim result)) ran state
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

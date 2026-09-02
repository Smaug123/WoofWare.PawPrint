namespace WoofWare.PawPrint

open WoofWare.PosixKernel

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
///
/// **A stochastic policy consumes randomness if and only if the decision is contended**, i.e.
/// iff more than one thread is Runnable. When it is not, every policy makes the same choice —
/// there is only one thread to make it about — so the step is not a decision and no policy
/// state may change. That is what lets a harness compute the single-threaded prefix of a run
/// *once* and fan every seed out from the first contended decision, instead of re-executing an
/// identical startup per seed: over a forced prefix, `SchedulerState.Pct (PctState.ofSeed s)` is
/// unchanged, so the prefix can be computed under `RoundRobin` and the seed installed at the
/// fork. See `Contention`, which carries the witness that entitles a policy to draw.
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
    let private runnableThreads (state : IlMachineState) : ThreadId list =
        // Written as a fold rather than a `Map.toSeq |> Seq.choose |> Seq.sortBy |> Seq.toList`
        // pipeline because this runs on every scheduler tick, i.e. once per interpreted IL
        // instruction, and that pipeline allocates several enumerators plus the sort's scratch
        // array per call. `Map.foldBack` visits keys in descending order, so consing during the
        // fold produces the ascending list directly; `ThreadId` is a single-field wrapper over the
        // `int`, so map-key order *is* ascending `ThreadId` order.
        (state.ThreadState, [])
        ||> Map.foldBack (fun tid ts acc ->
            match ts.Status with
            | ThreadStatus.Runnable -> tid :: acc
            | _ -> acc
        )

    /// How much choice the scheduler has at the imminent decision, and — for the one case where
    /// there is any — the witness that entitles a stochastic policy to draw.
    ///
    /// The `Contended` case carries its first two members separately rather than as a plain
    /// list, so "at least two threads are Runnable" is a fact about the *type* rather than one
    /// maintained by discipline. There is deliberately no way to reach `PctState` from the other
    /// two cases in `chooseNext`: the policy match sits inside the `Contended` branch, so a
    /// future edit that wants to draw on a forced decision has nothing to draw from and has to
    /// restructure the function to do it.
    ///
    /// Contention is defined over the *unfiltered* Runnable set, not over `candidates`. A thread
    /// held out by an outstanding yield debt still counts: the draws taken at such a tick shape
    /// later choices, so the tick is genuinely part of the seed's identity even though this
    /// tick's pick may be forced. Defining it the other way would also desynchronise the
    /// predicate from `PctState.ensurePriorityFor`'s sampling domain, which is the unfiltered
    /// set (see `chooseNext`).
    [<RequireQualifiedAccess>]
    type private Contention =
        /// Nothing is Runnable: the driver's deadlock signal.
        | NoRunnable
        /// Exactly one thread is Runnable. Every policy picks it, so this is not a decision.
        | Forced of ThreadId
        /// At least two threads are Runnable, so which one runs next is a genuine choice.
        | Contended of first : ThreadId * second : ThreadId * rest : ThreadId list

        /// The Runnable threads this classification was derived from, in ascending `ThreadId`
        /// order.
        member this.Runnable : ThreadId list =
            match this with
            | Contention.NoRunnable -> []
            | Contention.Forced only -> [ only ]
            | Contention.Contended (first, second, rest) -> first :: second :: rest

    /// Examine the runnable threads to determine whether the scheduler has
    /// a real ("contended") decision to make, or whether it's about to be forced to select
    /// a specific thread to execute.
    let private classify (state : IlMachineState) : Contention =
        match runnableThreads state with
        | [] -> Contention.NoRunnable
        | [ only ] -> Contention.Forced only
        | first :: second :: rest -> Contention.Contended (first, second, rest)

    /// The Runnable threads contending for the imminent scheduling decision, if there is a
    /// genuine choice to be made.
    ///
    /// Returns `None`, or `Some` of at least two threads in ascending `ThreadId` order,
    /// indicating the multiple contending Runnable threads.
    /// Equivalently: `Some` iff a stochastic policy may consume randomness at this tick,
    /// and iff this tick is a fork point in the schedule space.
    ///
    /// This answers about the state you hand it, and the state that matters is the one the
    /// scheduler is about to act on: the *post*-preamble state. Spurious wakeups, deadline
    /// firing, signal-handler spawn and the deadline jump can each make a second thread Runnable
    /// within a tick, so asking about the inter-tick state answers a different question and will
    /// miss those forks.
    ///
    /// Internal because that state cannot be built from outside this assembly — `Program`'s
    /// per-tick preamble is private — so an external caller could only ever ask the wrong
    /// question. Harnesses that want to find fork points use `Program.runToFirstFork` /
    /// `Program.runToNextFork`, which sequence the preamble and this probe correctly.
    let internal tryContenders (state : IlMachineState) : ThreadId list option =
        match classify state with
        | Contention.Contended _ as contention -> Some contention.Runnable
        | Contention.NoRunnable
        | Contention.Forced _ -> None

    /// Is `thread`'s yield debt discharged, given the currently-Runnable set? A debt member
    /// that is no longer Runnable has left the run queue and cannot be waited for, so it stops
    /// counting; this is what makes the debt self-clearing across a park/wake cycle, with no
    /// hook in any wake path.
    let private debtDischarged (runnable : ThreadId list) (ts : ThreadState) : bool =
        // The `IsEmpty` test is just a fast path; the global semantics are such that
        // we'd come to the same decision without it, but more slowly.
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
    /// to see it run has been satisfied.
    ///
    /// **This must be applied to every retired step, whatever that step turned out to be**, and
    /// there is exactly one caller: the driver applies it to the result of
    /// `AbstractMachine.executeOneStep` via `ExecutionResult.mapState`, before it looks at which
    /// outcome it got. Do not add a second call site, and in particular do not move it into the
    /// driver's per-outcome arms. Two outcomes there do not look like "a thread ran" and are
    /// easy to overlook:
    ///
    ///   * a thread's *final* step leaves the driver as `ExecutionResult.Terminated`, though it
    ///     is the step that most conclusively satisfies "I am waiting to see you run";
    ///   * the entry thread's synthetic `onlyRet` frame reports `NormalExit` even in the
    ///     pre-`Main` pump, after which that same thread is resurrected and keeps running.
    ///
    /// Missing either does not break correctness, because `candidates` filters debts against the
    /// live Runnable set — but it breaks the discharge lemma that `candidates`' non-emptiness
    /// proof rests on, and missing the first also makes the `IsEmpty` fast path below
    /// permanently unreachable for any thread that yielded before a peer exited.
    let dischargeYieldDebts (ran : ThreadId) (state : IlMachineState) : IlMachineState =
        let anyDebtNames =
            state.ThreadState |> Map.exists (fun _ ts -> ts.YieldDebt |> Set.contains ran)

        if not anyDebtNames then
            // Short-circuit on the common path the way the `BlockedOnClassInit` wake scan does: this
            // function runs once per interpreted IL instruction and almost never has anything to do,
            // because outstanding debts exist only during yield bursts.
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
    /// (the returned state is `=` to the input) and uses a deterministic
    /// ordering: among the Runnable threads, prefer the
    /// lowest id strictly greater than `lastRan`; if there isn't one, wrap
    /// to the lowest id overall. The policy is intentionally *not* sticky
    /// — staying on the most-recently-run thread minimises interleaving,
    /// which is the opposite of what a pruning harness wants.
    ///
    /// Returns `None` for the choice iff no thread is Runnable, which the
    /// driver treats as deadlock; the state is still returned so the caller
    /// always handles the same shape regardless of the outcome.
    ///
    /// The uncontended cases are handled ahead of the policy match, and are therefore
    /// policy-independent *in the code* rather than by two branches that happen to agree: with
    /// no Runnable thread there is nothing to choose, and with one there is nothing to choose
    /// *between*. Only the contended branch can see a `PctState` at all.
    let chooseNext (lastRan : ThreadId) (state : IlMachineState) : IlMachineState * ThreadId option =
        match classify state with
        | Contention.NoRunnable ->
            // No Runnable threads — deadlock signal. State is returned
            // unchanged (no RNG advance) so a quiescent probe followed by
            // a wake-up resumes from the same PRNG position as if the probe
            // never happened, keeping replay bit-exact across the boundary.
            state, None
        | Contention.Forced only ->
            // Exactly one Runnable thread, so every policy returns it and no policy state may
            // change. Note this path does not consult `candidates`, and does not need to: a
            // debt is only ever charged naming threads *other* than its holder, so `only`'s
            // debt cannot name `only`, and no other thread is Runnable for it to name — hence
            // `debtDischarged` holds vacuously and `candidates` here is exactly `[only]`.
            //
            // Nor does it consult `peekNextOp`. A forced decision has no use for the imminent
            // op: the weight exists solely to scale a demotion probability, and there is
            // nothing to demote towards.
            state, Some only
        | Contention.Contended _ as contention ->

        match state.Scheduling with
        | SchedulerState.RoundRobin ->
            let runnable = candidates state

            let chosen =
                match runnable with
                | [] ->
                    failwith
                        "Scheduler.chooseNext: `candidates` was empty on a contended decision, which the debt invariant makes impossible — see `candidates`."
                | _ ->
                    let (ThreadId lastRanId) = lastRan

                    runnable
                    |> List.tryFind (fun (ThreadId i) -> i > lastRanId)
                    |> Option.orElse (List.tryHead runnable)

            state, chosen
        | SchedulerState.Pct pct ->
            // Priorities are sampled over the *unfiltered* Runnable set, but the argmax runs
            // over the candidates. Keeping the sampling domain unfiltered means the sampling
            // sequence is a function of the seed plus the set of threads ever seen Runnable at
            // a contended decision; if the filter drove it, the RNG stream would additionally
            // depend on yield timing.
            let runnable = contention.Runnable
            let eligible = candidates state

            // Lazy first-observation insert: any Runnable thread without a
            // priority gets one sampled in ascending-ThreadId order, so the
            // sampling sequence is determined by the seed plus the set of
            // threads that have ever been seen Runnable at a contended
            // decision, not by the order in which they were created.
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

            // Single weighted-Bernoulli draw against `weight * P_BASE`, burned
            // regardless of weight so that `weight = 0.0` is correctly a no-op
            // without a branch that skips the draw. It is *not* burned when the
            // decision is uncontended — see the `Forced` arm above and the module
            // header: a draw that cannot change a decision is what would make the
            // policy state depend on a run's forced prefix, and hence unshareable.
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

    /// Transition `blocked` from Runnable to `BlockedOnJoin (target, deadlineTicks)`.
    /// Called from the `Thread.Join` intrinsic; exposed here so the set of places
    /// that mutate `ThreadStatus` stays small and auditable.
    ///
    /// `deadlineTicks = None` is an infinite wait (`Thread.Join()` /
    /// `Thread.Join(-1)`); `Some ms` is a finite timeout, expressed as the
    /// absolute virtual-clock tick at which the wait expires. The
    /// deadline-firing path in `Program.fireExpiredDeadlines` routes such
    /// threads through `fireJoinTimeout` below.
    let blockOnJoin
        (blocked : ThreadId)
        (target : ThreadId)
        (deadlineTicks : int64 option)
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
                            Status = ThreadStatus.BlockedOnJoin (target, deadlineTicks)
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
    /// when the virtual clock crosses the deadline. `deadlineTicks = None` is
    /// an infinite sleep (`Thread.Sleep(-1)` / `Timeout.Infinite`); `Some _`
    /// is a finite timeout. No optimistic eval-stack push is performed
    /// because `Thread.Sleep` returns `void`.
    ///
    /// Caller is responsible for advancing the program counter past the
    /// `Sleep` call site before parking (so the wake resumes after the
    /// call), matching the contract used by every other QCall handler that
    /// blocks.
    let blockOnSleep (blocked : ThreadId) (deadlineTicks : int64 option) (state : IlMachineState) : IlMachineState =
        { state with
            ThreadState =
                state.ThreadState
                |> Map.change
                    blocked
                    (Option.map (fun s ->
                        { s with
                            Status = ThreadStatus.BlockedOnSleep deadlineTicks
                        }
                    ))
        }

    /// Park `blocked` in `BlockedInSyscall`, transitioning out of `Runnable`.
    /// Called from the handler of whichever syscall is blocking — today
    /// `SystemNative_WaitForSocketEvents` and `SystemNative_FLock`.
    ///
    /// **The caller must have written the task's `ParkedSyscall` record first.**
    /// This function takes no payload and the status carries none: what the
    /// thread waits for is that record, which the emulated kernel needs anyway so
    /// that `close` can refuse to strand a waiter. The sweeps read it to decide
    /// whether to wake, and the re-entered handler finishes the call from it, so
    /// a park written here without a record is a thread nothing can ever wake —
    /// which is what `EmulatedKernel.checkTaskInvariants` refuses and what each
    /// sweep fails loudly on.
    ///
    /// Carries no deadline, unlike `blockOnJoin` and `blockOnSleep`: neither
    /// wait can time out, so `Program.fireExpiredDeadlines` will never route a
    /// thread out of this status. A parking syscall that *does* take a timeout
    /// puts its deadline in its record.
    ///
    /// Unlike every other blocking helper here, the caller must *not* advance the
    /// program counter past the call site before parking: the wake has to
    /// re-enter the handler so that it can write the call's results through the
    /// caller's own pointer arguments, which is only possible from the caller's
    /// frame. `NativeHandlerResult.BlockedRetainingFrame` is what keeps that
    /// frame in place.
    let parkInSyscall (blocked : ThreadId) (state : IlMachineState) : IlMachineState =
        { state with
            ThreadState =
                state.ThreadState
                |> Map.change
                    blocked
                    (Option.map (fun s ->
                        { s with
                            Status = ThreadStatus.BlockedInSyscall
                        }
                    ))
        }


    /// Wake a thread parked in a syscall: the sweep has observed that what it was
    /// waiting for has happened.
    ///
    /// Every park is re-entrant — it kept the native frame and the caller's
    /// program counter — so flipping the status is the whole wake, and the
    /// re-entered handler finishes the call itself, from the caller's own frame.
    /// One wake for all of them, because none of that varies by syscall.
    ///
    /// A wake is not a promise. Two threads can be woken for one lock and only
    /// one of them get it; the loser re-enters, finds it taken, and parks again
    /// on the record it still holds, which this leaves untouched.
    ///
    /// The status check is not ceremony: the only caller sweeps threads it
    /// observed parked, so a thread that is not parked by the time it is woken
    /// means the sweep raced its own observation.
    let wakeFromSyscall (thread : ThreadId) (state : IlMachineState) : IlMachineState =
        match state.ThreadState |> Map.tryFind thread with
        | None -> failwith $"Scheduler.wakeFromSyscall: thread %O{thread} has no ThreadState."
        | Some ts ->

        match ts.Status with
        | ThreadStatus.BlockedInSyscall -> ()
        | other ->
            failwith
                $"Scheduler.wakeFromSyscall: thread %O{thread} is not parked in a syscall (status: %O{other}); the sweep observed a satisfied wake condition against a thread that is not waiting on one."

        setThreadStatus thread ThreadStatus.Runnable state

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

    /// Record that the entry thread has returned from `Main`.
    ///
    /// The thread never runs again but is not dead: it goes to `WaitingForForegroundThreads`,
    /// keeping its final frame — `Main`'s return value on its eval stack is the process exit
    /// code — and any thread joined on it stays blocked. That is what CoreCLR does: after
    /// `Main`, `RunMainPost` blocks the entry thread in `WaitForOtherThreads` until every other
    /// foreground thread has finished, and a `Join` on it waits for a death that never comes.
    /// So none of the checks `onThreadTerminated` makes apply — a monitor the entry thread still
    /// holds is held by a live thread, and a waiter on it is stuck exactly as it would be on
    /// real .NET.
    let onMainReturned (entry : ThreadId) (state : IlMachineState) : IlMachineState =
        match state.ThreadState |> Map.tryFind entry with
        | None -> failwith $"Scheduler.onMainReturned: entry thread %O{entry} has no ThreadState."
        | Some ts ->
            match ts.Status with
            | ThreadStatus.Runnable -> ()
            | other ->
                // It has just retired `Main`'s `ret`, which only a Runnable thread can do.
                failwith
                    $"logic error: entry thread %O{entry} returned from Main while in status %O{other}; only a Runnable thread can retire an instruction."

        setThreadStatus entry ThreadStatus.WaitingForForegroundThreads state

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
        let orphanedSyncBlocks = ManagedHeap.syncBlocksHeldBy terminated state.ManagedHeap

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

        // No yield-debt pruning here: the driver has already discharged `terminated`,
        // because a thread's final `Ret` is a retired step like any other. See
        // `dischargeYieldDebts`.
        { state with
            ThreadState = threadState
            Scheduling = scheduling
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
        | WhatWeDid.Aborted fatal ->
            // A terminating outcome is the caller's to surface: this function only decides what
            // status the new worker should hold, and a process that has aborted has no use for one.
            let message = fatal.Message |> Option.defaultValue "<no message>"

            failwith
                $"logic error: spawning worker %O{worker} produced an abort (%O{fatal.Code}: %s{message}); the caller must surface a terminating outcome rather than applying it to the worker's status"
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
            // `ensureTypeInitialised` (which is what feeds this entry point) today.
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
    /// Under `Pct` the coin is tossed iff `others` is non-empty — i.e. iff the yield happens at
    /// a contended moment, `ran` itself being Runnable. When `others` is empty the result is
    /// forced to `false` by the guard below whichever way the coin lands, so tossing it would be
    /// a draw that cannot change anything, and the module header explains why the policy must
    /// not take those: they would make `PctState` depend on a run's forced prefix, which is
    /// exactly the prefix a schedule-sweeping harness wants to compute once and share.
    ///
    /// Careful: this contention test is *not* the same evaluation as `chooseNext`'s, because
    /// `onStepOutcome` wakes class-init waiters before calling here, so `others` is read against
    /// a possibly-larger Runnable set than the one the step was scheduled from. A tick can
    /// therefore be forced at choice time and contended here. That is harmless for the policy —
    /// the draw is still gated on a genuine choice existing — but a harness that snapshots
    /// "before the first contended decision" must treat it as a fork point too; see
    /// `tryContenders`.
    let private chargeYieldDebt (ran : ThreadId) (state : IlMachineState) : IlMachineState * bool =
        let others = runnableThreads state |> List.filter (fun tid -> tid <> ran)

        let state, honour =
            match state.Scheduling, others with
            | SchedulerState.RoundRobin, _ -> state, true
            | SchedulerState.Pct _, [] ->
                // Uncontended: `ran` is the only Runnable thread, so no switch can be
                // guaranteed and the guard below returns `false` regardless. No draw.
                state, false
            | SchedulerState.Pct pct, _ :: _ ->
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

    /// Apply the consequences that depend on *which* outcome the step produced. The
    /// consequences that apply to every retired step regardless — currently just discharging
    /// yield debts — deliberately do not live here, because this function only sees the
    /// `Stepped` family of outcomes; they live in the driver. See `dischargeYieldDebts`.
    ///
    /// On `Executed`, every thread BlockedOnClassInit on `ran` is speculatively woken.
    /// They re-check their blocker on their next turn and re-block if the cctor hasn't
    /// completed. This is correct but wasteful.
    let onStepOutcome (ran : ThreadId) (outcome : WhatWeDid) (state : IlMachineState) : IlMachineState =
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
        | WhatWeDid.Aborted fatal ->
            // `AbstractMachine` converts an aborting step into `ExecutionResult.Aborted` at the
            // point where an op's `WhatWeDid` becomes an `ExecutionResult`, which is upstream of
            // every call to this function -- so the scheduler never sees one. That is deliberate:
            // a step that tore the process down did not retire, so none of the bookkeeping here
            // (waking class-init waiters, charging yield debt) has a meaningful answer for it.
            let message = fatal.Message |> Option.defaultValue "<no message>"

            failwith
                $"logic error: thread %O{ran} reported an abort (%O{fatal.Code}: %s{message}) to the scheduler; an aborting step should have become ExecutionResult.Aborted before reaching here"
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

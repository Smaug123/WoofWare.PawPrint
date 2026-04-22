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
/// The long-term goal (see MEMORY: Antithesis-style pruning over thread interleaving)
/// is for the scheduling policy to become pluggable — a harness will want to drive
/// `chooseNext` from outside. Keep this module free of logging and of anything that
/// isn't a pure state transformation so that swap is cheap.
[<RequireQualifiedAccess>]
module Scheduler =

    /// Pick the next thread to run using a deterministic round-robin policy: among
    /// the Runnable threads, prefer the lowest id strictly greater than `lastRan`;
    /// if there isn't one, wrap to the lowest id overall. The policy is intentionally
    /// *not* sticky — staying on the most-recently-run thread minimises interleaving,
    /// which is the opposite of what a pruning harness wants. Returns `None` iff no
    /// thread is Runnable, which the driver treats as deadlock.
    let chooseNext (lastRan : ThreadId) (state : IlMachineState) : ThreadId option =
        let runnable =
            state.ThreadState
            |> Map.toSeq
            |> Seq.choose (fun (tid, ts) ->
                match ts.Status with
                | ThreadStatus.Runnable -> Some tid
                | _ -> None
            )
            |> Seq.sortBy (fun (ThreadId i) -> i)
            |> Seq.toList

        match runnable with
        | [] -> None
        | _ ->
            let (ThreadId lastRanId) = lastRan

            runnable
            |> List.tryFind (fun (ThreadId i) -> i > lastRanId)
            |> Option.orElse (List.tryHead runnable)

    /// Transition `blocked` from Runnable to BlockedOnJoin target. Called from the
    /// Thread.Join intrinsic in AbstractMachine; exposed here so the set of places
    /// that mutate ThreadStatus stays small and auditable.
    let blockOnJoin (blocked : ThreadId) (target : ThreadId) (state : IlMachineState) : IlMachineState =
        { state with
            ThreadState =
                state.ThreadState
                |> Map.change
                    blocked
                    (Option.map (fun s ->
                        { s with
                            Status = ThreadStatus.BlockedOnJoin target
                        }
                    ))
        }

    /// Record that `terminated` has finished executing its final `ret`.
    /// - Flips its own status to Terminated.
    /// - Wakes every thread that was BlockedOnJoin on it; they proceed past Join.
    /// - Fails loudly if `terminated` was still the `InProgress` initializer of any
    ///   type, because every thread waiting on that init would be stuck on a dead
    ///   blocker — a silent liveness bug. The real CLR wraps the dying cctor in a
    ///   TypeInitializationException; we don't synthesise one yet, so crash clearly.
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
                | ThreadStatus.BlockedOnJoin target when target = terminated ->
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
        | WhatWeDid.SuspendedForClassInit
        | WhatWeDid.ThrowingTypeInitializationException ->
            // The worker is free to run: either the type was already initialised
            // (Executed), a cctor frame was pushed on top of the target frame
            // (SuspendedForClassInit — the worker will run the cctor first, then
            // fall into the target method), or the cached TypeInitializationException
            // was dispatched onto the worker's frames (ThrowingTypeInit — the worker
            // will run the exception handler / terminate on its next step). In all
            // three cases the worker stays Runnable.
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
    /// cctor hasn't completed. This is correct but wasteful; see the MEMORY item on
    /// per-instruction interleaving — it's cheap to fix once the scheduler owns the
    /// policy, which is only true after this refactor.
    let onStepOutcome (ran : ThreadId) (outcome : WhatWeDid) (state : IlMachineState) : IlMachineState =
        match outcome with
        | WhatWeDid.Executed ->
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
        | WhatWeDid.SuspendedForClassInit -> state
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

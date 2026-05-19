namespace WoofWare.PawPrint

/// Deterministic state-machine for the seven `SystemNative_LowLevelMonitor_*`
/// P/Invokes that back `System.Threading.LowLevelMonitor`. The CoreCLR shape
/// is a `pthread_mutex_t` + `pthread_cond_t` pair; we reproduce the observable
/// semantics through `LowLevelMonitorState` (registry value) and three
/// `ThreadStatus` cases (`Runnable`, `BlockedOnMonitorAcquire`,
/// `BlockedOnMonitorWait`).
///
/// The module is pure: every transition is `IlMachineState -> IlMachineState`
/// and never reads from a real clock, the host's mutex implementation, or any
/// nondeterministic source. All ordering decisions are FIFO over the
/// `AcquireQueue` / `WaitQueue`, which is load-bearing for `LowLevelLock`
/// fairness: deviating from FIFO changes the observable interleaving and is
/// not a refactor.
///
/// Ownership transfer model: when `release`, `wait`, or `signalRelease` give
/// up the monitor and the AcquireQueue is non-empty, ownership is handed
/// directly from the releaser to the FIFO head — the head's status flips to
/// `Runnable` already holding the monitor. This mirrors pthread
/// `cond_wait`'s contract (the call returns owning the mutex) and matches
/// the native handler's posture: the IL `Acquire`/`Wait` call site advances
/// to its successor regardless of whether the thread blocked, so when the
/// scheduler later picks the woken thread, it resumes past the call site
/// and must already own the monitor for subsequent guest code to be sound.
///
/// Invariant: `Owner = None` implies `AcquireQueue = []` (equivalently,
/// `AcquireQueue ≠ []` implies `Owner = Some _`). A non-empty queue with no
/// owner would mean the head is "next in line" but holds nothing, leaving
/// `Release` to fire as unowned when the resumed thread eventually runs.
/// Uncontended ownership (`Owner = Some _`, `AcquireQueue = []`) is normal
/// and not forbidden by this invariant. Every transition below preserves
/// the implication; `destroy` asserts it.
///
/// Reentrancy: a monitor is non-reentrant, matching CoreCLR. A thread that
/// already owns the monitor and calls `acquire` again is deadlocking guest
/// code — we fail loudly rather than spin, so the bug surfaces here instead
/// of as a quiet round-robin starvation later. Reentrant locking is the
/// responsibility of `LowLevelLock`, layered on top of this primitive by the
/// BCL.
///
/// Timeouts: `wait` accepts an optional absolute virtual-clock
/// `deadlineMs`. `None` is the infinite-wait shape used by the void
/// `SystemNative_LowLevelMonitor_Wait` entry point; `Some ms` is the
/// finite-deadline shape used by `TimedWait`. The deadline is recorded
/// on the waiter's `BlockedOnMonitorWait` status, where the driver
/// loop's deadline-firing pass (`Program.fireExpiredDeadlines`) picks
/// it up: when the clock reaches the deadline, `fireTimeout` (below)
/// pulls the thread out of `WaitQueue` and routes it through the same
/// reacquire path `signalRelease` uses, additionally rewriting the
/// caller's `TimedWait` return slot from `Int32 1` (signalled) to
/// `Int32 0` (timed out). Pure `wait` callers, which return `void` from
/// `SystemNative_LowLevelMonitor_Wait`, do not push a return slot —
/// hence `fireTimeout` is only safe against threads that were parked
/// via the TimedWait path.
///
/// Spurious wakeups: injected from outside this module under control of
/// `EmulatedKernel.SpuriousWakeup`. The transition is `spuriousWake`
/// below, which routes a waiter through the same reacquire path as
/// `signalRelease` (take ownership if the monitor is free; otherwise park
/// at the tail of the acquire queue). The driver applies
/// `applySpuriousWakeups` once per scheduler tick. Guest code that
/// depends on the absence of spurious wakeups is incorrect against real
/// CoreCLR; switching the strategy to `AlwaysAll` is the deterministic
/// way to expose those bugs.
[<RequireQualifiedAccess>]
module LowLevelMonitor =

    /// Look up the monitor for `id`, or fail loud. A retained handle that
    /// outlives `destroy` lands here so use-after-free shows up at the use
    /// site rather than as a confusing null-handle bug elsewhere.
    let private lookup (id : LowLevelMonitorId) (state : IlMachineState) : LowLevelMonitorState =
        match Map.tryFind id state.Kernel.LowLevelMonitors with
        | Some monitor -> monitor
        | None ->
            failwith
                $"LowLevelMonitor %O{id} is not registered; either it was never created or it has already been destroyed (use-after-free on a stale handle)."

    let private writeMonitor
        (id : LowLevelMonitorId)
        (monitor : LowLevelMonitorState)
        (state : IlMachineState)
        : IlMachineState
        =
        state.MapKernel (fun kernel ->
            { kernel with
                LowLevelMonitors = kernel.LowLevelMonitors |> Map.add id monitor
            }
        )

    /// Allocate a fresh monitor. Returns the new handle alongside the
    /// updated state. The handle is non-zero (counters start at 1) so the
    /// guest's "create failed → throw OOM" check does not fire.
    let create (state : IlMachineState) : LowLevelMonitorId * IlMachineState =
        let id = LowLevelMonitorId state.Kernel.NextLowLevelMonitorId

        let state =
            state.MapKernel (fun kernel ->
                { kernel with
                    LowLevelMonitors = kernel.LowLevelMonitors |> Map.add id LowLevelMonitorState.empty
                    NextLowLevelMonitorId = kernel.NextLowLevelMonitorId + 1
                }
            )

        id, state

    /// Tear down a monitor. The CoreCLR contract is that `Destroy` runs after
    /// every `Acquire` has been paired with a `Release` and no thread is
    /// waiting — i.e. the monitor is fully quiescent. We enforce that contract
    /// loudly: destroying a monitor with an owner or a non-empty queue is a
    /// guest bug that would otherwise present as a use-after-free later.
    ///
    /// The AcquireQueue check is also a defensive assertion of the
    /// owner/queue invariant (`Owner = None` implies `AcquireQueue = []`):
    /// a non-empty queue with no owner means a transition somewhere
    /// violated the invariant, and surfacing that here is cheaper than
    /// chasing it from a downstream symptom.
    let destroy (id : LowLevelMonitorId) (state : IlMachineState) : IlMachineState =
        let monitor = lookup id state

        match monitor.Owner with
        | Some owner -> failwith $"LowLevelMonitor %O{id}: refusing to Destroy a monitor still held by thread %O{owner}"
        | None -> ()

        match monitor.AcquireQueue with
        | [] -> ()
        | waiters ->
            failwith
                $"LowLevelMonitor %O{id}: refusing to Destroy a monitor with %d{List.length waiters} thread(s) parked in BlockedOnMonitorAcquire (%A{waiters}); this also indicates a broken Owner/AcquireQueue invariant."

        match monitor.WaitQueue with
        | [] -> ()
        | waiters ->
            failwith
                $"LowLevelMonitor %O{id}: refusing to Destroy a monitor with %d{List.length waiters} thread(s) parked in BlockedOnMonitorWait (%A{waiters})"

        state.MapKernel (fun kernel ->
            { kernel with
                LowLevelMonitors = kernel.LowLevelMonitors |> Map.remove id
            }
        )

    /// Try to acquire the monitor on behalf of `thread`.
    ///
    /// Returns `Acquired` if the call returns to the guest with the monitor
    /// held: the IL `Acquire` site advances and the thread stays Runnable.
    ///
    /// Returns `Blocked` if the thread was parked. The IL site still
    /// advances (the native handler returns `WhatWeDid.Executed` in both
    /// cases) — when the thread is later woken via `release`/`wait`'s
    /// release path/`signalRelease`, ownership has already been transferred
    /// to it, so resuming past the `Acquire` call is correct without
    /// re-executing the acquire transition.
    [<RequireQualifiedAccess>]
    type AcquireOutcome =
        | Acquired of IlMachineState
        | Blocked of IlMachineState

    let acquire (thread : ThreadId) (id : LowLevelMonitorId) (state : IlMachineState) : AcquireOutcome =
        let monitor = lookup id state

        match monitor.Owner with
        | None ->
            // Uncontended fast path. By the Owner/AcquireQueue invariant,
            // AcquireQueue is empty here; we just take ownership.
            let monitor =
                { monitor with
                    Owner = Some thread
                }

            state |> writeMonitor id monitor |> AcquireOutcome.Acquired

        | Some owner when owner = thread ->
            // CoreCLR's LowLevelMonitor is non-reentrant; recursive acquire
            // would deadlock against itself on the pthread mutex. Failing
            // loud surfaces the bug here instead of as a silent deadlock at
            // the scheduler level.
            failwith
                $"LowLevelMonitor %O{id}: thread %O{thread} attempted recursive Acquire (monitor is non-reentrant; use LowLevelLock for reentrancy)."

        | Some _ ->
            // Contended path: park the thread at the tail of the queue.
            // Ownership will be handed to us atomically when our
            // predecessor releases (or signal-releases) the monitor.
            let monitor =
                { monitor with
                    AcquireQueue = monitor.AcquireQueue @ [ thread ]
                }

            state
            |> writeMonitor id monitor
            |> Scheduler.setThreadStatus thread (ThreadStatus.BlockedOnMonitorAcquire id)
            |> AcquireOutcome.Blocked

    /// Release the monitor held by `thread`. If the AcquireQueue is
    /// non-empty, ownership is transferred to the FIFO head — the head's
    /// status flips to `Runnable` and the head is popped from the queue.
    /// The released thread does NOT briefly observe an unowned monitor
    /// with a non-empty queue: that intermediate state would violate the
    /// Owner/AcquireQueue invariant, and (more importantly) would mean the
    /// woken thread resumes past its `Acquire` site without owning the
    /// monitor, so its subsequent `Release` would fail as unowned.
    let release (thread : ThreadId) (id : LowLevelMonitorId) (state : IlMachineState) : IlMachineState =
        let monitor = lookup id state

        match monitor.Owner with
        | Some owner when owner = thread -> ()
        | Some owner ->
            failwith $"LowLevelMonitor %O{id}: thread %O{thread} called Release but the monitor is owned by %O{owner}"
        | None -> failwith $"LowLevelMonitor %O{id}: thread %O{thread} called Release but the monitor is unowned"

        match monitor.AcquireQueue with
        | [] ->
            // No one waiting: just clear ownership.
            let monitor =
                { monitor with
                    Owner = None
                }

            state |> writeMonitor id monitor

        | head :: rest ->
            // Hand ownership directly to the FIFO head and wake them. The
            // woken thread will resume past its `Acquire` call site
            // already holding the monitor.
            let monitor =
                { monitor with
                    Owner = Some head
                    AcquireQueue = rest
                }

            state
            |> writeMonitor id monitor
            |> Scheduler.setThreadStatus head ThreadStatus.Runnable

    /// `Wait` is a single atomic operation in the CoreCLR API: the caller
    /// must hold the monitor, the call releases it and parks the thread on
    /// the condition variable, and on wakeup the monitor is reacquired
    /// before the call returns. We split that into the two transitions the
    /// scheduler can observe:
    ///
    ///   1. `wait` here: the caller (must be the current `Owner`) is moved
    ///      to `WaitQueue`, the monitor is released (transferring
    ///      ownership to the AcquireQueue head if any, otherwise clearing
    ///      Owner), and the caller's status becomes
    ///      `BlockedOnMonitorWait`.
    ///
    ///   2. The wake-up step is driven by `signalRelease` (below): it
    ///      moves the head of the wait queue onto the AcquireQueue (FIFO
    ///      tail) and then runs the same release path, so the new
    ///      AcquireQueue head ends up owning the monitor. When that
    ///      head's turn comes up, it resumes past its original `Wait`
    ///      call site already holding the monitor.
    ///
    /// This preserves the atomicity contract from the guest's perspective:
    /// the guest cannot observe a state in which it has released the
    /// monitor but has not yet been parked, because both happen in a single
    /// `wait` call.
    ///
    /// `deadlineMs = None` mirrors the infinite-timeout
    /// `SystemNative_LowLevelMonitor_Wait` entry point: the thread will
    /// only wake from a `Signal_Release` or a spurious wake. `Some ms`
    /// is the finite-deadline shape used by `TimedWait`: the driver
    /// loop will additionally fire `fireTimeout` if the clock reaches
    /// `ms` before a signal arrives.
    let wait
        (thread : ThreadId)
        (id : LowLevelMonitorId)
        (deadlineMs : int64 option)
        (state : IlMachineState)
        : IlMachineState
        =
        let monitor = lookup id state

        match monitor.Owner with
        | Some owner when owner = thread -> ()
        | Some owner ->
            failwith $"LowLevelMonitor %O{id}: thread %O{thread} called Wait but the monitor is owned by %O{owner}"
        | None -> failwith $"LowLevelMonitor %O{id}: thread %O{thread} called Wait but the monitor is unowned"

        // Atomically: park the caller in the wait queue and release the
        // monitor. The caller must NOT join the AcquireQueue — Wait does
        // not contend for the monitor until Signal_Release rouses it.
        match monitor.AcquireQueue with
        | [] ->
            let monitor =
                { monitor with
                    Owner = None
                    WaitQueue = monitor.WaitQueue @ [ thread ]
                }

            state
            |> writeMonitor id monitor
            |> Scheduler.setThreadStatus thread (ThreadStatus.BlockedOnMonitorWait (id, deadlineMs))

        | head :: rest ->
            // Transfer ownership to the AcquireQueue head and park the
            // caller on the WaitQueue.
            let monitor =
                { monitor with
                    Owner = Some head
                    AcquireQueue = rest
                    WaitQueue = monitor.WaitQueue @ [ thread ]
                }

            state
            |> writeMonitor id monitor
            |> Scheduler.setThreadStatus head ThreadStatus.Runnable
            |> Scheduler.setThreadStatus thread (ThreadStatus.BlockedOnMonitorWait (id, deadlineMs))

    /// `Signal_Release` is the wakeup half of the condvar protocol. The
    /// caller must hold the monitor; the call wakes at most one thread
    /// from the wait queue (FIFO) and releases the monitor. The woken
    /// thread is moved from the wait queue to the tail of the acquire
    /// queue and its status flips from `BlockedOnMonitorWait` to
    /// `BlockedOnMonitorAcquire` — it must then be reached as a normal
    /// AcquireQueue entry. The subsequent release picks the new head and
    /// transfers ownership to them.
    ///
    /// If the wait queue is empty, `Signal_Release` degenerates into a
    /// plain `release`: the guest is allowed to call it speculatively
    /// (LowLevelLifoSemaphore does this), and treating empty as a no-op on
    /// the wakeup side is the documented behaviour.
    let signalRelease (thread : ThreadId) (id : LowLevelMonitorId) (state : IlMachineState) : IlMachineState =
        let monitor = lookup id state

        match monitor.Owner with
        | Some owner when owner = thread -> ()
        | Some owner ->
            failwith
                $"LowLevelMonitor %O{id}: thread %O{thread} called Signal_Release but the monitor is owned by %O{owner}"
        | None -> failwith $"LowLevelMonitor %O{id}: thread %O{thread} called Signal_Release but the monitor is unowned"

        match monitor.WaitQueue with
        | [] ->
            // No waiter to signal: equivalent to a plain Release.
            release thread id state
        | waiter :: restWait ->
            // Move the woken waiter to the acquire queue (FIFO tail) and
            // delegate to release. The release path transfers ownership
            // to whichever thread now sits at the AcquireQueue head — if
            // the queue was empty before the move, that's the waiter
            // itself; otherwise it's an earlier acquirer and the waiter
            // stays parked behind them.
            let monitor =
                { monitor with
                    WaitQueue = restWait
                    AcquireQueue = monitor.AcquireQueue @ [ waiter ]
                }

            let state =
                state
                |> writeMonitor id monitor
                |> Scheduler.setThreadStatus waiter (ThreadStatus.BlockedOnMonitorAcquire id)

            release thread id state

    /// Pull `thread` out of `id`'s `WaitQueue` and route it through the
    /// same reacquire path that `signalRelease` would take. By the
    /// Owner/AcquireQueue invariant, an unowned monitor has an empty
    /// acquire queue, so a spurious wake of a free monitor grants
    /// ownership directly to the woken thread (status flips to
    /// `Runnable`); a held monitor parks the woken thread at the
    /// AcquireQueue tail (status flips to `BlockedOnMonitorAcquire`).
    /// In both cases the waiter eventually resumes past its `Wait` call
    /// site already owning the monitor — the same shape `signalRelease`
    /// produces — so the native handler does not need to distinguish
    /// signalled from spurious wakeups.
    ///
    /// Fails loudly if `thread` is not in `id`'s `WaitQueue`. The only
    /// caller is `applySpuriousWakeups`, which enumerates the waiters
    /// itself; a miss indicates either a script that named a stale
    /// thread (silent skip would let the script drift unnoticed) or a
    /// bug in the strategy interpreter.
    let spuriousWake (id : LowLevelMonitorId) (thread : ThreadId) (state : IlMachineState) : IlMachineState =
        let monitor = lookup id state

        if not (List.contains thread monitor.WaitQueue) then
            failwith
                $"LowLevelMonitor %O{id}: cannot spuriously wake thread %O{thread} because it is not in WaitQueue (queue: %A{monitor.WaitQueue})."

        let newWaitQueue = monitor.WaitQueue |> List.filter (fun t -> t <> thread)

        match monitor.Owner with
        | None ->
            // Uncontended: the invariant guarantees AcquireQueue is empty,
            // so we take ownership directly and become Runnable.
            let monitor =
                { monitor with
                    Owner = Some thread
                    WaitQueue = newWaitQueue
                }

            state
            |> writeMonitor id monitor
            |> Scheduler.setThreadStatus thread ThreadStatus.Runnable

        | Some _ ->
            // Contended: park at the AcquireQueue tail. Ownership will be
            // handed to us atomically when our predecessor releases.
            let monitor =
                { monitor with
                    AcquireQueue = monitor.AcquireQueue @ [ thread ]
                    WaitQueue = newWaitQueue
                }

            state
            |> writeMonitor id monitor
            |> Scheduler.setThreadStatus thread (ThreadStatus.BlockedOnMonitorAcquire id)

    /// Fire the finite-timeout wake for `thread` parked on monitor `id`'s
    /// `WaitQueue`. Only safe against TimedWait waiters — the
    /// `BlockedOnMonitorWait` deadline is the discriminator, and only
    /// TimedWait pushes the optimistic `Int32 1` slot that this function
    /// rewrites to `Int32 0`. The wake routes the thread through the same
    /// reacquire path as `signalRelease`/`spuriousWake` (take ownership
    /// directly if the monitor is free, otherwise park at the AcquireQueue
    /// tail), so on resumption the thread observes `0` from
    /// `Interop.Sys.LowLevelMonitor_TimedWait` already re-owning the
    /// monitor — mirroring `pthread_cond_timedwait`'s contract.
    ///
    /// Fails loud if `thread` is not in `id`'s `WaitQueue`. The only
    /// caller is `Program.fireExpiredDeadlines`, which enumerates
    /// `BlockedOnMonitorWait` statuses itself; a miss would mean a
    /// signal-wake raced our enumeration without flipping the status, or
    /// the deadline-firing path was reached for an untimed waiter — both
    /// indicate a structural bug worth surfacing here rather than letting
    /// the eval-stack pop misbehave silently.
    let fireTimeout (thread : ThreadId) (id : LowLevelMonitorId) (state : IlMachineState) : IlMachineState =
        let monitor = lookup id state

        if not (List.contains thread monitor.WaitQueue) then
            failwith
                $"LowLevelMonitor %O{id}: cannot fire timeout for thread %O{thread} because it is not in WaitQueue (queue: %A{monitor.WaitQueue})."

        let newWaitQueue = monitor.WaitQueue |> List.filter (fun t -> t <> thread)

        let state =
            match monitor.Owner with
            | None ->
                // Uncontended: the invariant guarantees AcquireQueue is empty,
                // so we take ownership directly and become Runnable.
                let monitor =
                    { monitor with
                        Owner = Some thread
                        WaitQueue = newWaitQueue
                    }

                state
                |> writeMonitor id monitor
                |> Scheduler.setThreadStatus thread ThreadStatus.Runnable

            | Some _ ->
                // Contended: park at the AcquireQueue tail. Ownership will be
                // handed to us atomically when our predecessor releases.
                // Status flips from BlockedOnMonitorWait to
                // BlockedOnMonitorAcquire — the deadline is implicitly
                // forgotten because the new variant carries no deadline
                // field. By the time we resume past TimedWait's call site we
                // already own the monitor, matching pthread_cond_timedwait.
                let monitor =
                    { monitor with
                        AcquireQueue = monitor.AcquireQueue @ [ thread ]
                        WaitQueue = newWaitQueue
                    }

                state
                |> writeMonitor id monitor
                |> Scheduler.setThreadStatus thread (ThreadStatus.BlockedOnMonitorAcquire id)

        // Rewrite the park-time `Int32 1` (signalled) slot pushed by the
        // TimedWait handler to `Int32 0` (timed out). Pop-then-push keeps
        // the stack depth invariant across the wake.
        let _, state = IlMachineState.popEvalStack thread state
        IlMachineState.pushToEvalStack' (EvalStackValue.Int32 0) thread state

    /// SplitMix64-style hash, used to derive a deterministic per-waiter
    /// coin flip in `[0.0, 1.0)` from `(seed, tick, monitorId, threadId)`.
    /// Replayability comes from the function being a pure hash with no
    /// mutable PRNG state — distinct ticks/waiters never share entropy.
    let private coinFlip (seed : uint64) (tick : int64) (LowLevelMonitorId mid) (ThreadId tid) : float =
        let mix (h : uint64) (x : uint64) : uint64 =
            let h = h ^^^ x
            h * 0x100000001B3UL

        let finalise (h : uint64) : uint64 =
            let h = h ^^^ (h >>> 33)
            let h = h * 0xff51afd7ed558ccdUL
            let h = h ^^^ (h >>> 33)
            let h = h * 0xc4ceb9fe1a85ec53UL
            h ^^^ (h >>> 33)

        let h = seed
        let h = mix h (uint64 tick)
        let h = mix h (uint64 mid)
        let h = mix h (uint64 tid)
        let h = finalise h
        // Top 53 bits as a float in [0, 1). Matches the common
        // "uniform double from uint64" recipe; precision loss in the low
        // bits is irrelevant for a fuzz-probability threshold.
        float (h >>> 11) / float (1UL <<< 53)

    /// Enumerate the spurious wakeups requested by `strategy` for `tick`
    /// against the current `WaitQueue` membership, then apply each
    /// `spuriousWake` in deterministic order (ascending monitor id, then
    /// FIFO position within `WaitQueue`). Snapshotting the (monitor,
    /// thread) list first means strategy decisions are computed against
    /// the state at entry — applying an earlier wake never affects which
    /// later wakes the strategy would have chosen on this tick.
    ///
    /// `Disabled` is the identity. `Random.probability` outside
    /// `[0.0, 1.0]` is a programmer error and fails loud. `Scripted`
    /// triples that name a thread not in the named monitor's WaitQueue
    /// at the named tick fail loud — silent skip would let scripts drift
    /// when the underlying interleaving changes.
    let applySpuriousWakeups
        (strategy : SpuriousWakeupStrategy)
        (tick : int64)
        (state : IlMachineState)
        : IlMachineState
        =
        match strategy with
        | SpuriousWakeupStrategy.Disabled -> state

        | SpuriousWakeupStrategy.AlwaysAll ->
            state.Kernel.LowLevelMonitors
            |> Map.toSeq
            |> Seq.sortBy (fun (LowLevelMonitorId i, _) -> i)
            |> Seq.collect (fun (mid, monitor) -> monitor.WaitQueue |> List.map (fun tid -> mid, tid))
            |> Seq.toList
            |> List.fold (fun acc (mid, tid) -> spuriousWake mid tid acc) state

        | SpuriousWakeupStrategy.Random (seed, probability) ->
            if probability < 0.0 || probability > 1.0 || System.Double.IsNaN probability then
                failwith
                    $"SpuriousWakeupStrategy.Random: probability %f{probability} is outside [0.0, 1.0] (NaN or out of range)."

            state.Kernel.LowLevelMonitors
            |> Map.toSeq
            |> Seq.sortBy (fun (LowLevelMonitorId i, _) -> i)
            |> Seq.collect (fun (mid, monitor) -> monitor.WaitQueue |> List.map (fun tid -> mid, tid))
            |> Seq.filter (fun (mid, tid) -> coinFlip seed tick mid tid < probability)
            |> Seq.toList
            |> List.fold (fun acc (mid, tid) -> spuriousWake mid tid acc) state

        | SpuriousWakeupStrategy.Scripted wakeups ->
            wakeups
            |> List.filter (fun (t, _, _) -> t = tick)
            |> List.fold (fun acc (_, mid, tid) -> spuriousWake mid tid acc) state

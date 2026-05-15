namespace WoofWare.PawPrint

/// Deterministic state-machine for the seven `SystemNative_LowLevelMonitor_*`
/// QCalls that back `System.Threading.LowLevelMonitor`. The CoreCLR shape is a
/// `pthread_mutex_t` + `pthread_cond_t` pair; we reproduce the observable
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
/// Reentrancy: a monitor is non-reentrant, matching CoreCLR. A thread that
/// already owns the monitor and calls `acquire` again is deadlocking guest
/// code — we fail loudly rather than spin, so the bug surfaces here instead
/// of as a quiet round-robin starvation later. Reentrant locking is the
/// responsibility of `LowLevelLock`, layered on top of this primitive by the
/// BCL.
///
/// Timeouts: `TimedWait` is not implemented today. PawPrint has no virtual
/// clock yet, and silently treating "wait N ms" as "wait forever" would
/// turn flaky guest code into deterministic deadlock without obvious blame.
/// Calls to `timedWait` therefore fail loud; see the QCall handler in
/// `NativeLowLevelMonitor.fs` for the failure site.
///
/// Spurious wakeups: not generated. Guest code that depends on the absence
/// of spurious wakeups is incorrect against real CoreCLR, but adding them
/// would amplify nondeterminism we are not yet prepared to control. The
/// model is shaped to make insertion straightforward in future: moving a
/// thread from `WaitQueue` to `AcquireQueue` from outside `signalRelease`
/// is structurally the same operation.
[<RequireQualifiedAccess>]
module LowLevelMonitor =

    /// Look up the monitor for `id`, or fail loud. A retained handle that
    /// outlives `destroy` lands here so use-after-free shows up at the use
    /// site rather than as a confusing null-handle bug elsewhere.
    let private lookup (id : LowLevelMonitorId) (state : IlMachineState) : LowLevelMonitorState =
        match Map.tryFind id state.LowLevelMonitors with
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
        { state with
            LowLevelMonitors = state.LowLevelMonitors |> Map.add id monitor
        }

    /// Allocate a fresh monitor. Returns the new handle alongside the
    /// updated state. The handle is non-zero (counters start at 1) so the
    /// guest's "create failed → throw OOM" check does not fire.
    let create (state : IlMachineState) : LowLevelMonitorId * IlMachineState =
        let id = LowLevelMonitorId state.NextLowLevelMonitorId

        let state =
            { state with
                LowLevelMonitors = state.LowLevelMonitors |> Map.add id LowLevelMonitorState.empty
                NextLowLevelMonitorId = state.NextLowLevelMonitorId + 1
            }

        id, state

    /// Tear down a monitor. The CoreCLR contract is that `Destroy` runs after
    /// every `Acquire` has been paired with a `Release` and no thread is
    /// waiting — i.e. the monitor is fully quiescent. We enforce that contract
    /// loudly: destroying a monitor with an owner or a non-empty queue is a
    /// guest bug that would otherwise present as a use-after-free later.
    let destroy (id : LowLevelMonitorId) (state : IlMachineState) : IlMachineState =
        let monitor = lookup id state

        match monitor.Owner with
        | Some owner -> failwith $"LowLevelMonitor %O{id}: refusing to Destroy a monitor still held by thread %O{owner}"
        | None -> ()

        match monitor.AcquireQueue with
        | [] -> ()
        | waiters ->
            failwith
                $"LowLevelMonitor %O{id}: refusing to Destroy a monitor with %d{List.length waiters} thread(s) parked in BlockedOnMonitorAcquire (%A{waiters})"

        match monitor.WaitQueue with
        | [] -> ()
        | waiters ->
            failwith
                $"LowLevelMonitor %O{id}: refusing to Destroy a monitor with %d{List.length waiters} thread(s) parked in BlockedOnMonitorWait (%A{waiters})"

        { state with
            LowLevelMonitors = state.LowLevelMonitors |> Map.remove id
        }

    /// Try to acquire the monitor on behalf of `thread`. Returns `Acquired`
    /// if the call returns to the guest in the same step; the callsite
    /// should advance the program counter and stay Runnable. Returns
    /// `Blocked` if the thread must park: the callsite must NOT advance the
    /// program counter (the `Acquire` instruction is re-executed when the
    /// thread is woken — its second execution will see the thread at the
    /// head of `AcquireQueue` with the monitor unowned, and will fall
    /// through the `Acquired` branch).
    ///
    /// The thread's status is flipped to `BlockedOnMonitorAcquire` on the
    /// `Blocked` path. Fast-path acquisition (uncontended monitor) does NOT
    /// touch the thread's status; it stays `Runnable`.
    [<RequireQualifiedAccess>]
    type AcquireOutcome =
        | Acquired of IlMachineState
        | Blocked of IlMachineState

    let acquire (thread : ThreadId) (id : LowLevelMonitorId) (state : IlMachineState) : AcquireOutcome =
        let monitor = lookup id state

        match monitor.Owner with
        | None when List.isEmpty monitor.AcquireQueue ->
            // Uncontended fast path: take ownership now, return to the guest.
            let monitor =
                { monitor with
                    Owner = Some thread
                }

            state |> writeMonitor id monitor |> AcquireOutcome.Acquired

        | None when List.head monitor.AcquireQueue = thread ->
            // The caller is the just-woken head of the queue, re-running
            // the IL `Acquire` site after being roused by `release` or
            // `signalRelease`. Pop self from the head and take ownership;
            // any remaining tail threads stay parked until the chain of
            // releases reaches them. We keep the head in the queue across
            // the wake/take split (rather than removing it on wake) so
            // that a contemporaneous Acquire by a third thread sees the
            // woken thread still ahead of it and joins the tail, instead
            // of stealing the lock and starving the woken thread.
            let monitor =
                { monitor with
                    Owner = Some thread
                    AcquireQueue = List.tail monitor.AcquireQueue
                }

            state |> writeMonitor id monitor |> AcquireOutcome.Acquired

        | None ->
            // Owner is None but the AcquireQueue is non-empty AND we are
            // not the head: a different thread is already at the head and
            // we must queue behind it. Joining the tail preserves FIFO
            // ordering.
            let monitor =
                { monitor with
                    AcquireQueue = monitor.AcquireQueue @ [ thread ]
                }

            state
            |> writeMonitor id monitor
            |> Scheduler.setThreadStatus thread (ThreadStatus.BlockedOnMonitorAcquire id)
            |> AcquireOutcome.Blocked

        | Some owner when owner = thread ->
            // CoreCLR's LowLevelMonitor is non-reentrant; recursive acquire
            // would deadlock against itself on the pthread mutex. Failing
            // loud surfaces the bug here instead of as a silent deadlock at
            // the scheduler level.
            failwith
                $"LowLevelMonitor %O{id}: thread %O{thread} attempted recursive Acquire (monitor is non-reentrant; use LowLevelLock for reentrancy)."

        | Some _ ->
            // Contended path: park the thread at the tail of the queue.
            let monitor =
                { monitor with
                    AcquireQueue = monitor.AcquireQueue @ [ thread ]
                }

            state
            |> writeMonitor id monitor
            |> Scheduler.setThreadStatus thread (ThreadStatus.BlockedOnMonitorAcquire id)
            |> AcquireOutcome.Blocked

    /// Release the monitor held by `thread`. Wakes the FIFO head of the
    /// `AcquireQueue` (if any) by setting its status back to `Runnable`;
    /// the woken thread stays in the queue at the head and will take
    /// ownership when it re-runs its IL `Acquire` site (the head-of-queue
    /// fast path in `acquire`). Keeping the woken thread in the queue
    /// during the wake/take split prevents a concurrently-arriving
    /// Acquire from a third thread from stealing the lock and starving
    /// the woken thread — the third thread sees a non-empty queue and
    /// joins the tail, preserving FIFO fairness.
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

        | head :: _ ->
            // Wake the FIFO head but leave it in the queue. The woken
            // thread reacquires through `acquire`'s head-of-queue branch
            // on its next scheduler step, popping itself off as it does.
            let monitor =
                { monitor with
                    Owner = None
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
    ///      to `WaitQueue`, the monitor is released, and the caller's
    ///      status becomes `BlockedOnMonitorWait`. If the `AcquireQueue` is
    ///      non-empty at the moment of release, its head is woken via the
    ///      same path as `release`.
    ///
    ///   2. The wake-up step is driven by `signalRelease` (below): it
    ///      flips the head of the wait queue into `BlockedOnMonitorAcquire`
    ///      and enqueues it on the `AcquireQueue`. The thread reacquires
    ///      through the normal `Runnable → Acquired` transition when its
    ///      turn comes up.
    ///
    /// This preserves the atomicity contract from the guest's perspective:
    /// the guest cannot observe a state in which it has released the
    /// monitor but has not yet been parked, because both happen in a single
    /// `wait` call.
    let wait (thread : ThreadId) (id : LowLevelMonitorId) (state : IlMachineState) : IlMachineState =
        let monitor = lookup id state

        match monitor.Owner with
        | Some owner when owner = thread -> ()
        | Some owner ->
            failwith $"LowLevelMonitor %O{id}: thread %O{thread} called Wait but the monitor is owned by %O{owner}"
        | None -> failwith $"LowLevelMonitor %O{id}: thread %O{thread} called Wait but the monitor is unowned"

        // Atomically: release the monitor and park the caller in the wait
        // queue. We must NOT push the caller onto the AcquireQueue — Wait
        // does not contend for the monitor until Signal_Release rouses it.
        match monitor.AcquireQueue with
        | [] ->
            let monitor =
                { monitor with
                    Owner = None
                    WaitQueue = monitor.WaitQueue @ [ thread ]
                }

            state
            |> writeMonitor id monitor
            |> Scheduler.setThreadStatus thread (ThreadStatus.BlockedOnMonitorWait id)

        | head :: _ ->
            // Acquire-queue head gets woken as part of the release; the
            // caller goes onto the wait queue with the monitor released.
            // The woken head stays in the AcquireQueue (head-of-queue
            // wake/take split — see `release`); it will pop itself off
            // when it re-runs its IL `Acquire` site.
            let monitor =
                { monitor with
                    Owner = None
                    WaitQueue = monitor.WaitQueue @ [ thread ]
                }

            state
            |> writeMonitor id monitor
            |> Scheduler.setThreadStatus head ThreadStatus.Runnable
            |> Scheduler.setThreadStatus thread (ThreadStatus.BlockedOnMonitorWait id)

    /// `Signal_Release` is the wakeup half of the condvar protocol. The
    /// caller must hold the monitor; the call wakes at most one thread
    /// from the wait queue (FIFO) and releases the monitor. The woken
    /// thread is moved from the wait queue to the tail of the acquire
    /// queue and its status flips from `BlockedOnMonitorWait` to
    /// `BlockedOnMonitorAcquire` — it must contend for the monitor again
    /// before its original `Wait` call returns. (CoreCLR's
    /// `LowLevelMonitor::Signal_Release` is documented as "signal a single
    /// waiter and release the lock"; our split into "wake + queue" matches
    /// the observable semantics.)
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
            // No waiter to signal: this is equivalent to a plain Release.
            release thread id state
        | waiter :: restWait ->
            // Move the woken waiter to the acquire queue (FIFO tail) and
            // release the monitor.
            //
            // Subtle: the woken waiter is BlockedOnMonitorAcquire, not
            // Runnable, even though the AcquireQueue head may also be empty.
            // Whoever ends up at the head of the AcquireQueue is woken by
            // `release` below. The waiter we just moved is at the tail,
            // unless the prior AcquireQueue was empty, in which case it's
            // at the head and gets woken immediately.
            let monitor =
                { monitor with
                    WaitQueue = restWait
                    AcquireQueue = monitor.AcquireQueue @ [ waiter ]
                }

            let state =
                state
                |> writeMonitor id monitor
                |> Scheduler.setThreadStatus waiter (ThreadStatus.BlockedOnMonitorAcquire id)

            // Now release on behalf of the caller. This will wake the head
            // of the (newly-updated) acquire queue.
            release thread id state

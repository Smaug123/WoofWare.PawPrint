namespace WoofWare.PawPrint

/// Deterministic state machine for the Win32-shaped wait-handle kernel
/// objects exposed to managed code through `CreateSemaphoreExW`,
/// `ReleaseSemaphore`, `CloseHandle`, `WaitHandle_WaitOneCore`,
/// `WaitHandle_WaitOnePrioritized`, `PAL_CreateMutexW`, and
/// `ReleaseMutex`. On .NET 10 CoreCLR-on-Unix the BCL compiles
/// `Semaphore.Windows.cs` / `Mutex.CoreCLR.Unix.cs` regardless of host,
/// with `Libraries.Kernel32` rebound to `RuntimeHelpers.QCall`; every
/// Kernel32 LibraryImport therefore routes to the runtime as a QCall
/// whose entry point uses the Win32 wide-string name. PawPrint
/// reproduces the observable semantics through `WaitHandleState`
/// (registry value, kind-tagged) and one `ThreadStatus` case
/// (`BlockedOnWaitHandle`).
///
/// Two kinds are supported today: semaphore (`Count`-based) and mutex
/// (ownership-based, re-entrant on owner, abandoned-flag-aware). The
/// DU's job is to keep "kind dispatch in WaitOne/Close" exhaustive so
/// the compiler catches a missing branch when `Event` lands.
///
/// The module is pure: every transition is `IlMachineState ->
/// IlMachineState` (or returns an outcome carrying the next state) and
/// never reads from a real clock, the host's semaphore implementation, or
/// any nondeterministic source. Ordering decisions over `WaitQueue` are
/// FIFO, mirroring the `LowLevelMonitor` AcquireQueue contract that
/// higher-level guest primitives (`LowLevelLifoSemaphore`,
/// `PortableThreadPool`) depend on.
///
/// Timeouts: non-zero finite timeouts on `WaitHandle_WaitOneCore` /
/// `WaitHandle_WaitOnePrioritized` are not implemented today, for the
/// same reason `LowLevelMonitor.TimedWait` is not — PawPrint has no
/// virtual clock. Calls with a non-zero finite timeout fail loud in
/// the native handler. The zero-timeout case is fully deterministic
/// (`tryWaitOne`): it does not park, never touches the queue, and
/// returns `WAIT_OBJECT_0` or `WAIT_TIMEOUT` depending on whether the
/// fast path applied.
///
/// Abandoned-mutex propagation: full support (rewriting the wake-time
/// return value of already-blocked waiters when their owner thread
/// terminates) is structural and out of scope for this slice — the
/// scheduler pushes `WAIT_OBJECT_0` at park time, so making the wake
/// produce `WAIT_ABANDONED` requires deferred-return-value
/// materialisation. Until that lands, `Scheduler.onThreadTerminated`
/// fails loud if a terminating thread still owns any mutex, so a real
/// guest reaching that case surfaces as a clean failure rather than a
/// silent permanent ownership.
[<RequireQualifiedAccess>]
module WaitHandle =

    /// Why `releaseSemaphore` rejected a request.
    [<RequireQualifiedAccess>]
    type ReleaseFailure =
        /// Adding `releaseCount` to the current count would breach the
        /// semaphore's maximum. The Win32 contract sets
        /// `ERROR_TOO_MANY_POSTS` and returns FALSE; the BCL turns that
        /// into a `SemaphoreFullException`. `attemptedTotal` is reported
        /// as `int64` so that the value stays meaningful when an `int32`
        /// add of two near-`Int32.MaxValue` ints would overflow — the
        /// check itself is done without computing the sum.
        | WouldExceedMaximum of attemptedTotal : int64 * maximum : int

    /// Why `releaseMutex` rejected a request. The Win32 `ReleaseMutex`
    /// returns FALSE and sets `ERROR_NOT_OWNER (0x120 = 288)` for both
    /// "free mutex" and "held by another thread" cases; the BCL's
    /// `Mutex.ReleaseMutex` translates that into
    /// `ApplicationException` / `SynchronizationLockException`.
    [<RequireQualifiedAccess>]
    type ReleaseMutexFailure =
        /// The mutex is free, or held by a thread other than the caller.
        | NotOwner

    /// Outcome of a `waitOne` call.
    ///
    /// `Acquired` and `AcquiredAbandoned` are both fast-path successes
    /// (the thread stays Runnable); the only observable difference is
    /// the integer return code (`WAIT_OBJECT_0` vs `WAIT_ABANDONED`),
    /// which the native handler unpacks. `AcquiredAbandoned` is only
    /// produced by the mutex variant on a `Free wasAbandoned=true`
    /// transition; semaphore acquires never produce it. The
    /// non-mutex-aware caller can still pattern-match against the
    /// constructor (it'd never fire) — that's tolerable because
    /// `waitOne` is the operation as a whole, and the abandoned outcome
    /// is properly a wait-handle-level concept rather than a
    /// kind-specific one.
    ///
    /// `Blocked` means the thread is parked at the wait queue and its
    /// status flipped to `BlockedOnWaitHandle`. The IL site advances in
    /// every case; when the parked thread is later woken its
    /// `WAIT_OBJECT_0` slot has already been pushed onto its eval stack
    /// at park time.
    [<RequireQualifiedAccess>]
    type WaitOutcome =
        | Acquired of IlMachineState
        | AcquiredAbandoned of IlMachineState
        | Blocked of IlMachineState

    /// Outcome of a non-blocking `tryWaitOne` probe (zero-timeout wait).
    /// `Acquired` means the fast path applied — count was decremented or
    /// the mutex was taken. `AcquiredAbandoned` is the mutex-on-abandoned
    /// flag analogue (still a fast-path success, but the caller pushes
    /// `WAIT_ABANDONED` rather than `WAIT_OBJECT_0`). `TimedOut` means
    /// the fast path could not apply; state is unchanged (no enqueue,
    /// no thread-status flip — that's the entire point of distinguishing
    /// this from `waitOne`). The native handler maps these to
    /// `WAIT_OBJECT_0` (0), `WAIT_ABANDONED` (0x80), and `WAIT_TIMEOUT`
    /// (0x102) respectively.
    [<RequireQualifiedAccess>]
    type TryWaitOutcome =
        | Acquired of IlMachineState
        | AcquiredAbandoned of IlMachineState
        | TimedOut of IlMachineState

    /// Look up the handle for `id`, or fail loud. A retained IntPtr that
    /// outlives `close` lands here so use-after-free shows up at the use
    /// site rather than as a confusing null-handle bug elsewhere.
    let private lookup (id : WaitHandleId) (state : IlMachineState) : WaitHandleState =
        match Map.tryFind id state.Kernel.WaitHandles with
        | Some handle -> handle
        | None ->
            failwith
                $"WaitHandle %O{id} is not registered; either it was never created or it has already been closed (use-after-free on a stale handle)."

    let private writeHandle (id : WaitHandleId) (handle : WaitHandleState) (state : IlMachineState) : IlMachineState =
        state.MapKernel (fun kernel ->
            { kernel with
                WaitHandles = kernel.WaitHandles |> Map.add id handle
            }
        )

    let private expectSemaphore (operation : string) (id : WaitHandleId) (handle : WaitHandleState) : SemaphoreState =
        match handle with
        | WaitHandleState.Semaphore s -> s
        | WaitHandleState.Mutex _ ->
            failwith
                $"%s{operation}: WaitHandle %O{id} is a Mutex, but this operation only accepts a Semaphore (e.g. ReleaseSemaphore / WaitOnePrioritized). This is a guest bug — the BCL would have failed in its own wrapper before reaching the runtime."

    let private expectMutex (operation : string) (id : WaitHandleId) (handle : WaitHandleState) : MutexState =
        match handle with
        | WaitHandleState.Mutex m -> m
        | WaitHandleState.Semaphore _ ->
            failwith
                $"%s{operation}: WaitHandle %O{id} is a Semaphore, but this operation only accepts a Mutex (e.g. ReleaseMutex). This is a guest bug — the BCL would have failed in its own wrapper before reaching the runtime."

    /// Allocate a fresh semaphore kernel object. Returns the new handle
    /// alongside the updated state. The handle is non-zero (counters
    /// start at 1) so the BCL's "create failed → throw" check does not
    /// fire.
    ///
    /// Fails loud on invalid `(initialCount, maximumCount)` rather than
    /// the Win32-style `ERROR_INVALID_PARAMETER` route: the BCL's
    /// `Semaphore..ctor` validates these arguments before the P/Invoke,
    /// so reaching the runtime with a bad pair indicates a guest that
    /// bypassed the wrapper (i.e. a bug).
    let createSemaphore
        (initialCount : int)
        (maximumCount : int)
        (state : IlMachineState)
        : WaitHandleId * IlMachineState
        =
        if maximumCount < 1 then
            failwith
                $"WaitHandle.createSemaphore: maximumCount = %d{maximumCount} is not strictly positive; the BCL Semaphore ctor would have thrown ArgumentOutOfRangeException before reaching the runtime."

        if initialCount < 0 then
            failwith
                $"WaitHandle.createSemaphore: initialCount = %d{initialCount} is negative; the BCL Semaphore ctor would have thrown ArgumentOutOfRangeException before reaching the runtime."

        if initialCount > maximumCount then
            failwith
                $"WaitHandle.createSemaphore: initialCount = %d{initialCount} exceeds maximumCount = %d{maximumCount}; the BCL Semaphore ctor would have thrown ArgumentException before reaching the runtime."

        let id = WaitHandleId state.Kernel.NextWaitHandleId

        let semaphore : SemaphoreState =
            {
                Count = initialCount
                Maximum = maximumCount
                WaitQueue = []
            }

        let state =
            state.MapKernel (fun kernel ->
                { kernel with
                    WaitHandles = kernel.WaitHandles |> Map.add id (WaitHandleState.Semaphore semaphore)
                    NextWaitHandleId = kernel.NextWaitHandleId + 1
                }
            )

        id, state

    /// Allocate a fresh mutex kernel object. Returns the new handle
    /// alongside the updated state. If `initialOwner = true`, the caller
    /// (`creator`) becomes the initial owner with `recursionCount = 1`,
    /// matching the Win32 `CreateMutex(bInitialOwner = TRUE)` contract;
    /// otherwise the mutex starts free.
    let createMutex
        (initialOwner : bool)
        (creator : ThreadId)
        (state : IlMachineState)
        : WaitHandleId * IlMachineState
        =
        let id = WaitHandleId state.Kernel.NextWaitHandleId

        let ownership =
            if initialOwner then
                MutexOwnership.Held (creator, 1)
            else
                MutexOwnership.Free false

        let mutex : MutexState =
            {
                Ownership = ownership
                WaitQueue = []
            }

        let state =
            state.MapKernel (fun kernel ->
                { kernel with
                    WaitHandles = kernel.WaitHandles |> Map.add id (WaitHandleState.Mutex mutex)
                    NextWaitHandleId = kernel.NextWaitHandleId + 1
                }
            )

        id, state

    /// Internal: kind-private semaphore waitOne. Used by the kind
    /// dispatcher in `waitOne` and the property tests that exercise the
    /// semaphore path directly.
    let private waitOneSemaphore
        (thread : ThreadId)
        (id : WaitHandleId)
        (semaphore : SemaphoreState)
        (state : IlMachineState)
        : WaitOutcome
        =
        if semaphore.Count > 0 then
            // Fast path: consume one unit and stay Runnable. The new
            // count is in `[0, Maximum - 1]`, preserving the invariant.
            let semaphore =
                { semaphore with
                    Count = semaphore.Count - 1
                }

            state
            |> writeHandle id (WaitHandleState.Semaphore semaphore)
            |> WaitOutcome.Acquired
        else
            // Slow path: park at the tail of the FIFO wait queue.
            // The unit will be consumed inline by `releaseSemaphore`
            // when it wakes us, so we leave Count at 0.
            let semaphore =
                { semaphore with
                    WaitQueue = semaphore.WaitQueue @ [ thread ]
                }

            state
            |> writeHandle id (WaitHandleState.Semaphore semaphore)
            |> Scheduler.setThreadStatus thread (ThreadStatus.BlockedOnWaitHandle id)
            |> WaitOutcome.Blocked

    /// Internal: kind-private mutex waitOne. Re-entrant on owner; the
    /// `Free wasAbandoned=true` transition produces `AcquiredAbandoned`
    /// and clears the flag.
    let private waitOneMutex
        (thread : ThreadId)
        (id : WaitHandleId)
        (mutex : MutexState)
        (state : IlMachineState)
        : WaitOutcome
        =
        match mutex.Ownership with
        | MutexOwnership.Free wasAbandoned ->
            // Take ownership; clear the abandoned flag.
            let mutex =
                { mutex with
                    Ownership = MutexOwnership.Held (thread, 1)
                }

            let state = writeHandle id (WaitHandleState.Mutex mutex) state

            if wasAbandoned then
                WaitOutcome.AcquiredAbandoned state
            else
                WaitOutcome.Acquired state
        | MutexOwnership.Held (owner, recursionCount) when owner = thread ->
            // Re-entrant acquisition: bump recursion count.
            let mutex =
                { mutex with
                    Ownership = MutexOwnership.Held (owner, recursionCount + 1)
                }

            state |> writeHandle id (WaitHandleState.Mutex mutex) |> WaitOutcome.Acquired
        | MutexOwnership.Held _ ->
            // Held by another thread: park at FIFO tail.
            let mutex =
                { mutex with
                    WaitQueue = mutex.WaitQueue @ [ thread ]
                }

            state
            |> writeHandle id (WaitHandleState.Mutex mutex)
            |> Scheduler.setThreadStatus thread (ThreadStatus.BlockedOnWaitHandle id)
            |> WaitOutcome.Blocked

    /// Try to take ownership of `id` on behalf of `thread`. Dispatches
    /// by kind:
    ///
    ///  - Semaphore: decrement the count if positive; otherwise park at
    ///    the FIFO tail of `WaitQueue` and flip the thread's status to
    ///    `BlockedOnWaitHandle`.
    ///  - Mutex: re-entrant fast path on the owning thread; take the
    ///    free mutex (producing `AcquiredAbandoned` iff the abandoned
    ///    flag was set, clearing it); otherwise park at the FIFO tail.
    ///
    /// The IL `WaitOne` site advances in every case (the native handler
    /// returns `Stepped/Executed` for all three outcomes). When a parked
    /// thread is later woken, its `WAIT_OBJECT_0` slot has already been
    /// pushed onto its eval stack at park time — see the
    /// abandoned-mutex-propagation note on the module docstring.
    let waitOne (thread : ThreadId) (id : WaitHandleId) (state : IlMachineState) : WaitOutcome =
        let handle = lookup id state

        match handle with
        | WaitHandleState.Semaphore semaphore -> waitOneSemaphore thread id semaphore state
        | WaitHandleState.Mutex mutex -> waitOneMutex thread id mutex state

    /// Non-blocking probe used to model the zero-timeout `WaitOne(0)`
    /// path. CoreCLR's contract for a zero millisecond timeout: try the
    /// fast path; if it cannot succeed, return `WAIT_TIMEOUT` without
    /// ever entering the wait queue. Distinct from `waitOne` because the
    /// caller is *not* parked — leaving the thread Runnable is the entire
    /// observable difference from a guest's point of view, and silently
    /// parking would deadlock guests that rely on `WaitOne(0)` as a poll.
    ///
    /// `thread` is consumed by the mutex kind (for re-entrancy and
    /// ownership identity); the semaphore kind ignores it.
    let tryWaitOne (thread : ThreadId) (id : WaitHandleId) (state : IlMachineState) : TryWaitOutcome =
        let handle = lookup id state

        match handle with
        | WaitHandleState.Semaphore semaphore ->
            if semaphore.Count > 0 then
                let semaphore =
                    { semaphore with
                        Count = semaphore.Count - 1
                    }

                state
                |> writeHandle id (WaitHandleState.Semaphore semaphore)
                |> TryWaitOutcome.Acquired
            else
                // State must be returned verbatim: no enqueue, no status flip.
                TryWaitOutcome.TimedOut state
        | WaitHandleState.Mutex mutex ->
            match mutex.Ownership with
            | MutexOwnership.Free wasAbandoned ->
                let mutex =
                    { mutex with
                        Ownership = MutexOwnership.Held (thread, 1)
                    }

                let state = writeHandle id (WaitHandleState.Mutex mutex) state

                if wasAbandoned then
                    TryWaitOutcome.AcquiredAbandoned state
                else
                    TryWaitOutcome.Acquired state
            | MutexOwnership.Held (owner, recursionCount) when owner = thread ->
                let mutex =
                    { mutex with
                        Ownership = MutexOwnership.Held (owner, recursionCount + 1)
                    }

                state |> writeHandle id (WaitHandleState.Mutex mutex) |> TryWaitOutcome.Acquired
            | MutexOwnership.Held _ ->
                // Held by another thread: no enqueue, no status flip.
                TryWaitOutcome.TimedOut state

    /// Semaphore-only non-blocking probe used by the prioritized
    /// dispatch path. Keeping the two `tryWait` variants split (rather
    /// than letting prioritized fall through to the kind-generic
    /// `tryWaitOne`) lets the prioritized native handler keep its
    /// "mutexes are not legal on this entry point" guarantee — passing
    /// a mutex handle to `WaitHandle_WaitOnePrioritized` is a guest bug,
    /// and we want it to fail loud rather than accidentally succeed via
    /// the kind-generic probe.
    let internal tryWaitOneSemaphore (id : WaitHandleId) (state : IlMachineState) : TryWaitOutcome =
        let handle = lookup id state
        let semaphore = expectSemaphore "WaitHandle.tryWaitOneSemaphore" id handle

        if semaphore.Count > 0 then
            let semaphore =
                { semaphore with
                    Count = semaphore.Count - 1
                }

            state
            |> writeHandle id (WaitHandleState.Semaphore semaphore)
            |> TryWaitOutcome.Acquired
        else
            TryWaitOutcome.TimedOut state

    /// Prioritized variant of `waitOne` matching the
    /// `PAL_WaitForSingleObjectPrioritized` contract: when the wait
    /// blocks, the thread is registered at the **head** of `WaitQueue`
    /// (not the tail). A subsequent `releaseSemaphore` therefore wakes
    /// the most recently registered prioritized waiter first, giving
    /// LIFO release semantics among prioritized waiters — and strict
    /// precedence over any earlier-arrived non-prioritized waiters.
    /// `LowLevelLifoSemaphore` (the PortableThreadPool worker park
    /// primitive on Unix) imports this entry point precisely because its
    /// fairness contract is LIFO over the kernel semaphore's FIFO base.
    ///
    /// The fast path is identical to `waitOne`: priority only matters
    /// when the call has to block.
    let waitOnePrioritized (thread : ThreadId) (id : WaitHandleId) (state : IlMachineState) : WaitOutcome =
        let handle = lookup id state
        let semaphore = expectSemaphore "WaitHandle.waitOnePrioritized" id handle

        if semaphore.Count > 0 then
            let semaphore =
                { semaphore with
                    Count = semaphore.Count - 1
                }

            state
            |> writeHandle id (WaitHandleState.Semaphore semaphore)
            |> WaitOutcome.Acquired
        else
            // Prioritized slow path: prepend to the head of the queue.
            // The wake step in `releaseSemaphore` pops from the head, so
            // a later release will hand its freshly-issued unit to this
            // thread before any earlier-enqueued waiter.
            let semaphore =
                { semaphore with
                    WaitQueue = thread :: semaphore.WaitQueue
                }

            state
            |> writeHandle id (WaitHandleState.Semaphore semaphore)
            |> Scheduler.setThreadStatus thread (ThreadStatus.BlockedOnWaitHandle id)
            |> WaitOutcome.Blocked

    /// Increment the semaphore by `releaseCount`, waking up to that many
    /// FIFO-head waiters. Returns the previous count on success (matching
    /// the Win32 `lpPreviousCount` out-parameter contract) and the
    /// updated state. On overflow returns `ReleaseFailure
    /// .WouldExceedMaximum` and the state is unchanged — the native
    /// handler then sets `LastPInvokeError = ERROR_TOO_MANY_POSTS` and
    /// returns FALSE.
    ///
    /// The wake step transfers ownership of the freshly-added units
    /// directly: each woken waiter consumes one unit before we add it
    /// to `Count`, so a release of N that wakes K waiters (K ≤ N) leaves
    /// only `(N - K)` units accumulated in `Count`. This mirrors the
    /// CoreCLR direct-handoff posture used by `Monitor.Exit` and
    /// `LowLevelMonitor.release` — the woken thread resumes past its
    /// `WaitOne` call site already holding a unit, so its IL flow is
    /// sound without re-running the decrement.
    let releaseSemaphore
        (id : WaitHandleId)
        (releaseCount : int)
        (state : IlMachineState)
        : Result<int, ReleaseFailure> * IlMachineState
        =
        if releaseCount < 1 then
            failwith
                $"WaitHandle.releaseSemaphore: releaseCount = %d{releaseCount} is not strictly positive; the BCL Semaphore.Release validates this before the P/Invoke."

        let handle = lookup id state
        let semaphore = expectSemaphore "WaitHandle.releaseSemaphore" id handle

        let previousCount = semaphore.Count
        // Order the comparison so the int32 add can never overflow:
        // `previousCount ≤ Maximum` is an invariant, so `Maximum -
        // previousCount` is non-negative, and `releaseCount ≥ 1` is
        // asserted above. A guest that creates a semaphore at near
        // `Int32.MaxValue` and posts a release would otherwise wrap to a
        // negative `attemptedTotal`, sneak past the maximum check, and
        // store a negative `Count`.
        if releaseCount > semaphore.Maximum - previousCount then
            // Compute `attemptedTotal` in int64 so the error stays
            // meaningful even in the overflow regime.
            let attemptedTotal = int64 previousCount + int64 releaseCount
            Error (ReleaseFailure.WouldExceedMaximum (attemptedTotal, semaphore.Maximum)), state
        else
            // Direct-handoff: each wake consumes one of the new units.
            // The wake step always pops from the head of `WaitQueue`;
            // ordering discipline lives at insertion time (`waitOne`
            // appends to the tail for FIFO; `waitOnePrioritized`
            // prepends to the head for LIFO over prioritized waiters,
            // matching `PAL_WaitForSingleObjectPrioritized`). The
            // LowLevelLifoSemaphore fairness contract that
            // PortableThreadPool depends on is delivered through that
            // insertion-side asymmetry, not by changing this wake step.
            let toWake = min releaseCount (List.length semaphore.WaitQueue)
            let wakers, remainingQueue = List.splitAt toWake semaphore.WaitQueue
            // Now safe: the guard above ensures `previousCount +
            // releaseCount ≤ Maximum`, so the int32 add cannot overflow.
            let newCount = previousCount + releaseCount - toWake

            let semaphore =
                { semaphore with
                    Count = newCount
                    WaitQueue = remainingQueue
                }

            let state =
                state
                |> writeHandle id (WaitHandleState.Semaphore semaphore)
                |> (fun s ->
                    wakers
                    |> List.fold (fun acc tid -> Scheduler.setThreadStatus tid ThreadStatus.Runnable acc) s
                )

            Ok previousCount, state

    /// Decrement the recursion count, or — if it was the outermost
    /// release — either mark the mutex free (no waiters) or hand
    /// ownership directly to the FIFO head of `WaitQueue` (the
    /// "direct-handoff" posture also used by `Monitor.Exit` and
    /// `LowLevelMonitor.release`; the woken thread resumes past its
    /// `WaitOne` call site already owning the mutex).
    ///
    /// Returns `Error NotOwner` if the mutex is free, or held by a
    /// thread other than `thread`; the state is unchanged in that case.
    /// `Error NotOwner` is what the Win32 `ReleaseMutex` returns for
    /// either case (with `ERROR_NOT_OWNER = 0x120`); the BCL maps the
    /// FALSE return into `SynchronizationLockException`.
    let releaseMutex
        (thread : ThreadId)
        (id : WaitHandleId)
        (state : IlMachineState)
        : Result<unit, ReleaseMutexFailure> * IlMachineState
        =
        let handle = lookup id state
        let mutex = expectMutex "WaitHandle.releaseMutex" id handle

        match mutex.Ownership with
        | MutexOwnership.Free _ -> Error ReleaseMutexFailure.NotOwner, state
        | MutexOwnership.Held (owner, _) when owner <> thread -> Error ReleaseMutexFailure.NotOwner, state
        | MutexOwnership.Held (_, recursionCount) when recursionCount > 1 ->
            // Inner release: drop the count by one, ownership unchanged.
            let mutex =
                { mutex with
                    Ownership = MutexOwnership.Held (thread, recursionCount - 1)
                }

            Ok (), writeHandle id (WaitHandleState.Mutex mutex) state
        | MutexOwnership.Held (_, _) ->
            // Outermost release: either mark free, or direct-handoff to
            // the FIFO head of the wait queue.
            match mutex.WaitQueue with
            | [] ->
                let mutex =
                    { mutex with
                        Ownership = MutexOwnership.Free false
                    }

                Ok (), writeHandle id (WaitHandleState.Mutex mutex) state
            | head :: rest ->
                let mutex =
                    { mutex with
                        Ownership = MutexOwnership.Held (head, 1)
                        WaitQueue = rest
                    }

                let state =
                    state
                    |> writeHandle id (WaitHandleState.Mutex mutex)
                    |> Scheduler.setThreadStatus head ThreadStatus.Runnable

                Ok (), state

    /// Tear down a wait handle. The Win32 contract is that `CloseHandle`
    /// runs after the handle is fully quiescent — no thread is currently
    /// waiting on it. We enforce that contract loudly: closing a handle
    /// with a non-empty wait queue is a guest bug that would otherwise
    /// present as the waiters being permanently parked on a recycled
    /// object.
    ///
    /// For mutexes, we additionally require the mutex to be free.
    /// Closing a still-held mutex is a guest bug: real CoreCLR closes
    /// the handle and the kernel reaps the mutex, but any waiter that
    /// somehow held a separate handle would deadlock. We don't model
    /// shared/duplicated handles, so the contract is "release before
    /// dispose", and we fail loud otherwise.
    ///
    /// IDs are never reused; the entry is removed from the table, so a
    /// retained `IntPtr` to the closed handle fails loudly at the next
    /// use.
    let close (id : WaitHandleId) (state : IlMachineState) : IlMachineState =
        let handle = lookup id state

        match handle with
        | WaitHandleState.Semaphore semaphore ->
            match semaphore.WaitQueue with
            | [] -> ()
            | waiters ->
                failwith
                    $"WaitHandle %O{id}: refusing to Close a semaphore with %d{List.length waiters} thread(s) parked in BlockedOnWaitHandle (%A{waiters}); the guest must Release before Disposing."
        | WaitHandleState.Mutex mutex ->
            match mutex.WaitQueue with
            | [] -> ()
            | waiters ->
                failwith
                    $"WaitHandle %O{id}: refusing to Close a mutex with %d{List.length waiters} thread(s) parked in BlockedOnWaitHandle (%A{waiters}); the guest must Release before Disposing."

            match mutex.Ownership with
            | MutexOwnership.Free _ -> ()
            | MutexOwnership.Held (owner, recursionCount) ->
                failwith
                    $"WaitHandle %O{id}: refusing to Close a mutex still held by thread %O{owner} (recursion count = %d{recursionCount}); the guest must ReleaseMutex before Disposing."

        state.MapKernel (fun kernel ->
            { kernel with
                WaitHandles = kernel.WaitHandles |> Map.remove id
            }
        )

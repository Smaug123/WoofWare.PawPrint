namespace WoofWare.PawPrint

/// Deterministic state machine for the Win32-shaped wait-handle kernel
/// objects exposed to managed code through `CreateSemaphoreExW`,
/// `ReleaseSemaphore`, `CloseHandle`, and `WaitHandle_WaitOneCore`. On
/// .NET 10 CoreCLR-on-Unix the BCL compiles `Semaphore.Windows.cs`
/// regardless of host, with `Libraries.Kernel32` rebound to
/// `RuntimeHelpers.QCall`; every Kernel32 LibraryImport therefore routes
/// to the runtime as a QCall whose entry point uses the Win32 wide-string
/// name. PawPrint reproduces the observable semantics through
/// `WaitHandleState` (registry value, kind-tagged) and one new
/// `ThreadStatus` case (`BlockedOnWaitHandle`).
///
/// This first slice supports the semaphore variant only. The DU's job is
/// to keep "kind dispatch in WaitOne/Close" exhaustive so the compiler
/// catches a missing branch when `Event` or `Mutex` lands.
///
/// The module is pure: every transition is `IlMachineState ->
/// IlMachineState` (or returns an outcome carrying the next state) and
/// never reads from a real clock, the host's semaphore implementation, or
/// any nondeterministic source. Ordering decisions over `WaitQueue` are
/// FIFO, mirroring the `LowLevelMonitor` AcquireQueue contract that
/// higher-level guest primitives (`LowLevelLifoSemaphore`,
/// `PortableThreadPool`) depend on.
///
/// Timeouts: finite timeouts on `WaitHandle_WaitOneCore` are not
/// implemented today, for the same reason `LowLevelMonitor.TimedWait`
/// is not — PawPrint has no virtual clock. Calls with a finite timeout
/// fail loud in the native handler.
[<RequireQualifiedAccess>]
module WaitHandle =

    /// Why `releaseSemaphore` rejected a request.
    [<RequireQualifiedAccess>]
    type ReleaseFailure =
        /// Adding `releaseCount` to the current count would breach the
        /// semaphore's maximum. The Win32 contract sets
        /// `ERROR_TOO_MANY_POSTS` and returns FALSE; the BCL turns that
        /// into a `SemaphoreFullException`.
        | WouldExceedMaximum of attemptedTotal : int * maximum : int

    /// Outcome of a `waitOne` call. The native handler treats both
    /// constructors identically — advance IL and return `WAIT_OBJECT_0` —
    /// because the IL `WaitOne` call site has already moved past itself
    /// by the time the scheduler picks the woken thread. The split
    /// exists so callers (notably future multi-handle waits) can
    /// distinguish "took ownership on the fast path" from "parked".
    [<RequireQualifiedAccess>]
    type WaitOutcome =
        | Acquired of IlMachineState
        | Blocked of IlMachineState

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

    /// Try to take one unit from the semaphore on behalf of `thread`.
    ///
    /// Returns `Acquired` if the fast path applied: count was > 0 and we
    /// decremented it. The IL `WaitOne` site advances and the thread
    /// stays Runnable.
    ///
    /// Returns `Blocked` if count was 0: the thread is parked at the
    /// FIFO tail of `WaitQueue` and its status flips to
    /// `BlockedOnWaitHandle`. The IL site still advances (the native
    /// handler returns `Stepped/Executed` in both cases); when the
    /// thread is later woken by `releaseSemaphore`, its count slot has
    /// already been consumed by the wake step, so resuming past the
    /// `WaitOne` call is correct without re-decrementing.
    let waitOne (thread : ThreadId) (id : WaitHandleId) (state : IlMachineState) : WaitOutcome =
        let handle = lookup id state
        let semaphore = expectSemaphore "WaitHandle.waitOne" id handle

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
        let attemptedTotal = previousCount + releaseCount

        if attemptedTotal > semaphore.Maximum then
            Error (ReleaseFailure.WouldExceedMaximum (attemptedTotal, semaphore.Maximum)), state
        else
            // Direct-handoff: each wake consumes one of the new units.
            // FIFO over WaitQueue is load-bearing for the
            // LowLevelLifoSemaphore fairness contract higher up the
            // stack — switching to LIFO or arbitrary order is not a
            // refactor.
            let toWake = min releaseCount (List.length semaphore.WaitQueue)
            let wakers, remainingQueue = List.splitAt toWake semaphore.WaitQueue

            let semaphore =
                { semaphore with
                    Count = attemptedTotal - toWake
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

    /// Tear down a wait handle. The Win32 contract is that `CloseHandle`
    /// runs after the handle is fully quiescent — no thread is currently
    /// waiting on it. We enforce that contract loudly: closing a handle
    /// with a non-empty wait queue is a guest bug that would otherwise
    /// present as the waiters being permanently parked on a recycled
    /// object.
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

        state.MapKernel (fun kernel ->
            { kernel with
                WaitHandles = kernel.WaitHandles |> Map.remove id
            }
        )

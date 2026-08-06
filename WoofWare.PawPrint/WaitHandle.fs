namespace WoofWare.PawPrint

/// Deterministic state machine for the Win32-shaped wait-handle kernel
/// objects exposed to managed code through `CreateSemaphoreExW`,
/// `ReleaseSemaphore`, `CloseHandle`, `WaitHandle_WaitOneCore`,
/// `WaitHandle_WaitOnePrioritized`, `PAL_CreateMutexW`, `ReleaseMutex`,
/// `CreateEventExW`, `SetEvent`, and `ResetEvent`. On .NET 10
/// CoreCLR-on-Unix the BCL compiles `Semaphore.Windows.cs` /
/// `Mutex.CoreCLR.Unix.cs` / `EventWaitHandle.Windows.cs` regardless of
/// host, with `Libraries.Kernel32` rebound to `RuntimeHelpers.QCall`;
/// every Kernel32 LibraryImport therefore routes to the runtime as a
/// QCall whose entry point uses the Win32 wide-string name. PawPrint
/// reproduces the observable semantics through `WaitHandleState`
/// (registry value, kind-tagged) and two `ThreadStatus` cases
/// (`BlockedOnWaitHandle`, `BlockedOnWaitHandles`).
///
/// Multi-handle waits (`WaitHandle.WaitAny` / `WaitAll`, through the
/// `WaitHandle_WaitMultipleIgnoringSyncContext` QCall) share the same
/// registry and queues. Three things distinguish them:
///
///  - A waiter is queued on *every* handle it names, so the wake path is a
///    walk (`tryGrantOne`) rather than a pop: the head-most *satisfiable*
///    entry wins, and a wait-all entry that is unacquirable on one of its
///    other handles is skipped and left queued. That mirrors the PAL's
///    waiting-thread walk, and weakens the per-kind queue invariants — see
///    the notes on `SemaphoreState` / `MutexState` / `EventState`.
///  - Signalling is expressed as "make the resource available, then drain
///    the queue" rather than as a bespoke handoff per kind, so `Count`,
///    ownership and `Signaled` are the only things the three release paths
///    differ in. Direct handoff survives because nothing runs between the
///    two steps.
///  - The value the guest sees is materialised at wake time, not park time:
///    a wait-any returns `WAIT_OBJECT_0 + index`, which is unknowable until
///    some handle satisfies it. `grantTo` rewrites the optimistic slot, the
///    same mechanism the timeout path already used.
///
/// Three kinds are supported today: semaphore (`Count`-based), mutex
/// (ownership-based, re-entrant on owner, abandoned-flag-aware), and
/// event (Manual or Auto reset, with a `Signaled` flag and a FIFO
/// `WaitQueue`). The DU's job is to keep "kind dispatch in WaitOne /
/// Close" exhaustive so the compiler catches a missing branch when a
/// further kind lands.
///
/// The module is pure: every transition is `IlMachineState ->
/// IlMachineState` (or returns an outcome carrying the next state) and
/// never reads from a real clock, the host's semaphore implementation, or
/// any nondeterministic source. Ordering decisions over `WaitQueue` are
/// FIFO, mirroring the `LowLevelMonitor` AcquireQueue contract that
/// higher-level guest primitives (`LowLevelLifoSemaphore`,
/// `PortableThreadPool`) depend on.
///
/// Timeouts: finite timeouts on `WaitHandle_WaitOneCore` /
/// `WaitHandle_WaitOnePrioritized` are supported through the
/// virtual-clock plumbing in `EmulatedKernel.VirtualClockMs`. The
/// native handler converts a millisecond timeout into an absolute
/// deadline against `VirtualClockMs` and threads it through `waitOne` /
/// `waitOnePrioritized`; if the slow path fires, the deadline is
/// recorded on the parked thread's `BlockedOnWaitHandle` status. The
/// driver loop fires `fireTimeout` for any thread whose deadline has
/// elapsed, which dequeues the thread, rewrites the park-time
/// `WAIT_OBJECT_0` slot on its eval stack to `WAIT_TIMEOUT`, and flips
/// it back to `Runnable`. The zero-timeout case is still routed
/// through `tryWaitOne` (fully deterministic, no park).
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
        | WaitHandleState.Event _ ->
            failwith
                $"%s{operation}: WaitHandle %O{id} is an Event, but this operation only accepts a Semaphore (e.g. ReleaseSemaphore / WaitOnePrioritized). This is a guest bug — the BCL would have failed in its own wrapper before reaching the runtime."

    let private expectMutex (operation : string) (id : WaitHandleId) (handle : WaitHandleState) : MutexState =
        match handle with
        | WaitHandleState.Mutex m -> m
        | WaitHandleState.Semaphore _ ->
            failwith
                $"%s{operation}: WaitHandle %O{id} is a Semaphore, but this operation only accepts a Mutex (e.g. ReleaseMutex). This is a guest bug — the BCL would have failed in its own wrapper before reaching the runtime."
        | WaitHandleState.Event _ ->
            failwith
                $"%s{operation}: WaitHandle %O{id} is an Event, but this operation only accepts a Mutex (e.g. ReleaseMutex). This is a guest bug — the BCL would have failed in its own wrapper before reaching the runtime."

    let private expectEvent (operation : string) (id : WaitHandleId) (handle : WaitHandleState) : EventState =
        match handle with
        | WaitHandleState.Event e -> e
        | WaitHandleState.Semaphore _ ->
            failwith
                $"%s{operation}: WaitHandle %O{id} is a Semaphore, but this operation only accepts an Event (e.g. SetEvent / ResetEvent). This is a guest bug — the BCL would have failed in its own wrapper before reaching the runtime."
        | WaitHandleState.Mutex _ ->
            failwith
                $"%s{operation}: WaitHandle %O{id} is a Mutex, but this operation only accepts an Event (e.g. SetEvent / ResetEvent). This is a guest bug — the BCL would have failed in its own wrapper before reaching the runtime."

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

    /// Allocate a fresh event kernel object. Returns the new handle
    /// alongside the updated state. `initialState = true` corresponds to
    /// `CREATE_EVENT_INITIAL_SET` (the event starts signalled) — by the
    /// `Signaled ⇒ WaitQueue = []` invariant the queue is necessarily
    /// empty at create time, so there is no waiter to wake. `mode` is
    /// fixed for the lifetime of the handle.
    let createEvent
        (initialState : bool)
        (mode : EventResetMode)
        (state : IlMachineState)
        : WaitHandleId * IlMachineState
        =
        let id = WaitHandleId state.Kernel.NextWaitHandleId

        let event : EventState =
            {
                Mode = mode
                Signaled = initialState
                WaitQueue = []
            }

        let state =
            state.MapKernel (fun kernel ->
                { kernel with
                    WaitHandles = kernel.WaitHandles |> Map.add id (WaitHandleState.Event event)
                    NextWaitHandleId = kernel.NextWaitHandleId + 1
                }
            )

        id, state

    /// `WAIT_OBJECT_0 = 0`. The Win32 return code for a successful wait — the
    /// wait acquired its target object. Matches `WaitHandle.WaitSuccess` in
    /// the BCL. A multi-handle wait-any returns this *plus* the index of the
    /// handle that satisfied it.
    let waitObjectZero : int = 0

    /// `WAIT_ABANDONED_0 = 0x80`. The wait acquired its target mutex, but the
    /// previous owner terminated without calling `ReleaseMutex`. The BCL
    /// translates this into `AbandonedMutexException`; a multi-handle wait-any
    /// adds the satisfying handle's index, and `WaitHandle.WaitMultiple` uses
    /// the `[WaitAbandoned, WaitAbandoned + count)` range to recover it.
    let waitAbandoned : int = 0x80

    /// `WAIT_TIMEOUT = 0x102`. The wait did not acquire its target before the
    /// timeout expired. `WaitHandle.WaitOne(int)` compares the return against
    /// `WaitHandle.WaitTimeout = 0x102` to decide what to hand the guest.
    let waitTimeout : int = 0x102

    /// The wait queue of a handle, independent of its kind. All three kinds
    /// carry the same `ThreadId list` with the same FIFO discipline, so the
    /// wake walk can be written once rather than three times.
    let private waitQueueOf (handle : WaitHandleState) : ThreadId list =
        match handle with
        | WaitHandleState.Semaphore s -> s.WaitQueue
        | WaitHandleState.Mutex m -> m.WaitQueue
        | WaitHandleState.Event e -> e.WaitQueue

    let private withWaitQueue (queue : ThreadId list) (handle : WaitHandleState) : WaitHandleState =
        match handle with
        | WaitHandleState.Semaphore s ->
            WaitHandleState.Semaphore
                { s with
                    WaitQueue = queue
                }
        | WaitHandleState.Mutex m ->
            WaitHandleState.Mutex
                { m with
                    WaitQueue = queue
                }
        | WaitHandleState.Event e ->
            WaitHandleState.Event
                { e with
                    WaitQueue = queue
                }

    /// Whether `thread` could take `handle` right now without blocking. This
    /// is the PAL's `CanThreadWaitWithoutBlocking`, and is the single
    /// definition consulted by every fast path, by the wake walk's
    /// satisfiability test, and by the wait-all "is the rest of the wait
    /// satisfied" check. Keeping one definition is what stops those three from
    /// drifting apart.
    ///
    /// `thread` matters only for mutexes, which are re-entrant on their owner.
    let private isAcquirable (thread : ThreadId) (handle : WaitHandleState) : bool =
        match handle with
        | WaitHandleState.Semaphore semaphore -> semaphore.Count > 0
        | WaitHandleState.Mutex mutex ->
            match mutex.Ownership with
            | MutexOwnership.Free _ -> true
            | MutexOwnership.Held (owner, _) -> owner = thread
        | WaitHandleState.Event event -> event.Signaled

    /// Consume `handle` on behalf of `thread`, returning the updated handle
    /// and whether the acquisition observed an abandoned mutex (which the
    /// caller reports as `WAIT_ABANDONED`, clearing the sticky flag).
    ///
    /// Precondition: `isAcquirable thread handle`. Violating it is an
    /// interpreter bug, not a guest one, so it fails loud rather than
    /// returning an outcome.
    let private acquireFor (thread : ThreadId) (handle : WaitHandleState) : WaitHandleState * bool =
        match handle with
        | WaitHandleState.Semaphore semaphore ->
            if semaphore.Count <= 0 then
                failwith
                    $"WaitHandle.acquireFor: semaphore count is %d{semaphore.Count}; caller must check isAcquirable first."

            WaitHandleState.Semaphore
                { semaphore with
                    Count = semaphore.Count - 1
                },
            false
        | WaitHandleState.Mutex mutex ->
            match mutex.Ownership with
            | MutexOwnership.Free wasAbandoned ->
                WaitHandleState.Mutex
                    { mutex with
                        Ownership = MutexOwnership.Held (thread, 1)
                    },
                wasAbandoned
            | MutexOwnership.Held (owner, recursionCount) when owner = thread ->
                WaitHandleState.Mutex
                    { mutex with
                        Ownership = MutexOwnership.Held (owner, recursionCount + 1)
                    },
                false
            | MutexOwnership.Held (owner, _) ->
                failwith
                    $"WaitHandle.acquireFor: mutex is held by thread %O{owner}, not %O{thread}; caller must check isAcquirable first."
        | WaitHandleState.Event event ->
            if not event.Signaled then
                failwith "WaitHandle.acquireFor: event is not signalled; caller must check isAcquirable first."

            match event.Mode with
            | EventResetMode.Auto ->
                WaitHandleState.Event
                    { event with
                        Signaled = false
                    },
                false
            // A Manual event stays signalled: every concurrent waiter passes
            // through until an explicit `ResetEvent`.
            | EventResetMode.Manual -> WaitHandleState.Event event, false

    /// What a queued thread is waiting for, recovered from its status. A
    /// thread sitting in a handle's `WaitQueue` under any other status is a
    /// structural invariant violation rather than a guest bug.
    [<RequireQualifiedAccess>]
    type private WaitRegistration =
        | Single of WaitHandleId
        | Multiple of handles : WaitHandleId list * waitAll : bool

    let private registrationOf (thread : ThreadId) (id : WaitHandleId) (state : IlMachineState) : WaitRegistration =
        match state.ThreadState.[thread].Status with
        | ThreadStatus.BlockedOnWaitHandle (handle, _) -> WaitRegistration.Single handle
        | ThreadStatus.BlockedOnWaitHandles (handles, waitAll, _) -> WaitRegistration.Multiple (handles, waitAll)
        | other ->
            failwith
                $"WaitHandle: thread %O{thread} sits in wait handle %O{id}'s WaitQueue but its status is %O{other}; a queued thread must be blocked on a wait handle."

    /// Every handle a registration names, without repeats. A wait-any array
    /// may name the same handle twice (legal on Win32); the thread is enqueued
    /// once per distinct handle, and the reported index comes from the
    /// registration's list rather than from queue membership.
    let private registeredHandles (id : WaitHandleId) (registration : WaitRegistration) : WaitHandleId list =
        match registration with
        | WaitRegistration.Single handle -> [ handle ]
        | WaitRegistration.Multiple (handles, _) ->
            ignore id
            List.distinct handles

    /// The handles a grant must actually consume. A wait-all consumes all of
    /// them atomically; a wait-any consumes only the one that satisfied it.
    let private handlesToConsume (id : WaitHandleId) (registration : WaitRegistration) : WaitHandleId list =
        match registration with
        | WaitRegistration.Single handle -> [ handle ]
        | WaitRegistration.Multiple (handles, true) -> List.distinct handles
        | WaitRegistration.Multiple (_, false) -> [ id ]

    /// Whether granting `id` to `thread` would satisfy its whole wait.
    ///
    /// Single-handle and wait-any registrations are satisfied by definition
    /// once `id` itself is acquirable. A wait-all additionally requires every
    /// *other* named handle to be acquirable at this instant — the PAL's
    /// `IsRestOfWaitAllSatisfied`.
    let private waitIsSatisfied
        (thread : ThreadId)
        (id : WaitHandleId)
        (registration : WaitRegistration)
        (state : IlMachineState)
        : bool
        =
        match registration with
        | WaitRegistration.Single _
        | WaitRegistration.Multiple (_, false) -> true
        | WaitRegistration.Multiple (handles, true) ->
            handles
            |> List.forall (fun handle -> handle = id || isAcquirable thread (lookup handle state))

    /// Rewrite the top of `thread`'s eval stack — the optimistic
    /// `WAIT_OBJECT_0` its park-time push installed — to `value`.
    ///
    /// Pop-then-push rather than peek-and-mutate because the eval stack
    /// surface only exposes those primitives; the net stack depth is
    /// unchanged, just the top value. Shared by the multi-wait signal wake and
    /// by every timeout fire.
    let private rewriteWaitResult (thread : ThreadId) (value : int) (state : IlMachineState) : IlMachineState =
        let _, state = IlMachineState.popEvalStack thread state
        IlMachineState.pushToEvalStack' (EvalStackValue.Int32 value) thread state

    /// Hand `id` (and, for a wait-all, every other handle it names) to
    /// `thread`, dequeue it from every handle it was registered on, publish
    /// the value the guest will see, and flip it back to `Runnable`.
    ///
    /// Precondition: `waitIsSatisfied` held for this thread against the
    /// current state.
    let private grantTo (thread : ThreadId) (id : WaitHandleId) (state : IlMachineState) : IlMachineState =
        let registration = registrationOf thread id state

        let state, abandoned =
            handlesToConsume id registration
            |> List.fold
                (fun (state, abandoned) handle ->
                    let acquired, wasAbandoned = acquireFor thread (lookup handle state)
                    writeHandle handle acquired state, abandoned || wasAbandoned
                )
                (state, false)

        let state =
            registeredHandles id registration
            |> List.fold
                (fun state handle ->
                    let current = lookup handle state

                    let dequeued = waitQueueOf current |> List.filter (fun queued -> queued <> thread)

                    writeHandle handle (withWaitQueue dequeued current) state
                )
                state

        // A single-handle waiter's `WAIT_OBJECT_0` was already pushed at park
        // time and is still correct, so its slot is left untouched (this is
        // what keeps single-handle wake behaviour byte-identical). A
        // multi-handle waiter's result is only known now: wait-any reports the
        // index of the handle that satisfied it, wait-all reports none because
        // the OS cannot say which of them was abandoned.
        let state =
            match registration with
            | WaitRegistration.Single _ -> state
            | WaitRegistration.Multiple (handles, waitAll) ->
                let baseCode = if abandoned then waitAbandoned else waitObjectZero

                let result =
                    if waitAll then
                        baseCode
                    else
                        match List.tryFindIndex (fun handle -> handle = id) handles with
                        | Some index -> baseCode + index
                        | None ->
                            failwith
                                $"WaitHandle.grantTo: thread %O{thread} was granted handle %O{id}, which its registration %A{handles} does not name."

                rewriteWaitResult thread result state

        Scheduler.setThreadStatus thread ThreadStatus.Runnable state

    /// Hand `id` to the head-most waiter its current state can satisfy, if
    /// any. Returns `None` — leaving the state untouched — when no queued
    /// waiter can be satisfied.
    ///
    /// Walk-and-skip, not pop-the-head: a wait-all waiter that is not
    /// currently satisfiable is passed over and a later waiter may be granted
    /// instead, which is what the PAL's waiting-thread walk does
    /// (`synchcontrollers.cpp` consults `IsRestOfWaitAllSatisfied` and
    /// `continue`s past a node it cannot satisfy, leaving it registered).
    /// Insertion-side ordering still carries the fairness discipline — `waitOne`
    /// appends to the tail for FIFO, `waitOnePrioritized` prepends for LIFO —
    /// and this walk preserves it among satisfiable entries.
    ///
    /// `id` not being acquirable at all short-circuits the walk: no waiter can
    /// be satisfied by a handle that has nothing to give.
    let private tryGrantOne (id : WaitHandleId) (state : IlMachineState) : IlMachineState option =
        let handle = lookup id state

        let candidate =
            waitQueueOf handle
            |> List.tryFind (fun thread ->
                isAcquirable thread handle
                && waitIsSatisfied thread id (registrationOf thread id state) state
            )

        candidate |> Option.map (fun thread -> grantTo thread id state)

    /// Repeatedly grant `id` until no queued waiter can be satisfied.
    ///
    /// Every grant consumes something from `id` (a semaphore unit, the mutex's
    /// ownership, an auto-event's signal) or dequeues a waiter from a manual
    /// event, so the loop always makes progress and terminates.
    let private drainGrants (id : WaitHandleId) (state : IlMachineState) : IlMachineState =
        let rec loop (state : IlMachineState) : IlMachineState =
            match tryGrantOne id state with
            | None -> state
            | Some state -> loop state

        loop state

    /// Internal: kind-private semaphore waitOne. Used by the kind
    /// dispatcher in `waitOne` and the property tests that exercise the
    /// semaphore path directly. `deadlineMs` is the absolute virtual-clock
    /// millisecond at which the wait expires (or `None` for an infinite
    /// wait); the value is recorded on the parked thread's `BlockedOn
    /// WaitHandle` status when the slow path fires, so the driver loop's
    /// deadline-firing pass knows when to time the wait out.
    let private waitOneSemaphore
        (thread : ThreadId)
        (id : WaitHandleId)
        (deadlineMs : int64 option)
        (semaphore : SemaphoreState)
        (state : IlMachineState)
        : WaitOutcome
        =
        if semaphore.Count > 0 then
            // Fast path: consume one unit and stay Runnable. The new
            // count is in `[0, Maximum - 1]`, preserving the invariant.
            // The deadline is irrelevant on the fast path; the wait
            // never blocked, so there is no parked-thread record to
            // attach it to.
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
            |> Scheduler.setThreadStatus thread (ThreadStatus.BlockedOnWaitHandle (id, deadlineMs))
            |> WaitOutcome.Blocked

    /// Internal: kind-private mutex waitOne. Re-entrant on owner; the
    /// `Free wasAbandoned=true` transition produces `AcquiredAbandoned`
    /// and clears the flag. `deadlineMs` is recorded on the parked
    /// thread's status when the slow path fires (held by another thread);
    /// the fast paths do not consult it.
    let private waitOneMutex
        (thread : ThreadId)
        (id : WaitHandleId)
        (deadlineMs : int64 option)
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
            |> Scheduler.setThreadStatus thread (ThreadStatus.BlockedOnWaitHandle (id, deadlineMs))
            |> WaitOutcome.Blocked

    /// Internal: kind-private event waitOne. Acquiring a signalled `Auto`
    /// event consumes the signal (clears `Signaled`); acquiring a
    /// signalled `Manual` event leaves it signalled (every concurrent
    /// waiter passes through). On an unsignalled event the thread parks
    /// at the FIFO tail with `deadlineMs` recorded on its status.
    let private waitOneEvent
        (thread : ThreadId)
        (id : WaitHandleId)
        (deadlineMs : int64 option)
        (event : EventState)
        (state : IlMachineState)
        : WaitOutcome
        =
        if event.Signaled then
            let event =
                match event.Mode with
                | EventResetMode.Auto ->
                    { event with
                        Signaled = false
                    }
                | EventResetMode.Manual -> event

            state |> writeHandle id (WaitHandleState.Event event) |> WaitOutcome.Acquired
        else
            // Slow path: park at the tail of the FIFO wait queue.
            // `setEvent` will wake (all parked waiters for Manual, the
            // FIFO head for Auto); a guest can never observe Signaled =
            // true while there is a parked waiter by invariant.
            let event =
                { event with
                    WaitQueue = event.WaitQueue @ [ thread ]
                }

            state
            |> writeHandle id (WaitHandleState.Event event)
            |> Scheduler.setThreadStatus thread (ThreadStatus.BlockedOnWaitHandle (id, deadlineMs))
            |> WaitOutcome.Blocked

    /// Try to take ownership of `id` on behalf of `thread`. Dispatches
    /// by kind:
    ///
    ///  - Semaphore: decrement the count if positive; otherwise park at
    ///    the FIFO tail of `WaitQueue` and flip the thread's status to
    ///    `BlockedOnWaitHandle (id, deadlineMs)`.
    ///  - Mutex: re-entrant fast path on the owning thread; take the
    ///    free mutex (producing `AcquiredAbandoned` iff the abandoned
    ///    flag was set, clearing it); otherwise park at the FIFO tail.
    ///  - Event: signalled events fast-path (consuming the signal for
    ///    `Auto`); unsignalled events park at the FIFO tail.
    ///
    /// `deadlineMs` is the absolute virtual-clock millisecond at which a
    /// finite timeout expires, or `None` for an infinite wait. The fast
    /// paths ignore it; only the slow paths thread it through to the
    /// parked thread's status, where the driver loop's deadline-firing
    /// pass picks it up.
    ///
    /// The IL `WaitOne` site advances in every case (the native handler
    /// returns `Stepped/Executed` for all three outcomes). When a parked
    /// thread is later signal-woken, its `WAIT_OBJECT_0` slot has
    /// already been pushed onto its eval stack at park time; if instead
    /// its deadline fires first, `fireTimeout` rewrites that slot to
    /// `WAIT_TIMEOUT` — see the abandoned-mutex-propagation note on the
    /// module docstring for why signal-side rewrite isn't symmetric.
    let waitOne
        (thread : ThreadId)
        (id : WaitHandleId)
        (deadlineMs : int64 option)
        (state : IlMachineState)
        : WaitOutcome
        =
        let handle = lookup id state

        match handle with
        | WaitHandleState.Semaphore semaphore -> waitOneSemaphore thread id deadlineMs semaphore state
        | WaitHandleState.Mutex mutex -> waitOneMutex thread id deadlineMs mutex state
        | WaitHandleState.Event event -> waitOneEvent thread id deadlineMs event state

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
        | WaitHandleState.Event event ->
            if event.Signaled then
                let event =
                    match event.Mode with
                    | EventResetMode.Auto ->
                        { event with
                            Signaled = false
                        }
                    | EventResetMode.Manual -> event

                state |> writeHandle id (WaitHandleState.Event event) |> TryWaitOutcome.Acquired
            else
                // No enqueue, no status flip.
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
    /// when the call has to block. `deadlineMs` propagates to the parked
    /// thread's status when the slow path fires, and is ignored on the
    /// fast path for the same reason as `waitOne`.
    let waitOnePrioritized
        (thread : ThreadId)
        (id : WaitHandleId)
        (deadlineMs : int64 option)
        (state : IlMachineState)
        : WaitOutcome
        =
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
            |> Scheduler.setThreadStatus thread (ThreadStatus.BlockedOnWaitHandle (id, deadlineMs))
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
            // Publish the new units, then hand them out. Expressing the
            // release this way — make the resource available, then drain the
            // queue — rather than as "pop N heads and skip Count entirely"
            // keeps direct handoff intact (a granted unit never lingers in
            // `Count` where a fresh waiter could steal it, because nothing
            // else runs between the two steps) while giving multi-handle
            // waiters a single place to be considered.
            //
            // Ordering discipline still lives at insertion time (`waitOne`
            // appends to the tail for FIFO; `waitOnePrioritized` prepends to
            // the head for LIFO over prioritized waiters, matching
            // `PAL_WaitForSingleObjectPrioritized`). The
            // LowLevelLifoSemaphore fairness contract that PortableThreadPool
            // depends on is delivered through that insertion-side asymmetry,
            // which `tryGrantOne`'s head-to-tail walk preserves.
            //
            // The add is safe: the guard above ensures `previousCount +
            // releaseCount ≤ Maximum`, so the int32 add cannot overflow.
            let semaphore =
                { semaphore with
                    Count = previousCount + releaseCount
                }

            let state =
                state |> writeHandle id (WaitHandleState.Semaphore semaphore) |> drainGrants id

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
            // Outermost release: mark free, then hand ownership to the
            // head-most satisfiable waiter. Absent multi-handle waiters this
            // is exactly the previous direct-handoff behaviour — the mutex is
            // only observably `Free` with a non-empty queue when every queued
            // entry is a wait-all waiter blocked on some other handle, which
            // is the case that has no waiter to hand ownership to.
            let mutex =
                { mutex with
                    Ownership = MutexOwnership.Free false
                }

            let state = state |> writeHandle id (WaitHandleState.Mutex mutex) |> drainGrants id

            Ok (), state

    /// Signal an event, then hand the signal out. Both modes set `Signaled =
    /// true` first and then drain the wait queue; the modes differ only in
    /// what acquiring costs, which `acquireFor` already encodes:
    ///
    ///  - `Manual`: acquiring does not consume the signal, so the drain wakes
    ///    every satisfiable waiter and the event stays signalled — further
    ///    waiters arriving before a `ResetEvent` pass straight through.
    ///  - `Auto`: acquiring consumes the signal, so the drain grants at most
    ///    one waiter and leaves `Signaled = false`. With no satisfiable waiter
    ///    the signal stays latched for the next `WaitOne`.
    ///
    /// Absent multi-handle waiters this is exactly the previous
    /// wake-all/direct-handoff behaviour. With them, a wait-all waiter blocked
    /// on some other handle is skipped and stays queued, so `Signaled = true`
    /// may now coexist with a non-empty queue — see the invariant note on
    /// `EventState`.
    ///
    /// Idempotent on already-signalled events. Never fails — the Win32
    /// `SetEvent` only returns FALSE on an invalid handle, which is
    /// caught by `expectEvent`.
    let setEvent (id : WaitHandleId) (state : IlMachineState) : IlMachineState =
        let handle = lookup id state
        let event = expectEvent "WaitHandle.setEvent" id handle

        let event =
            { event with
                Signaled = true
            }

        state |> writeHandle id (WaitHandleState.Event event) |> drainGrants id

    /// Clear the signalled flag. By invariant `WaitQueue` is empty if
    /// `Signaled` was true, so there is nothing to do beyond flipping the
    /// flag; if `Signaled` was already false `WaitQueue` may be non-empty
    /// but the waiters are untouched. Idempotent.
    let resetEvent (id : WaitHandleId) (state : IlMachineState) : IlMachineState =
        let handle = lookup id state
        let event = expectEvent "WaitHandle.resetEvent" id handle

        let event =
            { event with
                Signaled = false
            }

        state |> writeHandle id (WaitHandleState.Event event)

    /// Outcome of a multi-handle wait.
    ///
    /// `Acquired` carries the index (within the *caller's* handle array, so
    /// duplicates resolve to the first occurrence — Win32's "smallest index of
    /// all signalled objects") of the handle that satisfied a wait-any, and
    /// whether the acquisition observed an abandoned mutex. For a wait-all the
    /// index is meaningless and the caller must ignore it: the PAL returns a
    /// bare `WAIT_OBJECT_0` / `WAIT_ABANDONED_0` because the OS cannot say
    /// which of the mutexes was abandoned.
    ///
    /// `Blocked` means the thread was parked at every named handle's queue.
    ///
    /// `Failed` is the wait-all-with-duplicate-handles case. It is reachable
    /// from ordinary guest code (`WaitHandle.WaitAll(new[]{ e, e })` compiles
    /// and the BCL does not dedupe), so it is an outcome rather than a loud
    /// failure — but it is *not* a return value: the PAL answers
    /// `WAIT_FAILED` + `ERROR_INVALID_PARAMETER` and then
    /// `Thread::DoAppropriateWait`, still inside the QCall, converts that into
    /// a managed `DuplicateWaitObjectException`. The native handler raises
    /// that; nothing ever hands `WAIT_FAILED` to the managed wrapper, which
    /// would silently read as success.
    [<RequireQualifiedAccess>]
    type MultiWaitOutcome =
        | Acquired of index : int * abandoned : bool * IlMachineState
        | Blocked of IlMachineState
        | Failed of IlMachineState

    /// Outcome of a non-blocking multi-handle probe (the zero-timeout path).
    /// `TimedOut` leaves the state untouched: no enqueue, no status flip.
    [<RequireQualifiedAccess>]
    type MultiTryWaitOutcome =
        | Acquired of index : int * abandoned : bool * IlMachineState
        | TimedOut of IlMachineState
        | Failed of IlMachineState

    /// The PAL rejects a wait-all naming the same object twice
    /// (`wait.cpp`'s brute-force duplicate scan sets `ERROR_INVALID_PARAMETER`
    /// and returns `WAIT_FAILED`). Duplicates are legal for a wait-any.
    let private hasIllegalDuplicates (handles : WaitHandleId list) (waitAll : bool) : bool =
        waitAll && List.length (List.distinct handles) <> List.length handles

    /// Shared front half of both multi-wait entry points: validate, then try
    /// to satisfy the wait without blocking. Returns the outcome plus, when it
    /// could not be satisfied, `None` so the caller decides between parking
    /// and timing out.
    ///
    /// Wait-any scans in index order and stops at the first acquirable handle,
    /// so the reported index is the smallest signalled one. Wait-all requires
    /// every handle to be acquirable *before* consuming any of them, which is
    /// what makes the acquisition atomic: a partially-satisfiable wait-all
    /// leaves every handle untouched.
    let private tryAcquireMultiple
        (thread : ThreadId)
        (handles : WaitHandleId list)
        (waitAll : bool)
        (state : IlMachineState)
        : (int * bool * IlMachineState) option
        =
        // Touch every handle so a stale or never-created one fails loud here
        // rather than at some later, more confusing point.
        let resolved = handles |> List.map (fun handle -> handle, lookup handle state)

        if waitAll then
            if resolved |> List.forall (fun (_, handle) -> isAcquirable thread handle) then
                let state, abandoned =
                    handles
                    |> List.distinct
                    |> List.fold
                        (fun (state, abandoned) handle ->
                            let acquired, wasAbandoned = acquireFor thread (lookup handle state)
                            writeHandle handle acquired state, abandoned || wasAbandoned
                        )
                        (state, false)

                Some (0, abandoned, state)
            else
                None
        else
            match resolved |> List.tryFindIndex (fun (_, handle) -> isAcquirable thread handle) with
            | None -> None
            | Some index ->
                let handleId = fst resolved.[index]
                let acquired, abandoned = acquireFor thread (lookup handleId state)
                Some (index, abandoned, writeHandle handleId acquired state)

    /// Non-blocking multi-handle probe, modelling the zero-timeout
    /// `WaitAny(…, 0)` / `WaitAll(…, 0)` path. The caller is never enqueued:
    /// leaving the thread `Runnable` is the entire observable difference from
    /// `waitMultiple`, and silently parking would deadlock guests that use a
    /// zero timeout as a poll.
    let tryWaitMultiple
        (thread : ThreadId)
        (handles : WaitHandleId list)
        (waitAll : bool)
        (state : IlMachineState)
        : MultiTryWaitOutcome
        =
        if hasIllegalDuplicates handles waitAll then
            MultiTryWaitOutcome.Failed state
        else

        match tryAcquireMultiple thread handles waitAll state with
        | Some (index, abandoned, state) -> MultiTryWaitOutcome.Acquired (index, abandoned, state)
        | None -> MultiTryWaitOutcome.TimedOut state

    /// Blocking multi-handle wait. On the fast path the wait is satisfied
    /// inline and the thread stays `Runnable`; otherwise it is appended to the
    /// tail of every *distinct* named handle's queue and its status becomes
    /// `BlockedOnWaitHandles`.
    ///
    /// The thread is enqueued once per distinct handle even when a wait-any
    /// names one twice: queue membership only decides *whether* it can be
    /// woken, while the index it reports comes from the registration's list.
    /// Enqueuing twice would let one signal wake it and leave a stale entry
    /// behind.
    ///
    /// `deadlineMs` is the absolute virtual-clock millisecond at which a
    /// finite timeout expires, or `None` for `INFINITE`. As on the
    /// single-handle path the IL call site advances in every case, and the
    /// slow path pushes an optimistic `WAIT_OBJECT_0` that the eventual wake
    /// (`grantTo`) or timeout (`fireMultipleTimeout`) rewrites.
    let waitMultiple
        (thread : ThreadId)
        (handles : WaitHandleId list)
        (waitAll : bool)
        (deadlineMs : int64 option)
        (state : IlMachineState)
        : MultiWaitOutcome
        =
        if hasIllegalDuplicates handles waitAll then
            MultiWaitOutcome.Failed state
        else

        match tryAcquireMultiple thread handles waitAll state with
        | Some (index, abandoned, state) -> MultiWaitOutcome.Acquired (index, abandoned, state)
        | None ->
            let state =
                handles
                |> List.distinct
                |> List.fold
                    (fun state handleId ->
                        let handle = lookup handleId state

                        writeHandle handleId (withWaitQueue (waitQueueOf handle @ [ thread ]) handle) state
                    )
                    state

            state
            |> Scheduler.setThreadStatus thread (ThreadStatus.BlockedOnWaitHandles (handles, waitAll, deadlineMs))
            |> MultiWaitOutcome.Blocked

    /// Wake `thread` because its finite-timeout multi-handle wait has expired
    /// against the virtual clock. Dequeues it from *every* handle it was
    /// registered on — the whole registration expires together, not just the
    /// queue that happened to notice — rewrites the park-time
    /// `WAIT_OBJECT_0` slot to `WAIT_TIMEOUT`, and flips the thread back to
    /// `Runnable`.
    ///
    /// The driver only calls this for threads it observed in
    /// `BlockedOnWaitHandles (_, _, Some _)`, so a thread missing from one of
    /// its own queues indicates a scheduler-side bug and fails loud.
    let fireMultipleTimeout (thread : ThreadId) (state : IlMachineState) : IlMachineState =
        let handles =
            match state.ThreadState.[thread].Status with
            | ThreadStatus.BlockedOnWaitHandles (handles, _, _) -> List.distinct handles
            | other ->
                failwith
                    $"WaitHandle.fireMultipleTimeout: thread %O{thread} has status %O{other}, not BlockedOnWaitHandles; the driver fires this only for multi-handle waiters."

        let state =
            handles
            |> List.fold
                (fun state handleId ->
                    let handle = lookup handleId state
                    let queue = waitQueueOf handle
                    let dequeued = queue |> List.filter (fun queued -> queued <> thread)

                    if List.length dequeued = List.length queue then
                        failwith
                            $"WaitHandle.fireMultipleTimeout: thread %O{thread} is not in wait handle %O{handleId}'s wait queue; the scheduler observed a deadline on a thread the handle does not know about."

                    writeHandle handleId (withWaitQueue dequeued handle) state
                )
                state

        state
        |> rewriteWaitResult thread waitTimeout
        |> Scheduler.setThreadStatus thread ThreadStatus.Runnable

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
                    $"WaitHandle %O{id}: refusing to Close a semaphore with %d{List.length waiters} thread(s) parked in its wait queue (%A{waiters}); the guest must Release before Disposing."
        | WaitHandleState.Mutex mutex ->
            match mutex.WaitQueue with
            | [] -> ()
            | waiters ->
                failwith
                    $"WaitHandle %O{id}: refusing to Close a mutex with %d{List.length waiters} thread(s) parked in its wait queue (%A{waiters}); the guest must Release before Disposing."

            match mutex.Ownership with
            | MutexOwnership.Free _ -> ()
            | MutexOwnership.Held (owner, recursionCount) ->
                failwith
                    $"WaitHandle %O{id}: refusing to Close a mutex still held by thread %O{owner} (recursion count = %d{recursionCount}); the guest must ReleaseMutex before Disposing."
        | WaitHandleState.Event event ->
            match event.WaitQueue with
            | [] -> ()
            | waiters ->
                failwith
                    $"WaitHandle %O{id}: refusing to Close an event with %d{List.length waiters} thread(s) parked in its wait queue (%A{waiters}); the guest must Set/Reset and let waiters drain before Disposing."

        state.MapKernel (fun kernel ->
            { kernel with
                WaitHandles = kernel.WaitHandles |> Map.remove id
            }
        )

    /// Wake `thread` because its finite-timeout wait against handle `id`
    /// has expired against the virtual clock. The thread must currently
    /// be parked at this handle's wait queue (the driver only calls
    /// this for threads observed in `BlockedOnWaitHandle (id, Some _)`,
    /// so a mismatch here indicates a scheduler-side bug). Effects:
    ///
    ///  - Remove `thread` from whichever kind-specific `WaitQueue` it
    ///    sits in. A semaphore's queue is FIFO; a prioritized waiter
    ///    sits at the head. Either way the entry's position is opaque
    ///    to the timeout — we filter by identity.
    ///  - Rewrite the top of the thread's active frame's eval stack from
    ///    the `WAIT_OBJECT_0` slot the slow-path push installed at park
    ///    time to `WAIT_TIMEOUT (0x102)`. The IL `WaitOne` call site has
    ///    already advanced past itself; the BCL's wrapper reads this
    ///    value back from the QCall return and compares against
    ///    `WaitHandle.WaitTimeout = 0x102` to return `false`.
    ///  - Flip the thread's status back to `Runnable`. The deadline is
    ///    implicitly forgotten — the new status carries no deadline
    ///    field, which is exactly the invariant the variant encodes.
    ///
    /// Why pop-then-push rather than peek-and-mutate: the eval stack
    /// surface only exposes push/pop primitives, and popping the
    /// previously-pushed slot before pushing the timeout slot keeps the
    /// stack depth invariant across the wake (no net change in depth,
    /// just the top value).
    let fireTimeout (thread : ThreadId) (id : WaitHandleId) (state : IlMachineState) : IlMachineState =
        let handle = lookup id state

        let state =
            match handle with
            | WaitHandleState.Semaphore semaphore ->
                let newQueue = semaphore.WaitQueue |> List.filter (fun t -> t <> thread)

                if List.length newQueue = List.length semaphore.WaitQueue then
                    failwith
                        $"WaitHandle.fireTimeout: thread %O{thread} is not in semaphore %O{id}'s wait queue; the scheduler observed a deadline on a thread the handle does not know about."

                writeHandle
                    id
                    (WaitHandleState.Semaphore
                        { semaphore with
                            WaitQueue = newQueue
                        })
                    state
            | WaitHandleState.Mutex mutex ->
                let newQueue = mutex.WaitQueue |> List.filter (fun t -> t <> thread)

                if List.length newQueue = List.length mutex.WaitQueue then
                    failwith
                        $"WaitHandle.fireTimeout: thread %O{thread} is not in mutex %O{id}'s wait queue; the scheduler observed a deadline on a thread the handle does not know about."

                writeHandle
                    id
                    (WaitHandleState.Mutex
                        { mutex with
                            WaitQueue = newQueue
                        })
                    state
            | WaitHandleState.Event event ->
                let newQueue = event.WaitQueue |> List.filter (fun t -> t <> thread)

                if List.length newQueue = List.length event.WaitQueue then
                    failwith
                        $"WaitHandle.fireTimeout: thread %O{thread} is not in event %O{id}'s wait queue; the scheduler observed a deadline on a thread the handle does not know about."

                writeHandle
                    id
                    (WaitHandleState.Event
                        { event with
                            WaitQueue = newQueue
                        })
                    state

        // Rewrite the park-time `WAIT_OBJECT_0` slot to `WAIT_TIMEOUT`. The
        // slow-path waitOne / waitOnePrioritized pushed `WAIT_OBJECT_0`
        // because the slow path could not know in advance whether the wake
        // would be a signal (keep the value) or a timeout (rewrite); choosing
        // the optimistic value at park time makes signal-wake free.
        state
        |> rewriteWaitResult thread waitTimeout
        |> Scheduler.setThreadStatus thread ThreadStatus.Runnable

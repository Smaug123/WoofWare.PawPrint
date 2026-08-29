namespace WoofWare.PawPrint

open System.Collections.Immutable
open WoofWare.PosixKernel

/// Deterministic model of a single `System.Threading.LowLevelMonitor`, as
/// minted by `SystemNative_LowLevelMonitor_Create`. CoreCLR backs this with a
/// `pthread_mutex_t` + `pthread_cond_t` pair on Unix; PawPrint reproduces the
/// observable semantics through three pieces of state owned by the kernel's
/// `LowLevelMonitors` registry:
///
///   - `Owner` is `Some t` iff thread `t` currently holds the monitor;
///     mutual exclusion is just the invariant "at most one thread is the
///     `Owner` at any time."
///   - `AcquireQueue` is the FIFO list of threads parked in
///     `BlockedOnMonitorAcquire`. The head is the next thread that will
///     receive ownership when the current owner releases. FIFO order is
///     required for `LowLevelLock` fairness; switching to LIFO or
///     arbitrary order would change the program's observable interleaving.
///   - `WaitQueue` is the FIFO list of threads parked in
///     `BlockedOnMonitorWait`. `Signal_Release` moves the head of this queue
///     onto the tail of `AcquireQueue` (it must re-contend for the monitor
///     before its `Wait` call returns), atomically with releasing the owner.
///
/// The monitor is non-reentrant, matching CoreCLR's `LowLevelMonitor` — a
/// thread that holds the monitor and calls `Acquire` again deadlocks.
/// Reentrancy is supplied at a higher level by `LowLevelLock`.
///
/// Spurious wakeups are injected externally according to
/// `EmulatedKernel.SpuriousWakeup` — the model exposes a transition
/// (`LowLevelMonitor.spuriousWake`) that moves a thread from `WaitQueue`
/// through the same reacquire path that `Signal_Release` uses. Any guest
/// code that depends on the absence of spurious wakeups is incorrect on
/// real CoreCLR; the `SpuriousWakeup` field is the deterministic knob
/// for exposing those bugs.
type LowLevelMonitorState =
    {
        Owner : ThreadId option
        AcquireQueue : ThreadId list
        WaitQueue : ThreadId list
    }

[<RequireQualifiedAccess>]
module LowLevelMonitorState =
    let empty : LowLevelMonitorState =
        {
            Owner = None
            AcquireQueue = []
            WaitQueue = []
        }

/// Deterministic policy the driver consults each scheduler step to
/// decide whether to inject spurious wakeups for threads parked in
/// `BlockedOnMonitorWait`. Real CoreCLR (via pthread `cond_wait`) is
/// allowed to return from `Wait` without a matching `Signal_Release`,
/// which is why guest code is required to wrap `Wait` in a predicate
/// loop. This type drives that contract: a strategy other than
/// `Disabled` causes PawPrint to wake waiters according to a
/// deterministic recipe so guest predicate-loop bugs surface as failing
/// runs rather than latent races.
///
/// The strategy is data, not a closure, so it can be printed, diffed,
/// and replayed across runs. Each variant is independently
/// deterministic given the current `EmulatedKernel.StepCounter`.
[<RequireQualifiedAccess>]
type SpuriousWakeupStrategy =
    /// Default. Only `Signal_Release` wakes a waiter. Equivalent to a
    /// pthread implementation that never returns from `cond_wait`
    /// spuriously: permitted by POSIX but masks predicate-loop bugs.
    | Disabled
    /// Wake every waiter on every scheduler tick. Maximum fuzz
    /// pressure. Guest code that uses `Monitor.Wait` outside a
    /// re-checking predicate loop will produce wrong results — that is
    /// the point.
    | AlwaysAll
    /// Each (tick, monitor, waiter) tuple wakes independently with the
    /// given probability. The coin flip is a deterministic function of
    /// `(seed, tick, monitorId, threadId)`, so the same seed reproduces
    /// the same wakeup sequence across runs. `probability` is rejected
    /// at apply time if it is NaN or outside `[0.0, 1.0]` — values
    /// outside that range are a programmer error and fail loud.
    | Random of seed : uint64 * probability : float
    /// Explicit `(tick, monitorId, threadId)` triples. Fully
    /// replayable. A triple naming a thread that is not in the named
    /// monitor's `WaitQueue` at the named tick fails loudly — silent
    /// skip would let scripts drift unnoticed when the interleaving
    /// underneath them changes.
    | Scripted of wakeups : (int64 * LowLevelMonitorId * ThreadId) list

/// Parallel to `SpuriousWakeupStrategy`, but governs spurious wakeups out of
/// the managed `Monitor.Wait` (i.e. the SyncBlock-backed condition variable)
/// rather than the `LowLevelMonitor` primitive. CoreCLR's `Monitor.Wait` is
/// permitted to return without a matching `Pulse` / `PulseAll`, which is why
/// the documented usage pattern always wraps `Wait` in a predicate loop.
/// This type is the deterministic knob for forcing those wakeups in PawPrint.
///
/// Parallel but separate from `SpuriousWakeupStrategy`, so the
/// LowLevelMonitor-level and SyncBlock-level fuzz schedules are
/// independent dials.
[<RequireQualifiedAccess>]
type SyncBlockSpuriousWakeupStrategy =
    /// Default. Only `Pulse` / `PulseAll` wakes a waiter. Equivalent to a
    /// `SyncBlock` implementation that never wakes spuriously: permitted by
    /// the BCL contract but masks predicate-loop bugs.
    | Disabled
    /// Wake every waiter on every scheduler tick. Maximum fuzz pressure.
    /// Guest code that uses `Monitor.Wait` outside a re-checking predicate
    /// loop will produce wrong results — that is the point.
    | AlwaysAll
    /// Each (tick, lockObject, waiter) tuple wakes independently with the
    /// given probability. Coin flip is deterministic over
    /// `(seed, tick, lockObject, threadId)`. `probability` is rejected at
    /// apply time if NaN or outside `[0.0, 1.0]`.
    | Random of seed : uint64 * probability : float
    /// Explicit `(tick, lockObject, threadId)` triples. Fully replayable.
    /// A triple naming a thread not currently in the named SyncBlock's
    /// `WaitQueue` at the named tick fails loudly — silent skip would let
    /// scripts drift unnoticed when the interleaving underneath them
    /// changes.
    | Scripted of wakeups : (int64 * ManagedHeapAddress * ThreadId) list

/// Deterministic strategy governing *clock jitter*: whether the driver, on a
/// given scheduler tick, jumps the virtual clock forward onto a deadline some
/// thread is already waiting on.
///
/// Without it the virtual clock is `StepCounter * InstructionCostTicks`, so a
/// finite wait always expires after the same number of retired instructions and
/// a guest's timeout path is only ever reached when the guest's *own*
/// arithmetic sends it there. Real machines are not like that: a thread
/// descheduled for a few milliseconds makes a `WaitOne(50)` time out in the
/// middle of a window its author assumed was comfortable. This type is the
/// deterministic knob for provoking that, so bugs of the form "if too much time
/// elapses between these steps, something bad happens" surface as failing runs
/// rather than latent races.
///
/// The randomised variant is anchored to an *outstanding* deadline, so each
/// jump corresponds to a wait some thread had really posted — but it may land
/// past that deadline rather than exactly on it, because a real timeout fires
/// late by however long the thread waited to be rescheduled. Landing exactly on
/// the deadline models an overshoot of zero, which is the one value a real
/// timeout essentially never has, and modelling it as always-zero hides every
/// bug where a guest budgets against a wait: "I wait at most 50 ms against a
/// 100 ms lease, so the lease survives" is sound if and only if a 50 ms wait
/// takes 50 ms. See `EagerDeadlines`'s `maxOvershootTicks`.
///
/// The strategy is data, not a closure, so it can be printed, diffed, and
/// replayed across runs. Like `SpuriousWakeupStrategy`, each variant is
/// independently deterministic given the current `EmulatedKernel.StepCounter`.
[<RequireQualifiedAccess>]
type ClockJitterStrategy =
    /// Default. The clock advances only by `InstructionCostTicks` per retired
    /// step, plus the driver's jump to the nearest deadline when nothing is
    /// Runnable. Timeouts therefore fire on schedule, which masks
    /// elapsed-time-sensitive bugs.
    | Disabled
    /// On each tick, with the given probability, jump the clock to one of the
    /// deadlines currently outstanding — chosen uniformly among *all* of them,
    /// not just the nearest, so a single jump can expire several waits at once
    /// and the orderings that produces are reachable — plus an overshoot drawn
    /// uniformly from `[0, maxOvershootTicks]`.
    ///
    /// `maxOvershootTicks = 0` fires every timeout at exactly its deadline. That
    /// is the cheapest setting and it reaches the "a timeout arm ran that never
    /// otherwise runs" bug class, but *only* that class: a guest's measured
    /// elapsed time is then exactly what it asked to wait for, so any bug that
    /// needs a wait to overrun its nominal duration stays invisible. Give it a
    /// non-zero bound to reach those; a bound of a few times the guest's
    /// timeouts is the useful range, since an overshoot far smaller than a
    /// timeout perturbs nothing a guest can measure.
    ///
    /// Uniform rather than the heavy-tailed distribution real scheduling latency
    /// follows: this is a search, not a simulation, and the large overshoots are
    /// the ones that find bugs, so they should not be rare.
    ///
    /// The coin, the choice and the overshoot are independent deterministic
    /// functions of `(seed, stepCounter)`, so the same seed reproduces the same
    /// jump sequence across runs. `probability` is rejected if it is NaN or
    /// outside `[0.0, 1.0]`; `maxOvershootTicks` if it is negative (the clock is
    /// monotonic) or above `ClockJitter.maxOvershootBoundTicks`. A bound that is
    /// legal here but still large enough to run the clock past its representable
    /// range is not rejected — that fault belongs to
    /// `UnixMachineState.withVirtualClockTicks`, which raises it naming the wait
    /// responsible.
    ///
    /// Keep the probability small. Every jump discards the interval between the
    /// clock and the deadline, so a large one runs the guest's clock off into
    /// the distance and starves the very interleavings it is trying to explore.
    | EagerDeadlines of seed : uint64 * probability : float * maxOvershootTicks : int64
    /// Explicit `(stepCounter, targetTicks)` pairs: at the named tick, set the
    /// clock to the named value. Fully replayable, and the shrinking target —
    /// record what an `EagerDeadlines` run jumped and replay it with jumps
    /// removed to find the minimal set that still reproduces a failure.
    ///
    /// Unrestricted: a target need not be a deadline any thread is waiting on.
    /// That is deliberate, and it is what keeps shrinking well-behaved — drop an
    /// early jump from a script and the later ones must still mean what they
    /// meant, which they would not if each had to remain a live deadline in a
    /// run whose earlier history just changed.
    ///
    /// Targets are absolute clock readings rather than deltas precisely so that
    /// removing one jump leaves every other jump landing on the same instant it
    /// did before; with deltas, dropping an early jump would silently retime
    /// every later one and a shrink step would change more than it removed.
    ///
    /// A pair whose target is behind the clock at the named tick fails loudly
    /// rather than being skipped, for the same reason a `SpuriousWakeupStrategy`
    /// script naming an absent waiter does: a script that has drifted out of
    /// step with the run underneath it should say so.
    | Scripted of jumps : (int64 * int64) list

/// Interpretation of `ClockJitterStrategy`: given the tick, the clock, and the
/// deadlines threads are currently parked on, decide where (if anywhere) the
/// clock jumps before this tick's deadlines are fired.
[<RequireQualifiedAccess>]
module ClockJitter =

    /// Largest `maxOvershootTicks` an `EagerDeadlines` strategy may name.
    ///
    /// `2^53 - 1`, because the overshoot draw scales a float in `[0, 1)` by the
    /// inclusive range size: above `2^53` an `int64` is no longer exactly
    /// representable as a `float`, so that size would round and whole stretches
    /// of the range would become undrawable — an overshoot dial quietly
    /// narrower than the number it was given. Rejecting is better than silently
    /// sampling a different distribution from the documented one.
    ///
    /// Far above any useful setting: `2^53` ticks is about 28 years of
    /// simulated time, against timeouts measured in milliseconds.
    [<Literal>]
    let maxOvershootBoundTicks : int64 = 9007199254740991L

    /// SplitMix64-style hash over `(seed, stepCounter, salt)`, giving a value in
    /// `[0.0, 1.0)`. Replayability comes from it being a pure hash with no
    /// mutable PRNG state, so distinct ticks never share entropy; `salt`
    /// separates the "does this tick jump" draw from the "which deadline" draw
    /// so that the two are not perfectly correlated.
    let private draw (seed : uint64) (stepCounter : int64) (salt : uint64) : float =
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
        let h = mix h (uint64 stepCounter)
        let h = mix h salt
        let h = finalise h
        // Top 53 bits as a float in [0, 1). Matches the common "uniform double
        // from uint64" recipe; precision loss in the low bits is irrelevant for
        // a fuzz threshold.
        float (h >>> 11) / float (1UL <<< 53)

    /// Reject a strategy whose numbers cannot mean anything. Called both by
    /// `EmulatedKernel.withClockJitter`, so a misconfigured host finds out
    /// before any guest code runs, and by `chooseJump`, which a kernel assembled
    /// by record-copy reaches without having passed through that setter.
    let validate (strategy : ClockJitterStrategy) : unit =
        match strategy with
        | ClockJitterStrategy.Disabled
        | ClockJitterStrategy.Scripted _ -> ()
        | ClockJitterStrategy.EagerDeadlines (_, probability, maxOvershootTicks) ->
            // NaN first: every comparison against it is false, so a NaN
            // probability would otherwise slip through as "never fires" and
            // present a run that looked jittered and was not.
            if System.Double.IsNaN probability || probability < 0.0 || probability > 1.0 then
                failwith
                    $"ClockJitterStrategy.EagerDeadlines: probability %f{probability} is outside [0.0, 1.0] (NaN or out of range)."

            if maxOvershootTicks < 0L then
                failwith
                    $"ClockJitterStrategy.EagerDeadlines: maxOvershootTicks %d{maxOvershootTicks} is negative. An overshoot moves a timeout later, never earlier; the virtual clock is monotonic."

            // See `maxOvershootBoundTicks`: past it the draw could no longer
            // cover the range it was given, so the strategy would sample a
            // narrower distribution than the one it documents.
            if maxOvershootTicks > maxOvershootBoundTicks then
                failwith
                    $"ClockJitterStrategy.EagerDeadlines: maxOvershootTicks %d{maxOvershootTicks} exceeds %d{maxOvershootBoundTicks} (2^53 - 1), beyond which the overshoot draw cannot cover its own range exactly. That is about 28 years of simulated time; a bound anywhere near it is a unit mistake."

    /// Where the clock should jump to on this tick, if anywhere. `None` means
    /// the tick's clock advance is the ordinary `InstructionCostTicks` and
    /// nothing more.
    ///
    /// A returned target is always strictly greater than `currentClock`, so a
    /// caller can hand it straight to `UnixMachineState.withVirtualClockTicks`.
    /// `pendingDeadlines` may contain duplicates and need not be sorted: the
    /// answer depends only on the *set* of deadlines strictly ahead of the
    /// clock, so a caller need not enumerate threads in any particular order.
    ///
    /// Under `EagerDeadlines` the target lies in `[d, d + maxOvershootTicks]`
    /// for some `d` in that set — it need not be a deadline itself, and with a
    /// non-zero overshoot it usually is not.
    let chooseJump
        (strategy : ClockJitterStrategy)
        (stepCounter : int64)
        (currentClock : int64)
        (pendingDeadlines : int64 list)
        : int64 option
        =
        match strategy with
        | ClockJitterStrategy.Disabled -> None

        | ClockJitterStrategy.EagerDeadlines (seed, probability, maxOvershootTicks) ->
            validate strategy

            // Deadlines at or behind the clock are about to be fired by this
            // tick's ordinary expiry pass, so "jumping" to one would move the
            // clock nowhere. Dropping them here is what lets the caller treat a
            // `Some` as a genuine advance.
            //
            // Sorted and deduplicated so that the draw indexes a canonical
            // sequence: two threads parked on the same instant must not make
            // that instant twice as likely as any other, and the selection must
            // not depend on the order threads happen to be enumerated in.
            let candidates =
                pendingDeadlines
                |> List.filter (fun deadline -> deadline > currentClock)
                |> List.distinct
                |> List.sort

            match candidates with
            | [] -> None
            | _ ->

            if draw seed stepCounter 0UL >= probability then
                None
            else
                let index = int (draw seed stepCounter 1UL * float candidates.Length)
                // `draw` is in [0, 1), so the product is below `Length` and this
                // clamp cannot fire in exact arithmetic. It is here because the
                // product is a float: a value a hair below 1.0 times a large
                // length can round up to exactly `Length`.
                let deadline = candidates.[min index (candidates.Length - 1)]

                // Inclusive of `maxOvershootTicks`, hence the `+ 1L`: a bound
                // the draw could never actually reach would make
                // `maxOvershootTicks = 1` mean "always zero". Clamped for the
                // same float-rounding reason as the index above.
                let overshoot =
                    int64 (draw seed stepCounter 2UL * float (maxOvershootTicks + 1L))
                    |> min maxOvershootTicks

                // The deadlines are whatever the caller passed, so a deadline
                // close enough to `Int64.MaxValue` to make this sum wrap is
                // reachable from outside this module. Wrapping would return a
                // *negative* target, breaking the guarantee this function's
                // callers rely on — that a `Some` is strictly ahead of the
                // clock — and would surface downstream as a baffling
                // "clock cannot be negative" rather than as the arithmetic
                // problem it is. An over-horizon-but-representable target is
                // deliberately still returned, for `withVirtualClockTicks` to
                // diagnose naming the wait responsible.
                if deadline > System.Int64.MaxValue - overshoot then
                    failwith
                        $"ClockJitterStrategy.EagerDeadlines: deadline %d{deadline} plus an overshoot of %d{overshoot} ticks is not representable as an int64. A deadline that large cannot have come from a wait this kernel posted."

                Some (deadline + overshoot)

        | ClockJitterStrategy.Scripted jumps ->
            match jumps |> List.filter (fun (tick, _) -> tick = stepCounter) with
            | [] -> None
            | scheduled ->

            // Every target scheduled for this tick is checked, not only the one
            // that ends up winning: a script naming both 500 and 700 while the
            // clock reads 600 has drifted, and validating the maximum alone
            // would wave that through — which is exactly the silent
            // "replay that no longer describes the run" this check exists to
            // prevent.
            for _, target in scheduled do
                if target <= currentClock then
                    failwith
                        $"ClockJitterStrategy.Scripted: jump at step %d{stepCounter} names target %d{target} ticks, but the clock already reads %d{currentClock}; the clock is monotonic, so this script has drifted out of step with the run underneath it."

            // The furthest wins rather than each being applied in turn: the
            // clock is monotonic, so applying several jumps at one tick is
            // observationally identical to applying the largest, and taking the
            // max means a script listing them in any order behaves the same.
            Some (scheduled |> List.map snd |> List.max)

/// Deterministic model of a single Win32-shaped semaphore kernel object, as
/// minted by `CreateSemaphoreExW`. CoreCLR backs this with a real Win32
/// `CreateSemaphoreEx` on Windows and with a `SemaphoreSlim`-style construct
/// on Unix (via the QCall-rebound `Libraries.Kernel32`). PawPrint reproduces
/// the observable semantics through three pieces of state owned by the
/// kernel's `WaitHandles` registry:
///
///   - `Count` is the current signalled count: `WaitOne` decrements it
///     when positive, otherwise the caller is parked on `WaitQueue`.
///   - `Maximum` is the ceiling supplied at create time;
///     `ReleaseSemaphore` refuses (with `ERROR_TOO_MANY_POSTS`) when an
///     increment would breach it.
///   - `WaitQueue` is the FIFO list of threads parked in
///     `BlockedOnWaitHandle` or `BlockedOnWaitHandles`. The head-most
///     *satisfiable* entry is woken first by a subsequent `Release`; FIFO
///     order is required by the higher-level `LowLevelLifoSemaphore`
///     fairness contract.
///
/// Queue invariant (weakened by multi-handle wait): `Count > 0` does *not*
/// imply an empty `WaitQueue`. Every entry left in the queue after an
/// operation is either a single-handle waiter that the current `Count` could
/// not cover, or a wait-all multi-waiter that is verifiably unacquirable on
/// at least one of its *other* handles. Skipping such an entry rather than
/// blocking behind it is what the PAL does (`CSynchData`'s waiting-thread
/// walk consults `IsRestOfWaitAllSatisfied` and `continue`s past a node it
/// cannot satisfy, leaving it registered). One consequence looks like a bug
/// but is faithful: a *fresh* `WaitOne` arriving afterwards takes
/// the fast path and acquires ahead of that still-parked wait-all waiter. The
/// fast paths do not consult `WaitQueue`; adding a queue check
/// there would be a fidelity regression, not a fix.
type SemaphoreState =
    {
        Count : int
        Maximum : int
        WaitQueue : ThreadId list
    }

/// Ownership state of a single Win32-shaped mutex. Modelled as a DU
/// rather than an `Owner option + RecursionCount + IsAbandoned` triple
/// so that illegal states (held but with no owner; recursion count 0
/// while held; held *and* abandoned) are unrepresentable.
///
/// `Free wasAbandoned = true` is the post-abandonment Win32 contract: the
/// previous owner died while still holding the mutex, the kernel marks
/// the mutex free with a sticky "abandoned" flag, and the very next
/// `WaitOne` succeeds but returns `WAIT_ABANDONED` (which the BCL turns
/// into `AbandonedMutexException`). The flag is cleared by the
/// acquiring `WaitOne`.
[<RequireQualifiedAccess>]
type MutexOwnership =
    /// No thread currently owns the mutex. `wasAbandoned = true` means
    /// the previous owner terminated without calling `ReleaseMutex`;
    /// the next acquirer observes that fact (via `WAIT_ABANDONED`) and
    /// the flag clears as part of the acquire step.
    | Free of wasAbandoned : bool
    /// Held by `owner` with `recursionCount` outstanding `WaitOne`
    /// calls. `recursionCount` is always `≥ 1`; the released-the-last-
    /// nesting transition moves to `Free false` (or to a fresh `Held`
    /// state for the woken queue head, via direct handoff).
    | Held of owner : ThreadId * recursionCount : int

/// Deterministic model of a single Win32-shaped mutex kernel object, as
/// minted by `PAL_CreateMutexW`. Carries ownership (per `MutexOwnership`)
/// plus the FIFO wait queue of threads parked in `BlockedOnWaitHandle` or
/// `BlockedOnWaitHandles` because the mutex was held by another thread when
/// they called `WaitOne`. The wait queue lives outside the ownership DU
/// because it is orthogonal to who currently owns the mutex: a `Free` mutex
/// can have a non-empty queue. That happens when the only queued entries are
/// wait-all multi-waiters that are unacquirable on some *other* handle, so
/// the release found nobody to hand ownership to — see the queue-invariant
/// note on `SemaphoreState`. Absent multi-waiters the release path still
/// immediately re-installs the woken thread as the new owner, so the queue is
/// empty whenever the mutex is `Free`.
///
/// Mutexes are re-entrant on `owner`: a second `WaitOne` from the
/// owning thread succeeds on the fast path and bumps `recursionCount`.
/// Matching `ReleaseMutex` calls walk the count back down; the
/// outermost release either marks the mutex free or performs direct
/// handoff to the FIFO head of the queue.
type MutexState =
    {
        Ownership : MutexOwnership
        WaitQueue : ThreadId list
    }

/// Reset mode of a Win32-shaped event kernel object, set at create time and
/// immutable thereafter. `Manual` events stay signalled across waiters
/// (one `SetEvent` wakes every parked waiter and leaves the event
/// signalled until `ResetEvent`); `Auto` events have direct-handoff
/// semantics (one `SetEvent` wakes exactly one parked waiter, or sets the
/// signalled flag if none, and `WaitOne` on a signalled `Auto` event
/// consumes the signal as part of acquiring).
[<RequireQualifiedAccess>]
type EventResetMode =
    | Manual
    | Auto

/// Deterministic model of a single Win32-shaped event kernel object, as
/// minted by `CreateEventExW`. `Mode` is set at create time and never
/// changes. `Signaled` is the current signal state; `WaitQueue` is the
/// FIFO list of threads parked in `BlockedOnWaitHandle` or
/// `BlockedOnWaitHandles` because the event was unsignalled when they called
/// `WaitOne`.
///
/// Invariant: `Signaled = true` implies every remaining `WaitQueue` entry is
/// a wait-all multi-waiter that is verifiably unacquirable on at least one of
/// its *other* handles. Absent multi-waiters that degenerates to the stronger
/// `Signaled = true ⇒ WaitQueue = []`, which the operations enforce as
/// before: `setEvent` on a `Manual` event wakes every satisfiable waiter and
/// sets `Signaled = true`; `setEvent` on an `Auto` event either hands the
/// signal to the head-most satisfiable waiter (leaving `Signaled = false`) or
/// — if none is satisfiable — sets `Signaled = true`; `waitOne` on a
/// signalled `Auto` event consumes the signal as part of acquiring.
///
/// See the queue-invariant note on `SemaphoreState` for why an unsatisfiable
/// wait-all waiter is skipped rather than blocking the queue behind it, and
/// why the fast paths must not start consulting `WaitQueue` to compensate.
type EventState =
    {
        Mode : EventResetMode
        Signaled : bool
        WaitQueue : ThreadId list
    }

/// Kind-tagged state for a single Win32-shaped wait-handle kernel object
/// resident in `EmulatedKernel.WaitHandles`. Kind-agnostic operations
/// (`WaitHandle_WaitOneCore`, `CloseHandle`) take one map lookup and then
/// match on kind; new kinds slot in as additional cases without disturbing
/// the table or the wait/close handlers.
[<RequireQualifiedAccess>]
type WaitHandleState =
    | Semaphore of SemaphoreState
    | Mutex of MutexState
    | Event of EventState









/// Aggregates the slice of `IlMachineState` that models host-kernel /
/// syscall-emulation state: the per-thread last-error registers, the native
/// heap pool backing `Marshal.AllocHGlobal`, the CoreCLR PAL's synchronisation
/// objects, and the three records holding what a POSIX kernel owns — `Machine`,
/// `Process` and `Tasks`. These are the pieces of interpreter state that exist
/// because PawPrint refuses to use the host kernel; they don't belong in the
/// CIL execution model proper.
///
/// Pulling them into a sub-record keeps `IlMachineState` from sprawling and
/// makes it possible to swap the kernel implementation (e.g. for a Windows-
/// shaped emulation) without disturbing the rest of the state model.
type EmulatedKernel =
    {
        /// See `UnixProcessState`.
        ///
        /// Read through the forwarding members below rather than directly, so that
        /// moving a field in or out of here costs no call site.
        Process : UnixProcessState<ThreadId, SignalHandler>
        /// The POSIX machine this process is running on: see `UnixMachineState`.
        ///
        /// Read through the forwarding members below rather than directly, so that
        /// moving a field in or out of here costs no call site.
        Machine : UnixMachineState
        /// Per-thread value CoreCLR keeps in its `t_lastPInvokeError` thread-local and
        /// `Marshal.GetLastPInvokeError` (equivalently `GetLastWin32Error`) reads. A
        /// `SetLastError = true` P/Invoke's stub copies the system error here once the
        /// call returns.
        ///
        /// A `Map` rather than a `ThreadState` field, unlike `Cpu` and `OsThreadId`,
        /// because an absent key *does* have a truthful reading: 0, the value a thread
        /// that has had no error reported to it sees. `withLastPInvokeError` drops an
        /// entry it would set to 0, so "absent" and "zero" stay structurally equal —
        /// the same canonicalisation `SignalState.Blocked` performs for empty masks,
        /// and for the same reason: two states that differ only that way must compare
        /// equal.
        LastPInvokeError : Map<ThreadId, int>
        /// Per-thread system error: errno on Unix, `GetLastError` on Windows. CoreCLR's
        /// PAL stores its last-error *in* errno ("Reuse errno to store last error",
        /// pal/src/include/pal/thread.hpp), and `Marshal.Get/SetLastSystemError` read and
        /// write it directly.
        ///
        /// Tracked separately from `LastPInvokeError` because CoreLib's generated
        /// `LibraryImport` stubs read this and then write that. Same `Map` reasoning and
        /// same zero-drops-the-entry canonicalisation as `LastPInvokeError` above.
        LastSystemError : Map<ThreadId, int>
        /// Globally-scoped pool of native-heap blocks allocated by
        /// `Marshal.AllocHGlobal` / `NativeMemory.Alloc`. Freeing a block
        /// deletes it from this pool, so any retained byref into the block
        /// becomes a dangling reference that the simulator catches loudly at
        /// the use site. Unlike `StackMemoryPool` (which lives on each method
        /// frame and is reclaimed at frame exit), native-heap blocks outlive
        /// the frames that allocate them.
        NativeMemoryPool : NativeMemoryPool
        /// Every task the kernel knows about, by the thread that is it.
        ///
        /// Exactly the live threads: `IlMachineState.checkInvariants` refuses a
        /// state where a thread has no task or a task no thread. An absent key is
        /// therefore a bug rather than a default, which is what lets
        /// `UnixTaskState` be total.
        Tasks : Map<ThreadId, UnixTaskState>
        /// Registry of `System.Threading.LowLevelMonitor` instances minted by
        /// `SystemNative_LowLevelMonitor_Create`. The handle held by the
        /// guest (as an `IntPtr` in `LowLevelMonitor._nativeMonitor`) is the
        /// `LowLevelMonitorId` key; the value is the deterministic
        /// owner / queue state. `Destroy` removes the entry so any retained
        /// handle fails loudly at the next use rather than silently
        /// referencing a recycled monitor.
        LowLevelMonitors : Map<LowLevelMonitorId, LowLevelMonitorState>
        /// Monotonic ID source for `LowLevelMonitorPtr`. Starts at 1 so the
        /// guest's "create failed" check (`if _nativeMonitor == IntPtr.Zero`)
        /// is never triggered for a successfully-minted monitor. IDs are
        /// never reused; freeing a monitor leaves a gap.
        NextLowLevelMonitorId : int
        /// Registry of Win32-shaped wait-handle kernel objects (Semaphore /
        /// Event / Mutex), minted by `CreateSemaphoreExW` and its peers. The
        /// handle held by the guest (as an `IntPtr` produced by the QCall) is
        /// the `WaitHandleId` key; the value is the deterministic kind-tagged
        /// state. `CloseHandle` removes the entry so any retained handle
        /// fails loudly at the next use rather than silently referencing a
        /// recycled object.
        WaitHandles : Map<WaitHandleId, WaitHandleState>
        /// Monotonic ID source for `WaitHandlePtr`. Starts at 1 so the BCL's
        /// "create failed" check (`if (handle == IntPtr.Zero) throw new ...`)
        /// is never triggered for a successfully-minted handle. IDs are never
        /// reused; closing a handle leaves a gap.
        NextWaitHandleId : int
        /// Monotonic ID source for opaque EventPipe provider/event handles
        /// minted by the `EventPipeInternal_*` QCalls. PawPrint never opens a
        /// tracing session, so the IDs are not stored in any registry; they
        /// only need to be unique and non-zero (the BCL treats handle 0 as
        /// "create failed" and throws OOM).
        NextEventPipeId : int64
        /// Deterministic strategy governing spurious wakeups out of
        /// `LowLevelMonitor.Wait`. Defaults to `Disabled` so existing runs
        /// are bit-for-bit unchanged. Set this at construction time (or
        /// via record-copy in tests) to inject wakeups for fuzz /
        /// correctness testing of guest condition-variable code.
        SpuriousWakeup : SpuriousWakeupStrategy
        /// Deterministic strategy governing spurious wakeups out of the
        /// managed `Monitor.Wait` (SyncBlock-backed condition variable).
        /// Parallel-but-independent of `SpuriousWakeup` so a guest can fuzz
        /// the two condvar primitives separately. Defaults to `Disabled` so
        /// existing runs are bit-for-bit unchanged.
        SyncBlockSpuriousWakeup : SyncBlockSpuriousWakeupStrategy
        /// Deterministic strategy governing whether the driver jumps the virtual
        /// clock onto an outstanding deadline as part of a tick. Defaults to
        /// `Disabled` so existing runs are bit-for-bit unchanged. See
        /// `ClockJitterStrategy` for what it buys and `ClockJitter.chooseJump`
        /// for how it is interpreted.
        ClockJitter : ClockJitterStrategy
        /// Monotonically-advancing scheduler tick consumed by
        /// `SpuriousWakeupStrategy`. The driver loop applies the strategy
        /// against the current value and then increments by 1 before
        /// calling `Scheduler.chooseNext`. Threading the tick through state
        /// (rather than as a side argument to the scheduler) keeps the
        /// pure model self-contained and means tests can drive the strategy
        /// without spinning up a real driver.
        StepCounter : int64
        /// Virtual time charged for one retired IL instruction, in 100 ns ticks — how fast the
        /// simulated machine is. Must be >= 1; a cost of zero would freeze the clock and make
        /// every guest polling loop diverge.
        ///
        /// Kernel state rather than a constant because it is guest-observable: a guest can
        /// measure it by counting work against `Environment.TickCount64`, and it decides which
        /// BCL paths run — at a coarse enough rate `SpinWait` exhausts its spin budget in a
        /// couple of iterations and drops to the blocking path. So it belongs to the replay
        /// contract, alongside `WallClockEpochMs` and `ProcessorCount`, both of which are here
        /// for the same reason. See `EmulatedKernel.defaultInstructionCostTicks`.
        InstructionCostTicks : int64
        /// Number reported by `Thread.OptimalMaxSpinWaitsPerSpinIteration` (an
        /// `internal` property, reached only via `SpinWait.SpinOnce()` /
        /// `LowLevelSpinWaiter` in ordinary guest code). Deliberately a value
        /// in kernel state rather than a host read, for the same reason as
        /// `ProcessorCount`: real CoreCLR computes it in
        /// `YieldProcessorNormalization::PerformMeasurement`
        /// (`yieldprocessornormalizedshared.cpp`) by literally timing how long
        /// a `YieldProcessor()`/PAUSE instruction takes on the *host* CPU,
        /// dividing elapsed hi-res ticks by the yield count. The initial pass
        /// runs on a background finalizer-thread callback and takes
        /// `NsPerYieldMeasurementCount` = 8 samples of `DetermineMeasureDurationUs()`
        /// = 1 or 4 microseconds each, so 8–32 us of actual spinning; thereafter
        /// `MeasurementPeriodMs` = 4000 is a floor on how often it may
        /// re-measure (one sample per refresh), not time spent spinning. That
        /// measurement is about as host-dependent as a number gets, so it must
        /// be mocked.
        ///
        /// See `EmulatedKernel.defaultOptimalMaxSpinWaitsPerSpinIteration` for
        /// the default and how it was chosen, and
        /// `EmulatedKernel.maxOptimalMaxSpinWaitsPerSpinIteration` for the
        /// ceiling this field is validated against.
        ///
        /// Unlike `ProcessorCount`, nothing in CoreLib latches this into a
        /// cctor-time static: `SpinWait.SpinOnce`/`LowLevelSpinWaiter` re-read
        /// `Thread.OptimalMaxSpinWaitsPerSpinIteration` on every call, so a
        /// host may freely change it via record-copy mid-run if it ever needs
        /// to (though `KernelConfig` remains the normal way to set it, for
        /// the same "fixed for the whole recorded run" reason as the other
        /// kernel knobs).
        OptimalMaxSpinWaitsPerSpinIteration : int
        /// Which stream each guest-held `DIR*` names.
        ///
        /// An absent key is not a default and must never be read as one: it means
        /// the guest passed a `DIR*` this kernel never issued, or one it has
        /// already closed. Both are undefined behaviour on a real libc rather
        /// than errors it reports, so `directoryStreamId` refuses loudly.
        ///
        /// This is PawPrint's choice of how to represent a stream to its guest —
        /// the address of a native block, whose bytes are also the `d_name`
        /// buffer each `readdir` refills — so it is separate from
        /// `DirectoryStreams`, which is kernel state a POSIX simulator owns
        /// whatever its client hands out. A second client could key its own
        /// streams on anything at all.
        ///
        /// The two are maintained together by `withDirectoryStreamBlock` and
        /// `withoutDirectoryStream`, and `checkInvariants` refuses a state in
        /// which they disagree in either direction.
        DirectoryStreamBlocks : Map<NativeMemoryBlockId, DirectoryStreamId>
    }

    // Forwarding members for everything `Process` now holds, so that this split
    // costs no read site. They go when stage 6 moves the state to the library and
    // call sites learn to say `kernel.Process.X`.
    member this.FileDescriptors : FileDescriptorRegistry = this.Process.FileDescriptors
    member this.OutputLog : ImmutableArray<OutputLogEntry> = this.Process.OutputLog
    member this.Environment : Map<string, string> = this.Process.Environment
    member this.CurrentDirectory : AbsoluteUnixPath = this.Process.CurrentDirectory
    member this.CurrentDirectoryInode : InodeNumber = this.Process.CurrentDirectoryInode
    member this.ProcessPath : AbsoluteUnixPath option = this.Process.ProcessPath

    member this.DirectoryStreams : Map<DirectoryStreamId, DirectoryStream> =
        this.Process.DirectoryStreams

    member this.NextDirectoryStreamId : DirectoryStreamId =
        this.Process.NextDirectoryStreamId

    member this.UserId : uint32 = this.Process.UserId
    member this.GroupId : uint32 = this.Process.GroupId
    member this.Umask : PermissionBits = this.Process.Umask
    member this.Signals : SignalState<ThreadId, SignalHandler> = this.Process.Signals

    // Forwarding members for everything `Machine` now holds, so that this split
    // costs no read site. They go when stage 6 moves `UnixMachineState` to the
    // library and call sites learn to say `kernel.Machine.X`.
    member this.Sockets : Map<SocketId, SocketDescription> = this.Machine.Sockets
    member this.Connections : Map<ConnectionId, TcpConnection> = this.Machine.Connections
    member this.NextConnectionId : ConnectionId = this.Machine.NextConnectionId

    member this.NextSocketEventRegistrationOrdinal : int64 =
        this.Machine.NextSocketEventRegistrationOrdinal

    member this.NextEphemeralPort : uint16 = this.Machine.NextEphemeralPort
    member this.EphemeralPortRange : uint16 * uint16 = this.Machine.EphemeralPortRange
    member this.SoMaxConn : int = this.Machine.SoMaxConn
    member this.LocalAddresses : uint32 list = this.Machine.LocalAddresses
    member this.LocalRoutes : Ipv4Prefix list = this.Machine.LocalRoutes
    member this.NextSocketId : SocketId = this.Machine.NextSocketId
    member this.VirtualClockTicks : int64 = this.Machine.VirtualClockTicks
    member this.WallClockEpochMs : int64 = this.Machine.WallClockEpochMs
    member this.NonCryptoRandomState : uint64 = this.Machine.NonCryptoRandomState
    member this.CryptoRandomState : uint64 = this.Machine.CryptoRandomState
    member this.ProcessorCount : int = this.Machine.ProcessorCount
    member this.UserAddressLimit : uint64 = this.Machine.UserAddressLimit
    member this.UnixPlatform : SimulatedUnixPlatform = this.Machine.UnixPlatform
    member this.FileSystem : VirtualFileSystem = this.Machine.FileSystem
    member this.FileSystemType : EmulatedFileSystemType = this.Machine.FileSystemType

/// A way this kernel's own tables disagree with the POSIX system underneath
/// them — a state no kernel could be in, and which `EmulatedKernel` exists to
/// keep unreachable.
///
/// The system's own rules are `UnixSystemDefect`, which `System` carries. What
/// is left here is the two things PawPrint holds that no POSIX kernel does: the
/// native-heap blocks a guest's `DIR*` values are, and the threads its tasks
/// belong to.
[<RequireQualifiedAccess>]
type EmulatedKernelDefect =
    /// A way the POSIX system this kernel runs is itself unsound: see
    /// `UnixSystemDefect`.
    | System of defect : UnixSystemDefect
    /// A guest-held `DIR*` names a stream the stream table does not hold, so the
    /// next `readdir` through it would crash rather than enumerate.
    | DirectoryStreamBlockDangling of block : NativeMemoryBlockId * stream : DirectoryStreamId
    /// A thread exists with no task, so anything asking the kernel which
    /// processor it runs on or what OS thread id it reports would crash.
    | ThreadWithoutTask of thread : ThreadId
    /// A task exists for a thread that does not, so its processor placement and
    /// OS thread id are held for a thread that can never read them.
    | TaskWithoutThread of thread : ThreadId
    /// A thread is parked in `ThreadStatus.BlockedInSyscall` but its task records
    /// no park, so nothing says what it is waiting for: no sweep can decide
    /// whether to wake it, and the re-entered handler could not decide what to
    /// finish. Such a thread sleeps for the rest of the run.
    | SyscallWaiterWithoutRecord of thread : ThreadId
    /// A task records a park while its thread is in a status that cannot be
    /// holding a syscall open.
    ///
    /// `Runnable` is legitimate and not slack: between a sweep waking a waiter
    /// and the woken thread re-entering its handler, the thread is `Runnable`
    /// with its record intact — and the record is precisely what tells the
    /// re-entered handler that it is a re-entry.
    | SyscallRecordWithoutWaiter of thread : ThreadId * status : ThreadStatus
    /// The stream table holds a stream no `DIR*` names, so nothing can ever
    /// read or close it and the directory it pins is held for the run.
    | UnreachableDirectoryStream of stream : DirectoryStreamId
    /// More than one `DIR*` names one stream, so closing either would take the
    /// stream out from under the others. Two `opendir`s owe the guest
    /// independent cursors, so this is never a state a stream table should hold.
    | DirectoryStreamNamedTwice of stream : DirectoryStreamId * blocks : NativeMemoryBlockId list

[<RequireQualifiedAccess>]
module EmulatedKernel =

    /// Apply an operation to the POSIX machine this process runs on. Those
    /// operations live in `UnixMachineState`, which takes that machine rather
    /// than the kernel.
    let mapMachine (f : UnixMachineState -> UnixMachineState) (kernel : EmulatedKernel) : EmulatedKernel =
        { kernel with
            Machine = f kernel.Machine
        }

    /// This kernel's POSIX half, as `UnixSystem.step` and its per-syscall
    /// siblings want it. Allocates: `EmulatedKernel` stores the three parts
    /// flat, and this assembles a view of them.
    let unix (kernel : EmulatedKernel) : UnixSystem<ThreadId, SignalHandler> =
        {
            Machine = kernel.Machine
            Process = kernel.Process
            Tasks = kernel.Tasks
        }

    /// Put back a POSIX half a syscall answered from. Total in both directions
    /// with `unix`, which `TestUnixSystemProjection` asserts: a syscall's answer
    /// is lost if a caller forgets this, and gained twice if a caller writes
    /// back a system it did not step.
    let withUnix (system : UnixSystem<ThreadId, SignalHandler>) (kernel : EmulatedKernel) : EmulatedKernel =
        { kernel with
            Machine = system.Machine
            Process = system.Process
            Tasks = system.Tasks
        }

    /// Apply an operation that spans this kernel's whole POSIX half. Those
    /// operations live in `UnixSystem`, which takes the three parts as one
    /// record rather than the kernel.
    let mapUnix
        (f : UnixSystem<ThreadId, SignalHandler> -> UnixSystem<ThreadId, SignalHandler>)
        (kernel : EmulatedKernel)
        : EmulatedKernel
        =
        withUnix (f (unix kernel)) kernel

    /// Default environment variables for a freshly-minted simulated process.
    /// PawPrint only implements invariant-globalization today, so this seed
    /// must always be applied: callers that supply a custom environment
    /// overlay it on top of these defaults, which means the host (or a test)
    /// can override `DOTNET_SYSTEM_GLOBALIZATION_INVARIANT` if it really
    /// needs to, while forgetting to set it keeps the runtime in the regime
    /// it actually supports.
    let defaultEnvironment : Map<string, string> =
        Map.ofList [ "DOTNET_SYSTEM_GLOBALIZATION_INVARIANT", "1" ]

    /// Seed for `EmulatedKernel.CryptoRandomState`. The first 64 bits of the
    /// fractional part of pi — a nothing-up-my-sleeve constant chosen purely
    /// so that the crypto-entropy stream starts somewhere other than
    /// `NonCryptoRandom.initialState` (the golden-ratio constant). Any
    /// non-zero value distinct from that one would do; splitmix64 has no weak
    /// seeds. Changing it changes every `Guid.NewGuid` a recorded trace
    /// observes, so treat it as part of PawPrint's replay contract.
    let cryptoRandomInitialState : uint64 = 0x243F6A8885A308D3UL



    /// Ceiling `Thread.OptimalMaxSpinWaitsPerSpinIteration` can legally report,
    /// mirroring CoreCLR's own compile-time ceiling
    /// `YieldProcessorNormalization::MaxOptimalMaxNormalizedYieldsPerSpinIteration`
    /// (`yieldprocessornormalized.h`):
    /// `TargetMaxNsPerSpinIteration * 3 / (TargetNsPerNormalizedYield * 2) + 1`
    /// = `272 * 3 / (37 * 2) + 1` = `816 / 74 + 1` = `11 + 1` (integer
    /// division) = `12`. CoreCLR asserts its measured value never exceeds
    /// this; `withOptimalMaxSpinWaitsPerSpinIteration` enforces the same bound
    /// on a host-supplied value so PawPrint can never hand a guest a number
    /// the real property could not produce.
    [<Literal>]
    let maxOptimalMaxSpinWaitsPerSpinIteration : int = 12

    /// Default for `Thread.OptimalMaxSpinWaitsPerSpinIteration` a freshly-
    /// minted simulated process reports. Derived from the same formula
    /// CoreCLR's real measurement feeds
    /// (`yieldprocessornormalizedshared.cpp`,
    /// `s_optimalMaxNormalizedYieldsPerSpinIteration = max(1, round(
    /// TargetMaxNsPerSpinIteration / (yieldsPerNormalizedYield *
    /// establishedNsPerYield)))`), evaluated at the one input CoreCLR itself
    /// treats as the target rather than a measurement: assume the host's
    /// per-yield cost lands exactly on `TargetNsPerNormalizedYield` (37ns),
    /// the value CoreCLR's static `s_establishedNsPerYield` is seeded with
    /// before any measurement ever runs. That gives
    /// `yieldsPerNormalizedYield = max(1, round(37 / 37)) = 1`, and hence
    /// `optimalMaxNormalizedYieldsPerSpinIteration = max(1, round(272 / (1 *
    /// 37))) = round(7.35) = 7`. This is a "textbook" host, not an arbitrary
    /// number: it is what the exact same formula CoreCLR uses would compute
    /// for a CPU that matches the calibration's own design target precisely,
    /// which is a more defensible fixed point than either extreme
    /// (`1`, degenerate minimum spinning; `12`, the hard ceiling).
    ///
    /// CoreCLR also ships exactly this number as its own literal
    /// pre-measurement default: `src/coreclr/utilcode/yieldprocessornormalized.cpp`
    /// initialises `s_optimalMaxNormalizedYieldsPerSpinIteration` to
    /// `(unsigned int)(272.0 / 37.0 + 0.5)` = `7`, commented "Defaults are for
    /// when normalization has not yet been done". So 7 is the value a real
    /// CoreCLR process reports for the entire window before its background
    /// measurement completes; a simulated process that never performs that
    /// measurement reports the never-measured default.
    [<Literal>]
    let defaultOptimalMaxSpinWaitsPerSpinIteration : int = 7

    /// Virtual time charged for one retired IL instruction, in 100 ns ticks.
    ///
    /// This is the *rate* half of the clock; `ticksPerMillisecond` is the unit half. Together
    /// they say how fast the simulated machine is: at one tick per instruction it would be a
    /// self-consistent 10 MIPS machine.
    ///
    /// One tick — 100 ns, a 10 MIPS machine. The value is a calibration choice, and the
    /// quantity it calibrates is the ratio between the shortest sleep a guest can express and
    /// the cost of one instruction, because that ratio is what decides whether the BCL's
    /// spin-then-sleep backoff does anything at all.
    ///
    /// A cost at or near a whole millisecond makes that ratio 1:1, at which `Thread.Sleep(1)`'s
    /// deadline expires inside the very `fireExpiredDeadlines` pass that precedes the next
    /// scheduling decision: the sleeper misses *zero* decisions and `Sleep` is a no-op. That is
    /// the failure this value exists to avoid, so keep it several orders of magnitude below
    /// `ticksPerMillisecond`.
    ///
    /// Why 100 ns and not 1 µs. Post-backoff, a `SpinWait`
    /// spinner's cycle is one `Sleep(1)` park plus a *measured* 67 retired instructions. Sixteen
    /// of them therefore demand 1,072 instructions per park window, against a window of
    /// `ticksPerMillisecond / InstructionCostTicks` instructions. At 1 µs the window is 1,000 —
    /// smaller than the demand, so the spinners still saturate the machine and the fix does not
    /// work at all. At 100 ns it is 10,000, leaving the producer ~89%, which is the right shape
    /// for a single-core machine whose other threads are asleep. A further 10× buys ~10 points
    /// and costs 10× the run length.
    ///
    /// The cost is paid by guests that busy-poll a clock while another thread is runnable: such
    /// a loop waiting M ms now costs `M * 10^4` interpreted instructions, and the driver's
    /// jump-to-deadline shortcut cannot help because the poller is runnable. That is also what
    /// the loop would cost on a real 10 MIPS machine, and the BCL's own polling paths escalate
    /// to `Sleep(1)`, which now parks and lets the jump engage.
    [<Literal>]
    let defaultInstructionCostTicks : int64 = 1L











    /// A freshly-minted simulated process, as PawPrint starts one.
    ///
    /// The POSIX half is `UnixSystem.initial`'s; what is added here is the
    /// CoreCLR-shaped state no POSIX kernel has, and the three values PawPrint
    /// pins rather than inherits. Those three are stated rather than left to the
    /// library because each is part of PawPrint's replay contract: a change to
    /// the library's default must not silently change what a recorded trace
    /// observes.
    let initial : EmulatedKernel =
        let system : UnixSystem<ThreadId, SignalHandler> =
            UnixSystem.initial UnixSystem.defaultUnixPlatform

        {
            InstructionCostTicks = defaultInstructionCostTicks
            LastPInvokeError = Map.empty
            LastSystemError = Map.empty
            NativeMemoryPool = NativeMemoryPool.empty
            DirectoryStreamBlocks = Map.empty
            Tasks = Map.empty
            LowLevelMonitors = Map.empty
            NextLowLevelMonitorId = 1
            WaitHandles = Map.empty
            NextWaitHandleId = 1
            NextEventPipeId = 1L
            SpuriousWakeup = SpuriousWakeupStrategy.Disabled
            SyncBlockSpuriousWakeup = SyncBlockSpuriousWakeupStrategy.Disabled
            ClockJitter = ClockJitterStrategy.Disabled
            StepCounter = 0L
            OptimalMaxSpinWaitsPerSpinIteration = defaultOptimalMaxSpinWaitsPerSpinIteration
            Machine =
                { system.Machine with
                    NonCryptoRandomState = NonCryptoRandom.initialState
                    CryptoRandomState = cryptoRandomInitialState
                }
            Process =
                { system.Process with
                    Environment = defaultEnvironment
                }
        }


    /// The directory `directory` names in this kernel's filesystem, as the
    /// moment a process is started resolves it — which is the only moment
    /// PawPrint resolves it, because after that the process holds the directory
    /// rather than the name.
    ///
    /// Answers the inode *and* the path `getcwd` owes for it, which is not
    /// always the path that was asked for: `getcwd(3)` reports the **physical**
    /// path, with every symlink resolved away. Measured on both kernels —
    /// `chdir("outer/lnk")` with `lnk -> inner` is followed by
    /// `getcwd() == ".../outer/inner"`. Deriving both here is what stops the
    /// pair describing a process no Unix could produce.
    ///
    /// Privileged and symlink-following, deliberately: this is the host saying
    /// where its guest was launched, not a guest looking anything up, and a
    /// process is launched into a directory its parent had already reached. A
    /// failure here is therefore a host mistake with no honest errno — ENOENT
    /// would blame a guest path that does not exist yet — so it crashes, naming
    /// the two knobs that have to agree.
    let private currentDirectoryOf
        (directory : AbsoluteUnixPath)
        (platform : SimulatedUnixPlatform)
        (filesystem : VirtualFileSystem)
        : InodeNumber * AbsoluteUnixPath
        =
        let limits = SimulatedUnixPlatform.pathLimits platform
        let root = VirtualFileSystem.root filesystem

        match
            VirtualFileSystem.resolveExisting
                limits
                CallerPrivilege.Privileged
                root
                SymlinkPolicy.Follow
                (UnixPath.ofAbsolute directory)
                filesystem
        with
        | Ok inode ->
            match VirtualFileSystem.tryGetContent inode filesystem with
            | Some (InodeContent.Directory _) ->
                match VirtualFileSystem.pathOfDirectory inode filesystem with
                | Some physical -> inode, physical
                | None ->
                    failwith
                        $"EmulatedKernel.CurrentDirectory: \"%s{AbsoluteUnixPath.toString directory}\" resolved to inode %O{inode}, but no path from the root reaches it. Run VirtualFileSystem.checkInvariants."
            | Some (InodeContent.RegularFile _)
            | Some (InodeContent.Symlink _) ->
                failwith
                    $"EmulatedKernel.CurrentDirectory: \"%s{AbsoluteUnixPath.toString directory}\" resolves in KernelConfig.FileSystem, but not to a directory. No process can be started anywhere else; point KernelConfig.CurrentDirectory at a directory the seed contains."
            | None ->
                failwith
                    $"EmulatedKernel.CurrentDirectory: resolving \"%s{AbsoluteUnixPath.toString directory}\" gave inode %O{inode}, which the filesystem does not contain. Run VirtualFileSystem.checkInvariants."
        | Error UnixError.ENAMETOOLONG ->
            // Distinguished because the remedy is different, and the message
            // below would send the reader looking for a missing directory that
            // is in fact present.
            failwith
                $"EmulatedKernel.CurrentDirectory: \"%s{AbsoluteUnixPath.toString directory}\" contains a component longer than %O{SimulatedUnixPlatform.flavour platform}'s NAME_MAX, so no process could have been started in it. Shorten KernelConfig.CurrentDirectory."
        | Error error ->
            failwith
                $"EmulatedKernel.CurrentDirectory: \"%s{AbsoluteUnixPath.toString directory}\" does not resolve in KernelConfig.FileSystem (%O{error}). A process cannot be started in a directory that does not exist; make KernelConfig.FileSystem contain KernelConfig.CurrentDirectory."

    /// Apply an operation to the simulated process's own state. Those operations
    /// live in `UnixProcessState`, which takes that state rather than the kernel.
    let mapProcess
        (f : UnixProcessState<ThreadId, SignalHandler> -> UnixProcessState<ThreadId, SignalHandler>)
        (kernel : EmulatedKernel)
        : EmulatedKernel
        =
        { kernel with
            Process = f kernel.Process
        }



    /// Set the filesystem the guest sees, and the directory the simulated
    /// process starts in, together.
    ///
    /// One setter rather than two because neither answer is well-formed without
    /// the other: a current directory is an inode of *this* filesystem, and a
    /// filesystem replaces every inode number the previous one handed out. The
    /// same reason `withUnixPlatformAndFileSystemType` is one setter.
    ///
    /// Takes the moment and the platform explicitly rather than reading
    /// `kernel.WallClockEpochMs` and `kernel.UnixPlatform`, so that the result
    /// does not depend on whether the caller happened to set the clock or the
    /// flavour before or after the filesystem — an ordering dependence between
    /// two `with` functions is exactly the kind of thing that works until
    /// someone reorders `KernelConfig.applyTo`.
    ///
    /// The platform is here because its `NAME_MAX` decides whether the *path
    /// the host wrote* is one a process on that flavour could name at all: 255
    /// CJK characters is a legal directory name on Darwin and too long on
    /// Linux. It is a check on that path and not on the graph — the seed itself
    /// is realised without consulting any limit, so a filesystem may perfectly
    /// well contain a directory whose name the current directory could not
    /// spell.
    let withFileSystemAndCurrentDirectory
        (platform : SimulatedUnixPlatform)
        (createdAt : UnixTimestamp)
        (seed : Map<FileName, SeedEntry>)
        (directory : AbsoluteUnixPath)
        (kernel : EmulatedKernel)
        : EmulatedKernel
        =
        let platform =
            SimulatedUnixPlatform.assertValid "EmulatedKernel.UnixPlatform" platform

        let directory =
            AbsoluteUnixPath.assertValid "EmulatedKernel.CurrentDirectory" directory

        let filesystem = FileSystemSeed.toVirtualFileSystem createdAt seed
        let inode, physical = currentDirectoryOf directory platform filesystem

        { kernel with
            Machine =
                { kernel.Machine with
                    FileSystem = filesystem
                }
            Process =
                { kernel.Process with
                    CurrentDirectory = physical
                    CurrentDirectoryInode = inode
                }
        }



    /// Set the virtual time charged per retired instruction. See
    /// `EmulatedKernel.InstructionCostTicks` for what the number means and why it is
    /// configurable; `defaultInstructionCostTicks` for how the default was calibrated.
    let withInstructionCostTicks (cost : int64) (kernel : EmulatedKernel) : EmulatedKernel =
        if cost < 1L then
            failwith
                $"InstructionCostTicks must be at least 1; got %d{cost}. A cost of zero freezes the virtual clock, so any guest waiting for time to pass would spin forever."

        { kernel with
            InstructionCostTicks = cost
        }

    /// Install the clock-jitter strategy the driver applies each tick. See
    /// `ClockJitterStrategy` for what the variants mean.
    ///
    /// A malformed `EagerDeadlines` probability is rejected here rather than at
    /// the first tick that consults it, so a host that misconfigures a run finds
    /// out before any guest code has executed.
    let withClockJitter (strategy : ClockJitterStrategy) (kernel : EmulatedKernel) : EmulatedKernel =
        ClockJitter.validate strategy

        { kernel with
            ClockJitter = strategy
        }




    /// Set the value the simulated process reports from
    /// `Thread.OptimalMaxSpinWaitsPerSpinIteration`. Rejects values outside
    /// `[1, maxOptimalMaxSpinWaitsPerSpinIteration]` at the boundary: the
    /// lower bound matches CoreCLR's own `max(1u, ...)` floor, and the upper
    /// bound matches CoreCLR's own compile-time ceiling (see
    /// `maxOptimalMaxSpinWaitsPerSpinIteration`) — a value outside that range
    /// is not one the real property could ever produce, so accepting it here
    /// would let a guest observe an impossible host.
    let withOptimalMaxSpinWaitsPerSpinIteration (count : int) (kernel : EmulatedKernel) : EmulatedKernel =
        if count < 1 || count > maxOptimalMaxSpinWaitsPerSpinIteration then
            failwith
                $"OptimalMaxSpinWaitsPerSpinIteration must be between 1 and %d{maxOptimalMaxSpinWaitsPerSpinIteration} inclusive (CoreCLR's own bounds for this value); got %d{count}"

        { kernel with
            OptimalMaxSpinWaitsPerSpinIteration = count
        }


    /// Retire one interpreted instruction: bump `StepCounter` by one and charge
    /// `InstructionCostTicks` of virtual time, subject to exactly the checks `withVirtualClockTicks`
    /// applies.
    ///
    /// Equivalent to bumping `StepCounter` by record-copy and piping the result through
    /// `withVirtualClockTicks`, and exists only because that spelling costs two copies of a
    /// 31-field record where this costs one. The interpreter performs it once per retired IL
    /// instruction, which is what makes one record copy worth a named function.
    let retireStep (kernel : EmulatedKernel) : EmulatedKernel =
        let ticks = kernel.VirtualClockTicks + kernel.InstructionCostTicks

        // Through the same validation rather than trusting the arithmetic. `withInstructionCostTicks`
        // rejects a cost below 1, and `KernelConfig.applyTo` is the only production path that sets
        // the field, so a legally-assembled kernel cannot reach here with one — but a kernel built
        // by record-copy bypasses that setter entirely, which is the same hole the monotonicity
        // check below already exists to cover. Revalidating keeps this path's guarantee independent
        // of how its caller's kernel was assembled.
        UnixMachineState.validateVirtualClockTicks ticks kernel.Machine

        { kernel with
            StepCounter = kernel.StepCounter + 1L
            Machine =
                { kernel.Machine with
                    VirtualClockTicks = ticks
                }
        }




    /// Largest value CoreCLR will accept from the processor-count
    /// configuration knob (`MAX_PROCESSOR_COUNT` in
    /// coreclr/utilcode/util.cpp). Values above this fall back to detection.
    [<Literal>]
    let private maxConfiguredProcessorCount : int = 0xffff

    /// `strtoul`-shaped base-10 parse of a CLRConfig integer value, returning
    /// `None` where CoreCLR's `CLRConfig::GetConfigDWORD` would report failure
    /// (and hence substitute the knob's declared default of 0).
    ///
    /// Matches `u16_strtoul(val, &endPtr, 10)` plus the
    /// `errno != ERANGE && endPtr != val` success test in
    /// coreclr/utilcode/clrconfig.cpp: leading whitespace is skipped, at least
    /// one digit is required, and trailing garbage is ignored — so "4abc"
    /// really does yield 4 on the real runtime, and we reproduce that rather
    /// than being stricter than the thing we emulate.
    ///
    /// Deliberate divergence: a leading '-' is rejected here, whereas C
    /// `strtoul` would wrap it into a huge unsigned value. Every such value is
    /// then rejected by the caller's `<= 0xffff` window anyway, except for
    /// contrived inputs chosen to wrap exactly back into it (e.g.
    /// "-4294901761"). Reproducing that would mean modelling the platform's
    /// `unsigned long` width, and no real configuration depends on it.
    let private tryParseConfigBase10 (s : string) : int option =
        // strtoul skips leading whitespace as determined by `isspace` in the C
        // locale, which is exactly this six-character set. Deliberately NOT
        // `Char.IsWhiteSpace`, which also accepts U+00A0 and friends: on Unix
        // the value reaches `strtoul` as UTF-8 bytes, so a non-breaking space
        // is the two bytes 0xC2 0xA0 and stops the parse dead rather than being
        // skipped. Using the .NET predicate would make PawPrint accept
        // configuration the real runtime rejects.
        let isCLocaleSpace (c : char) : bool =
            c = ' ' || c = '\t' || c = '\n' || c = '\011' || c = '\012' || c = '\r'

        let mutable i = 0

        while i < s.Length && isCLocaleSpace s.[i] do
            i <- i + 1

        if i < s.Length && s.[i] = '+' then
            i <- i + 1
        elif i < s.Length && s.[i] = '-' then
            // See the divergence note above.
            i <- s.Length + 1

        let digitStart = i
        let mutable acc = 0L

        while i < s.Length && s.[i] >= '0' && s.[i] <= '9' do
            // Saturate rather than overflow: anything this large is out of the
            // caller's acceptance window regardless of its exact value.
            acc <- min (acc * 10L + int64 (int s.[i] - int '0')) (int64 maxConfiguredProcessorCount + 1L)
            i <- i + 1

        if i > s.Length || i = digitStart then
            None
        else
            Some (int acc)

    /// Processor count the guest actually observes from
    /// `Environment.ProcessorCount`.
    ///
    /// CoreCLR's `GetCurrentProcessCpuCount` (coreclr/utilcode/util.cpp) gives
    /// the `PROCESSOR_COUNT` configuration knob precedence over CPU detection,
    /// accepting it only when it lands in `(0, MAX_PROCESSOR_COUNT]`, and
    /// otherwise falling back to affinity/quota detection. PawPrint reproduces
    /// that shape with `ProcessorCount` standing in for the detection result.
    ///
    /// Reading the knob out of the *kernel's* environment table (rather than
    /// the host process's) is what keeps this deterministic: the table is
    /// recorded state that a replay reconstructs exactly, so honouring the
    /// standard knob costs nothing in reproducibility. `CLRConfig` tries the
    /// `DOTNET_` prefix first and falls back to the legacy `COMPlus_` prefix
    /// only when the former is absent (coreclr/utilcode/clrconfig.cpp), and
    /// both lookups are case-sensitive on the Unix hosts this project targets.
    let effectiveProcessorCount (kernel : EmulatedKernel) : int =
        // An empty value counts as absent, and so falls through to the legacy
        // prefix: CLRConfig's fallback is gated on
        // `WszGetEnvironmentVariable` returning length zero, which is what a
        // variable set to the empty string reports. `DOTNET_PROCESSOR_COUNT=`
        // with `COMPlus_PROCESSOR_COUNT=9` set therefore yields 9 upstream, not
        // the detected count.
        let lookup (name : string) : string option =
            match Map.tryFind name kernel.Environment with
            | Some "" -> None
            | other -> other

        let configured =
            match lookup "DOTNET_PROCESSOR_COUNT" with
            | Some v -> Some v
            | None -> lookup "COMPlus_PROCESSOR_COUNT"

        match configured |> Option.bind tryParseConfigBase10 with
        | Some count when count > 0 && count <= maxConfiguredProcessorCount -> count
        | _ -> kernel.ProcessorCount

    /// Apply an operation to the tasks this kernel knows about. Those operations
    /// live in `UnixTaskTable`, which takes the table rather than the kernel.
    let mapTasks
        (f : Map<ThreadId, UnixTaskState> -> Map<ThreadId, UnixTaskState>)
        (kernel : EmulatedKernel)
        : EmulatedKernel
        =
        { kernel with
            Tasks = f kernel.Tasks
        }

    /// The system error (errno on Unix, `GetLastError` on Windows) `thread` would read.
    /// 0 for a thread that has had none reported to it, which is what a fresh thread sees.
    let lastSystemErrorFor (thread : ThreadId) (kernel : EmulatedKernel) : int =
        match Map.tryFind thread kernel.LastSystemError with
        | None -> 0
        | Some value -> value

    /// Set `thread`'s system error. Setting 0 removes the entry rather than storing it, so
    /// that a state which has zeroed a thread's errno is structurally equal to one that
    /// never wrote it; `EmulatedKernel` is compared for equality to decide whether a step
    /// changed anything, so "absent" and "zero" must not be distinguishable.
    let withLastSystemError (thread : ThreadId) (value : int) (kernel : EmulatedKernel) : EmulatedKernel =
        { kernel with
            LastSystemError =
                if value = 0 then
                    Map.remove thread kernel.LastSystemError
                else
                    Map.add thread value kernel.LastSystemError
        }

    /// The value `thread` would read from `Marshal.GetLastPInvokeError`. 0 until a
    /// `SetLastError = true` P/Invoke on that thread copies a system error into it.
    let lastPInvokeErrorFor (thread : ThreadId) (kernel : EmulatedKernel) : int =
        match Map.tryFind thread kernel.LastPInvokeError with
        | None -> 0
        | Some value -> value

    /// Set `thread`'s last-P/Invoke error, with the same zero-drops-the-entry
    /// canonicalisation as `withLastSystemError`.
    let withLastPInvokeError (thread : ThreadId) (value : int) (kernel : EmulatedKernel) : EmulatedKernel =
        { kernel with
            LastPInvokeError =
                if value = 0 then
                    Map.remove thread kernel.LastPInvokeError
                else
                    Map.add thread value kernel.LastPInvokeError
        }

    /// Placement policy: which simulated logical processor the `rotation`-th
    /// guest-visible thread is pinned to. The only producer of `CpuId` for
    /// threads a guest can observe, so "every `CpuId` a guest can read names a
    /// processor it also counts" is established here once rather than
    /// re-checked at every read. (`IlMachineState.allocateParkedThread` also
    /// mints a `CpuId`, but a fixed core 0 for PawPrint-internal threads no
    /// guest can name; see there.)
    ///
    /// Round-robin over `effectiveProcessorCount`. That is a *placement*
    /// decision, not a measurement: PawPrint's scheduler runs one thread at a
    /// time and never migrates a thread between cores, so the core a thread is
    /// pinned to is also the core it is running on whenever it is running, and
    /// one value answers both questions `sched_getcpu` could be asked.
    ///
    /// Spreading threads over the available cores (rather than reporting a
    /// constant 0) is what makes a host-configured `ProcessorCount` mean
    /// something to the guest: CoreLib shards `ArrayPool<T>.Shared` partitions,
    /// `TimerQueue.Instances`, and `PoolingAsyncValueTaskMethodBuilder`'s cache
    /// by this value, so a constant would leave every one of those multi-shard
    /// paths permanently unexercised. With the default `ProcessorCount` of 1 it
    /// collapses to a constant 0 anyway, so existing runs are bit-for-bit
    /// unchanged.
    ///
    /// `rotation` deliberately is *not* the thread's `ThreadId`. `ThreadId`s
    /// are also consumed by PawPrint-internal auxiliary threads that never run
    /// guest IL (`IlMachineState.allocateParkedThread`, currently the signal
    /// dispatcher), so keying off them would let an interpreter-internal
    /// allocation shift which core every subsequently created *guest* thread
    /// lands on — an interpreter detail leaking into guest-observable state.
    /// The caller therefore threads a separate cursor
    /// (`IlMachineState.NextCpuRotation`) that only guest-visible thread
    /// creation advances. (`osThreadId`, below, makes the opposite choice for
    /// the opposite reason; see there.)
    let cpuForRotation (rotation : int) (kernel : EmulatedKernel) : CpuId =
        if rotation < 0 then
            failwith
                $"CPU rotation cursor must be non-negative (it counts guest-visible threads created so far); got %d{rotation}"

        let count = effectiveProcessorCount kernel

        // `withProcessorCount` rejects non-positive counts and
        // `effectiveProcessorCount` only ever returns a positive configured
        // value or `kernel.ProcessorCount`, but a kernel built by record-copy
        // can bypass the setter. Assert at the point of use rather than
        // dividing by zero, mirroring what `NativeEnvironment` does before
        // handing the count to the guest.
        if count < 1 then
            failwith
                $"effective ProcessorCount is %d{count}, but must be at least 1 for a simulated thread to be placed on a processor"

        CpuId (rotation % count)

    /// OS thread id policy: the id `thread` reports to the guest through
    /// `SystemNative_TryGetUInt32OSThreadId` and `SystemNative_GetUInt64OSThreadId`.
    ///
    /// The sole producer, and a function of the thread's `ThreadId` — the
    /// interpreter's own allocation counter. Uniqueness across live threads is
    /// the only property anything needs, and `ThreadId`s are already unique and
    /// never reused, so it comes for free; every thread PawPrint creates has
    /// one, guest-visible and interpreter-internal alike, so there is no second
    /// namespace to stay disjoint from.
    ///
    /// Deliberately unlike `cpuForRotation`, which must *not* key off
    /// `ThreadId`. The difference is what the guest can do with the number. A
    /// `CpuId` is drawn from a small cyclic range and is compared against other
    /// threads' (two threads sharing a core is a meaningful, observable fact),
    /// so letting an interpreter-internal allocation shift the rotation would
    /// change guest-observable behaviour. A thread id is opaque: no BCL code
    /// does anything with it but test it for equality — `System.Threading.Lock`
    /// uses it as an owner identity — so *which* number a thread gets is not
    /// observable, only whether two threads share one. Real Linux agrees: its
    /// signal-handling thread is an ordinary `pthread_create` and consumes a
    /// tid like any other, shifting every tid minted after it.
    ///
    /// A negative `ThreadId` is rejected rather than wrapped, because `-1`
    /// would mint exactly the `0` this function exists to avoid. No allocator
    /// produces one (`NextThreadId` counts up from `0`), but `FrameId -1` is an
    /// established sentinel in this codebase, so a `ThreadId -1` is a mistake
    /// someone could plausibly make.
    let osThreadId (thread : ThreadId) : OsThreadId =
        let (ThreadId.ThreadId i) = thread

        if i < 0 then
            failwith
                $"thread id must be non-negative to mint an OS thread id (a negative id would wrap onto the fatal 0, which CoreLib maps to the (uint32)-1 sentinel); got %d{i}"

        // The `+ 1` dodges `0`, which CoreLib's
        // `Lock.ThreadId.InitializeForCurrentThread` (Lock.NonNativeAot.cs) maps
        // to `0xFFFF_FFFF` by decrement — so every thread that minted `0` would
        // end up sharing one id. The other sentinel, the `(uint32)-1` that
        // `TryGetUInt32OSThreadId` returns to mean "this platform cannot determine
        // a thread id", is unreachable by construction: `ThreadId` wraps an `int`,
        // and `Int32.MaxValue + 1` is less than half of `0xFFFF_FFFF`.
        OsThreadId (uint32 i + 1u)



    /// `UnixSystem.changeSocketEventRegistration` — `epoll_ctl(2)` past a
    /// caller's own screens — through this kernel rather than through its POSIX
    /// half.
    ///
    /// Here for the reason `connectSocket`'s and `acceptConnection`'s adapters
    /// are: eleven fixtures call it holding an `EmulatedKernel`.
    let changeSocketEventRegistration
        (portFd : int)
        (targetFd : int)
        (change : SocketEventRegistrationChange)
        (kernel : EmulatedKernel)
        : Result<SocketEventRegistrationAnswer * EmulatedKernel, SocketEventRegistrationRefusal>
        =
        UnixSystem.changeSocketEventRegistration portFd targetFd change (unix kernel)
        |> Result.map (fun (answer, system) -> answer, withUnix system kernel)

    /// `UnixSystem.createSocket` — allocate a fresh socket and a descriptor onto
    /// it — through this kernel rather than through its POSIX half.
    ///
    /// Here for the reason the adapters beside it are: nine fixtures call it
    /// holding an `EmulatedKernel`.
    let createSocket
        (domain : SocketDomain)
        (kind : SocketKind)
        (protocol : SocketProtocol)
        (kernel : EmulatedKernel)
        : int * EmulatedKernel
        =
        let fd, system = UnixSystem.createSocket domain kind protocol (unix kernel)
        fd, withUnix system kernel

    /// The stream the `DIR*` backed by `block` names.
    ///
    /// Total, and loudly partial rather than an option: every `DIR*` a guest can
    /// legally hold came out of `SystemNative_OpenDir` and has not been closed,
    /// and passing anything else to `readdir`/`closedir` is undefined behaviour
    /// on a real libc rather than an error it reports. Inventing EBADF here
    /// would answer a question no kernel answers.
    let directoryStreamId (block : NativeMemoryBlockId) (kernel : EmulatedKernel) : DirectoryStreamId =
        match Map.tryFind block kernel.DirectoryStreamBlocks with
        | Some id -> id
        | None ->
            failwith
                $"EmulatedKernel.directoryStreamId: %O{block} names no open directory stream. The guest passed a DIR* this kernel never handed out, or one it has already closed — both are undefined behaviour on a real libc, which is why there is no errno to report."

    /// The stream `block` names.
    ///
    /// Total, and loudly partial for the same reason as `directoryStreamId`.
    let directoryStream (block : NativeMemoryBlockId) (kernel : EmulatedKernel) : DirectoryStream =
        let id = directoryStreamId block kernel

        match Map.tryFind id kernel.DirectoryStreams with
        | Some stream -> stream
        | None ->
            // Not the guest's doing: `DirectoryStreamBlocks` named this id, so
            // the two maps have drifted apart. `checkInvariants` reports the
            // same state as `DirectoryStreamBlockDangling`.
            failwith
                $"EmulatedKernel.directoryStream: %O{block} names directory stream %O{id}, which the stream table does not hold. This is an interpreter bug: the two maps are maintained together."

    /// Bind `block` — the native block whose address the guest holds as its
    /// `DIR*` — to a stream `UnixSystem.opendir` has just minted.
    ///
    /// The address is PawPrint's half of the stream and the identity is the
    /// library's, so opening one takes both steps. A client that took only this
    /// one, or only the library's, is caught rather than left to drift:
    /// `checkInvariants` refuses a state in which the two maps disagree in
    /// either direction.
    let withDirectoryStreamBlock
        (block : NativeMemoryBlockId)
        (id : DirectoryStreamId)
        (kernel : EmulatedKernel)
        : EmulatedKernel
        =
        { kernel with
            DirectoryStreamBlocks = Map.add block id kernel.DirectoryStreamBlocks
        }

    /// Forget a stream, which `SystemNative_CloseDir` does before closing the
    /// descriptor under it — that order matters, because the close is what
    /// reaps an orphaned directory and this entry is one of the things holding
    /// it.
    let withoutDirectoryStream (block : NativeMemoryBlockId) (kernel : EmulatedKernel) : EmulatedKernel =
        let id = directoryStreamId block kernel

        { kernel with
            DirectoryStreamBlocks = Map.remove block kernel.DirectoryStreamBlocks
            Process =
                { kernel.Process with
                    DirectoryStreams = Map.remove id kernel.DirectoryStreams
                }
        }


    /// `UnixSystem.connectSocket` — `connect(2)` past the caller's own screens
    /// and copy-in faults — through this kernel rather than through its POSIX
    /// half.
    ///
    /// Here for the reason `acceptConnection` below is: six fixtures call it
    /// holding an `EmulatedKernel`, and writing `unix` in and `withUnix` back
    /// out at each would be this function, copied.
    let connectSocket
        (socketId : SocketId)
        (nonBlocking : bool)
        (declaredLength : int)
        (family : int option)
        (destination : InternetEndpoint option)
        (kernel : EmulatedKernel)
        : ConnectOutcome * EmulatedKernel
        =
        let outcome, system =
            UnixSystem.connectSocket socketId nonBlocking declaredLength family destination (unix kernel)

        outcome, withUnix system kernel

    /// `UnixSystem.acceptConnection` — dequeue the oldest completed connection
    /// from `socketId`'s accept queue and materialise the server-side socket
    /// onto it — through this kernel rather than through its POSIX half.
    ///
    /// Here rather than at the call sites because there are ten of them, all in
    /// fixtures that hold an `EmulatedKernel`: writing `unix` in and `withUnix`
    /// back out at each would be this function, copied.
    let acceptConnection (socketId : SocketId) (kernel : EmulatedKernel) : int * TcpConnection * EmulatedKernel =
        let fd, connection, system = UnixSystem.acceptConnection socketId (unix kernel)
        fd, connection, withUnix system kernel

    /// Check that the kernel knows exactly the tasks that `liveThreads` are.
    ///
    /// Separate from `checkInvariants`, and taking the thread set as an argument,
    /// because `EmulatedKernel` compiles before `IlMachineState` and so cannot
    /// reach `ThreadState` to ask. Callers that have both should call both.
    ///
    /// This is what makes `UnixTaskState` total: `Cpu` and `OsThreadId` were
    /// fields on `ThreadState` because a `Map` has no truthful default for an
    /// absent key, and the replacement for that guarantee is that a key is never
    /// absent. Nothing removes a thread today, so this is not a leak check —
    /// it catches a thread created without `registerTask`, and a task minted for
    /// a thread that was never created.
    let checkTaskInvariants
        (liveThreads : Map<ThreadId, ThreadStatus>)
        (kernel : EmulatedKernel)
        : EmulatedKernelDefect list
        =
        // The comparison is the library's; naming the two failures is this
        // kernel's, because `EmulatedKernelDefect` is PawPrint's vocabulary.
        let missing, extra =
            UnixTaskTable.reconcile (liveThreads |> Map.toSeq |> Seq.map fst |> Set.ofSeq) kernel.Tasks

        // The park record and the park status, which are written together and
        // must be cleared together. Stated as an implication plus a bound rather
        // than as an equivalence, because the wake leaves the record standing for
        // the re-entry to find.
        //
        // One block for every parking syscall, which is what one record field and
        // one park status buy: the agreement is about *whether* a thread is parked
        // and *whether* it recorded a park, and neither half needs to know which
        // syscall. Two fields and two statuses needed the rule stating twice, and
        // a fifth syscall would have needed it a fifth time.
        let parkAgreement =
            liveThreads
            |> Map.toList
            |> List.collect (fun (thread, status) ->
                let recorded =
                    match Map.tryFind thread kernel.Tasks with
                    | Some task -> task.Parked
                    | None -> None

                match status, recorded with
                | ThreadStatus.BlockedInSyscall, None -> [ EmulatedKernelDefect.SyscallWaiterWithoutRecord thread ]
                | ThreadStatus.BlockedInSyscall, Some _
                | ThreadStatus.Runnable, _
                | _, None -> []
                | status, Some _ -> [ EmulatedKernelDefect.SyscallRecordWithoutWaiter (thread, status) ]
            )

        (missing |> List.map EmulatedKernelDefect.ThreadWithoutTask)
        @ (extra |> List.map EmulatedKernelDefect.TaskWithoutThread)
        @ parkAgreement

    /// Every way this kernel's tables disagree with each other, including the
    /// POSIX system's own rules: `UnixSystem.checkInvariants` answers those, and
    /// this adds the one thing PawPrint holds that no POSIX kernel does — the
    /// native-heap blocks a guest's `DIR*` values are.
    ///
    /// The descriptor table's own rules are `FileDescriptorRegistry.checkInvariants`,
    /// and the filesystem's are `VirtualFileSystem.checkInvariants`; this
    /// repeats neither. The latter takes a `pinned` argument, which is what
    /// `UnixSystem.pinnedInodes` computes, so a caller wanting the whole picture
    /// pairs this with
    /// `VirtualFileSystem.checkInvariants (UnixSystem.pinnedInodes (unix kernel))`.
    let checkInvariants (kernel : EmulatedKernel) : EmulatedKernelDefect list =
        // Both directions: a `DIR*` naming a stream that is gone would crash the
        // next `readdir`, and a stream no `DIR*` names can never be closed, so it
        // pins its directory through `UnixProcessState.heldInodes` for the rest of the run.
        let directoryStreamBlocks =
            let named = kernel.DirectoryStreamBlocks |> Map.toList |> List.map snd |> Set.ofList

            let dangling =
                kernel.DirectoryStreamBlocks
                |> Map.toList
                |> List.filter (fun (_, id) -> not (Map.containsKey id kernel.DirectoryStreams))
                |> List.map EmulatedKernelDefect.DirectoryStreamBlockDangling

            let unreachable =
                kernel.DirectoryStreams
                |> Map.toList
                |> List.map fst
                |> List.filter (fun id -> not (Set.contains id named))
                |> List.map EmulatedKernelDefect.UnreachableDirectoryStream

            // Injectivity, which neither check above can see: `named` is a set, so
            // two blocks naming one stream collapse into one element and both
            // directions come back clean. `withoutDirectoryStream` removes the
            // stream by id and the block by name, so a second block naming that id
            // would be left dangling by a close it had nothing to do with.
            let namedTwice =
                kernel.DirectoryStreamBlocks
                |> Map.toList
                |> List.groupBy snd
                |> List.filter (fun (_, bindings) -> List.length bindings > 1)
                |> List.map (fun (id, bindings) ->
                    EmulatedKernelDefect.DirectoryStreamNamedTwice (id, bindings |> List.map fst |> List.sort)
                )

            dangling @ unreachable @ namedTwice

        (UnixSystem.checkInvariants (unix kernel) |> List.map EmulatedKernelDefect.System)
        @ directoryStreamBlocks

/// Host-supplied configuration for the simulated process's kernel, applied by
/// `Program.prepare` before any guest code runs.
///
/// This has to be a parameter of `prepare` rather than something a host applies
/// to `PreparedProgram.State` afterwards: `prepare` pumps the entry type's
/// `.cctor`, and several of these values are latched by CoreLib during static
/// initialisation. `Environment.ProcessorCount` is the sharp case — CoreLib
/// declares it as `public static int ProcessorCount { get; } = GetProcessorCount()`
/// (Environment.cs), so the very first read freezes the value for the lifetime
/// of the process and a post-`prepare` record-copy would silently have no
/// effect on a guest that touched it during startup.
///
/// New kernel knobs belong here rather than as further positional parameters on
/// `prepare`/`run`, so that adding one does not churn every call site.
type KernelConfig =
    {
        /// Environment variables overlaid on top of
        /// `EmulatedKernel.defaultEnvironment`. Keys the caller does not set
        /// keep their seeded defaults, so the invariant-globalization switch
        /// survives a caller who supplies an unrelated overlay.
        ///
        /// A name must be non-empty and free of `=`, and neither a name nor a
        /// value may contain a NUL: those are exactly the variables a real
        /// process can have, and applying a config that breaks the rule fails
        /// rather than handing a guest an environment it could not observe on
        /// real .NET. See `UnixProcessState.environmentEntryProblem`.
        Environment : Map<string, string>
        /// Logical processor count the guest observes via
        /// `Environment.ProcessorCount`. Must be at least 1.
        ProcessorCount : int
        /// Greatest value `address + length` may take for a user buffer the
        /// simulated kernel will accept — the machine's `TASK_SIZE_MAX`. Must
        /// be positive. See `EmulatedKernel.UserAddressLimit` for why this is
        /// configuration rather than a property of the platform, and
        /// `ObservedUserAddressLimit` for values real machines have.
        UserAddressLimit : uint64
        /// Virtual time charged per retired IL instruction, in 100 ns ticks — the speed of the
        /// simulated machine. Must be at least 1. See
        /// `EmulatedKernel.InstructionCostTicks` for why this is part of the replay contract,
        /// and `EmulatedKernel.defaultInstructionCostTicks` for the calibration behind the
        /// default of one tick (a 10 MIPS machine).
        InstructionCostTicks : int64
        /// Whether, and how, the driver jitters the virtual clock forward onto
        /// outstanding deadlines. Defaults to `Disabled`; see
        /// `ClockJitterStrategy` for what turning it on buys.
        ///
        /// Configuration rather than something a host installs on
        /// `PreparedProgram.State` afterwards, because class initialisers run
        /// during `prepare` and are as entitled to have their waits jittered as
        /// anything in `Main` — a `.cctor` that starts a thread and waits on it
        /// with a timeout is exactly the shape this strategy exists to test.
        ///
        /// Whatever a host picks becomes part of that run's replay contract.
        ClockJitter : ClockJitterStrategy
        /// Value the guest observes via the internal
        /// `Thread.OptimalMaxSpinWaitsPerSpinIteration`, consulted by
        /// `SpinWait.SpinOnce()` / `LowLevelSpinWaiter` to size each spin
        /// burst passed to `Thread.SpinWait`. Must lie in
        /// `[1, EmulatedKernel.maxOptimalMaxSpinWaitsPerSpinIteration]`. See
        /// `EmulatedKernel.OptimalMaxSpinWaitsPerSpinIteration` for why this
        /// is simulated kernel state rather than a host CPU-timing read, and
        /// `EmulatedKernel.defaultOptimalMaxSpinWaitsPerSpinIteration` for how
        /// the default was derived.
        OptimalMaxSpinWaitsPerSpinIteration : int
        /// Wall-clock reading, in milliseconds since the Unix epoch, that the
        /// simulated process boots at — the instant `DateTime.UtcNow` reports
        /// before the virtual clock has advanced. Must lie in
        /// `[0, UnixMachineState.maxWallClockEpochMs]`. See
        /// `EmulatedKernel.WallClockEpochMs` for why the default of 0 (and
        /// hence a guest that thinks it is 1970) is the honest choice, and note
        /// that whatever a host picks here becomes part of that run's replay
        /// contract: reading the host's real clock to fill it in would make a
        /// recorded trace's timestamps depend on when it was recorded.
        WallClockEpochMs : int64
        /// Unix platform identity the guest observes via
        /// `Environment.OSVersion` (on a Unix CoreLib).
        UnixPlatform : SimulatedUnixPlatform
        /// Current working directory the guest observes via
        /// `Environment.CurrentDirectory`, and against which it resolves every
        /// relative `Path.GetFullPath`. Obtain one with
        /// `AbsoluteUnixPath.parse`; see `EmulatedKernel.CurrentDirectory` for
        /// why this is simulated kernel state rather than a host
        /// `getcwd(3)` read, and note that whatever a host picks here becomes
        /// part of that run's replay contract.
        CurrentDirectory : AbsoluteUnixPath
        /// Path to the executable that started the simulated process, observed
        /// via `Environment.ProcessPath`. Obtain one with `AbsoluteUnixPath.parse`.
        ///
        /// `None` — the default — is an answer rather than a request for one: it
        /// reports that this process has no executable path, which is what
        /// PawPrint modelling no `exec(2)` actually means, and which both Unix
        /// flavours express as a null return with errno `ENOENT`. Contrast
        /// `FileSystemType` above, whose `None` asks `applyTo` to pick a value.
        ///
        /// Not resolved against `FileSystem`: a host that wants
        /// `File.Exists(Environment.ProcessPath)` to hold — which is true on
        /// every real Unix, since `realpath` only succeeds if the path resolves
        /// — must seed that file too. See `EmulatedKernel.ProcessPath` for why
        /// this is simulated kernel state rather than a read of where PawPrint's
        /// own binary sits, and note that whatever a host picks here becomes
        /// part of that run's replay contract.
        ProcessPath : AbsoluteUnixPath option
        /// The filesystem the guest sees, as the entries of its root directory.
        /// A tree rather than a list of paths; see `SeedEntry`. Every inode is
        /// created at `WallClockEpochMs`, so a guest reading an mtime sees the
        /// instant its process booted.
        ///
        /// This, and not any host directory, is the replay input: PawPrint
        /// never reads the real filesystem, so two runs of the same seed see
        /// the same tree whatever the machine.
        FileSystem : Map<FileName, SeedEntry>
        /// Effective user ID the simulated process runs as, observed as every
        /// inode's `st_uid`. See `UnixSystem.defaultUserId` for why the
        /// default is 1000 rather than root.
        UserId : uint32
        /// Effective group ID the simulated process runs as, observed as every
        /// inode's `st_gid`.
        GroupId : uint32
        /// The file-mode creation mask `open(O_CREAT)` applies to the mode its
        /// caller asked for. See `EmulatedKernel.Umask`; it does not affect the
        /// modes `FileSystem` states, which describe a tree this process did not
        /// build.
        Umask : PermissionBits
        /// What `SystemNative_GetFileSystemType` reports for a file on
        /// `FileSystem`; `None` takes whichever filesystem `UnixPlatform`'s
        /// flavour would most honestly mount for an in-memory tree.
        ///
        /// This is configuration rather than something derived from the
        /// flavour because a flavour does not determine a mount's type — one
        /// Linux reports three different numbers for three directories in one
        /// process. It does constrain it, so a value incoherent with
        /// `UnixPlatform` is refused; see `EmulatedFileSystemType`.
        ///
        /// Setting it changes what that one native answers and nothing else.
        /// In particular the emulated filesystem's *behaviour* — its name and
        /// path limits, its creating-open rules — stays the flavour's
        /// throughout, so `Nfs` here buys a kernel that reports NFS, not one
        /// that behaves like a remote filesystem.
        FileSystemType : EmulatedFileSystemType option
        /// Range `bind(2)` draws an ephemeral port from, inclusive at both ends.
        /// See `UnixSystem.defaultEphemeralPortRange`; the low end must not
        /// exceed the high end, and neither may be zero, since port 0 is the
        /// request rather than an answer.
        EphemeralPortRange : uint16 * uint16
        /// The `somaxconn` sysctl, or `None` for the flavour's measured
        /// default (4096 on Linux, 128 on Darwin): the ceiling `listen(2)`
        /// clamps its backlog to. See `UnixMachineState.withSoMaxConn`.
        SoMaxConn : int option
        /// The IPv4 addresses this machine holds, as prefixes. See
        /// `UnixSystem.defaultLocalAddresses`, and note the flavours read one
        /// list differently.
        LocalAddresses : uint32 list
        /// Prefixes this machine has a local route to. Linux binds any address
        /// inside one; Darwin ignores them. See
        /// `UnixSystem.defaultLocalRoutes`.
        LocalRoutes : Ipv4Prefix list
    }

    /// Configuration a host gets if it expresses no preference: no environment
    /// overlay, the default single processor, a wall clock booting at the Unix
    /// epoch, the default Unix platform, and the root as the current directory.
    static member Default : KernelConfig =
        {
            Environment = Map.empty
            ProcessorCount = UnixSystem.defaultProcessorCount
            UserAddressLimit = UnixSystem.defaultUserAddressLimit
            InstructionCostTicks = EmulatedKernel.defaultInstructionCostTicks
            ClockJitter = ClockJitterStrategy.Disabled
            OptimalMaxSpinWaitsPerSpinIteration = EmulatedKernel.defaultOptimalMaxSpinWaitsPerSpinIteration
            WallClockEpochMs = 0L
            UnixPlatform = UnixSystem.defaultUnixPlatform
            CurrentDirectory = UnixSystem.defaultCurrentDirectory
            ProcessPath = UnixSystem.defaultProcessPath
            FileSystem = FileSystemSeed.empty
            UserId = UnixSystem.defaultUserId
            GroupId = UnixSystem.defaultGroupId
            Umask = UnixSystem.defaultUmask
            FileSystemType = None
            EphemeralPortRange = UnixSystem.defaultEphemeralPortRange
            SoMaxConn = None
            LocalAddresses = UnixSystem.defaultLocalAddresses
            LocalRoutes = UnixSystem.defaultLocalRoutes
        }

[<RequireQualifiedAccess>]
module KernelConfig =
    /// Apply a host configuration to a freshly-minted kernel. Each field is
    /// applied through its own `EmulatedKernel` setter, so the validation those
    /// setters perform (e.g. rejecting a non-positive processor count) also
    /// guards the configuration path.
    let applyTo (config : KernelConfig) (kernel : EmulatedKernel) : EmulatedKernel =
        kernel
        |> EmulatedKernel.mapProcess (UnixProcessState.withEnvironment "KernelConfig.Environment" config.Environment)
        |> EmulatedKernel.mapMachine (UnixMachineState.withProcessorCount config.ProcessorCount)
        |> EmulatedKernel.mapMachine (UnixMachineState.withUserAddressLimit config.UserAddressLimit)
        |> EmulatedKernel.withInstructionCostTicks config.InstructionCostTicks
        |> EmulatedKernel.withClockJitter config.ClockJitter
        |> EmulatedKernel.withOptimalMaxSpinWaitsPerSpinIteration config.OptimalMaxSpinWaitsPerSpinIteration
        |> EmulatedKernel.mapMachine (UnixMachineState.withWallClockEpochMs config.WallClockEpochMs)
        |> EmulatedKernel.mapMachine (
            UnixMachineState.withUnixPlatformAndFileSystemType config.UnixPlatform config.FileSystemType
        )
        |> EmulatedKernel.mapProcess (UnixProcessState.withProcessPath "KernelConfig.ProcessPath" config.ProcessPath)
        |> EmulatedKernel.withFileSystemAndCurrentDirectory
            config.UnixPlatform
            (UnixTimestamp.ofMillisecondsSinceEpoch config.WallClockEpochMs)
            config.FileSystem
            config.CurrentDirectory
        |> EmulatedKernel.mapProcess (UnixProcessState.withUserAndGroupId config.UserId config.GroupId)
        |> EmulatedKernel.mapMachine (UnixMachineState.withEphemeralPortRange config.EphemeralPortRange)
        |> EmulatedKernel.mapMachine (UnixMachineState.withSoMaxConn config.UnixPlatform config.SoMaxConn)
        |> EmulatedKernel.mapMachine (UnixMachineState.withLocalAddresses config.LocalAddresses config.LocalRoutes)
        |> EmulatedKernel.mapProcess (UnixProcessState.withUmask "KernelConfig.Umask" config.Umask)

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
    /// `EmulatedKernel.withVirtualClockTicks`, which raises it naming the wait
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
    /// caller can hand it straight to `EmulatedKernel.withVirtualClockTicks`.
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

/// One entry in `EmulatedKernel.OutputLog`: the role the guest targeted (a
/// writable standard stream — stdout or stderr) and the byte payload of
/// that single `SystemNative_Write` call. Chunks are not coalesced across
/// calls because guest write boundaries matter for diagnostics (line
/// boundaries, prompt boundaries) and for matching real-CLR observability.
type OutputLogEntry =
    {
        Role : FileDescriptorRole
        Bytes : ImmutableArray<byte>
    }

[<RequireQualifiedAccess>]
module OutputLogEntry =
    /// Concatenate every entry in `log` whose `Role` matches `role`,
    /// preserving the original write order. Used by tests that want to
    /// assert on the cumulative bytes the guest sent to a specific
    /// standard stream (the equivalent of capturing one of host
    /// stdout/stderr in isolation).
    let bytesFor (role : FileDescriptorRole) (log : ImmutableArray<OutputLogEntry>) : ImmutableArray<byte> =
        let builder = ImmutableArray.CreateBuilder<byte> ()

        for entry in log do
            if entry.Role = role then
                builder.AddRange (entry.Bytes : ImmutableArray<byte>)

        builder.ToImmutable ()

/// One TCP connection, as the emulated kernel's connection table holds it.
///
/// Keyed by `ConnectionId` and holding only the two endpoints' addresses.
/// Deliberately no references back to the sockets on its ends: a connection
/// outlives the client that opened it (measured: close the client while its
/// connection sits in an accept queue, and `accept(2)` still returns it), and
/// the server end has no socket at all until that accept, so an end-to-socket
/// field would spend most of its life dangling or `None`. Cleanup instead
/// scans the socket table for references, which `EmulatedKernel.closeFd`
/// does.
type TcpConnection =
    {
        /// The connecting side's address — what `accept(2)` reports as the
        /// peer.
        ClientAddress : InternetEndpoint
        /// The accepted side's address: the destination the client connected
        /// to, with a wildcard destination already rewritten to loopback. The
        /// accepted socket's own `getsockname(2)` reports this.
        ServerAddress : InternetEndpoint
    }

/// One thread's in-flight `SystemNative_WaitForSocketEvents` call: the state
/// the syscall captured when it was entered, which outlives anything the
/// guest does to its arguments afterwards. The port is held by *description
/// identity*, exactly as the real syscall holds a file reference — closing
/// the fd the wait was called through changes nothing, because the fd is
/// never consulted again.
type ParkedSocketWait =
    {
        /// The open file description of the port being waited on.
        Port : OpenFileDescriptionId
        /// The `*count` read at entry. A real `epoll_wait` keeps using the
        /// maxevents it was passed even if the guest overwrites the cell
        /// mid-wait.
        MaxEvents : int
    }

/// Aggregates the slice of `IlMachineState` that models host-kernel /
/// syscall-emulation state: the per-thread last-error registers, the native
/// heap pool backing `Marshal.AllocHGlobal`, the Unix file-descriptor table,
/// the `LowLevelMonitor` registry, and monotonic ID counters for opaque
/// kernel handles. These are the pieces of interpreter state that exist
/// because PawPrint refuses to use the host kernel; they don't belong in the
/// CIL execution model proper.
///
/// Pulling them into a sub-record keeps `IlMachineState` from sprawling and
/// makes it possible to swap the kernel implementation (e.g. for a Windows-
/// shaped emulation) without disturbing the rest of the state model.
/// One open directory stream: what `opendir(3)` returns and `readdir`/`closedir`
/// consume.
///
/// Held in `EmulatedKernel.DirectoryStreams` rather than on the descriptor,
/// because libc keeps a `DIR`'s buffer and position in userspace and the
/// descriptor carries only the kernel's. The consequence is that two `opendir`s
/// of one directory advance independently, and a `dup` of the descriptor would
/// not share the cursor. Unobservable: `dirfd` appears nowhere in CoreLib or
/// the PAL, so no managed caller can reach the descriptor to `dup` it.
/// Identity of one open directory stream. Never guest-visible: a guest holds a
/// `DIR*`, and what that pointer is made of is the client's business, not this
/// kernel's.
///
/// Minted monotonically and never reused, as `SocketId` and `InodeNumber` are;
/// `EmulatedKernel.NextDirectoryStreamId` is the counter, and `checkInvariants`
/// refuses a table holding an id at or above it.
[<Struct>]
type DirectoryStreamId =
    | DirectoryStreamId of value : int64

    override this.ToString () : string =
        match this with
        | DirectoryStreamId value -> string<int64> value

type DirectoryStream =
    {
        /// The descriptor `opendir` opened, closed again by `closedir`.
        Fd : int
        /// The directory being enumerated. Also reachable through `Fd`, but
        /// held directly so that a guest which closed that descriptor behind the
        /// stream's back — undefined behaviour on a real libc, and possible here
        /// because fd numbers are guessable — does not turn into an interpreter
        /// crash.
        Inode : InodeNumber
        /// How far through `Inode` this stream has read.
        Cursor : DirectoryCursor
    }

/// The kernel-image facts a POSIX simulator owns: the platform it is
/// impersonating, its filesystem, its clock and entropy, its network
/// configuration and socket table, and the two numbers a process reads back
/// about the machine it is running on.
///
/// Everything here is state a second client of a POSIX simulator would also
/// have; nothing in it is a CLR concept. Held flat for now — the target shape
/// groups the clock, entropy, network and socket fields into records of their
/// own, which is a change internal to this type once `EmulatedKernel`'s
/// forwarding members exist.
type UnixMachineState =
    {
        /// Every socket the simulated process owns, by identity.
        ///
        /// Separate from `FileDescriptors` because a socket's lifetime is not
        /// a descriptor's: an `OpenFileTarget.Socket` holds only the
        /// `SocketId`, and this is what it names. Every entry does have
        /// exactly one description naming it, and
        /// `EmulatedKernel.checkInvariants` enforces that — a connection
        /// awaiting `accept(2)` is a `TcpConnection` in `Connections`, not a
        /// socket, precisely so this rule can stay strict.
        Sockets : Map<SocketId, SocketDescription>
        /// Every TCP connection the simulated kernel holds: established ends
        /// referenced from a socket's `SocketPhase`, and completed
        /// connections waiting in some listener's accept queue. An entry is
        /// removed when nothing references it any more (`closeFd`).
        Connections : Map<ConnectionId, TcpConnection>
        /// The identity the next completed connect will allocate. Monotonic
        /// and never reused, for the same replay-trace reason as
        /// `NextSocketId`.
        NextConnectionId : ConnectionId
        /// The ordinal the next committed socket event registration records
        /// as its `RegisteredAt`. Monotonic, and bumped only when an ADD
        /// commits, so a failed `epoll_ctl` leaves the kernel exactly as it
        /// found it.
        NextSocketEventRegistrationOrdinal : int64
        /// The port a `bind(2)` of port 0 will try first.
        ///
        /// A counter rather than a draw from the seeded PRNG. Which port an
        /// ephemeral bind picks is unspecified — Linux randomises within its
        /// range and Darwin ascends — so PawPrint owes a guest only *a* free
        /// port, and a trace whose ports read 32768, 32769, 32770 is far easier
        /// to follow than one whose ports are scattered. Nothing guest-visible
        /// may depend on the value; `SocketBindListen.cs` asserts only that it is
        /// non-zero and unprivileged, which is all the two real kernels agree on.
        NextEphemeralPort : uint16
        /// Range `NextEphemeralPort` sweeps, inclusive at both ends. Host
        /// configuration; see `EmulatedKernel.defaultEphemeralPortRange`.
        EphemeralPortRange : uint16 * uint16
        /// The value of the `somaxconn` sysctl (`net.core.somaxconn` on
        /// Linux, `kern.ipc.somaxconn` on Darwin): the ceiling `listen(2)`
        /// clamps its backlog to before the accept-queue capacity is derived.
        /// Host configuration with a per-flavour default; see
        /// `EmulatedKernel.withSoMaxConn` for the measured clamp rules.
        SoMaxConn : int
        /// The IPv4 addresses this machine holds. Host configuration; see
        /// `EmulatedKernel.defaultLocalAddresses`.
        LocalAddresses : uint32 list
        /// Prefixes this machine has a local route to, which Linux will bind any
        /// address inside and Darwin ignores. See
        /// `EmulatedKernel.defaultLocalRoutes`.
        LocalRoutes : Ipv4Prefix list
        /// The identity the next `SystemNative_Socket` will allocate.
        ///
        /// Monotonic, and never reused: nothing guest-visible reports a
        /// `SocketId`, but a replay trace does, and reuse would make two
        /// distinct sockets indistinguishable in it. `NextLowLevelMonitorId`
        /// is stored beside its table for the same reason.
        NextSocketId : SocketId
        /// Deterministic virtual clock the simulated process observes, in
        /// monotonic milliseconds-since-boot. Read by
        /// `SystemNative_GetLowResolutionTimestamp` (the PAL backing
        /// `Environment.TickCount64` on Unix) and intended to be the single
        /// source of truth for every elapsed-time computation the guest
        /// performs. `SystemNative_GetSystemTimeAsTicks` (the wall clock
        /// behind `DateTime.UtcNow`) derives from it via
        /// `EmulatedKernel.systemTimeAsTicks`, and
        /// `SystemNative_GetTimestamp` (the high-resolution clock behind
        /// `Stopwatch`) derives from it via
        /// `EmulatedKernel.monotonicTimestampNanos` — rather than either
        /// maintaining a parallel clock.
        ///
        /// Denominated in 100 ns ticks — `DateTime`'s own quantum — so that
        /// `DateTime.UtcNow` needs no scaling and `Stopwatch` resolves finer
        /// than a millisecond. The driver loop advances it by
        /// `InstructionCostTicks` each time it increments `StepCounter`; see
        /// that constant for the rate and what it means as a machine speed.
        ///
        /// Elapsed-time polling loops such as `while (TickCount64 - start &lt; N)`
        /// therefore terminate in `N * ticksPerMillisecond / InstructionCostTicks`
        /// scheduler ticks, which is the cost to keep in mind when choosing the
        /// rate: it buys sleep fidelity and is paid for in run length.
        ///
        /// Reading the field never mutates it: the BCL's `TickCount64`
        /// observers stay pure, and the consistency property "two threads
        /// reading on the same tick observe the same value" falls out of
        /// the scheduler being the sole writer. *Not* derived
        /// from `StepCounter`: the driver's deadline jump moves the clock
        /// forward to the next deadline when no thread is Runnable, and
        /// that jump must not require a matching jump in `StepCounter`
        /// (which would skew the spurious-wakeup schedule).
        VirtualClockTicks : int64
        /// Wall-clock time, in milliseconds since the Unix epoch, that the
        /// simulated process boots at — i.e. the wall-clock reading that
        /// corresponds to `VirtualClockTicks = 0`. The realtime clock the guest
        /// observes is the affine image of the monotonic one:
        /// `systemTimeAsTicks = (WallClockEpochMs + VirtualClockTicks) * 10_000`.
        ///
        /// Deliberately *not* a second mutable clock advanced alongside
        /// `VirtualClockTicks`. A parallel field would be behaviourally identical
        /// today while silently drifting out of step the first time someone
        /// adds a new way for the monotonic clock to advance (the driver's
        /// deadline jump is exactly such a path) and forgets to update both.
        /// The cost is that the two clocks cannot diverge — real
        /// `CLOCK_REALTIME` can step backwards under NTP correction or
        /// `date -s`, and guest code that computes a duration as
        /// `DateTime.UtcNow - start` and assumes the result is non-negative is
        /// a real bug class. Modelling that means promoting this field to a
        /// mutable clock plus a scriptable skew strategy in the shape of
        /// `SpuriousWakeupStrategy`; it is deliberately deferred until there
        /// is a guest bug to hunt, and this field's arithmetic survives the
        /// change unaltered.
        ///
        /// Defaults to 0, so a default run reports a `DateTime.UtcNow` a few
        /// milliseconds after 1970-01-01T00:00:00Z. That is chosen precisely
        /// because it looks wrong to a human: a timestamp in a PawPrint trace
        /// is synthetic, and a plausible-looking "today" would invite someone
        /// to read meaning into it. Hosts that want the guest to run in a more
        /// conventional date regime set `KernelConfig.WallClockEpochMs`; that
        /// value is then part of the run's replay contract, exactly like the
        /// PRNG seeds.
        ///
        /// Must lie in `[0, maxWallClockEpochMs]`: CoreLib builds the result
        /// with `DateTime`'s *unvalidated* private ctor
        /// (`new DateTime(((ulong)(GetSystemTimeAsTicks() + UnixEpochTicks)) | KindUtc)`
        /// in DateTime.Unix.cs), so an out-of-range value would reach the guest
        /// as a silently corrupt `DateTime` rather than an exception.
        WallClockEpochMs : int64
        /// Deterministic state for the splitmix64 PRNG that backs
        /// `SystemNative_GetNonCryptographicallySecureRandomBytes`. Real
        /// CoreCLR fills this buffer from `arc4random_buf` /
        /// `BCryptGenRandom` / `/dev/urandom`; PawPrint refuses host
        /// entropy because the whole point of the runtime is bit-for-bit
        /// reproducibility. A seeded PRNG is the closest deterministic
        /// substitute that still survives downstream consumers: the BCL's
        /// `Random()` ctor retries until it sees a non-zero seed, so a
        /// constant-zero substitute would hang at construction time.
        NonCryptoRandomState : uint64
        /// Deterministic state for the splitmix64 PRNG that backs
        /// `SystemNative_GetCryptographicallySecureRandomBytes` — the entry
        /// point `Guid.NewGuid` draws its 16 bytes from on Unix, and the one
        /// CoreLib's `Interop.GetCryptographicallySecureRandomBytes` wrapper
        /// turns into a `CryptographicException` on any non-zero return.
        /// PawPrint substitutes the same seeded PRNG it uses for the
        /// non-crypto entry point: the output is emphatically *not*
        /// cryptographically secure, but nothing inside a deterministic
        /// interpreter can be, and reproducibility is the property this
        /// runtime exists to provide. Guests that need real entropy must not
        /// run under PawPrint.
        ///
        /// Deliberately a *separate* stream from `NonCryptoRandomState`,
        /// per the guidance on `NonCryptoRandom`: sharing one state would
        /// make an added `new Random()` (or any other non-crypto consumer)
        /// silently shift every subsequent `Guid.NewGuid`, which is exactly
        /// the kind of spooky action at a distance that makes a recorded
        /// trace hard to reason about. Seeded from a constant distinct from
        /// `NonCryptoRandom.initialState` so the two streams do not emit
        /// identical byte sequences.
        CryptoRandomState : uint64
        /// Number of logical processors the simulated process observes, as
        /// reported by `Environment.ProcessorCount`. Deliberately a value in
        /// kernel state rather than a host read: real CoreCLR answers this
        /// from `GetSystemInfo` / `sched_getaffinity`, which would make a
        /// replay depend on the machine that produced it. Guests size thread
        /// pools, partition `Parallel.For` ranges, and stripe arrays off this
        /// number, so letting the host leak in here would change guest
        /// *control flow* between runs — the single worst kind of
        /// nondeterminism for a runtime whose purpose is bit-for-bit replay.
        ///
        /// Defaults to 1 (see `EmulatedKernel.initial`); hosts choose a
        /// different value via `KernelConfig.ProcessorCount`, which
        /// `Program.prepare` applies before the entry type's `.cctor` is
        /// pumped — CoreLib latches `Environment.ProcessorCount` into a static
        /// on first read, so a later change would not be observed.
        ///
        /// Must be >= 1: the real property is documented as always positive
        /// and BCL callers divide by it, so `NativeEnvironment` asserts the
        /// invariant at the point of use rather than trusting construction.
        ProcessorCount : int
        /// Greatest value `address + length` may take for a user buffer the
        /// kernel will accept — the machine's `TASK_SIZE_MAX`. Consulted only
        /// where `SimulatedUnixPlatform.screensUserBufferUpFront` says the
        /// kernel screens before performing the operation, but a real fact
        /// about every machine regardless.
        ///
        /// Configuration rather than a constant derived from the platform
        /// because it varies by *machine*: 2^47 less a page with four-level
        /// paging on x86-64, 2^56 less a page with five-level, 2^48 on a
        /// 48-bit-VA arm64. Two GitHub runners of the same image were measured
        /// disagreeing, so no value derived from the flavour or the kernel
        /// release could be right everywhere. See `ObservedUserAddressLimit`
        /// for the values real machines have been seen to have.
        UserAddressLimit : uint64
        /// Unix-shaped platform identity the simulated process reports, as
        /// observed through `SystemNative_GetUnixRelease` (and hence
        /// `Environment.OSVersion` on a Unix CoreLib).
        ///
        /// Unlike `ProcessorCount`, CoreLib does *not* latch this during
        /// static initialisation — `Environment.OSVersion` is a lazily
        /// populated static that is only computed on first read — but hosts
        /// should still set it via `KernelConfig` rather than by record-copy
        /// after startup, so that the value is fixed for the whole run and a
        /// guest cannot observe it changing under it.
        UnixPlatform : SimulatedUnixPlatform
        /// The simulated process's filesystem: every inode a guest can reach
        /// through the `SystemNative_*` path calls.
        ///
        /// Seeded from `KernelConfig.FileSystem`, and mutated in place by the
        /// natives that write, create or truncate. It is emulated kernel state
        /// rather than anything the interpreter reads from the host, for the
        /// usual reason:
        /// a filesystem read from the host would make a replay depend on the
        /// machine that produced it, and guests branch on what they find.
        FileSystem : VirtualFileSystem
        /// The filesystem `FileSystem` claims to be, which is the whole of what
        /// `SystemNative_GetFileSystemType` reports for a file on it.
        ///
        /// Seeded from `KernelConfig.FileSystemType` and fixed for the run: no
        /// syscall in CoreLib's interop surface can mount anything, so nothing
        /// a guest does can change it. Set only by
        /// `withUnixPlatformAndFileSystemType`, which writes it and
        /// `UnixPlatform` together so that the two cannot disagree.
        FileSystemType : EmulatedFileSystemType
    }

type EmulatedKernel =
    {
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
        /// In-memory model of the simulated process's Unix file descriptor
        /// table. Pre-seeded at startup with stdin (0), stdout (1), stderr
        /// (2), matching the kernel's behaviour of populating these slots
        /// at `exec` time. SystemNative_Dup / Close / Read / Write etc.
        /// route through this table; the host's real fds are never used.
        FileDescriptors : FileDescriptorRegistry
        /// Each thread's in-flight `SystemNative_WaitForSocketEvents`
        /// call, stored at park and removed at delivery — so an absent key
        /// means a first entry into the wait, and a present one means the
        /// handler is being re-entered and must use the captured state
        /// instead of re-decoding its arguments. Present from the park
        /// through the wake to the delivering re-entry, which is a strict
        /// superset of the window `BlockedOnSocketEvents` covers: the
        /// close-time retention check reads this, not the thread status,
        /// so the woken-but-not-yet-run window is protected too.
        ParkedSocketWaits : Map<ThreadId, ParkedSocketWait>
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
        /// Ordered, append-only log of every write the guest has performed
        /// against a writable standard stream via `SystemNative_Write`.
        /// Each entry carries the destination `Role` and the exact byte
        /// payload of that one call (chunks are not coalesced; ordering
        /// across roles is preserved). Acts as the canonical record the
        /// driver's end-of-run host drain reads from, and is what
        /// PawPrint-only tests assert on instead of trying to capture host
        /// stdout. The log grows unboundedly: a guest that prints
        /// gigabytes will pay the memory cost, but PawPrint is a slow
        /// deterministic interpreter and a guest of that scale is not in
        /// scope. Bound this with a streaming sink (consuming `StepEffect.
        /// WroteToFd` at each step) when a need arises.
        ///
        /// The single ordered log (rather than per-stream buffers)
        /// preserves cross-stream ordering: a guest that writes
        /// `err1, out1, err2` is replayed in that order under `2>&1`,
        /// matching real-CLR behaviour. Per-stream views are derived in
        /// `OutputLogEntry.bytesFor`.
        OutputLog : ImmutableArray<OutputLogEntry>
        /// Simulated process environment variable table, and the analogue of the
        /// Unix PAL's `palEnvironment` — which is likewise a snapshot taken once
        /// at startup rather than a view of the host, because libc's `setenv` is
        /// not usable concurrently. Consulted by
        /// `Environment.GetEnvironmentVariable` through the Win32
        /// `GetEnvironmentVariableW` shim, and flattened into an environment
        /// block by the `GetEnvironmentStringsW` shim that backs
        /// `Environment.GetEnvironmentVariables`. Seeded with
        /// `DOTNET_SYSTEM_GLOBALIZATION_INVARIANT=1` so guest BCL code that
        /// reads it during startup gets the invariant-globalization mode
        /// PawPrint requires; the CLI overlays the host process's env on top
        /// of this default at startup, and tests can pass their own overlay
        /// via `Program.run`.
        ///
        /// No guest can write to this: PawPrint services no
        /// `SetEnvironmentVariableW`, so `Environment.SetEnvironmentVariable`
        /// aborts loudly rather than mutating the table.
        ///
        /// Every name here is one a real process could hold: non-empty, free of
        /// `=`, and free of NUL, as is every value. `EmulatedKernel
        /// .withEnvironment` — the only way an entry enters the table — rejects
        /// anything else, so readers may rely on it. See
        /// `environmentEntryProblem` for why those are exactly the expressible
        /// names.
        ///
        /// That invariant is what makes `GetEnvironmentVariableW`'s plain
        /// `Map.tryFind` faithful without reproducing the PAL's own two
        /// name guards: for a name the PAL would refuse, the lookup misses and
        /// reports `ERROR_ENVVAR_NOT_FOUND`, which is exactly what the PAL
        /// returns on that path.
        Environment : Map<string, string>
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
        /// The simulated process's current working directory, as observed
        /// through `SystemNative_GetCwd` — and hence through
        /// `Environment.CurrentDirectory` and every relative
        /// `Path.GetFullPath` on a Unix CoreLib.
        ///
        /// Like `UnixPlatform`, CoreLib does *not* latch this during static
        /// initialisation (`Interop.Sys.GetCwd()` is called afresh on every
        /// read), but hosts should still set it via `KernelConfig` rather than
        /// by record-copy after startup: PawPrint models no `chdir(2)`, so
        /// within a run the cwd is immutable and a guest must not be able to
        /// observe it changing under it.
        ///
        /// The **physical** path: every symlink resolved away, which is what
        /// `getcwd(3)` reports and so not necessarily the spelling
        /// `KernelConfig.CurrentDirectory` used. Derived from
        /// `CurrentDirectoryInode` when the kernel is built, so the two cannot
        /// describe a process no Unix could produce.
        CurrentDirectory : AbsoluteUnixPath
        /// The directory relative paths resolve against: the inode the
        /// simulated process holds its current directory *open on*, which is
        /// what a real process holds rather than a name it re-walks.
        ///
        /// Derived when the kernel is built, by the one setter that takes the
        /// current directory and the filesystem together — so this is not a
        /// second, independent knob a host may set. It is nonetheless the
        /// *identity* half of the pair, and the two answer
        /// different questions once a guest can delete a directory: this one
        /// says where a relative path starts, and `CurrentDirectory` says what
        /// the process would be told if it asked. A real kernel splits them the
        /// same way, which is why `getcwd` can fail while relative lookups
        /// still work.
        ///
        /// Holding the inode is also what makes the resolution of a relative
        /// path *not* a lookup: no component of the current directory's own
        /// path is walked, so none of its permission bits are consulted, and no
        /// intermediate symlink is re-traversed. Measured on both kernels: with
        /// the cwd at `outer/inner` and `outer` unsearchable, a relative
        /// `lstat("target")` succeeds while `lstat("../inner/target")` is
        /// EACCES.
        CurrentDirectoryInode : InodeNumber
        /// Path to the executable that started the simulated process, as
        /// observed through `SystemNative_GetProcessPath` and hence
        /// `Environment.ProcessPath`.
        ///
        /// `None` is an *answer*, not a request for a default: it says this
        /// process has no executable path, which the entry point reports the way
        /// both Unix flavours do — a null return with errno `ENOENT`. That is
        /// the truth about a PawPrint guest by default, because PawPrint models
        /// no `exec(2)`: nothing started this process from a file, and the
        /// emulated filesystem contains no image of it. Contrast
        /// `FileSystemType`, whose `None` *does* mean "derive one in `applyTo`".
        ///
        /// Not resolved against `FileSystem`. Real `realpath` succeeds only if
        /// every component resolves, so a host that wants
        /// `File.Exists(Environment.ProcessPath)` to hold must seed the file
        /// itself; see docs/divergences.md. The same is already true of
        /// `CurrentDirectory`.
        ///
        /// CoreLib latches this on first read — `Environment.ProcessPath` caches
        /// under an `Interlocked.CompareExchange` — so hosts must set it via
        /// `KernelConfig` rather than by record-copy after startup.
        ProcessPath : AbsoluteUnixPath option
        /// Every directory stream `SystemNative_OpenDir` has handed out and
        /// `SystemNative_CloseDir` has not yet reclaimed, under the id minted for
        /// it. `DirectoryStreamBlocks` is what turns a guest's `DIR*` into one of
        /// these ids.
        ///
        /// A stream is *not* a descriptor kind. Measured on both kernels,
        /// `opendir` consumes a file descriptor — an `open` either side of one
        /// returned fds 3 and 5 — so the descriptor is an ordinary
        /// `OpenFileTarget.File` on the directory, which is what pins the inode
        /// through `heldInodes` and so makes a stream over an `rmdir`'d
        /// directory behave. What cannot live there is the rest: the cursor and
        /// the name buffer have no home in `File (inode, offset)`.
        ///
        /// An absent key is not a default and must never be read as one. Every id
        /// `DirectoryStreamBlocks` names is present here — `checkInvariants`
        /// enforces that as `DirectoryStreamBlockDangling` — so an absent one is
        /// an interpreter bug rather than anything a guest did, and
        /// `directoryStream` says so loudly rather than inventing an errno, the
        /// way `EmulatedKernel.connection` does for a `ConnectionId`.
        DirectoryStreams : Map<DirectoryStreamId, DirectoryStream>
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
        /// The two are maintained together by `withNewDirectoryStream` and
        /// `withoutDirectoryStream`, and `checkInvariants` refuses a state in
        /// which they disagree in either direction.
        DirectoryStreamBlocks : Map<NativeMemoryBlockId, DirectoryStreamId>
        /// The id `withNewDirectoryStream` will hand out next.
        NextDirectoryStreamId : DirectoryStreamId
        /// The effective user ID the simulated process runs as, reported by
        /// `stat` as every inode's `st_uid` and by `SystemNative_GetEUid`.
        ///
        /// Process-wide rather than per-inode: no managed caller can change a
        /// file's owner, because `SystemNative_ChOwn` does not exist anywhere in
        /// the runtime's interop surface, so a per-inode field could never make
        /// two inodes differ and would carry no information this does not.
        UserId : uint32
        /// The effective group ID, reported as every inode's `st_gid`. See
        /// `UserId`.
        GroupId : uint32
        /// The simulated process's file-mode creation mask: the permission bits
        /// `open(O_CREAT)` clears from the mode its caller asked for.
        ///
        /// Process state rather than filesystem state, and immutable for the
        /// whole run: CoreLib's interop surface has no `SystemNative_UMask` at
        /// all, so no guest can read or change it, and a host that wants to
        /// replay a differently-masked process sets it once through
        /// `KernelConfig`.
        ///
        /// Deliberately *not* consulted for seed entries. A seed describes a
        /// tree that some other process built, so this run's mask has no bearing
        /// on it; `PermissionBits.defaultForRegularFile` shares the same 0o022
        /// literal but is not derived from this field, so raising the mask
        /// cannot silently change what an unannotated seed entry means.
        Umask : PermissionBits
        /// Pure data model of the simulated process's signal disposition,
        /// per-thread sigprocmasks, and pending-signal queue. Populated by
        /// future slices: nothing in the simulator dispatches signals yet,
        /// so the field stays at `SignalState.empty` across every run today.
        /// Held on `EmulatedKernel` (rather than per-thread) because POSIX
        /// signal disposition is process-wide; the per-thread piece lives
        /// inside `SignalState.Blocked`.
        Signals : SignalState<ThreadId, SignalHandler>
    }

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

/// A way the emulated kernel's socket table and its descriptor table could
/// disagree — a state no kernel could be in, and which `EmulatedKernel` exists
/// to keep unreachable.
///
/// Separate from `FileDescriptorRegistryDefect` because these are claims about
/// two tables at once, and `FileDescriptorRegistry` cannot see the socket table:
/// it is defined in a file that compiles before this one.
[<RequireQualifiedAccess>]
type EmulatedKernelDefect =
    /// A live open file description names a socket the socket table does not
    /// hold, so resolving that descriptor would fail.
    | DanglingSocket of description : OpenFileDescriptionId * socket : SocketId
    /// The socket table holds a socket no live description names.
    ///
    /// A leak, and deliberately a defect rather than a tolerated state: every
    /// way to make a socket — `SystemNative_Socket`, or `SystemNative_Accept`
    /// materialising a queued connection — hands back a descriptor at once,
    /// so an unreferenced socket means a close forgot to clean up. A
    /// connection awaiting accept is a `TcpConnection`, not a socket, which
    /// is what lets this rule stay strict.
    | UnreferencedSocket of socket : SocketId
    /// A socket in the table has an identity at or above the next one to
    /// allocate, so a future `socket(2)` would mint a duplicate.
    | NextSocketIdNotFresh of nextSocketId : SocketId * existing : SocketId
    /// `CurrentDirectoryInode` names something the filesystem does not hold, or
    /// holds as something other than a directory — so every relative path a
    /// guest passes would resolve from a place that is not a directory.
    ///
    /// Deliberately *not* "the inode is reachable from the root": a real process
    /// keeps its current directory alive after the last name for it has gone,
    /// and PawPrint's held inode is what expresses that.
    | CurrentDirectoryIsNotADirectory of inode : InodeNumber
    /// A live open file description names an inode the filesystem does not
    /// hold, so reading or `fstat`ing that descriptor would fail.
    ///
    /// The mirror image of `VirtualFileSystemDefect.UnreachableFromRoot`: that
    /// one catches an orphan nothing holds, and this one catches an inode freed
    /// while something still held it. Between them they bracket the reaping
    /// rule, so a `VirtualFileSystem.forget` that fires too late is caught there
    /// and one that fires too early is caught here.
    | DanglingOpenInode of description : OpenFileDescriptionId * inode : InodeNumber
    /// An open directory stream names an inode the filesystem no longer holds.
    ///
    /// Unreachable by construction — `heldInodes` counts a stream's inode among
    /// the things pinning it, so `forgetIfUnheld` cannot free one out from under
    /// a stream — which is exactly why a violation is an interpreter bug rather
    /// than something a guest did. The next `readdir` would crash the
    /// interpreter, and this names the cause instead.
    | DanglingDirectoryStreamInode of stream : DirectoryStreamId * inode : InodeNumber
    /// An open directory stream names an inode that is not a directory.
    | DirectoryStreamIsNotADirectory of stream : DirectoryStreamId * inode : InodeNumber
    /// The stream table holds an id at or above `NextDirectoryStreamId`, so the
    /// next `opendir` would hand out an id that is already in use.
    | NextDirectoryStreamIdNotFresh of nextDirectoryStreamId : DirectoryStreamId * existing : DirectoryStreamId
    /// A guest-held `DIR*` names a stream the stream table does not hold, so the
    /// next `readdir` through it would crash rather than enumerate.
    | DirectoryStreamBlockDangling of block : NativeMemoryBlockId * stream : DirectoryStreamId
    /// The stream table holds a stream no `DIR*` names, so nothing can ever
    /// read or close it and the directory it pins is held for the run.
    | UnreachableDirectoryStream of stream : DirectoryStreamId
    /// More than one `DIR*` names one stream, so closing either would take the
    /// stream out from under the others. Two `opendir`s owe the guest
    /// independent cursors, so this is never a state a stream table should hold.
    | DirectoryStreamNamedTwice of stream : DirectoryStreamId * blocks : NativeMemoryBlockId list
    /// `CurrentDirectory` is not the path that reaches `CurrentDirectoryInode`,
    /// so `getcwd` would report a directory the process is not in.
    ///
    /// Only raised while the inode still *has* a path: one held open after its
    /// last name has gone has none, and a real `getcwd` fails there rather than
    /// answering.
    | CurrentDirectoryPathDisagrees of stored : AbsoluteUnixPath * physical : AbsoluteUnixPath
    /// A socket's phase references a connection the connection table does not
    /// hold.
    | DanglingConnection of socket : SocketId * connection : ConnectionId
    /// A listener's accept queue references a connection the connection table
    /// does not hold.
    | DanglingQueuedConnection of listener : SocketId * connection : ConnectionId
    /// The connection table holds a connection no socket phase and no accept
    /// queue references — a leak `closeFd`'s sweep should have caught.
    | OrphanConnection of connection : ConnectionId
    /// One connection sits in two accept-queue slots (in one queue or two),
    /// so accepting it twice would materialise two sockets onto one
    /// connection.
    | DuplicateQueuedConnection of connection : ConnectionId
    /// A socket's phase is one its kind cannot enter: a datagram socket
    /// listening or holding a stream connection, or a non-datagram socket
    /// holding a datagram peer.
    | SocketPhaseKindMismatch of socket : SocketId * kind : SocketKind * phase : SocketPhase
    /// A connection in the table has an identity at or above the next one to
    /// allocate, so a future connect would mint a duplicate.
    | NextConnectionIdNotFresh of nextConnectionId : ConnectionId * existing : ConnectionId
    /// A socket event registration records an ADD ordinal at or above the
    /// next one to mint, so some future ADD would repeat it — and the
    /// ordinal's whole job is to order same-signal ties, which a repeat
    /// leaves unspecified.
    | SocketEventRegistrationOrdinalNotFresh of next : int64 * port : OpenFileDescriptionId * registeredAt : int64
    /// Two socket event registrations record the same ADD ordinal. Ordinals
    /// are minted from one monotonic counter, so a duplicate means two ADDs
    /// were stamped with one mint — and a same-signal tie between the pair
    /// would have no measured order.
    | DuplicateSocketEventRegistrationOrdinal of registeredAt : int64

[<RequireQualifiedAccess>]
module EmulatedKernel =
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

    /// Logical-processor count a freshly-minted simulated process reports.
    /// One, because only single-processor behaviour has been exercised
    /// end-to-end, and because a fixed default is a prerequisite for
    /// replayability.
    /// Hosts that want to exercise the guest's multi-processor code paths
    /// raise it via `KernelConfig.ProcessorCount`.
    [<Literal>]
    let defaultProcessorCount : int = 1

    /// The commonest configuration a guest could be running on: x86-64 with
    /// four-level paging. A host simulating a machine with a different
    /// address-space width sets `KernelConfig.UserAddressLimit`.
    let defaultUserAddressLimit : uint64 = ObservedUserAddressLimit.X64FourLevelPaging

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

    /// 100ns ticks per millisecond. The `SystemNative_GetSystemTime*` family
    /// speaks in ticks while PawPrint's virtual clock speaks in milliseconds,
    /// so every wall-clock derivation goes through this factor. A consequence
    /// is the unit `VirtualClockTicks` itself is denominated in, so no scaling
    /// is applied to the clock when deriving `DateTime.UtcNow`; the factor
    /// converts the *epoch* offset, and converts guest millisecond timeouts
    /// into deadlines.
    [<Literal>]
    let ticksPerMillisecond : int64 = 10_000L

    /// Largest legal wall-clock reading, in 100 ns ticks since the Unix epoch:
    /// `DateTime.MaxValue.Ticks - DateTime.UnixEpoch.Ticks`. `DateTime` cannot
    /// name an instant beyond it.
    ///
    /// Deliberately *not* `maxWallClockEpochMs * ticksPerMillisecond`, which is
    /// 9,999 ticks smaller. The two differ because they bound different things:
    /// `maxWallClockEpochMs` is the last whole millisecond, which is the right
    /// ceiling for `KernelConfig.WallClockEpochMs` because that knob is
    /// denominated in milliseconds, while the clock resolves every 100 ns tick
    /// up to the end of `DateTime`'s range. Deriving this one from the other
    /// would reject the final sub-millisecond of representable time.
    [<Literal>]
    let maxWallClockTicks : int64 = 2534023007999999999L

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

    /// Largest legal `EmulatedKernel.WallClockEpochMs`: 9999-12-31T23:59:59.999Z
    /// as milliseconds since the Unix epoch, which is the last instant
    /// `System.DateTime` can represent
    /// (`(DateTime.MaxValue.Ticks - DateTime.UnixEpoch.Ticks) / ticksPerMillisecond`).
    /// Beyond this the ticks CoreLib adds `UnixEpochTicks` to no longer name a
    /// `DateTime`, and because `DateTime.UtcNow` uses the unvalidated private
    /// ctor the guest would observe the corruption rather than an exception.
    [<Literal>]
    let maxWallClockEpochMs : int64 = 253402300799999L

    /// Unix platform identity a freshly-minted simulated process reports.
    /// Linux/x64 because that is the platform whose CoreLib actually routes
    /// `Environment.OSVersion` through `SystemNative_GetUnixRelease` (the
    /// macOS CoreLib uses `Interop.libobjc.GetOperatingSystemVersion`
    /// instead), and because it is what PawPrint's CI runs on. Hosts choose
    /// a different identity via `KernelConfig.UnixPlatform`.
    let defaultUnixPlatform : SimulatedUnixPlatform = SimulatedUnixPlatform.linuxX64

    /// Current working directory a freshly-minted simulated process reports.
    /// The root, because it is the one directory that exists on every Unix and
    /// needs no name invented for it — and once PawPrint grows a simulated
    /// filesystem, the one directory the default cwd is guaranteed to still
    /// name. (`init` itself starts at `/`, so this is not even an unusual cwd
    /// for a real process.) It is also the honest answer for a runtime that
    /// deliberately declines to read the host's: PawPrint has not been told
    /// where it is, so it claims nothing beyond the root. Hosts that want the
    /// guest to see a particular directory set `KernelConfig.CurrentDirectory`.
    let defaultCurrentDirectory : AbsoluteUnixPath = AbsoluteUnixPath.root

    /// Executable path a freshly-minted simulated process reports: none at all.
    ///
    /// PawPrint models no `exec(2)`, so there is no file that started this
    /// process, and the emulated filesystem holds no image of one. `None` is
    /// therefore the only true answer, and it is a *modelled* Unix state rather
    /// than an invention: both flavours report exactly this — NULL from
    /// `minipal_getexepath`, errno `ENOENT` — for a live process whose
    /// executable no longer resolves, because each of them reaches the path
    /// through `realpath`. Measured on both, by having a guest unlink its own
    /// executable before its first read.
    ///
    /// Synthesising a plausible path instead was rejected for the same reason
    /// `Assembly.Location` reports the empty string: nothing would be there, so
    /// the guest could not act on it. Hosts that want the guest to see a
    /// particular executable set `KernelConfig.ProcessPath`.
    let defaultProcessPath : AbsoluteUnixPath option = None

    /// Effective user ID a freshly-minted simulated process runs as.
    ///
    /// 1000 rather than 0: `Environment.IsPrivilegedProcess` is literally
    /// `GetEUid() == 0`, so a guest that defaulted to root would silently take
    /// the privileged branch of every check it makes about itself — the
    /// uninteresting one, and not the one most programs are written for. 1000
    /// is also the first interactive user on the Ubuntu-shaped platform
    /// `defaultUnixPlatform` already claims to be. A host that wants root says
    /// so in `KernelConfig.UserId`.
    /// The range `bind(2)` draws from when asked for port 0.
    ///
    /// A sysctl on both platforms rather than a property of the kernel image —
    /// Linux's `ip_local_port_range` reads 32768-60999 and Darwin's
    /// `net.inet.ip.portrange.first`/`last` read 49152-65535 — so this is
    /// configuration with one default, in the way `FileSystemType` is, and not a
    /// per-flavour derivation. The default is Linux's, matching
    /// `defaultUnixPlatform`.
    let defaultEphemeralPortRange : uint16 * uint16 = 32768us, 60999us

    /// The `somaxconn` sysctl's default on each flavour, measured on the
    /// probe machines (2026-08-21): `net.core.somaxconn` reads 4096 on the
    /// Linux 6.18 container (the kernel default since 5.4) and
    /// `kern.ipc.somaxconn` reads 128 on macOS 26.
    let defaultSoMaxConn (flavour : SimulatedUnixFlavour) : int =
        match flavour with
        | SimulatedUnixFlavour.Linux -> 4096
        | SimulatedUnixFlavour.Darwin -> 128

    /// Ports a process may bind only as root.
    ///
    /// Measured as 1024 on both: binding 1023 is `EACCES` for an unprivileged
    /// caller and 1024 succeeds. Not configurable, though Linux does expose it as
    /// `ip_unprivileged_port_start`: nothing needs to vary it yet, and a knob
    /// with no consumer is a knob no test covers.
    let privilegedPortCeiling : uint16 = 1024us

    /// The addresses this machine holds, as `bind(2)` decides whether an address
    /// is assignable. Loopback only: PawPrint models no interface a guest could
    /// reach, so anything else would be an address no packet could arrive on.
    ///
    /// `127.0.0.0/8` rather than `127.0.0.1/32` because that is what Linux
    /// assigns to `lo`, and the flavours read the list differently — see
    /// `SimulatedUnixPlatform.isBindableAddress`.
    let defaultLocalAddresses : uint32 list = [ InternetEndpoint.LoopbackAddress ]

    /// The prefixes Linux's local routing table holds, which it will `bind(2)`
    /// any address inside. Loopback's `127.0.0.0/8` is the one every Linux has,
    /// and is why `127.9.9.9` binds there and not on Darwin.
    let defaultLocalRoutes : Ipv4Prefix list = [ Ipv4Prefix.create 0x7F000000u 8 ]

    let defaultUserId : uint32 = 1000u

    /// Effective group ID a freshly-minted simulated process runs as. Matches
    /// `defaultUserId`, as a Linux user-private group does.
    let defaultGroupId : uint32 = 1000u

    /// File-mode creation mask a freshly-minted simulated process reports.
    /// 0o022 because that is what essentially every Unix login shell and service
    /// manager sets, and because it is the mask the existing seed defaults were
    /// written against (`PermissionBits.defaultForRegularFile` is 0o666 with
    /// these bits cleared). Hosts choose otherwise via `KernelConfig.Umask`.
    let defaultUmask : PermissionBits =
        PermissionBits.parseOrFail "EmulatedKernel.defaultUmask" 0o022

    /// The `st_dev` every inode in the emulated filesystem reports.
    ///
    /// One device for the whole tree, since PawPrint models no mounts. The
    /// value itself is unobservable beyond comparison — the BCL reads
    /// `(st_dev, st_ino)` pairs to decide whether two paths name the same file
    /// (`File.Copy`, `File.Move`, `File.Replace`) and never interprets the
    /// device number — but it is *non-zero*: no mounted filesystem
    /// reports 0, so a zero here would be indistinguishable from a field
    /// nobody remembered to write.
    let simulatedDeviceId : int64 = 0x1000001L

    let initial : EmulatedKernel =
        // Bound once so that `CurrentDirectoryInode` is the root of *this*
        // filesystem rather than of a second one that merely looks like it.
        let filesystem = VirtualFileSystem.empty (UnixTimestamp.ofMillisecondsSinceEpoch 0L)

        {
            InstructionCostTicks = defaultInstructionCostTicks
            LastPInvokeError = Map.empty
            LastSystemError = Map.empty
            NativeMemoryPool = NativeMemoryPool.empty
            FileDescriptors = FileDescriptorRegistry.initial
            DirectoryStreams = Map.empty
            DirectoryStreamBlocks = Map.empty
            NextDirectoryStreamId = DirectoryStreamId 0L
            ParkedSocketWaits = Map.empty
            LowLevelMonitors = Map.empty
            NextLowLevelMonitorId = 1
            WaitHandles = Map.empty
            NextWaitHandleId = 1
            NextEventPipeId = 1L
            SpuriousWakeup = SpuriousWakeupStrategy.Disabled
            SyncBlockSpuriousWakeup = SyncBlockSpuriousWakeupStrategy.Disabled
            ClockJitter = ClockJitterStrategy.Disabled
            StepCounter = 0L
            OutputLog = ImmutableArray<OutputLogEntry>.Empty
            Environment = defaultEnvironment
            OptimalMaxSpinWaitsPerSpinIteration = defaultOptimalMaxSpinWaitsPerSpinIteration
            CurrentDirectory = defaultCurrentDirectory
            // The default current directory is the root, which every filesystem
            // has and no operation can remove, so the pair starts consistent
            // whatever else a host goes on to set.
            CurrentDirectoryInode = VirtualFileSystem.root filesystem
            ProcessPath = defaultProcessPath
            UserId = defaultUserId
            GroupId = defaultGroupId
            Umask = defaultUmask
            Signals = SignalState.empty
            Machine =
                {
                    Sockets = Map.empty
                    Connections = Map.empty
                    NextConnectionId = ConnectionId 0L
                    NextSocketEventRegistrationOrdinal = 0L
                    NextSocketId = SocketId 0L
                    NextEphemeralPort = fst defaultEphemeralPortRange
                    EphemeralPortRange = defaultEphemeralPortRange
                    // The Linux default, matching `defaultUnixPlatform`;
                    // `KernelConfig.applyTo` re-resolves it beside the platform.
                    SoMaxConn = defaultSoMaxConn SimulatedUnixFlavour.Linux
                    LocalAddresses = defaultLocalAddresses
                    LocalRoutes = defaultLocalRoutes
                    VirtualClockTicks = 0L
                    WallClockEpochMs = 0L
                    NonCryptoRandomState = NonCryptoRandom.initialState
                    CryptoRandomState = cryptoRandomInitialState
                    ProcessorCount = defaultProcessorCount
                    UserAddressLimit = defaultUserAddressLimit
                    UnixPlatform = defaultUnixPlatform
                    FileSystem = filesystem
                    FileSystemType =
                        EmulatedFileSystemType.defaultFor (SimulatedUnixPlatform.flavour defaultUnixPlatform)
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

    /// Set the Unix platform identity the simulated process reports, together
    /// with the filesystem its mount claims to be. `None` takes the flavour's
    /// own default; an explicit type that flavour could not mount is refused.
    ///
    /// One setter for two fields, in the manner of `withUserAndGroupId`,
    /// because they are not independent: `SystemNative_GetFileSystemType`
    /// answers a *file* from the type and every other descriptor from the
    /// flavour, so a kernel carrying Linux with APFS would hand a guest a
    /// combination no machine could produce. Separate setters could each be
    /// called alone, which is exactly how that state would arise; fused, it is
    /// unrepresentable rather than merely checked.
    ///
    /// Rejects a forged `Unchecked.defaultof` platform, whose null release
    /// would otherwise reach a guest as its `uname -r`.
    let withUnixPlatformAndFileSystemType
        (platform : SimulatedUnixPlatform)
        (fileSystemType : EmulatedFileSystemType option)
        (kernel : EmulatedKernel)
        : EmulatedKernel
        =
        // No eager validation of the release string:
        // `SimulatedUnixPlatform.create` validates at construction, so a value
        // of the type is already a platform some Unix could be. `assertValid`
        // still catches the one value that can bypass that — the forged
        // `Unchecked.defaultof`.
        let platform =
            SimulatedUnixPlatform.assertValid "EmulatedKernel.UnixPlatform" platform

        let flavour = SimulatedUnixPlatform.flavour platform

        let resolved =
            match fileSystemType with
            | None -> EmulatedFileSystemType.defaultFor flavour
            | Some requested ->
                if not (EmulatedFileSystemType.isReportableUnder flavour requested) then
                    failwith
                        $"EmulatedKernel.FileSystemType: a %O{flavour} kernel cannot report %O{requested}, so a guest asking `fstatfs` would learn a fact no such system could tell it. Leave KernelConfig.FileSystemType as None to take %O{flavour}'s own default, or pick a type that flavour mounts."

                requested

        { kernel with
            Machine =
                { kernel.Machine with
                    UnixPlatform = platform
                    FileSystemType = resolved
                }
        }

    /// Set the path to the executable that started the simulated process, or
    /// `None` to report that it has none. `None` is preserved rather than
    /// defaulted; see `EmulatedKernel.ProcessPath`.
    let withProcessPath (path : AbsoluteUnixPath option) (kernel : EmulatedKernel) : EmulatedKernel =
        { kernel with
            ProcessPath = path |> Option.map (AbsoluteUnixPath.assertValid "EmulatedKernel.ProcessPath")
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
            CurrentDirectory = physical
            CurrentDirectoryInode = inode
            Machine =
                { kernel.Machine with
                    FileSystem = filesystem
                }
        }

    /// Set the effective user and group IDs the simulated process runs as.
    let withUmask (umask : PermissionBits) (kernel : EmulatedKernel) : EmulatedKernel =
        { kernel with
            Umask = PermissionBits.assertValid "EmulatedKernel.Umask" umask
        }

    let withUserAndGroupId (userId : uint32) (groupId : uint32) (kernel : EmulatedKernel) : EmulatedKernel =
        { kernel with
            UserId = userId
            GroupId = groupId
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

    /// Set the logical-processor count the simulated process reports. Rejects
    /// non-positive values at the boundary rather than letting them reach a
    /// guest that will divide by them.
    /// Set the greatest range end a user buffer may reach. Rejects zero, which
    /// leaves no address usable as a buffer and so describes no machine.
    let withUserAddressLimit (limit : uint64) (kernel : EmulatedKernel) : EmulatedKernel =
        if limit = 0UL then
            failwith "UserAddressLimit must be positive; got 0, which is a machine with no user address space"

        { kernel with
            Machine =
                { kernel.Machine with
                    UserAddressLimit = limit
                }
        }

    /// Whether, and where, this machine's kernel screens a read or write buffer
    /// before performing the operation: the flavour decides whether, the
    /// machine's address-space limit decides where.
    let userBufferCheck (kernel : EmulatedKernel) : UserBufferCheck =
        if SimulatedUnixPlatform.screensUserBufferUpFront kernel.UnixPlatform then
            UserBufferCheck.BeforeOperation kernel.UserAddressLimit
        else
            UserBufferCheck.AtCopyTime

    let withProcessorCount (count : int) (kernel : EmulatedKernel) : EmulatedKernel =
        if count < 1 then
            failwith $"ProcessorCount must be at least 1; got %d{count}"

        { kernel with
            Machine =
                { kernel.Machine with
                    ProcessorCount = count
                }
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

    /// Set the wall-clock reading the simulated process boots at. Rejects
    /// values outside the range `System.DateTime` can represent at the
    /// boundary, rather than letting them reach a guest that would receive a
    /// silently corrupt `DateTime` from `DateTime.UtcNow`'s unvalidated ctor.
    let withWallClockEpochMs (epochMs : int64) (kernel : EmulatedKernel) : EmulatedKernel =
        if epochMs < 0L then
            failwith
                $"WallClockEpochMs must be non-negative (PawPrint does not model a simulated process booting before the Unix epoch); got %d{epochMs}"

        if epochMs > maxWallClockEpochMs then
            failwith
                $"WallClockEpochMs must be at most %d{maxWallClockEpochMs} (9999-12-31T23:59:59.999Z, the last instant System.DateTime can represent); got %d{epochMs}"

        { kernel with
            Machine =
                { kernel.Machine with
                    WallClockEpochMs = epochMs
                }
        }

    /// Wall-clock time the simulated process currently observes, in 100ns ticks
    /// since the Unix epoch: exactly what `SystemNative_GetSystemTimeAsTicks`
    /// returns, and hence (once CoreLib has added `UnixEpochTicks` and stamped
    /// `DateTimeKind.Utc`) what `DateTime.UtcNow` reports.
    ///
    /// Pure: reading the clock never advances it, so two threads reading on the
    /// same scheduler tick observe the same instant — the same property
    /// `VirtualClockTicks` guarantees for `Environment.TickCount64`, and the
    /// reason this is a plain derivation rather than an advance-on-read
    /// counter. That does mean `DateTime.UtcNow` is only *weakly* monotonic
    /// here: repeated reads within one scheduler tick are equal, so it is not
    /// a source of unique values. Real `clock_gettime(CLOCK_REALTIME)` makes no
    /// uniqueness guarantee either, so guest code relying on one is broken on
    /// the real runtime too and should be caught rather than accommodated.
    let systemTimeAsTicks (kernel : EmulatedKernel) : int64 =
        // A kernel built by record-copy can bypass `withWallClockEpochMs`, so
        // re-assert the invariant here: the guest must never observe a tick
        // count that names no `DateTime`.
        //
        // The association matters. `WallClockEpochMs` is milliseconds and
        // `VirtualClockTicks` is already in `DateTime`'s own 100 ns unit, so the
        // scaling applies to the epoch alone: scaling their *sum* would first
        // have to convert the clock back to milliseconds and would throw away
        // its sub-millisecond digits. Doing it this way is also what keeps the
        // arithmetic in range — the guards below bound each operand, and
        // `maxWallClockEpochMs * ticksPerMillisecond` is 2.53e18, comfortably
        // inside int64, where the same bound expressed in nanoseconds
        // (2.53e20) would not be.
        if kernel.WallClockEpochMs < 0L || kernel.WallClockEpochMs > maxWallClockEpochMs then
            failwith
                $"kernel WallClockEpochMs is %d{kernel.WallClockEpochMs}, which is outside the range [0, %d{maxWallClockEpochMs}] that System.DateTime can represent"

        if kernel.VirtualClockTicks < 0L || kernel.VirtualClockTicks > maxWallClockTicks then
            failwith
                $"kernel VirtualClockTicks is %d{kernel.VirtualClockTicks}, which is outside the range [0, %d{maxWallClockTicks}] a wall-clock reading can be derived from"

        let ticks = kernel.WallClockEpochMs * ticksPerMillisecond + kernel.VirtualClockTicks

        if ticks > maxWallClockTicks then
            failwith
                $"simulated wall clock has reached %d{ticks} ticks since the Unix epoch, past the %d{maxWallClockTicks} that System.DateTime can represent; lower KernelConfig.WallClockEpochMs"

        ticks

    /// Nanoseconds per millisecond. `SystemNative_GetTimestamp` speaks in
    /// nanoseconds while PawPrint's virtual clock speaks in 100 ns ticks, so
    /// the high-resolution timestamp derivation goes through this factor. Every
    /// timestamp the guest observes is therefore a multiple of 100 — `Stopwatch`
    /// has 100 ns granularity here, matching `DateTime`'s quantum, where real
    /// `clock_gettime(CLOCK_MONOTONIC)` is finer still.
    [<Literal>]
    let nanosecondsPerTick : int64 = 100L

    /// Whether the simulated process is exempt from the permission rules a kernel
    /// applies to everyone else: uid 0, and nothing else.
    ///
    /// One definition rather than a comparison at each site, because the sites
    /// answer *different* questions from the same fact — whether `open` may ignore
    /// a mode that forbids the access it was asked for, and whether a write keeps
    /// a file's set-user-ID bits — and they must not be able to drift apart about
    /// who root is. `CallerPrivilege` rather than a `bool` for the same reason:
    /// the answer travels through several signatures before it is used, and a
    /// bare flag arrives at them saying nothing about which fact it is.
    ///
    /// `EmulatedKernel.defaultUserId` is deliberately not 0: `Environment.IsPrivilegedProcess`
    /// is literally `GetEUid() == 0`, so a guest run as root skips its own
    /// privilege guards.
    let callerPrivilege (kernel : EmulatedKernel) : CallerPrivilege =
        if kernel.UserId = 0u then
            CallerPrivilege.Privileged
        else
            CallerPrivilege.Unprivileged

    /// The moment the emulated kernel stamps on an inode it changes now, in the
    /// `struct timespec` an inode's timestamps are kept in.
    ///
    /// The same wall clock `SystemNative_GetSystemTimeAsTicks` reports, so a
    /// guest that writes a file and then reads `DateTime.UtcNow` sees two
    /// readings of one clock rather than two clocks that happen to agree. Its
    /// granularity is therefore the virtual clock's own 100 ns quantum: the
    /// nanosecond part is always a multiple of 100, where a real filesystem
    /// records whatever its kernel's clock offers.
    let fileTimestamp (kernel : EmulatedKernel) : UnixTimestamp =
        let ticks = systemTimeAsTicks kernel

        // `systemTimeAsTicks` has established the count is non-negative, so
        // neither the quotient nor the remainder can be, and the nanosecond part
        // lands in `[0, 1e9)` without the floor correction
        // `UnixTimestamp.ofMillisecondsSinceEpoch` needs for a pre-epoch instant.
        let ticksPerSecond = ticksPerMillisecond * 1000L

        UnixTimestamp.createOrFail
            "EmulatedKernel.fileTimestamp"
            (ticks / ticksPerSecond)
            (int (ticks % ticksPerSecond) * int nanosecondsPerTick)

    /// Largest `VirtualClockTicks` from which a nanosecond timestamp can be
    /// derived without overflowing the `int64` the PAL entry point returns:
    /// `Int64.MaxValue / nanosecondsPerTick`, i.e. about 29 years of simulated
    /// uptime.
    ///
    /// The horizon is reachable by ordinary guest code, not merely in
    /// principle. A sleep deadline is `VirtualClockTicks + timeout` with no cap,
    /// and when no thread is Runnable the driver's deadline jump moves the
    /// clock the whole way there, so each `Thread.Sleep(Int32.MaxValue)`
    /// advances it by about 2.1e13 ticks, and roughly 4,300 cross this bound. So
    /// `monotonicTimestampNanos` checks rather than assumes — silently wrapping
    /// into a negative timestamp would hand the guest a monotonic clock that
    /// had run backwards, which is the one guarantee the primitive exists to
    /// provide.
    ///
    /// The bound is *tighter* than `maxWallClockTicks` by a factor of about
    /// 27, so there is a band of clock readings from which `DateTime.UtcNow`
    /// and `Environment.TickCount64` are derivable but `Stopwatch.GetTimestamp`
    /// is not. `withVirtualClockTicks` bounds the field centrally at the
    /// scheduler, its sole writer, using *this* ceiling because it is the
    /// tightest; the per-reader guards remain because a kernel assembled by
    /// record-copy can bypass the writer, and `systemTimeAsTicks` has the same
    /// shape for the same reason.
    [<Literal>]
    let maxMonotonicTimestampClockTicks : int64 = 92233720368547758L

    /// The checks `withVirtualClockTicks` and `retireStep` share: shared so that the fused
    /// per-instruction advance cannot drift from the general setter's contract.
    let private validateVirtualClockTicks (ticks : int64) (kernel : EmulatedKernel) : unit =
        // Checked independently of the monotonicity comparison below, which on its own would
        // wave through a negative target whenever the current value is more negative still —
        // reachable because a kernel assembled by record-copy never passed through here.
        if ticks < 0L then
            failwith
                $"virtual clock would be set to %d{ticks} ticks; simulated uptime starts at zero and cannot be negative"

        if ticks < kernel.VirtualClockTicks then
            failwith
                $"virtual clock would move backwards, from %d{kernel.VirtualClockTicks} to %d{ticks} ticks; it is monotonic by construction and every guest-visible clock derives from it"

        // The bound also keeps deadline arithmetic total. A finite deadline is
        // `clock + timeoutMs * ticksPerMillisecond`, and `Thread.Sleep(Int32.MaxValue)`
        // contributes about 2.1e13 ticks; with the clock bounded at 9.2e16 the sum cannot
        // approach `Int64.MaxValue`, so the seven deadline sites need no checked arithmetic of
        // their own. Without the bound they would need it, and the horizon is close enough to
        // matter: the deadline jump advances the clock to a deadline *without* retiring a step,
        // so a loop of `Sleep(Int32.MaxValue)` reaches the wrap in about 430,000 iterations — a
        // few million interpreted instructions.
        if ticks > maxMonotonicTimestampClockTicks then
            failwith
                $"simulated uptime has reached %d{ticks} ticks (100 ns each), past the %d{maxMonotonicTimestampClockTicks} from which a monotonic nanosecond timestamp can still be derived — about 292 years. The guest has almost certainly been jumping the clock with long timed waits; PawPrint cannot represent time beyond this."

    /// Advance the virtual clock to `ticks`, which must not move it backwards and must keep it
    /// inside the range every clock-derived reading can be computed from.
    ///
    /// The bound is `maxMonotonicTimestampClockTicks` — the tightest of the per-reader ceilings
    /// — so this is deliberately stricter than any individual reader requires. Enforcing it at
    /// the writer means a guest that runs the clock off the end faults at the wait that did it,
    /// naming the operation responsible, rather than at whichever unlucky later `Stopwatch` read
    /// happens to trip over the value.
    let withVirtualClockTicks (ticks : int64) (kernel : EmulatedKernel) : EmulatedKernel =
        validateVirtualClockTicks ticks kernel

        { kernel with
            Machine =
                { kernel.Machine with
                    VirtualClockTicks = ticks
                }
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
        validateVirtualClockTicks ticks kernel

        { kernel with
            StepCounter = kernel.StepCounter + 1L
            Machine =
                { kernel.Machine with
                    VirtualClockTicks = ticks
                }
        }

    /// Monotonic time since the simulated process booted, in nanoseconds:
    /// exactly what `SystemNative_GetTimestamp` returns, and hence what
    /// `Stopwatch.GetTimestamp()` reports on a Unix CoreLib.
    ///
    /// Real CoreCLR answers this from `minipal_hires_ticks()`
    /// (`clock_gettime_nsec_np(CLOCK_UPTIME_RAW)` on macOS,
    /// `clock_gettime(CLOCK_MONOTONIC)` on Linux). PawPrint derives it from
    /// the same `VirtualClockTicks` that already backs
    /// `SystemNative_GetLowResolutionTimestamp` — which upstream is
    /// `minipal_lowres_ticks()`, *the same clock* read in milliseconds. Making
    /// both PawPrint entry points views of one field reproduces a relationship
    /// the guest can observe: `Environment.TickCount64` and `Stopwatch` must
    /// not disagree about how much time has passed.
    ///
    /// Unlike `systemTimeAsTicks` this is *not* offset by
    /// `WallClockEpochMs`: the monotonic clock counts from boot, and CoreLib
    /// only ever subtracts two readings of it, so an epoch offset would be
    /// both unfaithful and unobservable.
    ///
    /// Pure, like every other clock observer: reading never advances the
    /// clock, so two threads reading on the same scheduler tick observe the
    /// same timestamp, and `Stopwatch` is only weakly monotonic here (repeated
    /// reads within one tick are equal, so a zero-length measured interval is
    /// normal). Real `CLOCK_MONOTONIC` makes no uniqueness guarantee either.
    let monotonicTimestampNanos (kernel : EmulatedKernel) : int64 =
        // The driver loop is the only production writer of `VirtualClockTicks`
        // and only ever advances it from zero, but a kernel built by
        // record-copy (as tests do) can bypass that, so re-assert here rather
        // than trusting construction.
        if
            kernel.VirtualClockTicks < 0L
            || kernel.VirtualClockTicks > maxMonotonicTimestampClockTicks
        then
            failwith
                $"kernel VirtualClockTicks is %d{kernel.VirtualClockTicks}, which is outside the range [0, %d{maxMonotonicTimestampClockTicks}] a nanosecond monotonic timestamp can be derived from without overflowing int64"

        kernel.VirtualClockTicks * nanosecondsPerTick

    /// The guest-visible `Environment.TickCount64`, in whole milliseconds:
    /// `SystemNative_GetLowResolutionTimestamp`'s reading.
    ///
    /// Upstream the two monotonic entry points (`minipal_lowres_ticks` and
    /// `minipal_hires_ticks`) read the same clock at two resolutions, and the contract a guest
    /// depends on is that they never disagree — so this must be exactly the high-resolution
    /// reading truncated to milliseconds.
    ///
    /// Truncating rather than rounding is faithful: upstream's coarse clock truncates too.
    let lowResolutionTimestampMs (kernel : EmulatedKernel) : int64 =
        // Lives here beside `monotonicTimestampNanos` and `systemTimeAsTicks` rather than
        // inline in the PAL handler, so that all three projections of the one clock sit
        // together and can be checked against each other without a test having to restate the
        // arithmetic of any of them.
        //
        // Unguarded, unlike its siblings, because dividing a clock already bounded below
        // `Int64.MaxValue` cannot overflow or go negative.
        kernel.VirtualClockTicks / ticksPerMillisecond


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

    /// Why `name`/`value` could not be a variable of a real process, or `None` if
    /// it could. The string describes the problem for a caller to prefix with its
    /// own context. Total: a null name or value is itself one of the answers,
    /// rather than something this dereferences.
    ///
    /// A real process's environment is not a name-to-value map at all: it is a
    /// list of `name=value` strings, and the map every environment API presents
    /// is a *view* of that list, obtained by splitting each entry at its first
    /// `=`. CoreCLR makes that view total by refusing, in
    /// `GetEnvironmentVariableA` (`pal/src/misc/environ.cpp`), to look up a name
    /// that is empty or contains `=`; `Environment.GetEnvironmentVariables`
    /// likewise discards any entry whose first `=` is not after the first
    /// character. So the set of names the view can ever produce is exactly the
    /// non-empty, `=`-free ones, and a NUL cannot occur at all because the
    /// entries are C strings.
    ///
    /// PawPrint stores the map rather than the list, which is the more convenient
    /// representation but admits names that view could never yield. Such a name
    /// has no consistent behaviour to model: measured against real .NET, an
    /// inherited entry `A=B=C` is the variable `A` with value `B=C`, and looking
    /// up `A=B` returns null — so a PawPrint table holding the key `A=B` would
    /// have to answer that lookup both ways at once. Rejecting the table is what
    /// keeps the two environment APIs in agreement with each other and with the
    /// real runtime.
    ///
    /// Shared with the `GetEnvironmentStringsW` shim, which flattens the map back
    /// into a list and so re-checks; keeping one copy of the rule is what stops
    /// the two disagreeing about which tables are legal.
    let environmentEntryProblem (name : string) (value : string) : string option =
        // Null first, and as its own case rather than lumped in with the empty
        // name. `Map<string, string>` holds a null key or value quite happily —
        // F#'s comparer sorts null first, and a consumer of this package writing
        // C# has nothing stopping it — so this function would otherwise dereference
        // null and abort a run with a bare NullReferenceException, which is the
        // opposite of what a validating classifier is for. Same reason
        // `AbsoluteUnixPath.assertValid` exists.
        if isNull name then
            Some "a variable whose name is null, which is not a string an environment list could hold"
        elif isNull value then
            // `name` is known non-null by now, so it is safe to name the offender.
            Some $"a variable (%s{name}) whose value is null, which is not a string an environment list could hold"
        elif name = "" then
            Some
                "a variable with an empty name, which no environment list can express (the entry would read `=value`, which every reader discards)"
        elif name.Contains '=' then
            Some
                $"a variable whose name contains '=' (%s{name}), which no environment list can express unambiguously: a reader splits at the first '=', so it would see a different name and value"
        elif name.Contains (char 0) then
            Some $"a variable whose name contains a NUL code unit (%s{name}), which would terminate its entry early"
        elif value.Contains (char 0) then
            Some $"a variable (%s{name}) whose value contains a NUL code unit, which would terminate its entry early"
        else
            None

    /// Overlay the supplied environment variables on top of the kernel's
    /// existing `Environment` map. Used by `Program.run` / the CLI to layer
    /// host or test-supplied env vars on top of `defaultEnvironment` without
    /// losing the seeded invariant-globalization default for keys the
    /// caller does not set. Matches the Unix-PAL semantics of the env table
    /// (case-sensitive name comparison): overlay keys replace existing
    /// entries with the same exact name, and names that differ only in case
    /// are treated as distinct variables — which is what CoreCLR's Unix PAL
    /// does for `GetEnvironmentVariableW` on the macOS/Linux hosts this
    /// project runs on.
    ///
    /// Rejects an overlay entry that no real process could have, per
    /// `environmentEntryProblem`. This is the only way an entry enters the table
    /// — `defaultEnvironment` is the sole other source and satisfies the rule,
    /// and PawPrint services no `SetEnvironmentVariableW`, so no guest can add
    /// one — which is what lets every reader of the table treat its names as
    /// ones a real process could hold. Failing here rather than at the first read
    /// means a host learns at configuration time, before any guest code runs.
    /// Set the `somaxconn` sysctl. Takes the platform as a parameter rather
    /// than reading `kernel.UnixPlatform`, so that this and the platform
    /// setter cannot become order-dependent; `KernelConfig.applyTo` passes
    /// the same platform to both.
    ///
    /// `None` takes the flavour's measured default. The clamp this feeds
    /// (`connectSocket`'s capacity rule) was measured with the sysctl set to
    /// 3 on Linux and at the default 128 on Darwin, so a configured value is
    /// on measured ground, but it must be positive: no kernel was measured
    /// with a non-positive somaxconn.
    let withSoMaxConn
        (platform : SimulatedUnixPlatform)
        (value : int option)
        (kernel : EmulatedKernel)
        : EmulatedKernel
        =
        let resolved =
            match value with
            | None -> defaultSoMaxConn (SimulatedUnixPlatform.flavour platform)
            | Some value ->
                if value < 1 then
                    failwith
                        $"EmulatedKernel.SoMaxConn: %d{value} is not positive, and no kernel was measured with a non-positive somaxconn — the accept-queue capacity it would imply is a guess. Configure a positive value, or None for the flavour's default."

                value

        { kernel with
            Machine =
                { kernel.Machine with
                    SoMaxConn = resolved
                }
        }

    /// Sets the ephemeral range, and rewinds the cursor into it: a cursor left
    /// outside the range would hand out its first port from wherever the previous
    /// range had reached.
    let withEphemeralPortRange ((low, high) : uint16 * uint16) (kernel : EmulatedKernel) : EmulatedKernel =
        if low = 0us then
            failwith
                "EmulatedKernel.EphemeralPortRange: port 0 is how a guest *asks* for an ephemeral port, so it cannot also be one that gets handed out. Start the range at 1 or above."

        if low > high then
            failwith
                $"EmulatedKernel.EphemeralPortRange: the range %d{low}-%d{high} is empty, so no bind of port 0 could ever be answered."

        { kernel with
            Machine =
                { kernel.Machine with
                    EphemeralPortRange = low, high
                    NextEphemeralPort = low
                }
        }

    let withLocalAddresses
        (addresses : uint32 list)
        (routes : Ipv4Prefix list)
        (kernel : EmulatedKernel)
        : EmulatedKernel
        =
        // The prefix record is public, so a host can build one whose length is
        // outside [0, 32]; the CLI masks such a shift rather than faulting, which
        // would give an unrelated mask and a silently wrong bindability.
        let routes =
            routes |> List.map (Ipv4Prefix.assertValid "EmulatedKernel.LocalRoutes")

        // An empty list is legal and means a machine with no addresses at all,
        // on which only the wildcard binds. That is a strange machine but a
        // representable one, and refusing it here would be inventing a rule.
        { kernel with
            Machine =
                { kernel.Machine with
                    LocalAddresses = addresses
                    LocalRoutes = routes
                }
        }

    /// Hands out the lowest free port at or after the cursor, sweeping the range
    /// once and wrapping. `isAcceptable` decides freedom, and must be the same
    /// conflict test `bind(2)` itself applies — a port a TCP socket holds is free
    /// to a UDP one, so a naive "is this port taken" set would refuse a legal
    /// bind.
    ///
    /// `None` when a full sweep finds nothing. The caller decides what to do:
    /// there is no measured answer for an exhausted range, so inventing an errno
    /// here would be a guess.
    let allocateEphemeralPort
        (isAcceptable : uint16 -> bool)
        (kernel : EmulatedKernel)
        : (uint16 * EmulatedKernel) option
        =
        let low, high = kernel.EphemeralPortRange
        let width = int high - int low + 1

        let rec sweep (remaining : int) (candidate : uint16) =
            if remaining = 0 then
                None
            else

            let next = if candidate = high then low else candidate + 1us

            if isAcceptable candidate then
                Some (
                    candidate,
                    { kernel with
                        Machine =
                            { kernel.Machine with
                                NextEphemeralPort = next
                            }
                    }
                )
            else
                sweep (remaining - 1) next

        // A cursor outside the range can only come from a hand-built kernel;
        // start from the bottom rather than sweeping from nowhere.
        let start =
            if kernel.NextEphemeralPort < low || kernel.NextEphemeralPort > high then
                low
            else
                kernel.NextEphemeralPort

        sweep width start

    let withEnvironment (env : Map<string, string>) (kernel : EmulatedKernel) : EmulatedKernel =
        for KeyValue (name, value) in env do
            match environmentEntryProblem name value with
            | None -> ()
            | Some problem ->
                failwith
                    $"EmulatedKernel.Environment: refusing to install %s{problem}. Fix the KernelConfig.Environment this run was given."

        let merged =
            (kernel.Environment, env)
            ||> Map.fold (fun acc key value -> Map.add key value acc)

        { kernel with
            Environment = merged
        }

    /// The socket `socketId` names.
    ///
    /// Total, and loudly partial rather than an option: every `SocketId` a
    /// caller can hold came out of an `OpenFileTarget.Socket`, and
    /// `checkInvariants` rejects a kernel in which one of those names nothing.
    /// A `None` here would push that impossible case onto every call site.
    let socket (socketId : SocketId) (kernel : EmulatedKernel) : SocketDescription =
        match Map.tryFind socketId kernel.Sockets with
        | Some socket -> socket
        | None ->
            failwith
                $"EmulatedKernel.socket: %O{socketId} names no socket in this kernel's socket table. Every SocketId reachable by a caller comes from an open file description, and EmulatedKernelDefect.DanglingSocket exists to make that unreachable, so this is an interpreter bug rather than anything a guest did."

    /// The readiness a socket presents right now, before any waiter's interest
    /// mask is applied. Every row is measured on Linux 6.18.5 — `masks.c`
    /// (docs/plans/2026-08-21-socket-readiness-wake) through level-triggered
    /// `epoll_wait` with timeout 0, and `pollmask.c`
    /// (docs/plans/2026-08-23-socket-poll) through `poll(2)` with timeout 0,
    /// which agree on every phase.
    ///
    /// Darwin has no measured rows and needs none: both waiters refuse that
    /// flavour before reaching here — epoll at registration (kqueue is
    /// structurally different) and poll in its own handler — so no readiness
    /// question can be asked of a Darwin-flavoured kernel.
    let socketReadinessLevel (socketId : SocketId) (kernel : EmulatedKernel) : ReadinessLevel =
        let target = socket socketId kernel

        match target.Phase with
        | SocketPhase.Listening listenState ->
            { ReadinessLevel.none with
                In = not (List.isEmpty listenState.Queue)
            }
        | SocketPhase.Idle
        | SocketPhase.DatagramPeer _ ->
            match target.Kind with
            | SocketKind.Stream ->
                // A datagram socket never enters `DatagramPeer` with a
                // Stream kind, so this arm is `Idle` only.
                { ReadinessLevel.none with
                    Out = true
                    Hup = true
                }
            | SocketKind.Datagram ->
                { ReadinessLevel.none with
                    Out = true
                }
            | SocketKind.Raw
            | SocketKind.SeqPacket ->
                failwith
                    $"EmulatedKernel.socketReadinessLevel: socket %O{socketId} is %O{target.Kind}, whose readiness is measured for poll but not for epoll. Both kinds are reachable only in the AF_UNIX domain, and two callers arrive here: an epoll ADD (the registration screen rejects only regular files, so a socket of any kind is admitted) and `SystemNative_Poll` (which needs no registration at all). `poll(2)` reports OUT for a fresh SOCK_RAW and OUT|HUP for a fresh SOCK_SEQPACKET on Linux (docs/plans/2026-08-23-socket-poll/pollgaps.c). Those two rows are the whole answer only while PawPrint's own `listen`/`connect`/`accept` handlers keep refusing these kinds, which is what confines such a socket to `Idle` — the real kernel does accept connections on SOCK_SEQPACKET, so measuring those handlers reopens every other phase for it. They are still refused because what `epoll_wait` reports is only *inferred* from the two waiters sharing one poll handler, and every other row in this function is measured through both. Take an epoll measurement (an et.c-style probe on an AF_UNIX raw and seqpacket socket) before answering, since answering here makes epoll delivery answer too."
        | SocketPhase.EstablishedPendingReport connectionId
        | SocketPhase.Established connectionId ->
            // With the peer alive and no receive path modelled, both ends
            // are exactly write-ready; once the peer is gone, the level is
            // the measured half-closed one.
            let peerAlive =
                kernel.Sockets
                |> Map.exists (fun otherId other ->
                    otherId <> socketId
                    && (
                        match other.Phase with
                        | SocketPhase.Established c
                        | SocketPhase.EstablishedPendingReport c -> c = connectionId
                        | SocketPhase.Listening listenState -> List.contains connectionId listenState.Queue
                        | SocketPhase.Idle
                        | SocketPhase.RefusedPendingDelivery
                        | SocketPhase.Dead
                        | SocketPhase.DatagramPeer _ -> false
                    )
                )

            if peerAlive then
                { ReadinessLevel.none with
                    Out = true
                }
            else
                // The measured half-closed level (`order3.c` row Q). Peer
                // liveness is derived rather than stored: the connection
                // object outlives its ends exactly as long as something
                // references it, so the scan is the truth.
                {
                    In = true
                    Out = true
                    RdHup = true
                    Hup = false
                    Err = false
                }

        | SocketPhase.RefusedPendingDelivery ->
            {
                In = true
                Out = true
                RdHup = true
                Hup = true
                Err = true
            }
        | SocketPhase.Dead ->
            failwith
                $"EmulatedKernel.socketReadinessLevel: socket %O{socketId} is in the Darwin-only Dead phase. Both doors into this function refuse the Darwin flavour before any level is computed — `SystemNative_TryChangeSocketEventRegistration` because kqueue is structurally different, and `SystemNative_Poll` because its Darwin rows are measured but unmodelled — so reaching here is an interpreter bug. Darwin polls this phase IN|PRI|HUP (docs/plans/2026-08-23-socket-poll/pollmulti.c) if that changes."

    /// The epoll readiness of the descriptor `targetId` names, for computing
    /// what a registration on it would report.
    ///
    /// A standard stream's level is a constant of the launch shape PawPrint
    /// models (measured, `pipes.c`): stdin is the read end of a pipe whose
    /// write end the launcher closed — the same claim `SystemNative_Read`'s
    /// immediate-EOF makes — which presents `EPOLLHUP`, and the output
    /// streams are write ends with space and a live reader, which present
    /// `EPOLLOUT`. No modelled operation changes either, so the streams need
    /// no producer. A file or port target cannot reach here: the registry
    /// answers EPERM for the one and refuses the other.
    let epollReadinessOfDescription (targetId : OpenFileDescriptionId) (kernel : EmulatedKernel) : ReadinessLevel =
        match Map.tryFind targetId (FileDescriptorRegistry.descriptions kernel.FileDescriptors) with
        | None ->
            failwith
                $"EmulatedKernel.epollReadinessOfDescription: %O{targetId} names no live open file description. FileDescriptorRegistry.close sweeps destroyed descriptions out of every interest table, so this is an interpreter bug."
        | Some description ->

        match description.Target with
        | OpenFileTarget.Socket socketId -> socketReadinessLevel socketId kernel
        | OpenFileTarget.StandardStream FileDescriptorRole.StandardInput ->
            { ReadinessLevel.none with
                Hup = true
            }
        | OpenFileTarget.StandardStream FileDescriptorRole.StandardOutput
        | OpenFileTarget.StandardStream FileDescriptorRole.StandardError ->
            { ReadinessLevel.none with
                Out = true
            }
        | OpenFileTarget.File _ ->
            failwith
                $"EmulatedKernel.epollReadinessOfDescription: %O{targetId} is a regular file, which epoll_ctl answers EPERM for, so no registration can name it (this is an interpreter bug)."
        | OpenFileTarget.SocketEventPort _ ->
            failwith
                $"EmulatedKernel.epollReadinessOfDescription: %O{targetId} is itself a socket event port; the registry refuses a nested-port registration, so no registration can name it (this is an interpreter bug)."

    /// The readiness of the descriptor `targetId` names, for a `poll(2)`
    /// caller.
    ///
    /// A sibling of `epollReadinessOfDescription` rather than a widening of
    /// it: the two dispatchers refuse different things, because `epoll_ctl`
    /// screens targets that `poll(2)` accepts. The per-socket level they share
    /// (`socketReadinessLevel`) is the part measurement says is one function.
    ///
    /// Linux rows only; the handler refuses the Darwin flavour before calling
    /// this, which is what lets the file row below be a single answer — on
    /// Darwin a regular file polls `IN|PRI|OUT` but a directory polls `NVAL`,
    /// so the same `OpenFileTarget.File` would need two.
    let pollReadinessOfDescription (targetId : OpenFileDescriptionId) (kernel : EmulatedKernel) : ReadinessLevel =
        match Map.tryFind targetId (FileDescriptorRegistry.descriptions kernel.FileDescriptors) with
        | None ->
            failwith
                $"EmulatedKernel.pollReadinessOfDescription: %O{targetId} names no live open file description. `SystemNative_Poll` answers POLLNVAL for an fd that names nothing, without ever reaching here, so this is an interpreter bug."
        | Some description ->

        match description.Target with
        | OpenFileTarget.Socket socketId -> socketReadinessLevel socketId kernel
        | OpenFileTarget.File _ ->
            // Measured (`pollgaps.c`): a regular file answers IN|OUT at every
            // offset and under O_RDONLY as much as O_RDWR, and a directory
            // answers the same. Files have no `->poll` handler, so the VFS
            // default reports them always-ready; nothing about this varies
            // with the file's contents or the description's position.
            { ReadinessLevel.none with
                In = true
                Out = true
            }
        | OpenFileTarget.StandardStream FileDescriptorRole.StandardInput ->
            // The same launch-shape constants `epollReadinessOfDescription`
            // holds, and poll agrees with both on Linux (`pollmask.c` rows 19
            // and 20). Not shared with that function: it refuses two of the
            // targets this one answers, so the common part is the socket
            // level, not the dispatch.
            { ReadinessLevel.none with
                Hup = true
            }
        | OpenFileTarget.StandardStream FileDescriptorRole.StandardOutput
        | OpenFileTarget.StandardStream FileDescriptorRole.StandardError ->
            { ReadinessLevel.none with
                Out = true
            }
        | OpenFileTarget.SocketEventPort _ ->
            failwith
                $"EmulatedKernel.pollReadinessOfDescription: %O{targetId} is a socket event port, and what poll(2) reports for one is unmeasured. Unlike the epoll dispatcher's refusal of this case, a guest genuinely can reach it — poll accepts any descriptor — but no managed caller does: CoreLib polls only sockets (System.Net.Sockets), a standard stream (ConsolePal.Write) and an inotify descriptor (FileSystemWatcher, a kind PawPrint does not model). Measure what an epoll descriptor with and without ready events reports before answering."

    /// Every live open file description naming `socketId`.
    let private descriptionsNamingSocket (socketId : SocketId) (kernel : EmulatedKernel) : Set<OpenFileDescriptionId> =
        FileDescriptorRegistry.descriptions kernel.FileDescriptors
        |> Map.toSeq
        |> Seq.choose (fun (descriptionId, description) ->
            match description.Target with
            | OpenFileTarget.Socket target when target = socketId -> Some descriptionId
            | _ -> None
        )
        |> Set.ofSeq

    /// Whether any socket event port holds a registration targeting an open
    /// file description that names `socketId`.
    ///
    /// This is what makes a readiness change on the socket *observable*:
    /// `closeFd` consults it before destroying the peer of an established
    /// pair, because the survivor's level would change to one this kernel
    /// cannot represent, and with no registration there is nothing that
    /// could deliver the difference.
    let socketIsRegisteredWithAnyEventPort (socketId : SocketId) (kernel : EmulatedKernel) : bool =
        let namingDescriptions = descriptionsNamingSocket socketId kernel

        FileDescriptorRegistry.descriptions kernel.FileDescriptors
        |> Map.exists (fun _ description ->
            match description.Target with
            | OpenFileTarget.SocketEventPort portState ->
                portState.Registrations
                |> Map.exists (fun (_, targetId) _ -> Set.contains targetId namingDescriptions)
            | _ -> false
        )

    /// A *data-ready* wake on `socketId` — the accept-queue push is the one
    /// modelled producer. Keyed: the producer signals synchronously with the
    /// state change, so the socket's new level is the signalled mask, and a
    /// registration whose interest misses it entirely is never queued
    /// (measured, `order6.c`). Lazy so the level is computed only when a
    /// registration actually targets the socket.
    ///
    /// The producers are a measured set, not "anything that writes the
    /// socket table": a datagram re-target or dissolve, `bind(2)`, and the
    /// completion-reporting connect measurably signal nothing at all
    /// (`order3.c` rows N, O, P).
    let signalSocketDataReady (socketId : SocketId) (kernel : EmulatedKernel) : EmulatedKernel =
        { kernel with
            FileDescriptors =
                FileDescriptorRegistry.signalSocketEventPorts
                    (descriptionsNamingSocket socketId kernel)
                    (Some (lazy (socketReadinessLevel socketId kernel)))
                    kernel.FileDescriptors
        }

    /// A *state-change* wake on `socketId` — a connect resolving (completion
    /// or refusal), the refusal delivery's reset, a peer's FIN. Unkeyed:
    /// measured (`order8.c`, `order9.c`), such a wake queues every
    /// registration regardless of interest, the entry keeps the wake's
    /// position through a later interest change, and delivery's re-poll does
    /// the filtering.
    let signalSocketStateChange (socketId : SocketId) (kernel : EmulatedKernel) : EmulatedKernel =
        { kernel with
            FileDescriptors =
                FileDescriptorRegistry.signalSocketEventPorts
                    (descriptionsNamingSocket socketId kernel)
                    None
                    kernel.FileDescriptors
        }

    /// Each pending entry of the port, in delivery order, with what it would
    /// report if `epoll_wait` re-polled it right now: the target's current
    /// level restricted to the registration's interest.
    let private annotatedReady
        (portState : SocketEventPortState)
        (kernel : EmulatedKernel)
        : ((int * OpenFileDescriptionId) * SocketEventRegistration * ReadinessLevel) list
        =
        portState.Ready
        |> List.map (fun (_, targetId as key) ->
            let registration =
                match Map.tryFind key portState.Registrations with
                | Some registration -> registration
                | None ->
                    failwith
                        $"EmulatedKernel.annotatedReady: pending entry %A{key} has no registration. FileDescriptorRegistryDefect.SocketEventReadyEntryUnregistered exists to make this unreachable, so this is an interpreter bug."

            let reported =
                epollReadinessOfDescription targetId kernel
                |> ReadinessLevel.reportedUnder registration.Interest

            key, registration, reported
        )

    /// Whether an `epoll_wait` on the port `portId` names would return at
    /// least one event right now — the readiness sweep's wake condition, and
    /// by construction the same question `deliverSocketEvents` answers,
    /// because both read the same annotated walk.
    ///
    /// Total in `portId`: a dead or non-port description answers `false`,
    /// because a thread can park on a port whose last descriptor later
    /// closes, and a real `epoll_wait` sleeps on regardless.
    let hasDeliverableSocketEvents (portId : OpenFileDescriptionId) (kernel : EmulatedKernel) : bool =
        match Map.tryFind portId (FileDescriptorRegistry.descriptions kernel.FileDescriptors) with
        | None -> false
        | Some description ->

        match description.Target with
        | OpenFileTarget.StandardStream _
        | OpenFileTarget.File _
        | OpenFileTarget.Socket _ -> false
        | OpenFileTarget.SocketEventPort portState ->
            annotatedReady portState kernel
            |> List.exists (fun (_, _, reported) -> not (ReadinessLevel.isEmpty reported))

    /// Drain the port as one `epoll_wait(maxevents = maxCount)` would: walk
    /// the pending entries in order, re-polling each; report the ones whose
    /// re-poll is nonempty, silently drop the stale ones, and stop once
    /// `maxCount` events are reported — every walked entry is consumed, and
    /// the entries the stop spared stay pending in order (measured,
    /// `order2.c` row J).
    ///
    /// Returns the reported rows — each the registration's `Data` and the
    /// reported readiness, in epoll's terms; the PAL-level conversion
    /// (`EPOLLHUP` folding into `EPOLLIN|EPOLLOUT`) is the caller's — and
    /// the kernel with the walked entries consumed.
    ///
    /// Loudly partial in `portId`: callers hold a live port description in
    /// hand.
    let deliverSocketEvents
        (portId : OpenFileDescriptionId)
        (maxCount : int)
        (kernel : EmulatedKernel)
        : (uint64 * ReadinessLevel) list * EmulatedKernel
        =
        if maxCount <= 0 then
            failwith
                $"EmulatedKernel.deliverSocketEvents: maxCount %d{maxCount} is not positive; epoll answers EINVAL for it before reaching the ready list, so this is an interpreter bug."

        match Map.tryFind portId (FileDescriptorRegistry.descriptions kernel.FileDescriptors) with
        | None ->
            failwith
                $"EmulatedKernel.deliverSocketEvents: %O{portId} names no live open file description (this is an interpreter bug)."
        | Some description ->

        match description.Target with
        | OpenFileTarget.StandardStream _
        | OpenFileTarget.File _
        | OpenFileTarget.Socket _ ->
            failwith
                $"EmulatedKernel.deliverSocketEvents: %O{portId} is not a socket event port (this is an interpreter bug)."
        | OpenFileTarget.SocketEventPort portState ->

        let rec walk
            (delivered : (uint64 * ReadinessLevel) list)
            (remaining : ((int * OpenFileDescriptionId) * SocketEventRegistration * ReadinessLevel) list)
            : (uint64 * ReadinessLevel) list * (int * OpenFileDescriptionId) list
            =
            match remaining with
            | [] -> List.rev delivered, []
            | (_, registration, reported) :: rest ->
                if List.length delivered = maxCount then
                    List.rev delivered, remaining |> List.map (fun (key, _, _) -> key)
                elif ReadinessLevel.isEmpty reported then
                    walk delivered rest
                else
                    walk ((registration.Data, reported) :: delivered) rest

        let delivered, surviving = walk [] (annotatedReady portState kernel)

        delivered,
        { kernel with
            FileDescriptors = FileDescriptorRegistry.setSocketEventReady portId surviving kernel.FileDescriptors
        }

    /// `SystemNative_TryChangeSocketEventRegistration` past the wrapper's
    /// screens: apply `change` to the port's interest table, and bring the
    /// ready list with it — an ADD or MOD whose target is ready under the
    /// *new* interest makes the registration pending at that moment (measured
    /// rows E, I and K: the entry enters at ADD/MOD time, and a MOD of an
    /// entry already pending leaves its place alone, row L).
    let changeSocketEventRegistration
        (portFd : int)
        (targetFd : int)
        (change : SocketEventRegistrationChange)
        (kernel : EmulatedKernel)
        : Result<EmulatedKernel, SocketEventRegistrationError>
        =
        let ordinal = kernel.NextSocketEventRegistrationOrdinal

        match
            FileDescriptorRegistry.changeSocketEventRegistration portFd targetFd ordinal change kernel.FileDescriptors
        with
        | Error error -> Error error
        | Ok registry ->

        let kernel =
            { kernel with
                FileDescriptors = registry
                Machine =
                    { kernel.Machine with
                        NextSocketEventRegistrationOrdinal =
                            match change with
                            | SocketEventRegistrationChange.Add _ -> ordinal + 1L
                            | SocketEventRegistrationChange.Modify _
                            | SocketEventRegistrationChange.Remove -> ordinal
                    }
            }

        match change with
        | SocketEventRegistrationChange.Remove -> Ok kernel
        | SocketEventRegistrationChange.Add (interest, _)
        | SocketEventRegistrationChange.Modify (interest, _) ->

        // Both fds resolved a moment ago inside the registry change, so these
        // lookups cannot miss.
        let portId =
            match FileDescriptorRegistry.tryFindId portFd kernel.FileDescriptors with
            | Some id -> id
            | None ->
                failwith
                    $"EmulatedKernel.changeSocketEventRegistration: port fd %d{portFd} was live moments ago (this is an interpreter bug)."

        let key, targetId =
            match FileDescriptorRegistry.tryFindId targetFd kernel.FileDescriptors with
            | Some id -> (targetFd, id), id
            | None ->
                failwith
                    $"EmulatedKernel.changeSocketEventRegistration: target fd %d{targetFd} was live moments ago (this is an interpreter bug)."

        let alreadyPending =
            match Map.tryFind portId (FileDescriptorRegistry.descriptions kernel.FileDescriptors) with
            | Some description ->
                match description.Target with
                | OpenFileTarget.SocketEventPort portState -> List.contains key portState.Ready
                | _ ->
                    failwith
                        $"EmulatedKernel.changeSocketEventRegistration: %O{portId} committed a registration change moments ago yet is not a socket event port (this is an interpreter bug)."
            | None ->
                failwith
                    $"EmulatedKernel.changeSocketEventRegistration: %O{portId} was live moments ago (this is an interpreter bug)."

        let readyNow =
            epollReadinessOfDescription targetId kernel
            |> ReadinessLevel.reportedUnder interest
            |> ReadinessLevel.isEmpty
            |> not

        if readyNow && not alreadyPending then
            Ok
                { kernel with
                    FileDescriptors = FileDescriptorRegistry.appendSocketEventReady portId key kernel.FileDescriptors
                }
        else
            Ok kernel

    /// Mirrors `socket(2)`: allocate a fresh socket, and a fresh descriptor onto
    /// it.
    ///
    /// One operation for both allocations, rather than a socket-table insert
    /// beside a separate `FileDescriptorRegistry.createSocket`, because the two
    /// must agree: the identity this mints is the identity the description
    /// names, and splitting them would let a caller do one without the other.
    ///
    /// Says nothing about whether this domain/kind/protocol combination *can*
    /// exist — `SimulatedUnixPlatform.socketCreation` answers that, and this is
    /// reached only once it has said yes.
    let createSocket
        (domain : SocketDomain)
        (kind : SocketKind)
        (protocol : SocketProtocol)
        (kernel : EmulatedKernel)
        : int * EmulatedKernel
        =
        let socketId = kernel.NextSocketId
        let (SocketId raw) = socketId

        let fd, registry =
            FileDescriptorRegistry.createSocket socketId kernel.FileDescriptors

        fd,
        { kernel with
            FileDescriptors = registry
            Machine =
                { kernel.Machine with
                    Sockets =
                        Map.add
                            socketId
                            {
                                Domain = domain
                                Kind = kind
                                Protocol = protocol
                                // `socket(2)` binds nothing and connects nothing.
                                Binding = None
                                Phase = SocketPhase.Idle
                                ReuseAddress = false
                            }
                            kernel.Sockets
                    NextSocketId = SocketId (raw + 1L)
                }
        }

    /// Every inode this kernel holds a reference to *directly*, independently of
    /// any name the filesystem binds to it.
    ///
    /// A real kernel keeps an inode alive while any reference survives; this
    /// enumerates the references PawPrint has of its own. Every live open file
    /// description onto a file is one, and so is the current directory — a
    /// process that has `chdir`ed somewhere keeps that directory alive whether
    /// or not its name outlives the call.
    ///
    /// Everything that can *create* a reference must appear here: an omission
    /// makes a live inode look free, and freeing it leaves a descriptor pointing
    /// at nothing. It is not what callers want, though — see `pinnedInodes`,
    /// which adds the references the *filesystem* holds on behalf of these.
    let heldInodes (kernel : EmulatedKernel) : Set<InodeNumber> =
        kernel.FileDescriptors
        |> FileDescriptorRegistry.descriptions
        |> Map.toSeq
        |> Seq.choose (fun (_, description) ->
            match description.Target with
            | OpenFileTarget.File (inode, _) -> Some inode
            | OpenFileTarget.StandardStream _
            | OpenFileTarget.Socket _
            | OpenFileTarget.SocketEventPort _ -> None
        )
        |> Set.ofSeq
        |> Set.add kernel.CurrentDirectoryInode
        // An open directory stream holds its directory too. The descriptor it
        // opened already does, so this adds nothing while the stream is intact
        // — it is here for the guest that closes that descriptor out from under
        // the stream, which is undefined behaviour on a real libc but a
        // guessable fd number away here. Without it the next `readdir` would
        // reach a reaped inode and crash the interpreter.
        |> Set.union (
            kernel.DirectoryStreams
            |> Map.toSeq
            |> Seq.map (fun (_, stream) -> stream.Inode)
            |> Set.ofSeq
        )

    /// Every inode that must not be freed: `heldInodes`, closed under
    /// `DirectoryContent.Parent`.
    ///
    /// The closure is not caution — it is measured. `rmdir` can remove a
    /// directory something still holds, and that orphan keeps its "..": probed
    /// on both flavours, with `a/b` and the current directory inside `b`,
    /// `rmdir(b)` then `rmdir(a)` both succeed and `stat("..")` still answers
    /// `a`'s inode while `stat("../..")` still answers the live grandparent's.
    /// So a held orphan holds its whole ancestor chain, and freeing one of them
    /// would leave a `DirectoryContent.Parent` naming an inode the graph no
    /// longer contains.
    ///
    /// This is the set `VirtualFileSystem.checkInvariants` takes as `pinned`,
    /// and the check `forgetIfUnheld` makes before freeing an inode. Ancestors
    /// that are still reachable from the root are in it too, harmlessly: both
    /// callers only ever ask about an inode no name reaches.
    let pinnedInodes (kernel : EmulatedKernel) : Set<InodeNumber> =
        let rec climb (frontier : InodeNumber list) (seen : Set<InodeNumber>) : Set<InodeNumber> =
            match frontier with
            | [] -> seen
            | inode :: rest ->
                if Set.contains inode seen then
                    climb rest seen
                else

                let seen = Set.add inode seen

                match VirtualFileSystem.tryGetContent inode kernel.FileSystem with
                | Some (InodeContent.Directory directory) -> climb (directory.Parent :: rest) seen
                // A file or a link records no parent, and a held inode the graph
                // has already forgotten records nothing at all — which is a
                // defect (`EmulatedKernelDefect.DanglingOpenInode`) rather than
                // something to climb from.
                | Some (InodeContent.RegularFile _)
                | Some (InodeContent.Symlink _)
                | None -> climb rest seen

        climb (heldInodes kernel |> Set.toList) Set.empty

    /// Free `inode` if the filesystem no longer names it and this kernel holds
    /// no reference to it — what a real kernel does once the last link and the
    /// last descriptor have both gone.
    ///
    /// Total and idempotent: an inode that still has a name, that something
    /// still holds, or that is already gone, is left exactly as it was. Call it
    /// after anything that can drop a reference of either kind — removing a
    /// name, and closing a descriptor — because either may be the one that
    /// finishes the job, and which one that is cannot be known from the call
    /// site.
    ///
    /// Freeing a *directory* cascades onto its recorded parent, which the
    /// directory's ".." was the last reference to. So one call collects a whole
    /// orphaned chain, and the caller passes only the inode whose reference it
    /// just dropped.
    let rec forgetIfUnheld (inode : InodeNumber) (kernel : EmulatedKernel) : EmulatedKernel =
        // The root is excluded explicitly rather than by the binding count,
        // which is zero for it by construction: nothing holds an entry naming
        // the root (`VirtualFileSystemDefect.RootHasIncomingLink` states that),
        // so the count alone would free the filesystem out from under every
        // path. A guest can reach here with it — `close(open("/"))` is an
        // ordinary thing to do.
        if inode = VirtualFileSystem.root kernel.FileSystem then
            kernel
        elif (VirtualFileSystem.tryGet inode kernel.FileSystem).IsNone then
            kernel
        elif VirtualFileSystem.bindingCount inode kernel.FileSystem <> 0 then
            kernel
        elif Set.contains inode (pinnedInodes kernel) then
            kernel
        else

        // Read before the removal, because it is the removal that makes the
        // parent's own reference count drop.
        let parent =
            match VirtualFileSystem.tryGetContent inode kernel.FileSystem with
            | Some (InodeContent.Directory directory) -> Some directory.Parent
            | Some (InodeContent.RegularFile _)
            | Some (InodeContent.Symlink _)
            | None -> None

        let freed =
            { kernel with
                Machine =
                    { kernel.Machine with
                        FileSystem = VirtualFileSystem.forget inode kernel.FileSystem
                    }
            }

        // A directory freed here was the last thing holding its parent's ".."
        // reference, so the parent may now be free in turn — the chain a held
        // orphan kept alive is collected as soon as the last holder goes.
        // Terminating: each step has removed one inode, and the root is refused
        // above.
        match parent with
        | None -> freed
        | Some parent -> forgetIfUnheld parent freed

    /// Mirrors `close(2)`, including the kernel objects a description was the
    /// last reference to: the socket it named, or the inode whose last name had
    /// already gone.
    ///
    /// `FileDescriptorRegistry.close` cannot do this itself: the socket table
    /// lives here, in a file that compiles after it, and whether an inode is
    /// still named is a question about the filesystem. Closing one of several
    /// descriptors onto a description destroys nothing, and so frees neither.
    let closeFd (fd : int) (kernel : EmulatedKernel) : Result<EmulatedKernel, FileDescriptorCloseError> =
        // Resolved before the close so both refusals below can name what the
        // fd referred to.
        let closing = FileDescriptorRegistry.tryFindWithId fd kernel.FileDescriptors

        match FileDescriptorRegistry.close fd kernel.FileDescriptors with
        | Error e -> Error e
        | Ok (registry, destroyed) ->
            // Closing a descriptor onto a port with an in-flight
            // SystemNative_WaitForSocketEvents is where the flavours part,
            // and each side is measured (SocketEventWaitSurvivesCloseLinux.cs
            // and its macOS run):
            //
            //   * Linux's epoll_wait holds the port by file reference — a
            //     close that leaves a dup changes nothing, and even the last
            //     close leaves the in-flight syscall's registrations alive
            //     for a later edge to complete. The dup case is modelled
            //     (the description survives and the wait completes); the
            //     last-close case would need retention this table does not
            //     represent, so it refuses.
            //   * Darwin's kevent *ends* with an error when the fd it was
            //     entered through closes (measured; which error, and what a
            //     close of a different descriptor onto the same kqueue does,
            //     are not), so any such close refuses.
            //
            // Checked against the in-flight wait map rather than thread
            // status, so the window between a wake and the woken thread's
            // re-entry is covered too.
            (match closing with
             | Some (closingId, description) ->
                 (match description.Target with
                  | OpenFileTarget.SocketEventPort _ ->
                      let waiter =
                          kernel.ParkedSocketWaits
                          |> Map.tryPick (fun thread wait -> if wait.Port = closingId then Some thread else None)

                      match waiter with
                      | None -> ()
                      | Some thread ->
                          match SimulatedUnixPlatform.flavour kernel.UnixPlatform with
                          | SimulatedUnixFlavour.Linux ->
                              if destroyed.IsSome then
                                  failwith
                                      $"EmulatedKernel.closeFd: fd %d{fd} was the last descriptor onto socket event port %O{closingId}, and thread %O{thread} has an in-flight SystemNative_WaitForSocketEvents on it. A real close leaves the in-flight epoll_wait holding the port — its registrations stay live and a later edge can still complete the wait — which PawPrint's descriptor table cannot represent. Implement port retention for in-flight waits before closing one out from under a waiter."
                          | SimulatedUnixFlavour.Darwin ->
                              failwith
                                  $"EmulatedKernel.closeFd: fd %d{fd} names socket event port %O{closingId}, and thread %O{thread} has an in-flight SystemNative_WaitForSocketEvents on it. Measured, Darwin's kevent ends such a wait with an error when the fd it was entered through closes — an error PawPrint has not measured precisely, and closing a different descriptor onto the kqueue is unmeasured entirely. Measure what the woken wait reports before closing a kqueue out from under a waiter."
                  | OpenFileTarget.StandardStream _
                  | OpenFileTarget.File _
                  | OpenFileTarget.Socket _ -> ())
             | None -> ())

            let sockets, connections, establishedSurvivors =
                match destroyed with
                | Some description ->
                    match description.Target with
                    | OpenFileTarget.Socket socketId ->
                        let dying =
                            match Map.tryFind socketId kernel.Sockets with
                            | Some socket -> socket
                            | None ->
                                failwith
                                    $"EmulatedKernel.closeFd: fd %d{fd}'s description names socket %O{socketId}, which the socket table does not hold. EmulatedKernelDefect.DanglingSocket exists to make this unreachable, so this is an interpreter bug."

                        let sockets = Map.remove socketId kernel.Sockets

                        // A connection lives while any socket phase or accept
                        // queue references it. The dying socket may have been
                        // the last such reference — directly, or by being the
                        // listener whose queue held it (the queue dies with
                        // the listener, as Linux's inet_csk_listen_stop
                        // discards a closed listener's accept queue).
                        let candidates =
                            match dying.Phase with
                            | SocketPhase.Established connection
                            | SocketPhase.EstablishedPendingReport connection -> [ connection ]
                            | SocketPhase.Listening listenState -> listenState.Queue
                            | SocketPhase.Idle
                            | SocketPhase.RefusedPendingDelivery
                            | SocketPhase.Dead
                            | SocketPhase.DatagramPeer _ -> []

                        let stillReferenced (connection : ConnectionId) : bool =
                            sockets
                            |> Map.exists (fun _ survivor ->
                                match survivor.Phase with
                                | SocketPhase.Established c
                                | SocketPhase.EstablishedPendingReport c -> c = connection
                                | SocketPhase.Listening listenState -> List.contains connection listenState.Queue
                                | SocketPhase.Idle
                                | SocketPhase.RefusedPendingDelivery
                                | SocketPhase.Dead
                                | SocketPhase.DatagramPeer _ -> false
                            )

                        // What this close does to the sockets sharing the
                        // dying socket's connections splits by which end is
                        // dying. The peer of an established pair sees the
                        // FIN: its level becomes the measured half-closed
                        // IN|OUT|RDHUP and the driver signals it (`order3.c`
                        // row Q) — collected here and signalled below, once
                        // the socket table reflects the close, so the level
                        // the signal filters against is the survivor's new
                        // one. A dying *listener* instead RSTs its unaccepted
                        // queue entries' clients, whose resulting level is
                        // unmeasured — that case refuses when a registration
                        // could observe it, and an RST raises ERR, which no
                        // interest mask can hide, so any registration could.
                        let establishedSurvivors =
                            match dying.Phase with
                            | SocketPhase.Established _
                            | SocketPhase.EstablishedPendingReport _ ->
                                sockets
                                |> Map.toList
                                |> List.choose (fun (survivorId, survivor) ->
                                    match survivor.Phase with
                                    | SocketPhase.Established c
                                    | SocketPhase.EstablishedPendingReport c when List.contains c candidates ->
                                        Some survivorId
                                    | _ -> None
                                )
                            | SocketPhase.Listening _ ->
                                for candidate in candidates do
                                    let liveClients =
                                        sockets
                                        |> Map.toSeq
                                        |> Seq.filter (fun (_, survivor) ->
                                            match survivor.Phase with
                                            | SocketPhase.Established c
                                            | SocketPhase.EstablishedPendingReport c -> c = candidate
                                            | SocketPhase.Listening _
                                            | SocketPhase.Idle
                                            | SocketPhase.RefusedPendingDelivery
                                            | SocketPhase.Dead
                                            | SocketPhase.DatagramPeer _ -> false
                                        )
                                        |> Seq.map fst
                                        |> List.ofSeq

                                    match liveClients with
                                    | [] -> ()
                                    | survivor :: _ ->
                                        failwith
                                            $"EmulatedKernel.closeFd: closing fd %d{fd} destroys listening socket %O{socketId} while connection %O{candidate} sits unaccepted in its queue, and that connection's client (socket %O{survivor}) is still open. A real kernel RSTs the unaccepted client on listener close, leaving it in a state PawPrint has not measured — its readiness level, and what connect(2) then answers, are both unknown, and the client would otherwise be indistinguishable from a cleanly FIN'd peer. Accept the connection or close the client before closing the listener."

                                []
                            | SocketPhase.Idle
                            | SocketPhase.RefusedPendingDelivery
                            | SocketPhase.Dead
                            | SocketPhase.DatagramPeer _ -> []

                        let connections =
                            (kernel.Connections, candidates)
                            ||> List.fold (fun connections connection ->
                                if stillReferenced connection then
                                    connections
                                else
                                    Map.remove connection connections
                            )

                        sockets, connections, establishedSurvivors
                    | OpenFileTarget.StandardStream _
                    | OpenFileTarget.SocketEventPort _
                    | OpenFileTarget.File _ -> kernel.Sockets, kernel.Connections, []
                | None -> kernel.Sockets, kernel.Connections, []

            let closed =
                { kernel with
                    FileDescriptors = registry
                    Machine =
                        { kernel.Machine with
                            Sockets = sockets
                            Connections = connections
                        }
                }

            // The FIN's edge, raised now that the survivor's level is the
            // half-closed one. The signal filters by each registration's
            // interest, so a survivor nobody watches — or one watched only
            // for conditions the half-closed level does not meet — records
            // nothing.
            let closed =
                (closed, establishedSurvivors)
                ||> List.fold (fun k s -> signalSocketStateChange s k)

            // The close may have been the last reference to an inode whose last
            // name went away earlier, which is what keeps `read` on an unlinked
            // descriptor working right up until the descriptor goes. Reaped
            // against the *closed* kernel, so this description no longer counts
            // as holding it.
            let reaped =
                match destroyed with
                | Some description ->
                    match description.Target with
                    | OpenFileTarget.File (inode, _) -> forgetIfUnheld inode closed
                    | OpenFileTarget.StandardStream _
                    | OpenFileTarget.SocketEventPort _
                    | OpenFileTarget.Socket _ -> closed
                | None -> closed

            Ok reaped

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

    /// Record a newly-opened stream, minting its id and binding `block` — the
    /// native block whose address the guest holds as its `DIR*` — to it.
    let withNewDirectoryStream
        (block : NativeMemoryBlockId)
        (stream : DirectoryStream)
        (kernel : EmulatedKernel)
        : EmulatedKernel
        =
        let id = kernel.NextDirectoryStreamId
        let (DirectoryStreamId raw) = id

        { kernel with
            DirectoryStreams = Map.add id stream kernel.DirectoryStreams
            DirectoryStreamBlocks = Map.add block id kernel.DirectoryStreamBlocks
            NextDirectoryStreamId = DirectoryStreamId (raw + 1L)
        }

    /// Move a stream's cursor on, leaving everything else about it alone.
    // Updates the stream in place under its existing id rather than going
    // through `withNewDirectoryStream`: a `readdir` must not mint a second id
    // for a stream that is already open.
    let withDirectoryCursor
        (block : NativeMemoryBlockId)
        (cursor : DirectoryCursor)
        (kernel : EmulatedKernel)
        : EmulatedKernel
        =
        let id = directoryStreamId block kernel
        let stream = directoryStream block kernel

        { kernel with
            DirectoryStreams =
                Map.add
                    id
                    { stream with
                        Cursor = cursor
                    }
                    kernel.DirectoryStreams
        }

    /// Forget a stream, which `SystemNative_CloseDir` does before closing the
    /// descriptor under it — that order matters, because the close is what
    /// reaps an orphaned directory and this entry is one of the things holding
    /// it.
    let withoutDirectoryStream (block : NativeMemoryBlockId) (kernel : EmulatedKernel) : EmulatedKernel =
        let id = directoryStreamId block kernel

        { kernel with
            DirectoryStreams = Map.remove id kernel.DirectoryStreams
            DirectoryStreamBlocks = Map.remove block kernel.DirectoryStreamBlocks
        }

    /// The connection `connectionId` names.
    ///
    /// Total, and loudly partial rather than an option: every `ConnectionId` a
    /// caller can hold came out of a socket phase or an accept queue, and
    /// `checkInvariants` rejects a kernel in which one of those dangles.
    let connection (connectionId : ConnectionId) (kernel : EmulatedKernel) : TcpConnection =
        match Map.tryFind connectionId kernel.Connections with
        | Some connection -> connection
        | None ->
            failwith
                $"EmulatedKernel.connection: %O{connectionId} names no connection in this kernel's connection table. EmulatedKernelDefect.DanglingConnection and DanglingQueuedConnection exist to make this unreachable, so this is an interpreter bug."

    /// One `connect(2)` call's answer: PAL SUCCESS, or a failure carrying the
    /// error the syscall left in errno. EINPROGRESS is a `Failed` like any
    /// other — the wrapper maps it to a PAL return and errno just the same —
    /// and the outcome it defers is already latched on the socket's phase.
    [<RequireQualifiedAccess>]
    type ConnectOutcome =
        | Completed
        | Failed of UnixError

    /// `connect(2)` past the wrapper's screens and the copy-in faults, which
    /// stay with the caller (they are about guest memory, which this module
    /// cannot see): the per-flavour ladder over the socket's phase, the
    /// declared length, the sockaddr family, and the destination.
    ///
    /// `family` (the *platform* family number) and `destination` are `None`
    /// when the declared length does not reach the field — this function only
    /// ever answers for an unreadable field, never reads one.
    ///
    /// Every answered row is measured (`connect_probe.c` and successors,
    /// 2026-08-21; docs/plans/2026-08-21-socket-connect.md holds the table);
    /// the failwiths name the unmeasured or unmodellable inputs.
    let connectSocket
        (socketId : SocketId)
        (nonBlocking : bool)
        (declaredLength : int)
        (family : int option)
        (destination : InternetEndpoint option)
        (kernel : EmulatedKernel)
        : ConnectOutcome * EmulatedKernel
        =
        let sock = socket socketId kernel
        let platform = kernel.UnixPlatform
        let flavour = SimulatedUnixPlatform.flavour platform
        let exactSize = (SimulatedUnixPlatform.socketAddressSizes platform).InterNetwork

        // connect(2) copies the sockaddr in through the same helpers bind(2)
        // uses (Linux's move_addr_to_kernel, Darwin's getsockaddr), and the
        // measured lengths agree with bind's rule exactly: Linux takes 16
        // through 128 and answers EINVAL outside, Darwin takes exactly 16,
        // EINVAL otherwise and ENAMETOOLONG past 255. So the verdict function
        // is shared.
        let lengthVerdict =
            SimulatedUnixPlatform.bindAddressLength platform exactSize declaredLength

        let fail (error : UnixError) : ConnectOutcome * EmulatedKernel = ConnectOutcome.Failed error, kernel

        let withPhase (phase : SocketPhase) (kernel : EmulatedKernel) : EmulatedKernel =
            { kernel with
                Machine =
                    { kernel.Machine with
                        Sockets =
                            Map.add
                                socketId
                                { sock with
                                    Phase = phase
                                }
                                kernel.Sockets
                    }
            }

        let destinationIsLocal (address : uint32) : bool =
            List.contains address kernel.LocalAddresses
            || kernel.LocalRoutes |> List.exists (Ipv4Prefix.contains address)

        // What a refusal delivery leaves in the socket's binding. Measured
        // for all three provenances (implicit, bind(2) to 127.0.0.1, bind(2)
        // to 0.0.0.0): Darwin keeps the resolved source; Linux's reset
        // reverts the address to whatever bind(2) locked — the wildcard when
        // the address only ever came from source resolution — while keeping
        // the port.
        let bindingAfterRefusalDelivery (flavour : SimulatedUnixFlavour) (binding : SocketBinding) : SocketBinding =
            match flavour with
            | SimulatedUnixFlavour.Darwin -> binding
            | SimulatedUnixFlavour.Linux ->
                { binding with
                    Endpoint =
                        { binding.Endpoint with
                            Address = binding.LockedAddress |> Option.defaultValue InternetEndpoint.WildcardAddress
                        }
                }

        // connect(2)'s implicit bind, when the socket has no local address
        // yet: loopback source, ephemeral port, the same conflict rule as
        // bind(2)'s own port-0 path. The source address for a non-loopback
        // destination is the route's preferred source, which is unmeasured,
        // so that input is refused.
        let ensureBound (dest : InternetEndpoint) (kernel : EmulatedKernel) : SocketBinding * EmulatedKernel =
            match sock.Binding with
            | Some binding when binding.Endpoint.Address <> InternetEndpoint.WildcardAddress -> binding, kernel
            | Some binding ->
                // A client bound to the wildcard gets a concrete source
                // address at connect — measured on both kernels, TCP and UDP
                // alike: the address becomes 127.0.0.1 for a loopback
                // destination and the port is kept, and getsockname reports
                // the rewrite afterwards, so the *binding* itself changes
                // rather than merely the connection's record of it. Which
                // source a kernel picks for any other destination is
                // unmeasured.
                if dest.Address <> InternetEndpoint.LoopbackAddress then
                    failwith
                        $"SystemNative_Connect: a socket bound to the wildcard is connecting to %s{InternetEndpoint.toString dest}, and which source address a kernel resolves the wildcard to for a destination other than 127.0.0.1 is unmeasured. Bind to a concrete address first, or connect to 127.0.0.1."

                { binding with
                    Endpoint =
                        { binding.Endpoint with
                            Address = InternetEndpoint.LoopbackAddress
                        }
                },
                kernel
            | None ->

            if dest.Address <> InternetEndpoint.LoopbackAddress then
                failwith
                    $"SystemNative_Connect: an unbound socket is connecting to %s{InternetEndpoint.toString dest}, and which source address a kernel picks for a destination other than 127.0.0.1 is unmeasured. Bind the socket first, or connect to 127.0.0.1."

            let candidate (port : uint16) : SocketBinding =
                {
                    Endpoint = InternetEndpoint.ofParts InternetEndpoint.LoopbackAddress port
                    // No bind(2) ran: a Linux refusal delivery reverts the
                    // address all the way to the wildcard.
                    LockedAddress = None
                }

            let acceptable (port : uint16) : bool =
                not (
                    kernel.Sockets
                    |> Map.exists (fun otherId other ->
                        if otherId = socketId then
                            false
                        else

                        match other.Binding with
                        | None -> false
                        | Some existing ->
                            other.Kind = sock.Kind
                            && SimulatedUnixPlatform.bindConflict
                                platform
                                existing
                                other.ReuseAddress
                                other.Phase
                                (candidate port)
                                sock.ReuseAddress
                    )
                )
                // A connection can outlive the socket that opened it (a
                // closed client whose connection sits queued or accepted),
                // and its four-tuple stays occupied for this destination
                // even though no socket holds the port any more. A real
                // kernel's connect-time port selection skips such tuples,
                // so the allocator must too, in either orientation.
                && not (
                    kernel.Connections
                    |> Map.exists (fun _ connection ->
                        let endpoint = (candidate port).Endpoint

                        (connection.ClientAddress = endpoint && connection.ServerAddress = dest)
                        || (connection.ClientAddress = dest && connection.ServerAddress = endpoint)
                    )
                )

            match allocateEphemeralPort acceptable kernel with
            | Some (port, kernel) -> candidate port, kernel
            | None ->
                let low, high = kernel.EphemeralPortRange

                failwith
                    $"SystemNative_Connect: every port in the ephemeral range %d{low}-%d{high} is taken, so this implicit bind has no answer. Widen KernelConfig.EphemeralPortRange, or measure what a real kernel says here."

        // The established/refused attempt, shared by both flavours once the
        // per-flavour screens have let an idle stream socket through.
        let attemptStream (dest : InternetEndpoint) : ConnectOutcome * EmulatedKernel =
            // A wildcard destination means loopback: measured on both,
            // connect to 0.0.0.0:port reaches a loopback listener.
            let dest =
                if dest.Address = InternetEndpoint.WildcardAddress then
                    { dest with
                        Address = InternetEndpoint.LoopbackAddress
                    }
                else
                    dest

            if not (destinationIsLocal dest.Address) then
                failwith
                    $"SystemNative_Connect: destination %s{InternetEndpoint.toString dest} is not a local address of this simulated machine, and PawPrint models no network to carry a SYN anywhere else. Add the address to the kernel's LocalAddresses/LocalRoutes if it should be local, or connect to loopback."

            let listeners =
                kernel.Sockets
                |> Map.toList
                |> List.choose (fun (otherId, other) ->
                    match other.Phase with
                    | SocketPhase.Listening listenState ->
                        match other.Binding with
                        | Some binding when
                            other.Kind = SocketKind.Stream
                            && binding.Endpoint.Port = dest.Port
                            && (binding.Endpoint.Address = dest.Address
                                || InternetEndpoint.isWildcard binding.Endpoint)
                            ->
                            Some (otherId, other, listenState, binding)
                        | _ -> None
                    | _ -> None
                )

            // A specific-address listener beats the wildcard — both kernels'
            // documented most-specific-match rule. The pair can only coexist
            // under SO_REUSEADDR, which no current guest exercises, so the
            // preference has no observer today and is recorded for when it
            // does.
            let listener =
                match
                    listeners
                    |> List.tryFind (fun (_, _, _, binding) -> not (InternetEndpoint.isWildcard binding.Endpoint))
                with
                | Some found -> Some found
                | None -> List.tryHead listeners

            match listener with
            | Some (listenerId, listenerSocket, listenState, _) ->
                // Int64, so that the Linux `+ 1` cannot wrap when the
                // configured somaxconn is itself Int32.MaxValue.
                let capacity : int64 =
                    match flavour with
                    | SimulatedUnixFlavour.Linux ->
                        // Measured, with the sysctl set to 3 to bring the
                        // boundary in reach: listen(0) admits 1, listen(1)
                        // admits 2, listen(5) admits 6, and listen(-1) and
                        // listen(INT_MAX) both admit somaxconn + 1 — the
                        // kernel compares the backlog *unsigned* against
                        // somaxconn and clamps, and the queue then admits
                        // one more than the clamped value. The clamp also
                        // keeps the `+ 1` from overflowing on the
                        // Int32.MaxValue a parameterless Socket.Listen()
                        // passes.
                        let clamped =
                            if listenState.Backlog < 0 || listenState.Backlog > kernel.SoMaxConn then
                                kernel.SoMaxConn
                            else
                                listenState.Backlog

                        int64 clamped + 1L
                    | SimulatedUnixFlavour.Darwin ->
                        // Measured at the default sysctl of 128: listen(1)
                        // admits 1, listen(5) admits 5, and listen(0),
                        // listen(-1) and listen(INT_MAX) all admit exactly
                        // somaxconn — a non-positive or over-large backlog
                        // clamps to somaxconn, and the queue admits exactly
                        // the clamped value.
                        if listenState.Backlog <= 0 || listenState.Backlog > kernel.SoMaxConn then
                            int64 kernel.SoMaxConn
                        else
                            int64 listenState.Backlog

                if int64 (List.length listenState.Queue) >= capacity then
                    failwith
                        $"SystemNative_Connect: the accept queue of the listener at %s{InternetEndpoint.toString dest} already holds %d{List.length listenState.Queue} connections, its measured capacity. A real kernel leaves this SYN unanswered and the client retries on a timer — timing PawPrint cannot honour deterministically — so this connect has no faithful answer. Accept from the listener before connecting again, or listen with a larger backlog."

                let clientBinding, kernel = ensureBound dest kernel

                // Two corners a REUSEADDR-bound client can engineer, each
                // refused because the real answer is unmeasured (no managed
                // path reaches either: managed clients connect from fresh
                // ephemeral ports).
                if clientBinding.Endpoint = dest then
                    // A wildcard listener at P beside a reuse-bound client at
                    // 127.0.0.1:P, connecting to 127.0.0.1:P: source equals
                    // destination even though a listener matched.
                    failwith
                        $"SystemNative_Connect: the resolved source %s{InternetEndpoint.toString clientBinding.Endpoint} equals the destination, with a listener present. What a real kernel does with this self-tuple (plausibly EINVAL on Darwin, a completed self-connect on Linux) is unmeasured, so measure it rather than guessing."

                if
                    kernel.Connections
                    |> Map.exists (fun _ connection ->
                        // In either orientation: a connection's endpoint
                        // pair occupies the tuple from both ends.
                        (connection.ClientAddress = clientBinding.Endpoint
                         && connection.ServerAddress = dest)
                        || (connection.ClientAddress = dest
                            && connection.ServerAddress = clientBinding.Endpoint)
                    )
                then
                    // Established tuples are unique in a real kernel; a second
                    // identical (source, destination) pair — two clients
                    // reuse-bound to one source endpoint, connecting to one
                    // listener — is refused there (plausibly EADDRINUSE),
                    // which is unmeasured.
                    failwith
                        $"SystemNative_Connect: a connection from %s{InternetEndpoint.toString clientBinding.Endpoint} to %s{InternetEndpoint.toString dest} already exists, and a real kernel refuses a duplicate four-tuple in ways that are unmeasured (plausibly EADDRINUSE at connect time). Measure it rather than guessing."

                let connectionId = kernel.NextConnectionId
                let (ConnectionId rawConnectionId) = connectionId

                let tcpConnection =
                    {
                        ClientAddress = clientBinding.Endpoint
                        ServerAddress = dest
                    }

                let clientPhase =
                    if not nonBlocking then
                        SocketPhase.Established connectionId
                    else
                        match flavour with
                        | SimulatedUnixFlavour.Linux ->
                            // The next connect reports the completion with
                            // one SUCCESS (measured), which is what this
                            // phase defers.
                            SocketPhase.EstablishedPendingReport connectionId
                        | SimulatedUnixFlavour.Darwin ->
                            // Darwin's retry answers EISCONN directly
                            // (measured), so nothing is deferred.
                            SocketPhase.Established connectionId

                let kernel =
                    { kernel with
                        Machine =
                            { kernel.Machine with
                                Sockets =
                                    kernel.Sockets
                                    |> Map.add
                                        socketId
                                        { sock with
                                            Binding = Some clientBinding
                                            Phase = clientPhase
                                        }
                                    |> Map.add
                                        listenerId
                                        { listenerSocket with
                                            Phase =
                                                SocketPhase.Listening
                                                    { listenState with
                                                        // Oldest first: accept(2)
                                                        // dequeues the head.
                                                        Queue = listenState.Queue @ [ connectionId ]
                                                    }
                                        }
                                Connections = Map.add connectionId tcpConnection kernel.Connections
                                NextConnectionId = ConnectionId (rawConnectionId + 1L)
                            }
                    }

                // The two edges this call raises, in the measured order
                // (`order7.c`, three runs): the client's completion enters
                // the ready list *before* the listener's accept edge — the
                // client processes the SYN-ACK and becomes writable before
                // its final ACK puts the child on the accept queue. The
                // client's phase resolves in this call whether or not the
                // syscall's own answer is deferred to EINPROGRESS.
                let kernel =
                    kernel |> signalSocketStateChange socketId |> signalSocketDataReady listenerId

                if nonBlocking then
                    // The syscall itself still answers EINPROGRESS —
                    // measured on both kernels, even on loopback — and the
                    // completion is what the phase above latches.
                    ConnectOutcome.Failed UnixError.EINPROGRESS, kernel
                else
                    ConnectOutcome.Completed, kernel
            | None ->
                // The client's own endpoint with no listener behind it is
                // TCP simultaneous open: a real kernel can complete it,
                // connecting the socket to itself. Unmodelled.
                match sock.Binding with
                | Some binding when
                    binding.Endpoint.Port = dest.Port
                    && InternetEndpoint.addressesOverlap binding.Endpoint dest
                    ->
                    failwith
                        $"SystemNative_Connect: destination %s{InternetEndpoint.toString dest} is this socket's own bound address and nothing is listening there. A real kernel can complete this as a TCP simultaneous open — connecting the socket to itself — which PawPrint does not model."
                | _ ->

                match flavour with
                | SimulatedUnixFlavour.Darwin when
                    kernel.Sockets
                    |> Map.exists (fun otherId other ->
                        otherId <> socketId
                        && other.Kind = SocketKind.Stream
                        // Only a bound-but-unconnected socket makes Darwin
                        // drop the SYN. A port held by established ends
                        // (their pcbs are keyed by the full peer tuple) or
                        // by a refused socket answers RST — measured, both
                        // refuse like a closed port.
                        && (
                            match other.Phase with
                            | SocketPhase.Idle -> true
                            | _ -> false
                        )
                        && (
                            match other.Binding with
                            | Some binding ->
                                binding.Endpoint.Port = dest.Port
                                && InternetEndpoint.addressesOverlap binding.Endpoint dest
                            | None -> false
                        )
                    )
                    ->
                    failwith
                        $"SystemNative_Connect: destination %s{InternetEndpoint.toString dest} is bound but nothing is listening there, and Darwin *drops* such a SYN rather than answering RST: the connect pends on the client's retransmission schedule (a blocking one was measured to stall into ETIMEDOUT), which PawPrint cannot honour deterministically. Listen on the destination socket, or connect to a fully closed port."
                | _ ->

                // The implicit bind happens before the SYN, so a refused
                // socket has a concrete local endpoint too — measured,
                // getsockname reports 127.0.0.1 and a nonzero port while the
                // refusal is pending, on both kernels.
                let binding, kernel = ensureBound dest kernel

                if not nonBlocking then
                    // The refusal is delivered inline, and the socket's fate
                    // diverges by flavour exactly as for the deferred
                    // delivery below: measured, a Linux retry is a fresh
                    // attempt and a Darwin one answers EINVAL forever.
                    let phase =
                        match flavour with
                        | SimulatedUnixFlavour.Linux -> SocketPhase.Idle
                        | SimulatedUnixFlavour.Darwin -> SocketPhase.Dead

                    let kernel =
                        { kernel with
                            Machine =
                                { kernel.Machine with
                                    Sockets =
                                        Map.add
                                            socketId
                                            { sock with
                                                Binding = Some (bindingAfterRefusalDelivery flavour binding)
                                                Phase = phase
                                            }
                                            kernel.Sockets
                                }
                        }

                    // The error's arrival and its reset both signal
                    // (measured separately for the deferred path, `order3.c`
                    // row M); inline delivery collapses them into this one
                    // state change, so one signal carries both.
                    let kernel = signalSocketStateChange socketId kernel

                    ConnectOutcome.Failed UnixError.ECONNREFUSED, kernel
                else
                    // EINPROGRESS now; the first later connect delivers
                    // ECONNREFUSED. Measured on both — with no SO_ERROR read
                    // in between, which would consume the pending error and
                    // change these answers; GetSocketErrorOption is not
                    // modelled yet, so only this path is reachable.
                    let kernel =
                        { kernel with
                            Machine =
                                { kernel.Machine with
                                    Sockets =
                                        Map.add
                                            socketId
                                            { sock with
                                                Binding = Some binding
                                                Phase = SocketPhase.RefusedPendingDelivery
                                            }
                                            kernel.Sockets
                                }
                        }

                    // The error's arrival signals the client (measured,
                    // `order3.c` row M: the 0x201d edge).
                    let kernel = signalSocketStateChange socketId kernel

                    ConnectOutcome.Failed UnixError.EINPROGRESS, kernel

        match sock.Kind with
        | SocketKind.Raw
        | SocketKind.SeqPacket ->
            failwith
                $"SystemNative_Connect: socket %O{socketId} is a %O{sock.Kind} socket, and what connect(2) does for one is unmeasured, so measure it rather than guessing."
        | SocketKind.Stream ->
            // The copy layer answers before any socket state on both
            // flavours: Linux's move_addr_to_kernel rejects an oversized
            // sockaddr and Darwin's getsockaddr rejects both bounds, each in
            // the syscall layer ahead of the protocol's own checks.
            match lengthVerdict with
            | BindLengthVerdict.RejectedBeforeCopy error -> fail error
            | BindLengthVerdict.Accepted
            | BindLengthVerdict.Invalid ->

            match family with
            | None ->
                // Too short to carry the family: EINVAL on both — Linux in
                // inet_stream_connect's first screen, Darwin in getsockaddr.
                fail UnixError.EINVAL
            | Some family ->

            match flavour with
            | SimulatedUnixFlavour.Linux ->
                // inet_stream_connect's order: the AF_UNSPEC branch, then
                // the state machine, then tcp_v4_connect's length and family
                // checks. Measured where a guest reaches it; the state arms'
                // precedence over the argument checks is the pinned source's.
                if family = 0 then
                    match sock.Phase with
                    | SocketPhase.Idle ->
                        // Measured: an accepted no-op, and the socket stays
                        // usable.
                        ConnectOutcome.Completed, kernel
                    | phase ->
                        failwith
                            $"SystemNative_Connect: AF_UNSPEC on a stream socket in %A{phase} under Linux runs tcp_disconnect, whose consequences for this phase (a connected socket's peer, a listener's queue) are unmeasured and unmodelled."
                else

                match sock.Phase with
                | SocketPhase.EstablishedPendingReport connectionId ->
                    // The one completion-reporting SUCCESS (measured). The
                    // destination is ignored, as the state transition is.
                    ConnectOutcome.Completed, withPhase (SocketPhase.Established connectionId) kernel
                | SocketPhase.RefusedPendingDelivery ->
                    // Deliver the latched refusal once, then reset: the next
                    // connect is a fresh attempt, and the source address the
                    // pending attempt resolved reverts to whatever bind(2)
                    // locked (both measured).
                    let kernel =
                        { kernel with
                            Machine =
                                { kernel.Machine with
                                    Sockets =
                                        Map.add
                                            socketId
                                            { sock with
                                                Binding =
                                                    sock.Binding
                                                    |> Option.map (
                                                        bindingAfterRefusalDelivery SimulatedUnixFlavour.Linux
                                                    )
                                                Phase = SocketPhase.Idle
                                            }
                                            kernel.Sockets
                                }
                        }

                    // The reset signals: a registered client whose error edge
                    // was already consumed sees a fresh OUT|HUP edge after
                    // the delivering connect (measured, `order3.c` row M).
                    let kernel = signalSocketStateChange socketId kernel

                    ConnectOutcome.Failed UnixError.ECONNREFUSED, kernel
                | SocketPhase.Dead ->
                    failwith
                        "SystemNative_Connect: a stream socket is in SocketPhase.Dead under the Linux flavour, which only Darwin's refusal delivery produces. This is an interpreter bug."
                | SocketPhase.Established _ -> fail UnixError.EISCONN
                | SocketPhase.Listening _ ->
                    // Measured: Linux answers a connect on the listening
                    // socket itself with EISCONN, where Darwin answers
                    // EOPNOTSUPP.
                    fail UnixError.EISCONN
                | SocketPhase.DatagramPeer _ ->
                    failwith
                        "SystemNative_Connect: a stream socket holds SocketPhase.DatagramPeer. EmulatedKernelDefect.SocketPhaseKindMismatch exists to make this unreachable, so this is an interpreter bug."
                | SocketPhase.Idle ->

                match lengthVerdict with
                | BindLengthVerdict.Invalid -> fail UnixError.EINVAL
                | BindLengthVerdict.RejectedBeforeCopy _
                | BindLengthVerdict.Accepted ->

                if family <> SimulatedUnixPlatform.internetAddressFamily then
                    fail UnixError.EAFNOSUPPORT
                else

                match destination with
                | Some dest -> attemptStream dest
                | None ->
                    failwith
                        "SystemNative_Connect: the declared length passed the AF_INET verdict but the destination was not supplied; the caller reads it whenever the length reaches it. This is an interpreter bug."
            | SimulatedUnixFlavour.Darwin ->
                // The state arms answer first — measured three ways: the
                // dead latch beats a good destination, EISCONN beats
                // AF_UNSPEC, and the refusal delivery beats a changed
                // destination.
                match sock.Phase with
                | SocketPhase.EstablishedPendingReport _ ->
                    failwith
                        "SystemNative_Connect: a stream socket is in SocketPhase.EstablishedPendingReport under the Darwin flavour, which never constructs it (its retry answers EISCONN directly). This is an interpreter bug."
                | SocketPhase.RefusedPendingDelivery ->
                    // Deliver once; the socket is then dead (measured).
                    ConnectOutcome.Failed UnixError.ECONNREFUSED, withPhase SocketPhase.Dead kernel
                | SocketPhase.Dead ->
                    // Measured, whatever the destination.
                    fail UnixError.EINVAL
                | SocketPhase.Established _ ->
                    // Measured, including against an AF_UNSPEC destination.
                    fail UnixError.EISCONN
                | SocketPhase.Listening _ ->
                    if family = 0 then
                        failwith
                            "SystemNative_Connect: AF_UNSPEC on a listening stream socket under Darwin is unmeasured (the measured EOPNOTSUPP row used an AF_INET destination), so measure it rather than extrapolating."
                    else
                        // Measured: EOPNOTSUPP, where Linux answers EISCONN.
                        fail UnixError.EOPNOTSUPP
                | SocketPhase.DatagramPeer _ ->
                    failwith
                        "SystemNative_Connect: a stream socket holds SocketPhase.DatagramPeer. EmulatedKernelDefect.SocketPhaseKindMismatch exists to make this unreachable, so this is an interpreter bug."
                | SocketPhase.Idle ->

                if family = 0 then
                    // Measured at the exact sockaddr_in length:
                    // EADDRNOTAVAIL, and the socket stays usable. Other
                    // lengths are unmeasured.
                    if declaredLength <> exactSize then
                        failwith
                            $"SystemNative_Connect: AF_UNSPEC with a declared length of %d{declaredLength} on an idle Darwin stream socket is unmeasured (only %d{exactSize} is), so measure it rather than guessing."
                    else
                        fail UnixError.EADDRNOTAVAIL
                else

                match lengthVerdict with
                | BindLengthVerdict.Invalid -> fail UnixError.EINVAL
                | BindLengthVerdict.RejectedBeforeCopy _
                | BindLengthVerdict.Accepted ->

                if family <> SimulatedUnixPlatform.internetAddressFamily then
                    fail UnixError.EAFNOSUPPORT
                else

                match destination with
                | Some dest -> attemptStream dest
                | None ->
                    failwith
                        "SystemNative_Connect: the declared length passed the AF_INET verdict but the destination was not supplied; the caller reads it whenever the length reaches it. This is an interpreter bug."
        | SocketKind.Datagram ->
            match lengthVerdict with
            | BindLengthVerdict.RejectedBeforeCopy error -> fail error
            | BindLengthVerdict.Accepted
            | BindLengthVerdict.Invalid ->

            match family with
            | None -> fail UnixError.EINVAL
            | Some family ->

            match sock.Phase with
            | SocketPhase.Idle
            | SocketPhase.DatagramPeer _ -> ()
            | phase ->
                failwith
                    $"SystemNative_Connect: a datagram socket holds %A{phase}. EmulatedKernelDefect.SocketPhaseKindMismatch exists to make this unreachable, so this is an interpreter bug."

            if family = 0 then
                match flavour with
                | SimulatedUnixFlavour.Linux ->
                    if declaredLength < exactSize then
                        failwith
                            $"SystemNative_Connect: AF_UNSPEC with a declared length of %d{declaredLength} on a Linux datagram socket is unmeasured (only %d{exactSize} and above are), so measure it rather than guessing."
                    else

                    // Measured with and without a peer set: dissolves the
                    // filter and answers SUCCESS. The dissolve also unbinds
                    // what connect resolved — unlike TCP's reset, the *port*
                    // is dropped too (probe8: getsockname reads 0.0.0.0:0
                    // afterwards for an implicitly bound socket and for one
                    // whose bind(2) gave the wildcard), so a socket with no
                    // locked concrete address ends up fully unbound and the
                    // next connect binds afresh. A locked concrete address
                    // was measured to survive with the port zeroed —
                    // 127.0.0.1:0 — but whether a bind(2)-chosen port would
                    // also drop, and how such a half-bound socket rebinds,
                    // is unmeasured, so that provenance is refused.
                    match sock.Phase with
                    | SocketPhase.DatagramPeer _ ->
                        let binding =
                            match sock.Binding with
                            | None ->
                                failwith
                                    "SystemNative_Connect: a datagram socket holds a peer but no binding; connect binds before it records the peer, so this is an interpreter bug."
                            | Some binding ->
                                match binding.LockedAddress with
                                | None -> None
                                | Some locked when locked = InternetEndpoint.WildcardAddress -> None
                                | Some _ ->
                                    failwith
                                        $"SystemNative_Connect: AF_UNSPEC on a datagram socket whose bind(2) locked %s{InternetEndpoint.toString binding.Endpoint}'s address is only measured for a kernel-chosen port (the address survives, the port zeroes); what survives a bind(2)-chosen port, and how the half-bound socket rebinds, is unmeasured. Measure it rather than guessing."

                        ConnectOutcome.Completed,
                        { kernel with
                            Machine =
                                { kernel.Machine with
                                    Sockets =
                                        Map.add
                                            socketId
                                            { sock with
                                                Binding = binding
                                                Phase = SocketPhase.Idle
                                            }
                                            kernel.Sockets
                                }
                        }
                    | _ ->

                    match sock.Binding with
                    | None ->
                        // No peer to dissolve and nothing bound: the
                        // accepted no-op (measured).
                        ConnectOutcome.Completed, kernel
                    | Some _ ->
                        failwith
                            "SystemNative_Connect: AF_UNSPEC on a bound but unconnected Linux datagram socket is unmeasured (whether the dissolve drops the binding as it does for a connected one), so measure it rather than guessing."
                | SimulatedUnixFlavour.Darwin ->
                    if declaredLength <> exactSize then
                        failwith
                            $"SystemNative_Connect: AF_UNSPEC with a declared length of %d{declaredLength} on a Darwin datagram socket is unmeasured (only %d{exactSize} is), so measure it rather than guessing."
                    else
                        // Measured with and without a peer set.
                        fail UnixError.EAFNOSUPPORT
            else

            match lengthVerdict with
            | BindLengthVerdict.Invalid -> fail UnixError.EINVAL
            | BindLengthVerdict.RejectedBeforeCopy _
            | BindLengthVerdict.Accepted ->

            if family <> SimulatedUnixPlatform.internetAddressFamily then
                fail UnixError.EAFNOSUPPORT
            else

            match destination with
            | None ->
                failwith
                    "SystemNative_Connect: the declared length passed the AF_INET verdict but the destination was not supplied; the caller reads it whenever the length reaches it. This is an interpreter bug."
            | Some dest ->

            if dest.Address = InternetEndpoint.WildcardAddress then
                failwith
                    "SystemNative_Connect: a datagram connect to 0.0.0.0 is unmeasured (the kernels remap it, but which address the peer filter then holds was not probed), so measure it rather than guessing."
            elif not (destinationIsLocal dest.Address) then
                failwith
                    $"SystemNative_Connect: destination %s{InternetEndpoint.toString dest} is not a local address of this simulated machine, and PawPrint models no network to carry a datagram anywhere else. Add the address to the kernel's LocalAddresses/LocalRoutes if it should be local, or connect to loopback."
            else

            // A datagram connect is a peer filter, not a handshake: it
            // succeeds with nothing at the destination and a re-connect
            // re-targets, both measured. It binds implicitly just as a
            // stream connect does.
            let binding, kernel = ensureBound dest kernel

            let kernel =
                { kernel with
                    Machine =
                        { kernel.Machine with
                            Sockets =
                                Map.add
                                    socketId
                                    { sock with
                                        Binding = Some binding
                                        Phase = SocketPhase.DatagramPeer dest
                                    }
                                    kernel.Sockets
                        }
                }

            ConnectOutcome.Completed, kernel

    /// Dequeue the oldest completed connection from `socketId`'s accept queue
    /// and materialise the server-side socket onto it: a fresh socket, bound
    /// at the connection's server address, on a fresh (blocking) descriptor.
    /// Answers the new fd and the connection, whose `ClientAddress` is what
    /// `accept(2)` reports as the peer.
    ///
    /// Partial: the caller has already answered EAGAIN (or refused to park)
    /// for an empty queue, and EINVAL/EOPNOTSUPP for a socket that is not a
    /// listening stream socket, so reaching this in any other state is an
    /// interpreter bug.
    let acceptConnection (socketId : SocketId) (kernel : EmulatedKernel) : int * TcpConnection * EmulatedKernel =
        let listener = socket socketId kernel

        match listener.Phase with
        | SocketPhase.Listening ({
                                     Queue = connectionId :: rest
                                 } as listenState) ->
            let tcpConnection = connection connectionId kernel
            let acceptedId = kernel.NextSocketId
            let (SocketId rawAcceptedId) = acceptedId

            let fd, registry =
                FileDescriptorRegistry.createSocket acceptedId kernel.FileDescriptors

            let accepted =
                {
                    Domain = listener.Domain
                    Kind = SocketKind.Stream
                    Protocol = listener.Protocol
                    Binding =
                        Some
                            {
                                Endpoint = tcpConnection.ServerAddress
                                // Nothing reads this on an accepted socket:
                                // its phase is Established for life, so no
                                // refusal delivery can ever revert it.
                                LockedAddress = None
                            }
                    // Both kernels copy the listener's socket options onto
                    // the accepted socket (inet_csk_clone_lock; sonewconn),
                    // and this flag's one modelled effect is bind-conflict
                    // admission. No current guest observes the inheritance.
                    ReuseAddress = listener.ReuseAddress
                    Phase = SocketPhase.Established connectionId
                }

            let kernel =
                { kernel with
                    FileDescriptors = registry
                    Machine =
                        { kernel.Machine with
                            Sockets =
                                kernel.Sockets
                                |> Map.add acceptedId accepted
                                |> Map.add
                                    socketId
                                    { listener with
                                        Phase =
                                            SocketPhase.Listening
                                                { listenState with
                                                    Queue = rest
                                                }
                                    }
                            NextSocketId = SocketId (rawAcceptedId + 1L)
                        }
                }

            fd, tcpConnection, kernel
        | SocketPhase.Listening {
                                    Queue = []
                                } ->
            failwith
                "EmulatedKernel.acceptConnection: the accept queue is empty; the caller answers EAGAIN (or refuses to park) before reaching this. This is an interpreter bug."
        | phase ->
            failwith
                $"EmulatedKernel.acceptConnection: socket %O{socketId} is in %A{phase}, not listening; the caller screens this. This is an interpreter bug."

    /// Every way this kernel's tables disagree with each other: the socket
    /// table against the descriptor table, the descriptor table against the
    /// filesystem, and the current directory against both.
    ///
    /// The descriptor table's own rules are `FileDescriptorRegistry.checkInvariants`,
    /// and the filesystem's are `VirtualFileSystem.checkInvariants`; this
    /// repeats neither. The latter takes a `pinned` argument that only this
    /// layer can supply, so a caller wanting the whole picture pairs this with
    /// `VirtualFileSystem.checkInvariants (EmulatedKernel.pinnedInodes kernel)`.
    let checkInvariants (kernel : EmulatedKernel) : EmulatedKernelDefect list =
        let named =
            FileDescriptorRegistry.descriptions kernel.FileDescriptors
            |> Map.toList
            |> List.choose (fun (id, description) ->
                match description.Target with
                | OpenFileTarget.StandardStream _
                | OpenFileTarget.SocketEventPort _
                | OpenFileTarget.File _ -> None
                | OpenFileTarget.Socket socketId -> Some (id, socketId)
            )

        let dangling =
            named
            |> List.filter (fun (_, socketId) -> not (Map.containsKey socketId kernel.Sockets))
            |> List.map EmulatedKernelDefect.DanglingSocket

        let namedIds = named |> List.map snd |> Set.ofList

        let unreferenced =
            kernel.Sockets
            |> Map.toList
            |> List.map fst
            |> List.filter (fun socketId -> not (Set.contains socketId namedIds))
            |> List.map EmulatedKernelDefect.UnreferencedSocket

        // Against the table rather than against the descriptions: the table is
        // where a socket lives, so it is the table that must stay below the
        // counter even once a socket can outlive every descriptor of it.
        let freshness =
            kernel.Sockets
            |> Map.toList
            |> List.map fst
            |> List.filter (fun socketId -> socketId >= kernel.NextSocketId)
            |> List.map (fun socketId -> EmulatedKernelDefect.NextSocketIdNotFresh (kernel.NextSocketId, socketId))

        let danglingInodes =
            kernel.FileDescriptors
            |> FileDescriptorRegistry.descriptions
            |> Map.toList
            |> List.choose (fun (id, description) ->
                match description.Target with
                | OpenFileTarget.File (inode, _) ->
                    if (VirtualFileSystem.tryGet inode kernel.FileSystem).IsNone then
                        Some (EmulatedKernelDefect.DanglingOpenInode (id, inode))
                    else
                        None
                | OpenFileTarget.StandardStream _
                | OpenFileTarget.SocketEventPort _
                | OpenFileTarget.Socket _ -> None
            )

        let danglingStreams =
            kernel.DirectoryStreams
            |> Map.toList
            |> List.choose (fun (id, stream) ->
                match VirtualFileSystem.tryGetContent stream.Inode kernel.FileSystem with
                | Some (InodeContent.Directory _) -> None
                | Some (InodeContent.RegularFile _)
                | Some (InodeContent.Symlink _) ->
                    Some (EmulatedKernelDefect.DirectoryStreamIsNotADirectory (id, stream.Inode))
                | None -> Some (EmulatedKernelDefect.DanglingDirectoryStreamInode (id, stream.Inode))
            )

        let directoryStreamFreshness =
            kernel.DirectoryStreams
            |> Map.toList
            |> List.map fst
            |> List.filter (fun id -> id >= kernel.NextDirectoryStreamId)
            |> List.map (fun id ->
                EmulatedKernelDefect.NextDirectoryStreamIdNotFresh (kernel.NextDirectoryStreamId, id)
            )

        // Both directions: a `DIR*` naming a stream that is gone would crash the
        // next `readdir`, and a stream no `DIR*` names can never be closed, so it
        // pins its directory through `heldInodes` for the rest of the run.
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

        let currentDirectory =
            match VirtualFileSystem.tryGetContent kernel.CurrentDirectoryInode kernel.FileSystem with
            | Some (InodeContent.Directory _) ->
                // Only when some path still reaches it: an inode a process
                // holds open after its last name has gone has no path, and
                // `getcwd` on a real system fails rather than lying.
                match VirtualFileSystem.pathOfDirectory kernel.CurrentDirectoryInode kernel.FileSystem with
                | Some physical when physical <> kernel.CurrentDirectory ->
                    [
                        EmulatedKernelDefect.CurrentDirectoryPathDisagrees (kernel.CurrentDirectory, physical)
                    ]
                | Some _
                | None -> []
            | Some (InodeContent.RegularFile _)
            | Some (InodeContent.Symlink _)
            | None ->
                [
                    EmulatedKernelDefect.CurrentDirectoryIsNotADirectory kernel.CurrentDirectoryInode
                ]

        // Every reference any socket makes to a connection, with whether it
        // came through an accept queue (which has its own defect case and its
        // own no-duplicates rule).
        let connectionReferences =
            kernel.Sockets
            |> Map.toList
            |> List.collect (fun (socketId, socket) ->
                match socket.Phase with
                | SocketPhase.Established connection
                | SocketPhase.EstablishedPendingReport connection -> [ socketId, connection, false ]
                | SocketPhase.Listening listenState ->
                    listenState.Queue |> List.map (fun connection -> socketId, connection, true)
                | SocketPhase.Idle
                | SocketPhase.RefusedPendingDelivery
                | SocketPhase.Dead
                | SocketPhase.DatagramPeer _ -> []
            )

        let danglingConnections =
            connectionReferences
            |> List.filter (fun (_, connection, _) -> not (Map.containsKey connection kernel.Connections))
            |> List.map (fun (socketId, connection, queued) ->
                if queued then
                    EmulatedKernelDefect.DanglingQueuedConnection (socketId, connection)
                else
                    EmulatedKernelDefect.DanglingConnection (socketId, connection)
            )

        let referencedConnections =
            connectionReferences
            |> List.map (fun (_, connection, _) -> connection)
            |> Set.ofList

        let orphanConnections =
            kernel.Connections
            |> Map.toList
            |> List.map fst
            |> List.filter (fun connection -> not (Set.contains connection referencedConnections))
            |> List.map EmulatedKernelDefect.OrphanConnection

        let duplicateQueued =
            connectionReferences
            |> List.choose (fun (_, connection, queued) -> if queued then Some connection else None)
            |> List.countBy id
            |> List.filter (fun (_, count) -> count > 1)
            |> List.map (fun (connection, _) -> EmulatedKernelDefect.DuplicateQueuedConnection connection)

        let phaseKindMismatches =
            kernel.Sockets
            |> Map.toList
            |> List.choose (fun (socketId, socket) ->
                let mismatched =
                    match socket.Kind, socket.Phase with
                    | SocketKind.Datagram, SocketPhase.Idle
                    | SocketKind.Datagram, SocketPhase.DatagramPeer _ -> false
                    | SocketKind.Datagram, _ -> true
                    | _, SocketPhase.DatagramPeer _ -> true
                    | _, _ -> false

                if mismatched then
                    Some (EmulatedKernelDefect.SocketPhaseKindMismatch (socketId, socket.Kind, socket.Phase))
                else
                    None
            )

        let connectionFreshness =
            kernel.Connections
            |> Map.toList
            |> List.map fst
            |> List.filter (fun connection -> connection >= kernel.NextConnectionId)
            |> List.map (fun connection ->
                EmulatedKernelDefect.NextConnectionIdNotFresh (kernel.NextConnectionId, connection)
            )

        let registrationOrdinals =
            kernel.FileDescriptors
            |> FileDescriptorRegistry.descriptions
            |> Map.toList
            |> List.collect (fun (portId, description) ->
                match description.Target with
                | OpenFileTarget.StandardStream _
                | OpenFileTarget.File _
                | OpenFileTarget.Socket _ -> []
                | OpenFileTarget.SocketEventPort portState ->
                    portState.Registrations
                    |> Map.toList
                    |> List.map (fun (_, registration) -> portId, registration.RegisteredAt)
            )

        let ordinalFreshness =
            registrationOrdinals
            |> List.filter (fun (_, registeredAt) -> registeredAt >= kernel.NextSocketEventRegistrationOrdinal)
            |> List.map (fun (portId, registeredAt) ->
                EmulatedKernelDefect.SocketEventRegistrationOrdinalNotFresh (
                    kernel.NextSocketEventRegistrationOrdinal,
                    portId,
                    registeredAt
                )
            )

        let ordinalDuplicates =
            registrationOrdinals
            |> List.countBy snd
            |> List.filter (fun (_, count) -> count > 1)
            |> List.map (fun (registeredAt, _) ->
                EmulatedKernelDefect.DuplicateSocketEventRegistrationOrdinal registeredAt
            )

        dangling
        @ unreferenced
        @ freshness
        @ danglingInodes
        @ danglingStreams
        @ directoryStreamFreshness
        @ directoryStreamBlocks
        @ currentDirectory
        @ danglingConnections
        @ orphanConnections
        @ duplicateQueued
        @ phaseKindMismatches
        @ connectionFreshness
        @ ordinalFreshness
        @ ordinalDuplicates

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
        /// real .NET. See `EmulatedKernel.environmentEntryProblem`.
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
        /// `[0, EmulatedKernel.maxWallClockEpochMs]`. See
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
        /// inode's `st_uid`. See `EmulatedKernel.defaultUserId` for why the
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
        /// See `EmulatedKernel.defaultEphemeralPortRange`; the low end must not
        /// exceed the high end, and neither may be zero, since port 0 is the
        /// request rather than an answer.
        EphemeralPortRange : uint16 * uint16
        /// The `somaxconn` sysctl, or `None` for the flavour's measured
        /// default (4096 on Linux, 128 on Darwin): the ceiling `listen(2)`
        /// clamps its backlog to. See `EmulatedKernel.withSoMaxConn`.
        SoMaxConn : int option
        /// The IPv4 addresses this machine holds, as prefixes. See
        /// `EmulatedKernel.defaultLocalAddresses`, and note the flavours read one
        /// list differently.
        LocalAddresses : uint32 list
        /// Prefixes this machine has a local route to. Linux binds any address
        /// inside one; Darwin ignores them. See
        /// `EmulatedKernel.defaultLocalRoutes`.
        LocalRoutes : Ipv4Prefix list
    }

    /// Configuration a host gets if it expresses no preference: no environment
    /// overlay, the default single processor, a wall clock booting at the Unix
    /// epoch, the default Unix platform, and the root as the current directory.
    static member Default : KernelConfig =
        {
            Environment = Map.empty
            ProcessorCount = EmulatedKernel.defaultProcessorCount
            UserAddressLimit = EmulatedKernel.defaultUserAddressLimit
            InstructionCostTicks = EmulatedKernel.defaultInstructionCostTicks
            ClockJitter = ClockJitterStrategy.Disabled
            OptimalMaxSpinWaitsPerSpinIteration = EmulatedKernel.defaultOptimalMaxSpinWaitsPerSpinIteration
            WallClockEpochMs = 0L
            UnixPlatform = EmulatedKernel.defaultUnixPlatform
            CurrentDirectory = EmulatedKernel.defaultCurrentDirectory
            ProcessPath = EmulatedKernel.defaultProcessPath
            FileSystem = FileSystemSeed.empty
            UserId = EmulatedKernel.defaultUserId
            GroupId = EmulatedKernel.defaultGroupId
            Umask = EmulatedKernel.defaultUmask
            FileSystemType = None
            EphemeralPortRange = EmulatedKernel.defaultEphemeralPortRange
            SoMaxConn = None
            LocalAddresses = EmulatedKernel.defaultLocalAddresses
            LocalRoutes = EmulatedKernel.defaultLocalRoutes
        }

[<RequireQualifiedAccess>]
module KernelConfig =
    /// Apply a host configuration to a freshly-minted kernel. Each field is
    /// applied through its own `EmulatedKernel` setter, so the validation those
    /// setters perform (e.g. rejecting a non-positive processor count) also
    /// guards the configuration path.
    let applyTo (config : KernelConfig) (kernel : EmulatedKernel) : EmulatedKernel =
        kernel
        |> EmulatedKernel.withEnvironment config.Environment
        |> EmulatedKernel.withProcessorCount config.ProcessorCount
        |> EmulatedKernel.withUserAddressLimit config.UserAddressLimit
        |> EmulatedKernel.withInstructionCostTicks config.InstructionCostTicks
        |> EmulatedKernel.withClockJitter config.ClockJitter
        |> EmulatedKernel.withOptimalMaxSpinWaitsPerSpinIteration config.OptimalMaxSpinWaitsPerSpinIteration
        |> EmulatedKernel.withWallClockEpochMs config.WallClockEpochMs
        |> EmulatedKernel.withUnixPlatformAndFileSystemType config.UnixPlatform config.FileSystemType
        |> EmulatedKernel.withProcessPath config.ProcessPath
        |> EmulatedKernel.withFileSystemAndCurrentDirectory
            config.UnixPlatform
            (UnixTimestamp.ofMillisecondsSinceEpoch config.WallClockEpochMs)
            config.FileSystem
            config.CurrentDirectory
        |> EmulatedKernel.withUserAndGroupId config.UserId config.GroupId
        |> EmulatedKernel.withEphemeralPortRange config.EphemeralPortRange
        |> EmulatedKernel.withSoMaxConn config.UnixPlatform config.SoMaxConn
        |> EmulatedKernel.withLocalAddresses config.LocalAddresses config.LocalRoutes
        |> EmulatedKernel.withUmask config.Umask

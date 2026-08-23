namespace WoofWare.PawPrint

open System.Collections.Immutable

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

/// Which Unix a simulated platform *is*.
///
/// The axis along which the systems PawPrint models differ, and the
/// only one: everything else it needs to know about a platform — the errno
/// numbering, a symlink's permission bits, whether `stat` reports a creation
/// time — is a consequence of this rather than an independent choice. Those
/// are measured properties of real systems (the symlink mode was probed; the
/// birth time is a `#if` in `pal_io.c`), so they are derived here from the
/// flavour rather than supplied by a host, which could only invent them.
///
/// A third Unix therefore arrives as a case *here*, and every derivation below
/// stops compiling until whoever adds it has looked each fact up. That is the
/// same compile-time fork `RawErrnoPortability` makes, and for the same reason.
[<RequireQualifiedAccess>]
type SimulatedUnixFlavour =
    /// Linux, whose `<errno.h>` numbering, always-0o777 symlink modes and
    /// birth-time-less `stat` PawPrint follows.
    | Linux
    /// Darwin, i.e. macOS. `uname -r` reports the *Darwin* kernel
    /// release rather than the macOS product version.
    | Darwin

/// The filesystem an emulated mount claims to be, as `fstatfs(2)` reports it.
///
/// A *choice* rather than a measured fact, because PawPrint's filesystem is an
/// in-memory graph that is not any real filesystem. That is why it is
/// configuration (`KernelConfig.FileSystemType`) rather than a derivation from
/// the flavour the way the errno numbering is: a single Linux reports `0xEF53`,
/// `0x01021994` and `0x9FA0` for three directories in one process, so a flavour
/// does not determine a mount's type. It does *constrain* it, which is what
/// `EmulatedFileSystemType.isReportableUnder` carries.
///
/// This changes what `SystemNative_GetFileSystemType` answers and nothing else.
/// Path resolution keeps its flavour's limits either way — `pathLimits`
/// carries `NameLengthLimit` as an ext4-versus-APFS fact — so a kernel
/// configured `Nfs` reports NFS while still resolving names as its flavour
/// does.
///
/// Only three cases, because only three have a consumer. Note that a fourth
/// could not be `Ext4`: the managed layer cannot distinguish it, CoreLib's
/// `UnixFileSystemTypes` having no such member (it is `ext2 = 0xEF53`, with
/// `ext4` commented out as an alias).
[<RequireQualifiedAccess>]
type EmulatedFileSystemType =
    /// Linux's in-memory filesystem, and so the honest analogue of a
    /// filesystem that only ever exists in memory.
    | Tmpfs
    /// What a macOS file is on. Darwin's answer, since it mounts no tmpfs.
    | Apfs
    /// One of the four filesystems CoreCLR refuses to take a *shared* lock on
    /// (`SafeFileHandle.CanLockTheFile`), so a mount of this type is the one
    /// configuration under which a `FileShare.Read` handle opened for writing
    /// takes no `flock` at all.
    | Nfs

/// What `fstatfs(2)` does when asked about one descriptor.
///
/// Modelled as a success-or-failure rather than as the bare `uint32` the PAL
/// returns, because the PAL folds *every* failure to 0 and the errno the
/// kernel left behind is still observable to a guest that declares
/// `SetLastError`. Collapsing the two here would lose it.
[<RequireQualifiedAccess>]
type FileSystemTypeAnswer =
    /// `fstatfs` succeeded and named this filesystem.
    | Reported of magic : uint32
    /// `fstatfs` failed, leaving this errno. The PAL reports 0 to its caller.
    | Failed of error : UnixError

[<RequireQualifiedAccess>]
module EmulatedFileSystemType =
    /// The number `fstatfs(2)` reports for a file on a mount of this type.
    ///
    /// These are the values CoreLib's `Interop.Sys.UnixFileSystemTypes` gives
    /// them, which is what matters: that enum is how the only managed consumer
    /// reads the number back. Each was also measured on a live kernel — tmpfs
    /// on Linux's `/dev/shm`, APFS on a macOS `/tmp`.
    ///
    /// Linux returns its `statfs.f_type` verbatim while Darwin maps
    /// `f_fstypename` through a name table (`MapFileSystemNameToEnum`,
    /// `pal_io.c`), so the two arrive at the same number by different routes;
    /// `Nfs` is the one case both flavours can produce, and both produce
    /// `0x6969`.
    let magic (fsType : EmulatedFileSystemType) : uint32 =
        match fsType with
        | EmulatedFileSystemType.Tmpfs -> 0x01021994u
        | EmulatedFileSystemType.Apfs -> 0x1Au
        | EmulatedFileSystemType.Nfs -> 0x6969u

    /// The type a mount reports when a host expresses no preference.
    ///
    /// `Tmpfs` under Linux because PawPrint's filesystem really is in memory,
    /// and `Apfs` under Darwin because macOS mounts no tmpfs, so nothing there
    /// could report one.
    let defaultFor (flavour : SimulatedUnixFlavour) : EmulatedFileSystemType =
        match flavour with
        | SimulatedUnixFlavour.Linux -> EmulatedFileSystemType.Tmpfs
        | SimulatedUnixFlavour.Darwin -> EmulatedFileSystemType.Apfs

    /// Whether a kernel of this flavour could report this filesystem type at
    /// all.
    ///
    /// The flavour does not *determine* a mount's type, but it does rule
    /// several out, and a kernel that claimed one of those would be handing a
    /// guest a fact no real system of the platform it impersonates could
    /// produce. Written as an exhaustive pair match rather than as a
    /// predicate over one axis, so that a new flavour or a new filesystem
    /// stops compiling until someone has looked the combination up.
    let isReportableUnder (flavour : SimulatedUnixFlavour) (fsType : EmulatedFileSystemType) : bool =
        match fsType, flavour with
        // Measured: `/dev/shm` reports it. macOS mounts no tmpfs at all, so
        // its `f_fstypename` is never "tmpfs" — the name table has a row for
        // it, but nothing on Darwin ever hits that row.
        | EmulatedFileSystemType.Tmpfs, SimulatedUnixFlavour.Linux -> true
        | EmulatedFileSystemType.Tmpfs, SimulatedUnixFlavour.Darwin -> false
        // No mainline Linux filesystem reports `0x1A`; a FUSE-mounted APFS
        // reports fuse's own `0x65735546`.
        | EmulatedFileSystemType.Apfs, SimulatedUnixFlavour.Linux -> false
        | EmulatedFileSystemType.Apfs, SimulatedUnixFlavour.Darwin -> true
        // Both mount NFS, and both report `0x6969` for it.
        | EmulatedFileSystemType.Nfs, SimulatedUnixFlavour.Linux
        | EmulatedFileSystemType.Nfs, SimulatedUnixFlavour.Darwin -> true

    /// What `fstatfs(2)` answers about one descriptor: `None` for an fd the
    /// process does not hold.
    ///
    /// The whole table lives here rather than in the handler, so that the unit
    /// tests, the host-comparison oracle and the guest all exercise the same
    /// function — a mutation swapping two of the rows below has nowhere to
    /// hide.
    ///
    /// Every row measured on both flavours (macOS 26.6, Linux 6.x), for both
    /// ends of a pipe, an `AF_INET` and an `AF_UNIX` socket, an epoll port, a
    /// kqueue, a regular file, a directory and an unknown descriptor.
    ///
    /// Refuses a `flavour` and `mount` that do not describe one machine.
    let reportedFor
        (flavour : SimulatedUnixFlavour)
        (mount : EmulatedFileSystemType)
        (target : OpenFileObject option)
        : FileSystemTypeAnswer
        =
        // The two arguments are a *pair*: a file's answer comes from the mount
        // and every other descriptor's from the flavour, so a caller supplying
        // one of each would get a machine that is Linux for its pipes and macOS
        // for its files. `withUnixPlatformAndFileSystemType` writes both fields
        // together so the kernel cannot hold such a pair, but `EmulatedKernel`
        // is a public record and `{ kernel with UnixPlatform = ... }` bypasses
        // every setter on it. Checking here rather than trusting the caller is
        // what keeps this function's contract true wherever it is reached: the
        // handler, the unit tests and the host-comparison oracle all arrive
        // through it.
        if not (isReportableUnder flavour mount) then
            failwith
                $"EmulatedFileSystemType.reportedFor: asked what a %O{flavour} kernel reports for a %O{mount} mount, which %O{flavour} cannot have. The kernel's UnixPlatform and FileSystemType have come apart — set them together with EmulatedKernel.withUnixPlatformAndFileSystemType rather than by updating the record directly."

        /// Darwin's `fstatfs` refuses every object that is not on a
        /// filesystem, uniformly; Linux's succeeds and names the
        /// pseudo-filesystem the object lives on. So each of these rows is a
        /// measured number rather than an invention — unlike `fstat`, which
        /// refuses the same descriptors because it owes them seventeen fields
        /// and the platforms agree on none of them.
        let pseudoFileSystem (linux : uint32) : FileSystemTypeAnswer =
            match flavour with
            | SimulatedUnixFlavour.Linux -> FileSystemTypeAnswer.Reported linux
            | SimulatedUnixFlavour.Darwin -> FileSystemTypeAnswer.Failed UnixError.EINVAL

        match target with
        | None -> FileSystemTypeAnswer.Failed UnixError.EBADF
        // Regular files and directories alike: measured identical, and one
        // mount has one answer.
        | Some (OpenFileObject.File _) -> FileSystemTypeAnswer.Reported (magic mount)
        // PawPrint models the standard streams as pipes (see
        // `FileDescriptorRegistry.initial`), so this row is a consequence of
        // that existing decision rather than a new one: Linux's `pipefs`.
        | Some (OpenFileObject.StandardStream _) -> pseudoFileSystem 0x50495045u
        // Linux's `sockfs`.
        | Some (OpenFileObject.Socket _) -> pseudoFileSystem 0x534F434Bu
        // Linux's `anon_inodefs`, which is where an epoll port lives — and
        // exactly the granularity this answer needs, which is why
        // `OpenFileObject` folding every anonymous object into one case costs
        // nothing here.
        | Some OpenFileObject.AnonymousInode -> pseudoFileSystem 0x09041934u

/// Why a string is not usable as a `utsname.release`.
[<RequireQualifiedAccess>]
type SimulatedUnixReleaseError =
    /// Every Unix fills `utsname.release`, so the empty string names no system.
    | Empty
    /// Longer than any `utsname.release` can hold.
    | TooLong of length : int * limit : int
    /// The value is handed to the guest as a C string of single bytes, so a
    /// non-ASCII character has no faithful encoding and an embedded NUL would
    /// silently truncate what the guest sees.
    | NotPrintableAscii of index : int * character : char

/// Whether a kernel validates the whole of a user buffer before it performs a
/// read or write, and if so which ranges it accepts.
///
/// The two Unixes differ, and the difference is observable: a platform that
/// checks up front refuses an out-of-range buffer even when the call would have
/// transferred nothing, and even when the descriptor names something the call
/// would have refused for another reason.
[<RequireQualifiedAccess>]
type UserBufferCheck =
    /// `vfs_read` and `vfs_write` run `access_ok(buf, count)` before reaching
    /// the file operation. A range is accepted when `address + length`, in
    /// exact arithmetic, is at most this value — the machine's
    /// `EmulatedKernel.UserAddressLimit`.
    | BeforeOperation of highestRangeEnd : uint64
    /// No up-front check, so a bad address is discovered by the copy itself and
    /// a call that copies nothing never faults.
    | AtCopyTime

/// Limits on the user half of the address space that real machines have been
/// observed to impose, for a host picking an `EmulatedKernel.UserAddressLimit`.
///
/// Every one of these is `TASK_SIZE_MAX` for some real configuration; the value
/// is a property of the *machine* (its paging depth, its virtual-address width)
/// rather than of the kernel or the distribution, which is why the simulated
/// one is configuration rather than a constant derived from the platform.
[<RequireQualifiedAccess>]
module ObservedUserAddressLimit =
    /// x86-64 with four-level paging: 2^47 less one page. Measured on a GitHub
    /// `ubuntu-latest` runner.
    [<Literal>]
    let X64FourLevelPaging : uint64 = 0x0000_7FFF_FFFF_F000UL

    /// x86-64 with five-level paging (LA57): 2^56 less one page. Measured on a
    /// different `ubuntu-latest` runner in the same CI run as the above, which
    /// is what shows this varies by machine rather than by kernel.
    [<Literal>]
    let X64FiveLevelPaging : uint64 = 0x00FF_FFFF_FFFF_F000UL

    /// arm64 with a 48-bit virtual address: 2^48 exactly, the one observed
    /// value that is not a page short of a power of two. Measured on a Linux
    /// guest under Apple's `container`.
    [<Literal>]
    let Arm64FortyEightBit : uint64 = 0x0001_0000_0000_0000UL

/// The two constants of Linux's `epoll_wait` that follow from
/// `sizeof(struct epoll_event)`.
///
/// That size is an *architecture* fact, not a flavour one, so these are not
/// derived from `SimulatedUnixPlatform`: `linux/eventpoll.h` defines
/// `EPOLL_PACKED` as `__attribute__((packed))` under `#ifdef __x86_64__` and
/// empty otherwise, over `{ __poll_t events; __u64 data; }`. The values here are
/// x86-64's, which is right for `SimulatedUnixPlatform.linuxX64` — the only
/// Linux platform PawPrint can currently be asked to simulate. A linux-arm64
/// preset would want 16 and 134_217_727, and this is the one place to teach.
///
/// Kept out of `SimulatedUnixPlatform` itself because every fact derived from
/// that type is a total function of the flavour, and epoll has no Darwin answer:
/// `SystemNative_WaitForSocketEvents`' kqueue arm reads neither of these.
///
/// Note that `SocketEventBufferElementSize` — the stride of the buffer CoreLib
/// allocates — is *not* affected, and so is absent here: it is
/// `max(sizeof(struct epoll_event), sizeof(SocketEvent))`, and that `max` is 16
/// under either packing.
[<RequireQualifiedAccess>]
module LinuxEpollLimits =
    /// `sizeof(struct epoll_event)`. The unit of the byte range `epoll_wait`
    /// screens with `access_ok(events, maxevents * sizeof(struct epoll_event))`.
    [<Literal>]
    let EventSize : int = 12

    /// `EP_MAX_EVENTS`, which is `INT_MAX / sizeof(struct epoll_event)`
    /// (fs/eventpoll.c). `epoll_wait` rejects a `maxevents` above this with
    /// EINVAL, and the bound is what keeps `maxevents * EventSize` inside
    /// `int32` for every count that gets past it — so a handler must consult it
    /// before computing that product, not after.
    ///
    /// `TestLinuxEpollLimits` checks the arithmetic rather than trusting the
    /// literal.
    [<Literal>]
    let MaxEvents : int = 178_956_970

[<RequireQualifiedAccess>]
module UserBufferCheck =
    /// Whether this platform refuses a buffer of `length` bytes at `address`
    /// before performing the operation at all.
    let faultsBeforeOperation (check : UserBufferCheck) (address : uint64) (length : uint64) : bool =
        match check with
        | UserBufferCheck.AtCopyTime -> false
        | UserBufferCheck.BeforeOperation highestRangeEnd ->
            // Rearranged to subtract rather than add, so that a range end past
            // `UInt64.MaxValue` is a refusal instead of wrapping onto a low
            // address the check would accept. The first disjunct is what keeps
            // the subtraction in the second from underflowing.
            length > highestRangeEnd || address > highestRangeEnd - length

/// Identity of the Unix-shaped platform the simulated process believes it is
/// running on. Consulted by the `SystemNative_*` entry points that report
/// host identity — today only `SystemNative_GetUnixRelease`, which surfaces
/// as `Environment.OSVersion` on a Unix CoreLib.
///
/// This is a value in kernel state rather than a host read, for the same
/// reason `ProcessorCount` is: real CoreCLR answers it from `uname(2)`, which
/// would make a replay depend on the machine that produced it — and worse,
/// guests branch on `Environment.OSVersion` (feature detection, quirk
/// workarounds), so letting the host leak in here would change guest
/// *control flow* between runs.
///
/// Modelled as a flavour plus a release string, rather than as a bag of loose
/// `utsname` fields, so that the facts we report stay mutually consistent as
/// more of `utsname` gets implemented: a future `SystemNative_GetUnixVersion`
/// or `SystemNative_GetOSArchitecture` is a new total *function* of the
/// flavour, not a new independently-settable string that could claim a Darwin
/// release alongside an x86_64 machine.
///
/// One representation per platform, which is what the flavour buys: every
/// platform-dependent fact below is a total function of it, with no failure
/// arms for an unclassifiable platform.
///
/// Construct with `SimulatedUnixPlatform.linuxX64`, `macOsArm64`, or `create`
/// for a specific release string.
[<CustomEquality ; NoComparison>]
type SimulatedUnixPlatform =
    private
        {
            Flavour : SimulatedUnixFlavour
            Release : string
        }

    override this.ToString () : string = $"%O{this.Flavour} %s{this.Release}"

    override this.Equals (other : obj) : bool =
        match other with
        | :? SimulatedUnixPlatform as other -> this.Flavour = other.Flavour && this.Release = other.Release
        | _ -> false

    override this.GetHashCode () : int =
        System.HashCode.Combine (this.Flavour, this.Release)

/// The four `sizeof`s `SystemNative_GetSocketAddressSizes` reports in one call,
/// which `System.Net.Primitives`' `SocketAddressPal` class initialiser latches
/// and every `SocketAddress` is then sized by.
///
/// Compile-time properties of the native shim rather than of any socket, like
/// `reportsBirthTime`. Measured with a `sizeof` probe compiled on macOS arm64 and
/// on Linux, rather than recalled; all four are invariant of pointer width, since
/// every member of these structs is fixed-width and the two variable-length tails
/// (`sun_path`, `sockaddr_storage`'s padding) are sized from a constant.
type SocketAddressSizes =
    {
        /// `sizeof(struct sockaddr_in)`. 16 on both.
        InterNetwork : int
        /// `sizeof(struct sockaddr_in6)`. 28 on both.
        InterNetworkV6 : int
        /// `sizeof(struct sockaddr_un)`. The one of the four that differs: 110 on
        /// Linux, whose `sun_path` is 108 bytes, against 106 on Darwin, whose is
        /// 104.
        UnixDomain : int
        /// `sizeof(struct sockaddr_storage)`. 128 on both, and the same number
        /// `SystemNative_GetMaximumAddressSize` reports through its own entry
        /// point — hence `SimulatedUnixPlatform.maximumSocketAddressSize` rather
        /// than a second literal.
        Storage : int
    }

/// Where a `struct sockaddr`'s address family sits and how wide it is — the only
/// part of the socket-address layout the two Unixes lay out differently.
///
/// BSD gave `struct sockaddr` a leading one-byte `sa_len` and narrowed
/// `sa_family_t` to one byte to pay for it; Linux kept the original two-byte
/// `sa_family_t` and has no length byte. That is why every *later* field agrees
/// between the two — `sin_port` at 2, `sin_addr` at 4, `sin6_addr` at 8,
/// `sin6_scope_id` at 24, all measured on both — since the two layouts spend the
/// same two leading bytes differently rather than in different amounts.
///
/// A pair of numbers rather than an `int * int` so that no caller can pair an
/// offset with the wrong width: the two vary together and never independently.
[<RequireQualifiedAccess>]
type SockaddrFamilyField =
    /// Linux: `sa_family_t` is a two-byte `unsigned short` at offset 0, in the
    /// machine's own byte order, and there is no length byte before it.
    | TwoBytesAtOffsetZero
    /// Darwin and the BSDs: `sa_len` occupies byte 0 and the one-byte
    /// `sa_family_t` follows it at offset 1.
    ///
    /// Nothing in the shim writes `sa_len` — grep `pal_networking.c` and there is
    /// no mention of it. The byte a guest sees there is written by managed code:
    /// `SocketAddress..ctor` stores `(byte) _size` at index 0 before calling
    /// `SetAddressFamily`, unconditionally on every platform, so BSD gets its
    /// length byte and Linux has the same store overwritten by the wider family.
    | OneByteAtOffsetOne

[<RequireQualifiedAccess>]
module SockaddrFamilyField =
    /// Byte offset of the family field within any `struct sockaddr`.
    let offset (field : SockaddrFamilyField) : int =
        match field with
        | SockaddrFamilyField.TwoBytesAtOffsetZero -> 0
        | SockaddrFamilyField.OneByteAtOffsetOne -> 1

    /// Width of the family field in bytes. Also what the shim's
    /// `sizeof_member(sockaddr, sa_family)` bounds check uses, and what a
    /// conversion failure truncates the unconvertible value to.
    let width (field : SockaddrFamilyField) : int =
        match field with
        | SockaddrFamilyField.TwoBytesAtOffsetZero -> 2
        | SockaddrFamilyField.OneByteAtOffsetOne -> 1

/// Why `socketCreation` would not hand back a socket.
[<RequireQualifiedAccess>]
type SocketCreationRefusal =
    /// The shim's address-family conversion has no case for this value, so
    /// it returns `Error_EAFNOSUPPORT` without reaching `socket(2)`.
    | AddressFamily
    /// The shim's socket-type conversion has no case for this value:
    /// `Error_EPROTOTYPE`. Note that is the *shim's* choice of errno; a
    /// kernel asked the same question would say `ESOCKTNOSUPPORT`.
    | SocketType
    /// The shim's protocol conversion has no case for this value *in this
    /// address family*: `Error_EPROTONOSUPPORT`. Per-family, so the same
    /// protocol number can convert under one family and be refused under
    /// another.
    | Protocol
    /// Every one of the shim's screens passed, so a real run would reach
    /// `socket(2)` — and PawPrint has not decided what this socket is. Not
    /// an errno: there is nothing truthful to report.
    | Unmodelled

/// Everything a kernel does differently when `open(2)` is asked to *create*.
///
/// One record rather than a scatter of booleans, because the divergence is
/// several facts that always travel together: a platform that answers one of
/// them Linux's way answers all of them Linux's way, and a third Unix must
/// supply every field before it compiles. All four were measured on macOS
/// 26.6/APFS and Linux 6.x, at an unprivileged uid.
type CreatingOpenRules =
    {
        /// What the walk owes a final component carrying a trailing separator.
        /// Linux refuses such a path outright; Darwin resolves it as any lookup
        /// would, so `open("d/", O_CREAT)` opens the directory there and is
        /// EISDIR on Linux.
        TrailingSeparator : TrailingSeparatorPolicy
        /// Whether a creating open that lands on an existing *directory* is
        /// refused. Linux answers EISDIR — so `open(dir, O_RDONLY|O_CREAT)`
        /// fails where a plain `open(dir, O_RDONLY)` succeeds — while Darwin
        /// treats `O_CREAT` as having no bearing on an object that exists.
        ///
        /// `O_EXCL`'s EEXIST is measured to beat this on both, so a caller must
        /// check that first.
        RefusesExistingDirectory : bool
        /// What a path that consumed *no component at all* — "/" itself, or a
        /// symlink whose target is "/" — owes a creating open.
        ///
        /// Darwin answers EEXIST even without `O_EXCL`; Linux folds the case
        /// into `RefusesExistingDirectory` and so wants `None` here. Pinned as a
        /// property of the *navigation* rather than of the root inode: on macOS
        /// "/" is EEXIST while "/.", "/../" and "/private/.." reach the same
        /// inode and open fine, and "/System/Volumes/Data" — a writable volume's
        /// mount root — opens fine too, which rules out a read-only-mount
        /// artefact.
        RootNavigation : UnixError option
        /// The bits `open(2)` keeps from its `mode` argument before the umask is
        /// applied. XNU masks with `ACCESSPERMS`, so a Darwin guest cannot
        /// create a setuid, setgid or sticky file at all — measured, 0o4644,
        /// 0o2644 and 0o1644 all land as 0o644. Linux keeps all twelve bits.
        ModeMask : PermissionBits
    }

/// What `open(2)` should do next, once the path has been resolved and the
/// creating flags have been read.
///
/// A verdict rather than an action, so the rule can be decided — and compared
/// against a real kernel — without a machine to act on it. The handler is then
/// only the part that cannot be pure: allocating the inode, registering a
/// descriptor and pushing the result.
[<RequireQualifiedAccess>]
type CreatingOpenVerdict =
    /// Answer the guest with this errno.
    | Refuse of error : UnixError
    /// Bind a new empty regular file under `name` in `directory`.
    | Create of directory : InodeNumber * name : FileName
    /// The object is already there; open it, subject to the checks any
    /// non-creating open would apply.
    | OpenExisting of inode : InodeNumber

[<RequireQualifiedAccess>]
module CreatingOpenRules =
    /// Decide what an `open(2)` owes, given how its path resolved and whether it
    /// carried `O_CREAT` and `O_EXCL`.
    ///
    /// The order of the refusals is measured, and each beats the ones below it:
    ///
    ///  * `O_EXCL` on anything that exists is EEXIST — including a directory,
    ///    where it beats the EISDIR below: `open(".", O_CREAT|O_EXCL)` is EEXIST
    ///    while `open(".", O_CREAT)` is EISDIR on Linux.
    ///  * A *free* name that demands to be a directory creates nothing and is
    ///    ENOENT. Only Darwin reaches this: Linux refuses such a path inside the
    ///    walk, via `CreatingOpenRules.TrailingSeparator`.
    ///  * A path that consumed no component at all — "/" — is whatever
    ///    `RootNavigation` says, which is Darwin's EEXIST.
    ///  * A creating open landing on an existing directory is EISDIR on Linux.
    ///  * Binding a name needs the *write* bit on the directory that will hold
    ///    it: measured at uid 1000, 0o333 and 0o300 succeed while 0o644 and
    ///    0o555 are EACCES. Root bypasses it.
    ///
    ///    Binding needs the directory's *search* bit too — 0o111 is EACCES on
    ///    both kernels — but that half is not checked here: no resolution can
    ///    reach this function without it, because the walk refuses an
    ///    unsearchable directory before it looks a component up at all. See
    ///    `VirtualFileSystem.resolveFull`, which is also where the rows that
    ///    pin it live.
    ///
    /// A freshly created inode is deliberately *not* screened against the mode
    /// it was just given — measured unanimously, `open(free, O_CREAT|O_RDWR, 0)`
    /// succeeds and stores mode 0, while re-opening that same file `O_RDONLY` is
    /// EACCES. That is why `Create` is a distinct verdict from `OpenExisting`
    /// rather than a step before it.
    let verdict
        (rules : CreatingOpenRules)
        (privilege : CallerPrivilege)
        (creating : bool)
        (exclusive : bool)
        (resolution : Resolution)
        (vfs : VirtualFileSystem)
        : CreatingOpenVerdict
        =
        let existing = VirtualFileSystem.existingOf resolution.Target |> Result.toOption

        if not creating then
            match existing with
            | Some inode -> CreatingOpenVerdict.OpenExisting inode
            | None -> CreatingOpenVerdict.Refuse UnixError.ENOENT
        elif exclusive && existing.IsSome then
            CreatingOpenVerdict.Refuse UnixError.EEXIST
        else

        let isDirectory (inode : InodeNumber) : bool =
            match VirtualFileSystem.tryGetContent inode vfs with
            | Some (InodeContent.Directory _) -> true
            | Some (InodeContent.RegularFile _)
            | Some (InodeContent.Symlink _)
            | None -> false

        match resolution.Target with
        | ResolvedTarget.Entry (_, _, None) when resolution.TrailingSeparatorDemanded ->
            CreatingOpenVerdict.Refuse UnixError.ENOENT
        | ResolvedTarget.Directory (_, FinalNavigation.Root) when rules.RootNavigation.IsSome ->
            CreatingOpenVerdict.Refuse rules.RootNavigation.Value
        | ResolvedTarget.Directory (inode, _) ->
            if rules.RefusesExistingDirectory then
                CreatingOpenVerdict.Refuse UnixError.EISDIR
            else
                CreatingOpenVerdict.OpenExisting inode
        | ResolvedTarget.Entry (_, _, Some inode) ->
            if rules.RefusesExistingDirectory && isDirectory inode then
                CreatingOpenVerdict.Refuse UnixError.EISDIR
            else
                CreatingOpenVerdict.OpenExisting inode
        | ResolvedTarget.Entry (directory, name, None) ->

        // Nothing can be created inside a directory whose own last name has
        // gone: measured on both, `open("x", O_CREAT)` from inside an orphan is
        // ENOENT, at 0o755 and at 0o555 alike, so this beats the EACCES below.
        // `MkDirRules.verdict` states the same rule for the other creating
        // syscall.
        if VirtualFileSystem.isOrphanedDirectory directory vfs then
            CreatingOpenVerdict.Refuse UnixError.ENOENT
        else

        // Write alone: the search half of the rule is the walk's, and a
        // resolution that reached here has already passed it. Only the owner
        // triple can ever apply, since `stat` reports `Kernel.UserId` as every
        // inode's `st_uid`.
        let bindBits = 0o200

        let parentBits =
            match VirtualFileSystem.tryGet directory vfs with
            | Some parent ->
                match VirtualFileSystem.permissions parent with
                | InodePermissions.Stored bits -> PermissionBits.toInt bits
                | InodePermissions.PlatformSymlinkDefault ->
                    failwith
                        $"CreatingOpenRules.verdict: the walk resolved \"%s{FileName.toString name}\" inside inode %O{directory}, which reports platform-default symlink permissions -- but only a directory can hold an entry (this is an interpreter bug)."
            | None ->
                failwith
                    $"CreatingOpenRules.verdict: resolution named inode %O{directory} as the directory to create \"%s{FileName.toString name}\" in, but the filesystem does not contain it. Run VirtualFileSystem.checkInvariants."

        let lacksBindBits =
            match privilege with
            | CallerPrivilege.Privileged -> false
            | CallerPrivilege.Unprivileged -> parentBits &&& bindBits <> bindBits

        if lacksBindBits then
            CreatingOpenVerdict.Refuse UnixError.EACCES
        else
            CreatingOpenVerdict.Create (directory, name)

    /// The permission bits a file created with this `mode` argument ends up
    /// with, under `umask`. See `PermissionBits.fromCreationMode`, which states
    /// the rule once for every creating syscall; `ModeMask` is `open`'s half of
    /// it, and is how a Darwin guest cannot create a setuid file at all.
    let createdPermissions (rules : CreatingOpenRules) (umask : PermissionBits) (mode : int) : PermissionBits =
        PermissionBits.fromCreationMode rules.ModeMask umask mode

/// Everything a kernel does differently when `mkdir(2)` creates a directory.
///
/// Deliberately not folded into `CreatingOpenRules`, even though two fields
/// share a name with one of its: the values differ, so a shared record would
/// have to be right for both syscalls at once and is right for neither.
/// Measured at `umask 022` on macOS 25.6/APFS at uid 501 and Linux 6.x arm64 at
/// uid 1000, fresh tree per row.
type MkDirRules =
    {
        /// The walk `mkdir` resolves its path with. Linux's last component is a
        /// plain dentry lookup (`filename_create`), so a trailing separator buys
        /// nothing there and every existing final name is EEXIST; Darwin
        /// resolves it as a lookup would, which is how `mkdir("dang/")` creates
        /// the dangling link's *target* on that platform and answers ENOTDIR for
        /// "f/" and ELOOP for "cyc/".
        ///
        /// This field is why `MkDirRules.verdict` needs no rules: the divergence
        /// is spent inside the walk, and what comes out the other side is
        /// decided identically on both platforms.
        TrailingSeparator : TrailingSeparatorPolicy
        /// The bits `mkdir(2)` keeps from its `mode` argument before the umask
        /// is applied — which is *not* `CreatingOpenRules.ModeMask`. Linux keeps
        /// the sticky bit and drops both set-ID bits (`vfs_mkdir` masks with
        /// `S_IRWXUGO|S_ISVTX`), where its `open` keeps all twelve: measured,
        /// `mkdir(p, 0o7777)` gives 0o1755 and `mkdir(p, 0o2777)` gives 0o755.
        /// Darwin drops all three, as its `open` does.
        ModeMask : PermissionBits
        /// Whether a new directory inherits `S_ISGID` from the directory that
        /// holds it. Measured with a parent `chmod`ed to 0o2777 and read back at
        /// 0o2777 first: Linux gives the child 0o2755 from mode 0o777 and
        /// 0o3755 from 0o7777, so the bit is OR-ed in *after* both masks;
        /// Darwin gives 0o755 from every mode and does not inherit it at all.
        ///
        /// A kernel fact rather than a mount one on Linux — `inode_init_owner`
        /// (fs/inode.c) is VFS-generic, and a directory inherits the bit
        /// unconditionally when its parent carries it. The group-membership
        /// proviso beside it in that function applies only to non-directories,
        /// which is source-derived rather than measured, and is unobservable
        /// here anyway: PawPrint has one process-wide gid, so a new inode's
        /// group always matches its parent's. (`mount -o grpid` varies *gid*
        /// inheritance, not the bit, and one gid cannot see that either.)
        InheritsSetGroupIdFromParent : bool
    }

/// What `mkdir(2)` should do next, once its path has been resolved.
///
/// The same shape as `CreatingOpenVerdict`, less `OpenExisting`: `mkdir` has no
/// success that is not a creation.
[<RequireQualifiedAccess>]
type MkDirVerdict =
    /// Answer the guest with this errno.
    | Refuse of error : UnixError
    /// Bind a new empty directory under `name` in `directory`, whose own
    /// permission bits are `parentPermissions` — carried out of the verdict
    /// because it read them to decide, and because `S_ISGID` inheritance needs
    /// them again.
    | Create of directory : InodeNumber * name : FileName * parentPermissions : PermissionBits

[<RequireQualifiedAccess>]
module MkDirRules =
    /// Decide what a `mkdir(2)` owes, given how its path resolved.
    ///
    /// Takes no `MkDirRules`, and that is the point: every rule below is
    /// measured *identical* on both platforms. Everything `mkdir` diverges about
    /// is spent earlier, in the walk `MkDirRules.TrailingSeparator` selects, or
    /// later, in `createdPermissions`. Contrast `CreatingOpenRules.verdict`,
    /// which genuinely reads two of its fields.
    ///
    /// The order of the refusals is measured, and each beats the ones below it:
    ///
    ///  * A path that consumed no component at all — "/", ".", ".." — is
    ///    EEXIST, whichever `FinalNavigation` it was. `mkdir` does not
    ///    distinguish them, where `rmdir` does: EBUSY, EINVAL and ENOTEMPTY on
    ///    Linux, and on Darwin EISDIR for the first with EBUSY swallowing the
    ///    other two at the root.
    ///  * An existing final name is EEXIST: a file, a directory, or a symlink,
    ///    dangling or cyclic or not.
    ///  * EEXIST beats the *write* bit. Measured on both: an existing child of a
    ///    0o555 directory is EEXIST, where a free name there is EACCES.
    ///  * Binding a new name needs write on the directory that will hold it:
    ///    measured, 0o333 and 0o300 succeed while 0o555 and 0o644 are EACCES.
    ///    Root bypasses it.
    ///
    /// The holding directory's *search* bit is needed as well — and needed
    /// earlier, since without it the final name cannot be looked up at all, so
    /// its absence beats even EEXIST. That check is the walk's
    /// (`VirtualFileSystem.resolveFull`), which refuses before this function is
    /// reached; the rows that pin it live there.
    ///
    /// A *free* final name carrying a trailing separator creates, on both
    /// platforms — `mkdir("nx/")` succeeds. This is the one place `mkdir` and a
    /// creating `open` disagree about a resolution of the same shape: `open`
    /// owes it ENOENT on Darwin.
    ///
    let verdict (privilege : CallerPrivilege) (resolution : Resolution) (vfs : VirtualFileSystem) : MkDirVerdict =
        match resolution.Target with
        | ResolvedTarget.Directory _ -> MkDirVerdict.Refuse UnixError.EEXIST
        | ResolvedTarget.Entry (directory, name, existing) ->

        // Nothing can be created inside a directory whose own last name has
        // gone. Measured on both, at 0o755 and at 0o555, so ENOENT beats the
        // EACCES below; and `mkdir(".")` inside an orphan is still EEXIST, which
        // is why this sits under the `Directory` arm rather than above it.
        //
        // Above the `existing` match because that is where a real kernel puts
        // it: the ENOENT comes from the lookup itself failing against a dead
        // parent. The ordering is not observable — an orphan is necessarily
        // empty, since `rmdir` refuses a populated directory and this rule stops
        // one ever gaining an entry.
        if VirtualFileSystem.isOrphanedDirectory directory vfs then
            MkDirVerdict.Refuse UnixError.ENOENT
        else

        // Write alone: the search half of the rule is the walk's, and a
        // resolution that reached here has already passed it. Only the owner
        // triple can ever apply, since `stat` reports `Kernel.UserId` as every
        // inode's `st_uid`.
        let write = 0o200

        let parentPermissions =
            match VirtualFileSystem.tryGet directory vfs with
            | Some parent ->
                match VirtualFileSystem.permissions parent with
                | InodePermissions.Stored bits -> bits
                | InodePermissions.PlatformSymlinkDefault ->
                    failwith
                        $"MkDirRules.verdict: the walk resolved \"%s{FileName.toString name}\" inside inode %O{directory}, which reports platform-default symlink permissions -- but only a directory can hold an entry (this is an interpreter bug)."
            | None ->
                failwith
                    $"MkDirRules.verdict: resolution named inode %O{directory} as the directory to create \"%s{FileName.toString name}\" in, but the filesystem does not contain it. Run VirtualFileSystem.checkInvariants."

        let lacks (bit : int) : bool =
            match privilege with
            | CallerPrivilege.Privileged -> false
            | CallerPrivilege.Unprivileged -> PermissionBits.toInt parentPermissions &&& bit <> bit

        match existing with
        | Some _ -> MkDirVerdict.Refuse UnixError.EEXIST
        | None ->

        if lacks write then
            MkDirVerdict.Refuse UnixError.EACCES
        else
            MkDirVerdict.Create (directory, name, parentPermissions)

    /// The permission bits a directory created with this `mode` argument ends up
    /// with, inside a parent whose own bits are `parentPermissions`.
    ///
    /// `PermissionBits.fromCreationMode` under `MkDirRules.ModeMask`, then
    /// `S_ISGID` OR-ed in where the platform inherits it. The OR is last, and
    /// measured to be: Linux's `mkdir(sg, 0o7777)` in a 0o2777 parent gives
    /// 0o3755, so the bit survives a mask that would otherwise have cleared it.
    let createdPermissions
        (rules : MkDirRules)
        (parentPermissions : PermissionBits)
        (umask : PermissionBits)
        (mode : int)
        : PermissionBits
        =
        let setGroupId = 0o2000
        let masked = PermissionBits.fromCreationMode rules.ModeMask umask mode

        let inherited =
            rules.InheritsSetGroupIdFromParent
            && PermissionBits.toInt parentPermissions &&& setGroupId <> 0

        if inherited then
            PermissionBits.toInt masked ||| setGroupId
            |> PermissionBits.parseOrFail "MkDirRules.createdPermissions"
        else
            masked

/// Everything a kernel does differently when `unlink(2)` removes a name.
///
/// One field, and that is the whole record: unlike `mkdir`, whose divergence is
/// spent entirely inside the walk, `unlink` diverges in the *order and
/// vocabulary* of its refusals as well, and those live in
/// `UnlinkRules.linuxVerdict` and `UnlinkRules.darwinVerdict` rather than in
/// fields here. See `UnlinkRules.verdict` for why there are two functions
/// rather than a table.
///
/// Measured on macOS 26.6/APFS at uid 501 and 0, and Linux 6.x arm64 at uid
/// 1000 and 0, one fresh tree per row.
type UnlinkRules =
    {
        /// The walk `unlink` resolves its path with, under
        /// `SymlinkPolicy.NoFollowFinal` on both platforms.
        ///
        /// Linux's `do_unlinkat` takes a parent and a name and never resolves
        /// the final component at all, so a trailing separator neither
        /// dereferences a final symlink nor is enforced by the walk: it is
        /// reported on `Resolution.TrailingSeparatorDemanded` and enforced by
        /// `linuxVerdict`. Darwin's `namei` resolves it like any other lookup,
        /// which is `Demand`.
        ///
        /// The row that separates them is `unlink("lroot/")` with `lroot -> "/"`:
        /// ENOTDIR on Linux, which cannot have traversed the link, against
        /// EISDIR on Darwin, which did.
        TrailingSeparator : TrailingSeparatorPolicy
    }

/// What `unlink(2)` should do next, once its path has been resolved.
[<RequireQualifiedAccess>]
type UnlinkVerdict =
    /// Answer the guest with this errno.
    | Refuse of error : UnixError
    /// Remove `name` from `directory`, and — if that was the last name the
    /// inode had and no open file description holds it — free the inode.
    ///
    /// Carries no inode, though the verdict read one to decide. The removing
    /// code gets it from `VirtualFileSystem.unbind`, which answers the inode it
    /// actually unbound — so there is one source for "which inode lost a name",
    /// and it is the one the removal performed rather than the one a lookup saw
    /// beforehand.
    | Remove of directory : InodeNumber * name : FileName

/// The two questions `unlink(2)` and `rmdir(2)` both ask about a name they have
/// been asked to remove. Neither is a policy: which of them is asked first, and
/// what a "yes" costs, is each syscall's own measured business.
[<RequireQualifiedAccess>]
module private RemovalChecks =
    /// Whether the *holding* directory refuses this caller the write bit it
    /// needs to remove a name from it.
    ///
    /// Write alone: the search half is the walk's, and a resolution that got
    /// this far has passed it. Only the owner triple can ever apply, since
    /// `stat` reports `Kernel.UserId` as every inode's `st_uid`, and the sticky
    /// bit can never refuse for the same reason — POSIX permits the removal when
    /// the caller owns the file *or* the directory, and one kernel-wide identity
    /// owns both.
    ///
    /// Partial in `directory`, which the walk has just reported as the directory
    /// holding `name`.
    let lacksWrite
        (privilege : CallerPrivilege)
        (directory : InodeNumber)
        (name : FileName)
        (vfs : VirtualFileSystem)
        : bool
        =
        match privilege with
        | CallerPrivilege.Privileged -> false
        | CallerPrivilege.Unprivileged ->

        let permissions =
            match VirtualFileSystem.tryGet directory vfs with
            | Some parent ->
                match VirtualFileSystem.permissions parent with
                | InodePermissions.Stored bits -> bits
                | InodePermissions.PlatformSymlinkDefault ->
                    failwith
                        $"RemovalChecks.lacksWrite: the walk resolved \"%s{FileName.toString name}\" inside inode %O{directory}, which reports platform-default symlink permissions -- but only a directory can hold an entry (this is an interpreter bug)."
            | None ->
                failwith
                    $"RemovalChecks.lacksWrite: resolution named inode %O{directory} as the directory holding \"%s{FileName.toString name}\", but the filesystem does not contain it. Run VirtualFileSystem.checkInvariants."

        PermissionBits.toInt permissions &&& 0o200 <> 0o200

    /// Whether the inode a name is bound to is a directory. Partial in the same
    /// way `lacksWrite` is: the walk has just reported this inode.
    let isDirectory (inode : InodeNumber) (vfs : VirtualFileSystem) : bool =
        match VirtualFileSystem.tryGetContent inode vfs with
        | Some (InodeContent.Directory _) -> true
        | Some (InodeContent.RegularFile _)
        | Some (InodeContent.Symlink _) -> false
        | None ->
            failwith
                $"RemovalChecks.isDirectory: the walk resolved a name to inode %O{inode}, which the filesystem does not contain. Run VirtualFileSystem.checkInvariants."

    /// Whether the directory at `inode` still holds an entry, which is what
    /// `rmdir(2)` answers ENOTEMPTY for. "." and ".." do not count: they are
    /// derived rather than stored (see `DirectoryContent.Entries`), and a real
    /// `rmdir` does not count them either.
    ///
    /// Partial in the same way the two above are, and additionally in the inode
    /// being a directory: the caller has just asked `isDirectory`.
    let isEmptyDirectory (inode : InodeNumber) (vfs : VirtualFileSystem) : bool =
        match VirtualFileSystem.tryGetContent inode vfs with
        | Some (InodeContent.Directory directory) -> Map.isEmpty directory.Entries
        | Some (InodeContent.RegularFile _)
        | Some (InodeContent.Symlink _) ->
            failwith
                $"RemovalChecks.isEmptyDirectory: inode %O{inode} is not a directory, so it has no entries to count. Ask isDirectory first (this is an interpreter bug)."
        | None ->
            failwith
                $"RemovalChecks.isEmptyDirectory: the walk resolved a name to inode %O{inode}, which the filesystem does not contain. Run VirtualFileSystem.checkInvariants."

[<RequireQualifiedAccess>]
module UnlinkRules =
    /// Linux's `unlink(2)`, transcribed from the measured ordering. Each arm
    /// beats the ones below it, and each bullet is a measured row:
    ///
    ///  * A path that consumed no component — "/", ".", "..", and any symlink
    ///    expansion of them — is EISDIR, whichever `FinalNavigation` it was and
    ///    whether or not the directory it reached is the root. Linux spends no
    ///    errno distinguishing them, where `rmdir` gives each its own (EBUSY,
    ///    EINVAL and ENOTEMPTY).
    ///  * A free final name is ENOENT, and that beats every check below:
    ///    `unlink("nowrite/nx/")` is ENOENT rather than the ENOTDIR the trailing
    ///    separator would earn or the EACCES the parent would.
    ///  * A trailing separator demands a directory, and reports what it found:
    ///    EISDIR for a directory, ENOTDIR for anything else. This is the arm
    ///    Linux's walk declines to make (`TrailingSeparatorPolicy.Ignore`), so
    ///    it never traverses a final symlink to get here — `unlink("ld/")`,
    ///    `unlink("dang/")`, `unlink("cyc/")` and `unlink("lroot/")` are all
    ///    ENOTDIR, with no ELOOP and no chance of destroying a link's target.
    ///  * Removing a name needs write on the directory holding it: EACCES.
    ///  * The target being a directory is EISDIR — *below* the write check, and
    ///    measured to be: `unlink("nowrite/kdir")` is EACCES where
    ///    `unlink("nowrite/kdir/")` is EISDIR. That pair is the only thing
    ///    separating this arm from the trailing-separator one, since they share
    ///    an errno.
    ///
    /// EISDIR here is privilege-independent: measured at uid 0, Linux still
    /// refuses to `unlink` a directory. `CallerPrivilege` gates the write bit
    /// and nothing else.
    let private linuxVerdict
        (privilege : CallerPrivilege)
        (resolution : Resolution)
        (vfs : VirtualFileSystem)
        : UnlinkVerdict
        =
        match resolution.Target with
        | ResolvedTarget.Directory _ -> UnlinkVerdict.Refuse UnixError.EISDIR
        | ResolvedTarget.Entry (directory, name, existing) ->

        match existing with
        | None -> UnlinkVerdict.Refuse UnixError.ENOENT
        | Some target ->

        if resolution.TrailingSeparatorDemanded then
            if RemovalChecks.isDirectory target vfs then
                UnlinkVerdict.Refuse UnixError.EISDIR
            else
                UnlinkVerdict.Refuse UnixError.ENOTDIR
        elif RemovalChecks.lacksWrite privilege directory name vfs then
            UnlinkVerdict.Refuse UnixError.EACCES
        elif RemovalChecks.isDirectory target vfs then
            UnlinkVerdict.Refuse UnixError.EISDIR
        else
            UnlinkVerdict.Remove (directory, name)

    /// Darwin's `unlink(2)`, transcribed from the measured ordering. Each arm
    /// beats the ones below it:
    ///
    ///  * A path that consumed no component at all — "/", or a symlink whose
    ///    target was "/" — is EISDIR.
    ///  * The root reached by "." or ".." is EBUSY, which is XNU's `unlink1`
    ///    refusing a mount's root vnode (`vp->v_flag & VROOT`). PawPrint mounts
    ///    one filesystem, so "the root of a mount" and "the root" are the same
    ///    inode. Measured: `unlink("/.")`, `unlink("/..")` and — through
    ///    `lroot -> "/"` — `unlink("lroot/.")` are EBUSY, where `unlink("d/.")`
    ///    on an ordinary directory is EPERM.
    ///  * Any other directory reached with no final name is EPERM.
    ///  * A free final name is ENOENT.
    ///  * The target being a directory is EPERM, and beats the write check:
    ///    `unlink("nowrite/kdir")` is EPERM where `unlink("nowrite/kid")` is
    ///    EACCES. This is the arm Linux orders the other way round.
    ///  * Removing a name needs write on the directory holding it: EACCES.
    ///
    /// EPERM is privilege-independent — measured at uid 0, where `unlink("d")`
    /// is still EPERM and `rmdir("d")` succeeds. The `unlink(2)` man page's "and
    /// the effective user ID of the process is not the super-user" is stale
    /// relative to modern XNU, which refuses unconditionally.
    ///
    /// Darwin's walk is `TrailingSeparatorPolicy.Demand`, so this function never
    /// sees `TrailingSeparatorDemanded` against a non-directory: the walk has
    /// already answered ENOTDIR (`unlink("f/")`, `unlink("lf/")`), ELOOP
    /// (`unlink("cyc/")`) or ENOENT (`unlink("dang/")`). What does reach here is
    /// a separator over a *directory*, whether named directly (`unlink("d/")`)
    /// or reached by following a final symlink (`unlink("ld/")`) — both EPERM,
    /// from the arm below, which is why the destructive divergence
    /// `Resolution.FinalSymlinkFollowed` warns about costs `unlink` nothing.
    let private darwinVerdict
        (privilege : CallerPrivilege)
        (resolution : Resolution)
        (vfs : VirtualFileSystem)
        : UnlinkVerdict
        =
        match resolution.Target with
        | ResolvedTarget.Directory (inode, reachedBy) ->
            match reachedBy with
            | FinalNavigation.Root -> UnlinkVerdict.Refuse UnixError.EISDIR
            | FinalNavigation.Current
            | FinalNavigation.Parent ->
                if inode = VirtualFileSystem.root vfs then
                    UnlinkVerdict.Refuse UnixError.EBUSY
                else
                    UnlinkVerdict.Refuse UnixError.EPERM
        | ResolvedTarget.Entry (directory, name, existing) ->

        match existing with
        | None -> UnlinkVerdict.Refuse UnixError.ENOENT
        | Some target ->

        if RemovalChecks.isDirectory target vfs then
            UnlinkVerdict.Refuse UnixError.EPERM
        elif RemovalChecks.lacksWrite privilege directory name vfs then
            UnlinkVerdict.Refuse UnixError.EACCES
        else
            UnlinkVerdict.Remove (directory, name)

    /// Decide what an `unlink(2)` owes, given how its path resolved.
    ///
    /// Two whole functions rather than one reading a rules record, against the
    /// `MkDirRules.verdict` precedent, because what diverges here is the *order*
    /// of the checks and the errno vocabulary rather than a constant they both
    /// consult. A record spelling that as `{ DirectoryErrno; RootNavigationErrno;
    /// TypeCheckPrecedesPermission : bool }` would make most of its inhabitants
    /// describe a kernel nobody ships, and a boolean that reorders control flow
    /// is exactly the illegal-state-representable shape this codebase avoids.
    /// Each function above instead reads top-to-bottom against its own measured
    /// column.
    ///
    /// The same argument rules out `SimulatedUnixPlatform.bindFaultOrder`'s
    /// shape — compute the fault set, then pick the first by a per-flavour
    /// order — which works there because both flavours agree on the faults and
    /// on the errno each carries. Here they agree on neither.
    let verdict
        (flavour : SimulatedUnixFlavour)
        (privilege : CallerPrivilege)
        (resolution : Resolution)
        (vfs : VirtualFileSystem)
        : UnlinkVerdict
        =
        match flavour with
        | SimulatedUnixFlavour.Linux -> linuxVerdict privilege resolution vfs
        | SimulatedUnixFlavour.Darwin -> darwinVerdict privilege resolution vfs

/// What `getcwd(3)` answers when the current directory has been *removed* — so
/// there is no path to report — and how small a buffer can still change that
/// answer.
///
/// Only reachable since `rmdir` could orphan a current directory. Measured on
/// both with the cwd removed out from under the process, sweeping the size from
/// 1 past the length of the path that used to be there: a zero-length buffer is
/// EINVAL everywhere (the shim's own guard, before `getcwd` is called at all),
/// and everything else splits on the *first byte* only.
/// What the PAL puts in `DirectoryEntry.NameLength`, which is a fact about the
/// libc it was compiled against rather than about any directory.
///
/// `ConvertDirent` (`pal_io.c:497`) copies `d_namlen` under
/// `HAVE_DIRENT_NAME_LEN` and writes `-1` otherwise, the sentinel meaning "walk
/// to the NUL yourself". Established by compiling rather than by reading:
/// glibc's `struct dirent` has no `d_namlen` member at all (`gcc` rejects
/// `d.d_namlen`), while macOS's `sys/dirent.h` declares one.
///
/// Invisible to managed code — `DirectoryEntry.GetName` takes
/// `CreateReadOnlySpanFromNullTerminated` for the sentinel and a plain span
/// otherwise — so only a guest that hand-rolls the P/Invoke can tell.
[<RequireQualifiedAccess>]
type DirectoryEntryNameLength =
    /// The name's length in bytes, as macOS reports it.
    | Reported
    /// `-1`, as every libc without `d_namlen` gets.
    | WalkToTerminator

[<RequireQualifiedAccess>]
type GetCwdOrphanAnswer =
    /// ENOENT whatever the size. Linux's `sys_getcwd` builds the path, fails
    /// because it is disconnected, and never reaches the length comparison —
    /// measured ENOENT at every size from 1 up.
    | AlwaysDetached
    /// ENOENT unless the buffer cannot hold even `"/"` and a terminator, which
    /// is ERANGE. Darwin's `getcwd(3)` builds the path from the root downwards,
    /// so it needs those two bytes before it can start; measured, size 1 is
    /// ERANGE and *every* larger size is ENOENT — including sizes far below the
    /// length of the path that used to be there. It is a minimum, not a
    /// comparison against a path that no longer exists.
    | ShortestPathFirst

/// Everything a kernel does differently when `rmdir(2)` removes a directory.
///
/// Two fields, and the rest of the divergence — the *order* of the refusals and
/// the errno vocabulary — lives in `RmDirRules.linuxVerdict` and
/// `RmDirRules.darwinVerdict` rather than here, for the reason
/// `UnlinkRules.verdict` gives.
///
/// Measured on macOS 26.6/APFS at uid 501, and Linux 6.x arm64 at uid 1000 and
/// uid 0, one fresh tree per row.
type RmDirRules =
    {
        /// The walk `rmdir` resolves its path with, under
        /// `SymlinkPolicy.NoFollowFinal` on both platforms. Linux `Ignore`,
        /// Darwin `Demand`, exactly as `unlink`'s is and for the same reason.
        ///
        /// This is the field that makes the two flavours **destroy different
        /// objects**. With `ld -> d` and `d` an empty directory, `rmdir("ld/")`
        /// is ENOTDIR on Linux — whose walk cannot have traversed the link — and
        /// *removes `d`* on Darwin, whose walk did. It is the divergence
        /// `Resolution.FinalSymlinkFollowed` warns about, and the reason this
        /// syscall dispatches on the flavour rather than picking a column.
        TrailingSeparator : TrailingSeparatorPolicy
        /// What removing the directory does to the removed directory's own
        /// inode, which the flavours do not agree on.
        ///
        /// Measured through a descriptor held across the call, reproduced 3/3 on
        /// each: Linux drops the directory's `st_nlink` from 2 to 0 and moves its
        /// `ctime`, while Darwin leaves both alone. It is one fact, not two —
        /// nothing about the Darwin inode changed, so its `ctime` has no reason
        /// to move.
        ///
        /// Guest-observable, which is why it is modelled rather than approximated:
        /// `SystemNative_FStat` on a directory descriptor writes
        /// `InodeTimes.StatusChange` into `FileStatus`. (`st_nlink` itself is not
        /// a `FileStatus` field, so only its shadow on `ctime` can be read.)
        ///
        /// `unlink` needs no such field: removing a *file*'s last name moves its
        /// `ctime` on both.
        RemovedDirectoryEffect : UnbindTargetEffect
    }

/// What `opendir(3)` should do next, once its path has been resolved.
[<RequireQualifiedAccess>]
type OpenDirVerdict =
    /// Answer the guest with this errno, and a NULL `DIR*`.
    | Refuse of error : UnixError
    /// Open a stream over this directory.
    | Open of directory : InodeNumber

[<RequireQualifiedAccess>]
module OpenDirRules =
    /// `opendir(3)`, transcribed from the measured ordering. Each arm beats the
    /// ones below it, and each bullet is a row measured on **both** kernels —
    /// there is no flavour parameter because there is no row they disagree on,
    /// which is why this takes none rather than defaulting one:
    ///
    ///  * A name nothing binds is ENOENT, and so is a dangling symlink: the walk
    ///    follows the final link, so there is nothing left to open.
    ///  * A target that is not a directory is ENOTDIR, and that beats the
    ///    permission check. The row proving it is a **mode-0000 regular file**,
    ///    which is ENOTDIR rather than EACCES — with and without a trailing
    ///    separator, and through a symlink to one. Pleasingly symmetric with
    ///    `open`'s own measured "EISDIR beats EACCES".
    ///  * A directory that refuses this caller the **read** bit is EACCES. Read,
    ///    not search, and this is the first place in this codebase where the two
    ///    come apart: a `0o111` directory (search, no read) is EACCES, while a
    ///    `0o444` one (read, no search) opens and lists every name. Search on the
    ///    *ancestors* is the walk's business and a resolution that got here has
    ///    passed it.
    ///
    /// `Resolution.TrailingSeparatorDemanded` is never read, and does not need
    /// to be: the demand is "the final component must be a directory", which
    /// `opendir` owes anyway. Measured, every `X/` row answers what its `X` row
    /// answers.
    ///
    /// There is no root-navigation arm either, and `rmdir`'s three are the
    /// reason to say so rather than leave it implied: `opendir("/")`,
    /// `opendir("d/.")` and `opendir("d/..")` all simply succeed, on both.
    let verdict (privilege : CallerPrivilege) (resolution : Resolution) (vfs : VirtualFileSystem) : OpenDirVerdict =
        match VirtualFileSystem.existingOf resolution.Target with
        | Error error -> OpenDirVerdict.Refuse error
        | Ok inode ->

        match VirtualFileSystem.tryGetContent inode vfs with
        | None ->
            failwith
                $"OpenDirRules.verdict: the walk resolved to inode %O{inode}, which the filesystem does not contain. Run VirtualFileSystem.checkInvariants."
        | Some (InodeContent.RegularFile _)
        | Some (InodeContent.Symlink _) ->
            // The symlink arm is unreachable through the resolver, which
            // followed every final link and answered ENOENT for a dangling one.
            // It is the same answer either way, so there is nothing to refuse.
            OpenDirVerdict.Refuse UnixError.ENOTDIR
        | Some (InodeContent.Directory content) ->

        if PermissionBits.deniedTo privilege 0o400 content.Permissions then
            OpenDirVerdict.Refuse UnixError.EACCES
        else
            OpenDirVerdict.Open inode

/// What `rmdir(2)` should do next, once its path has been resolved.
[<RequireQualifiedAccess>]
type RmDirVerdict =
    /// Answer the guest with this errno.
    | Refuse of error : UnixError
    /// Remove `name` from `directory`, and — since no other name can point at a
    /// directory — free the inode unless a descriptor or the current directory
    /// still holds it.
    ///
    /// Carries no inode for the reason `UnlinkVerdict.Remove` carries none: the
    /// removing code gets it from `VirtualFileSystem.unbind`, which answers the
    /// inode it actually unbound.
    | Remove of directory : InodeNumber * name : FileName

[<RequireQualifiedAccess>]
module RmDirRules =
    /// Linux's `rmdir(2)`, transcribed from the measured ordering. Each arm
    /// beats the ones below it, and each bullet is a measured row:
    ///
    ///  * A path that consumed no component at all — "/" — is EBUSY. Linux
    ///    specialises the *path*, not the inode: `rmdir("/")` is EBUSY where
    ///    `rmdir("/.")` is EINVAL.
    ///  * A path whose last component was "." is EINVAL, whatever directory it
    ///    reached: `rmdir(".")`, `rmdir("d/.")` and `rmdir("/.")` all are.
    ///  * A path whose last component was ".." is ENOTEMPTY, again whatever it
    ///    reached. Not a coincidence with the emptiness check below — the parent
    ///    of any directory necessarily contains that directory — but it *is* a
    ///    separate arm, and the row proving it is `rmdir("nowrite/kdir/..")`,
    ///    which is ENOTEMPTY where the write check below would say EACCES.
    ///  * A free final name is ENOENT, and that beats the write check:
    ///    `rmdir("nowrite/nx")` is ENOENT.
    ///  * Removing a name needs write on the directory holding it: EACCES.
    ///  * The target not being a directory is ENOTDIR — *below* the write check,
    ///    and measured to be: `rmdir("nowrite/kid")` is EACCES at uid 1000 and
    ///    ENOTDIR at uid 0. This is the arm Darwin orders the other way round.
    ///  * A directory that still holds an entry is ENOTEMPTY.
    ///
    /// `Resolution.TrailingSeparatorDemanded` is never read, and does not need
    /// to be: the demand is "the final component must be a directory", which
    /// `rmdir` owes anyway. Measured, every `X/` row answers what its `X` row
    /// answers.
    ///
    /// Measured at uid 0, every row: the EACCES rows fall through to their next
    /// check and nothing else moves, so `CallerPrivilege` gates the write bit
    /// and nothing else.
    let private linuxVerdict
        (privilege : CallerPrivilege)
        (resolution : Resolution)
        (vfs : VirtualFileSystem)
        : RmDirVerdict
        =
        match resolution.Target with
        | ResolvedTarget.Directory (_, reachedBy) ->
            match reachedBy with
            | FinalNavigation.Root -> RmDirVerdict.Refuse UnixError.EBUSY
            | FinalNavigation.Current -> RmDirVerdict.Refuse UnixError.EINVAL
            | FinalNavigation.Parent -> RmDirVerdict.Refuse UnixError.ENOTEMPTY
        | ResolvedTarget.Entry (directory, name, existing) ->

        match existing with
        | None -> RmDirVerdict.Refuse UnixError.ENOENT
        | Some target ->

        if RemovalChecks.lacksWrite privilege directory name vfs then
            RmDirVerdict.Refuse UnixError.EACCES
        elif not (RemovalChecks.isDirectory target vfs) then
            RmDirVerdict.Refuse UnixError.ENOTDIR
        elif not (RemovalChecks.isEmptyDirectory target vfs) then
            RmDirVerdict.Refuse UnixError.ENOTEMPTY
        else
            RmDirVerdict.Remove (directory, name)

    /// Darwin's `rmdir(2)`, transcribed from the measured ordering. Each arm
    /// beats the ones below it:
    ///
    ///  * A path that consumed no component at all — "/", or a symlink whose
    ///    target was "/" — is EISDIR. Where Linux gives that path EBUSY.
    ///  * The root reached by "." or ".." is EBUSY, which is XNU refusing a
    ///    mount's root vnode; PawPrint mounts one filesystem, so "the root of a
    ///    mount" and "the root" are the same inode. Measured: `rmdir("/.")`,
    ///    `rmdir("/..")` and — through `lroot -> "/"` — `rmdir("lroot/.")` are
    ///    EBUSY, where Linux answers those EINVAL and ENOTEMPTY. So Darwin
    ///    specialises the *inode* where Linux specialises the path.
    ///  * Any other directory reached by "." is EINVAL, and by ".." is
    ///    ENOTEMPTY — agreeing with Linux once the root is out of the way.
    ///  * A free final name is ENOENT.
    ///  * The target not being a directory is ENOTDIR, and beats the write
    ///    check: `rmdir("nowrite/kid")` is ENOTDIR where `rmdir("nowrite/kdir")`
    ///    is EACCES. This is the arm Linux orders the other way round.
    ///  * Removing a name needs write on the directory holding it: EACCES.
    ///  * A directory that still holds an entry is ENOTEMPTY, and the write
    ///    check beats it: `rmdir("nowrite/kfull")` is EACCES.
    ///
    /// Darwin's walk is `TrailingSeparatorPolicy.Demand`, so a separator over a
    /// non-directory never reaches here — the walk has already answered ENOTDIR
    /// (`rmdir("f/")`, `rmdir("lf/")`), ELOOP (`rmdir("cyc/")`) or ENOENT
    /// (`rmdir("dang/")`). What does reach here is a separator over a directory
    /// a final symlink named, and that is the destructive row: `rmdir("ld/")`
    /// removes `d`.
    let private darwinVerdict
        (privilege : CallerPrivilege)
        (resolution : Resolution)
        (vfs : VirtualFileSystem)
        : RmDirVerdict
        =
        match resolution.Target with
        | ResolvedTarget.Directory (inode, reachedBy) ->
            match reachedBy with
            | FinalNavigation.Root -> RmDirVerdict.Refuse UnixError.EISDIR
            | FinalNavigation.Current ->
                if inode = VirtualFileSystem.root vfs then
                    RmDirVerdict.Refuse UnixError.EBUSY
                else
                    RmDirVerdict.Refuse UnixError.EINVAL
            | FinalNavigation.Parent ->
                if inode = VirtualFileSystem.root vfs then
                    RmDirVerdict.Refuse UnixError.EBUSY
                else
                    RmDirVerdict.Refuse UnixError.ENOTEMPTY
        | ResolvedTarget.Entry (directory, name, existing) ->

        match existing with
        | None -> RmDirVerdict.Refuse UnixError.ENOENT
        | Some target ->

        if not (RemovalChecks.isDirectory target vfs) then
            RmDirVerdict.Refuse UnixError.ENOTDIR
        elif RemovalChecks.lacksWrite privilege directory name vfs then
            RmDirVerdict.Refuse UnixError.EACCES
        elif not (RemovalChecks.isEmptyDirectory target vfs) then
            RmDirVerdict.Refuse UnixError.ENOTEMPTY
        else
            RmDirVerdict.Remove (directory, name)

    /// Decide what an `rmdir(2)` owes, given how its path resolved.
    ///
    /// Two whole functions rather than one reading a rules record, for the
    /// reason `UnlinkRules.verdict` states: what diverges is the order of the
    /// checks and the errno vocabulary rather than a constant they both consult.
    /// `rmdir` makes the case more strongly than `unlink` did — the two flavours
    /// disagree about which of the root and the *path to it* is the special
    /// thing, which no table of errnos can express.
    let verdict
        (flavour : SimulatedUnixFlavour)
        (privilege : CallerPrivilege)
        (resolution : Resolution)
        (vfs : VirtualFileSystem)
        : RmDirVerdict
        =
        match flavour with
        | SimulatedUnixFlavour.Linux -> linuxVerdict privilege resolution vfs
        | SimulatedUnixFlavour.Darwin -> darwinVerdict privilege resolution vfs

/// Everything a kernel does differently when `rename(2)` moves a name.
///
/// One field, and the rest of the divergence — the *order* of the refusals and
/// the errno vocabulary — lives in `RenameRules.linuxVerdict` and
/// `RenameRules.darwinVerdict`, for the reason `UnlinkRules.verdict` gives.
/// `rename` diverges more than any operation before it: the two flavours
/// disagree about where the permission checks sit, about *which* directory's
/// write bit a directory-over-directory rename even consults, and about where
/// the no-op sits.
///
/// Measured on macOS 26.6/APFS at uid 501 and Linux 6.x arm64 at uid 1000 and
/// uid 0, one fresh tree per row; `docs/probes/rename/` holds the probes.
type RenameRules =
    {
        /// The walk `rename` resolves *both* of its paths with, under
        /// `SymlinkPolicy.NoFollowFinal` on both platforms. Linux `Ignore`,
        /// Darwin `Demand`, exactly as `unlink`'s and `rmdir`'s are.
        ///
        /// One field for two paths because, measured, each kernel resolves its
        /// source and its destination under the same policy — there is no row
        /// where a separator costs one path something it does not cost the
        /// other.
        ///
        /// This is the field that makes the two flavours **destroy different
        /// objects**, the divergence `Resolution.FinalSymlinkFollowed` warns
        /// about. With `s -> real` a directory, `rename("s/", "moved")` moves
        /// *real* on Darwin, leaving `s` dangling, and is ENOTDIR on Linux;
        /// `rename("src", "s/")` replaces *real* on Darwin and is ENOTDIR on
        /// Linux.
        TrailingSeparator : TrailingSeparatorPolicy
    }

/// What `rename(2)` should do next, once both of its paths have been resolved.
[<RequireQualifiedAccess>]
type RenameVerdict =
    /// Answer the guest with this errno.
    | Refuse of error : UnixError
    /// Both paths name one inode. Succeed, and change nothing at all — not a
    /// binding, not a timestamp.
    ///
    /// A case here rather than a short-circuit in `VirtualFileSystem.rename`
    /// because its *position* is one of the things the flavours disagree about:
    /// Linux answers success for a no-op whose parent the caller may not write,
    /// and Darwin answers EACCES for the same call.
    | NoOp
    /// Move `sourceName` out of `sourceDirectory` and bind it as
    /// `destinationName` in `destinationDirectory`, displacing whatever is bound
    /// there.
    ///
    /// Carries no inode, though the verdict read several to decide: the moving
    /// code gets the displaced one from `VirtualFileSystem.rename`, which
    /// answers what it actually displaced, so there is one source for "which
    /// inode lost a name" and it is the one the move performed.
    | Move of
        sourceDirectory : InodeNumber *
        sourceName : FileName *
        destinationDirectory : InodeNumber *
        destinationName : FileName

/// The questions `rename(2)` asks about the four directories it can refuse for.
[<RequireQualifiedAccess>]
module private RenameChecks =
    /// Whether `inode` refuses this caller the write bit.
    ///
    /// Asked of four different directories — the source's parent, the
    /// destination's parent, the moved directory (whose ".." a change of parent
    /// rewrites) and, on Darwin only, the directory a directory displaces — so
    /// `role` names which, for the crash message. Only the owner triple can ever
    /// apply, since `stat` reports `Kernel.UserId` as every inode's `st_uid`,
    /// and the sticky bit can never refuse for the same reason.
    ///
    /// Partial in `inode`, which every caller has just obtained from a
    /// resolution or from a directory entry.
    let lacksWrite
        (role : string)
        (privilege : CallerPrivilege)
        (inode : InodeNumber)
        (vfs : VirtualFileSystem)
        : bool
        =
        match VirtualFileSystem.tryGet inode vfs with
        | Some entry ->
            match VirtualFileSystem.permissions entry with
            | InodePermissions.Stored bits -> PermissionBits.deniedTo privilege 0o200 bits
            | InodePermissions.PlatformSymlinkDefault ->
                failwith
                    $"RenameChecks.lacksWrite: %s{role} is inode %O{inode}, which reports platform-default symlink permissions -- but rename only asks this of a directory (this is an interpreter bug)."
        | None ->
            failwith
                $"RenameChecks.lacksWrite: %s{role} is inode %O{inode}, which the filesystem does not contain. Run VirtualFileSystem.checkInvariants."

    /// The inode the destination name is bound to, when that inode is a
    /// directory. `None` covers both "the name is free" and "it names something
    /// that is not a directory", which no arm needs to tell apart — the arms
    /// that care about a non-directory ask `namesNonDirectory` instead.
    let existingDirectory (existing : InodeNumber option) (vfs : VirtualFileSystem) : InodeNumber option =
        existing |> Option.filter (fun inode -> RemovalChecks.isDirectory inode vfs)

    /// Whether the name is bound, and bound to something that is not a
    /// directory. False for a free name, which is what separates this from
    /// `not existingDirectory.IsSome`.
    let namesNonDirectory (existing : InodeNumber option) (vfs : VirtualFileSystem) : bool =
        match existing with
        | Some inode -> not (RemovalChecks.isDirectory inode vfs)
        | None -> false

[<RequireQualifiedAccess>]
module RenameRules =
    /// Linux's `rename(2)`, transcribed from the measured ordering. Each arm
    /// beats the ones below it, and each bullet is a measured row:
    ///
    ///  * A path — either path — that consumed no final name, so "/", "." and
    ///    ".." and any symlink expansion of them, is EBUSY. Linux spends one
    ///    errno on all six positions where `rmdir` spends three: `rename("/", x)`,
    ///    `rename("/.", x)`, `rename("d/.", x)`, `rename("d/..", x)`,
    ///    `rename(x, "d/.")` and `rename(dir, "/")` are all EBUSY. The source is
    ///    asked before the destination, which no row can see, since they share
    ///    an errno.
    ///  * A destination whose parent directory has lost its own last name is
    ///    ENOENT, exactly as `mkdir` and `open(O_CREAT)` answer — and it beats
    ///    the source's trailing-separator demand and the write checks below.
    ///    Only reachable when the current directory is that orphan.
    ///  * A free source name is ENOENT, and beats the trailing-separator arms:
    ///    `rename("nope/", "g")` is ENOENT where `rename("f/", "g")` is ENOTDIR.
    ///  * A trailing separator on the **source** demands the source be a
    ///    directory: ENOTDIR otherwise. This is the arm Linux's walk declines to
    ///    make (`TrailingSeparatorPolicy.Ignore`), so it never traverses a final
    ///    symlink to get here — `rename("lf/", "g")`, `rename("dang/", "g")` and
    ///    `rename("lroot/", "g")` are all ENOTDIR, with no chance of moving a
    ///    link's target.
    ///  * A trailing separator on the **destination** demands that the *source*
    ///    be a directory: `rename(f, "absent/")` and `rename(f, "d/")` are
    ///    ENOTDIR, and so is `rename(p/f, "q/absent/")` with `q` unwritable,
    ///    which is what puts this arm above the write checks.
    ///
    ///    It demands nothing of the destination, and seeing that needs an
    ///    unwritable parent: `rename(d, "q/l/")` with `l` a symlink to a
    ///    directory is ENOTDIR when `q` is writable and **EACCES** when it is
    ///    not. So the ENOTDIR there is the ordinary type rule further down, not
    ///    this arm — the two are indistinguishable until a check between them
    ///    fires.
    ///  * Both paths naming one inode changes nothing and succeeds, and that
    ///    beats every permission check below: `rename(f, g)` with `g` a hard link
    ///    to `f` succeeds from a parent the caller may not write, and so does the
    ///    self-rename of a non-empty directory.
    ///  * A destination directory inside the source's own subtree is EINVAL, and
    ///    it beats *both* write checks — `rename(p/a, p/a/b)` is EINVAL with `p`
    ///    unwritable — as well as the type arm below: `rename(a, a/b/f)` with
    ///    `a/b/f` a regular file is EINVAL, not ENOTDIR.
    ///  * Each parent must grant write: EACCES. Above the type arm, which is
    ///    where Linux and Darwin part company — `rename(p/f, q/dir)` with `p`
    ///    unwritable is EACCES here and EISDIR on Darwin.
    ///  * Then the type rule: a directory over a non-directory is ENOTDIR, a
    ///    non-directory over a directory is EISDIR. A symlink is a
    ///    non-directory whatever it points at, since both walks are
    ///    `NoFollowFinal`.
    ///  * Moving a **directory to a different parent** rewrites its own ".."
    ///    entry, so it demands write on the moved directory itself: EACCES.
    ///    Renaming one within its parent changes nothing inside it and demands
    ///    nothing -- and that holds even when it *displaces* a directory there,
    ///    which is measured (40/40) and is where Darwin diverges a second time. This check is *below* the type arm, unlike the parents' —
    ///    `rename(p/m, q/file)` with `p/m` unwritable is ENOTDIR — and above
    ///    ENOTEMPTY.
    ///  * A destination directory that still holds an entry is ENOTEMPTY.
    ///
    /// Linux never consults the mode of the thing being displaced: measured,
    /// `rename(dir, emptydir)` succeeds with the destination at mode 0. That is
    /// the arm Darwin has and this one does not.
    ///
    /// Measured at uid 0, every row: the EACCES rows fall through to their next
    /// check and nothing else moves, so `CallerPrivilege` gates the write bits
    /// and nothing else.
    let private linuxVerdict
        (privilege : CallerPrivilege)
        (source : Resolution)
        (destination : Resolution)
        (vfs : VirtualFileSystem)
        : RenameVerdict
        =
        match source.Target with
        | ResolvedTarget.Directory _ -> RenameVerdict.Refuse UnixError.EBUSY
        | ResolvedTarget.Entry (sourceDirectory, sourceName, sourceExisting) ->

        match destination.Target with
        | ResolvedTarget.Directory _ -> RenameVerdict.Refuse UnixError.EBUSY
        | ResolvedTarget.Entry (destinationDirectory, destinationName, destinationExisting) ->

        if VirtualFileSystem.isOrphanedDirectory destinationDirectory vfs then
            RenameVerdict.Refuse UnixError.ENOENT
        else

        match sourceExisting with
        | None -> RenameVerdict.Refuse UnixError.ENOENT
        | Some moved ->

        let movedIsDirectory = RemovalChecks.isDirectory moved vfs
        let displacedDirectory = RenameChecks.existingDirectory destinationExisting vfs
        let displacesNonDirectory = RenameChecks.namesNonDirectory destinationExisting vfs

        if source.TrailingSeparatorDemanded && not movedIsDirectory then
            RenameVerdict.Refuse UnixError.ENOTDIR
        elif destination.TrailingSeparatorDemanded && not movedIsDirectory then
            RenameVerdict.Refuse UnixError.ENOTDIR
        elif destinationExisting = Some moved then
            RenameVerdict.NoOp
        elif
            movedIsDirectory
            && VirtualFileSystem.isWithinSubtree moved destinationDirectory vfs
        then
            RenameVerdict.Refuse UnixError.EINVAL
        elif RenameChecks.lacksWrite "the source's parent" privilege sourceDirectory vfs then
            RenameVerdict.Refuse UnixError.EACCES
        elif RenameChecks.lacksWrite "the destination's parent" privilege destinationDirectory vfs then
            RenameVerdict.Refuse UnixError.EACCES
        elif movedIsDirectory && displacesNonDirectory then
            RenameVerdict.Refuse UnixError.ENOTDIR
        elif not movedIsDirectory && displacedDirectory.IsSome then
            RenameVerdict.Refuse UnixError.EISDIR
        elif
            movedIsDirectory
            && sourceDirectory <> destinationDirectory
            && RenameChecks.lacksWrite "the moved directory" privilege moved vfs
        then
            RenameVerdict.Refuse UnixError.EACCES
        else

        match displacedDirectory with
        | Some displaced when not (RemovalChecks.isEmptyDirectory displaced vfs) ->
            RenameVerdict.Refuse UnixError.ENOTEMPTY
        | Some _
        | None -> RenameVerdict.Move (sourceDirectory, sourceName, destinationDirectory, destinationName)

    /// Darwin's `rename(2)`, transcribed from the measured ordering. Each arm
    /// beats the ones below it:
    ///
    ///  * A destination whose parent directory has lost its own last name is
    ///    ENOENT, and on this flavour that beats *everything*, including the
    ///    source's navigation refusal: from inside an `rmdir`'d current
    ///    directory, `rename("d/.", "x")` is ENOENT where it is EINVAL from
    ///    anywhere else.
    ///  * A **source** that consumed no final name: "/" is EISDIR, and any
    ///    directory reached by "." or ".." is EINVAL -- the root included, which
    ///    took an APFS disk image to establish because EXDEV masks it on some
    ///    approaches. See the arm for the rows. Where Linux spends EBUSY on all
    ///    of them.
    ///  * A free source name is ENOENT, and beats the destination's navigation
    ///    arm below: `rename("nope", "d/.")` is ENOENT here and EBUSY on Linux.
    ///  * A **destination** that consumed no final name: "." and ".." are EINVAL
    ///    whatever the source is and whatever they reached — measured with a ".."
    ///    that is not an ancestor of the source, so the rule is about the
    ///    component rather than about ancestry. "/" is not special-cased and
    ///    falls to the type rule: `rename(file, "/")` is EISDIR while
    ///    `rename(dir, "/")` is EINVAL.
    ///  * The type rule, which on this flavour is above everything below it: a
    ///    directory over a non-directory is ENOTDIR, a non-directory over a
    ///    directory is EISDIR. `rename(p/f, q/dir)` with `p` unwritable is EISDIR
    ///    here and EACCES on Linux, and `rename(a, a/b/f)` with `a/b/f` a file is
    ///    ENOTDIR here and EINVAL on Linux.
    ///  * A trailing separator on the destination, over a name that is *free*,
    ///    demands that the source be a directory: ENOENT otherwise.
    ///    `rename(f, "absent/")` is ENOENT where `rename(d, "absent/")` succeeds
    ///    — XNU passes `WILLBEDIR` to the destination lookup exactly when the
    ///    source is a directory. Linux answers the same shape ENOTDIR. The
    ///    source's own separator needs no arm: Darwin's walk is `Demand` and has
    ///    already refused it.
    ///  * A destination directory inside the source's own subtree is EINVAL,
    ///    beating both write checks below.
    ///  * The source's parent must grant write: EACCES. Above the no-op, which
    ///    is the arm Linux orders the other way round — `rename(f, g)` with `g` a
    ///    hard link to `f` is EACCES here from an unwritable parent, and succeeds
    ///    on Linux.
    ///  * Then a write check on the destination side, and *which* directory it
    ///    asks about is the strangest measured fact in this syscall: when a
    ///    directory replaces an existing directory, Darwin consults the write bit
    ///    of the **directory being displaced** and never looks at its parent at
    ///    all. Measured four ways — with the parent at 0o555 and the displaced
    ///    directory at 0o755 it succeeds, at 0o755 and 0o000 it is EACCES, at
    ///    0o555 and 0o300 it succeeds, and a control confirms the parent really
    ///    does refuse an ordinary create. Every other shape consults the
    ///    destination's parent as Linux does.
    ///  * Both paths naming one inode changes nothing and succeeds — below the
    ///    two write checks above, which is why the self-rename of a directory
    ///    whose own write bit is missing is EACCES here and succeeds on Linux.
    ///  * Moving a directory demands write on the moved directory -- on *two*
    ///    occasions where Linux wants one. Linux asks only when the parent
    ///    changes, which is the ".." rewrite; Darwin asks then and also whenever
    ///    the moved directory displaces another directory, within one parent
    ///    included. Measured 40/40: `rename("p/m", "p/d")` with `m` at 0o555 and
    ///    `d` an existing directory is EACCES here and succeeds on Linux, while
    ///    the same call to a free name succeeds on both.
    ///  * A destination directory that still holds an entry is ENOTEMPTY, below
    ///    the displaced-directory write check: `rename(dir, fulldir)` with the
    ///    non-empty destination at mode 0 is EACCES here and ENOTEMPTY on Linux.
    ///
    /// Darwin's walk is `TrailingSeparatorPolicy.Demand`, so a separator over an
    /// *existing* non-directory never reaches here — the walk has already
    /// answered ENOTDIR, ELOOP or ENOENT. What does reach here is a separator
    /// over a directory a final symlink named, and that is the destructive row:
    /// `rename("s/", "moved")` moves the link's target.
    let private darwinVerdict
        (privilege : CallerPrivilege)
        (source : Resolution)
        (destination : Resolution)
        (vfs : VirtualFileSystem)
        : RenameVerdict
        =
        let destinationParentIsOrphan =
            match destination.Target with
            | ResolvedTarget.Entry (destinationDirectory, _, _) ->
                VirtualFileSystem.isOrphanedDirectory destinationDirectory vfs
            | ResolvedTarget.Directory _ -> false

        if destinationParentIsOrphan then
            RenameVerdict.Refuse UnixError.ENOENT
        else

        match source.Target with
        | ResolvedTarget.Directory (_, reachedBy) ->
            match reachedBy with
            | FinalNavigation.Root -> RenameVerdict.Refuse UnixError.EISDIR
            // No root special case, unlike Darwin's `unlink` and `rmdir`, which
            // each give the root its own EBUSY arm. Establishing that took some
            // care, because the obvious measurement is masked: a filesystem root
            // that is not "/" is a *mount* root, and renaming one is liable to
            // EXDEV.
            //
            // Measured on a fresh APFS image, 40 trials per row, all stable. The
            // discriminator turns out not to be "." against ".." but whether the
            // source's parent directory and the destination's parent directory
            // are the same object: with `p` a directory inside the mount,
            // `rename("base/.", "p/x")` and `rename("p/..", "base/x")` both reach
            // the mount root and both answer **EINVAL**, while the same two
            // sources with the destination in the other directory answer EXDEV.
            // So EXDEV is the mount boundary talking, and where it stays quiet
            // the root answers exactly what any other directory answers.
            //
            // PawPrint has one filesystem and no mounts, so nothing here can
            // produce EXDEV and the EINVAL readings are the applicable ones.
            | FinalNavigation.Current
            | FinalNavigation.Parent -> RenameVerdict.Refuse UnixError.EINVAL
        | ResolvedTarget.Entry (sourceDirectory, sourceName, sourceExisting) ->

        match sourceExisting with
        | None -> RenameVerdict.Refuse UnixError.ENOENT
        | Some moved ->

        let movedIsDirectory = RemovalChecks.isDirectory moved vfs

        match destination.Target with
        | ResolvedTarget.Directory (_, reachedBy) ->
            match reachedBy with
            | FinalNavigation.Current
            | FinalNavigation.Parent -> RenameVerdict.Refuse UnixError.EINVAL
            | FinalNavigation.Root ->
                if movedIsDirectory then
                    RenameVerdict.Refuse UnixError.EINVAL
                else
                    RenameVerdict.Refuse UnixError.EISDIR
        | ResolvedTarget.Entry (destinationDirectory, destinationName, destinationExisting) ->

        let displacedDirectory = RenameChecks.existingDirectory destinationExisting vfs
        let displacesNonDirectory = RenameChecks.namesNonDirectory destinationExisting vfs

        if movedIsDirectory && displacesNonDirectory then
            RenameVerdict.Refuse UnixError.ENOTDIR
        elif not movedIsDirectory && displacedDirectory.IsSome then
            RenameVerdict.Refuse UnixError.EISDIR
        elif
            destination.TrailingSeparatorDemanded
            && destinationExisting.IsNone
            && not movedIsDirectory
        then
            RenameVerdict.Refuse UnixError.ENOENT
        elif
            movedIsDirectory
            && VirtualFileSystem.isWithinSubtree moved destinationDirectory vfs
        then
            RenameVerdict.Refuse UnixError.EINVAL
        elif RenameChecks.lacksWrite "the source's parent" privilege sourceDirectory vfs then
            RenameVerdict.Refuse UnixError.EACCES
        elif
            // Which directory this asks about is the measured oddity. A
            // directory displacing a directory is the one shape where Darwin
            // consults the displaced object rather than the directory holding
            // it -- and `displacedDirectory` being `Some` here already implies
            // the source is a directory, because the EISDIR arm above refused
            // the only other way to reach this line with one.
            (match displacedDirectory with
             | Some displaced -> RenameChecks.lacksWrite "the displaced directory" privilege displaced vfs
             | None -> RenameChecks.lacksWrite "the destination's parent" privilege destinationDirectory vfs)
        then
            RenameVerdict.Refuse UnixError.EACCES
        elif destinationExisting = Some moved then
            RenameVerdict.NoOp
        elif
            // Two occasions, not one, and this is where Darwin parts from Linux
            // a second time. Linux wants this bit only when the parent changes,
            // which is the ".." rewrite and nothing else. Darwin wants it then
            // *and* whenever the moved directory displaces another directory,
            // even within one parent: measured 40/40, `rename("p/m", "p/d")`
            // with `m` at 0o555 and `d` an existing directory is EACCES, where
            // the same call to a free name succeeds and Linux allows both.
            //
            // It beats ENOTEMPTY below on the same shape -- a non-empty `d` is
            // still EACCES -- which is what makes it a check in its own right
            // rather than a spelling of the displaced-directory one above.
            movedIsDirectory
            && (sourceDirectory <> destinationDirectory || displacedDirectory.IsSome)
            && RenameChecks.lacksWrite "the moved directory" privilege moved vfs
        then
            RenameVerdict.Refuse UnixError.EACCES
        else

        match displacedDirectory with
        | Some displaced when not (RemovalChecks.isEmptyDirectory displaced vfs) ->
            RenameVerdict.Refuse UnixError.ENOTEMPTY
        | Some _
        | None -> RenameVerdict.Move (sourceDirectory, sourceName, destinationDirectory, destinationName)

    /// Decide what a `rename(2)` owes, given how its two paths resolved.
    ///
    /// Two whole functions rather than one reading a rules record, for the
    /// reason `UnlinkRules.verdict` states: what diverges is the order of the
    /// checks and the errno vocabulary rather than a constant they both consult.
    /// `rename` makes the case more strongly than either removal did, because
    /// here the flavours do not even agree on *which object* a check is about —
    /// a directory displacing a directory consults the displaced directory's
    /// write bit on Darwin and its parent's on Linux, which is not a reordering
    /// of one check but two different checks.
    ///
    /// `source` and `destination` must both have been resolved under
    /// `SimulatedUnixPlatform.renameRules`' `TrailingSeparator` and
    /// `SymlinkPolicy.NoFollowFinal`, which is what makes the trailing-separator
    /// arms above mean what they say.
    let verdict
        (flavour : SimulatedUnixFlavour)
        (privilege : CallerPrivilege)
        (source : Resolution)
        (destination : Resolution)
        (vfs : VirtualFileSystem)
        : RenameVerdict
        =
        match flavour with
        | SimulatedUnixFlavour.Linux -> linuxVerdict privilege source destination vfs
        | SimulatedUnixFlavour.Darwin -> darwinVerdict privilege source destination vfs

/// A reason `bind(2)` refuses, as one of the checks it makes rather than as an
/// errno: which errno a fault becomes is fixed, but *which fault is reported*
/// when several hold at once is per-flavour. See
/// `SimulatedUnixPlatform.bindFaultOrder`.
/// What this platform's `bind(2)` makes of a declared `socketAddressLen`.
///
/// The two rejections are not interchangeable, and the difference is *when* they
/// happen rather than which errno they carry. Measured on both: a length past the
/// upper bound is rejected before the kernel copies anything, so it beats a
/// faulting pointer and beats the family check — an unmapped pointer at 129 is
/// EINVAL on Linux where at 8 it is EFAULT, and a wrong-family blob at 256 is
/// ENAMETOOLONG on Darwin where at 129 it is EAFNOSUPPORT. A length merely too
/// short takes its ordinary place in `bindFaultOrder`.
[<RequireQualifiedAccess>]
type BindLengthVerdict =
    /// A length this platform will parse an address out of.
    | Accepted
    /// Past the greatest length this platform will consider, and so refused
    /// before the address is copied or read at all. Linux answers `EINVAL` above
    /// `sizeof(struct sockaddr_storage)`; Darwin answers `ENAMETOOLONG` above its
    /// own, larger threshold.
    | RejectedBeforeCopy of error : UnixError
    /// `EINVAL`, from the `Length` position of this platform's fault order.
    | Invalid

[<RequireQualifiedAccess>]
type BindFault =
    /// The declared `socketAddressLen` is not one this platform accepts for the
    /// address family in the blob. Which errno that becomes is the
    /// `BindLengthVerdict` the length classifier gave — `EINVAL`, or
    /// `ENAMETOOLONG` past the greatest length the platform considers — but the
    /// *position* in the order is the same either way, which is why the verdict
    /// is not carried here.
    | Length
    /// The blob's address family is not the socket's. `EAFNOSUPPORT`.
    | Family
    /// No local interface holds the address. `EADDRNOTAVAIL`.
    | AddressNotLocal
    /// The port is below `privilegedPortCeiling` and the process is not root.
    /// `EACCES`.
    | PrivilegedPort
    /// This socket already has a local address. `EINVAL`.
    | AlreadyBound
    /// Another socket holds a conflicting address. `EADDRINUSE`.
    | AddressInUse

[<RequireQualifiedAccess>]
module SimulatedUnixPlatform =
    /// Loosest ceiling any Unix we model imposes on `utsname.release`:
    /// macOS's `_SYS_NAMELEN` is 256 (including the NUL), while Linux's
    /// `_UTSNAME_LENGTH` is only 65. Bounded by the looser of the two rather
    /// than per-flavour, because the limit is about what a *guest* can be
    /// handed rather than about which kernel wrote it, and an unbounded string
    /// could hand a guest a release no real `uname` could produce.
    [<Literal>]
    let private maxReleaseLength : int = 255

    let describe (error : SimulatedUnixReleaseError) : string =
        match error with
        | SimulatedUnixReleaseError.Empty ->
            "release string is empty, but every Unix `uname(2)` fills `utsname.release`"
        | SimulatedUnixReleaseError.TooLong (length, limit) ->
            $"release string is %d{length} characters, exceeding the %d{limit}-character limit any Unix `utsname.release` can hold"
        | SimulatedUnixReleaseError.NotPrintableAscii (index, character) ->
            $"release string contains non-printable-ASCII character U+%04X{int character} at index %d{index}; `utsname.release` is reported to the guest as single-byte characters, so only printable ASCII round-trips faithfully"

    /// A platform of the given flavour reporting `release` from `uname -r`.
    ///
    /// Validated here rather than when the release is read, which is what makes
    /// every accessor below total: a value of this type is a platform some Unix
    /// could actually be.
    let create
        (flavour : SimulatedUnixFlavour)
        (release : string)
        : Result<SimulatedUnixPlatform, SimulatedUnixReleaseError>
        =
        if System.String.IsNullOrEmpty release then
            Error SimulatedUnixReleaseError.Empty
        elif String.length release > maxReleaseLength then
            Error (SimulatedUnixReleaseError.TooLong (String.length release, maxReleaseLength))
        else

        match release |> Seq.tryFindIndex (fun c -> c < ' ' || c > '~') with
        | Some i -> Error (SimulatedUnixReleaseError.NotPrintableAscii (i, release.[i]))
        | None ->
            Ok
                {
                    Flavour = flavour
                    Release = release
                }

    let createOrFail (context : string) (flavour : SimulatedUnixFlavour) (release : string) : SimulatedUnixPlatform =
        match create flavour release with
        | Ok platform -> platform
        | Error error -> failwith $"%s{context}: %s{describe error}"

    /// 64-bit x86 Linux, at the exact kernel PawPrint's CI runs: the release
    /// this reports and the behaviour derived from it below therefore describe
    /// one real machine rather than a plausible composite. The default, and the
    /// flavour whose CoreLib actually routes `Environment.OSVersion` through
    /// `SystemNative_GetUnixRelease` at all (the macOS CoreLib goes via
    /// `Interop.libobjc.GetOperatingSystemVersion` instead).
    ///
    /// Naming a real kernel rather than a plausible one matters because facts
    /// derived from a platform are claims about a machine somebody could be
    /// running. Note the division of labour with `EmulatedKernel`: identity
    /// that a guest reads back, like this release, belongs here; a fact that
    /// varies between two machines running this very kernel, like
    /// `UserAddressLimit`, is configuration instead.
    let linuxX64 : SimulatedUnixPlatform =
        createOrFail "SimulatedUnixPlatform.linuxX64" SimulatedUnixFlavour.Linux "6.17.0-1022-azure"

    /// 64-bit ARM macOS. The release is the *Darwin* kernel's, so `24.6.0`
    /// (macOS 15.6) rather than `15.6.0`.
    let macOsArm64 : SimulatedUnixPlatform =
        createOrFail "SimulatedUnixPlatform.macOsArm64" SimulatedUnixFlavour.Darwin "24.6.0"

    /// Which Unix this platform is.
    let flavour (platform : SimulatedUnixPlatform) : SimulatedUnixFlavour = platform.Flavour

    /// The `utsname.release` string this platform reports, i.e. exactly what
    /// `uname -r` would print. Part of PawPrint's replay contract: changing a
    /// preset's value changes the `Environment.OSVersion` every recorded trace
    /// on that platform observes.
    let unixRelease (platform : SimulatedUnixPlatform) : string = platform.Release

    /// Re-check the invariant of a value that may not have come from `create`.
    /// See `FileName.assertValid`: the only value this can reject is
    /// `Unchecked.defaultof` / C# `default`, whose null release would otherwise
    /// be handed to a guest as its `uname -r`.
    let assertValid (context : string) (platform : SimulatedUnixPlatform) : SimulatedUnixPlatform =
        // A record is a reference type, so the forged value is `null` itself
        // rather than a record with a null field — and reading `Flavour` off it
        // would throw a `NullReferenceException` naming nothing useful.
        match box platform with
        | null ->
            failwith
                $"%s{context}: the platform is null, which it can only be if it came from `Unchecked.defaultof` or C# `default`; construct one with SimulatedUnixPlatform.create, or use the linuxX64 / macOsArm64 presets."
        | _ ->

        match create platform.Flavour platform.Release with
        | Ok _ -> platform
        | Error error ->
            failwith
                $"%s{context}: %s{describe error}. A SimulatedUnixPlatform that fails its own invariant can only have come from `Unchecked.defaultof` or C# `default`; construct one with SimulatedUnixPlatform.create instead."

    /// Whose `<errno.h>` numbering this platform reports, for the errors where
    /// the two Unixes disagree.
    ///
    /// This is the choice `UnixError.toRawErrno` refuses to make on its own, and
    /// it is what lets an `ELOOP` reach a guest at all: raw 40 is `ELOOP` on
    /// Linux but `EMSGSIZE` on Darwin, so the number is meaningless until
    /// something says which Unix is being impersonated. The flavour says.
    let rawErrnoNumbering (platform : SimulatedUnixPlatform) : RawErrnoNumbering =
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> RawErrnoNumbering.Linux
        | SimulatedUnixFlavour.Darwin -> RawErrnoNumbering.Darwin

    /// What this platform's `getcwd(3)` reports for a removed current directory.
    /// See `GetCwdOrphanAnswer`.
    let getCwdOrphanAnswer (platform : SimulatedUnixPlatform) : GetCwdOrphanAnswer =
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> GetCwdOrphanAnswer.AlwaysDetached
        | SimulatedUnixFlavour.Darwin -> GetCwdOrphanAnswer.ShortestPathFirst

    /// What this platform's PAL puts in `DirectoryEntry.NameLength`. See
    /// `DirectoryEntryNameLength`.
    let directoryEntryNameLength (platform : SimulatedUnixPlatform) : DirectoryEntryNameLength =
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> DirectoryEntryNameLength.WalkToTerminator
        | SimulatedUnixFlavour.Darwin -> DirectoryEntryNameLength.Reported

    /// Whether this platform's `stat` reports a creation time.
    ///
    /// A compile-time property of the native shim rather than of any file:
    /// `ConvertFileStatus` in `pal_io.c` sets `BirthTime` and the
    /// `HAS_BIRTHTIME` flag under `#if HAVE_STAT_BIRTHTIME` — true on macOS,
    /// false on Linux, where it hard-zeroes both with the comment "Linux path:
    /// until we use statx()". So the birth time is a real fact about the inode
    /// on both, and this governs only whether the guest is told it.
    let reportsBirthTime (platform : SimulatedUnixPlatform) : bool =
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> false
        | SimulatedUnixFlavour.Darwin -> true

    /// The permission bits this platform reports for a symbolic link, which no
    /// syscall can set and which the two Unixes disagree about.
    ///
    /// Measured rather than read: with `umask 022` macOS reports 0o755 for a
    /// fresh symlink, with `umask 077` it reports 0o700 and with `umask 000`
    /// 0o777 — it applies the creating process's umask, exactly as it does to a
    /// regular file. Linux reports 0o777 whatever the umask, which is why
    /// `InodePermissions` derives this rather than storing it: under a Linux
    /// simulation a stored value could only ever describe a filesystem no
    /// kernel produced.
    ///
    /// The Darwin answer here is the `umask 022` one, and stays a constant even
    /// though `EmulatedKernel.Umask` now exists: a symbolic link can only enter
    /// this filesystem through a *seed*, and a seed describes a tree some other
    /// process built, so this run's configured umask is not the one that applied
    /// to it. The day `SystemNative_SymLink` lets a guest create one, that link
    /// *is* created by this process and this must become a function of
    /// `Kernel.Umask` — that is the trigger, not the existence of the field.
    let symlinkPermissions (platform : SimulatedUnixPlatform) : PermissionBits =
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> PermissionBits.parseOrFail "SimulatedUnixPlatform.symlinkPermissions" 0o777
        | SimulatedUnixFlavour.Darwin ->
            PermissionBits.parseOrFail "SimulatedUnixPlatform.symlinkPermissions" (0o777 &&& ~~~0o022)

    /// Whether this platform clears a truncated file's set-user-ID and
    /// set-group-ID bits.
    ///
    /// The only thing about truncation the two Unixes disagree about — every
    /// other row measured (the errno order, which descriptors refuse, the
    /// zero-fill, the timestamps, and `O_TRUNC`'s extra write-permission
    /// requirement) is unanimous, which is why this is a lone value rather than a
    /// `CreatingOpenRules`-shaped record.
    ///
    /// Measured non-root on macOS 26.6 and Linux 6.18.5, for `ftruncate(2)`,
    /// `O_TRUNC` and a no-op `ftruncate` alike; `PermissionBits.afterTruncation`
    /// carries the table. Linux applies the same rule it applies to a write.
    /// **Darwin strips nothing at all**, and that is isolated rather than
    /// inferred: in one process, on one file, a one-byte `write` takes `04755` to
    /// `00755` there while `ftruncate` leaves it `04755`.
    let setIdBitsOnTruncation (platform : SimulatedUnixPlatform) : SetIdBitsOnTruncation =
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> SetIdBitsOnTruncation.Strip
        | SimulatedUnixFlavour.Darwin -> SetIdBitsOnTruncation.Preserve

    /// Whether this platform's content-changing `write(2)` clears `S_ISGID` on a
    /// file that is not group-executable.
    ///
    /// The only thing about a write's effect on the mode that the two Unixes
    /// disagree about: `S_ISUID` goes on both whatever the execute bits say, and
    /// the sticky bit is left alone by both. So this is a lone value rather than
    /// a `CreatingOpenRules`-shaped record, for the reason
    /// `setIdBitsOnTruncation` above gives.
    ///
    /// Measured non-root on macOS 26.6 and Linux 6.18.5, one byte written over
    /// the front of a four-byte file; `PermissionBits.afterContentChangingWrite`
    /// carries the table. Linux applies to a write the same rule it applies to a
    /// truncation, and **Darwin does not** — there a write strips `02644` to
    /// `00644` while an `ftruncate` on the same file leaves the whole mode alone,
    /// which is why the two rules are separate values rather than one.
    ///
    /// The file must be handed to a group the caller belongs to before `chmod`,
    /// or the kernel drops `S_ISGID` silently and the measurement reads as
    /// agreement.
    let setGroupIdOnWrite (platform : SimulatedUnixPlatform) : SetGroupIdOnWrite =
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> SetGroupIdOnWrite.StripWhenGroupExecutable
        | SimulatedUnixFlavour.Darwin -> SetGroupIdOnWrite.StripAlways

    /// How this platform's `open(2)` behaves when asked to create; see
    /// `CreatingOpenRules` for what each field means and how it was measured.
    let creatingOpenRules (platform : SimulatedUnixPlatform) : CreatingOpenRules =
        match flavour platform with
        | SimulatedUnixFlavour.Linux ->
            {
                TrailingSeparator = TrailingSeparatorPolicy.RefuseIsDirectory
                RefusesExistingDirectory = true
                RootNavigation = None
                ModeMask = PermissionBits.parseOrFail "SimulatedUnixPlatform.creatingOpenRules" 0o7777
            }
        | SimulatedUnixFlavour.Darwin ->
            {
                TrailingSeparator = TrailingSeparatorPolicy.Demand
                RefusesExistingDirectory = false
                RootNavigation = Some UnixError.EEXIST
                ModeMask = PermissionBits.parseOrFail "SimulatedUnixPlatform.creatingOpenRules" 0o0777
            }

    /// Everything this platform's `mkdir(2)` does differently. See `MkDirRules`
    /// for the measurements; note in particular that `ModeMask` is not
    /// `creatingOpenRules`' one on Linux.
    let mkDirRules (platform : SimulatedUnixPlatform) : MkDirRules =
        match flavour platform with
        | SimulatedUnixFlavour.Linux ->
            {
                TrailingSeparator = TrailingSeparatorPolicy.Ignore
                ModeMask = PermissionBits.parseOrFail "SimulatedUnixPlatform.mkDirRules" 0o1777
                InheritsSetGroupIdFromParent = true
            }
        | SimulatedUnixFlavour.Darwin ->
            {
                TrailingSeparator = TrailingSeparatorPolicy.Demand
                ModeMask = PermissionBits.parseOrFail "SimulatedUnixPlatform.mkDirRules" 0o0777
                InheritsSetGroupIdFromParent = false
            }

    /// Everything this platform's `unlink(2)` does differently. See
    /// `UnlinkRules`, whose one field this picks; the rest of the divergence is
    /// in `UnlinkRules.verdict`, which takes the flavour directly.
    let unlinkRules (platform : SimulatedUnixPlatform) : UnlinkRules =
        match flavour platform with
        | SimulatedUnixFlavour.Linux ->
            {
                TrailingSeparator = TrailingSeparatorPolicy.Ignore
            }
        | SimulatedUnixFlavour.Darwin ->
            {
                TrailingSeparator = TrailingSeparatorPolicy.Demand
            }

    /// Everything this platform's `rmdir(2)` does differently. See `RmDirRules`,
    /// whose two fields this picks; the ordering half of the divergence is in
    /// `RmDirRules.verdict`, which takes the flavour directly.
    let rmDirRules (platform : SimulatedUnixPlatform) : RmDirRules =
        match flavour platform with
        | SimulatedUnixFlavour.Linux ->
            {
                TrailingSeparator = TrailingSeparatorPolicy.Ignore
                RemovedDirectoryEffect = UnbindTargetEffect.LostALink
            }
        | SimulatedUnixFlavour.Darwin ->
            {
                TrailingSeparator = TrailingSeparatorPolicy.Demand
                RemovedDirectoryEffect = UnbindTargetEffect.Untouched
            }

    /// Everything this platform's `rename(2)` does differently. See
    /// `RenameRules`, whose one field this picks; the ordering half of the
    /// divergence — which is most of it — is in `RenameRules.verdict`, which
    /// takes the flavour directly.
    let renameRules (platform : SimulatedUnixPlatform) : RenameRules =
        match flavour platform with
        | SimulatedUnixFlavour.Linux ->
            {
                TrailingSeparator = TrailingSeparatorPolicy.Ignore
            }
        | SimulatedUnixFlavour.Darwin ->
            {
                TrailingSeparator = TrailingSeparatorPolicy.Demand
            }

    /// Whether this platform's kernel screens a read or write buffer before it
    /// performs the operation.
    ///
    /// Linux's `vfs_read`/`vfs_write` (fs/read_write.c) reject an out-of-range
    /// buffer with EFAULT between the descriptor's access-mode check and the
    /// file operation, so the fault beats EISDIR and fires for a zero-length
    /// request. macOS screens nothing up front, so a call that transfers no
    /// bytes never looks at the buffer: measured, `read(f, (void*)-1, 5)` on a
    /// descriptor at end-of-file is EFAULT on Linux and 0 on macOS.
    ///
    /// *Where* it screens is the machine's `UserAddressLimit`, not a property
    /// of the flavour: both architectures compare the range end against
    /// `TASK_SIZE_MAX` (`valid_user_address` against `USER_PTR_MAX` in
    /// arch/x86/include/asm/uaccess_64.h, and the
    /// `(u65)addr + (u65)size <= (u65)TASK_SIZE_MAX` that
    /// arch/arm64/include/asm/uaccess.h documents), and that value varies with
    /// paging depth and virtual-address width — measured, two GitHub runners in
    /// one CI run disagreed. `EmulatedKernel.userBufferCheck` combines the two.
    let screensUserBufferUpFront (platform : SimulatedUnixPlatform) : bool =
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> true
        | SimulatedUnixFlavour.Darwin -> false

    /// The bounds this platform's kernel puts on path resolution.
    ///
    /// The numbers are measured facts about real kernels, which is why they are
    /// derived from the flavour rather than configured: a host that could set
    /// them could describe a Unix that does not exist, and a guest would then
    /// see a `MAXSYMLINKS` no real system has. `TestVirtualFileSystemAgainstHost`
    /// pins the value for whichever flavour it is running on against that
    /// kernel's *measured* behaviour, so macOS locally and Linux in CI each
    /// check one column.
    /// `PATH_MAX` counts the NUL, so the usable lengths are one less: measured,
    /// an argument of 1023 bytes resolves on macOS and 1024 does not, and 4095
    /// and 4096 respectively on Linux.
    ///
    /// `NAME_MAX` is 255 on both — but *of different things*, which is why it
    /// carries its unit. See `NameLengthLimit`: `中`×255 is 765 bytes and 255
    /// UTF-16 units, and APFS resolves it where ext4 refuses it.
    let pathLimits (platform : SimulatedUnixPlatform) : PathLimits =
        match flavour platform with
        | SimulatedUnixFlavour.Linux ->
            PathLimits.create 40 4096 (NameLengthLimit.Utf8Bytes 255) SpliceLengthRecheck.NoRecheck
        | SimulatedUnixFlavour.Darwin ->
            PathLimits.create 32 1024 (NameLengthLimit.Utf16CodeUnits 255) SpliceLengthRecheck.Recheck

    /// `sizeof(struct sockaddr_storage)`: the size of the largest socket address
    /// any Unix we model can hand back, and so the buffer size CoreLib sizes
    /// every socket-address buffer by. Reported to the guest by
    /// `SystemNative_GetMaximumAddressSize`.
    ///
    /// A compile-time property of the native shim rather than of any socket, like
    /// `reportsBirthTime`. Unlike that one it takes no flavour: both families
    /// *define* the constant in their headers rather than computing it
    /// (`_SS_MAXSIZE` on Darwin, `_SS_SIZE` in glibc's generic `bits/sockaddr.h`)
    /// and derive the padding members from it, so the value is invariant of
    /// pointer width as well as agreed between the two — both descend from
    /// RFC 2553's sample definition. Measured 128 on macOS arm64 and on Linux
    /// alike, and re-pinned against a real platform on every test run by
    /// `sourcesPure/SystemNativeGetMaximumAddressSize.cs`. Make it a function of
    /// the flavour on the day one of them disagrees.
    ///
    /// Contrast `sockaddr_un`, which genuinely does differ (106 on Darwin, 110 on
    /// Linux). That is `SocketAddressSizes.UnixDomain` below, reported through a
    /// different entry point again; this binding is where the shared 128 is
    /// defined, and `socketAddressSizes` reads it rather than repeating it.
    let maximumSocketAddressSize : int = 128

    /// The sizes `SystemNative_GetSocketAddressSizes` reports. See
    /// `SocketAddressSizes` for where each number was measured.
    let socketAddressSizes (platform : SimulatedUnixPlatform) : SocketAddressSizes =
        {
            InterNetwork = 16
            InterNetworkV6 = 28
            UnixDomain =
                match flavour platform with
                | SimulatedUnixFlavour.Linux -> 110
                | SimulatedUnixFlavour.Darwin -> 106
            Storage = maximumSocketAddressSize
        }

    /// Where this platform keeps a socket address's family, and how wide it is.
    /// See `SockaddrFamilyField`, which is also where the reason every other
    /// field's offset is flavour-free is written down.
    /// The order `bind(2)` reports its faults in, which is **not** the same on
    /// the two flavours.
    ///
    /// Measured pairwise, by presenting each pair of faults together and seeing
    /// which errno came back. Linux checks the declared length before it reads
    /// the family, and defers "this socket is already bound" until after it has
    /// validated the address; Darwin reads the family first and rejects an
    /// already-bound socket before it looks at the address at all. So
    /// a rebind to a non-local address is `EADDRNOTAVAIL` on Linux and `EINVAL`
    /// on Darwin, and a short `sockaddr_in6` on an IPv4 socket is `EINVAL` on
    /// Linux and `EAFNOSUPPORT` on Darwin.
    ///
    /// Expressed as an order over faults rather than as nested branches so that
    /// the divergence is one list rather than two code paths, and so a test can
    /// assert the order directly.
    let bindFaultOrder (platform : SimulatedUnixPlatform) : BindFault list =
        match flavour platform with
        | SimulatedUnixFlavour.Linux ->
            [
                BindFault.Length
                BindFault.Family
                BindFault.AddressNotLocal
                BindFault.PrivilegedPort
                BindFault.AlreadyBound
                BindFault.AddressInUse
            ]
        | SimulatedUnixFlavour.Darwin ->
            [
                BindFault.Family
                BindFault.Length
                BindFault.AlreadyBound
                BindFault.AddressNotLocal
                BindFault.PrivilegedPort
                BindFault.AddressInUse
            ]

    /// The first fault in this platform's order that `faults` contains.
    let firstBindFault (platform : SimulatedUnixPlatform) (faults : Set<BindFault>) : BindFault option =
        bindFaultOrder platform |> List.tryFind (fun fault -> Set.contains fault faults)

    /// How long `bind(2)` insists a `struct sockaddr_in` argument is.
    ///
    /// Measured, and not the same shape on the two: Linux accepts any length from
    /// the family's own `sizeof` up to `sizeof(struct sockaddr_storage)` — 16
    /// through 128 inclusive for IPv4, with 129 the least rejected — while Darwin
    /// insists on exactly 16 and answers `EINVAL` for every value from 17 to 32.
    ///
    /// Invisible through the managed API, which always passes
    /// `SocketAddress.Size`; a hand-rolled `[DllImport]` sees it immediately.
    /// The greatest `socketAddressLen` Darwin's `bind(2)` will consider at all.
    /// Above it the answer is `ENAMETOOLONG` rather than `EINVAL`; measured, 255
    /// is `EINVAL` and 256 is `ENAMETOOLONG`. Linux has no such threshold.
    let maximumDarwinSocketAddressLength : int = 255

    let bindAddressLength (platform : SimulatedUnixPlatform) (exactSize : int) (declared : int) : BindLengthVerdict =
        match flavour platform with
        | SimulatedUnixFlavour.Linux ->
            if declared > maximumSocketAddressSize then
                BindLengthVerdict.RejectedBeforeCopy UnixError.EINVAL
            elif declared >= exactSize then
                BindLengthVerdict.Accepted
            else
                BindLengthVerdict.Invalid
        | SimulatedUnixFlavour.Darwin ->
            if declared > maximumDarwinSocketAddressLength then
                BindLengthVerdict.RejectedBeforeCopy UnixError.ENAMETOOLONG
            elif declared = exactSize then
                BindLengthVerdict.Accepted
            else
                BindLengthVerdict.Invalid

    /// May a socket bind to this address, given the addresses this machine holds?
    ///
    /// The wildcard always binds. Beyond that the flavours read the same list
    /// differently, which is measured rather than inferred: `127.9.9.9` binds on
    /// Linux and is `EADDRNOTAVAIL` on Darwin, because Linux treats every address
    /// inside a local prefix as assigned while Darwin assigns loopback exactly
    /// one address.
    ///
    /// Is this the all-ones broadcast address, or a multicast one
    /// (`224.0.0.0/4`)?
    ///
    /// **PawPrint refuses to bind either**, rather than answering. Measured, the
    /// rule is not one rule: Linux takes both on a stream socket, Darwin answers
    /// `EAFNOSUPPORT` there, and on Darwin the answer depends on the socket's
    /// *kind* besides — a datagram socket binds a multicast group where a stream
    /// socket does not. Modelling that is modelling multicast, which is group
    /// membership and an interface to receive on, and PawPrint has neither; a
    /// bind that succeeded here would become a lie the moment `recvfrom` landed.
    ///
    /// So this classifier exists to *refuse* precisely, at the point in
    /// `bindFaultOrder` where the address is judged — a fault the platform ranks
    /// earlier still wins, which is what keeps the refusal from swallowing
    /// answers PawPrint does know.
    let isBroadcastOrMulticast (address : uint32) : bool =
        address = System.UInt32.MaxValue || (address >>> 28) = 0xEu

    /// Broadcast and multicast are a further Linux-only allowance
    /// (`255.255.255.255` and `224.0.0.1` bind there and are `EAFNOSUPPORT` on
    /// Darwin). Neither is modelled: PawPrint has no interface to broadcast on,
    /// and the entry point refuses such an address rather than answering, so a
    /// guest that needs one gets a diagnosis instead of a wrong errno.
    let isBindableAddress
        (platform : SimulatedUnixPlatform)
        (localAddresses : uint32 list)
        (localRoutes : Ipv4Prefix list)
        (address : uint32)
        : bool
        =
        if address = InternetEndpoint.WildcardAddress then
            true
        elif List.contains address localAddresses then
            // An address this machine holds binds on either flavour.
            true
        else

        match flavour platform with
        // Linux additionally takes anything it has a *local route* to, which is
        // why `127.9.9.9` binds there. An interface's subnet is not such a route
        // — holding `192.168.1.10/24` does not make `192.168.1.11` bindable — so
        // this reads the route table rather than widening the assigned addresses.
        | SimulatedUnixFlavour.Linux -> localRoutes |> List.exists (Ipv4Prefix.contains address)
        | SimulatedUnixFlavour.Darwin -> false

    /// Whether `bind(2)` has something to say about the address itself, as
    /// opposed to about the length, the family, or another socket. Callers rank
    /// this against the other faults in `bindFaultOrder`, at
    /// `BindFault.AddressNotLocal`.
    ///
    /// That is `EADDRNOTAVAIL` in every case PawPrint answers. A broadcast or
    /// multicast address faults here too, and its caller refuses it outright
    /// rather than reporting an errno — which is why this is not simply
    /// `not isBindableAddress`. Such an address is not necessarily *unbindable*:
    /// Linux binds `224.0.0.1` on a stream socket quite happily. It is one
    /// PawPrint declines to answer for, and a host that listed it in
    /// `LocalAddresses`, or covered it with a `LocalRoutes` prefix, would
    /// otherwise silence the refusal and record a multicast binding that nothing
    /// downstream can honour.
    let bindAddressFaults
        (platform : SimulatedUnixPlatform)
        (localAddresses : uint32 list)
        (localRoutes : Ipv4Prefix list)
        (address : uint32)
        : bool
        =
        isBroadcastOrMulticast address
        || not (isBindableAddress platform localAddresses localRoutes address)

    /// Does a bind of `candidate` collide with the socket already bound at
    /// `existing`?
    ///
    /// Both flavours refuse two sockets the same port on overlapping addresses,
    /// and both relax that when `SO_REUSEADDR` is set — in opposite directions,
    /// which is the whole of the divergence here and is measured in both:
    ///
    /// * **Linux** relaxes only while nothing is listening. Two sockets that both
    ///   set the flag may share an address, exactly or through the wildcard,
    ///   until one of them calls `listen(2)`; after that the second bind is
    ///   `EADDRINUSE`.
    /// * **Darwin** relaxes only for addresses that differ. Two sockets that both
    ///   set the flag may hold the wildcard and a specific address on one port,
    ///   listening or not; the exact duplicate is `EADDRINUSE` either way.
    ///
    /// With the flag absent on either side — every UDP bind through the shim, and
    /// every `ProtocolType.Unspecified` one — the two agree and refuse.
    ///
    /// The same relation answers `listen(2)`, which is measured rather than
    /// assumed: on Linux two reuse-carrying sockets may share an endpoint until
    /// one listens, and the *second* `listen` is then EADDRINUSE — exactly what
    /// this says when the other socket is already listening. Darwin never refuses
    /// a listen, and never lets the pair coexist in the first place.
    let bindConflict
        (platform : SimulatedUnixPlatform)
        (existing : SocketBinding)
        (existingReuse : bool)
        (existingPhase : SocketPhase)
        (candidate : SocketBinding)
        (candidateReuse : bool)
        : bool
        =
        if existing.Endpoint.Port <> candidate.Endpoint.Port then
            false
        elif not (InternetEndpoint.addressesOverlap existing.Endpoint candidate.Endpoint) then
            false
        else

        let existingIsListening = SocketPhase.isListening existingPhase

        // An established socket's pcb is keyed by its full peer tuple, and a
        // replacement listener can bind over it: measured on both kernels
        // (accept a connection, close the listener, bind a reuse-carrying
        // replacement at the exact endpoint — OK; without the candidate's
        // reuse flag — EADDRINUSE).
        let existingIsEstablished =
            match existingPhase with
            | SocketPhase.Established _
            | SocketPhase.EstablishedPendingReport _ -> true
            | SocketPhase.Idle
            | SocketPhase.Listening _
            | SocketPhase.RefusedPendingDelivery
            | SocketPhase.Dead
            | SocketPhase.DatagramPeer _ -> false

        match flavour platform with
        // Linux relaxes only while nothing listens, and only when *both* sockets
        // carry the flag. That rule already answers the measured established
        // rows correctly: an established child carries its listener's flag, so
        // a reuse-carrying rebind over it passes and a flagless one conflicts.
        | SimulatedUnixFlavour.Linux -> not (existingReuse && candidateReuse) || existingIsListening
        // Darwin relaxes only for addresses that differ, and keys on the
        // *candidate's* flag alone — measured: a wildcard listener that
        // `listen(2)` bound implicitly carries no flag at all, and a later
        // reuse-carrying bind to a specific address on its port still succeeds.
        // The exact-duplicate refusal exempts established sockets (measured
        // above).
        | SimulatedUnixFlavour.Darwin ->
            (existing.Endpoint.Address = candidate.Endpoint.Address
             && not existingIsEstablished)
            || not candidateReuse

    /// Whether `listen(2)` on a socket that is *already bound* asks the port
    /// admission question again, so that a binding admitted earlier can still be
    /// refused a listen.
    ///
    /// The flavours differ, and not merely in strictness. Linux's
    /// `inet_csk_listen_start` calls `get_port` a second time, which is why two
    /// sockets carrying SO_REUSEADDR may share an endpoint right up until one of
    /// them listens; Darwin's `tcp_usr_listen` binds only when the socket has no
    /// port yet, so an already-bound listen consults nothing. Both measured.
    ///
    /// This is not a strictness knob that could be left on for safety. Darwin's
    /// bind rule is asymmetric in SO_REUSEADDR -- it keys on the *candidate's*
    /// flag alone -- so re-asking it at listen time asks with the roles swapped,
    /// and a pair admitted at bind time answers the other way. Re-checking there
    /// would invent an EADDRINUSE, not merely tighten one.
    let listenRescreensBinding (platform : SimulatedUnixPlatform) : bool =
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> true
        | SimulatedUnixFlavour.Darwin -> false

    let sockaddrFamilyField (platform : SimulatedUnixPlatform) : SockaddrFamilyField =
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> SockaddrFamilyField.TwoBytesAtOffsetZero
        | SimulatedUnixFlavour.Darwin -> SockaddrFamilyField.OneByteAtOffsetOne

    /// Whether this platform's sockets report IPv4 packet information on a
    /// dual-mode socket — an IPv6 socket receiving IPv4-mapped traffic. Reported
    /// to the guest by `SystemNative_PlatformSupportsDualModeIPv4PacketInfo`.
    ///
    /// A compile-time property of the native shim rather than of any socket, like
    /// `reportsBirthTime`: upstream the whole function body is
    /// `#if HAVE_SUPPORT_FOR_DUAL_MODE_IPV4_PACKET_INFO return 1 #else return 0`,
    /// and `configure.cmake` sets that define to 1 for every Linux target and
    /// leaves it 0 elsewhere. There is no probe of the running kernel involved, so
    /// this is not a fact about the machine but about which shim was built.
    ///
    /// (Linux includes Android here: the `NOT CLR_CMAKE_TARGET_ANDROID` test
    /// nested inside that `if` scopes only a `CMAKE_REQUIRED_LIBRARIES` setting,
    /// not the define.)
    ///
    /// Follows the flavour rather than conservatively reporting `false`
    /// everywhere, because both of CoreLib's readers of it are guest-visible
    /// control flow (see the handler arm for which): answering `false` while
    /// impersonating Linux makes a guest see a `PlatformNotSupportedException`
    /// real Linux does not raise, and does so silently, with no abort and no
    /// diagnostic.
    ///
    /// Answering `true` carries an obligation for whoever implements the socket
    /// emulation this leads on to: a Linux-flavour `recvmsg` on a dual-mode
    /// socket must actually produce the IPv4 `pktinfo` control message, because
    /// CoreLib latches this once per process and will thereafter ask for the
    /// packet information and expect to be given it. Reporting support and then
    /// handing back a default `IPPacketInformation` would be the data-level
    /// version of the lie this function exists to avoid.
    let supportsDualModeIPv4PacketInfo (platform : SimulatedUnixPlatform) : bool =
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> true
        | SimulatedUnixFlavour.Darwin -> false

    /// The stride of the event buffer `SystemNative_CreateSocketEventBuffer`
    /// allocates and `SystemNative_WaitForSocketEvents` fills, in bytes.
    ///
    /// A compile-time property of the native shim, like `reportsBirthTime`:
    /// `pal_networking.c` defines `SocketEventBufferElementSize` once per backend,
    /// as `max(sizeof(struct epoll_event), sizeof(SocketEvent))` under epoll and
    /// `sizeof(struct kevent)` under kqueue.
    ///
    /// Note what the epoll `max` does, because it is the reason this is a total
    /// function of the flavour where `LinuxEpollLimits.EventSize` is not.
    /// `sizeof(struct epoll_event)` is architecture-dependent — 12 on x86-64 under
    /// `EPOLL_PACKED`, 16 everywhere else — and the `max` against the 16-byte
    /// `SocketEvent` erases exactly that difference, since `max(12, 16)` and
    /// `max(16, 16)` are both 16. So the buffer stride follows the flavour alone,
    /// while the `epoll_wait` constants that skip the `max` do not.
    ///
    /// `sizeof(struct kevent)` is 32 on every 64-bit Darwin:
    /// `{ uintptr_t ident; int16_t filter; uint16_t flags; uint32_t fflags;
    /// intptr_t data; void* udata; }`, measured rather than recalled.
    let socketEventBufferElementSize (platform : SimulatedUnixPlatform) : int =
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> 16
        | SimulatedUnixFlavour.Darwin -> 32

    /// What `fcntl(F_SETFL)` answers on a socket event port — `None` for
    /// success — the `O_NONBLOCK` bit having changed *either way*.
    ///
    /// Measured, not derived: on Linux 6.18.5 the call succeeds and the flag
    /// round-trips; on Darwin (through the real shim's
    /// `SystemNative_FcntlSetIsNonBlocking`, macOS 26) it returns -1 with
    /// ENOTTY and a subsequent `F_GETFL` nevertheless reports the toggled bit,
    /// in both directions. So the caller must store the flag first and then
    /// report this answer.
    ///
    /// The stored bit changes no modelled wait: both `epoll_wait` and `kevent`
    /// take their blocking behaviour from their own timeout argument rather
    /// than from the descriptor's status flags, so
    /// `SystemNative_WaitForSocketEvents` rightly never consults it.
    let eventPortSetStatusFlagsError (platform : SimulatedUnixPlatform) : UnixError option =
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> None
        | SimulatedUnixFlavour.Darwin -> Some UnixError.ENOTTY

    /// The PAL numbering `SystemNative_Socket`'s three arguments arrive in
    /// (`AddressFamily`, `SocketType` and `ProtocolType` in `pal_networking.h`).
    /// Platform-independent by construction: upstream chose values that do not
    /// coincide with any kernel's, precisely so the shim has to translate.
    [<RequireQualifiedAccess>]
    module private Pal =
        [<Literal>]
        let AfUnspec = 0

        [<Literal>]
        let AfUnix = 1

        [<Literal>]
        let AfInet = 2

        [<Literal>]
        let AfInet6 = 23

        [<Literal>]
        let AfPacket = 65536

        [<Literal>]
        let AfCan = 65537

        [<Literal>]
        let SockStream = 1

        [<Literal>]
        let SockDgram = 2

        [<Literal>]
        let SockRaw = 3

        [<Literal>]
        let SockRdm = 4

        [<Literal>]
        let SockSeqPacket = 5

        [<Literal>]
        let PtUnspecified = 0

        [<Literal>]
        let PtIcmp = 1

        [<Literal>]
        let PtIgmp = 2

        [<Literal>]
        let PtTcp = 6

        [<Literal>]
        let PtUdp = 17

        [<Literal>]
        let PtRouting = 43

        [<Literal>]
        let PtFragment = 44

        [<Literal>]
        let PtIcmpV6 = 58

        [<Literal>]
        let PtNone = 59

        [<Literal>]
        let PtDstOpts = 60

        [<Literal>]
        let PtRaw = 255

    /// `AF_INET`, in the platform's own numbering. 2 on both, and on essentially
    /// every Unix — it is one of the handful of `AF_*` values that predate the
    /// BSD/Linux split and never moved.
    ///
    /// Exposed alongside `internetV6AddressFamily` because the `sockaddr`
    /// accessors switch on the raw `sa_family` in the blob rather than on a
    /// converted value: `SystemNative_GetPort` is a `switch (sockAddr->sa_family)`
    /// over exactly these two, and `SystemNative_GetIPv4Address` is an equality
    /// against the first.
    let internetAddressFamily : int = 2

    /// `AF_INET6`, in the platform's own numbering, which unlike `AF_INET` the two
    /// families disagree about: 10 on Linux against 30 on Darwin. Measured.
    let internetV6AddressFamily (platform : SimulatedUnixPlatform) : int =
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> 10
        | SimulatedUnixFlavour.Darwin -> 30

    /// `TryConvertAddressFamilyPalToPlatform` (`pal_networking.c:218`): the
    /// platform `AF_*` this PAL address family names, or `None` where the shim's
    /// switch has no case for it.
    ///
    /// `None` is not the same as "refuse". Upstream the failing branch still
    /// stores `(sa_family_t) palAddressFamily` — truncated to
    /// `SockaddrFamilyField.width` — through the out-parameter before returning
    /// false, so a caller that writes the family into a blob writes a truncated
    /// value there *and* reports `EAFNOSUPPORT`. Callers must reproduce both
    /// halves; see the `SystemNative_SetAddressFamily` handler.
    ///
    /// `AF_PACKET` and `AF_CAN` are the only flavour-dependent arms, and their
    /// dependence is the shim's `#ifdef`s rather than any kernel's: Linux's
    /// headers define the symbols (17 and 29, measured) and Darwin's do not, so
    /// on Darwin those two arms are not compiled and the value falls to the
    /// default.
    let addressFamilyPalToPlatform (platform : SimulatedUnixPlatform) (palAddressFamily : int) : int option =
        let isLinux =
            match flavour platform with
            | SimulatedUnixFlavour.Linux -> true
            | SimulatedUnixFlavour.Darwin -> false

        match palAddressFamily with
        | Pal.AfUnspec -> Some 0
        | Pal.AfUnix -> Some 1
        | Pal.AfInet -> Some internetAddressFamily
        | Pal.AfInet6 -> Some (internetV6AddressFamily platform)
        | Pal.AfPacket -> if isLinux then Some 17 else None
        | Pal.AfCan -> if isLinux then Some 29 else None
        | _ -> None

    /// `TryConvertAddressFamilyPlatformToPal` (`pal_networking.c:184`), the
    /// inverse of `addressFamilyPalToPlatform` over exactly the same rows.
    ///
    /// `None` where the switch has no case. Upstream's failing branch copies the
    /// platform number through unconverted, but `SystemNative_GetAddressFamily`
    /// — its only caller that a guest can reach — overwrites that with
    /// `AddressFamily_AF_UNKNOWN` and still reports success, so the unconverted
    /// value never escapes and this returns no analogue of it.
    let addressFamilyPlatformToPal (platform : SimulatedUnixPlatform) (platformAddressFamily : int) : int option =
        let isLinux =
            match flavour platform with
            | SimulatedUnixFlavour.Linux -> true
            | SimulatedUnixFlavour.Darwin -> false

        match platformAddressFamily with
        | 0 -> Some Pal.AfUnspec
        | 1 -> Some Pal.AfUnix
        | family when family = internetAddressFamily -> Some Pal.AfInet
        | family when family = internetV6AddressFamily platform -> Some Pal.AfInet6
        | 17 -> if isLinux then Some Pal.AfPacket else None
        | 29 -> if isLinux then Some Pal.AfCan else None
        | _ -> None

    /// What `SystemNative_Socket` does with a domain, type and protocol, all in
    /// the PAL numbering its caller supplies them in.
    ///
    /// Three of the four answers are the native shim's own screens, transcribed
    /// from `TryConvertAddressFamilyPalToPlatform`,
    /// `TryConvertSocketTypePalToPlatform` and
    /// `TryConvertProtocolTypePalToPlatform` (`pal_networking.c:218`, `:2497`,
    /// `:2535`) and applied in the order `SystemNative_Socket` applies them. They
    /// are pure C running before any syscall, so they are exactly knowable, and
    /// their flavour-dependence is the shim's `#ifdef`s rather than any kernel's
    /// behaviour.
    ///
    /// The fourth, `Unmodelled`, stands where the kernel's answer would be. The
    /// combinations that do *not* get one are refused rather than reported for
    /// three different reasons — some are privilege-dependent (every raw and
    /// packet socket: measured, 70 Linux rows change answer between euid 1000
    /// and euid 0), some sysctl-dependent (Linux's ping sockets, gated by
    /// `net.ipv4.ping_group_range`), and some deterministic but simply not
    /// modelled (`AF_INET`/`SOCK_STREAM`/`PT_UDP` and friends). The set below is
    /// this emulated kernel's declared protocol table; a row outside it is a
    /// socket PawPrint has not decided how to be, and a refusal leaves that
    /// decision open where a guessed errno would not.
    /// Is this the PAL protocol type `SystemNative_Bind` sets `SO_REUSEADDR`
    /// for? The C keys on its own `protocolType` *argument* being `PT_TCP`
    /// (`pal_networking.c:1770`), not on the socket's protocol, so this asks
    /// about the argument.
    let isTcpProtocolType (palProtocolType : int) : bool = palProtocolType = Pal.PtTcp

    let socketCreation
        (platform : SimulatedUnixPlatform)
        (palAddressFamily : int)
        (palSocketType : int)
        (palProtocolType : int)
        : Result<SocketDomain * SocketKind * SocketProtocol, SocketCreationRefusal>
        =
        let isLinux =
            match flavour platform with
            | SimulatedUnixFlavour.Linux -> true
            | SimulatedUnixFlavour.Darwin -> false

        // `TryConvertAddressFamilyPalToPlatform`, which is
        // `addressFamilyPalToPlatform` above — the same C function screens
        // `SystemNative_Socket`'s first argument and converts
        // `SystemNative_SetAddressFamily`'s, so there is one rule here, not two.
        // Only whether it converts matters to this caller; the number it converts
        // to is a socket address's business.
        let familyConverts = (addressFamilyPalToPlatform platform palAddressFamily).IsSome

        if not familyConverts then
            Error SocketCreationRefusal.AddressFamily
        else

        // `TryConvertSocketTypePalToPlatform`. Every arm is `#ifdef`-guarded on a
        // `SOCK_*` symbol, but both flavours define all five, so this screen
        // takes no flavour and fires only for a value outside the enum.
        let typeConverts =
            match palSocketType with
            | Pal.SockStream
            | Pal.SockDgram
            | Pal.SockRaw
            | Pal.SockRdm
            | Pal.SockSeqPacket -> true
            | _ -> false

        if not typeConverts then
            Error SocketCreationRefusal.SocketType
        else

        // `TryConvertProtocolTypePalToPlatform`, whose table is per address
        // family. Only the *converts or not* answer matters here: the platform
        // protocol number it produces can differ from the PAL one it was given
        // (`AF_INET6` with `PT_ICMP` becomes `IPPROTO_ICMPV6`), and it is the PAL
        // value that is worth keeping.
        let protocolConverts =
            match palAddressFamily with
            // The `AF_PACKET` arm passes the number straight through as an IEEE
            // 802.3 protocol in network order, so every value converts.
            | Pal.AfPacket -> true
            // `#if HAVE_LINUX_CAN_H` — a `check_include_files` probe of the
            // *shim's* build host (`configure.cmake:970`) rather than of any
            // kernel. PawPrint models the header as present, which is what an
            // official linux-x64 build has. Were it absent, this arm would
            // vanish and every `AF_CAN` protocol would be refused below.
            | Pal.AfCan ->
                match palProtocolType with
                | Pal.PtUnspecified
                | Pal.PtRaw -> true
                | _ -> false
            | Pal.AfInet ->
                match palProtocolType with
                | Pal.PtUnspecified
                | Pal.PtIcmp
                | Pal.PtTcp
                | Pal.PtUdp
                | Pal.PtIgmp
                | Pal.PtRaw -> true
                | _ -> false
            | Pal.AfInet6 ->
                match palProtocolType with
                | Pal.PtUnspecified
                | Pal.PtIcmpV6
                | Pal.PtIcmp
                | Pal.PtTcp
                | Pal.PtUdp
                | Pal.PtIgmp
                | Pal.PtRaw
                | Pal.PtDstOpts
                | Pal.PtNone
                | Pal.PtRouting
                | Pal.PtFragment -> true
                | _ -> false
            // `AF_UNSPEC` and `AF_UNIX` share the C's `default` arm, which
            // accepts the unspecified protocol and nothing else.
            | _ ->
                match palProtocolType with
                | Pal.PtUnspecified -> true
                | _ -> false

        if not protocolConverts then
            Error SocketCreationRefusal.Protocol
        else

        // Past every screen the shim applies, so a real run would now call
        // `socket(2)`. These are the rows measured to succeed unprivileged, on
        // Linux 6.18.5 and Darwin 25.6.0 respectively.
        //
        // The protocol conjunct on the `AF_UNIX` rows is not falsifiable: the
        // conversion above already refused every protocol but `PT_UNSPECIFIED`
        // for that family. It is written out because the alternative — a
        // wildcard — would read as a claim that any protocol is accepted, which
        // is a claim about a *different* screen and would silently become true
        // if that screen ever changed.
        match palAddressFamily, palSocketType, palProtocolType with
        | Pal.AfInet, Pal.SockStream, Pal.PtUnspecified ->
            Ok (SocketDomain.InterNetwork, SocketKind.Stream, SocketProtocol.Unspecified)
        | Pal.AfInet, Pal.SockStream, Pal.PtTcp -> Ok (SocketDomain.InterNetwork, SocketKind.Stream, SocketProtocol.Tcp)
        | Pal.AfInet, Pal.SockDgram, Pal.PtUnspecified ->
            Ok (SocketDomain.InterNetwork, SocketKind.Datagram, SocketProtocol.Unspecified)
        | Pal.AfInet, Pal.SockDgram, Pal.PtUdp ->
            Ok (SocketDomain.InterNetwork, SocketKind.Datagram, SocketProtocol.Udp)
        | Pal.AfInet6, Pal.SockStream, Pal.PtUnspecified ->
            Ok (SocketDomain.InterNetworkV6, SocketKind.Stream, SocketProtocol.Unspecified)
        | Pal.AfInet6, Pal.SockStream, Pal.PtTcp ->
            Ok (SocketDomain.InterNetworkV6, SocketKind.Stream, SocketProtocol.Tcp)
        | Pal.AfInet6, Pal.SockDgram, Pal.PtUnspecified ->
            Ok (SocketDomain.InterNetworkV6, SocketKind.Datagram, SocketProtocol.Unspecified)
        | Pal.AfInet6, Pal.SockDgram, Pal.PtUdp ->
            Ok (SocketDomain.InterNetworkV6, SocketKind.Datagram, SocketProtocol.Udp)
        | Pal.AfUnix, Pal.SockStream, Pal.PtUnspecified ->
            Ok (SocketDomain.Unix, SocketKind.Stream, SocketProtocol.Unspecified)
        | Pal.AfUnix, Pal.SockDgram, Pal.PtUnspecified ->
            Ok (SocketDomain.Unix, SocketKind.Datagram, SocketProtocol.Unspecified)
        // Linux-only, and measured rather than reasoned: Darwin refuses both
        // with EPROTONOSUPPORT from the kernel, not from a shim screen.
        | Pal.AfUnix, Pal.SockRaw, Pal.PtUnspecified when isLinux ->
            Ok (SocketDomain.Unix, SocketKind.Raw, SocketProtocol.Unspecified)
        | Pal.AfUnix, Pal.SockSeqPacket, Pal.PtUnspecified when isLinux ->
            Ok (SocketDomain.Unix, SocketKind.SeqPacket, SocketProtocol.Unspecified)
        | _, _, _ -> Error SocketCreationRefusal.Unmodelled

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

type EmulatedKernel =
    {
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
        /// Every directory stream `SystemNative_OpenDir` has handed out and
        /// `SystemNative_CloseDir` has not yet reclaimed, keyed by the native
        /// block whose address the guest holds as its `DIR*`.
        ///
        /// A stream is *not* a descriptor kind. Measured on both kernels,
        /// `opendir` consumes a file descriptor — an `open` either side of one
        /// returned fds 3 and 5 — so the descriptor is an ordinary
        /// `OpenFileTarget.File` on the directory, which is what pins the inode
        /// through `heldInodes` and so makes a stream over an `rmdir`'d
        /// directory behave. What cannot live there is the rest: the cursor and
        /// the name buffer have no home in `File (inode, offset)`.
        ///
        /// An absent key is not a default and must never be read as one: it
        /// means the guest passed a `DIR*` this kernel never issued, or one it
        /// has already closed. `directoryStream` says so loudly rather than
        /// inventing an errno, the way `EmulatedKernel.connection` does for a
        /// `ConnectionId`.
        DirectoryStreams : Map<NativeMemoryBlockId, DirectoryStream>
        /// The filesystem `FileSystem` claims to be, which is the whole of what
        /// `SystemNative_GetFileSystemType` reports for a file on it.
        ///
        /// Seeded from `KernelConfig.FileSystemType` and fixed for the run: no
        /// syscall in CoreLib's interop surface can mount anything, so nothing
        /// a guest does can change it. Set only by
        /// `withUnixPlatformAndFileSystemType`, which writes it and
        /// `UnixPlatform` together so that the two cannot disagree.
        FileSystemType : EmulatedFileSystemType
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
        Signals : SignalState
    }

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
    | DanglingDirectoryStreamInode of block : NativeMemoryBlockId * inode : InodeNumber
    /// An open directory stream names an inode that is not a directory.
    | DirectoryStreamIsNotADirectory of block : NativeMemoryBlockId * inode : InodeNumber
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
            Sockets = Map.empty
            Connections = Map.empty
            NextConnectionId = ConnectionId 0L
            NextSocketEventRegistrationOrdinal = 0L
            ParkedSocketWaits = Map.empty
            NextSocketId = SocketId 0L
            NextEphemeralPort = fst defaultEphemeralPortRange
            EphemeralPortRange = defaultEphemeralPortRange
            // The Linux default, matching `defaultUnixPlatform`;
            // `KernelConfig.applyTo` re-resolves it beside the platform.
            SoMaxConn = defaultSoMaxConn SimulatedUnixFlavour.Linux
            LocalAddresses = defaultLocalAddresses
            LocalRoutes = defaultLocalRoutes
            LowLevelMonitors = Map.empty
            NextLowLevelMonitorId = 1
            WaitHandles = Map.empty
            NextWaitHandleId = 1
            NextEventPipeId = 1L
            SpuriousWakeup = SpuriousWakeupStrategy.Disabled
            SyncBlockSpuriousWakeup = SyncBlockSpuriousWakeupStrategy.Disabled
            ClockJitter = ClockJitterStrategy.Disabled
            StepCounter = 0L
            VirtualClockTicks = 0L
            WallClockEpochMs = 0L
            NonCryptoRandomState = NonCryptoRandom.initialState
            CryptoRandomState = cryptoRandomInitialState
            OutputLog = ImmutableArray<OutputLogEntry>.Empty
            Environment = defaultEnvironment
            ProcessorCount = defaultProcessorCount
            UserAddressLimit = defaultUserAddressLimit
            OptimalMaxSpinWaitsPerSpinIteration = defaultOptimalMaxSpinWaitsPerSpinIteration
            UnixPlatform = defaultUnixPlatform
            CurrentDirectory = defaultCurrentDirectory
            // The default current directory is the root, which every filesystem
            // has and no operation can remove, so the pair starts consistent
            // whatever else a host goes on to set.
            CurrentDirectoryInode = VirtualFileSystem.root filesystem
            ProcessPath = defaultProcessPath
            FileSystem = filesystem
            FileSystemType = EmulatedFileSystemType.defaultFor (SimulatedUnixPlatform.flavour defaultUnixPlatform)
            UserId = defaultUserId
            GroupId = defaultGroupId
            Umask = defaultUmask
            Signals = SignalState.empty
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
            UnixPlatform = platform
            FileSystemType = resolved
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
            FileSystem = filesystem
            CurrentDirectory = physical
            CurrentDirectoryInode = inode
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
            UserAddressLimit = limit
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
            ProcessorCount = count
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
            WallClockEpochMs = epochMs
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
            VirtualClockTicks = ticks
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
            VirtualClockTicks = ticks
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
            SoMaxConn = resolved
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
            EphemeralPortRange = low, high
            NextEphemeralPort = low
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
            LocalAddresses = addresses
            LocalRoutes = routes
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
                        NextEphemeralPort = next
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
                NextSocketEventRegistrationOrdinal =
                    match change with
                    | SocketEventRegistrationChange.Add _ -> ordinal + 1L
                    | SocketEventRegistrationChange.Modify _
                    | SocketEventRegistrationChange.Remove -> ordinal
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
                FileSystem = VirtualFileSystem.forget inode kernel.FileSystem
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
                    Sockets = sockets
                    Connections = connections
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

    /// The stream `block` names.
    ///
    /// Total, and loudly partial rather than an option: every `DIR*` a guest can
    /// legally hold came out of `SystemNative_OpenDir` and has not been closed,
    /// and passing anything else to `readdir`/`closedir` is undefined behaviour
    /// on a real libc rather than an error it reports. Inventing EBADF here
    /// would answer a question no kernel answers.
    let directoryStream (block : NativeMemoryBlockId) (kernel : EmulatedKernel) : DirectoryStream =
        match Map.tryFind block kernel.DirectoryStreams with
        | Some stream -> stream
        | None ->
            failwith
                $"EmulatedKernel.directoryStream: %O{block} names no open directory stream. The guest passed a DIR* this kernel never handed out, or one it has already closed — both are undefined behaviour on a real libc, which is why there is no errno to report."

    /// Record a newly-opened stream against the block whose address is its
    /// `DIR*`.
    let withDirectoryStream
        (block : NativeMemoryBlockId)
        (stream : DirectoryStream)
        (kernel : EmulatedKernel)
        : EmulatedKernel
        =
        { kernel with
            DirectoryStreams = Map.add block stream kernel.DirectoryStreams
        }

    /// Move a stream's cursor on, leaving everything else about it alone.
    let withDirectoryCursor
        (block : NativeMemoryBlockId)
        (cursor : DirectoryCursor)
        (kernel : EmulatedKernel)
        : EmulatedKernel
        =
        let stream = directoryStream block kernel

        withDirectoryStream
            block
            { stream with
                Cursor = cursor
            }
            kernel

    /// Forget a stream, which `SystemNative_CloseDir` does before closing the
    /// descriptor under it — that order matters, because the close is what
    /// reaps an orphaned directory and this entry is one of the things holding
    /// it.
    let withoutDirectoryStream (block : NativeMemoryBlockId) (kernel : EmulatedKernel) : EmulatedKernel =
        { kernel with
            DirectoryStreams = Map.remove block kernel.DirectoryStreams
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
                Sockets =
                    Map.add
                        socketId
                        { sock with
                            Phase = phase
                        }
                        kernel.Sockets
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
                            Sockets =
                                Map.add
                                    socketId
                                    { sock with
                                        Binding = Some (bindingAfterRefusalDelivery flavour binding)
                                        Phase = phase
                                    }
                                    kernel.Sockets
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
                            Sockets =
                                Map.add
                                    socketId
                                    { sock with
                                        Binding = Some binding
                                        Phase = SocketPhase.RefusedPendingDelivery
                                    }
                                    kernel.Sockets
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
                            Sockets =
                                Map.add
                                    socketId
                                    { sock with
                                        Binding =
                                            sock.Binding
                                            |> Option.map (bindingAfterRefusalDelivery SimulatedUnixFlavour.Linux)
                                        Phase = SocketPhase.Idle
                                    }
                                    kernel.Sockets
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
                            Sockets =
                                Map.add
                                    socketId
                                    { sock with
                                        Binding = binding
                                        Phase = SocketPhase.Idle
                                    }
                                    kernel.Sockets
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
                    Sockets =
                        Map.add
                            socketId
                            { sock with
                                Binding = Some binding
                                Phase = SocketPhase.DatagramPeer dest
                            }
                            kernel.Sockets
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
            kernel.FileDescriptors.Descriptions
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
            |> List.choose (fun (block, stream) ->
                match VirtualFileSystem.tryGetContent stream.Inode kernel.FileSystem with
                | Some (InodeContent.Directory _) -> None
                | Some (InodeContent.RegularFile _)
                | Some (InodeContent.Symlink _) ->
                    Some (EmulatedKernelDefect.DirectoryStreamIsNotADirectory (block, stream.Inode))
                | None -> Some (EmulatedKernelDefect.DanglingDirectoryStreamInode (block, stream.Inode))
            )

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

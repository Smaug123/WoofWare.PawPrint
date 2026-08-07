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
///     load-bearing for `LowLevelLock` fairness; switching to LIFO or
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
/// Kept structurally parallel-but-separate from `SpuriousWakeupStrategy` so
/// a guest's LowLevelMonitor-level fuzz schedule and its SyncBlock-level
/// schedule are independent dials: a single strategy covering both would
/// either be hard to script (per-tick wakeups tagged by which queue family
/// they target) or coarsely tied together (any waiter is fair game on a
/// given tick), neither of which the user wants when they're trying to
/// reproduce a specific managed-Monitor bug without disturbing the
/// LowLevelMonitor schedule.
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
///     `BlockedOnWaitHandle`. The head is woken first by a subsequent
///     `Release`; FIFO order is load-bearing for the higher-level
///     `LowLevelLifoSemaphore` fairness contract.
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
/// plus the FIFO wait queue of threads parked in `BlockedOnWaitHandle`
/// because the mutex was held by another thread when they called
/// `WaitOne`. The wait queue lives outside the ownership DU because it
/// is orthogonal to who currently owns the mutex — a free mutex can
/// have a non-empty queue (transient, between direct-handoff release
/// and the woken thread being picked by the scheduler) although our
/// release path immediately re-installs the woken thread as the new
/// owner so this is in practice always empty when `Free`.
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
/// FIFO list of threads parked in `BlockedOnWaitHandle` because the event
/// was unsignalled when they called `WaitOne`.
///
/// Invariant: `Signaled = true ⇒ WaitQueue = []`. The operations enforce
/// it: `setEvent` on a `Manual` event with parked waiters wakes them all
/// and sets `Signaled = true` (leaving the queue empty); `setEvent` on an
/// `Auto` event either wakes the FIFO head (leaving `Signaled = false`) or
/// — if no waiters — sets `Signaled = true`. `waitOne` on a signalled
/// `Auto` event consumes the signal as part of acquiring, so a thread can
/// never observe `Signaled = true` while there is a parked waiter.
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
/// Modelled as a closed set of platform identities rather than as a bag of
/// loose `utsname` strings so that the facts we report stay mutually
/// consistent as more of `utsname` gets implemented: a future
/// `SystemNative_GetUnixVersion` or `SystemNative_GetOSArchitecture` is a new
/// total *function* over this DU, not a new independently-settable string
/// that could claim a Darwin release alongside an x86_64 machine.
[<RequireQualifiedAccess>]
type SimulatedUnixPlatform =
    /// 64-bit x86 Linux, kernel release shaped like Ubuntu 24.04 LTS. The
    /// default: it is the platform PawPrint's CI runs on, and the one whose
    /// CoreLib actually routes `Environment.OSVersion` through
    /// `SystemNative_GetUnixRelease` at all (the macOS CoreLib goes via
    /// `Interop.libobjc.GetOperatingSystemVersion` instead).
    | LinuxX64
    /// 64-bit ARM macOS. Note that `uname -r` on macOS reports the *Darwin*
    /// kernel release, not the macOS product version — so this is `24.6.0`
    /// (macOS 15.6), not `15.6.0`.
    | MacOsArm64
    /// Explicit `utsname.release`, for guests that need a specific kernel
    /// version string (e.g. to exercise a version-sniffing code path, or the
    /// integer-overflow branch in CoreLib's
    /// `Environment.FindAndParseNextNumber`). Validated by
    /// `SimulatedUnixPlatform.unixRelease`; see there for what a legal
    /// release string is.
    | Custom of release : string

[<RequireQualifiedAccess>]
module SimulatedUnixPlatform =
    /// Loosest ceiling any Unix we model imposes on `utsname.release`:
    /// macOS's `_SYS_NAMELEN` is 256 (including the NUL), while Linux's
    /// `_UTSNAME_LENGTH` is only 65. We bound `Custom` by the looser of the
    /// two because the case deliberately does not say which platform it is
    /// impersonating, so neither limit is uniquely correct — but an
    /// unbounded string could hand a guest a release no real `uname` could
    /// ever produce.
    [<Literal>]
    let private maxReleaseLength : int = 255

    /// The `utsname.release` string this platform reports, i.e. exactly what
    /// `uname -r` would print. Part of PawPrint's replay contract: changing a
    /// preset's value changes the `Environment.OSVersion` every recorded
    /// trace on that platform observes.
    ///
    /// Rejects a `Custom` payload that no real `uname` could produce: empty
    /// (every Unix fills `release`), longer than `maxReleaseLength`, or
    /// containing a byte outside printable ASCII. The last is load-bearing
    /// rather than fussy — the value is handed to the guest as a C string of
    /// single bytes, so a non-ASCII character has no faithful encoding here,
    /// and an embedded NUL would silently truncate the string the guest sees.
    let unixRelease (platform : SimulatedUnixPlatform) : string =
        match platform with
        | SimulatedUnixPlatform.LinuxX64 -> "6.8.0-51-generic"
        | SimulatedUnixPlatform.MacOsArm64 -> "24.6.0"
        | SimulatedUnixPlatform.Custom release ->
            if String.length release = 0 then
                failwith
                    "SimulatedUnixPlatform.Custom: release string is empty, but every Unix `uname(2)` fills `utsname.release`"

            if String.length release > maxReleaseLength then
                failwith
                    $"SimulatedUnixPlatform.Custom: release string is %d{String.length release} characters, exceeding the %d{maxReleaseLength}-character limit any Unix `utsname.release` can hold"

            match release |> Seq.tryFindIndex (fun c -> c < ' ' || c > '~') with
            | Some i ->
                failwith
                    $"SimulatedUnixPlatform.Custom: release string contains non-printable-ASCII character U+%04X{int release.[i]} at index %d{i}; `utsname.release` is reported to the guest as single-byte characters, so only printable ASCII round-trips faithfully"
            | None -> release

/// Aggregates the slice of `IlMachineState` that models host-kernel /
/// syscall-emulation state: process-wide last-error registers, the native
/// heap pool backing `Marshal.AllocHGlobal`, the Unix file-descriptor table,
/// the `LowLevelMonitor` registry, and monotonic ID counters for opaque
/// kernel handles. These are the pieces of interpreter state that exist
/// because PawPrint refuses to use the host kernel; they don't belong in the
/// CIL execution model proper.
///
/// Pulling them into a sub-record keeps `IlMachineState` from sprawling and
/// makes it possible to swap the kernel implementation (e.g. for a Windows-
/// shaped emulation) without disturbing the rest of the state model.
type EmulatedKernel =
    {
        /// Last error reported by a modelled P/Invoke with SetLastError=true.
        /// This is currently process-wide; model it per-thread when a guest
        /// depends on thread-local last-error state.
        LastPInvokeError : int
        /// Last system error tracked separately from LastPInvokeError because
        /// CoreLib wrappers can read this and then write LastPInvokeError.
        /// This is currently process-wide; model it per-thread when a guest
        /// depends on thread-local GetLastError or errno state.
        LastSystemError : int
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
        /// The driver loop advances this by 1 ms each time it increments
        /// `StepCounter`, so the guest sees one wall-clock millisecond per
        /// scheduler tick. That makes elapsed-time polling loops like
        /// `while (TickCount64 - start &lt; N)` terminate in O(N) ticks —
        /// the absolute rate is "very slow computer" by wall-clock
        /// standards but exact bit-for-bit reproducibility is the goal,
        /// not realism.
        ///
        /// Reading the field never mutates it: the BCL's `TickCount64`
        /// observers stay pure, and the consistency property "two threads
        /// reading on the same tick observe the same value" falls out of
        /// the scheduler being the sole writer. Deliberately *not* derived
        /// from `StepCounter`: a future PR adding deadline-aware waits
        /// will want to jump the clock forward to the next deadline when
        /// no thread is Runnable, and that jump must not require a
        /// matching jump in `StepCounter` (which would skew the spurious-
        /// wakeup schedule).
        VirtualClockMs : int64
        /// Wall-clock time, in milliseconds since the Unix epoch, that the
        /// simulated process boots at — i.e. the wall-clock reading that
        /// corresponds to `VirtualClockMs = 0`. The realtime clock the guest
        /// observes is the affine image of the monotonic one:
        /// `systemTimeAsTicks = (WallClockEpochMs + VirtualClockMs) * 10_000`.
        ///
        /// Deliberately *not* a second mutable clock advanced alongside
        /// `VirtualClockMs`. A parallel field would be behaviourally identical
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
        /// Simulated process environment variable table. Consulted by
        /// `Environment.GetEnvironmentVariable` and the Win32
        /// `GetEnvironmentVariableW` shim. Seeded with
        /// `DOTNET_SYSTEM_GLOBALIZATION_INVARIANT=1` so guest BCL code that
        /// reads it during startup gets the invariant-globalization mode
        /// PawPrint requires; the CLI overlays the host process's env on top
        /// of this default at startup, and tests can pass their own overlay
        /// via `Program.run`.
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
        /// `Environment.OSVersion` on a Unix CoreLib). Deliberately kernel
        /// state rather than a host `uname(2)` read; see
        /// `SimulatedUnixPlatform` for why.
        ///
        /// Unlike `ProcessorCount`, CoreLib does *not* latch this during
        /// static initialisation — `Environment.OSVersion` is a lazily
        /// populated static that is only computed on first read — but hosts
        /// should still set it via `KernelConfig` rather than by record-copy
        /// after startup, so that the value is fixed for the whole run and a
        /// guest cannot observe it changing under it.
        UnixPlatform : SimulatedUnixPlatform
        /// Pure data model of the simulated process's signal disposition,
        /// per-thread sigprocmasks, and pending-signal queue. Populated by
        /// future slices: nothing in the simulator dispatches signals yet,
        /// so the field stays at `SignalState.empty` across every run today.
        /// Held on `EmulatedKernel` (rather than per-thread) because POSIX
        /// signal disposition is process-wide; the per-thread piece lives
        /// inside `SignalState.Blocked`.
        Signals : SignalState
    }

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
    /// One, because that is the value every existing run already observed
    /// (the test harness hard-coded it and the CLI read the host's count, so
    /// only single-processor behaviour has ever been exercised end-to-end),
    /// and because a fixed default is a prerequisite for replayability.
    /// Hosts that want to exercise the guest's multi-processor code paths
    /// raise it via `KernelConfig.ProcessorCount`.
    [<Literal>]
    let defaultProcessorCount : int = 1

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
    /// There is in fact a stronger justification than that derivation:
    /// CoreCLR ships exactly this number as its own literal
    /// pre-measurement default. `src/coreclr/utilcode/yieldprocessornormalized.cpp`
    /// initialises `s_optimalMaxNormalizedYieldsPerSpinIteration` to
    /// `(unsigned int)(272.0 / 37.0 + 0.5)` = `7`, commented "Defaults are for
    /// when normalization has not yet been done". So 7 is not merely what the
    /// formula *would* yield for an idealised host -- it is the value a real
    /// CoreCLR process genuinely reports for the entire window before its
    /// background measurement completes. A simulated process that never
    /// performs that measurement reporting the never-measured default is
    /// about as faithful as this can be.
    [<Literal>]
    let defaultOptimalMaxSpinWaitsPerSpinIteration : int = 7

    /// 100ns ticks per millisecond. The `SystemNative_GetSystemTime*` family
    /// speaks in ticks while PawPrint's virtual clock speaks in milliseconds,
    /// so every wall-clock derivation goes through this factor. A consequence
    /// is that every tick value the guest ever observes is a multiple of
    /// 10,000: `DateTime.UtcNow` has millisecond granularity here, where real
    /// `clock_gettime(CLOCK_REALTIME)` is far finer.
    [<Literal>]
    let ticksPerMillisecond : int64 = 10_000L

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
    let defaultUnixPlatform : SimulatedUnixPlatform = SimulatedUnixPlatform.LinuxX64

    let initial : EmulatedKernel =
        {
            LastPInvokeError = 0
            LastSystemError = 0
            NativeMemoryPool = NativeMemoryPool.empty
            FileDescriptors = FileDescriptorRegistry.initial
            LowLevelMonitors = Map.empty
            NextLowLevelMonitorId = 1
            WaitHandles = Map.empty
            NextWaitHandleId = 1
            NextEventPipeId = 1L
            SpuriousWakeup = SpuriousWakeupStrategy.Disabled
            SyncBlockSpuriousWakeup = SyncBlockSpuriousWakeupStrategy.Disabled
            StepCounter = 0L
            VirtualClockMs = 0L
            WallClockEpochMs = 0L
            NonCryptoRandomState = NonCryptoRandom.initialState
            CryptoRandomState = cryptoRandomInitialState
            OutputLog = ImmutableArray<OutputLogEntry>.Empty
            Environment = defaultEnvironment
            ProcessorCount = defaultProcessorCount
            OptimalMaxSpinWaitsPerSpinIteration = defaultOptimalMaxSpinWaitsPerSpinIteration
            UnixPlatform = defaultUnixPlatform
            Signals = SignalState.empty
        }

    /// Set the Unix platform identity the simulated process reports. Forces
    /// the release string eagerly so that an invalid `Custom` payload fails
    /// at configuration time — where the caller can see which knob is wrong —
    /// rather than at the first `Environment.OSVersion` read deep inside
    /// guest code.
    let withUnixPlatform (platform : SimulatedUnixPlatform) (kernel : EmulatedKernel) : EmulatedKernel =
        SimulatedUnixPlatform.unixRelease platform |> ignore<string>

        { kernel with
            UnixPlatform = platform
        }

    /// Set the logical-processor count the simulated process reports. Rejects
    /// non-positive values at the boundary rather than letting them reach a
    /// guest that will divide by them.
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
    /// `VirtualClockMs` guarantees for `Environment.TickCount64`, and the
    /// reason this is a plain derivation rather than an advance-on-read
    /// counter. That does mean `DateTime.UtcNow` is only *weakly* monotonic
    /// here: repeated reads within one scheduler tick are equal, so it is not
    /// a source of unique values. Real `clock_gettime(CLOCK_REALTIME)` makes no
    /// uniqueness guarantee either, so guest code relying on one is broken on
    /// the real runtime too and should be caught rather than accommodated.
    let systemTimeAsTicks (kernel : EmulatedKernel) : int64 =
        // A kernel built by record-copy can bypass `withWallClockEpochMs`, so
        // re-assert the invariant here: the guest must never observe a tick
        // count that names no `DateTime`. Checking both operands first also
        // establishes that neither the addition nor the multiplication below
        // can overflow (each is at most `maxWallClockEpochMs`, whose doubled
        // value scaled by `ticksPerMillisecond` still fits in an int64).
        if kernel.WallClockEpochMs < 0L || kernel.WallClockEpochMs > maxWallClockEpochMs then
            failwith
                $"kernel WallClockEpochMs is %d{kernel.WallClockEpochMs}, which is outside the range [0, %d{maxWallClockEpochMs}] that System.DateTime can represent"

        if kernel.VirtualClockMs < 0L || kernel.VirtualClockMs > maxWallClockEpochMs then
            failwith
                $"kernel VirtualClockMs is %d{kernel.VirtualClockMs}, which is outside the range [0, %d{maxWallClockEpochMs}] a wall-clock reading can be derived from"

        let ms = kernel.WallClockEpochMs + kernel.VirtualClockMs

        if ms > maxWallClockEpochMs then
            failwith
                $"simulated wall clock has reached %d{ms} ms since the Unix epoch, past the %d{maxWallClockEpochMs} ms that System.DateTime can represent; lower KernelConfig.WallClockEpochMs"

        ms * ticksPerMillisecond

    /// Nanoseconds per millisecond. `SystemNative_GetTimestamp` speaks in
    /// nanoseconds while PawPrint's virtual clock speaks in milliseconds, so
    /// the high-resolution timestamp derivation goes through this factor. A
    /// consequence is that every timestamp the guest ever observes is a
    /// multiple of 1,000,000: `Stopwatch` has millisecond granularity here,
    /// where real `clock_gettime(CLOCK_MONOTONIC)` is far finer.
    [<Literal>]
    let nanosecondsPerMillisecond : int64 = 1_000_000L

    /// Largest `VirtualClockMs` from which a nanosecond timestamp can be
    /// derived without overflowing the `int64` the PAL entry point returns:
    /// `Int64.MaxValue / nanosecondsPerMillisecond`, i.e. about 292 years of
    /// simulated uptime.
    ///
    /// The horizon is reachable by ordinary guest code, not merely in
    /// principle. A sleep deadline is `VirtualClockMs + timeout` with no cap,
    /// and when no thread is Runnable the driver's deadline jump moves the
    /// clock the whole way there, so each `Thread.Sleep(Int32.MaxValue)`
    /// advances it by about 2.1e9 ms: eight of them advance it by
    /// 17,179,869,451 ms, and roughly 4,300 cross this bound. So
    /// `monotonicTimestampNanos` checks rather than assumes — silently wrapping
    /// into a negative timestamp would hand the guest a monotonic clock that
    /// had run backwards, which is the one guarantee the primitive exists to
    /// provide.
    ///
    /// The bound is *tighter* than `maxWallClockEpochMs` by a factor of about
    /// 27, so there is a band of clock readings from which `DateTime.UtcNow`
    /// and `Environment.TickCount64` are derivable but `Stopwatch.GetTimestamp`
    /// is not. Nothing bounds `VirtualClockMs` at its write sites, so each
    /// clock-derived PAL entry enforces its own ceiling lazily, at the moment
    /// the guest reads that particular clock — `systemTimeAsTicks` has the same
    /// shape. Bounding the field centrally at the scheduler, its sole writer,
    /// would collapse these into one invariant and fault at the wait that
    /// pushed time past the horizon rather than at an arbitrary later read.
    [<Literal>]
    let maxMonotonicTimestampClockMs : int64 = 9223372036854L

    /// Monotonic time since the simulated process booted, in nanoseconds:
    /// exactly what `SystemNative_GetTimestamp` returns, and hence what
    /// `Stopwatch.GetTimestamp()` reports on a Unix CoreLib.
    ///
    /// Real CoreCLR answers this from `minipal_hires_ticks()`
    /// (`clock_gettime_nsec_np(CLOCK_UPTIME_RAW)` on macOS,
    /// `clock_gettime(CLOCK_MONOTONIC)` on Linux). PawPrint derives it from
    /// the same `VirtualClockMs` that already backs
    /// `SystemNative_GetLowResolutionTimestamp` — which upstream is
    /// `minipal_lowres_ticks()`, *the same clock* read in milliseconds. Making
    /// both PawPrint entry points views of one field is therefore not merely
    /// convenient: it reproduces a relationship the guest can observe, since
    /// `Environment.TickCount64` and `Stopwatch` must not disagree about how
    /// much time has passed.
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
        // The driver loop is the only production writer of `VirtualClockMs`
        // and only ever advances it from zero, but a kernel built by
        // record-copy (as tests do) can bypass that, so re-assert here rather
        // than trusting construction.
        if
            kernel.VirtualClockMs < 0L
            || kernel.VirtualClockMs > maxMonotonicTimestampClockMs
        then
            failwith
                $"kernel VirtualClockMs is %d{kernel.VirtualClockMs}, which is outside the range [0, %d{maxMonotonicTimestampClockMs}] a nanosecond monotonic timestamp can be derived from without overflowing int64"

        kernel.VirtualClockMs * nanosecondsPerMillisecond

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
    /// (`IlMachineState.NextGuestThreadOrdinal`) that only guest-visible thread
    /// creation advances.
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

    /// The largest thread ordinal either `osThreadId` producer accepts.
    ///
    /// `Int32.MaxValue` is excluded, not merely `Int32.MaxValue + 1`: the guest
    /// producer computes `2*ordinal + 1`, which at `ordinal = Int32.MaxValue`
    /// is exactly `0xFFFF_FFFF` — the `TryGetUInt32OSThreadId` "this platform
    /// cannot determine a thread id" sentinel. Excluding it makes both sentinel
    /// values (`0` and `0xFFFF_FFFF`) unreachable by construction rather than
    /// by discipline. `IlMachineState`'s cursors are `int`s that would have
    /// overflowed long before a run created two billion threads, so this is a
    /// tripwire rather than a limit anyone can hit.
    let private maxOsThreadOrdinal : int = System.Int32.MaxValue - 1

    /// OS thread id policy for guest-visible threads: the id the `ordinal`-th
    /// such thread reports through `SystemNative_TryGetUInt32OSThreadId` and
    /// `SystemNative_GetUInt64OSThreadId`.
    ///
    /// `ordinal` is `IlMachineState.NextGuestThreadOrdinal`, the same cursor
    /// `cpuForRotation` consumes, and deliberately *not* the thread's
    /// `ThreadId`, which is PawPrint-internal rather than guest-visible.
    ///
    /// Ids are **odd**, so that they never collide with PawPrint's minted
    /// `OsThreadId`s for synthetic threads.
    let osThreadIdForGuest (ordinal : int) : OsThreadId =
        if ordinal < 0 || ordinal > maxOsThreadOrdinal then
            failwith
                $"guest thread ordinal must be in [0, %d{maxOsThreadOrdinal}] to mint an OS thread id that is neither 0 nor the (uint32)-1 sentinel; got %d{ordinal}"

        OsThreadId (2u * uint32 ordinal + 1u)

    /// OS thread id policy for PawPrint-internal auxiliary threads — currently
    /// just the signal dispatcher minted by `IlMachineState.allocateParkedThread`.
    ///
    /// Such a thread needs a real, distinct id even though no guest can name
    /// it, because it *runs* guest code: `SignalDispatch` wakes it onto a
    /// managed signal handler, and that handler may take a `System.Threading.Lock`.
    ///
    /// Ids are **even**, so that they never collide with guest-visible threads.
    let osThreadIdForInternal (i : int) : OsThreadId =
        if i < 0 || i > maxOsThreadOrdinal then
            failwith
                $"internal thread ordinal must be in [0, %d{maxOsThreadOrdinal}] to mint an OS thread id that is not 0; got %d{i}"

        OsThreadId (2u * uint32 i + 2u)

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
    let withEnvironment (env : Map<string, string>) (kernel : EmulatedKernel) : EmulatedKernel =
        let merged =
            (kernel.Environment, env)
            ||> Map.fold (fun acc key value -> Map.add key value acc)

        { kernel with
            Environment = merged
        }

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
        Environment : Map<string, string>
        /// Logical processor count the guest observes via
        /// `Environment.ProcessorCount`. Must be at least 1.
        ProcessorCount : int
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
    }

    /// Configuration a host gets if it expresses no preference: no environment
    /// overlay, the default single processor, a wall clock booting at the Unix
    /// epoch, and the default Unix platform.
    static member Default : KernelConfig =
        {
            Environment = Map.empty
            ProcessorCount = EmulatedKernel.defaultProcessorCount
            OptimalMaxSpinWaitsPerSpinIteration = EmulatedKernel.defaultOptimalMaxSpinWaitsPerSpinIteration
            WallClockEpochMs = 0L
            UnixPlatform = EmulatedKernel.defaultUnixPlatform
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
        |> EmulatedKernel.withOptimalMaxSpinWaitsPerSpinIteration config.OptimalMaxSpinWaitsPerSpinIteration
        |> EmulatedKernel.withWallClockEpochMs config.WallClockEpochMs
        |> EmulatedKernel.withUnixPlatform config.UnixPlatform

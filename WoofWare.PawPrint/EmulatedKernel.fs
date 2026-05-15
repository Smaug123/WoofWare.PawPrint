namespace WoofWare.PawPrint

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
        /// Monotonically-advancing scheduler tick consumed by
        /// `SpuriousWakeupStrategy`. The driver loop applies the strategy
        /// against the current value and then increments by 1 before
        /// calling `Scheduler.chooseNext`. Threading the tick through state
        /// (rather than as a side argument to the scheduler) keeps the
        /// pure model self-contained and means tests can drive the strategy
        /// without spinning up a real driver.
        StepCounter : int64
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
    }

/// Deterministic non-cryptographic PRNG that backs
/// `SystemNative_GetNonCryptographicallySecureRandomBytes`. This is the
/// `splitmix64` step from Vigna's reference at
/// <http://prng.di.unimi.it/splitmix64.c>: stateful, full-period over the
/// 2^64 64-bit states, and famously fine for *seeding* other PRNGs (which
/// is exactly what the BCL does here — it feeds the buffer into
/// `Random.XoshiroImpl`, `Marvin.DefaultSeed`, `HashCode`'s seed, etc.).
///
/// Quality is intentionally non-cryptographic; that matches the entry-point
/// name. The state being non-zero is preserved across the step (splitmix64
/// has no fixed points other than `0 -> 0x9E3779B97F4A7C15`-style transient
/// behaviour from a zero start, which is why `initial` seeds with the
/// golden-ratio constant rather than zero).
[<RequireQualifiedAccess>]
module NonCryptoRandom =
    /// `floor(2^64 / phi)`, the same constant the reference splitmix64 uses
    /// as its weyl increment. Picked as the default initial state so a
    /// fresh interpreter doesn't start from zero (which would still produce
    /// a non-zero output after one step, but is the documented degenerate
    /// input for splitmix64-style mixers).
    let initialState : uint64 = 0x9E3779B97F4A7C15UL

    /// Advance the splitmix64 state by one step and return the 64-bit
    /// output drawn from the new state. Reference implementation:
    /// <http://prng.di.unimi.it/splitmix64.c>. Pure: same input state
    /// produces the same `(output, newState)` pair on every call.
    let step (state : uint64) : uint64 * uint64 =
        let newState = state + 0x9E3779B97F4A7C15UL
        let mutable z = newState
        z <- (z ^^^ (z >>> 30)) * 0xBF58476D1CE4E5B9UL
        z <- (z ^^^ (z >>> 27)) * 0x94D049BB133111EBUL
        z <- z ^^^ (z >>> 31)
        z, newState

    /// Draw `count` pseudo-random bytes from the splitmix64 state. Returns
    /// the bytes in little-endian unpack order from each 64-bit step, plus
    /// the new state. `count` must be non-negative; a negative count is a
    /// caller bug and the function will fail loudly.
    let drawBytes (count : int) (state : uint64) : byte[] * uint64 =
        if count < 0 then
            failwith $"NonCryptoRandom.drawBytes: byte count %d{count} is negative"

        let buffer = Array.zeroCreate<byte> count
        let mutable state = state
        let mutable i = 0

        while i < count do
            let output, newState = step state
            state <- newState
            let remaining = count - i
            let chunk = if remaining > 8 then 8 else remaining

            for j = 0 to chunk - 1 do
                buffer.[i + j] <- byte (output >>> (8 * j))

            i <- i + chunk

        buffer, state

[<RequireQualifiedAccess>]
module EmulatedKernel =
    let initial : EmulatedKernel =
        {
            LastPInvokeError = 0
            LastSystemError = 0
            NativeMemoryPool = NativeMemoryPool.empty
            FileDescriptors = FileDescriptorRegistry.initial
            LowLevelMonitors = Map.empty
            NextLowLevelMonitorId = 1
            NextEventPipeId = 1L
            SpuriousWakeup = SpuriousWakeupStrategy.Disabled
            StepCounter = 0L
            NonCryptoRandomState = NonCryptoRandom.initialState
        }

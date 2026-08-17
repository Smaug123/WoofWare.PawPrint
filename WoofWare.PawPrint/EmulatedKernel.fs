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
    /// The Darwin answer here is the `umask 022` one, which PawPrint has to
    /// invent because it models no umask yet (nothing can read or set one —
    /// CoreLib's interop surface has no `SystemNative_UMask` — and no creating
    /// native exists). When a umask arrives, this becomes a function of it.
    let symlinkPermissions (platform : SimulatedUnixPlatform) : PermissionBits =
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> PermissionBits.parseOrFail "SimulatedUnixPlatform.symlinkPermissions" 0o777
        | SimulatedUnixFlavour.Darwin ->
            PermissionBits.parseOrFail "SimulatedUnixPlatform.symlinkPermissions" (0o777 &&& ~~~0o022)

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
        CurrentDirectory : AbsoluteUnixPath
        /// The simulated process's filesystem: every inode a guest can reach
        /// through the `SystemNative_*` path calls.
        ///
        /// Seeded from `KernelConfig.FileSystem` and, for now, immutable — no
        /// native mutates it yet. It is emulated kernel state rather than
        /// anything the interpreter reads from the host, for the usual reason:
        /// a filesystem read from the host would make a replay depend on the
        /// machine that produced it, and guests branch on what they find.
        FileSystem : VirtualFileSystem
        /// The effective user ID the simulated process runs as, reported by
        /// `stat` as every inode's `st_uid` and (when it lands) by
        /// `SystemNative_GetEUid`.
        ///
        /// Process-wide rather than per-inode: no managed caller can change a
        /// file's owner, because `SystemNative_ChOwn` does not exist anywhere in
        /// the runtime's interop surface, so a per-inode field could never make
        /// two inodes differ and would carry no information this does not.
        UserId : uint32
        /// The effective group ID, reported as every inode's `st_gid`. See
        /// `UserId`.
        GroupId : uint32
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

    /// Effective user ID a freshly-minted simulated process runs as.
    ///
    /// 1000 rather than 0: `Environment.IsPrivilegedProcess` is literally
    /// `GetEUid() == 0`, so a guest that defaulted to root would silently take
    /// the privileged branch of every check it makes about itself — the
    /// uninteresting one, and not the one most programs are written for. 1000
    /// is also the first interactive user on the Ubuntu-shaped platform
    /// `defaultUnixPlatform` already claims to be. A host that wants root says
    /// so in `KernelConfig.UserId`.
    let defaultUserId : uint32 = 1000u

    /// Effective group ID a freshly-minted simulated process runs as. Matches
    /// `defaultUserId`, as a Linux user-private group does.
    let defaultGroupId : uint32 = 1000u

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
        {
            InstructionCostTicks = defaultInstructionCostTicks
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
            FileSystem = VirtualFileSystem.empty (UnixTimestamp.ofMillisecondsSinceEpoch 0L)
            UserId = defaultUserId
            GroupId = defaultGroupId
            Signals = SignalState.empty
        }

    /// Set the Unix platform identity the simulated process reports. Rejects a
    /// forged `Unchecked.defaultof` platform, whose null release would
    /// otherwise reach a guest as its `uname -r`.
    let withUnixPlatform (platform : SimulatedUnixPlatform) (kernel : EmulatedKernel) : EmulatedKernel =
        // No eager validation of the release string:
        // `SimulatedUnixPlatform.create` validates at construction, so a value
        // of the type is already a platform some Unix could be. `assertValid`
        // still catches the one value that can bypass that — the forged
        // `Unchecked.defaultof`.
        { kernel with
            UnixPlatform = SimulatedUnixPlatform.assertValid "EmulatedKernel.UnixPlatform" platform
        }

    /// Set the simulated process's current working directory.
    let withCurrentDirectory (dir : AbsoluteUnixPath) (kernel : EmulatedKernel) : EmulatedKernel =
        { kernel with
            CurrentDirectory = AbsoluteUnixPath.assertValid "EmulatedKernel.CurrentDirectory" dir
        }

    /// Realise a host's filesystem seed, with every inode created at
    /// `createdAt`.
    ///
    /// Takes the moment explicitly rather than reading `kernel.WallClockEpochMs`
    /// so that the result does not depend on whether the caller happened to set
    /// the clock before or after the filesystem — an ordering dependence between
    /// two `with` functions is exactly the kind of thing that works until
    /// someone reorders `KernelConfig.applyTo`.
    let withFileSystem
        (createdAt : UnixTimestamp)
        (seed : Map<FileName, SeedEntry>)
        (kernel : EmulatedKernel)
        : EmulatedKernel
        =
        { kernel with
            FileSystem = FileSystemSeed.toVirtualFileSystem createdAt seed
        }

    /// Set the effective user and group IDs the simulated process runs as.
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

    /// Advance the virtual clock to `ticks`, which must not move it backwards and must keep it
    /// inside the range every clock-derived reading can be computed from.
    ///
    /// The bound is `maxMonotonicTimestampClockTicks` — the tightest of the per-reader ceilings
    /// — so this is deliberately stricter than any individual reader requires. Enforcing it at
    /// the writer means a guest that runs the clock off the end faults at the wait that did it,
    /// naming the operation responsible, rather than at whichever unlucky later `Stopwatch` read
    /// happens to trip over the value.
    let withVirtualClockTicks (ticks : int64) (kernel : EmulatedKernel) : EmulatedKernel =
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

        { kernel with
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
            OptimalMaxSpinWaitsPerSpinIteration = EmulatedKernel.defaultOptimalMaxSpinWaitsPerSpinIteration
            WallClockEpochMs = 0L
            UnixPlatform = EmulatedKernel.defaultUnixPlatform
            CurrentDirectory = EmulatedKernel.defaultCurrentDirectory
            FileSystem = FileSystemSeed.empty
            UserId = EmulatedKernel.defaultUserId
            GroupId = EmulatedKernel.defaultGroupId
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
        |> EmulatedKernel.withOptimalMaxSpinWaitsPerSpinIteration config.OptimalMaxSpinWaitsPerSpinIteration
        |> EmulatedKernel.withWallClockEpochMs config.WallClockEpochMs
        |> EmulatedKernel.withUnixPlatform config.UnixPlatform
        |> EmulatedKernel.withCurrentDirectory config.CurrentDirectory
        |> EmulatedKernel.withFileSystem
            (UnixTimestamp.ofMillisecondsSinceEpoch config.WallClockEpochMs)
            config.FileSystem
        |> EmulatedKernel.withUserAndGroupId config.UserId config.GroupId

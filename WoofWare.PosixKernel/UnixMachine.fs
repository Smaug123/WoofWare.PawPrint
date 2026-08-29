namespace WoofWare.PosixKernel

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
        /// `UnixSystem.checkInvariants` enforces that — a connection
        /// awaiting `accept(2)` is a `TcpConnection` in `Connections`, not a
        /// socket, precisely so this rule can stay strict.
        Sockets : Map<SocketId, SocketDescription>
        /// Every TCP connection the simulated kernel holds: established ends
        /// referenced from a socket's `SocketPhase`, and completed
        /// connections waiting in some listener's accept queue. An entry is
        /// removed when nothing references it any more (`UnixSystem.close`).
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
        /// configuration; see `UnixSystem.defaultEphemeralPortRange`.
        EphemeralPortRange : uint16 * uint16
        /// The value of the `somaxconn` sysctl (`net.core.somaxconn` on
        /// Linux, `kern.ipc.somaxconn` on Darwin): the ceiling `listen(2)`
        /// clamps its backlog to before the accept-queue capacity is derived.
        /// Host configuration with a per-flavour default; see
        /// `UnixMachineState.withSoMaxConn` for the measured clamp rules.
        SoMaxConn : int
        /// The IPv4 addresses this machine holds. Host configuration; see
        /// `UnixSystem.defaultLocalAddresses`.
        LocalAddresses : uint32 list
        /// Prefixes this machine has a local route to, which Linux will bind any
        /// address inside and Darwin ignores. See
        /// `UnixSystem.defaultLocalRoutes`.
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
        /// `UnixMachineState.systemTimeAsTicks`, and
        /// `SystemNative_GetTimestamp` (the high-resolution clock behind
        /// `Stopwatch`) derives from it via
        /// `UnixMachineState.monotonicTimestampNanos` — rather than either
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

[<RequireQualifiedAccess>]
module UnixMachineState =

    /// Set the greatest range end a user buffer may reach. Rejects zero, which
    /// leaves no address usable as a buffer and so describes no machine.
    let withUserAddressLimit (limit : uint64) (machine : UnixMachineState) : UnixMachineState =
        if limit = 0UL then
            failwith "UserAddressLimit must be positive; got 0, which is a machine with no user address space"

        { machine with
            UserAddressLimit = limit
        }

    /// Set the logical-processor count the simulated process reports. Rejects
    /// non-positive values at the boundary rather than letting them reach a
    /// guest that will divide by them.
    let withProcessorCount (count : int) (machine : UnixMachineState) : UnixMachineState =
        if count < 1 then
            failwith $"ProcessorCount must be at least 1; got %d{count}"

        { machine with
            ProcessorCount = count
        }

    /// Sets the ephemeral range, and rewinds the cursor into it: a cursor left
    /// outside the range would hand out its first port from wherever the previous
    /// range had reached.
    let withEphemeralPortRange ((low, high) : uint16 * uint16) (machine : UnixMachineState) : UnixMachineState =
        if low = 0us then
            failwith
                "UnixMachineState.EphemeralPortRange: port 0 is how a guest *asks* for an ephemeral port, so it cannot also be one that gets handed out. Start the range at 1 or above."

        if low > high then
            failwith
                $"UnixMachineState.EphemeralPortRange: the range %d{low}-%d{high} is empty, so no bind of port 0 could ever be answered."

        { machine with
            EphemeralPortRange = low, high
            NextEphemeralPort = low
        }

    /// Largest legal `EmulatedKernel.WallClockEpochMs`: 9999-12-31T23:59:59.999Z
    /// as milliseconds since the Unix epoch, which is the last instant
    /// `System.DateTime` can represent
    /// (`(DateTime.MaxValue.Ticks - DateTime.UnixEpoch.Ticks) / ticksPerMillisecond`).
    /// Beyond this the ticks CoreLib adds `UnixEpochTicks` to no longer name a
    /// `DateTime`, and because `DateTime.UtcNow` uses the unvalidated private
    /// ctor the guest would observe the corruption rather than an exception.
    [<Literal>]
    let maxWallClockEpochMs : int64 = 253402300799999L

    /// Set the wall-clock reading the simulated process boots at. Rejects
    /// values outside the range `System.DateTime` can represent at the
    /// boundary, rather than letting them reach a guest that would receive a
    /// silently corrupt `DateTime` from `DateTime.UtcNow`'s unvalidated ctor.
    let withWallClockEpochMs (epochMs : int64) (machine : UnixMachineState) : UnixMachineState =
        if epochMs < 0L then
            failwith
                $"WallClockEpochMs must be non-negative (PawPrint does not model a simulated process booting before the Unix epoch); got %d{epochMs}"

        if epochMs > maxWallClockEpochMs then
            failwith
                $"WallClockEpochMs must be at most %d{maxWallClockEpochMs} (9999-12-31T23:59:59.999Z, the last instant System.DateTime can represent); got %d{epochMs}"

        { machine with
            WallClockEpochMs = epochMs
        }

    /// Largest `VirtualClockTicks` from which a nanosecond timestamp can be
    /// derived without overflowing the `int64` the PAL entry point returns:
    /// `Int64.MaxValue / nanosecondsPerTick`, i.e. about 292 years of simulated
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
    let validateVirtualClockTicks (ticks : int64) (machine : UnixMachineState) : unit =
        // Checked independently of the monotonicity comparison below, which on its own would
        // wave through a negative target whenever the current value is more negative still —
        // reachable because a machine assembled by record-copy never passed through here.
        if ticks < 0L then
            failwith
                $"virtual clock would be set to %d{ticks} ticks; simulated uptime starts at zero and cannot be negative"

        if ticks < machine.VirtualClockTicks then
            failwith
                $"virtual clock would move backwards, from %d{machine.VirtualClockTicks} to %d{ticks} ticks; it is monotonic by construction and every guest-visible clock derives from it"

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
    let withVirtualClockTicks (ticks : int64) (machine : UnixMachineState) : UnixMachineState =
        validateVirtualClockTicks ticks machine

        { machine with
            VirtualClockTicks = ticks
        }

    let withLocalAddresses
        (addresses : uint32 list)
        (routes : Ipv4Prefix list)
        (machine : UnixMachineState)
        : UnixMachineState
        =
        // The prefix record is public, so a host can build one whose length is
        // outside [0, 32]; the CLI masks such a shift rather than faulting, which
        // would give an unrelated mask and a silently wrong bindability.
        let routes =
            routes |> List.map (Ipv4Prefix.assertValid "UnixMachineState.LocalRoutes")

        // An empty list is legal and means a machine with no addresses at all,
        // on which only the wildcard binds. That is a strange machine but a
        // representable one, and refusing it here would be inventing a rule.
        { machine with
            LocalAddresses = addresses
            LocalRoutes = routes
        }

    /// Set the Unix platform identity the simulated process reports, together
    /// with the filesystem its mount claims to be. `None` takes the flavour's
    /// own default; an explicit type that flavour could not mount is refused.
    ///
    /// One setter for two fields, in the manner of `withUserAndGroupId`,
    /// because they are not independent: `SystemNative_GetFileSystemType`
    /// answers a *file* from the type and every other descriptor from the
    /// flavour, so a machine carrying Linux with APFS would hand a guest a
    /// combination no machine could produce. Separate setters could each be
    /// called alone, which is exactly how that state would arise; fused, it is
    /// unrepresentable rather than merely checked.
    ///
    /// Rejects a forged `Unchecked.defaultof` platform, whose null release
    /// would otherwise reach a guest as its `uname -r`.
    let withUnixPlatformAndFileSystemType
        (platform : SimulatedUnixPlatform)
        (fileSystemType : EmulatedFileSystemType option)
        (machine : UnixMachineState)
        : UnixMachineState
        =
        // No eager validation of the release string:
        // `SimulatedUnixPlatform.create` validates at construction, so a value
        // of the type is already a platform some Unix could be. `assertValid`
        // still catches the one value that can bypass that — the forged
        // `Unchecked.defaultof`.
        let platform =
            SimulatedUnixPlatform.assertValid "UnixMachineState.UnixPlatform" platform

        let flavour = SimulatedUnixPlatform.flavour platform

        let resolved =
            match fileSystemType with
            | None -> EmulatedFileSystemType.defaultFor flavour
            | Some requested ->
                if not (EmulatedFileSystemType.isReportableUnder flavour requested) then
                    failwith
                        $"UnixMachineState.FileSystemType: a %O{flavour} kernel cannot report %O{requested}, so a guest asking `fstatfs` would learn a fact no such system could tell it. Leave KernelConfig.FileSystemType as None to take %O{flavour}'s own default, or pick a type that flavour mounts."

                requested

        { machine with
            UnixPlatform = platform
            FileSystemType = resolved
        }

    /// Whether, and where, this machine's machine screens a read or write buffer
    /// before performing the operation: the flavour decides whether, the
    /// machine's address-space limit decides where.
    let userBufferCheck (machine : UnixMachineState) : UserBufferCheck =
        if SimulatedUnixPlatform.screensUserBufferUpFront machine.UnixPlatform then
            UserBufferCheck.BeforeOperation machine.UserAddressLimit
        else
            UserBufferCheck.AtCopyTime

    /// The socket `socketId` names.
    ///
    /// Total, and loudly partial rather than an option: every `SocketId` a
    /// caller can hold came out of an `OpenFileTarget.Socket`, and
    /// `checkInvariants` rejects a machine in which one of those names nothing.
    /// A `None` here would push that impossible case onto every call site.
    let socket (socketId : SocketId) (machine : UnixMachineState) : SocketDescription =
        match Map.tryFind socketId machine.Sockets with
        | Some socket -> socket
        | None ->
            failwith
                $"UnixMachineState.socket: %O{socketId} names no socket in this kernel's socket table. Every SocketId reachable by a caller comes from an open file description, and UnixSystemDefect.DanglingSocket exists to make that unreachable, so this is an interpreter bug rather than anything a guest did."

    /// The connection `connectionId` names.
    ///
    /// Total, and loudly partial rather than an option: every `ConnectionId` a
    /// caller can hold came out of a socket phase or an accept queue, and
    /// `checkInvariants` rejects a machine in which one of those dangles.
    let connection (connectionId : ConnectionId) (machine : UnixMachineState) : TcpConnection =
        match Map.tryFind connectionId machine.Connections with
        | Some connection -> connection
        | None ->
            failwith
                $"UnixMachineState.connection: %O{connectionId} names no connection in this kernel's connection table. UnixSystemDefect.DanglingConnection and DanglingQueuedConnection exist to make this unreachable, so this is an interpreter bug."

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
    /// question can be asked of a Darwin-flavoured machine.
    let socketReadinessLevel (socketId : SocketId) (machine : UnixMachineState) : ReadinessLevel =
        let target = socket socketId machine

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
                    $"UnixMachineState.socketReadinessLevel: socket %O{socketId} is %O{target.Kind}, whose readiness is measured for poll but not for epoll. Both kinds are reachable only in the AF_UNIX domain, and two callers arrive here: an epoll ADD (the registration screen rejects only regular files, so a socket of any kind is admitted) and `SystemNative_Poll` (which needs no registration at all). `poll(2)` reports OUT for a fresh SOCK_RAW and OUT|HUP for a fresh SOCK_SEQPACKET on Linux (docs/plans/2026-08-23-socket-poll/pollgaps.c). Those two rows are the whole answer only while PawPrint's own `listen`/`connect`/`accept` handlers keep refusing these kinds, which is what confines such a socket to `Idle` — the real kernel does accept connections on SOCK_SEQPACKET, so measuring those handlers reopens every other phase for it. They are still refused because what `epoll_wait` reports is only *inferred* from the two waiters sharing one poll handler, and every other row in this function is measured through both. Take an epoll measurement (an et.c-style probe on an AF_UNIX raw and seqpacket socket) before answering, since answering here makes epoll delivery answer too."
        | SocketPhase.EstablishedPendingReport connectionId
        | SocketPhase.Established connectionId ->
            // With the peer alive and no receive path modelled, both ends
            // are exactly write-ready; once the peer is gone, the level is
            // the measured half-closed one.
            let peerAlive =
                machine.Sockets
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
                $"UnixMachineState.socketReadinessLevel: socket %O{socketId} is in the Darwin-only Dead phase. Both doors into this function refuse the Darwin flavour before any level is computed — `SystemNative_TryChangeSocketEventRegistration` because kqueue is structurally different, and `SystemNative_Poll` because its Darwin rows are measured but unmodelled — so reaching here is an interpreter bug. Darwin polls this phase IN|PRI|HUP (docs/plans/2026-08-23-socket-poll/pollmulti.c) if that changes."

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
        (machine : UnixMachineState)
        : (uint16 * UnixMachineState) option
        =
        let low, high = machine.EphemeralPortRange
        let width = int high - int low + 1

        let rec sweep (remaining : int) (candidate : uint16) =
            if remaining = 0 then
                None
            else

            let next = if candidate = high then low else candidate + 1us

            if isAcceptable candidate then
                Some (
                    candidate,
                    { machine with
                        NextEphemeralPort = next
                    }
                )
            else
                sweep (remaining - 1) next

        // A cursor outside the range can only come from a hand-built machine;
        // start from the bottom rather than sweeping from nowhere.
        let start =
            if machine.NextEphemeralPort < low || machine.NextEphemeralPort > high then
                low
            else
                machine.NextEphemeralPort

        sweep width start

    /// The `somaxconn` sysctl's default on each flavour, measured on the
    /// probe machines (2026-08-21): `net.core.somaxconn` reads 4096 on the
    /// Linux 6.18 container (the machine default since 5.4) and
    /// `kern.ipc.somaxconn` reads 128 on macOS 26.
    let defaultSoMaxConn (flavour : SimulatedUnixFlavour) : int =
        match flavour with
        | SimulatedUnixFlavour.Linux -> 4096
        | SimulatedUnixFlavour.Darwin -> 128

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

    /// Nanoseconds per 100 ns tick. `SystemNative_GetTimestamp` speaks in
    /// nanoseconds while PawPrint's virtual clock speaks in 100 ns ticks, so
    /// the high-resolution timestamp derivation goes through this factor. Every
    /// timestamp the guest observes is therefore a multiple of 100 — `Stopwatch`
    /// has 100 ns granularity here, matching `DateTime`'s quantum, where real
    /// `clock_gettime(CLOCK_MONOTONIC)` is finer still.
    [<Literal>]
    let nanosecondsPerTick : int64 = 100L

    /// 100 ns ticks per millisecond. `VirtualClockTicks` is already denominated
    /// in the same 100 ns unit `System.DateTime` uses, so deriving
    /// `DateTime.UtcNow` scales no part of the clock itself. This factor
    /// converts the quantities that arrive in milliseconds and meet it:
    /// `WallClockEpochMs` into the epoch offset the clock is added to, and a
    /// guest's millisecond timeout into a deadline.
    [<Literal>]
    let ticksPerMillisecond : int64 = 10_000L

    /// Overlay the supplied environment variables on top of the machine's
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
    /// than reading `machine.UnixPlatform`, so that this and the platform
    /// setter cannot become order-dependent; `KernelConfig.applyTo` passes
    /// the same platform to both.
    ///
    /// `None` takes the flavour's measured default. The clamp this feeds
    /// (`connectSocket`'s capacity rule) was measured with the sysctl set to
    /// 3 on Linux and at the default 128 on Darwin, so a configured value is
    /// on measured ground, but it must be positive: no machine was measured
    /// with a non-positive somaxconn.
    let withSoMaxConn
        (platform : SimulatedUnixPlatform)
        (value : int option)
        (machine : UnixMachineState)
        : UnixMachineState
        =
        let resolved =
            match value with
            | None -> defaultSoMaxConn (SimulatedUnixPlatform.flavour platform)
            | Some value ->
                if value < 1 then
                    failwith
                        $"UnixMachineState.SoMaxConn: %d{value} is not positive, and no kernel was measured with a non-positive somaxconn — the accept-queue capacity it would imply is a guess. Configure a positive value, or None for the flavour's default."

                value

        { machine with
            SoMaxConn = resolved
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
    let systemTimeAsTicks (machine : UnixMachineState) : int64 =
        // A machine built by record-copy can bypass `withWallClockEpochMs`, so
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
        if machine.WallClockEpochMs < 0L || machine.WallClockEpochMs > maxWallClockEpochMs then
            failwith
                $"kernel WallClockEpochMs is %d{machine.WallClockEpochMs}, which is outside the range [0, %d{maxWallClockEpochMs}] that System.DateTime can represent"

        if machine.VirtualClockTicks < 0L || machine.VirtualClockTicks > maxWallClockTicks then
            failwith
                $"kernel VirtualClockTicks is %d{machine.VirtualClockTicks}, which is outside the range [0, %d{maxWallClockTicks}] a wall-clock reading can be derived from"

        let ticks =
            machine.WallClockEpochMs * ticksPerMillisecond + machine.VirtualClockTicks

        if ticks > maxWallClockTicks then
            failwith
                $"simulated wall clock has reached %d{ticks} ticks since the Unix epoch, past the %d{maxWallClockTicks} that System.DateTime can represent; lower KernelConfig.WallClockEpochMs"

        ticks

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
    let monotonicTimestampNanos (machine : UnixMachineState) : int64 =
        // The driver loop is the only production writer of `VirtualClockTicks`
        // and only ever advances it from zero, but a machine built by
        // record-copy (as tests do) can bypass that, so re-assert here rather
        // than trusting construction.
        if
            machine.VirtualClockTicks < 0L
            || machine.VirtualClockTicks > maxMonotonicTimestampClockTicks
        then
            failwith
                $"kernel VirtualClockTicks is %d{machine.VirtualClockTicks}, which is outside the range [0, %d{maxMonotonicTimestampClockTicks}] a nanosecond monotonic timestamp can be derived from without overflowing int64"

        machine.VirtualClockTicks * nanosecondsPerTick

    /// The guest-visible `Environment.TickCount64`, in whole milliseconds:
    /// `SystemNative_GetLowResolutionTimestamp`'s reading.
    ///
    /// Upstream the two monotonic entry points (`minipal_lowres_ticks` and
    /// `minipal_hires_ticks`) read the same clock at two resolutions, and the contract a guest
    /// depends on is that they never disagree — so this must be exactly the high-resolution
    /// reading truncated to milliseconds.
    ///
    /// Truncating rather than rounding is faithful: upstream's coarse clock truncates too.
    let lowResolutionTimestampMs (machine : UnixMachineState) : int64 =
        // Lives here beside `monotonicTimestampNanos` and `systemTimeAsTicks` rather than
        // inline in the PAL handler, so that all three projections of the one clock sit
        // together and can be checked against each other without a test having to restate the
        // arithmetic of any of them.
        //
        // Unguarded, unlike its siblings, because dividing a clock already bounded below
        // `Int64.MaxValue` cannot overflow or go negative.
        machine.VirtualClockTicks / ticksPerMillisecond

    /// The moment the emulated machine stamps on an inode it changes now, in the
    /// `struct timespec` an inode's timestamps are kept in.
    ///
    /// The same wall clock `SystemNative_GetSystemTimeAsTicks` reports, so a
    /// guest that writes a file and then reads `DateTime.UtcNow` sees two
    /// readings of one clock rather than two clocks that happen to agree. Its
    /// granularity is therefore the virtual clock's own 100 ns quantum: the
    /// nanosecond part is always a multiple of 100, where a real filesystem
    /// records whatever its machine's clock offers.
    let fileTimestamp (machine : UnixMachineState) : UnixTimestamp =
        let ticks = systemTimeAsTicks machine

        // `systemTimeAsTicks` has established the count is non-negative, so
        // neither the quotient nor the remainder can be, and the nanosecond part
        // lands in `[0, 1e9)` without the floor correction
        // `UnixTimestamp.ofMillisecondsSinceEpoch` needs for a pre-epoch instant.
        let ticksPerSecond = ticksPerMillisecond * 1000L

        UnixTimestamp.createOrFail
            "UnixMachineState.fileTimestamp"
            (ticks / ticksPerSecond)
            (int (ticks % ticksPerSecond) * int nanosecondsPerTick)

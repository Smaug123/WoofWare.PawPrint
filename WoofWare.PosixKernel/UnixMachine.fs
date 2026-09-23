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
        /// `SocketId`, and this is what it names. Every entry has exactly one
        /// description naming it, enforced in two halves: at least one by
        /// `UnixSystem.checkInvariants` (`UnreferencedSocket`), at most one by
        /// `FileDescriptorRegistry.checkInvariants` (`DuplicateSocketId`). A
        /// connection awaiting `accept(2)` is a `TcpConnection` in
        /// `Connections`, not a socket, precisely so this rule can stay
        /// strict.
        Sockets : Map<SocketId, SocketDescription>
        /// Every TCP connection the simulated kernel holds: established ends
        /// referenced from a socket's `SocketPhase`, and completed
        /// connections waiting in some listener's accept queue. An entry is
        /// removed when nothing references it any more (`UnixDescriptor.close`).
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
        /// Time since this machine booted, in nanoseconds: what its monotonic
        /// clocks read. Never negative.
        ///
        /// Nothing in this library moves it. The client decides how fast its
        /// simulated machine runs and when time passes, and says so through
        /// `UnixMachineState.advanceClock`, the only way it changes; between two
        /// advances every clock stands still, so readings taken between them all
        /// name the same instant.
        NanosecondsSinceBoot : int64
        /// What the realtime clock read when this machine booted. The realtime
        /// clock reads this plus `NanosecondsSinceBoot`.
        ///
        /// So the two clocks cannot drift apart, and the realtime clock never
        /// steps or slews on its own as a real one does under NTP or
        /// `settimeofday`. Set by `UnixMachineState.withBootTime`, which says
        /// what it admits.
        BootTime : UnixTimestamp
        /// The kernel's entropy pool, which every random-bytes syscall draws
        /// from: `getrandom(2)` on Linux, `getentropy(2)` on Darwin. Seeded
        /// by `UnixSystem.initial` from `UnixSystem.defaultEntropySeed`.
        EntropyPool : EntropyPool
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
        /// a guest does can change it. Derived from the flavour by
        /// `UnixSystem.initial` and set only by `withFileSystemType`, which
        /// refuses a type this machine's flavour cannot report.
        FileSystemType : EmulatedFileSystemType
    }

/// What a socket is taking an ephemeral port for, which decides what stands
/// in a candidate port's way.
[<RequireQualifiedAccess>]
type EphemeralPortUse =
    /// `bind(2)` with port 0, and the implicit bind `listen(2)` performs on an
    /// unbound socket: the port is reserved outright. Another socket's binding
    /// stands in the way as `bind(2)` decides, and so does an endpoint of any
    /// TCP connection whose socket has closed, because a real kernel keeps a
    /// closing socket in its bind table until its connection is gone.
    | Reserve
    /// The implicit bind `connect(2)` performs on an unbound socket, towards
    /// `destination`. Another socket's binding stands in the way as for
    /// `Reserve`, but a connection does so only when it occupies the
    /// four-tuple to `destination`: a real kernel selects a connect-time port
    /// from its connection table, so a port held towards another destination
    /// is free.
    | ConnectTo of destination : InternetEndpoint

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

    /// Latest `BootTime`, in whole seconds since the Unix epoch, from which the
    /// realtime clock stays inside `time_t` however long the machine stays up: the
    /// largest `time_t`, less the whole seconds in the longest uptime
    /// `NanosecondsSinceBoot` can hold, less one more second that the two
    /// nanosecond parts can carry.
    [<Literal>]
    let maxBootTimeSeconds : int64 = 9223372027631403770L

    let private nanosecondsPerSecond : int64 = 1_000_000_000L

    /// Set what the realtime clock read when this machine booted.
    ///
    /// Refuses an instant before the Unix epoch, since this library models no
    /// realtime clock reading before it, and one after `maxBootTimeSeconds`, from
    /// which the realtime clock could leave `time_t`. On Darwin, also refuses an
    /// instant with a nonzero sub-microsecond part: Darwin keeps the time it
    /// booted as a `struct timeval`, so it has no finer boot instant.
    let withBootTime (bootTime : UnixTimestamp) (machine : UnixMachineState) : UnixMachineState =
        let seconds = UnixTimestamp.seconds bootTime

        if seconds < 0L then
            failwith
                $"UnixMachineState.BootTime: %O{bootTime} is before the Unix epoch, and this kernel does not model a realtime clock reading before it."

        if seconds > maxBootTimeSeconds then
            failwith
                $"UnixMachineState.BootTime: %O{bootTime} is after %d{maxBootTimeSeconds} seconds since the Unix epoch, from which the realtime clock could pass the largest time_t within the longest uptime this kernel represents."

        match SimulatedUnixPlatform.flavour machine.UnixPlatform with
        | SimulatedUnixFlavour.Linux -> ()
        | SimulatedUnixFlavour.Darwin ->
            if UnixTimestamp.nanoseconds bootTime % 1000 <> 0 then
                failwith
                    $"UnixMachineState.BootTime: %O{bootTime} is finer than a microsecond, and Darwin keeps its boot instant as a struct timeval (sysctl kern.boottime), so no Darwin machine booted at it."

        { machine with
            BootTime = bootTime
        }

    /// Let `nanoseconds` pass on this machine: every clock it has moves forward
    /// by that much. Advancing by zero changes nothing.
    ///
    /// Refuses a negative amount, since no clock this kernel models runs
    /// backwards, and one that would take `NanosecondsSinceBoot` past
    /// `Int64.MaxValue` (about 292 years of uptime), beyond which no monotonic
    /// reading can be represented.
    let advanceClock (nanoseconds : int64) (machine : UnixMachineState) : UnixMachineState =
        if nanoseconds < 0L then
            failwith
                $"UnixMachineState.advanceClock: %d{nanoseconds} ns is negative, and every clock this kernel models is monotonic."

        // Reachable only by a record-copy past this function, and checked because
        // the overflow test below is only sound for a non-negative uptime.
        if machine.NanosecondsSinceBoot < 0L then
            failwith
                $"UnixMachineState.advanceClock: the machine has been up for %d{machine.NanosecondsSinceBoot} ns, which is negative. No uptime can be; the machine was assembled without this function."

        if nanoseconds > System.Int64.MaxValue - machine.NanosecondsSinceBoot then
            failwith
                $"UnixMachineState.advanceClock: advancing %d{machine.NanosecondsSinceBoot} ns of uptime by %d{nanoseconds} ns passes %d{System.Int64.MaxValue} ns (about 292 years), the longest uptime this kernel represents."

        { machine with
            NanosecondsSinceBoot = machine.NanosecondsSinceBoot + nanoseconds
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

    /// Set the filesystem type the machine's mount claims to be. `None` takes
    /// the flavour's own default; an explicit type this machine's flavour
    /// could not mount is refused, because `SystemNative_GetFileSystemType`
    /// answers a *file* from the type and every other descriptor from the
    /// flavour, so the pair must describe one machine.
    let withFileSystemType
        (fileSystemType : EmulatedFileSystemType option)
        (machine : UnixMachineState)
        : UnixMachineState
        =
        let flavour = SimulatedUnixPlatform.flavour machine.UnixPlatform

        let resolved =
            match fileSystemType with
            | None -> EmulatedFileSystemType.defaultFor flavour
            | Some requested ->
                if not (EmulatedFileSystemType.isReportableUnder flavour requested) then
                    failwith
                        $"UnixMachineState.FileSystemType: a %O{flavour} kernel cannot report %O{requested}, so a guest asking `fstatfs` would learn a fact no such system could tell it. Leave KernelConfig.FileSystemType as None to take %O{flavour}'s own default, or pick a type that flavour mounts."

                requested

        { machine with
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

    /// Whether any *other* socket's binding conflicts with `candidate`, taken
    /// on behalf of `socket`.
    ///
    /// The relation `bind(2)` decides admission with, `listen(2)` asks again
    /// on the flavour that re-screens an already-bound socket, and every
    /// ephemeral-port choice asks of each port it considers.
    let bindingConflicts
        (socketId : SocketId)
        (socket : SocketDescription)
        (candidate : SocketBinding)
        (machine : UnixMachineState)
        : bool
        =
        machine.Sockets
        |> Map.exists (fun otherId (other : SocketDescription) ->
            if otherId = socketId then
                false
            else

            match other.Binding with
            | None -> false
            | Some existing ->
                // Separate port namespaces per transport, measured: a UDP socket
                // takes a port a listening TCP socket holds.
                other.Kind = socket.Kind
                && SimulatedUnixPlatform.bindConflict
                    machine.UnixPlatform
                    existing
                    other.ReuseAddress
                    other.Phase
                    candidate
                    socket.ReuseAddress
        )

    /// Whether `first` and `second` name a common address: equal, or either
    /// the wildcard.
    let private addressesOverlap (first : InternetEndpoint) (second : InternetEndpoint) : bool =
        first.Address = second.Address
        || first.Address = InternetEndpoint.WildcardAddress
        || second.Address = InternetEndpoint.WildcardAddress

    /// Whether a TCP connection endpoint whose socket has gone still occupies
    /// `endpoint`'s port.
    ///
    /// An endpoint is held while a socket references its connection from
    /// that end: an established socket bound at the endpoint, or a listener
    /// whose accept queue still holds the connection, which owns the server
    /// end until `accept(2)` mints a socket for it.
    let private orphanedConnectionOccupies (endpoint : InternetEndpoint) (machine : UnixMachineState) : bool =
        let heldFrom (connectionId : ConnectionId) (held : InternetEndpoint) (isServerEnd : bool) : bool =
            machine.Sockets
            |> Map.exists (fun _ socket ->
                match socket.Phase with
                | SocketPhase.Established c
                | SocketPhase.EstablishedPendingReport c ->
                    c = connectionId
                    && (socket.Binding |> Option.exists (fun binding -> binding.Endpoint = held))
                | SocketPhase.Listening listenState -> isServerEnd && List.contains connectionId listenState.Queue
                | SocketPhase.Idle
                | SocketPhase.RefusedPendingDelivery
                | SocketPhase.Dead
                | SocketPhase.DatagramPeer _ -> false
            )

        machine.Connections
        |> Map.exists (fun connectionId connection ->
            [ connection.ClientAddress, false ; connection.ServerAddress, true ]
            |> List.exists (fun (held, isServerEnd) ->
                held.Port = endpoint.Port
                && addressesOverlap held endpoint
                && not (heldFrom connectionId held isServerEnd)
            )
        )

    /// Whether a TCP connection occupies the four-tuple between `source` and
    /// `destination`, in either orientation.
    let private connectionOccupiesTuple
        (source : InternetEndpoint)
        (destination : InternetEndpoint)
        (machine : UnixMachineState)
        : bool
        =
        machine.Connections
        |> Map.exists (fun _ connection ->
            (connection.ClientAddress = source && connection.ServerAddress = destination)
            || (connection.ClientAddress = destination && connection.ServerAddress = source)
        )

    /// Hands out the lowest free port at or after the cursor, sweeping the
    /// range once and wrapping, as the binding `candidate` makes of it, on
    /// behalf of `socket`. `purpose` decides what makes a port free.
    ///
    /// `None` when a full sweep finds nothing. The caller decides what to do:
    /// there is no measured answer for an exhausted range, so inventing an
    /// errno here would be a guess.
    let allocateEphemeralPort
        (purpose : EphemeralPortUse)
        (socketId : SocketId)
        (socket : SocketDescription)
        (candidate : uint16 -> SocketBinding)
        (machine : UnixMachineState)
        : (SocketBinding * UnixMachineState) option
        =
        let low, high = machine.EphemeralPortRange
        let width = int high - int low + 1

        let acceptable (port : uint16) : SocketBinding option =
            let binding = candidate port

            if binding.Endpoint.Port <> port then
                failwith
                    $"UnixMachineState.allocateEphemeralPort: the candidate binding for port %d{port} names port %d{binding.Endpoint.Port} instead (this is a bug in this library)."

            let free =
                not (bindingConflicts socketId socket binding machine)
                && (
                    match purpose with
                    | EphemeralPortUse.Reserve ->
                        // Only a stream socket shares a namespace with the
                        // TCP connections.
                        socket.Kind <> SocketKind.Stream
                        || not (orphanedConnectionOccupies binding.Endpoint machine)
                    | EphemeralPortUse.ConnectTo destination ->
                        not (connectionOccupiesTuple binding.Endpoint destination machine)
                )

            if free then Some binding else None

        let rec sweep (remaining : int) (port : uint16) : (SocketBinding * UnixMachineState) option =
            if remaining = 0 then
                None
            else

            let next = if port = high then low else port + 1us

            match acceptable port with
            | Some binding ->
                Some (
                    binding,
                    { machine with
                        NextEphemeralPort = next
                    }
                )
            | None -> sweep (remaining - 1) next

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

    /// Set the `somaxconn` sysctl.
    ///
    /// `None` takes the measured default of this machine's flavour. The clamp
    /// this feeds (`connectSocket`'s capacity rule) was measured with the
    /// sysctl set to 3 on Linux and at the default 128 on Darwin, so a
    /// configured value is on measured ground, but it must be positive: no
    /// machine was measured with a non-positive somaxconn.
    let withSoMaxConn (value : int option) (machine : UnixMachineState) : UnixMachineState =
        let resolved =
            match value with
            | None -> defaultSoMaxConn (SimulatedUnixPlatform.flavour machine.UnixPlatform)
            | Some value ->
                if value < 1 then
                    failwith
                        $"UnixMachineState.SoMaxConn: %d{value} is not positive, and no kernel was measured with a non-positive somaxconn — the accept-queue capacity it would imply is a guess. Configure a positive value, or None for the flavour's default."

                value

        { machine with
            SoMaxConn = resolved
        }

    /// The realtime clock's reading, to the nanosecond: `BootTime` plus
    /// `NanosecondsSinceBoot`.
    ///
    /// This is the instant the kernel stamps on an inode it changes.
    /// `clock_gettime(CLOCK_REALTIME)` reports the same clock, but at the
    /// granularity its flavour reports it at; see `UnixClock.clockGettime`.
    let realtime (machine : UnixMachineState) : UnixTimestamp =
        // Both reachable only by a record-copy past `withBootTime` and
        // `advanceClock`; checked because the carry below is only sound when each
        // operand is inside the range those two admit.
        if machine.NanosecondsSinceBoot < 0L then
            failwith
                $"UnixMachineState.realtime: the machine has been up for %d{machine.NanosecondsSinceBoot} ns, which is negative. No uptime can be; the machine was assembled without advanceClock."

        let bootSeconds = UnixTimestamp.seconds machine.BootTime

        if bootSeconds < 0L || bootSeconds > maxBootTimeSeconds then
            failwith
                $"UnixMachineState.realtime: the machine booted at %O{machine.BootTime}, outside the [0, %d{maxBootTimeSeconds}] seconds withBootTime admits; the machine was assembled without it."

        let nanoseconds =
            int64 (UnixTimestamp.nanoseconds machine.BootTime)
            + machine.NanosecondsSinceBoot % nanosecondsPerSecond

        let carry = nanoseconds / nanosecondsPerSecond

        UnixTimestamp.createOrFail
            "UnixMachineState.realtime"
            (bootSeconds + machine.NanosecondsSinceBoot / nanosecondsPerSecond + carry)
            (int (nanoseconds % nanosecondsPerSecond))

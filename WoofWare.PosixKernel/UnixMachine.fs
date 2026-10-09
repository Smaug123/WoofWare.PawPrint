namespace WoofWare.PosixKernel

open System.Collections.Immutable

/// The kernel-image facts a POSIX simulator owns: the platform it is
/// impersonating, its filesystem, its clock and entropy, its network
/// configuration, its open file descriptions and its socket and pipe tables,
/// what clients draining its pipes have received, and the two numbers a process
/// reads back about the machine it is running on.
///
/// Everything here is state any client of a POSIX simulator would have.
type UnixMachineState =
    internal
        {
            /// Every open file description on the machine, whichever process's
            /// descriptors name it, with the state an epoll instance or a kqueue
            /// keeps in its description. A process's descriptor table
            /// (`UnixProcessState.FileDescriptors`) holds only which of these
            /// each descriptor names.
            OpenFiles : OpenFileTable
            /// Every socket on the machine, by identity.
            ///
            /// Separate from `OpenFiles` because a socket's lifetime is not
            /// a description's: an `OpenFileTarget.Socket` holds only the
            /// `SocketId`, and this is what it names. Every entry has exactly one
            /// description naming it, enforced in two halves: at least one by
            /// `UnixSystem.checkInvariants` (`UnreferencedSocket`), at most one by
            /// `OpenFileTable.checkInvariants` (`DuplicateSocketId`). A
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
            /// The ordinal the next committed event registration records
            /// as its `RegisteredAt`, whether an epoll instance's
            /// (`EpollRegistration`) or a kqueue's (`KqueueRegistration`).
            /// Monotonic, and bumped only when an `EPOLL_CTL_ADD` or the first
            /// `EV_ADD` of a kqueue registration commits, so a failed `epoll_ctl`
            /// leaves the kernel exactly as it found it.
            NextEventRegistrationOrdinal : int64
            /// The kqueue of every Darwin `poll(2)` asleep on the machine, in
            /// whichever process: see `PollQueue`. Made as the call goes to
            /// sleep, and destroyed with the park that names it.
            PollQueues : Map<PollQueueId, PollQueue>
            /// The identity the next sleeping Darwin `poll` gives its kqueue.
            /// Monotonic and never reused, for the replay-trace reason
            /// `NextSocketId` gives.
            NextPollQueueId : PollQueueId
            /// The ordinal the next park of any task records as its
            /// `TaskPark.Ordinal`. Monotonic, and bumped only by `UnixWait.park`.
            ///
            /// The machine's rather than a process's, because what a kernel orders
            /// by park is a wait queue on a kernel object, and processes can share
            /// one.
            NextParkOrdinal : ParkOrdinal
            /// Where the next thread's id comes from, and which ids live tasks
            /// hold: see `ThreadIdAllocator`.
            ///
            /// The machine's rather than a process's, because on both flavours the
            /// counter is shared by every process on the machine, and an id one
            /// process's task holds is not handed to another's. Set by
            /// `UnixSystem.initial`, `UnixBootImage.withProcessId`,
            /// `UnixBootImage.withLeaderThreadId` and `UnixSystem.writePidMaxSysctl`;
            /// advanced only by `UnixTaskLifecycle.spawn`, and an id is freed by its
            /// task's exit (`UnixTaskLifecycle.exitThread`, and the end of the
            /// process). `UnixSystem.checkInvariants` holds the live ids to the
            /// tasks'.
            ThreadIds : ThreadIdAllocator
            /// Where the next process's ID comes from, and the ID of every
            /// process on the machine: see `ProcessIdTable`. A process joins it
            /// when it is launched (`UnixBootImage.boot`, `SimulatedMachine.launch`)
            /// and leaves it when it ends.
            ProcessIds : ProcessIdTable
            /// For each directory some process stands in, how many processes
            /// do: the reference a current directory holds on its inode.
            ///
            /// The machine's rather than read off each process, because a
            /// directory one process stands in must outlive a removal by
            /// another, and a process's view cannot see the other processes. A
            /// launch takes a hold on the process's starting directory, `chdir`
            /// moves one, and the process's end lets its own go. An inode no
            /// process stands in has no entry.
            CurrentDirectories : Map<InodeNumber, int>
            /// The port a `bind(2)` of port 0 will try first.
            ///
            /// A counter rather than a draw from the seeded PRNG. Which port an
            /// ephemeral bind picks is unspecified — Linux randomises within its
            /// range and Darwin ascends — so this kernel owes a process only *a* free
            /// port, and a trace whose ports read 32768, 32769, 32770 is far easier
            /// to follow than one whose ports are scattered. A program may depend on
            /// nothing about the value but that it is non-zero and unprivileged,
            /// which is all the two real kernels agree on.
            NextEphemeralPort : uint16
            /// Range `NextEphemeralPort` sweeps, inclusive at both ends. Host
            /// configuration; see `UnixSystem.defaultEphemeralPortRange`.
            EphemeralPortRange : uint16 * uint16
            /// The value of the `somaxconn` sysctl (`net.core.somaxconn` on
            /// Linux, `kern.ipc.somaxconn` on Darwin): the ceiling `listen(2)`
            /// clamps its backlog to before the accept-queue capacity is derived.
            /// Host configuration with a per-flavour default; see
            /// `UnixBootImage.withSoMaxConn` for the measured clamp rules.
            SoMaxConn : int
            /// The send buffer a new TCP socket starts with, in bytes: Darwin's
            /// `net.inet.tcp.sendspace` sysctl, and Linux's `net.ipv4.tcp_wmem`
            /// default. Host configuration with a per-flavour default; see
            /// `UnixBootImage.withTcpSendSpace`. Only the Darwin flavour reads
            /// it, through `TcpBufferSizing`.
            TcpSendSpace : int
            /// The `IPV6_V6ONLY` a new IPv6 socket starts with: Linux's
            /// `net.ipv6.bindv6only` sysctl, Darwin's `net.inet6.ip6.v6only`.
            /// Host configuration, off by default on both (measured); see
            /// `UnixBootImage.withIpv6OnlyByDefault`.
            Ipv6OnlyByDefault : bool
            /// The receive buffer a new TCP socket starts with, in bytes:
            /// Darwin's `net.inet.tcp.recvspace` sysctl, and Linux's
            /// `net.ipv4.tcp_rmem` default (its second value). Host
            /// configuration with a per-flavour default; see
            /// `UnixBootImage.withTcpReceiveSpace`. Both flavours read it,
            /// through `TcpBufferSizing`.
            TcpReceiveSpace : int
            /// The most a TCP socket's send buffer grows to, in bytes: Linux's
            /// `net.ipv4.tcp_wmem` maximum (its third value), which a
            /// connection's send buffer autotunes up to, and Darwin's
            /// `net.inet.tcp.autosndbufmax`. Host configuration with a
            /// per-flavour default; see `UnixBootImage.withTcpSendSpaceMax`.
            /// Only the Linux flavour reads it, through `TcpBufferSizing`.
            TcpSendSpaceMax : int
            /// The IPv4 addresses this machine holds. Host configuration; see
            /// `UnixSystem.defaultLocalAddresses`.
            LocalAddresses : uint32 list
            /// Prefixes this machine has a local route to, which Linux will bind any
            /// address inside and Darwin ignores. See
            /// `UnixSystem.defaultLocalRoutes`.
            LocalRoutes : Ipv4Prefix list
            /// Every pipe with an end open, by identity: an end an open file
            /// description names, or one the client holds (`PipeState.heldByClient`).
            ///
            /// Separate from `OpenFiles` for the reason `Sockets` is: an
            /// `OpenFileTarget.Pipe` holds only the `PipeId`, and both ends' descriptions
            /// name the one pipe. A pipe is in the table exactly while one of its
            /// ends is open (`UnixSystem.checkInvariants` states both halves), and
            /// `UnixDescriptor.close` removes it with the last one.
            Pipes : Map<PipeId, PipeState>
            /// The identity the next pipe will be given, by `UnixPipe.pipe2`.
            /// Monotonic and never reused, for the replay-trace reason
            /// `NextSocketId` gives.
            NextPipeId : PipeId
            /// Every write that reached a client draining a pipe, oldest first: the
            /// bytes the outside world has received from the machine's processes.
            ///
            /// A client reads what its own ends received by filtering on
            /// `Delivery.Endpoint`, which names the process the pipe was launched
            /// into as well as the descriptor. One log rather than one per endpoint, so that
            /// the order of writes across endpoints is kept: a process writing to
            /// its output, then its error stream, then its output again is read
            /// back in that order. It grows without bound: a process that writes
            /// gigabytes costs that much memory.
            Delivered : DeliveryLog
            /// The inode number the next pipe end will be given, as `fstat(2)`
            /// reports it. Monotonic and never reused: a process can compare the
            /// numbers two descriptors report to decide whether they name one pipe,
            /// so a reused number would make a dead pipe and a live one look the
            /// same.
            NextPipeInode : InodeNumber
            /// The `st_dev` every end of every pipe reports.
            ///
            /// On Linux this is the anonymous device the kernel gave its pipe
            /// filesystem at boot, which depends on what else was mounted before
            /// it: configuration of this machine rather than a fact of the kernel.
            /// On Darwin it is 0 for every pipe on every machine (measured, 27.0.0).
            /// See `UnixBootImage.withPipeDevice`.
            PipeDevice : int64
            /// The identity the next `UnixSocket.socket` will allocate.
            ///
            /// Monotonic, and never reused: no syscall reports a
            /// `SocketId`, but a replay trace does, and reuse would make two
            /// distinct sockets indistinguishable in it.
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
            /// `settimeofday`. Set by `UnixBootImage.withBootTime`, which says
            /// what it admits.
            BootTime : UnixTimestamp
            /// The kernel's entropy pool, which every random-bytes syscall draws
            /// from: `getrandom(2)` on Linux, `getentropy(2)` on Darwin. Seeded
            /// by `UnixSystem.initial` from `UnixSystem.defaultEntropySeed`, or by
            /// `UnixBootImage.withEntropySeed` from another.
            EntropyPool : EntropyPool
            /// Number of logical processors the machine reports to the simulated
            /// process. Deliberately a value in kernel state rather than a host
            /// read: a host read would make a replay depend on the machine that
            /// produced it. Programs size thread pools, partition work, and stripe
            /// arrays off this number, so letting the host leak in here would change
            /// their *control flow* between runs — the single worst kind of
            /// nondeterminism for a simulation whose purpose is bit-for-bit replay.
            ///
            /// Defaults to `UnixSystem.defaultProcessorCount`; a client chooses a
            /// different value with `UnixBootImage.withProcessorCount`, which
            /// refuses anything below 1, since programs divide by it.
            ProcessorCount : int
            /// Whether this machine's kernel screens a read or write buffer before
            /// it performs the operation, and if so the greatest value
            /// `address + length` may take: the machine's `TASK_SIZE_MAX`.
            ///
            /// Whether it screens is the platform's
            /// `SimulatedUnixPlatform.screensUserBufferUpFront`. The limit is
            /// configuration rather than a constant derived from the platform,
            /// because it varies by *machine* as well as by architecture: 2^47 less
            /// a page with four-level paging on x86-64, 2^56 less a page with
            /// five-level, 2^48 on a 48-bit-VA arm64. Two GitHub runners of the same
            /// image were measured disagreeing, so no value derived from the kernel
            /// could be right everywhere.
            ///
            /// `UnixSystem.initial` sets the platform's default, and
            /// `UnixBootImage.withUserAddressLimit` another limit the platform's
            /// architecture has. `UnixSystem.checkInvariants` reports a check that
            /// disagrees with the platform (`UnixSystemDefect.UserBufferCheckNotOfPlatform`).
            UserBufferCheck : UserBufferCheck
            /// Unix-shaped platform identity the simulated process reports, as
            /// observed through `uname(2)`, and the flavour every other
            /// platform-dependent answer follows.
            ///
            /// Fixed for the whole run: `UnixSystem.initial` takes it and nothing
            /// changes it afterwards, so a process cannot observe it changing
            /// under it.
            UnixPlatform : SimulatedUnixPlatform
            /// The simulated process's filesystem: every inode a process can reach
            /// through the path syscalls.
            ///
            /// Set by `UnixBootImage.withFileSystem`, and changed by
            /// the syscalls that write, create or truncate. It is emulated kernel
            /// state rather than anything read from the host, for the usual reason:
            /// a filesystem read from the host would make a replay depend on the
            /// machine that produced it, and programs branch on what they find.
            FileSystem : VirtualFileSystem
            /// The mount `FileSystem`'s root filesystem claims to be: its type and
            /// what `statfs(2)` reports about it (see `FileSystemStatistics.ofMount`). Its type also
            /// decides a directory's `st_size`, and where `lseek(2)` with
            /// `SEEK_END` lands on a directory.
            ///
            /// Fixed for the run: this library models no `mount(2)`, so nothing
            /// a process does can change it. Derived from the flavour by
            /// `UnixSystem.initial` and set only by `withMount`, which refuses a
            /// type this machine's flavour cannot report.
            Mount : EmulatedMount
            /// The filesystem mounted at `/dev`, which the kernel mounts at boot
            /// over the root's entry `dev`. What `stat(2)` and `statfs(2)` report
            /// about it and the device nodes on it comes from here.
            ///
            /// Fixed for the run, and derived from the flavour by
            /// `UnixSystem.initial`.
            DeviceMount : DeviceFileSystemMount
            /// Linux's `fs.protected_symlinks`, `fs.protected_regular` and
            /// `fs.protected_fifos` sysctls.
            ///
            /// `UnixSystem.initial` sets `ProtectedFiles.off`, the kernel's own
            /// default, and `UnixBootImage.withProtectedFiles` any other. Only
            /// `ProtectedFiles.off` is admitted on Darwin, which has no such
            /// settings (`UnixSystemDefect.ProtectedFilesNotOfFlavour`).
            ProtectedFiles : ProtectedFiles
        }

/// What a socket is taking an ephemeral port for, which decides what stands
/// in a candidate port's way.
[<RequireQualifiedAccess>]
type internal EphemeralPortUse =
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

    /// Whether `check` is one a machine on `platform` can have: an up-front
    /// screen exactly where the platform screens, at a limit that machines of the
    /// platform's architecture have been observed to have.
    let isUserBufferCheckOf (platform : SimulatedUnixPlatform) (check : UserBufferCheck) : bool =
        match check with
        | UserBufferCheck.AtCopyTime -> not (SimulatedUnixPlatform.screensUserBufferUpFront platform)
        | UserBufferCheck.BeforeOperation limit ->
            SimulatedUnixPlatform.screensUserBufferUpFront platform
            && ObservedUserAddressLimit.architectureOf limit = Some (SimulatedUnixPlatform.architecture platform)

    /// Latest `BootTime`, in whole seconds since the Unix epoch, from which the
    /// realtime clock stays inside `time_t` however long the machine stays up: the
    /// largest `time_t`, less the whole seconds in the longest uptime
    /// `NanosecondsSinceBoot` can hold, less one more second that the two
    /// nanosecond parts can carry.
    [<Literal>]
    let maxBootTimeSeconds : int64 = 9223372027631403770L

    let private nanosecondsPerSecond : int64 = 1_000_000_000L

    /// Let `nanoseconds` pass on this machine: every clock it has moves forward
    /// by that much. Advancing by zero changes nothing.
    ///
    /// Refuses a negative amount, since no clock this kernel models runs
    /// backwards, and one that would take `NanosecondsSinceBoot` past
    /// `Int64.MaxValue` (about 292 years of uptime), beyond which no monotonic
    /// reading can be represented.
    let internal advanceClock (nanoseconds : int64) (machine : UnixMachineState) : UnixMachineState =
        if nanoseconds < 0L then
            failwith
                $"UnixMachineState.advanceClock: %d{nanoseconds} ns is negative, and every clock this kernel models is monotonic."

        // Unreachable through the public API, which builds no machine but through
        // this function and `UnixSystem.initial`; checked because
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

    /// How long this machine has been up, to the nanosecond.
    ///
    /// The client that decides when time passes reads this through
    /// `UnixSystem.nanosecondsSinceBoot`, to know how far to `advanceClock`. A
    /// process reads the same instant through `UnixClock.clockGettime`, at the
    /// granularity its flavour reports.
    let internal nanosecondsSinceBoot (machine : UnixMachineState) : int64 = machine.NanosecondsSinceBoot

    /// The type of filesystem `inode` is on: the root filesystem's mount, or,
    /// for an inode on the device filesystem, tmpfs, which a Linux devtmpfs
    /// is underneath (measured: its `f_type`, and its directories' sizes,
    /// follow tmpfs's rules).
    let internal fileSystemTypeOf (inode : InodeNumber) (machine : UnixMachineState) : EmulatedFileSystemType =
        match VirtualFileSystem.mountedRootOf inode machine.FileSystem, machine.DeviceMount with
        | None, _ -> EmulatedMount.fileSystemType machine.Mount
        | Some _, DeviceFileSystemMount.Devtmpfs _ -> EmulatedFileSystemType.Tmpfs
        | Some root, DeviceFileSystemMount.Devfs ->
            failwith
                $"UnixMachineState.fileSystemTypeOf: inode %O{inode} is on the filesystem mounted at inode %O{root}, which is Darwin's devfs; no path or descriptor can reach it (this is a bug in this library)."

    /// Whether `protection` is something a machine of `flavour` can be
    /// configured with: anything on Linux, and on Darwin, which has none of
    /// these sysctls, only `ProtectedFiles.off`.
    let isProtectedFilesOf (flavour : SimulatedUnixFlavour) (protection : ProtectedFiles) : bool =
        match flavour with
        | SimulatedUnixFlavour.Linux -> true
        | SimulatedUnixFlavour.Darwin -> protection = ProtectedFiles.off

    /// Whether, and where, this machine's kernel screens a read or write buffer
    /// before performing the operation. See `UnixMachineState.UserBufferCheck`.
    let internal userBufferCheck (machine : UnixMachineState) : UserBufferCheck = machine.UserBufferCheck

    /// The platform this machine impersonates, which `UnixSystem.initial` fixed
    /// for the machine's life.
    ///
    /// A process knows its platform by having been built for it, and learns the
    /// kernel's release through `uname(2)`. A client reads it through
    /// `UnixSystem.platform`, to speak to the kernel in the platform's own
    /// numbering.
    let internal platform (machine : UnixMachineState) : SimulatedUnixPlatform = machine.UnixPlatform

    /// The number of logical processors this machine reports to a process. See
    /// `UnixMachineState.ProcessorCount`.
    let internal processorCount (machine : UnixMachineState) : int = machine.ProcessorCount

    /// Every write that has reached a client draining one of this machine's
    /// pipes, oldest first: what the outside world has received from the
    /// process. See `UnixMachineState.Delivered`.
    ///
    /// The client that drains those pipes reads this through
    /// `UnixSystem.delivered`. No process can read this back, because the bytes
    /// have left the machine.
    let internal delivered (machine : UnixMachineState) : DeliveryLog = machine.Delivered

    /// The socket `socketId` names.
    ///
    /// Total, and loudly partial rather than an option: every `SocketId` a
    /// caller can hold came out of an `OpenFileTarget.Socket`, and
    /// `checkInvariants` rejects a machine in which one of those names nothing.
    /// A `None` here would push that impossible case onto every call site.
    let internal socket (socketId : SocketId) (machine : UnixMachineState) : SocketDescription =
        match Map.tryFind socketId machine.Sockets with
        | Some socket -> socket
        | None ->
            failwith
                $"UnixMachineState.socket: %O{socketId} names no socket in this kernel's socket table. Every SocketId reachable by a caller comes from an open file description, and UnixSystemDefect.DanglingSocket exists to make that unreachable, so the system breaks UnixSystem.checkInvariants: this is a bug in this library, or in a caller that assembled the state by hand, rather than anything the simulated process did."

    /// A new kqueue for a Darwin `poll` about to sleep, holding `queue`: its
    /// identity, and the machine holding it.
    let internal addPollQueue (queue : PollQueue) (machine : UnixMachineState) : PollQueueId * UnixMachineState =
        let (PollQueueId next) = machine.NextPollQueueId
        let id = PollQueueId next

        id,
        { machine with
            PollQueues = Map.add id queue machine.PollQueues
            NextPollQueueId = PollQueueId (next + 1L)
        }

    /// The kqueue `queue` of a sleeping Darwin `poll`. Loudly partial: the
    /// caller read the identity from a park, which holds it.
    let internal pollQueue (queue : PollQueueId) (machine : UnixMachineState) : PollQueue =
        match Map.tryFind queue machine.PollQueues with
        | Some found -> found
        | None ->
            failwith
                $"UnixMachineState.pollQueue: %O{queue} is not on the machine, but a park names it, and a park holds its poll's kqueue until it ends (this is a bug in this library, or in a caller that assembled the state by hand)."

    /// The machine with the sleeping Darwin poll's kqueue `queue` replaced by
    /// `state`. Loudly partial, as `pollQueue` is.
    let internal setPollQueue
        (queue : PollQueueId)
        (state : PollQueue)
        (machine : UnixMachineState)
        : UnixMachineState
        =
        if not (Map.containsKey queue machine.PollQueues) then
            failwith $"UnixMachineState.setPollQueue: %O{queue} is not on the machine (this is a bug in this library)."

        { machine with
            PollQueues = Map.add queue state machine.PollQueues
        }

    /// Every live open file description on the machine naming `socketId`.
    let internal descriptionsNamingSocket
        (socketId : SocketId)
        (machine : UnixMachineState)
        : Set<OpenFileDescriptionId>
        =
        OpenFileTable.toSeq machine.OpenFiles
        |> Seq.choose (fun (descriptionId, description) ->
            match description.Target with
            | OpenFileTarget.Socket target when target = socketId -> Some descriptionId
            | _ -> None
        )
        |> Set.ofSeq

    /// Every live open file description on the machine naming `pipeEnd` of
    /// `pipeId`.
    let internal descriptionsNamingPipeEnd
        (pipeId : PipeId)
        (pipeEnd : PipeEnd)
        (machine : UnixMachineState)
        : Set<OpenFileDescriptionId>
        =
        OpenFileTable.toSeq machine.OpenFiles
        |> Seq.choose (fun (descriptionId, description) ->
            if description.Target = OpenFileTarget.Pipe (pipeId, pipeEnd) then
                Some descriptionId
            else
                None
        )
        |> Set.ofSeq

    /// Whether `pipeEnd` of the pipe `pipeId`, which is `pipe`, is still open:
    /// whether some open file description on the machine names it, or the
    /// client holds it (`PipeState.heldByClient`).
    ///
    /// Derived rather than stored, so it cannot disagree with the table: the
    /// end closes when the last description onto it goes, which is when its
    /// last descriptor closes, or when a call that held it returns after that,
    /// unless the client holds it; and `dup` keeps it open.
    let internal pipeEndOpen
        (pipeId : PipeId)
        (pipe : PipeState)
        (pipeEnd : PipeEnd)
        (machine : UnixMachineState)
        : bool
        =
        PipeState.heldByClient pipeEnd pipe
        || OpenFileTable.toSeq machine.OpenFiles
           |> Seq.exists (fun (_, description) -> description.Target = OpenFileTarget.Pipe (pipeId, pipeEnd))

    /// Every inode something on the machine holds a reference to *directly*,
    /// independently of any name the filesystem binds to it: each open file
    /// description onto a file, a directory or a device's node, and each
    /// directory some process stands in (`CurrentDirectories`).
    ///
    /// A real kernel keeps an inode alive while any reference survives. Every
    /// kind of reference a process can *create* must appear here: an omission
    /// makes a live inode look free, and freeing it leaves a descriptor, or a
    /// process standing in it, pointing at nothing. It is not what callers
    /// want, though — see `ObjectLifetime.pinnedInodes`, which adds those the
    /// *filesystem* holds on behalf of these.
    let internal heldInodes (machine : UnixMachineState) : Set<InodeNumber> =
        OpenFileTable.toSeq machine.OpenFiles
        |> Seq.choose (fun (_, description) ->
            match description.Target with
            | OpenFileTarget.File (inode, _)
            | OpenFileTarget.Directory (inode, _)
            | OpenFileTarget.CharacterDevice (inode, _) -> Some inode
            | OpenFileTarget.Socket _
            | OpenFileTarget.Kqueue _
            | OpenFileTarget.Epoll _
            | OpenFileTarget.Pipe _ -> None
        )
        |> Set.ofSeq
        |> Set.union (machine.CurrentDirectories |> Map.keys |> Set.ofSeq)

    /// `machine` with one more process standing in `inode`.
    let internal holdCurrentDirectory (inode : InodeNumber) (machine : UnixMachineState) : UnixMachineState =
        let count = Map.tryFind inode machine.CurrentDirectories |> Option.defaultValue 0

        { machine with
            CurrentDirectories = Map.add inode (count + 1) machine.CurrentDirectories
        }

    /// `machine` with one process fewer standing in `inode`. Loudly partial on
    /// an inode no process stands in.
    let internal releaseCurrentDirectory (inode : InodeNumber) (machine : UnixMachineState) : UnixMachineState =
        match Map.tryFind inode machine.CurrentDirectories with
        | None ->
            failwith
                $"UnixMachineState.releaseCurrentDirectory: no process stands in inode %O{inode} (this is a bug in this library)."
        | Some 1 ->
            { machine with
                CurrentDirectories = Map.remove inode machine.CurrentDirectories
            }
        | Some count ->
            { machine with
                CurrentDirectories = Map.add inode (count - 1) machine.CurrentDirectories
            }

    /// The pipe `pipeId` names.
    ///
    /// Loudly partial rather than an option, as `socket` is: every `PipeId` a
    /// caller can hold came out of an `OpenFileTarget.Pipe`, and
    /// `UnixSystem.checkInvariants` rejects a machine in which one of those
    /// names nothing.
    let internal pipe (pipeId : PipeId) (machine : UnixMachineState) : PipeState =
        match Map.tryFind pipeId machine.Pipes with
        | Some pipe -> pipe
        | None ->
            failwith
                $"UnixMachineState.pipe: %O{pipeId} names no pipe in this kernel's pipe table. Every PipeId reachable by a caller comes from an open file description, and UnixSystemDefect.DanglingPipe exists to make that unreachable, so the system breaks UnixSystem.checkInvariants: this is a bug in this library, or in a caller that assembled the state by hand."

    /// The `st_dev` a pipe reports on a machine of `flavour` that has not
    /// configured one.
    ///
    /// On Linux, 0xc: the pipe filesystem's device on the Linux 6.18.5 machine
    /// its pipes were measured on. Any other small number would do as well,
    /// since each boot numbers it afresh. On Darwin, 0, which is what every
    /// Darwin pipe reports.
    let defaultPipeDevice (flavour : SimulatedUnixFlavour) : int64 =
        match flavour with
        | SimulatedUnixFlavour.Linux -> 0xcL
        | SimulatedUnixFlavour.Darwin -> 0L

    /// The connection `connectionId` names.
    ///
    /// Total, and loudly partial rather than an option: every `ConnectionId` a
    /// caller can hold came out of a socket phase or an accept queue, and
    /// `checkInvariants` rejects a machine in which one of those dangles.
    let internal connection (connectionId : ConnectionId) (machine : UnixMachineState) : TcpConnection =
        match Map.tryFind connectionId machine.Connections with
        | Some connection -> connection
        | None ->
            failwith
                $"UnixMachineState.connection: %O{connectionId} names no connection in this kernel's connection table. UnixSystemDefect.DanglingConnection and DanglingQueuedConnection exist to make this unreachable, so the system breaks UnixSystem.checkInvariants: this is a bug in this library, or in a caller that assembled the state by hand."

    /// `connectionId`'s entry in the connection table, with the bytes and end
    /// states it carries replaced by `transfer`. Loudly partial, as
    /// `connection` is.
    let internal withTransfer
        (connectionId : ConnectionId)
        (transfer : TcpTransfer)
        (machine : UnixMachineState)
        : UnixMachineState
        =
        let existing = connection connectionId machine

        { machine with
            Connections =
                Map.add
                    connectionId
                    { existing with
                        Transfer = transfer
                    }
                    machine.Connections
        }

    /// The socket that is the `connectionEnd` end of `connectionId`
    /// (`SocketPhase.connectionEnd`), if one is: none for a server end still
    /// in a listener's accept queue, nor for an end whose socket has closed.
    let internal socketHoldingEnd
        (connectionId : ConnectionId)
        (connectionEnd : ConnectionEnd)
        (machine : UnixMachineState)
        : SocketId option
        =
        machine.Sockets
        |> Map.tryFindKey (fun _ socket -> SocketPhase.connectionEnd socket.Phase = Some (connectionId, connectionEnd))

    /// The readiness a socket presents right now, before any waiter's interest
    /// mask is applied. Every row is measured on Linux 6.18.5 — `masks.c`
    /// (docs/plans/2026-08-21-socket-readiness-wake) through level-triggered
    /// `epoll_wait` with timeout 0, and `pollmask.c`
    /// (docs/plans/2026-08-23-socket-poll) through `poll(2)` with timeout 0,
    /// which agree on every phase, and `consumed-epoll.c` and `soerror.c`
    /// (docs/probes/so-error) for a refusal an `SO_ERROR` read has taken.
    ///
    /// A connected TCP socket's level is read off its connection's bytes and
    /// end states (`TcpTransfer`), so on Linux alone: it uses Linux's
    /// writability rule.
    ///
    /// Darwin has no measured rows and needs none: both waiters refuse that
    /// flavour before reaching here — epoll, which Darwin does not have, and
    /// `UnixPoll.poll` — and Darwin's kqueue reads its own filters'
    /// readiness (`DarwinReadiness`), not this.
    ///
    /// Defined only for a socket `LinuxReadiness.modelsSocket` accepts, and
    /// failing for a `SOCK_SEQPACKET` one, whose epoll level is unmeasured.
    let internal socketReadinessLevel (socketId : SocketId) (machine : UnixMachineState) : ReadinessLevel =
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
            | SocketKind.SeqPacket ->
                // On Linux `poll(2)` reports OUT|HUP|WRNORM|WRBAND for a
                // fresh SOCK_SEQPACKET (docs/plans/2026-08-23-socket-poll/pollgaps.c,
                // and docs/plans/2026-08-23-posix-kernel-extraction/poll-alphabet.c
                // for the WRNORM and WRBAND bits). That row is the whole answer
                // only while `listen`, `connect` and `accept` keep refusing the
                // kind (their `UnmeasuredKind` refusals), which is what confines
                // such a socket to `Idle`. What `epoll_wait` reports would only
                // be *inferred* from the two waiters sharing one poll handler,
                // and every other row here is measured through both; answering
                // here makes epoll delivery answer too. So both waiters refuse
                // the kind first (`PollRefusal.UnmeasuredSocketKind`,
                // `EpollCtlRefusal.UnmeasuredSocketKind`).
                failwith
                    $"UnixMachineState.socketReadinessLevel: socket %O{socketId} is %O{target.Kind}, whose readiness is measured for poll but not for epoll, so it is not modelled. `UnixPoll.poll` and `UnixPoll.epollCtl` both refuse such a socket (LinuxReadiness.modelsSocket) before asking, so this is a bug in this library, or in a caller that asked about a socket LinuxReadiness.modelsSocket rejects. Take an epoll measurement (an et.c-style probe on an AF_UNIX seqpacket socket) before modelling the kind."
        | SocketPhase.EstablishedPendingReport _
        | SocketPhase.Established _ ->
            let connectionId, connectionEnd =
                match SocketPhase.connectionEnd target.Phase with
                | Some held -> held
                | None ->
                    failwith
                        $"UnixMachineState.socketReadinessLevel: socket %O{socketId} is in %A{target.Phase}, which holds no connection end (this is a bug in this library)."

            let transfer = (connection connectionId machine).Transfer
            let inbound = TcpTransfer.towards connectionEnd transfer
            let unread = ByteQueue.length inbound.Receiving > 0

            // `tcp_poll`, every row measured (`tcp-transfer.c`, section S, on
            // Linux 6.18.5): IN with bytes unread, OUT while the send buffer
            // is at most two thirds full, RDHUP once the peer's FIN has
            // arrived (`order3.c` row Q), and a reset adds HUP, and ERR while
            // its error is pending, and makes the socket writable whatever its
            // buffer holds, since a write then fails at once.
            match inbound.Receiver with
            | TcpEndState.Open
            | TcpEndState.FinQueued ->
                { ReadinessLevel.none with
                    In = unread
                    Out = TcpTransfer.linuxSendable connectionEnd transfer
                }
            | TcpEndState.FinReceived ->
                { ReadinessLevel.none with
                    In = true
                    Out = TcpTransfer.linuxSendable connectionEnd transfer
                    RdHup = true
                }
            | TcpEndState.Reset (_, errorPending) ->
                {
                    In = true
                    Out = true
                    RdHup = true
                    Hup = true
                    Err = errorPending
                }
            | TcpEndState.Closed ->
                failwith
                    $"UnixMachineState.socketReadinessLevel: socket %O{socketId} is the %A{connectionEnd} end of %O{connectionId}, which the connection records as closed (this is a bug in this library: UnixSystem.checkInvariants reports it as ConnectionEndClosedUnderSocket)."

        | SocketPhase.Refused RefusalError.Pending ->
            {
                In = true
                Out = true
                RdHup = true
                Hup = true
                Err = true
            }
        | SocketPhase.Refused RefusalError.Reported ->
            // An `SO_ERROR` read took the refusal: everything the pending
            // level holds but ERR (`consumed-epoll.c` R1).
            {
                In = true
                Out = true
                RdHup = true
                Hup = true
                Err = false
            }

    /// Whether a reset has reached `connection`, which then no longer
    /// occupies its four-tuple: measured on both (`reset-tuple.c` in
    /// docs/plans/2026-10-07-tcp-byte-transfer), a fresh socket bound to the
    /// surviving end's address connects to the same destination once a reset
    /// has reached it, though not after a FIN.
    let internal resetReleasedTuple (connection : TcpConnection) : bool =
        [ ConnectionEnd.Client ; ConnectionEnd.Server ]
        |> List.exists (fun connectionEnd ->
            match (TcpTransfer.towards connectionEnd connection.Transfer).Receiver with
            | TcpEndState.Reset _ -> true
            | TcpEndState.Open
            | TcpEndState.FinQueued
            | TcpEndState.FinReceived
            | TcpEndState.Closed -> false
        )

    /// Whether the connected socket `socket`'s binding stopped reserving its
    /// port when its connection was reset: on Darwin always, and on Linux
    /// unless the port was bound explicitly (`SocketBinding.LockedPort`).
    /// Measured on both (`reset-binding.c` in
    /// docs/plans/2026-10-07-tcp-byte-transfer): a fresh socket binds the
    /// survivor's exact endpoint once a reset has reached it, at either end,
    /// though `getsockname` still reports the port; after a FIN it cannot.
    let internal resetReleasedPort (socket : SocketDescription) (machine : UnixMachineState) : bool =
        match SocketPhase.connectionEnd socket.Phase with
        | None -> false
        | Some (connectionId, connectionEnd) ->
            match (TcpTransfer.towards connectionEnd (connection connectionId machine).Transfer).Receiver with
            | TcpEndState.Reset _ ->
                match SimulatedUnixPlatform.flavour machine.UnixPlatform, socket.Binding with
                | SimulatedUnixFlavour.Darwin, _ -> true
                | SimulatedUnixFlavour.Linux, Some binding -> not binding.LockedPort
                | SimulatedUnixFlavour.Linux, None -> true
            | TcpEndState.Open
            | TcpEndState.FinQueued
            | TcpEndState.FinReceived
            | TcpEndState.Closed -> false

    /// Whether any *other* socket's binding conflicts with `candidate`, taken
    /// on behalf of `socket`. A binding a reset released
    /// (`resetReleasedPort`) conflicts with nothing.
    ///
    /// The relation `bind(2)` decides admission with, `listen(2)` asks again
    /// on the flavour that re-screens an already-bound socket, and every
    /// ephemeral-port choice asks of each port it considers.
    let internal bindingConflicts
        (socketId : SocketId)
        (socket : SocketDescription)
        (candidate : SocketBinding)
        (machine : UnixMachineState)
        : bool
        =
        machine.Sockets
        |> Map.exists (fun otherId (other : SocketDescription) ->
            if otherId = socketId || resetReleasedPort other machine then
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
    /// end until `accept(2)` mints a socket for it. A connection a reset has
    /// reached (`resetReleasedTuple`) occupies no port of an end whose socket
    /// has gone: measured on both (`reset-closer.c` in
    /// docs/plans/2026-10-07-tcp-byte-transfer), the endpoint of a socket
    /// that closed over unread bytes is free to a fresh bind, where after a
    /// FIN it is not.
    let private orphanedConnectionOccupies (endpoint : InternetEndpoint) (machine : UnixMachineState) : bool =
        let heldFrom (connectionId : ConnectionId) (held : InternetEndpoint) (isServerEnd : bool) : bool =
            machine.Sockets
            |> Map.exists (fun _ socket ->
                match socket.Phase with
                | SocketPhase.Established (c, _)
                | SocketPhase.EstablishedPendingReport c ->
                    c = connectionId
                    && (socket.Binding |> Option.exists (fun binding -> binding.Endpoint = held))
                | SocketPhase.Listening listenState -> isServerEnd && List.contains connectionId listenState.Queue
                | SocketPhase.Idle
                | SocketPhase.Refused _
                | SocketPhase.DatagramPeer _ -> false
            )

        machine.Connections
        |> Map.exists (fun connectionId connection ->
            not (resetReleasedTuple connection)
            && [ connection.ClientAddress, false ; connection.ServerAddress, true ]
               |> List.exists (fun (held, isServerEnd) ->
                   held.Port = endpoint.Port
                   && addressesOverlap held endpoint
                   && not (heldFrom connectionId held isServerEnd)
               )
        )

    /// Whether a TCP connection occupies the four-tuple between `source` and
    /// `destination`, in either orientation. One a reset has reached does
    /// not (`resetReleasedTuple`).
    let private connectionOccupiesTuple
        (source : InternetEndpoint)
        (destination : InternetEndpoint)
        (machine : UnixMachineState)
        : bool
        =
        machine.Connections
        |> Map.exists (fun _ connection ->
            not (resetReleasedTuple connection)
            && ((connection.ClientAddress = source && connection.ServerAddress = destination)
                || (connection.ClientAddress = destination && connection.ServerAddress = source))
        )

    /// Hands out the lowest free port at or after the cursor, sweeping the
    /// range once and wrapping, as the binding `candidate` makes of it, on
    /// behalf of `socket`. `purpose` decides what makes a port free.
    ///
    /// `None` when a full sweep finds nothing. The caller decides what to do:
    /// there is no measured answer for an exhausted range, so inventing an
    /// errno here would be a guess.
    let internal allocateEphemeralPort
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

    /// The TCP send buffer sysctl's default on each flavour, measured on the
    /// probe machines (2026-10-02): `net.inet.tcp.sendspace` reads 131072 on
    /// Darwin 27.0.0, and `net.ipv4.tcp_wmem` reads `4096 16384 4194304` on
    /// the Linux 6.18.5 container, whose middle value a fresh TCP socket's
    /// `SO_SNDBUF` reports.
    let defaultTcpSendSpace (flavour : SimulatedUnixFlavour) : int =
        match flavour with
        | SimulatedUnixFlavour.Linux -> 16384
        | SimulatedUnixFlavour.Darwin -> 131072

    /// The TCP receive buffer sysctl's default on each flavour, measured on
    /// the probe machines (2026-10-07): `net.inet.tcp.recvspace` reads 131072
    /// on Darwin 27.0.0, and `net.ipv4.tcp_rmem` reads `4096 131072 9042912`
    /// on the Linux 6.18.5 container, whose middle value a fresh TCP socket's
    /// `SO_RCVBUF` reports.
    let defaultTcpReceiveSpace (flavour : SimulatedUnixFlavour) : int =
        match flavour with
        | SimulatedUnixFlavour.Linux -> 131072
        | SimulatedUnixFlavour.Darwin -> 131072

    /// The ceiling a TCP send buffer autotunes to, by default on each flavour,
    /// measured on the probe machines (2026-10-07): `net.ipv4.tcp_wmem` reads
    /// `4096 16384 4194304` on the Linux 6.18.5 container, and a connection's
    /// `SO_SNDBUF` grows to its last value as a writer fills it;
    /// `net.inet.tcp.autosndbufmax` reads 4194304 on Darwin 27.0.0.
    let defaultTcpSendSpaceMax (flavour : SimulatedUnixFlavour) : int =
        match flavour with
        | SimulatedUnixFlavour.Linux -> 4194304
        | SimulatedUnixFlavour.Darwin -> 4194304

    /// The receive pipe of a Darwin machine's route to 127.0.0.1, in bytes:
    /// three times the loopback interface's MTU of 16384, as the send pipe
    /// is. A connection's handshake grows a receive buffer smaller than this
    /// to it (measured, `tcp-transfer.c` section C: a listener's `SO_RCVBUF` of
    /// 4096 or of 16384 both end as the same buffer on the accepted socket),
    /// and routes to the machine's other addresses have none.
    let darwinLoopbackReceivePipe : int = 49152

    /// The send pipe of a Darwin machine's route to 127.0.0.1, in bytes: three
    /// times the loopback interface's MTU of 16384. A connection's handshake
    /// grows a send buffer smaller than this to it (measured,
    /// `kevent-write-data.c` section B), and routes to the machine's other
    /// addresses have none, so below it a buffer's size depends on the route.
    let darwinLoopbackSendPipe : int = 49152

    /// The most a Darwin socket buffer can hold, in bytes: the default of the
    /// `kern.ipc.maxsockbuf` sysctl, measured on Darwin 27.0.0. Darwin refuses
    /// a `net.inet.tcp.sendspace` above it, and caps any buffer at it.
    let darwinSocketBufferMax : int = 8388608

    /// The realtime clock's reading, to the nanosecond: `BootTime` plus
    /// `NanosecondsSinceBoot`.
    ///
    /// This is the instant the kernel stamps on an inode it changes.
    /// `clock_gettime(CLOCK_REALTIME)` reports the same clock, but at the
    /// granularity its flavour reports it at; see `UnixClock.clockGettime`.
    let internal realtime (machine : UnixMachineState) : UnixTimestamp =
        // Both unreachable through the public API, which sets these only through
        // `withBootTime` and `advanceClock`; checked because the carry below is only sound when each
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

    /// `machine` with the timestamps of the pipe `pipeId` passed through
    /// `touch`, if this kernel holds them: it holds none for a pipe the process
    /// was launched with, whose timestamps are the launcher's.
    let private withPipeTimes
        (pipeId : PipeId)
        (touch : PipeTimes -> PipeTimes)
        (machine : UnixMachineState)
        : UnixMachineState
        =
        let piped = pipe pipeId machine

        match piped.Origin with
        | PipeOrigin.Launched _ -> machine
        | PipeOrigin.Made status ->
            { machine with
                Pipes =
                    Map.add
                        pipeId
                        { piped with
                            Origin =
                                PipeOrigin.Made
                                    { status with
                                        Times = touch status.Times
                                    }
                        }
                        machine.Pipes
            }

    /// What a read reaching `pipeId`'s read operation, or ending a sleep on it,
    /// does to its timestamps: on Darwin, moves the read end's `st_atime` to
    /// now; on Linux, nothing.
    let internal touchedByPipeRead (pipeId : PipeId) (machine : UnixMachineState) : UnixMachineState =
        match SimulatedUnixPlatform.flavour machine.UnixPlatform with
        | SimulatedUnixFlavour.Linux -> machine
        | SimulatedUnixFlavour.Darwin ->
            let now = realtime machine

            withPipeTimes
                pipeId
                (fun times ->
                    { times with
                        ReadEndAccess = now
                    }
                )
                machine

    /// What a write reaching `pipeId`'s write operation, or ending a sleep on
    /// it, does to its timestamps: on Darwin, moves `st_mtime` and `st_ctime`
    /// of both ends to now; on Linux, nothing.
    let internal touchedByPipeWrite (pipeId : PipeId) (machine : UnixMachineState) : UnixMachineState =
        match SimulatedUnixPlatform.flavour machine.UnixPlatform with
        | SimulatedUnixFlavour.Linux -> machine
        | SimulatedUnixFlavour.Darwin ->
            let now = realtime machine

            withPipeTimes
                pipeId
                (fun times ->
                    { times with
                        Modification = now
                        StatusChange = now
                    }
                )
                machine

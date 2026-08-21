namespace WoofWare.PawPrint

/// Which of the simulated process's inherited standard streams an open file
/// description refers to. Also the routing key for `EmulatedKernel.OutputLog`
/// and `StepEffect.WroteToFd`, so it names a stream rather than a file: the
/// host is what ultimately receives stdout/stderr bytes.
[<RequireQualifiedAccess>]
type FileDescriptorRole =
    | StandardInput
    | StandardOutput
    | StandardError

/// Identity of an open file description. Never guest-visible: no modelled
/// syscall reports one (Linux's `kcmp(2)`, which would, is not modelled), so
/// this exists purely to let two file descriptors denote the *same* open file
/// description rather than two equal copies of one.
[<Struct>]
type OpenFileDescriptionId =
    | OpenFileDescriptionId of value : int64

    override this.ToString () : string =
        match this with
        | OpenFileDescriptionId value -> string<int64> value

/// Identity of a socket. Never guest-visible: `SystemNative_FStat` refuses a
/// socket, so no modelled syscall reports one.
///
/// Deliberately *not* an inode number, despite Linux putting sockets on
/// `sockfs` and giving each one an inode. Measured, a Darwin `AF_INET` socket
/// reports `st_dev` and `st_ino` of 0, so there is no number here that both
/// platforms would agree this value *is*. Its jobs are to keep two sockets from
/// contending under `flock` (see `OpenFileObject.Socket`) and to be what a
/// socket table keys on if one is ever needed; it is `OpenFileDescriptionId`'s
/// sibling, not `InodeNumber`'s.
[<Struct>]
type SocketId =
    | SocketId of value : int64

    override this.ToString () : string =
        match this with
        | SocketId value -> string<int64> value

/// The communication domain of a socket PawPrint can create, as the PAL numbers
/// it (`AddressFamily` in `pal_networking.h`).
///
/// Only the domains a socket can actually *be*: `AF_UNSPEC`, `AF_PACKET` and
/// `AF_CAN` all convert in the shim's address-family screen but reach no socket
/// PawPrint models, so they are refused by `EmulatedKernel.createSocket` rather
/// than represented here.
[<RequireQualifiedAccess>]
type SocketDomain =
    /// `AF_INET`, PAL 2.
    | InterNetwork
    /// `AF_INET6`, PAL 23.
    | InterNetworkV6
    /// `AF_UNIX`, PAL 1.
    | Unix

/// The communication semantics of a socket PawPrint can create, as the PAL
/// numbers it (`SocketType` in `pal_networking.h`).
///
/// `SOCK_RDM` is absent: it converts in the shim's screen but no kernel we model
/// creates one, so it never reaches a live socket.
[<RequireQualifiedAccess>]
type SocketKind =
    /// `SOCK_STREAM`, PAL 1.
    | Stream
    /// `SOCK_DGRAM`, PAL 2.
    | Datagram
    /// `SOCK_RAW`, PAL 3. Reachable only in the `AF_UNIX` domain under the Linux
    /// flavour: an IP raw socket needs `CAP_NET_RAW`, which is not modelled.
    | Raw
    /// `SOCK_SEQPACKET`, PAL 5. Reachable only in the `AF_UNIX` domain under the
    /// Linux flavour.
    | SeqPacket

/// The protocol of a socket PawPrint can create, as the PAL numbers it
/// (`ProtocolType` in `pal_networking.h`) — *not* as the platform numbers it.
/// The shim's conversion can change the value (`AF_INET6` with `PT_ICMP`
/// becomes `IPPROTO_ICMPV6`), and it is the PAL value that
/// `SystemNative_GetSocketType` will owe a caller.
[<RequireQualifiedAccess>]
type SocketProtocol =
    /// `PT_UNSPECIFIED`, PAL 0: "the default protocol for this domain and kind".
    /// Not resolved to that default here — the kernel resolves it, and what it
    /// resolves to is `SystemNative_GetSocketType`'s question to measure.
    | Unspecified
    /// `PT_TCP`, PAL 6.
    | Tcp
    /// `PT_UDP`, PAL 17.
    | Udp

/// The local address a socket holds, once `bind(2)` — or `listen(2)`'s implicit
/// bind — has given it one.
type SocketBinding =
    {
        /// Where the socket is bound.
        Endpoint : InternetEndpoint
    }

/// A socket, as the emulated kernel's socket table holds it.
///
/// Carries no identity of its own: the table is keyed by `SocketId`, so a field
/// here would be a second copy of the key, free to disagree with it.
type SocketDescription =
    {
        /// The domain given to `socket(2)`, and fixed for the socket's life:
        /// no modelled syscall can change it.
        Domain : SocketDomain
        /// The type given to `socket(2)`, likewise fixed.
        Kind : SocketKind
        /// The protocol given to `socket(2)`, likewise fixed.
        Protocol : SocketProtocol
        /// Where this socket is bound, if anywhere. `None` until `bind(2)` or a
        /// `listen(2)` that binds implicitly.
        Binding : SocketBinding option
        /// Whether `SO_REUSEADDR` is set on this socket.
        ///
        /// Socket state rather than binding state, and that is measured rather
        /// than assumed: `SystemNative_Bind` issues the `setsockopt` *before*
        /// `bind(2)` and only when its own `protocolType` argument is `PT_TCP`
        /// (`pal_networking.c:1770`), so a bind that then fails still leaves the
        /// option on — confirmed by reading it back after a bind that answered
        /// EADDRNOTAVAIL. A later successful bind with `PT_UNSPECIFIED` does not
        /// clear it, so deriving the flag from the successful call alone would
        /// lose it and change which later binds are refused.
        ///
        /// Not readable back by a guest: the PAL maps managed `ReuseAddress` to
        /// `SO_REUSEPORT` where that exists (`pal_networking.c:2274`). Its whole
        /// observable effect is which later binds and listens are refused.
        ReuseAddress : bool
        /// Whether `listen(2)` has been called on this socket.
        ///
        /// A flag rather than the backlog, because nothing this slice implements
        /// can read a backlog: its only observer is the depth of the queue
        /// `accept(2)` drains, and storing a number nothing reads is state no
        /// test can cover. `Accept` adds it together with that queue.
        ///
        /// Load-bearing for `bind(2)` all the same: a listening socket's address
        /// conflicts with a second bind on both flavours, where a merely-bound
        /// one may not.
        IsListening : bool
    }

/// What an open file description refers to — the kernel object on the far side
/// of the descriptor.
[<RequireQualifiedAccess>]
type OpenFileObject =
    | StandardStream of FileDescriptorRole
    /// A regular file, directory, or anything else `open(2)` returned a
    /// descriptor for, identified by the inode it resolved to at open time.
    /// Not by path: renaming or deleting the path leaves this description
    /// naming the same file, which is what a real kernel does.
    | File of inode : InodeNumber
    /// A file on Linux's `anon_inodefs` — today only a socket event port, but
    /// `eventfd`, `timerfd` and `signalfd` all live here too.
    ///
    /// **Payload-free on purpose, and it is a `flock` fact rather than an
    /// aesthetic one.** Every anon-inode file in a process shares a *single*
    /// inode, so they all contend with one another. Measured on Linux 6.18.5:
    /// two `epoll_create1` descriptors and an `eventfd` all report
    /// `st_dev=13, st_ino=15`; `flock(LOCK_EX|LOCK_NB)` succeeds on the first
    /// and returns `EWOULDBLOCK` on either of the others; and releasing the
    /// first lets the second take it.
    ///
    /// So giving each port its own identity here would be wrong in a way a
    /// guest can see: PawPrint would grant two exclusive locks where Linux
    /// grants one. `OpenFileObject` is the contention key (see this type's
    /// summary), not a general-purpose identity — code that wants to tell two
    /// ports apart wants `OpenFileDescriptionId`, which is what
    /// `ThreadStatus.BlockedOnSocketEvents` keys on.
    ///
    /// Not the answer for a socket: Linux puts those on `sockfs` with an inode
    /// each, not on `anon_inodefs`. See `Socket`.
    | AnonymousInode
    /// One socket. Carries an identity, and that is a `flock` fact rather than
    /// an aesthetic one — it is exactly where a socket differs from
    /// `AnonymousInode` above. Measured on Linux 6.18.5: two `socket(2)` calls
    /// report distinct `st_ino` (4127 and 4130, both `st_dev` 8), and
    /// `flock(LOCK_EX|LOCK_NB)` succeeds on *both*, where two epoll ports
    /// contend. A payload-free case here would grant one exclusive lock where
    /// Linux grants two.
    ///
    /// Darwin never reaches this: measured, `flock` on any socket there is
    /// ENOTSUP, which `SystemNative_FLock` refuses ahead of any contention
    /// test.
    | Socket of SocketId

/// The mode of an advisory whole-file lock taken by `flock(2)`. "No lock" is
/// the absence of one of these (`OpenFileDescription.Flock` is an option).
[<RequireQualifiedAccess>]
type FlockMode =
    /// `LOCK_SH`. Any number of descriptions may hold this on one file at once.
    | Shared
    /// `LOCK_EX`. Excludes every other description's lock on the same file,
    /// shared or exclusive.
    | Exclusive

/// What an open file description refers to, together with the state that only
/// that kind of object carries.
///
/// Distinct from `OpenFileObject`, the *identity*: the two differ by exactly
/// the file offset. Do not fold the offset into `OpenFileObject` — the `flock`
/// conflict test compares objects for equality, and two descriptions at
/// different offsets on one file must still contend.
/// `OpenFileDescription.object` is the projection back to identity.
///
/// A standard stream has no offset: PawPrint models the standard streams as
/// pipes (see `FileDescriptorRegistry.initial`), which are not seekable —
/// `lseek` on one is `ESPIPE`.
[<RequireQualifiedAccess>]
type OpenFileTarget =
    /// One of the inherited standard streams. No offset: not seekable.
    | StandardStream of role : FileDescriptorRole
    /// A regular file or directory, and where in it this description is
    /// positioned. `read(2)` consumes from here and advances it; `lseek(2)`
    /// sets it; `pread(2)` leaves it alone.
    ///
    /// A real kernel permits an offset arbitrarily far past the end of the
    /// file (`lseek` beyond EOF is how sparse files are made), so this is not
    /// bounded by the file's length — only by being non-negative, which
    /// `VirtualFileSystem.seekTarget` enforces.
    | File of inode : InodeNumber * offset : int64
    /// An epoll instance (Linux) or kqueue (Darwin), handed out by
    /// `SystemNative_CreateSocketEventPort` and destroyed by
    /// `SystemNative_CloseSocketEventPort` — which is `close(2)`, which is why
    /// the port is a descriptor at all rather than a separate kernel table.
    ///
    /// No offset, because neither kernel maintains one for it: measured,
    /// Linux's `lseek` on an epoll descriptor is `noop_llseek`, returning 0 for
    /// any whence in 0..4 and any offset (`-1` and `INT64_MAX` alike), while
    /// Darwin refuses with `ESPIPE`. So there is no position for a caller to
    /// move or read.
    ///
    /// No interest list either: registering a descriptor with a port is
    /// `SystemNative_TryChangeSocketEventRegistration`, and observing the
    /// resulting readiness is `SystemNative_WaitForSocketEvents`. Neither
    /// exists yet, so a registration stored here would be state nothing reads.
    | SocketEventPort
    /// A socket, handed out by `SystemNative_Socket`.
    ///
    /// No offset, because neither kernel maintains one: measured, `lseek` on a
    /// socket is ESPIPE on both for every whence in 0..4 and every offset.
    ///
    /// The socket this names lives in `EmulatedKernel.Sockets`, not here: a
    /// socket outlives, and can precede, any particular description of it. That
    /// is not yet true — `SystemNative_Socket` is the only way to make one, and
    /// it hands back a descriptor in the same breath — but it is what a
    /// completed connection waiting in a listening socket's backlog *is*, and
    /// `SystemNative_Accept` produces those.
    ///
    /// So the description names a socket rather than containing one, and the
    /// kernel is where a socket's lifetime is decided. `EmulatedKernel.socket`
    /// resolves the name.
    | Socket of socket : SocketId

/// Which transfers `open(2)`'s access mode permits: `O_RDONLY`, `O_WRONLY` or
/// `O_RDWR`.
///
/// A three-case DU rather than a readable/writable pair of booleans, because
/// `open(2)` has no fourth answer: an access mode of neither is what the shim
/// rejects with EINVAL before a descriptor exists at all.
///
/// Fixed when the description is created and never changed afterwards — POSIX
/// offers no way to alter one, and Linux's nearest equivalent (reopening through
/// `/proc/self/fd`) is a fresh `open`. So it belongs to the open file
/// description rather than to the descriptor, and `dup(2)` shares it.
[<RequireQualifiedAccess>]
type FileAccessMode =
    /// `O_RDONLY`.
    | ReadOnly
    /// `O_WRONLY`.
    | WriteOnly
    /// `O_RDWR`.
    | ReadWrite

[<RequireQualifiedAccess>]
module FileAccessMode =
    /// Whether `read(2)` and `pread(2)` may transfer through a description
    /// opened this way. A descriptor that fails this is EBADF, which is
    /// `vfs_read`'s answer for a file whose `FMODE_READ` is clear — measured
    /// identically on Linux and Darwin, for a regular file and for a pipe's
    /// write end alike.
    let permitsRead (mode : FileAccessMode) : bool =
        match mode with
        | FileAccessMode.ReadOnly
        | FileAccessMode.ReadWrite -> true
        | FileAccessMode.WriteOnly -> false

    /// Whether `write(2)` and `pwrite(2)` may transfer through a description
    /// opened this way; EBADF otherwise, and again measured the same on both
    /// platforms.
    let permitsWrite (mode : FileAccessMode) : bool =
        match mode with
        | FileAccessMode.WriteOnly
        | FileAccessMode.ReadWrite -> true
        | FileAccessMode.ReadOnly -> false

/// The kernel object a file descriptor points at: POSIX's "open file
/// description". Everything shared between file descriptors that `dup(2)`
/// produced belongs here.
///
/// Of the status flags, only `O_NONBLOCK` is present: `O_APPEND` is absent
/// because no modelled syscall can set it, `SystemNative_Open` accepting
/// neither bit.
type OpenFileDescription =
    {
        /// What this description refers to, and where in it.
        Target : OpenFileTarget
        /// Which transfers this description permits, from the access mode
        /// `open(2)` was given.
        AccessMode : FileAccessMode
        /// Whether `O_NONBLOCK` is set. On the description, not the
        /// descriptor — that is where POSIX keeps the status flags, and why a
        /// `dup(2)` pair shares them. Set through
        /// `SystemNative_FcntlSetIsNonBlocking` (`fcntl(F_SETFL)`).
        ///
        /// `true` is recorded only against a target whose every modelled
        /// transfer honours it — see `setNonBlocking` — so a handler that
        /// consults this may trust it rather than re-checking the target kind.
        NonBlocking : bool
        /// The `flock(2)` lock this description holds, if any.
        ///
        /// On the description, not on the inode: that is where POSIX puts it,
        /// and is why two `open(2)` calls on one path contend while a `dup(2)`
        /// pair does not.
        ///
        /// This is `flock(2)` specifically. `fcntl(2)` record locks — which
        /// CoreLib reaches through `SystemNative_LockFileRegion`, and hence
        /// `FileStream.Lock` — belong to a *(process, file)* pair instead, and
        /// so must not be stored here when they land; see the note on
        /// `FileDescriptorRegistry`.
        Flock : FlockMode option
    }

[<RequireQualifiedAccess>]
module OpenFileDescription =
    /// Which kernel object this description names — its *identity*, with the
    /// per-description position discarded.
    ///
    /// `flock(2)` contention is decided on this: two descriptions contend
    /// exactly when they name the same object, whatever their offsets. Callers
    /// asking "are these the same file?" must compare these rather than the
    /// descriptions.
    ///
    let object (description : OpenFileDescription) : OpenFileObject =
        match description.Target with
        | OpenFileTarget.StandardStream role -> OpenFileObject.StandardStream role
        | OpenFileTarget.File (inode, _) -> OpenFileObject.File inode
        // Every socket event port collapses to one object, because on Linux
        // every anon-inode file shares one inode and so they all contend under
        // `flock`. See `OpenFileObject.AnonymousInode`.
        | OpenFileTarget.SocketEventPort -> OpenFileObject.AnonymousInode
        // Each socket is its own object, unlike the ports above: measured, two
        // sockets do not contend under `flock`. See `OpenFileObject.Socket`.
        | OpenFileTarget.Socket socketId -> OpenFileObject.Socket socketId

/// In-memory model of a Unix per-process file descriptor table, and of the
/// open file descriptions those descriptors point at.
///
/// The indirection is POSIX's, not an implementation detail: a file descriptor
/// is a per-process integer *naming* an open file description, and `dup(2)`
/// allocates a fresh descriptor pointing at the same description. State that
/// belongs to the description (offset, status flags) is therefore shared by
/// every descriptor that names it, while the per-descriptor flags — `FD_CLOEXEC`,
/// to which POSIX-2024 adds `FD_CLOFORK` — are not. PawPrint models neither
/// per-descriptor flag, because it models neither `fork` nor `exec`.
///
/// Beware that the descriptor/description split does not exhaust kernel state.
/// `fcntl(2)` record locks — which CoreLib reaches through
/// `SystemNative_LockFileRegion`, and hence `FileStream.Lock`, on the Linux
/// platform PawPrint simulates — are associated with a *(process, file)* pair:
/// closing *any* descriptor for that file drops them, even one whose
/// description another live descriptor still shares. (Measured on macOS: with
/// `b = dup a`, a lock taken via `a` was released by `close b`.) `flock(2)`
/// locks, by contrast, do belong to the description, and so live in
/// `OpenFileDescription.Flock`. A record lock must *not* join them there when
/// `SystemNative_LockFileRegion` lands: it would inherit the wrong release rule.
type FileDescriptorRegistry =
    private
        {
            /// The per-process descriptor table: which description each live
            /// file descriptor names.
            Fds : Map<int, OpenFileDescriptionId>
            /// The open file descriptions themselves. A description is live
            /// exactly while some descriptor in `Fds` names it; PawPrint models
            /// none of the references that would make liveness more than
            /// reachability (`SCM_RIGHTS` descriptor passing, `mmap`).
            Descriptions : Map<OpenFileDescriptionId, OpenFileDescription>
            /// The identity the next `open` will allocate. Stored and
            /// monotonic rather than derived as one past the highest live id,
            /// which would reuse the identity of a closed description. Nothing
            /// guest-visible could tell the difference — the id is never
            /// reported by any syscall — but a replay trace could.
            /// `VirtualFileSystem.NextInode` is stored for the stronger version
            /// of this reason, inode reuse being guest-visible.
            NextId : OpenFileDescriptionId
        }

[<RequireQualifiedAccess>]
type FileDescriptorDupError =
    /// The supplied fd is not a live entry in the table. `dup(2)` reports
    /// this as `EBADF`; the SystemNative_Dup handler translates this into
    /// a -1 return and `LastSystemError = EBADF`.
    | BadFd

[<RequireQualifiedAccess>]
type FileDescriptorCloseError =
    /// The supplied fd is not a live entry in the table. `close(2)` reports
    /// this as `EBADF`.
    | BadFd

/// What `flock(2)` was asked to do, once the operation bits have been decoded.
///
/// `LOCK_NB` is not part of this: the registry reports that the lock is
/// unavailable, and the handler decides between failing and waiting.
[<RequireQualifiedAccess>]
type FlockRequest =
    /// `LOCK_SH` or `LOCK_EX`. Replaces whatever lock this description already
    /// held, which is how `flock(2)` spells conversion — there is no separate
    /// upgrade operation.
    | Acquire of mode : FlockMode
    /// `LOCK_UN`. Succeeds whether or not a lock was held, as `flock(2)` does.
    | Release

[<RequireQualifiedAccess>]
type FlockError =
    /// The supplied fd is not a live entry in the table; `EBADF`.
    | BadFd
    /// Another open file description holds a conflicting lock on the same file.
    /// A caller that passed `LOCK_NB` reports this as `EWOULDBLOCK`; one that
    /// did not would have to wait for the holder to release.
    | WouldBlock

/// A way in which a `FileDescriptorRegistry` fails to be a descriptor table any
/// kernel could produce. `FileDescriptorRegistry.checkInvariants` returns these.
[<RequireQualifiedAccess>]
type FileDescriptorRegistryDefect =
    /// A live descriptor names a description that is not present. Every lookup
    /// through this descriptor would fail, which no kernel permits.
    | DanglingFd of fd : int * description : OpenFileDescriptionId
    /// A description survives that no descriptor names. The kernel destroys a
    /// description when its last descriptor closes, so this is a leak.
    | UnreferencedDescription of description : OpenFileDescriptionId
    /// A live description's identity is at or above the next one to allocate,
    /// so some future `open` would collide with it — silently retargeting
    /// every descriptor that named it. "At or above" rather than "equal to":
    /// a cursor *below* a live id is just as unsound, it merely takes a few
    /// more opens to do the damage. `VirtualFileSystem`'s `NextInodeNotFresh`
    /// is the same check for the same reason.
    | NextIdNotFresh of nextId : OpenFileDescriptionId * existing : OpenFileDescriptionId
    /// A description is positioned at a negative file offset. No kernel permits
    /// one: `lseek(2)` rejects a computation landing below zero with `EINVAL`
    /// rather than clamping, and `read(2)` never moves the offset backwards.
    ///
    /// There is no matching "too large" defect: seeking arbitrarily far past
    /// EOF is legal, and is how sparse files are made.
    | NegativeOffset of description : OpenFileDescriptionId * offset : int64
    /// Two distinct descriptions name the same file and hold locks that
    /// `flock(2)` would never have granted together — at least one of them
    /// exclusive. This is the mutual-exclusion property itself rather than a
    /// bookkeeping check.
    | ConflictingFlocks of first : OpenFileDescriptionId * second : OpenFileDescriptionId
    /// Two distinct open file descriptions name the same socket. PawPrint
    /// models no way to produce that — `dup(2)` shares a description rather
    /// than copying it — and it would be guest-visible through `flock`, which
    /// contends between descriptions naming one object but not within one.
    | DuplicateSocketId of first : OpenFileDescriptionId * second : OpenFileDescriptionId * socket : SocketId

[<RequireQualifiedAccess>]
module FileDescriptorRegistry =
    let private stdinId : OpenFileDescriptionId = OpenFileDescriptionId 0L
    let private stdoutId : OpenFileDescriptionId = OpenFileDescriptionId 1L
    let private stderrId : OpenFileDescriptionId = OpenFileDescriptionId 2L

    /// Descriptor table as the simulated process inherits it at `exec` time:
    /// stdin (fd 0), stdout (fd 1), stderr (fd 2).
    ///
    /// The three descriptors name three *distinct* descriptions, which models a
    /// process launched with each standard stream separately redirected — the
    /// shape `RealRuntime` itself uses when it launches a guest on real .NET as
    /// PawPrint's differential oracle, giving it three separate pipes.
    ///
    /// This is not the only shape a real process can inherit, and not the
    /// terminal one. Under a tty, fds 0/1/2 are `dup`s of a *single*
    /// `O_RDWR` description: measured via `forkpty`, setting `O_NONBLOCK`
    /// through fd 1 becomes visible on fds 0 and 2, and `write(0, _, _)`
    /// succeeds. PawPrint has already committed against that model elsewhere —
    /// `SystemNative_IsATty` always reports 0, and `SystemNative_Write` to fd 0
    /// returns `EBADF`, which is true only of a redirected `O_RDONLY` stdin.
    /// Seeding one shared description here would contradict both.
    let initial : FileDescriptorRegistry =
        {
            Fds = Map.empty |> Map.add 0 stdinId |> Map.add 1 stdoutId |> Map.add 2 stderrId
            Descriptions =
                let stream (role : FileDescriptorRole) (accessMode : FileAccessMode) : OpenFileDescription =
                    {
                        Target = OpenFileTarget.StandardStream role
                        AccessMode = accessMode
                        NonBlocking = false
                        Flock = None
                    }

                // The access modes a *redirected* launch produces, which is the
                // shape described above: the shell opens stdin `O_RDONLY` and
                // each output stream `O_WRONLY`. Under a tty all three would be
                // `O_RDWR`, which is the same fact as their sharing one
                // description and is rejected here for the same reasons.
                Map.empty
                |> Map.add stdinId (stream FileDescriptorRole.StandardInput FileAccessMode.ReadOnly)
                |> Map.add stdoutId (stream FileDescriptorRole.StandardOutput FileAccessMode.WriteOnly)
                |> Map.add stderrId (stream FileDescriptorRole.StandardError FileAccessMode.WriteOnly)
            NextId = OpenFileDescriptionId 3L
        }

    /// Which description `fd` names, if `fd` is live. Callers that need to know
    /// whether two descriptors share a description — rather than merely name
    /// equal ones — must compare these rather than the payloads.
    let tryFindId (fd : int) (registry : FileDescriptorRegistry) : OpenFileDescriptionId option =
        Map.tryFind fd registry.Fds

    /// The description `fd` names *and* its identity, if `fd` is live.
    ///
    /// For callers that need both, which is otherwise two lookups whose results
    /// could not be shown to agree: `SystemNative_WaitForSocketEvents` keys the
    /// waiter it parks on the identity, while which answer it gives at all
    /// depends on the target.
    let tryFindWithId
        (fd : int)
        (registry : FileDescriptorRegistry)
        : (OpenFileDescriptionId * OpenFileDescription) option
        =
        Map.tryFind fd registry.Fds
        |> Option.map (fun id ->
            match Map.tryFind id registry.Descriptions with
            | Some description -> id, description
            | None ->
                // `checkInvariants` calls this a `DanglingFd`; reaching it
                // through a lookup means the table was mutated by something
                // other than this module's operations.
                failwith
                    $"file descriptor %d{fd} names open file description %O{id}, which is not present in the table (this is an interpreter bug)"
        )

    /// The description `fd` names, if `fd` is live.
    let tryFind (fd : int) (registry : FileDescriptorRegistry) : OpenFileDescription option =
        tryFindWithId fd registry |> Option.map snd

    /// What `fd` refers to, if `fd` is live. Discards the offset, so it is the
    /// wrong lookup for `read(2)` and `lseek(2)`; they want `tryFindTarget`.
    let tryFindObject (fd : int) (registry : FileDescriptorRegistry) : OpenFileObject option =
        tryFind fd registry |> Option.map OpenFileDescription.object

    /// What `fd` refers to and where in it, if `fd` is live. For the callers
    /// that move or consume the file offset.
    let tryFindTarget (fd : int) (registry : FileDescriptorRegistry) : OpenFileTarget option =
        tryFind fd registry |> Option.map (fun description -> description.Target)

    /// Every live file descriptor, and the description each names.
    let fds (registry : FileDescriptorRegistry) : Map<int, OpenFileDescriptionId> = registry.Fds

    /// Every live open file description.
    let descriptions (registry : FileDescriptorRegistry) : Map<OpenFileDescriptionId, OpenFileDescription> =
        registry.Descriptions

    /// Lowest non-negative integer not currently used as a file descriptor.
    /// O(n) in the number of live fds; process fd tables are small.
    let private lowestFree (fds : Map<int, OpenFileDescriptionId>) : int =
        let rec scan (candidate : int) =
            if Map.containsKey candidate fds then
                scan (candidate + 1)
            else
                candidate

        scan 0

    /// Mirrors `dup(2)`: allocate the lowest non-negative fd not in use, naming
    /// the *same* open file description as `oldFd`. No new description is
    /// created, so the description's state is shared with `oldFd` rather than
    /// copied. When `oldFd` is not a live entry, returns `Error BadFd`,
    /// matching the `EBADF` behaviour of `dup(2)`.
    let dup
        (oldFd : int)
        (registry : FileDescriptorRegistry)
        : Result<int * FileDescriptorRegistry, FileDescriptorDupError>
        =
        match Map.tryFind oldFd registry.Fds with
        | None -> Error FileDescriptorDupError.BadFd
        | Some id ->
            let newFd = lowestFree registry.Fds

            Ok (
                newFd,
                { registry with
                    Fds = Map.add newFd id registry.Fds
                }
            )

    /// Remove a descriptor from the table, destroying the description it named
    /// if that was the last descriptor naming it. Mirrors `close(2)`: returns
    /// `Error BadFd` (= `EBADF`) when `fd` is not currently live.
    ///
    /// Closing one descriptor of a `dup` pair leaves the other's description
    /// intact — true of everything PawPrint models, though not of POSIX in
    /// general (see the record-lock note on `FileDescriptorRegistry`).
    ///
    /// Wired into the interpreter via the `SystemNative_Close` handler in
    /// `NativeSystemNative.fs`; the in-house property tests drive close+dup
    /// cycles directly against this function to exercise the `lowestFree`
    /// invariant against the gap structure that close produces.
    ///
    /// Reports the description it destroyed, if this was the last descriptor
    /// naming one: closing a `dup(2)` of a live descriptor destroys nothing and
    /// answers `None`. The caller needs this because a description can be the
    /// last reference to a *kernel object* whose lifetime is decided elsewhere —
    /// `EmulatedKernel.Sockets` is the one that exists today — and this registry
    /// cannot reach that state to clean it up itself.
    let close
        (fd : int)
        (registry : FileDescriptorRegistry)
        : Result<FileDescriptorRegistry * OpenFileDescription option, FileDescriptorCloseError>
        =
        match Map.tryFind fd registry.Fds with
        | None -> Error FileDescriptorCloseError.BadFd
        | Some id ->
            let fds = Map.remove fd registry.Fds

            let stillNamed =
                fds |> Map.exists (fun _ (other : OpenFileDescriptionId) -> other = id)

            if stillNamed then
                Ok (
                    { registry with
                        Fds = fds
                    },
                    None
                )
            else

            // Present by `DanglingFd`: a live descriptor names a live
            // description, so the lookup that found `id` above proves this one.
            let destroyed = Map.find id registry.Descriptions

            Ok (
                { registry with
                    Fds = fds
                    Descriptions = Map.remove id registry.Descriptions
                },
                Some destroyed
            )

    /// Mirrors the descriptor half of `open(2)`: allocate a *fresh* open file
    /// description naming `inode`, and the lowest non-negative descriptor not
    /// in use to point at it.
    ///
    /// Fresh, unlike `dup`: two `open` calls on one path give two descriptions,
    /// which is why they can hold separate offsets and separate `flock` locks.
    ///
    /// The offset starts at 0 for *every* flag, not merely the ones PawPrint
    /// accepts. `O_APPEND` is no exception: measured on both platforms, a
    /// descriptor opened `O_WRONLY | O_APPEND` on a five-byte file reports 0
    /// from `lseek(0, SEEK_CUR)` immediately afterwards, and only reaches 6
    /// after a one-byte write. The flag repositions to the end before each
    /// individual *write*, not at open time, so when the write path lands it
    /// belongs there.
    ///
    /// The BCL would not exercise it in any case: `Interop.Sys.OpenFlags` has no
    /// append bit at all, and `SafeFileHandle.Init` implements `FileMode.Append`
    /// as `OpenOrCreate` plus an explicit seek to the end
    /// (SafeFileHandle.Unix.cs:255).
    ///
    /// Total — there is no failure mode at this level. Whether the path
    /// resolves, whether the flags are ones PawPrint honours, and whether the
    /// process may open the file at all are decided before this is reached; a
    /// real kernel's `EMFILE`/`ENFILE` would belong here, but PawPrint models
    /// no descriptor limit (`RLIMIT_NOFILE` is not in the interop surface).
    let openFile
        (inode : InodeNumber)
        (accessMode : FileAccessMode)
        (registry : FileDescriptorRegistry)
        : int * FileDescriptorRegistry
        =
        let id = registry.NextId
        let (OpenFileDescriptionId raw) = id
        let fd = lowestFree registry.Fds

        fd,
        { registry with
            Fds = Map.add fd id registry.Fds
            Descriptions =
                Map.add
                    id
                    {
                        Target = OpenFileTarget.File (inode, 0L)
                        AccessMode = accessMode
                        // `SystemNative_Open` accepts no `O_NONBLOCK` bit, so
                        // every modelled open starts blocking.
                        NonBlocking = false
                        // `open(2)` never takes a lock; `FileStream` issues a
                        // separate `flock` immediately afterwards, which is
                        // why `FileShare` is not atomic with opening on Unix
                        // (CoreLib's own comment says so).
                        Flock = None
                    }
                    registry.Descriptions
            NextId = OpenFileDescriptionId (raw + 1L)
        }

    /// Mirrors `epoll_create1(EPOLL_CLOEXEC)` (Linux) / `kqueue()` (Darwin):
    /// allocate a fresh open file description naming a new, empty socket event
    /// port, and the lowest non-negative descriptor not in use to point at it.
    ///
    /// Fresh, like `openFile` and unlike `dup`: two `epoll_create1` calls give
    /// two instances, which is what makes them separately identifiable (see
    /// `OpenFileObject.SocketEventPort`).
    ///
    /// The access mode is `ReadWrite`, and that is load-bearing rather than
    /// cosmetic: `SystemNative_Read` checks `FileAccessMode.permitsRead` before
    /// it looks at the target kind and answers `EBADF` if it fails, whereas a
    /// real port answers `EINVAL` (Linux) or `ENXIO` (Darwin) — measured. Both
    /// kernels open the underlying anonymous file `O_RDWR`.
    ///
    /// Total, like `openFile` and for the same reason: PawPrint models no
    /// descriptor limit, so there is no `EMFILE`/`ENFILE` to report.
    let createSocketEventPort (registry : FileDescriptorRegistry) : int * FileDescriptorRegistry =
        let id = registry.NextId
        let (OpenFileDescriptionId raw) = id
        let fd = lowestFree registry.Fds

        fd,
        { registry with
            Fds = Map.add fd id registry.Fds
            Descriptions =
                Map.add
                    id
                    {
                        Target = OpenFileTarget.SocketEventPort
                        AccessMode = FileAccessMode.ReadWrite
                        NonBlocking = false
                        Flock = None
                    }
                    registry.Descriptions
            NextId = OpenFileDescriptionId (raw + 1L)
        }

    /// Mirrors `socket(2)`: allocate a fresh socket, a fresh open file
    /// description naming it, and the lowest non-negative descriptor not in use.
    ///
    /// Says nothing about whether this domain/kind/protocol combination *can*
    /// exist — that is `SimulatedUnixPlatform.socketCreation`'s question, and
    /// this is reached only once it has answered yes.
    ///
    /// `socketId` is minted by the caller, because the socket it names lives in
    /// the emulated kernel's socket table rather than here; `EmulatedKernel.createSocket`
    /// is the one operation that allocates both and is the only thing that
    /// should call this.
    ///
    /// The access mode is `ReadWrite`, and that is load-bearing rather than
    /// cosmetic, for the reason `createSocketEventPort`'s is:
    /// `SystemNative_Read` and `SystemNative_Write` test the access mode before
    /// they look at the target, so anything narrower would answer EBADF where a
    /// real socket answers about its connection state instead (measured:
    /// ENOTCONN, EINVAL, or a block, never EBADF).
    ///
    /// Total, like `openFile` and `createSocketEventPort`: PawPrint models no
    /// descriptor limit, so there is no `EMFILE`/`ENFILE` to report, and no
    /// resource a socket could exhaust.
    let createSocket (socketId : SocketId) (registry : FileDescriptorRegistry) : int * FileDescriptorRegistry =
        let id = registry.NextId
        let (OpenFileDescriptionId raw) = id
        let fd = lowestFree registry.Fds

        fd,
        { registry with
            Fds = Map.add fd id registry.Fds
            Descriptions =
                Map.add
                    id
                    {
                        Target = OpenFileTarget.Socket socketId
                        AccessMode = FileAccessMode.ReadWrite
                        // No `SOCK_NONBLOCK`: the shim's type conversion adds
                        // `SOCK_CLOEXEC` only, and CoreLib switches a socket to
                        // non-blocking through a separate fcntl afterwards.
                        NonBlocking = false
                        // `socket(2)` takes no lock, exactly as `open(2)` does not.
                        Flock = None
                    }
                    registry.Descriptions
            NextId = OpenFileDescriptionId (raw + 1L)
        }

    /// May two *different* open file descriptions on one file hold these two
    /// locks at the same time? Symmetric, so `checkInvariants` can apply it to
    /// an unordered pair.
    let private locksConflict (a : FlockMode) (b : FlockMode) : bool =
        match a, b with
        | FlockMode.Shared, FlockMode.Shared -> false
        | _, _ -> true

    /// Mirrors `flock(2)`.
    ///
    /// The lock belongs to the open file description `fd` names, so two
    /// descriptors from one `dup(2)` share a single lock (releasing through
    /// either releases it), while two separate `open(2)` calls on one path hold
    /// two and therefore contend. That contention is the mechanism behind
    /// `FileShare` on Unix, and it works *within* one process, so a
    /// single-threaded guest can observe it.
    ///
    /// Contention is between descriptions naming the same `OpenFileObject`. For
    /// a standard stream that set is empty by construction — `initial` gives
    /// each role exactly one description and `dup` shares rather than copies —
    /// so `flock` on fd 0/1/2 succeeds and conflicts with nothing. That is what
    /// Linux does (measured: `flock` on a pipe returns 0).
    ///
    /// This is Linux's mechanism. Darwin diverges in three measured ways — it
    /// answers `ENOTSUP` for a pipe, it validates the operation differently,
    /// and it *keeps* a lock that a failed conversion would drop here. None of
    /// those live in this module: deciding what a Darwin-flavoured kernel does
    /// is the handler's job, and it currently refuses rather than modelling it
    /// (see `SystemNative_FLock` in `NativeSystemNative.fs`).
    ///
    /// `Acquire` replaces any lock this description already held, so a
    /// conversion cannot conflict with itself: `SH` to `EX` succeeds when this
    /// description is the only holder, and reports `WouldBlock` when another
    /// still holds `SH`.
    ///
    /// **A failed conversion still drops the old lock**, which is why this
    /// returns a table even on failure. `flock(2)` converts by removing the
    /// existing lock and then establishing the new one, non-atomically — when
    /// the second step fails, the caller is left holding nothing. Documented
    /// BSD-derived behaviour, and measured: with `a` and `b` both holding `SH`,
    /// a failed `a: SH -> EX` leaves `a` unlocked on Linux (a third description
    /// can then take `EX` once `b` releases) but still holding `SH` on Darwin.
    /// PawPrint simulates Linux. The *error* is the same on both platforms, so
    /// only a third description can tell them apart, which is what the test for
    /// this uses.
    ///
    /// `Release` succeeds whether or not a lock was held.
    let flock
        (fd : int)
        (request : FlockRequest)
        (registry : FileDescriptorRegistry)
        : FileDescriptorRegistry * FlockError option
        =
        match Map.tryFind fd registry.Fds with
        | None -> registry, Some FlockError.BadFd
        | Some id ->

        let description =
            match Map.tryFind id registry.Descriptions with
            | Some description -> description
            | None ->
                failwith
                    $"file descriptor %d{fd} names open file description %O{id}, which is not present in the table (this is an interpreter bug)"

        let withFlock (flock : FlockMode option) : FileDescriptorRegistry =
            { registry with
                Descriptions =
                    Map.add
                        id
                        { description with
                            Flock = flock
                        }
                        registry.Descriptions
            }

        match request with
        | FlockRequest.Release -> withFlock None, None
        | FlockRequest.Acquire mode ->

        let blocked =
            registry.Descriptions
            |> Map.exists (fun otherId (other : OpenFileDescription) ->
                // `otherId <> id` is what makes conversion work: this
                // description's own lock is not an obstacle to replacing it.
                otherId <> id
                // Identity, not the whole description: two descriptions on one
                // file contend however far apart their offsets are.
                && OpenFileDescription.object other = OpenFileDescription.object description
                && (
                    match other.Flock with
                    | None -> false
                    | Some held -> locksConflict mode held
                )
            )

        if blocked then
            // The old lock is gone either way — see the note above.
            withFlock None, Some FlockError.WouldBlock
        else
            withFlock (Some mode), None

    /// Move the file offset of the description `fd` names.
    ///
    /// Total in the offset — every non-negative `int64` is a position a real
    /// kernel would accept, including far past the end of the file — and
    /// *partial* in the descriptor: reaching this with an fd that is not live,
    /// or one naming an unseekable object, is an interpreter bug rather than a
    /// guest error. Both callers (`SystemNative_LSeek` and `SystemNative_Read`)
    /// have already resolved the description and rejected `EBADF`/`ESPIPE`
    /// before they get here.
    ///
    /// Deciding *which* offset is not this module's business: `lseek`'s
    /// arithmetic needs the file's size, which lives in the filesystem, and its
    /// error vocabulary differs by platform. `VirtualFileSystem.seekTarget`
    /// computes the target and this stores it.
    let setOffset (fd : int) (offset : int64) (registry : FileDescriptorRegistry) : FileDescriptorRegistry =
        if offset < 0L then
            failwith
                $"setOffset: fd %d{fd} was asked to move to offset %d{offset}, which is negative. No kernel permits a negative file offset; the caller must reject this as EINVAL before storing it (this is an interpreter bug)."

        match Map.tryFind fd registry.Fds with
        | None ->
            failwith
                $"setOffset: fd %d{fd} is not a live file descriptor, so there is no offset to move (this is an interpreter bug: the caller should have answered EBADF)."
        | Some id ->

        let description =
            match Map.tryFind id registry.Descriptions with
            | Some description -> description
            | None ->
                failwith
                    $"file descriptor %d{fd} names open file description %O{id}, which is not present in the table (this is an interpreter bug)"

        match description.Target with
        | OpenFileTarget.StandardStream role ->
            failwith
                $"setOffset: fd %d{fd} names standard stream %O{role}, which PawPrint models as a pipe and so has no file offset (this is an interpreter bug: the caller should have answered ESPIPE)."
        | OpenFileTarget.SocketEventPort ->
            failwith
                $"setOffset: fd %d{fd} names a socket event port, which holds no file offset on either platform — Linux's lseek on one is noop_llseek and Darwin's is ESPIPE (this is an interpreter bug: the caller should have answered without moving a position)."
        | OpenFileTarget.Socket socketId ->
            failwith
                $"setOffset: fd %d{fd} names socket %O{socketId}, which holds no file offset on either platform — `lseek` on a socket is ESPIPE on both (this is an interpreter bug: the caller should have answered ESPIPE)."
        | OpenFileTarget.File (inode, _) ->

        { registry with
            Descriptions =
                Map.add
                    id
                    { description with
                        Target = OpenFileTarget.File (inode, offset)
                    }
                    registry.Descriptions
        }

    /// Mirrors the `O_NONBLOCK` half of `fcntl(F_SETFL)`: record whether this
    /// description's transfers should refuse to block. On the description, so
    /// shared with every descriptor `dup(2)` has produced for it.
    ///
    /// Like `setOffset`, *partial* in the descriptor: the caller
    /// (`SystemNative_FcntlSetIsNonBlocking`) has already answered `EBADF` for
    /// a dead fd. It has also refused to *set* the flag on the targets whose
    /// modelled transfers do not consult it — a standard stream (modelled as a
    /// pipe, whose reads a real kernel's `O_NONBLOCK` turns into `EAGAIN`
    /// while PawPrint's stream handlers would block regardless) and a socket
    /// event port. Storing `true` against either would be a divergence nothing
    /// could see coming, so that is an interpreter bug here; clearing is
    /// always honest and always permitted.
    let setNonBlocking (fd : int) (value : bool) (registry : FileDescriptorRegistry) : FileDescriptorRegistry =
        match Map.tryFind fd registry.Fds with
        | None ->
            failwith
                $"setNonBlocking: fd %d{fd} is not a live file descriptor, so there is no description to flag (this is an interpreter bug: the caller should have answered EBADF)."
        | Some id ->

        let description =
            match Map.tryFind id registry.Descriptions with
            | Some description -> description
            | None ->
                failwith
                    $"file descriptor %d{fd} names open file description %O{id}, which is not present in the table (this is an interpreter bug)"

        match description.Target, value with
        | OpenFileTarget.StandardStream role, true ->
            failwith
                $"setNonBlocking: fd %d{fd} names standard stream %O{role}, and no modelled stream transfer consults O_NONBLOCK, so a stored `true` would silently keep blocking semantics (this is an interpreter bug: the caller should have refused)."
        | OpenFileTarget.SocketEventPort, true ->
            failwith
                $"setNonBlocking: fd %d{fd} names a socket event port, and PawPrint has not decided what O_NONBLOCK on an epoll or kqueue descriptor even means (this is an interpreter bug: the caller should have refused)."
        | OpenFileTarget.StandardStream _, false
        | OpenFileTarget.SocketEventPort, false
        | OpenFileTarget.File _, _
        | OpenFileTarget.Socket _, _ ->

        { registry with
            Descriptions =
                Map.add
                    id
                    { description with
                        NonBlocking = value
                    }
                    registry.Descriptions
        }

    /// Every way in which `registry` fails to be a descriptor table a kernel
    /// could produce. Empty for any registry built out of `initial`, `dup` and
    /// `close`; the property tests assert exactly that.
    let checkInvariants (registry : FileDescriptorRegistry) : FileDescriptorRegistryDefect list =
        let dangling =
            registry.Fds
            |> Map.toList
            |> List.filter (fun (_, id) -> not (Map.containsKey id registry.Descriptions))
            |> List.map FileDescriptorRegistryDefect.DanglingFd

        let named = registry.Fds |> Map.toList |> List.map snd |> Set.ofList

        let unreferenced =
            registry.Descriptions
            |> Map.toList
            |> List.map fst
            |> List.filter (fun id -> not (Set.contains id named))
            |> List.map FileDescriptorRegistryDefect.UnreferencedDescription

        let freshness =
            registry.Descriptions
            |> Map.toList
            |> List.map fst
            |> List.filter (fun id -> id >= registry.NextId)
            |> List.map (fun id -> FileDescriptorRegistryDefect.NextIdNotFresh (registry.NextId, id))

        let negativeOffsets =
            registry.Descriptions
            |> Map.toList
            |> List.choose (fun (id, description) ->
                match description.Target with
                | OpenFileTarget.StandardStream _
                | OpenFileTarget.SocketEventPort
                | OpenFileTarget.Socket _ -> None
                | OpenFileTarget.File (_, offset) ->
                    if offset < 0L then
                        Some (FileDescriptorRegistryDefect.NegativeOffset (id, offset))
                    else
                        None
            )

        let locked =
            registry.Descriptions
            |> Map.toList
            |> List.choose (fun (id, description) ->
                description.Flock
                |> Option.map (fun mode -> id, OpenFileDescription.object description, mode)
            )

        // Every unordered pair of distinct locked descriptions naming one file.
        // Quadratic in the number of live descriptions, which is a handful; the
        // clarity is worth more here than the asymptotics, since this is the one
        // check that states the actual `flock` guarantee.
        let conflicting =
            locked
            |> List.collect (fun (firstId, firstObject, firstMode) ->
                locked
                |> List.filter (fun (secondId, secondObject, secondMode) ->
                    firstId < secondId
                    && firstObject = secondObject
                    && locksConflict firstMode secondMode
                )
                |> List.map (fun (secondId, _, _) ->
                    FileDescriptorRegistryDefect.ConflictingFlocks (firstId, secondId)
                )
            )

        let sockets =
            registry.Descriptions
            |> Map.toList
            |> List.choose (fun (id, description) ->
                match description.Target with
                | OpenFileTarget.StandardStream _
                | OpenFileTarget.SocketEventPort
                | OpenFileTarget.File _ -> None
                | OpenFileTarget.Socket socketId -> Some (id, socketId)
            )

        // Every unordered pair of distinct descriptions, as `conflicting` above
        // does it and for the same reason: a handful of live descriptions, and
        // the clarity is worth more than the asymptotics.
        let duplicateSockets =
            sockets
            |> List.collect (fun (firstId, firstSocket) ->
                sockets
                |> List.choose (fun (secondId, secondSocket) ->
                    if firstId < secondId && firstSocket = secondSocket then
                        Some (FileDescriptorRegistryDefect.DuplicateSocketId (firstId, secondId, firstSocket))
                    else
                        None
                )
            )

        dangling
        @ unreferenced
        @ freshness
        @ negativeOffsets
        @ conflicting
        @ duplicateSockets

    /// Fail loudly if `registry` is not sound, naming `context`.
    let assertInvariants (context : string) (registry : FileDescriptorRegistry) : FileDescriptorRegistry =
        match checkInvariants registry with
        | [] -> registry
        | defects ->
            let rendered = defects |> List.map (sprintf "%A") |> String.concat "; "

            failwith $"%s{context}: the file descriptor table is not one any kernel could produce: %s{rendered}"

    /// Construction that bypasses every invariant this module maintains.
    ///
    /// Exists so that `checkInvariants` can be tested. One greppable token;
    /// nothing outside tests should use it.
    [<RequireQualifiedAccess>]
    module Unchecked =
        let ofParts
            (fds : Map<int, OpenFileDescriptionId>)
            (descriptions : Map<OpenFileDescriptionId, OpenFileDescription>)
            (nextId : OpenFileDescriptionId)
            : FileDescriptorRegistry
            =
            {
                Fds = fds
                Descriptions = descriptions
                NextId = nextId
            }

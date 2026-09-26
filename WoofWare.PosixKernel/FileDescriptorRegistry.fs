namespace WoofWare.PosixKernel

open System.Collections.Immutable

/// <summary>
/// Which of the simulated process's inherited standard streams an open file
/// description refers to.
/// </summary>
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

/// Identity of a socket. Never visible to the simulated process:
/// `UnixPathResolution.fstat` refuses a socket, so no modelled syscall reports
/// one.
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

/// Identity of one TCP connection — the kernel object a completed loopback
/// handshake creates. Never guest-visible.
///
/// Distinct from either endpoint's `SocketId` because a connection outlives
/// the sockets that made it: measured, a client closed while its connection
/// sits in a listener's accept queue leaves the connection acceptable, and
/// `accept(2)` then returns a working descriptor onto it. The server side has
/// no socket at all until that accept. The connection table itself lives on
/// `UnixMachineState`, beside the socket table.
[<Struct>]
type ConnectionId =
    | ConnectionId of value : int64

    override this.ToString () : string =
        match this with
        | ConnectionId value -> string<int64> value

/// The communication domain of a socket this kernel can create.
///
/// Only the domains a socket can actually *be*, so this is narrower than any
/// `AF_*` list: `UnixSocket.socket` answers or refuses every other domain
/// before a socket exists.
[<RequireQualifiedAccess>]
type SocketDomain =
    /// `AF_INET`.
    | Inet
    /// `AF_INET6`.
    | Inet6
    /// `AF_UNIX`.
    | Unix

/// The communication semantics of a socket this kernel can create: what
/// `getsockopt(SO_TYPE)` would report for it.
///
/// No `SOCK_RAW`: an internet raw socket is one `UnixSocket.socket` refuses,
/// and Linux makes an `AF_UNIX` `SOCK_RAW` request into a `SOCK_DGRAM` socket.
[<RequireQualifiedAccess>]
type SocketKind =
    /// `SOCK_STREAM`.
    | Stream
    /// `SOCK_DGRAM`.
    | Datagram
    /// `SOCK_SEQPACKET`. Reachable only in the `AF_UNIX` domain under the Linux
    /// flavour.
    | SeqPacket

/// The protocol a socket was asked for.
///
/// Holds no numbering of its own: `UnixSocket.socket` reads the caller's
/// protocol number in the simulated flavour's numbering, and this is the
/// condition that number named.
[<RequireQualifiedAccess>]
type SocketProtocol =
    /// The default protocol for this domain and kind: the caller passed 0, or,
    /// on Linux, named `AF_UNIX`'s only protocol.
    | Default
    /// TCP.
    | Tcp
    /// UDP.
    | Udp

/// The local address a socket holds, once `bind(2)` — or `listen(2)`'s implicit
/// bind — has given it one.
type SocketBinding =
    {
        /// Where the socket is bound, with any source-address resolution a
        /// connect performed already applied: a wildcard-bound or unbound
        /// socket that connects over loopback reads back 127.0.0.1 here.
        Endpoint : InternetEndpoint
        /// The address the guest's own `bind(2)` gave the socket, or `None`
        /// when the binding arose implicitly (a connect or listen minted it).
        /// The kernel state Linux calls SOCK_BINDADDR_LOCK: a Linux refusal
        /// delivery reverts `Endpoint`'s address to this (the wildcard when
        /// `None`) while keeping the port — measured for all three
        /// provenances — where Darwin keeps the resolved address.
        LockedAddress : uint32 option
        /// Whether the guest's own `bind(2)` chose the port: true only when it
        /// asked for a non-zero one. The kernel state Linux calls
        /// SOCK_BINDPORT_LOCK: a datagram `connect(AF_UNSPEC)` there keeps a
        /// locked port and drops an unlocked one, measured
        /// (`docs/probes/udp-connect/dissolve.py`), so a socket bound to
        /// `0.0.0.0:5555` dissolves to `0.0.0.0:5555` where one bound to
        /// `0.0.0.0:0` dissolves to nothing.
        LockedPort : bool
    }

/// What `listen(2)` gave a socket: the number it was called with, and the
/// queue of completed connections `accept(2)` drains.
type ListenState =
    {
        /// The backlog argument `listen(2)` recorded, verbatim. Its one reader
        /// is the accept-queue capacity check in `UnixConnection.connectSocket`,
        /// which derives the flavour's admission bound from it — measured,
        /// Linux admits `backlog + 1` completed connections and Darwin exactly
        /// `backlog` — so this stores the input to that rule rather than a
        /// pre-computed capacity that would bake one flavour's arithmetic in.
        Backlog : int
        /// Completed connections not yet accepted, oldest first: `accept(2)`
        /// dequeues from the head. Measured on both flavours: accept returns
        /// connections in the order the connects completed.
        Queue : ConnectionId list
    }

/// Where a socket is in its connection lifecycle. One value, rather than an
/// `IsListening` flag beside a connection field, because the states are
/// mutually exclusive in the kernel being modelled: a listening socket cannot
/// also be connected, and two fields would represent that conjunction only to
/// forbid it by invariant.
///
/// What a refused socket's later connects answer is measured:
/// `probe3.c`/`probe4.c` on Linux (2026-08-21; see
/// docs/plans/2026-08-21-socket-connect.md for the full table), and Darwin
/// 27.0.0 (2026-09-25).
[<RequireQualifiedAccess>]
type SocketPhase =
    /// Fresh from `socket(2)`, dissolved by a Linux `AF_UNSPEC` connect, or
    /// reset by a Linux refusal delivery. Bound or not is `Binding`'s
    /// business, not this one's.
    | Idle
    /// `listen(2)` has been called.
    | Listening of ListenState
    /// A non-blocking connect completed, and no later connect has reported
    /// that completion yet: the next `connect(2)` answers SUCCESS exactly
    /// once (Linux; Darwin never enters this state — its retry answers
    /// EISCONN directly, so its non-blocking completion goes straight to
    /// `Established`).
    | EstablishedPendingReport of connection : ConnectionId
    /// Connected. `connect(2)` answers EISCONN.
    | Established of connection : ConnectionId
    /// A non-blocking connect was refused and the ECONNREFUSED is still
    /// pending. On Linux the next `connect(2)` delivers it and transitions to
    /// `Idle`. Darwin's `connect(2)` never delivers it: every one answers
    /// EISCONN, whatever the destination, and the socket stays here.
    | RefusedPendingDelivery
    /// Darwin's refused socket after a *blocking* refusal, which delivered the
    /// error inline: nothing is pending, and every later `connect(2)` answers
    /// EISCONN, whatever the destination. Unreachable under the Linux flavour.
    | Dead
    /// A datagram socket's default peer, set by `connect(2)` on it. Filters
    /// nothing yet — no receive path exists — but re-connect re-targets it
    /// and a Linux `AF_UNSPEC` connect dissolves it back to `Idle`, both
    /// guest-visible through the return codes.
    | DatagramPeer of peer : InternetEndpoint

[<RequireQualifiedAccess>]
module SocketPhase =
    /// Whether `listen(2)` has been called: the reading `bind(2)`'s conflict
    /// rule takes.
    let isListening (phase : SocketPhase) : bool =
        match phase with
        | SocketPhase.Listening _ -> true
        | SocketPhase.Idle
        | SocketPhase.EstablishedPendingReport _
        | SocketPhase.Established _
        | SocketPhase.RefusedPendingDelivery
        | SocketPhase.Dead
        | SocketPhase.DatagramPeer _ -> false

/// A socket, as the emulated kernel's socket table holds it.
///
/// Carries no identity of its own: the table is keyed by `SocketId`, so a field
/// here would be a second copy of the key, free to disagree with it.
type SocketDescription =
    {
        /// The domain given to `socket(2)`, and fixed for the socket's life:
        /// no modelled syscall can change it.
        Domain : SocketDomain
        /// The socket's type, as `getsockopt(SO_TYPE)` reports it, and likewise
        /// fixed. Not always the type `socket(2)` was asked for: see `SocketKind`.
        Kind : SocketKind
        /// The protocol given to `socket(2)`, likewise fixed.
        Protocol : SocketProtocol
        /// Where this socket is bound, if anywhere. `None` until `bind(2)` or a
        /// `listen(2)` that binds implicitly.
        Binding : SocketBinding option
        /// Whether `SO_REUSEADDR` is set on this socket. `setsockopt(2)` sets
        /// and clears it at any point in the socket's life, `getsockopt(2)`
        /// reads it back, and `accept(2)` gives the socket it returns the
        /// listener's value.
        ///
        /// Its effect is on which bindings conflict, which `bind(2)` and Linux's
        /// `listen(2)` decide from the value each socket holds at the time of
        /// the call rather than when it was bound. See
        /// `SimulatedUnixPlatform.bindConflict`.
        ReuseAddress : bool
        /// Where this socket is in its connection lifecycle: idle, listening
        /// (with the accept queue), connected, or latched by a refusal.
        ///
        /// Load-bearing for `bind(2)` too: a listening socket's address
        /// conflicts with a second bind on both flavours, where a merely-bound
        /// one may not.
        Phase : SocketPhase
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
    /// process can see: this kernel would grant two exclusive locks where Linux
    /// grants one. `OpenFileObject` is the contention key (see this type's
    /// summary), not a general-purpose identity — code that wants to tell two
    /// ports apart wants `OpenFileDescriptionId`, which is what
    /// `ParkedSocketWait` keys on.
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
    /// ENOTSUP, which `UnixDescriptor.flock` refuses to answer
    /// (`FLockRefusal.DarwinSocket`) ahead of any contention test.
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

/// Linux's `<sys/epoll.h>` event bits: what `epoll_ctl(2)` reads in
/// `struct epoll_event.events` and `epoll_wait(2)` writes back.
///
/// The readiness bits below 0x10000 are numbered as Linux's `<poll.h>` numbers
/// the `POLL*` condition of the same name. The top four are not conditions but
/// modes of the registration, which the kernel keeps in the same word.
[<RequireQualifiedAccess>]
module EpollEvents =
    /// `EPOLLIN`.
    [<Literal>]
    let In : uint32 = 0x0001u

    /// `EPOLLPRI`.
    [<Literal>]
    let Pri : uint32 = 0x0002u

    /// `EPOLLOUT`.
    [<Literal>]
    let Out : uint32 = 0x0004u

    /// `EPOLLERR`. Reported whether or not a registration asked for it.
    [<Literal>]
    let Err : uint32 = 0x0008u

    /// `EPOLLHUP`. Reported whether or not a registration asked for it.
    [<Literal>]
    let Hup : uint32 = 0x0010u

    /// `EPOLLRDNORM`.
    [<Literal>]
    let RdNorm : uint32 = 0x0040u

    /// `EPOLLRDBAND`.
    [<Literal>]
    let RdBand : uint32 = 0x0080u

    /// `EPOLLWRNORM`.
    [<Literal>]
    let WrNorm : uint32 = 0x0100u

    /// `EPOLLWRBAND`.
    [<Literal>]
    let WrBand : uint32 = 0x0200u

    /// `EPOLLMSG`.
    [<Literal>]
    let Msg : uint32 = 0x0400u

    /// `EPOLLRDHUP`.
    [<Literal>]
    let RdHup : uint32 = 0x2000u

    /// `EPOLLEXCLUSIVE`: wake one of several ports registered on the same
    /// target rather than all of them.
    [<Literal>]
    let Exclusive : uint32 = 0x10000000u

    /// `EPOLLWAKEUP`: hold a wakeup source while an event is pending.
    [<Literal>]
    let WakeUp : uint32 = 0x20000000u

    /// `EPOLLONESHOT`: disarm the registration once it has reported.
    [<Literal>]
    let OneShot : uint32 = 0x40000000u

    /// `EPOLLET`: report readiness edges rather than a level.
    [<Literal>]
    let EdgeTriggered : uint32 = 0x80000000u

/// The five readiness conditions a socket presents right now, before any
/// waiter's request is applied.
///
/// Shared by both waiters this kernel models, because on Linux they read the
/// same thing: `poll(2)` and epoll's `ep_item_poll` both take their mask from
/// the file's own `->poll` handler, and measurement agrees on every phase
/// (docs/plans/2026-08-23-socket-poll, and `epoll-ctl.c` in
/// docs/plans/2026-08-23-posix-kernel-extraction). The handler also sets
/// `*NORM` and `*BAND` bits, which follow from these five and the socket's
/// kind; `UnixPoll` states the full mask in Linux's numbering.
type ReadinessLevel =
    {
        /// `EPOLLIN`.
        In : bool
        /// `EPOLLOUT`.
        Out : bool
        /// `EPOLLRDHUP`.
        RdHup : bool
        /// `EPOLLHUP`.
        Hup : bool
        /// `EPOLLERR`.
        Err : bool
    }

[<RequireQualifiedAccess>]
module ReadinessLevel =
    let none : ReadinessLevel =
        {
            In = false
            Out = false
            RdHup = false
            Hup = false
            Err = false
        }

    let isEmpty (readiness : ReadinessLevel) : bool = readiness = none

/// One registration held by a socket event port: what `epoll_ctl(2)` recorded
/// for one target.
type EpollRegistration =
    {
        /// The event mask the kernel stores for this registration, in Linux's
        /// `<sys/epoll.h>` numbering (`EpollEvents`): the caller's `events`
        /// with `EPOLLERR` and `EPOLLHUP` added, because `epoll_ctl` forces
        /// those two into every stored mask.
        ///
        /// What a wait reports for this registration is the target's readiness
        /// masked by this, and a keyed wake queues the registration only when
        /// the wake's key meets it.
        Events : uint32
        /// The caller's `epoll_data`, delivered verbatim when an event fires.
        Data : uint64
        /// When this registration's ADD committed, as an ordinal from the
        /// kernel's counter. One signal can make several registrations of the
        /// same socket pending at once (they share the socket's wait queue),
        /// and the measured delivery order for that tie is newest-registered
        /// first — the wait queue is LIFO. A MOD preserves this: the wait
        /// queue entry the order comes from is created at ADD and untouched
        /// by MOD.
        RegisteredAt : int64
    }

/// Everything one socket event port holds: its interest table, and the ready
/// list `epoll_wait` drains.
type SocketEventPortState =
    {
        /// The interest table, keyed exactly as epoll keys a registration:
        /// the (fd number, open file description) pair of the target.
        Registrations : Map<int * OpenFileDescriptionId, EpollRegistration>
        /// The registrations with an edge outstanding, in delivery order.
        /// A registration enters when the driver signals it and it is not
        /// already here, or when an ADD/MOD finds its target ready; delivery
        /// walks the prefix, re-polls each entry against the target's current
        /// readiness, and removes what it walked — reporting the nonempty
        /// re-polls and silently dropping the stale ones — leaving only what
        /// batch truncation spared. Always a subset of `Registrations`, with
        /// no duplicates (`checkInvariants` states both).
        Ready : (int * OpenFileDescriptionId) list
    }

/// What an open file description refers to, together with the state that only
/// that kind of object carries.
///
/// Distinct from `OpenFileObject`, the *identity*: the two differ by exactly
/// the file offset. Do not fold the offset into `OpenFileObject` — the `flock`
/// conflict test compares objects for equality, and two descriptions at
/// different offsets on one file must still contend.
/// `OpenFileDescription.object` is the projection back to identity.
///
/// A standard stream has no offset: this library models the standard streams as
/// pipes (see `FileDescriptorRegistry.initial`), which are not seekable —
/// `lseek` on one is `ESPIPE`.
[<RequireQualifiedAccess>]
type OpenFileTarget =
    /// One of the inherited standard streams. No offset: not seekable.
    | StandardStream of role : FileDescriptorRole
    /// A regular file, and where in it this description is positioned.
    /// `read(2)` consumes from here and advances it; `lseek(2)` sets it;
    /// `pread(2)` leaves it alone.
    ///
    /// A real kernel permits an offset arbitrarily far past the end of the
    /// file (`lseek` beyond EOF is how sparse files are made), so this is not
    /// bounded by the file's length — only by being non-negative, which
    /// `VirtualFileSystem.seekTarget` enforces.
    | File of inode : InodeNumber * offset : int64
    /// A directory, and how far through it this description has read.
    ///
    /// Not a `File` with an offset, because a directory's position is not a
    /// byte offset on either kernel: it is a resumption point in the
    /// directory's entries, which is what `DirectoryPosition` states. `lseek(2)`
    /// moves it, and it is shared by every descriptor `dup(2)` makes.
    ///
    /// Always opened for reading only: a directory cannot be opened for writing
    /// on either kernel.
    | Directory of inode : InodeNumber * position : DirectoryPosition
    /// An epoll instance (Linux) or kqueue (Darwin), handed out by
    /// `FileDescriptorRegistry.createSocketEventPort` and destroyed by
    /// `close(2)`, which is why the port is a descriptor at all rather than a
    /// separate kernel table.
    ///
    /// No offset, because neither kernel maintains one for it: measured,
    /// Linux's `lseek` on an epoll descriptor is `noop_llseek`, returning 0 for
    /// any whence in 0..4 and any offset (`-1` and `INT64_MAX` alike), while
    /// Darwin refuses with `ESPIPE`. So there is no position for a caller to
    /// move or read.
    ///
    /// Carries the port's interest table and ready list. The registration
    /// key is the **(fd number, open file description) pair** of the target;
    /// both halves are measured — an ADD through a `dup` of a registered
    /// target succeeds and creates a second registration, while an ADD
    /// through a `dup` of the *port* answers EEXIST for an already-registered
    /// target, because the `dup` pair shares this description and so this
    /// table.
    | SocketEventPort of state : SocketEventPortState
    /// A socket, handed out by `UnixSocket.socket`.
    ///
    /// No offset, because neither kernel maintains one: measured, `lseek` on a
    /// socket is ESPIPE on both for every whence in 0..4 and every offset.
    ///
    /// The socket this names lives in `UnixMachineState.Sockets`, not here: a
    /// socket outlives, and can precede, any particular description of it.
    /// `UnixConnection.accept` is where one precedes its description: it turns a
    /// completed connection waiting in a listening socket's backlog into a
    /// socket and a descriptor.
    ///
    /// So the description names a socket rather than containing one, and the
    /// kernel is where a socket's lifetime is decided. `UnixMachineState.socket`
    /// resolves the name.
    | Socket of socket : SocketId

/// Which transfers `open(2)`'s access mode permits: `O_RDONLY`, `O_WRONLY` or
/// `O_RDWR`.
///
/// A three-case DU rather than a readable/writable pair of booleans, because
/// there is no fourth: an access mode of neither is refused before a
/// descriptor exists at all (see `OpenFlags`).
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
/// because no modelled syscall can set it, `OpenFlags` carrying neither bit.
type OpenFileDescription =
    {
        /// What this description refers to, and where in it.
        Target : OpenFileTarget
        /// Which transfers this description permits, from the access mode
        /// `open(2)` was given.
        AccessMode : FileAccessMode
        /// Whether `O_NONBLOCK` is set. On the description, not the
        /// descriptor — that is where POSIX keeps the status flags, and why a
        /// `dup(2)` pair shares them. Set through `fcntl(F_SETFL)`, which is
        /// `UnixSocket.setNonBlocking`.
        ///
        /// `true` is recorded only against a target whose every modelled
        /// transfer honours it — see `setNonBlocking` — so a caller that
        /// consults this may trust it rather than re-checking the target kind.
        NonBlocking : bool
        /// The `flock(2)` lock this description holds, if any.
        ///
        /// On the description, not on the inode: that is where POSIX puts it,
        /// and is why two `open(2)` calls on one path contend while a `dup(2)`
        /// pair does not.
        ///
        /// This is `flock(2)` specifically. `fcntl(2)` record locks belong to a
        /// *(process, file)* pair instead, and so must not be stored here when
        /// they land; see the note on `FileDescriptorRegistry`.
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
        | OpenFileTarget.File (inode, _)
        | OpenFileTarget.Directory (inode, _) -> OpenFileObject.File inode
        // Every socket event port collapses to one object, because on Linux
        // every anon-inode file shares one inode and so they all contend under
        // `flock`. See `OpenFileObject.AnonymousInode`.
        | OpenFileTarget.SocketEventPort _ -> OpenFileObject.AnonymousInode
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
/// to which POSIX-2024 adds `FD_CLOFORK` — are not. This library models neither
/// per-descriptor flag, because it models neither `fork` nor `exec`.
///
/// Beware that the descriptor/description split does not exhaust kernel state.
/// `fcntl(2)` record locks are associated with a *(process, file)* pair:
/// closing *any* descriptor for that file drops them, even one whose
/// description another live descriptor still shares. (Measured on macOS: with
/// `b = dup a`, a lock taken via `a` was released by `close b`.) `flock(2)`
/// locks, by contrast, do belong to the description, and so live in
/// `OpenFileDescription.Flock`. A record lock must *not* join them there when
/// record locks are modelled: it would inherit the wrong release rule.
type FileDescriptorRegistry =
    private
        {
            /// The per-process descriptor table: which description each live
            /// file descriptor names.
            Fds : Map<int, OpenFileDescriptionId>
            /// The open file descriptions themselves. A description is live
            /// exactly while some descriptor in `Fds` names it; this library models
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
    /// this as `EBADF`.
    | BadFd

[<RequireQualifiedAccess>]
type FileDescriptorCloseError =
    /// The supplied fd is not a live entry in the table. `close(2)` reports
    /// this as `EBADF`.
    | BadFd

/// What `flock(2)` was asked to do, once the operation bits have been decoded.
///
/// `LOCK_NB` is not part of this: the registry reports that the lock is
/// unavailable, and `UnixDescriptor.flock` decides between failing and waiting.
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
    /// A directory description is at `DirectoryPosition.Unenumerable` with an
    /// offset that is not positive. A negative one is `lseek`'s EINVAL on both
    /// kernels, and zero is the start of the directory, which is
    /// `DirectoryPosition.Cursor DirectoryCursor.Start`: holding it as an
    /// offset would refuse a read both kernels answer.
    | UnenumerableDirectoryPositionNotPositive of description : OpenFileDescriptionId * offset : int64
    /// A directory description is open for writing, which no kernel permits:
    /// `open(2)` on a directory for writing is EISDIR on both.
    | WritableDirectory of description : OpenFileDescriptionId
    /// Two distinct descriptions name the same file and hold locks that
    /// `flock(2)` would never have granted together — at least one of them
    /// exclusive. This is the mutual-exclusion property itself rather than a
    /// bookkeeping check.
    | ConflictingFlocks of first : OpenFileDescriptionId * second : OpenFileDescriptionId
    /// Two distinct open file descriptions name the same socket. This library
    /// models no way to produce that — `dup(2)` shares a description rather
    /// than copying it — and it would be visible to a process through `flock`, which
    /// contends between descriptions naming one object but not within one.
    | DuplicateSocketId of first : OpenFileDescriptionId * second : OpenFileDescriptionId * socket : SocketId
    /// A socket event port's interest table registers an open file description
    /// that no longer exists. Linux removes these at file-release time, which
    /// is `close`'s sweep here, so a survivor is a leak — invisible to every
    /// syscall (no fd can name the dead description again) but exactly what
    /// the readiness wake must never deliver from.
    | SocketEventRegistrationTargetDead of port : OpenFileDescriptionId * target : OpenFileDescriptionId
    /// A socket event port's ready list holds an entry its interest table does
    /// not register. Every path that removes a registration (DEL, and close's
    /// sweep) removes its pending entry in the same step, so a survivor would
    /// deliver an event from a corpse.
    | SocketEventReadyEntryUnregistered of port : OpenFileDescriptionId * key : int * target : OpenFileDescriptionId
    /// A socket event port's ready list holds the same entry twice. A pending
    /// registration keeps its place rather than being re-queued (measured:
    /// a re-signal does not move it), so a duplicate would deliver one edge
    /// twice.
    | SocketEventReadyEntryDuplicated of port : OpenFileDescriptionId * key : int * target : OpenFileDescriptionId

[<RequireQualifiedAccess>]
module FileDescriptorRegistry =
    let private stdinId : OpenFileDescriptionId = OpenFileDescriptionId 0L
    let private stdoutId : OpenFileDescriptionId = OpenFileDescriptionId 1L
    let private stderrId : OpenFileDescriptionId = OpenFileDescriptionId 2L

    /// Descriptor table as the simulated process inherits it at `exec` time:
    /// stdin (fd 0), stdout (fd 1), stderr (fd 2).
    ///
    /// The three descriptors name three *distinct* descriptions, which models a
    /// process launched with each standard stream separately redirected to its
    /// own pipe.
    ///
    /// This is not the only shape a real process can inherit, and not the
    /// terminal one. Under a tty, fds 0/1/2 are `dup`s of a *single*
    /// `O_RDWR` description: measured via `forkpty`, setting `O_NONBLOCK`
    /// through fd 1 becomes visible on fds 0 and 2, and `write(0, _, _)`
    /// succeeds. Seeding one shared description here would contradict what
    /// this library answers elsewhere: `UnixReadWrite.write` to fd 0 returns
    /// `EBADF`, which is true only of a redirected `O_RDONLY` stdin.
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
    /// could not be shown to agree: `UnixPoll.admitSocketWait` keys the waiter
    /// it parks on the identity, while which answer it gives at all depends on
    /// the target.
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
                    $"file descriptor %d{fd} names open file description %O{id}, which is not present in the table (this is a bug in this library: every descriptor names a description in the table)"
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
    /// intact — true of everything this library models, though not of POSIX in
    /// general (see the record-lock note on `FileDescriptorRegistry`).
    ///
    /// The descriptor-table half of `close(2)`, and only that half: it drops
    /// the descriptor and, if it was the last one, the description, and it
    /// releases nothing that description referenced. `UnixDescriptor.close` is
    /// the syscall, and the one caller; a client that wants `close(2)` wants
    /// that. The in-house property tests drive close+dup cycles directly
    /// against this function to exercise the `lowestFree` invariant against
    /// the gap structure that closing produces.
    ///
    /// Reports the description it destroyed, if this was the last descriptor
    /// naming one: closing a `dup(2)` of a live descriptor destroys nothing and
    /// answers `None`. The caller needs this because a description can be the
    /// last reference to a *kernel object* whose lifetime is decided elsewhere —
    /// `UnixMachineState.Sockets` is the one that exists today — and this registry
    /// cannot reach that state to clean it up itself.
    let dropDescriptor
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

            // A destroyed description also vanishes from every socket event
            // port's interest table, which is what Linux does at file-release
            // time (`eventpoll_release`). No syscall can tell the difference —
            // the dead pair's key can never be probed again, since no fd names
            // the description — but the readiness wake, when it lands, must
            // not deliver from a corpse, so the tables stay truthful now and
            // `checkInvariants` states it.
            let descriptions =
                Map.remove id registry.Descriptions
                |> Map.map (fun _ description ->
                    match description.Target with
                    | OpenFileTarget.SocketEventPort portState ->
                        { description with
                            Target =
                                OpenFileTarget.SocketEventPort
                                    {
                                        Registrations =
                                            portState.Registrations |> Map.filter (fun (_, target) _ -> target <> id)
                                        Ready = portState.Ready |> List.filter (fun (_, target) -> target <> id)
                                    }
                        }
                    | OpenFileTarget.StandardStream _
                    | OpenFileTarget.File _
                    | OpenFileTarget.Directory _
                    | OpenFileTarget.Socket _ -> description
                )

            Ok (
                { registry with
                    Fds = fds
                    Descriptions = descriptions
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
    /// The offset starts at 0 for *every* flag, not merely the ones `OpenFlags`
    /// carries. `O_APPEND` is no exception: measured on both platforms, a
    /// descriptor opened `O_WRONLY | O_APPEND` on a five-byte file reports 0
    /// from `lseek(0, SEEK_CUR)` immediately afterwards, and only reaches 6
    /// after a one-byte write. The flag repositions to the end before each
    /// individual *write*, not at open time, so when the write path lands it
    /// belongs there.
    ///
    /// Total — there is no failure mode at this level. Whether the path
    /// resolves, whether the flags are ones this library honours, and whether the
    /// process may open the file at all are decided before this is reached; a
    /// real kernel's `EMFILE`/`ENFILE` would belong here, but this library
    /// models no descriptor limit (`RLIMIT_NOFILE`).
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
                        // `OpenFlags` carries no `O_NONBLOCK` bit, so every
                        // modelled open starts blocking.
                        NonBlocking = false
                        // `open(2)` never takes a lock.
                        Flock = None
                    }
                    registry.Descriptions
            NextId = OpenFileDescriptionId (raw + 1L)
        }

    /// Mirrors the descriptor half of `open(2)` on a directory: allocate a
    /// fresh open file description positioned at the start of `inode`'s
    /// entries, open for reading, and the lowest non-negative descriptor not in
    /// use to point at it.
    ///
    /// Total, for the reasons `openFile` is; whether `inode` is a directory the
    /// process may read is decided before this is reached.
    let openDirectory (inode : InodeNumber) (registry : FileDescriptorRegistry) : int * FileDescriptorRegistry =
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
                        Target = OpenFileTarget.Directory (inode, DirectoryPosition.Cursor DirectoryCursor.Start)
                        AccessMode = FileAccessMode.ReadOnly
                        NonBlocking = false
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
    /// cosmetic: `UnixReadWrite.read` checks `FileAccessMode.permitsRead` before
    /// it looks at the target kind and answers `EBADF` if it fails, whereas a
    /// real port answers `EINVAL` (Linux) or `ENXIO` (Darwin) — measured. Both
    /// kernels open the underlying anonymous file `O_RDWR`.
    ///
    /// Total, like `openFile` and for the same reason: this library models no
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
                        // Fresh instance, empty interest table: nothing is
                        // registered with a port at creation.
                        Target =
                            OpenFileTarget.SocketEventPort
                                {
                                    Registrations = Map.empty
                                    Ready = []
                                }
                        AccessMode = FileAccessMode.ReadWrite
                        NonBlocking = false
                        Flock = None
                    }
                    registry.Descriptions
            NextId = OpenFileDescriptionId (raw + 1L)
        }

    /// Allocate a fresh open file description naming the socket `socketId`,
    /// and the lowest non-negative descriptor not in use to point at it. The
    /// description is blocking.
    ///
    /// Says nothing about whether such a socket *can* exist: that is
    /// `UnixSocket.socket`'s question.
    ///
    /// `socketId` is minted by the caller, because the socket it names lives in
    /// the emulated kernel's socket table rather than here; `UnixSocket.socket`
    /// and `UnixConnection.accept` allocate both, and are the only things that
    /// should call this.
    ///
    /// The access mode is `ReadWrite`, and that is load-bearing rather than
    /// cosmetic, for the reason `createSocketEventPort`'s is:
    /// `UnixReadWrite.read` and `UnixReadWrite.write` test the access mode before
    /// they look at the target, so anything narrower would answer EBADF where a
    /// real socket answers about its connection state instead (measured:
    /// ENOTCONN, EINVAL, or a block, never EBADF).
    ///
    /// Total, like `openFile` and `createSocketEventPort`: this library models no
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
                        // A `socket(2)` asked for `SOCK_NONBLOCK` sets it
                        // afterwards, through `setNonBlocking`.
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

    /// Would an `flock` acquisition of `mode`, by the open file description
    /// `requester` onto `object`, have to wait? True exactly when some *other*
    /// description naming `object` holds a lock that could not be held
    /// alongside it.
    ///
    /// `requester`'s own lock is never an obstacle: `Acquire` replaces it, which
    /// is how `flock(2)` spells conversion. `requester` need not still be a live
    /// description — a caller polling this on behalf of a parked waiter is
    /// asking whether the lock *would* be granted, and the answer does not
    /// depend on the requester holding anything.
    ///
    /// The acquire path is the primary caller, and a client's wake predicate is
    /// the other: parking on a lock means waiting for exactly the condition the
    /// acquire tested, so the two must be one function rather than two that
    /// agree.
    let flockConflicts
        (object : OpenFileObject)
        (requester : OpenFileDescriptionId)
        (mode : FlockMode)
        (registry : FileDescriptorRegistry)
        : bool
        =
        registry.Descriptions
        |> Map.exists (fun otherId (other : OpenFileDescription) ->
            otherId <> requester
            // Identity, not the whole description: two descriptions on one
            // file contend however far apart their offsets are.
            && OpenFileDescription.object other = object
            && (
                match other.Flock with
                | None -> false
                | Some held -> locksConflict mode held
            )
        )

    /// `flock(2)` on the open file description directly, for a caller that holds
    /// one rather than a descriptor.
    ///
    /// The primitive: `flock` above is this with a descriptor resolved first,
    /// and everything that docstring says about conversion, contention and the
    /// dropped old lock is decided here.
    ///
    /// A caller finishing a *parked* acquisition wants this rather than the
    /// by-fd version, and not as a convenience: descriptor numbers are reused as
    /// soon as they are free, so the number a waiter parked on can name an
    /// entirely different object by the time the lock becomes available.
    ///
    /// Loudly partial in `id`, which is not a guest-reachable failure: a
    /// description a client still holds an identity for is one it must not have
    /// let `close` destroy.
    let flockOn
        (id : OpenFileDescriptionId)
        (request : FlockRequest)
        (registry : FileDescriptorRegistry)
        : FileDescriptorRegistry * FlockError option
        =
        let description =
            match Map.tryFind id registry.Descriptions with
            | Some description -> description
            | None ->
                failwith
                    $"open file description %O{id} is not present in the table (this is a bug in the caller of FileDescriptorRegistry.flockOn, which holds the identity of a description it let close destroy)"

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
            flockConflicts (OpenFileDescription.object description) id mode registry

        if blocked then
            // The old lock is gone either way — see the note on `flock`.
            withFlock None, Some FlockError.WouldBlock
        else
            withFlock (Some mode), None

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
    /// those live in this module: `UnixDescriptor.flock` decides what a
    /// Darwin-flavoured kernel does, and refuses (`FLockRefusal`) wherever
    /// Darwin would answer differently.
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
    /// This module models Linux. The *error* is the same on both platforms, so
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
        | Some id -> flockOn id request registry

    /// Move the file offset of the description `fd` names.
    ///
    /// Total in the offset — every non-negative `int64` is a position a real
    /// kernel would accept, including far past the end of the file — and
    /// *partial* in the descriptor: reaching this with an fd that is not live,
    /// or one naming an unseekable object, is a bug in the caller. This
    /// library's callers (`UnixDescriptor.lseek` and `UnixReadWrite.read`)
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
                $"setOffset: fd %d{fd} was asked to move to offset %d{offset}, which is negative. No kernel permits a negative file offset; the caller must reject this as EINVAL before storing it (this is a bug in the caller of FileDescriptorRegistry.setOffset)."

        match Map.tryFind fd registry.Fds with
        | None ->
            failwith
                $"setOffset: fd %d{fd} is not a live file descriptor, so there is no offset to move (this is a bug in the caller of FileDescriptorRegistry.setOffset, which should have answered EBADF)."
        | Some id ->

        let description =
            match Map.tryFind id registry.Descriptions with
            | Some description -> description
            | None ->
                failwith
                    $"file descriptor %d{fd} names open file description %O{id}, which is not present in the table (this is a bug in this library: every descriptor names a description in the table)"

        match description.Target with
        | OpenFileTarget.StandardStream role ->
            failwith
                $"setOffset: fd %d{fd} names standard stream %O{role}, which this library models as a pipe and so has no file offset (this is a bug in the caller of FileDescriptorRegistry.setOffset, which should have answered ESPIPE)."
        | OpenFileTarget.SocketEventPort _ ->
            failwith
                $"setOffset: fd %d{fd} names a socket event port, which holds no file offset on either platform — Linux's lseek on one is noop_llseek and Darwin's is ESPIPE (this is a bug in the caller of FileDescriptorRegistry.setOffset, which should have answered without moving a position)."
        | OpenFileTarget.Socket socketId ->
            failwith
                $"setOffset: fd %d{fd} names socket %O{socketId}, which holds no file offset on either platform — `lseek` on a socket is ESPIPE on both (this is a bug in the caller of FileDescriptorRegistry.setOffset, which should have answered ESPIPE)."
        | OpenFileTarget.Directory (inode, _) ->
            failwith
                $"setOffset: fd %d{fd} names directory %O{inode}, whose position is not a byte offset (this is a bug in the caller of FileDescriptorRegistry.setOffset, which should have called setDirectoryPosition)."
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

    /// Move the position of the directory description `fd` names, which every
    /// descriptor `dup(2)` made for it shares.
    ///
    /// Partial in the descriptor, like `setOffset`: the caller has already
    /// answered EBADF for a dead fd, and has established that `fd` names a
    /// directory.
    let setDirectoryPosition
        (fd : int)
        (position : DirectoryPosition)
        (registry : FileDescriptorRegistry)
        : FileDescriptorRegistry
        =
        let id =
            match Map.tryFind fd registry.Fds with
            | Some id -> id
            | None ->
                failwith
                    $"setDirectoryPosition: fd %d{fd} is not a live file descriptor (this is a bug in the caller of FileDescriptorRegistry.setDirectoryPosition, which should have answered EBADF)."

        let description =
            match Map.tryFind id registry.Descriptions with
            | Some description -> description
            | None ->
                failwith
                    $"file descriptor %d{fd} names open file description %O{id}, which is not present in the table (this is a bug in this library: every descriptor names a description in the table)"

        match description.Target with
        | OpenFileTarget.Directory (inode, _) ->
            { registry with
                Descriptions =
                    Map.add
                        id
                        { description with
                            Target = OpenFileTarget.Directory (inode, position)
                        }
                        registry.Descriptions
            }
        | OpenFileTarget.StandardStream _
        | OpenFileTarget.File _
        | OpenFileTarget.SocketEventPort _
        | OpenFileTarget.Socket _ ->
            failwith
                $"setDirectoryPosition: fd %d{fd} names %O{description.Target}, which is not a directory (this is a bug in the caller of FileDescriptorRegistry.setDirectoryPosition)."

    /// Mirrors the `O_NONBLOCK` half of `fcntl(F_SETFL)`: record whether this
    /// description's transfers should refuse to block. On the description, so
    /// shared with every descriptor `dup(2)` has produced for it.
    ///
    /// Like `setOffset`, *partial* in the descriptor: the caller
    /// (`UnixSocket.setNonBlocking`) has already answered `EBADF` for
    /// a dead fd. It has also refused to *set* the flag on a standard stream —
    /// modelled as a pipe, whose reads a real kernel's `O_NONBLOCK` turns into
    /// `EAGAIN` while this library's stream reads would block regardless, so a
    /// stored `true` there would be a divergence nothing could see coming, and
    /// is a bug in the caller here. Clearing is always honest and always
    /// permitted. A socket event port stores freely: measured on both
    /// flavours, `F_SETFL` genuinely toggles the bit there (even on Darwin,
    /// where the call also reports ENOTTY — the caller's business, not this
    /// store's), and no modelled wait consults it, because `epoll_wait` and
    /// `kevent` block per their own timeout argument rather than per the
    /// descriptor's flags.
    let setNonBlocking (fd : int) (value : bool) (registry : FileDescriptorRegistry) : FileDescriptorRegistry =
        match Map.tryFind fd registry.Fds with
        | None ->
            failwith
                $"setNonBlocking: fd %d{fd} is not a live file descriptor, so there is no description to flag (this is a bug in the caller of FileDescriptorRegistry.setNonBlocking, which should have answered EBADF)."
        | Some id ->

        let description =
            match Map.tryFind id registry.Descriptions with
            | Some description -> description
            | None ->
                failwith
                    $"file descriptor %d{fd} names open file description %O{id}, which is not present in the table (this is a bug in this library: every descriptor names a description in the table)"

        match description.Target, value with
        | OpenFileTarget.StandardStream role, true ->
            failwith
                $"setNonBlocking: fd %d{fd} names standard stream %O{role}, and no modelled stream transfer consults O_NONBLOCK, so a stored `true` would silently keep blocking semantics (this is a bug in the caller of FileDescriptorRegistry.setNonBlocking, which should have refused)."
        | OpenFileTarget.StandardStream _, false
        | OpenFileTarget.SocketEventPort _, _
        | OpenFileTarget.File _, _
        | OpenFileTarget.Directory _, _
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

    /// Rewrite the state of the socket event port `portId` names. Loudly
    /// partial on a dead or non-port description: every caller resolved it as
    /// a port moments ago, so either means it wrote against a different table
    /// than the one it read. `operation` names the caller for that message.
    let private mapSocketEventPort
        (operation : string)
        (portId : OpenFileDescriptionId)
        (f : SocketEventPortState -> SocketEventPortState)
        (registry : FileDescriptorRegistry)
        : FileDescriptorRegistry
        =
        match Map.tryFind portId registry.Descriptions with
        | None ->
            failwith
                $"%s{operation}: %O{portId} names no live open file description; the caller resolved it moments ago, so this is a bug in the caller of FileDescriptorRegistry.%s{operation}."
        | Some description ->

        match description.Target with
        | OpenFileTarget.StandardStream _
        | OpenFileTarget.File _
        | OpenFileTarget.Directory _
        | OpenFileTarget.Socket _ ->
            failwith
                $"%s{operation}: %O{portId} is not a socket event port; the caller resolved it as one moments ago, so this is a bug in the caller of FileDescriptorRegistry.%s{operation}."
        | OpenFileTarget.SocketEventPort portState ->

        { registry with
            Descriptions =
                Map.add
                    portId
                    { description with
                        Target = OpenFileTarget.SocketEventPort (f portState)
                    }
                    registry.Descriptions
        }

    /// Record `registration` under `key` in the interest table of the port
    /// `portId` names: the table half of a committed `EPOLL_CTL_ADD`.
    ///
    /// The key is epoll's own, the target's (fd number, open file description)
    /// pair. Loudly partial on a key already registered, which `epoll_ctl`
    /// answers `EEXIST` for before it reaches the table; that answer is the
    /// caller's (`UnixPoll.epollCtl`).
    let addEpollRegistration
        (portId : OpenFileDescriptionId)
        (key : int * OpenFileDescriptionId)
        (registration : EpollRegistration)
        (registry : FileDescriptorRegistry)
        : FileDescriptorRegistry
        =
        registry
        |> mapSocketEventPort
            "addEpollRegistration"
            portId
            (fun portState ->
                if Map.containsKey key portState.Registrations then
                    failwith
                        $"addEpollRegistration: %A{key} is already registered with port %O{portId}, which epoll_ctl answers EEXIST for (this is a bug in the caller of FileDescriptorRegistry.addEpollRegistration)."

                { portState with
                    Registrations = Map.add key registration portState.Registrations
                }
            )

    /// Replace the stored event mask and data of the registration under `key`:
    /// the table half of a committed `EPOLL_CTL_MOD`. The registration keeps
    /// its `RegisteredAt`, and a pending entry keeps its place in the ready
    /// list (measured, `order3.c` row L).
    ///
    /// Loudly partial on a key not registered, which `epoll_ctl` answers
    /// `ENOENT` for before it reaches the table.
    let modifyEpollRegistration
        (portId : OpenFileDescriptionId)
        (key : int * OpenFileDescriptionId)
        (events : uint32)
        (data : uint64)
        (registry : FileDescriptorRegistry)
        : FileDescriptorRegistry
        =
        registry
        |> mapSocketEventPort
            "modifyEpollRegistration"
            portId
            (fun portState ->
                match Map.tryFind key portState.Registrations with
                | None ->
                    failwith
                        $"modifyEpollRegistration: %A{key} is not registered with port %O{portId}, which epoll_ctl answers ENOENT for (this is a bug in the caller of FileDescriptorRegistry.modifyEpollRegistration)."
                | Some existing ->
                    { portState with
                        Registrations =
                            Map.add
                                key
                                { existing with
                                    Events = events
                                    Data = data
                                }
                                portState.Registrations
                    }
            )

    /// Remove the registration under `key`, and its pending entry if it has
    /// one: the table half of a committed `EPOLL_CTL_DEL`.
    ///
    /// Loudly partial on a key not registered, which `epoll_ctl` answers
    /// `ENOENT` for before it reaches the table.
    let removeEpollRegistration
        (portId : OpenFileDescriptionId)
        (key : int * OpenFileDescriptionId)
        (registry : FileDescriptorRegistry)
        : FileDescriptorRegistry
        =
        registry
        |> mapSocketEventPort
            "removeEpollRegistration"
            portId
            (fun portState ->
                if not (Map.containsKey key portState.Registrations) then
                    failwith
                        $"removeEpollRegistration: %A{key} is not registered with port %O{portId}, which epoll_ctl answers ENOENT for (this is a bug in the caller of FileDescriptorRegistry.removeEpollRegistration)."

                {
                    Registrations = Map.remove key portState.Registrations
                    Ready = portState.Ready |> List.filter (fun k -> k <> key)
                }
            )

    /// Append `key` to the ready list of the port `portId` names. The caller
    /// has decided the entry belongs there (an ADD/MOD found the target ready,
    /// or the driver signalled it); this only performs the append, and it is
    /// loudly partial on a key that is not registered or is already pending —
    /// both would mean the caller's decision was made against a different
    /// table than the one being written.
    let appendSocketEventReady
        (portId : OpenFileDescriptionId)
        (key : int * OpenFileDescriptionId)
        (registry : FileDescriptorRegistry)
        : FileDescriptorRegistry
        =
        match Map.tryFind portId registry.Descriptions with
        | None ->
            failwith
                $"appendSocketEventReady: %O{portId} names no live open file description; the caller resolved it moments ago, so this is a bug in the caller of FileDescriptorRegistry.appendSocketEventReady."
        | Some description ->

        match description.Target with
        | OpenFileTarget.StandardStream _
        | OpenFileTarget.File _
        | OpenFileTarget.Directory _
        | OpenFileTarget.Socket _ ->
            failwith
                $"appendSocketEventReady: %O{portId} is not a socket event port; the caller resolved it as one moments ago, so this is a bug in the caller of FileDescriptorRegistry.appendSocketEventReady."
        | OpenFileTarget.SocketEventPort portState ->

        if not (Map.containsKey key portState.Registrations) then
            failwith
                $"appendSocketEventReady: %A{key} is not registered with port %O{portId}, so it cannot become pending on it (this is a bug in the caller of FileDescriptorRegistry.appendSocketEventReady)."

        if List.contains key portState.Ready then
            failwith
                $"appendSocketEventReady: %A{key} is already pending on port %O{portId}; a pending entry keeps its place rather than being re-queued, so the caller should not have asked (this is a bug in the caller of FileDescriptorRegistry.appendSocketEventReady)."

        { registry with
            Descriptions =
                Map.add
                    portId
                    { description with
                        Target =
                            OpenFileTarget.SocketEventPort
                                { portState with
                                    Ready = portState.Ready @ [ key ]
                                }
                    }
                    registry.Descriptions
        }

    /// Replace the ready list of the port `portId` names — delivery's
    /// write-back once a walk has consumed a prefix. Loudly partial on a
    /// dead or non-port description, on an entry the interest table does not
    /// register, and on a duplicate: the caller derived `ready` from the
    /// port's own state moments ago, so any of those means it wrote against
    /// a different table than the one it read.
    let setSocketEventReady
        (portId : OpenFileDescriptionId)
        (ready : (int * OpenFileDescriptionId) list)
        (registry : FileDescriptorRegistry)
        : FileDescriptorRegistry
        =
        match Map.tryFind portId registry.Descriptions with
        | None ->
            failwith
                $"setSocketEventReady: %O{portId} names no live open file description (this is a bug in the caller of FileDescriptorRegistry.setSocketEventReady, which derived the list from a different table)."
        | Some description ->

        match description.Target with
        | OpenFileTarget.StandardStream _
        | OpenFileTarget.File _
        | OpenFileTarget.Directory _
        | OpenFileTarget.Socket _ ->
            failwith
                $"setSocketEventReady: %O{portId} is not a socket event port (this is a bug in the caller of FileDescriptorRegistry.setSocketEventReady, which derived the list from a different table)."
        | OpenFileTarget.SocketEventPort portState ->

        for key in ready do
            if not (Map.containsKey key portState.Registrations) then
                failwith
                    $"setSocketEventReady: %A{key} is not registered with port %O{portId} (this is a bug in the caller of FileDescriptorRegistry.setSocketEventReady, which derived the list from a different table)."

        if List.length (List.distinct ready) <> List.length ready then
            failwith
                $"setSocketEventReady: the ready list for port %O{portId} repeats an entry (this is a bug in the caller of FileDescriptorRegistry.setSocketEventReady, which derived the list from a different table)."

        { registry with
            Descriptions =
                Map.add
                    portId
                    { description with
                        Target =
                            OpenFileTarget.SocketEventPort
                                { portState with
                                    Ready = ready
                                }
                    }
                    registry.Descriptions
        }

    /// The driver signalled every description in `naming` (all of one
    /// socket's descriptions): on every port, each registration targeting one
    /// of them becomes pending unless it already is. `wakeKey` is what the
    /// waker carried, in Linux's `<sys/epoll.h>` numbering, and the two kinds
    /// are both measured:
    ///
    ///   * a *keyed* wake queues only the registrations whose stored mask
    ///     meets its key (`order6.c`: an IN edge at a WRITE-only registration
    ///     leaves no trace, and a later MOD to READ enqueues fresh at MOD
    ///     time). The key is the waker's, not the target's level: a data-ready
    ///     wake queues a registration for `EPOLLPRI` alone, which a listener
    ///     never reports (`epoll-ctl.c`'s WAKE section);
    ///   * an *unkeyed* wake (a connect completing, a peer's FIN) queues every
    ///     registration regardless of its mask — the entry keeps the wake's
    ///     position through a later interest change, and delivery's re-poll is
    ///     what filters (`order8.c`, `order9.c`).
    ///
    /// When one signal makes several registrations pending at once they enter
    /// newest-registered first — the socket's wait queue is LIFO (measured,
    /// `order4.c`) — and a registration already pending keeps its place
    /// (`order2.c` row H).
    let signalSocketEventPorts
        (naming : Set<OpenFileDescriptionId>)
        (wakeKey : uint32 option)
        (registry : FileDescriptorRegistry)
        : FileDescriptorRegistry
        =
        let descriptions =
            registry.Descriptions
            |> Map.map (fun _ description ->
                match description.Target with
                | OpenFileTarget.StandardStream _
                | OpenFileTarget.File _
                | OpenFileTarget.Directory _
                | OpenFileTarget.Socket _ -> description
                | OpenFileTarget.SocketEventPort portState ->
                    let entering =
                        portState.Registrations
                        |> Map.toList
                        |> List.filter (fun ((_, targetId as key), registration) ->
                            Set.contains targetId naming
                            && not (List.contains key portState.Ready)
                            && (
                                match wakeKey with
                                | None -> true
                                | Some key -> key &&& registration.Events <> 0u
                            )
                        )
                        |> List.sortByDescending (fun (_, registration) -> registration.RegisteredAt)
                        |> List.map fst

                    match entering with
                    | [] -> description
                    | entering ->
                        { description with
                            Target =
                                OpenFileTarget.SocketEventPort
                                    { portState with
                                        Ready = portState.Ready @ entering
                                    }
                        }
            )

        { registry with
            Descriptions = descriptions
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
                | OpenFileTarget.SocketEventPort _
                | OpenFileTarget.Socket _ -> None
                | OpenFileTarget.File (_, offset) ->
                    if offset < 0L then
                        Some (FileDescriptorRegistryDefect.NegativeOffset (id, offset))
                    else
                        None
                | OpenFileTarget.Directory (_, DirectoryPosition.Cursor _) -> None
                | OpenFileTarget.Directory (_, DirectoryPosition.Unenumerable offset) ->
                    if offset <= 0L then
                        Some (FileDescriptorRegistryDefect.UnenumerableDirectoryPositionNotPositive (id, offset))
                    else
                        None
            )

        let writableDirectories =
            registry.Descriptions
            |> Map.toList
            |> List.choose (fun (id, description) ->
                match description.Target with
                | OpenFileTarget.Directory _ when FileAccessMode.permitsWrite description.AccessMode ->
                    Some (FileDescriptorRegistryDefect.WritableDirectory id)
                | OpenFileTarget.Directory _
                | OpenFileTarget.StandardStream _
                | OpenFileTarget.SocketEventPort _
                | OpenFileTarget.Socket _
                | OpenFileTarget.File _ -> None
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
                | OpenFileTarget.SocketEventPort _
                | OpenFileTarget.File _
                | OpenFileTarget.Directory _ -> None
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

        let deadRegistrations =
            registry.Descriptions
            |> Map.toList
            |> List.collect (fun (portId, description) ->
                match description.Target with
                | OpenFileTarget.StandardStream _
                | OpenFileTarget.File _
                | OpenFileTarget.Directory _
                | OpenFileTarget.Socket _ -> []
                | OpenFileTarget.SocketEventPort portState ->
                    portState.Registrations
                    |> Map.toList
                    |> List.choose (fun ((_, targetId), _) ->
                        if Map.containsKey targetId registry.Descriptions then
                            None
                        else
                            Some (FileDescriptorRegistryDefect.SocketEventRegistrationTargetDead (portId, targetId))
                    )
            )

        let readyEntries =
            registry.Descriptions
            |> Map.toList
            |> List.collect (fun (portId, description) ->
                match description.Target with
                | OpenFileTarget.StandardStream _
                | OpenFileTarget.File _
                | OpenFileTarget.Directory _
                | OpenFileTarget.Socket _ -> []
                | OpenFileTarget.SocketEventPort portState ->
                    let unregistered =
                        portState.Ready
                        |> List.choose (fun (fd, targetId as key) ->
                            if Map.containsKey key portState.Registrations then
                                None
                            else
                                Some (
                                    FileDescriptorRegistryDefect.SocketEventReadyEntryUnregistered (
                                        portId,
                                        fd,
                                        targetId
                                    )
                                )
                        )

                    let duplicated =
                        portState.Ready
                        |> List.countBy id
                        |> List.choose (fun ((fd, targetId), count) ->
                            if count > 1 then
                                Some (
                                    FileDescriptorRegistryDefect.SocketEventReadyEntryDuplicated (
                                        portId,
                                        fd,
                                        targetId
                                    )
                                )
                            else
                                None
                        )

                    unregistered @ duplicated
            )

        dangling
        @ unreferenced
        @ freshness
        @ negativeOffsets
        @ writableDirectories
        @ conflicting
        @ duplicateSockets
        @ deadRegistrations
        @ readyEntries

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

        /// Rewrite one description in place, however unsoundly. Partial: the
        /// id must be live.
        let mapDescription
            (id : OpenFileDescriptionId)
            (f : OpenFileDescription -> OpenFileDescription)
            (registry : FileDescriptorRegistry)
            : FileDescriptorRegistry
            =
            { registry with
                Descriptions = Map.add id (f (Map.find id registry.Descriptions)) registry.Descriptions
            }

/// One entry in `UnixProcessState.OutputLog`: the role the process targeted (a
/// writable standard stream — stdout or stderr) and the byte payload of
/// that single `write(2)` call. Chunks are not coalesced across
/// calls because write boundaries matter for diagnostics (line
/// boundaries, prompt boundaries) and are what a real reader of the stream
/// could observe.
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

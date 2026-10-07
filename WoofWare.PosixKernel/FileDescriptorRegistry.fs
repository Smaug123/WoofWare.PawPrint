namespace WoofWare.PosixKernel

/// Identity of an open file description. Never visible to a process: no modelled
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
/// handshake creates. Never visible to a process.
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

/// Identity of a pipe. Never visible to the simulated process: `fstat` reports
/// the inode numbers the pipe table mints for it, which are a separate thing.
///
/// Its jobs are to be what the pipe table keys on, and what both ends of one
/// pipe share, so that they contend under `flock` (see `OpenFileObject.Pipe`).
[<Struct>]
type PipeId =
    | PipeId of value : int64

    override this.ToString () : string =
        match this with
        | PipeId value -> string<int64> value

/// Which end of a pipe a descriptor names.
[<RequireQualifiedAccess>]
type PipeEnd =
    /// The end `read(2)` takes bytes from: the first descriptor `pipe(2)` returns.
    | Read
    /// The end `write(2)` puts bytes into: the second descriptor `pipe(2)`
    /// returns.
    | Write

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
        /// The address the process's own `bind(2)` gave the socket, or `None`
        /// when the binding arose implicitly (a connect or listen minted it).
        /// The kernel state Linux calls SOCK_BINDADDR_LOCK: a Linux refusal
        /// delivery reverts `Endpoint`'s address to this (the wildcard when
        /// `None`) while keeping the port — measured for all three
        /// provenances — where Darwin keeps the resolved address.
        LockedAddress : uint32 option
        /// Whether the process's own `bind(2)` chose the port: true only when it
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
        /// is the accept-queue capacity check in `UnixConnection.connect`,
        /// which derives the flavour's admission bound from it — measured,
        /// Linux admits `backlog + 1` completed connections and Darwin exactly
        /// `backlog` — so this stores the input to that rule rather than a
        /// pre-computed capacity that would bake one flavour's arithmetic in.
        Backlog : int
        /// Completed connections not yet accepted, oldest first: `accept(2)`
        /// dequeues from the head. Measured on both flavours: accept returns
        /// connections in the order the connects completed.
        Queue : ConnectionId list
        /// Under Darwin, whether a close of the descriptor an `accept(2)` asleep
        /// on this listener was made through has ended every accept asleep on
        /// it. Never under Linux.
        ///
        /// It stays so for as long as the socket listens, a second `listen(2)`
        /// included: a later accept that finds a connection queued takes it,
        /// but one that would sleep answers `ECONNABORTED` as soon as anything
        /// wakes it, a connection or a signal.
        Drained : bool
    }

/// Whether a refused connection's error is still waiting to be reported. The
/// kernels keep it in a slot of its own beside the socket's state (Linux's
/// `sk_err`, Darwin's `so_error`), and so does `SocketPhase.Refused`.
[<RequireQualifiedAccess>]
type RefusalError =
    /// ECONNREFUSED is pending, and an `SO_ERROR` read will report it.
    | Pending
    /// The ECONNREFUSED has been reported, by a blocking connect or by an
    /// `SO_ERROR` read, and nothing is pending.
    | Reported

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
    /// A connect was refused.
    ///
    /// A non-blocking refusal leaves its ECONNREFUSED `Pending`, and an
    /// `SO_ERROR` read reports it and leaves it `Reported`. A blocking refusal
    /// reports it inline: Darwin's socket is then here with the error
    /// `Reported`, and Linux's goes to `Idle` instead.
    ///
    /// On Linux the next `connect(2)` resets the socket to `Idle`, wherever it
    /// is aimed, and answers ECONNREFUSED if the error is `Pending` and
    /// ECONNABORTED if it is `Reported`. Darwin's `connect(2)` never resets
    /// it: every one answers EISCONN, whatever the destination, and the socket
    /// stays here.
    | Refused of error : RefusalError
    /// A datagram socket's default peer, set by `connect(2)` on it. Filters
    /// nothing yet — no receive path exists — but re-connect re-targets it
    /// and a Linux `AF_UNSPEC` connect dissolves it back to `Idle`, both
    /// visible to the process through the return codes.
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
        | SocketPhase.Refused _
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
    /// A regular file, directory, or anything else `open(2)` returned a
    /// descriptor for, identified by the inode it resolved to at open time.
    /// Not by path: renaming or deleting the path leaves this description
    /// naming the same file, which is what a real kernel does.
    | File of inode : InodeNumber
    /// A file on Linux's `anon_inodefs` — today only an epoll instance, but
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
    /// So giving each epoll instance its own identity here would be wrong in a way a
    /// process can see: this kernel would grant two exclusive locks where Linux
    /// grants one. `OpenFileObject` is the contention key (see this type's
    /// summary), not a general-purpose identity — code that wants to tell two
    /// epoll instances apart wants `OpenFileDescriptionId`, which is what
    /// `ParkedEpollWait` keys on.
    ///
    /// Not the answer for a socket: Linux puts those on `sockfs` with an inode
    /// each, not on `anon_inodefs`. See `Socket`.
    | AnonymousInode
    /// One Darwin kqueue, identified by the open file description `kqueue()`
    /// made for it: a kqueue is reached through that description and the
    /// descriptors `dup(2)` makes for it, and through nothing else.
    ///
    /// Not `AnonymousInode`, which is a fact about Linux's `anon_inodefs`.
    /// Nothing contends on this: measured, `flock` on a kqueue is ENOTSUP for
    /// every operation, which `UnixDescriptor.flock` refuses to answer
    /// (`FLockRefusal.DarwinKqueue`) ahead of any contention test.
    | Kqueue of kqueue : OpenFileDescriptionId
    /// One socket. Carries an identity, and that is a `flock` fact rather than
    /// an aesthetic one — it is exactly where a socket differs from
    /// `AnonymousInode` above. Measured on Linux 6.18.5: two `socket(2)` calls
    /// report distinct `st_ino` (4127 and 4130, both `st_dev` 8), and
    /// `flock(LOCK_EX|LOCK_NB)` succeeds on *both*, where two epoll instances
    /// contend. A payload-free case here would grant one exclusive lock where
    /// Linux grants two.
    ///
    /// Darwin never reaches this: measured, `flock` on any socket there is
    /// ENOTSUP, which `UnixDescriptor.flock` refuses to answer
    /// (`FLockRefusal.DarwinSocket`) ahead of any contention test.
    | Socket of SocketId
    /// One pipe, both of its ends together. Measured on Linux 6.18.5: with
    /// `LOCK_EX|LOCK_NB` held through a pipe's read end, the same request
    /// through its write end is `EWOULDBLOCK`, while another pipe's read end
    /// takes the lock. So the pipe, not the end, is what contends.
    ///
    /// Darwin never reaches this: measured, `flock` on any pipe there is
    /// ENOTSUP, which `UnixDescriptor.flock` refuses to answer
    /// (`FLockRefusal.DarwinPipe`) ahead of any contention test.
    | Pipe of PipeId

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

    /// `EPOLLEXCLUSIVE`: wake one of several epoll instances registered on the same
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

/// One registration held by an epoll instance: what `epoll_ctl(2)` recorded
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

/// Everything one epoll instance holds: its interest table, and the ready list
/// `epoll_wait` drains.
type EpollState =
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

/// One of the two kqueue filters this library models: what a registration
/// watches its descriptor for.
[<RequireQualifiedAccess>]
type KqueueFilter =
    /// `EVFILT_READ`: something to read, or a connection to accept.
    | Read
    /// `EVFILT_WRITE`: room to write.
    | Write

/// One registration held by a kqueue: what `kevent(2)`'s `EV_ADD` recorded for
/// one (descriptor, filter) pair.
type KqueueRegistration =
    {
        /// Whether the registration was first added with `EV_CLEAR`. One that
        /// was is reported once each time something activates it; one that was
        /// not is reported by every wait while its filter stays ready. A later
        /// `EV_ADD` of the same pair cannot change this.
        Clear : bool
        /// Whether the registration was first added with `EV_RECEIPT`, which
        /// every event it reports carries in its flags. A later `EV_ADD` of the
        /// same pair cannot change this.
        Receipt : bool
        /// The caller's `udata` from the latest `EV_ADD` of this pair, which
        /// every event the registration reports carries back verbatim.
        UserData : uint64
        /// When this registration's first `EV_ADD` committed, as an ordinal
        /// from the kernel's counter (`UnixMachineState.NextEventRegistrationOrdinal`).
        /// One event can activate several registrations of the same socket
        /// and filter at once, made through different descriptors onto it, and
        /// they are queued newest-registered first.
        RegisteredAt : int64
    }

/// Everything one Darwin kqueue holds.
type KqueueState =
    {
        /// The process that created the kqueue, whose descriptor table its
        /// registrations' descriptor numbers are read in.
        ///
        /// A kqueue belongs to that process alone: Darwin closes a kqueue's
        /// descriptor in the child of a `fork(2)` (it carries `FD_CLOFORK`), and
        /// this library models no other way for a descriptor to reach another
        /// process, so no other process's descriptor names it.
        Owner : ProcessId
        /// Whether a `close(2)` has ended a `kevent` wait on this kqueue, which
        /// Darwin calls draining it.
        ///
        /// Closing a descriptor while a task is asleep in `kevent` through that
        /// same descriptor ends that task's wait, and every other task's wait on
        /// the kqueue, with `EBADF`; and from then on every wait on the kqueue
        /// through a descriptor that survived the close is `EBADF` at once.
        /// Closing a descriptor no waiter entered through changes nothing.
        Drained : bool
        /// The registrations, keyed as Darwin keys them: the descriptor number
        /// the registration was made through, in `Owner`'s descriptor table,
        /// and the filter. Closing that descriptor removes the registration,
        /// in every kqueue `Owner` owns, even while something else keeps what
        /// it named alive (another descriptor, or a call in flight that holds
        /// it); so every registration names an open descriptor, and destroying
        /// a description touches none.
        Registrations : Map<int * KqueueFilter, KqueueRegistration>
        /// The registrations activated and still to be reported, in the order
        /// a wait reports them; and, for a registration added without
        /// `EV_CLEAR`, one reported and still active. A wait walks this list in
        /// order, reporting each entry whose filter is still ready and dropping
        /// each that is not. Always a subset of `Registrations`, with no
        /// duplicates (`checkInvariants` states both).
        Active : (int * KqueueFilter) list
    }

/// What an open file description refers to, together with the state that only
/// that kind of object carries.
///
/// Distinct from `OpenFileObject`, the *identity*: the two differ by exactly
/// the file offset. Do not fold the offset into `OpenFileObject` — the `flock`
/// conflict test compares objects for equality, and two descriptions at
/// different offsets on one file must still contend.
/// `OpenFileDescription.object` is the projection back to identity.
[<RequireQualifiedAccess>]
type OpenFileTarget =
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
    /// A Linux epoll instance, handed out by `UnixPoll.epollCreate1` and
    /// destroyed when its last reference goes, as any open file is, which is
    /// why the instance is a descriptor at all rather than a separate kernel
    /// table.
    ///
    /// No offset, because Linux maintains none for it: measured, its `lseek`
    /// on an epoll descriptor is `noop_llseek`, returning 0 for any whence in
    /// 0..4 and any offset (`-1` and `INT64_MAX` alike). So there is no
    /// position for a caller to move or read.
    ///
    /// Carries the instance's interest table and ready list. The registration
    /// key is the **(fd number, open file description) pair** of the target;
    /// both halves are measured — an ADD through a `dup` of a registered
    /// target succeeds and creates a second registration, while an ADD
    /// through a `dup` of the *instance* answers EEXIST for an
    /// already-registered target, because the `dup` pair shares this
    /// description and so this table.
    | Epoll of state : EpollState
    /// A Darwin kqueue, handed out by `UnixKqueue.kqueue` and destroyed when
    /// its last reference goes, as any open file is.
    ///
    /// No offset: measured, Darwin's `lseek` on a kqueue is `ESPIPE`.
    | Kqueue of state : KqueueState
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
    /// One end of a pipe, handed out by `UnixPipe.pipe2`, or by the launch
    /// table a process is launched with (`ProcessLaunch.create`).
    ///
    /// No offset: measured, `lseek` on either end of a pipe is ESPIPE on both
    /// flavours.
    ///
    /// The pipe this names lives in `UnixMachineState.Pipes`, as a socket
    /// lives in the socket table, and for the same reason: a pipe is shared by
    /// the descriptions of both its ends, and outlives either. Whether an end
    /// is still open is whether any description names it.
    | Pipe of pipe : PipeId * pipeEnd : PipeEnd
    /// The node of a character device, opened: Linux's `/dev/null` or
    /// `/dev/urandom`.
    ///
    /// No offset, because the device has none to keep: measured on Linux,
    /// `lseek` answers 0 for every whence in 0..4 and every offset, and a read
    /// or write moves nothing a later call could see. `pread` and `pwrite`
    /// still check the position they are given.
    ///
    /// `device` is the device the inode stands for, which never changes, kept
    /// here so that an operation on the descriptor answers without consulting
    /// the filesystem; `UnixSystem.checkInvariants` holds the two in step.
    | CharacterDevice of inode : InodeNumber * device : CharacterDevice

/// Which transfers `open(2)`'s access mode permits: `O_RDONLY`, `O_WRONLY` or
/// `O_RDWR`.
///
/// A three-case DU rather than a readable/writable pair of booleans, because
/// this kernel opens no fourth: Darwin answers EINVAL for access mode 3, and
/// Linux's descriptor that permits neither is refused before one exists (see
/// `OpenRefusal.IoctlOnlyAccessMode`).
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

/// The status an open file description carries besides its access mode,
/// `O_NONBLOCK` and its `flock`: the rest of what `fcntl(F_GETFL)` reports.
///
/// Each field is kept only under the flavour whose `F_GETFL` reports it, and is
/// `false` under the other, so that two descriptions no syscall could tell
/// apart are equal: `OpenedDirectory` and `OpenedNoFollow` under Linux,
/// `Written` and `Flocked` under Darwin. `UnixDescriptor.fcntl` decides which
/// bit each is reported in.
///
/// `O_APPEND`, `O_ASYNC`, Linux's `O_DIRECT` and `O_NOATIME` are absent
/// because no description this kernel makes carries them: `open(2)` refuses
/// each of them, and so does `fcntl(F_SETFL)`.
type OpenFileStatus =
    {
        /// `O_SYNC`.
        Synchronous : bool
        /// `O_DSYNC`.
        DataSynchronous : bool
        /// Whether `open(2)` was given `O_DIRECTORY`. Linux only.
        OpenedDirectory : bool
        /// Whether `open(2)` was given `O_NOFOLLOW`. Linux only.
        OpenedNoFollow : bool
        /// Whether a `write(2)` or `pwrite(2)` through this description has
        /// returned having moved bytes, an `ftruncate(2)` through it has
        /// succeeded, or the `open(2)` that made it truncated a file that
        /// already existed. Darwin only.
        Written : bool
        /// Whether an `flock(2)` lock has ever been granted to this
        /// description. Releasing the lock does not clear it. Darwin only.
        Flocked : bool
    }

[<RequireQualifiedAccess>]
module OpenFileStatus =
    /// The status of a description nothing has yet happened to, made by a call
    /// asking for none of these flags.
    let none : OpenFileStatus =
        {
            Synchronous = false
            DataSynchronous = false
            OpenedDirectory = false
            OpenedNoFollow = false
            Written = false
            Flocked = false
        }

/// The kernel object a file descriptor points at: POSIX's "open file
/// description". Everything shared between file descriptors that `dup(2)`
/// produced belongs here.
type OpenFileDescription =
    {
        /// What this description refers to, and where in it.
        Target : OpenFileTarget
        /// Which transfers this description permits, from the access mode
        /// `open(2)` was given.
        AccessMode : FileAccessMode
        /// Whether `O_NONBLOCK` is set. On the description, not the
        /// descriptor — that is where POSIX keeps the status flags, and why a
        /// `dup(2)` pair shares them. Set through `fcntl(F_SETFL)`
        /// (`UnixDescriptor.fcntl`).
        ///
        /// Every modelled operation on every target honours a stored `true`,
        /// or refuses where it cannot, so a caller that consults this may trust
        /// it rather than re-checking the target kind.
        NonBlocking : bool
        /// The rest of its status flags.
        Status : OpenFileStatus
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
    /// Which kernel object the description `id` names — its *identity*, with
    /// the per-description position discarded. `description` is what `id`
    /// names in the table.
    ///
    /// `flock(2)` contention is decided on this: two descriptions contend
    /// exactly when they name the same object, whatever their offsets. Callers
    /// asking "are these the same file?" must compare these rather than the
    /// descriptions.
    ///
    let object (id : OpenFileDescriptionId) (description : OpenFileDescription) : OpenFileObject =
        match description.Target with
        | OpenFileTarget.File (inode, _)
        | OpenFileTarget.Directory (inode, _)
        // A device's node too: measured, two descriptions of `/dev/null`
        // contend under `flock`, as two of one regular file do.
        | OpenFileTarget.CharacterDevice (inode, _) -> OpenFileObject.File inode
        // Every epoll instance collapses to one object, because on Linux every
        // anon-inode file shares one inode and so they all contend under
        // `flock`. See `OpenFileObject.AnonymousInode`.
        | OpenFileTarget.Epoll _ -> OpenFileObject.AnonymousInode
        // A kqueue is reached only through the description `kqueue()` made
        // for it. See `OpenFileObject.Kqueue`.
        | OpenFileTarget.Kqueue _ -> OpenFileObject.Kqueue id
        // Each socket is its own object, unlike the epoll instances above: measured, two
        // sockets do not contend under `flock`. See `OpenFileObject.Socket`.
        | OpenFileTarget.Socket socketId -> OpenFileObject.Socket socketId
        // Both ends of a pipe are one object: measured, they contend under
        // `flock`. See `OpenFileObject.Pipe`.
        | OpenFileTarget.Pipe (pipeId, _) -> OpenFileObject.Pipe pipeId

/// The flags `fcntl(F_GETFD)` reports, which belong to one descriptor rather
/// than to the open file description it names: `dup(2)` gives the new
/// descriptor none of them.
///
/// Neither changes what any modelled syscall does, since this kernel models
/// neither `exec` nor `fork`; they are kept so that `F_GETFD` answers what
/// `F_SETFD`, `O_CLOEXEC` and their kin set.
type DescriptorFlags =
    {
        /// `FD_CLOEXEC`.
        CloseOnExec : bool
        /// `FD_CLOFORK`, which only Darwin has.
        CloseOnFork : bool
    }

[<RequireQualifiedAccess>]
module DescriptorFlags =
    /// Neither flag, which is what `dup(2)`, `dup2(2)` and every call not
    /// asked for a flag give a new descriptor.
    let none : DescriptorFlags =
        {
            CloseOnExec = false
            CloseOnFork = false
        }

/// One entry of the descriptor table: the open file description the
/// descriptor names, and the descriptor's own flags.
type private DescriptorEntry =
    {
        Description : OpenFileDescriptionId
        Flags : DescriptorFlags
    }

/// One process's descriptor table: which open file description each of its
/// descriptors names, and each descriptor's own flags (`DescriptorFlags`).
///
/// Holds no description, only the identity of one: the descriptions are the
/// machine's (`OpenFileTable`), as a real kernel's `struct file` is shared by
/// every descriptor table that names it. `FileDescriptorRegistry` reads the two
/// together.
type DescriptorTable =
    private
        {
            Fds : Map<int, DescriptorEntry>
        }

/// One entry of the machine's open file table: the description, how many
/// descriptors name it, and how many holds calls in flight have on it.
type private OpenFileEntry =
    {
        Description : OpenFileDescription
        /// How many descriptors name this description, in every descriptor table
        /// on the machine. Stored rather than derived because a descriptor table
        /// that holds one of them cannot see the others; `OpenFileTable.checkInvariants`
        /// holds it to the tables it is given.
        Descriptors : int
        /// How many holds syscalls in flight have on this description, in every
        /// process on the machine: one for each time a park names it
        /// (`ParkedSyscall.descriptions`). Stored rather than derived for the
        /// reason `Descriptors` is: a process's view cannot see another
        /// process's parks. `UnixSystem.checkInvariants` holds it to the parks.
        Holds : int
    }

/// The machine's open file descriptions, POSIX's "open file description" and
/// Linux's `struct file`: what every descriptor in every process names, by
/// identity, and which identity the next one gets.
///
/// A description is live exactly while some descriptor names it or something
/// outside the descriptor tables holds it, as a real kernel keeps a file while
/// anything holds a reference to it. Each description counts both: the
/// descriptors naming it (`OpenFileTable.descriptorCount`), and the holds of
/// syscalls in flight (`OpenFileTable.holdCount`, one for each time a park
/// names it, `ParkedSyscall.descriptions`). It is destroyed when both are
/// zero. `SCM_RIGHTS` messages and `mmap` would hold descriptions too, and are
/// not modelled.
///
/// An epoll instance's interest table and ready list, and a kqueue's
/// registrations, are the state of the description they are reached through
/// (`OpenFileTarget.Epoll`, `OpenFileTarget.Kqueue`), so they live here too.
type OpenFileTable =
    private
        {
            Entries : Map<OpenFileDescriptionId, OpenFileEntry>
            /// The identity the next description will be given. Stored and
            /// monotonic rather than derived as one past the highest live id,
            /// which would reuse the identity of a closed description. Nothing
            /// a process sees could tell the difference — the id is never
            /// reported by any syscall — but a replay trace could.
            /// `VirtualFileSystem.NextInode` is stored for the stronger version
            /// of this reason, inode reuse being visible to a process.
            NextId : OpenFileDescriptionId
        }

/// One process's descriptor table together with the machine's open file
/// descriptions those descriptors point at: the two halves of a Unix file
/// descriptor as one process sees them. `UnixSystem.fileDescriptors` reads it.
///
/// The indirection is POSIX's, not an implementation detail: a file descriptor
/// is a per-process integer *naming* an open file description, and `dup(2)`
/// allocates a fresh descriptor pointing at the same description. State that
/// belongs to the description (offset, status flags) is therefore shared by
/// every descriptor that names it, while the per-descriptor flags — `FD_CLOEXEC`,
/// to which POSIX-2024 adds `FD_CLOFORK` — are not (`DescriptorFlags`).
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
            /// The process's descriptor table.
            Descriptors : DescriptorTable
            /// The machine's open file descriptions, every one the descriptors
            /// name and any others the machine holds.
            OpenFiles : OpenFileTable
        }


/// A call that would put a descriptor at `Descriptor`, at or above `Bound`.
///
/// This kernel models no `RLIMIT_NOFILE`. It assumes the process's soft limit
/// is at least `Bound` (`SimulatedUnixPlatform.descriptorBound`), so it can
/// answer every call whose descriptors all lie below it; one that would reach
/// `Bound` is answered `EMFILE`, `EINVAL` or `EBADF` by a process whose limit is
/// `Bound` and succeeds for one whose limit is higher, so this kernel refuses
/// it.
type DescriptorLimitRefusal =
    {
        /// The lowest number the call could have given its descriptor.
        Descriptor : int
        /// The bound it reaches.
        Bound : int
    }

[<RequireQualifiedAccess>]
module DescriptorLimitRefusal =
    /// What this kernel knows about why it cannot answer.
    let describe (refusal : DescriptorLimitRefusal) : string =
        $"the call would put a descriptor at %d{refusal.Descriptor}, at or above %d{refusal.Bound}. This kernel assumes the process's RLIMIT_NOFILE soft limit is at least %d{refusal.Bound}, the default a process starts with, and models no higher one: a process whose limit is %d{refusal.Bound} gets EMFILE, EINVAL or EBADF here, and one whose limit is higher gets the descriptor."

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
/// kernel could produce, or an `OpenFileTable` the open file descriptions of one.
/// `FileDescriptorRegistry.checkInvariants` and `OpenFileTable.checkInvariants`
/// return these.
[<RequireQualifiedAccess>]
type FileDescriptorRegistryDefect =
    /// A live descriptor names a description that is not present. Every lookup
    /// through this descriptor would fail, which no kernel permits.
    | DanglingFd of fd : int * description : OpenFileDescriptionId
    /// A description records `recorded` descriptors naming it, where the
    /// descriptor tables hold `naming`. The count is what decides when the last
    /// descriptor's `close` destroys the description, so one too high keeps a
    /// description no descriptor can reach, and one too low destroys a
    /// description that a descriptor still names.
    | DescriptorCountMismatch of description : OpenFileDescriptionId * recorded : int * naming : int
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
    /// An epoll instance's interest table registers an open file description
    /// that no longer exists. Linux removes these at file-release time, which
    /// is `close`'s sweep here, so a survivor is a leak — invisible to every
    /// syscall (no fd can name the dead description again) but exactly what
    /// the readiness wake must never deliver from.
    | EpollRegistrationTargetDead of epoll : OpenFileDescriptionId * target : OpenFileDescriptionId
    /// An epoll instance's ready list holds an entry its interest table does
    /// not register. Every path that removes a registration (DEL, and close's
    /// sweep) removes its pending entry in the same step, so a survivor would
    /// deliver an event from a corpse.
    | EpollReadyEntryUnregistered of epoll : OpenFileDescriptionId * key : int * target : OpenFileDescriptionId
    /// An epoll instance's ready list holds the same entry twice. A pending
    /// registration keeps its place rather than being re-queued (measured:
    /// a re-signal does not move it), so a duplicate would deliver one edge
    /// twice.
    | EpollReadyEntryDuplicated of epoll : OpenFileDescriptionId * key : int * target : OpenFileDescriptionId
    /// A kqueue holds a registration made through a descriptor that is not
    /// open. Closing a descriptor removes every registration made through it,
    /// so the registration has outlived its descriptor.
    | KqueueRegistrationThroughClosedDescriptor of kqueue : OpenFileDescriptionId * fd : int * filter : KqueueFilter
    /// A kqueue holds a registration made through a descriptor that names
    /// something other than a socket, which `kevent` never registers.
    | KqueueRegistrationNotOnSocket of
        kqueue : OpenFileDescriptionId *
        fd : int *
        filter : KqueueFilter *
        target : OpenFileTarget
    /// A kqueue's queue of activated registrations holds an entry it does not
    /// register. Every path that removes a registration removes its queue entry
    /// in the same step.
    | KqueueActiveEntryUnregistered of kqueue : OpenFileDescriptionId * fd : int * filter : KqueueFilter
    /// A kqueue's queue of activated registrations holds the same entry twice.
    /// An activated registration keeps its place when activated again, so a
    /// duplicate would report one event twice.
    | KqueueActiveEntryDuplicated of kqueue : OpenFileDescriptionId * fd : int * filter : KqueueFilter


[<RequireQualifiedAccess>]
module OpenFileTable =
    /// A machine with no open file description, whose first gets identity 0.
    let internal empty : OpenFileTable =
        {
            Entries = Map.empty
            NextId = OpenFileDescriptionId 0L
        }

    /// Every live open file description on the machine.
    ///
    /// Builds the map afresh, in time and space linear in the number of live
    /// descriptions; a caller that wants one description wants `tryFind`.
    let descriptions (table : OpenFileTable) : Map<OpenFileDescriptionId, OpenFileDescription> =
        table.Entries |> Map.map (fun _ entry -> entry.Description)

    /// The description `id` names, if it is live.
    let tryFind (id : OpenFileDescriptionId) (table : OpenFileTable) : OpenFileDescription option =
        Map.tryFind id table.Entries |> Option.map (fun entry -> entry.Description)

    /// Every live description and its identity, in identity order, for a walk
    /// that would otherwise build `descriptions` only to read it once.
    let internal toSeq (table : OpenFileTable) : (OpenFileDescriptionId * OpenFileDescription) seq =
        table.Entries |> Map.toSeq |> Seq.map (fun (id, entry) -> id, entry.Description)

    /// The live description `id` names. Loudly partial: `operation`, named in
    /// the message, holds an identity it resolved moments ago.
    let internal get (operation : string) (id : OpenFileDescriptionId) (table : OpenFileTable) : OpenFileDescription =
        match tryFind id table with
        | Some description -> description
        | None ->
            failwith
                $"%s{operation}: open file description %O{id} is not present in the table (this is a bug in the caller, which resolved it moments ago)."

    /// How many descriptors name the description `id`, in every descriptor
    /// table on the machine, if it is live. Zero for a description only a call
    /// in flight holds.
    let descriptorCount (id : OpenFileDescriptionId) (table : OpenFileTable) : int option =
        Map.tryFind id table.Entries |> Option.map (fun entry -> entry.Descriptors)

    /// How many holds syscalls in flight have on the description `id`, in every
    /// process on the machine, if it is live: one for each time a park names it
    /// (`ParkedSyscall.descriptions`).
    let holdCount (id : OpenFileDescriptionId) (table : OpenFileTable) : int option =
        Map.tryFind id table.Entries |> Option.map (fun entry -> entry.Holds)

    /// A fresh description, named by the one descriptor its creator is about to
    /// install, and its identity.
    let internal create
        (description : OpenFileDescription)
        (table : OpenFileTable)
        : OpenFileDescriptionId * OpenFileTable
        =
        let id = table.NextId
        let (OpenFileDescriptionId raw) = id

        id,
        {
            Entries =
                Map.add
                    id
                    {
                        Description = description
                        Descriptors = 1
                        Holds = 0
                    }
                    table.Entries
            NextId = OpenFileDescriptionId (raw + 1L)
        }

    /// Rewrite the entry of the live description `id`. `operation` names the
    /// caller for the message if it is not live.
    let private mapEntry
        (operation : string)
        (id : OpenFileDescriptionId)
        (f : OpenFileEntry -> OpenFileEntry)
        (table : OpenFileTable)
        : OpenFileTable
        =
        match Map.tryFind id table.Entries with
        | None ->
            failwith
                $"OpenFileTable.%s{operation}: open file description %O{id} is not present in the table (this is a bug in the caller, which resolved it moments ago)."
        | Some entry ->
            { table with
                Entries = Map.add id (f entry) table.Entries
            }

    /// Rewrite the live description `id`. `operation` names the caller for the
    /// message if it is not live.
    let internal mapDescription
        (operation : string)
        (id : OpenFileDescriptionId)
        (f : OpenFileDescription -> OpenFileDescription)
        (table : OpenFileTable)
        : OpenFileTable
        =
        table
        |> mapEntry
            operation
            id
            (fun entry ->
                { entry with
                    Description = f entry.Description
                }
            )

    /// One more descriptor names the live description `id`.
    let internal retain (id : OpenFileDescriptionId) (table : OpenFileTable) : OpenFileTable =
        table
        |> mapEntry
            "retain"
            id
            (fun entry ->
                { entry with
                    Descriptors = entry.Descriptors + 1
                }
            )

    /// One fewer descriptor names the live description `id`. Loudly partial on
    /// a description no descriptor names.
    let internal release (id : OpenFileDescriptionId) (table : OpenFileTable) : OpenFileTable =
        table
        |> mapEntry
            "release"
            id
            (fun entry ->
                if entry.Descriptors <= 0 then
                    failwith
                        $"OpenFileTable.release: open file description %O{id} records %d{entry.Descriptors} descriptors naming it, so none can close (this is a bug in this library)."

                { entry with
                    Descriptors = entry.Descriptors - 1
                }
            )

    /// A syscall going to sleep takes one more hold on the live description
    /// `id`.
    let internal hold (id : OpenFileDescriptionId) (table : OpenFileTable) : OpenFileTable =
        table
        |> mapEntry
            "hold"
            id
            (fun entry ->
                { entry with
                    Holds = entry.Holds + 1
                }
            )

    /// A syscall in flight lets go of one hold on the live description `id`.
    /// Loudly partial on a description no call holds. Destroys nothing:
    /// `destroyIfUnreferenced` is what frees a description nothing references.
    let internal releaseHold (id : OpenFileDescriptionId) (table : OpenFileTable) : OpenFileTable =
        table
        |> mapEntry
            "releaseHold"
            id
            (fun entry ->
                if entry.Holds <= 0 then
                    failwith
                        $"OpenFileTable.releaseHold: open file description %O{id} records %d{entry.Holds} holds by calls in flight, so none can be let go of (this is a bug in this library)."

                { entry with
                    Holds = entry.Holds - 1
                }
            )

    /// Rewrite the status of the description `id`. Partial: `id` must be
    /// live, which every caller has just established.
    let internal mapStatus
        (id : OpenFileDescriptionId)
        (f : OpenFileStatus -> OpenFileStatus)
        (table : OpenFileTable)
        : OpenFileTable
        =
        table
        |> mapDescription
            "mapStatus"
            id
            (fun description ->
                { description with
                    Status = f description.Status
                }
            )

    /// Remove `id` from the table, and from every epoll instance's interest
    /// table. `id` must be live and no descriptor may name it.
    let private destroy (id : OpenFileDescriptionId) (table : OpenFileTable) : OpenFileTable =
        // A destroyed description also vanishes from every epoll instance's
        // interest table, which is what Linux does at file-release time
        // (`eventpoll_release`). No syscall can tell the difference — the dead
        // pair's key can never be probed again, since no fd names the
        // description — but the readiness wake must not deliver from a corpse,
        // so the tables stay truthful now and `checkInvariants` states it.
        //
        // A kqueue's registrations need no purge here: Darwin keys each by the
        // descriptor it was made through and drops it when that descriptor
        // closes (`FileDescriptorRegistry.dropDescriptor`), not when the file
        // is released, so every registration names an open descriptor, which
        // names a live description, never this one
        // (`KqueueRegistrationThroughClosedDescriptor`).
        let entries =
            Map.remove id table.Entries
            |> Map.map (fun _ entry ->
                match entry.Description.Target with
                | OpenFileTarget.Epoll epollState ->
                    { entry with
                        Description =
                            { entry.Description with
                                Target =
                                    OpenFileTarget.Epoll
                                        {
                                            Registrations =
                                                epollState.Registrations
                                                |> Map.filter (fun (_, target) _ -> target <> id)
                                            Ready = epollState.Ready |> List.filter (fun (_, target) -> target <> id)
                                        }
                            }
                    }
                | OpenFileTarget.Kqueue _
                | OpenFileTarget.File _
                | OpenFileTarget.Directory _
                | OpenFileTarget.Socket _
                | OpenFileTarget.CharacterDevice _
                | OpenFileTarget.Pipe _ -> entry
            )

        { table with
            Entries = entries
        }

    /// Destroy the description `id` if nothing references it any more: no
    /// descriptor in any process names it, and no syscall in flight holds it
    /// (`holdCount`). Reports the description it destroyed, if it did; a
    /// description already gone, or still referenced, is left as it is and
    /// answers `None`.
    ///
    /// For a holder that has just let go of `id`. Like
    /// `FileDescriptorRegistry.dropDescriptor`, it releases nothing the
    /// description referenced.
    let destroyIfUnreferenced
        (id : OpenFileDescriptionId)
        (table : OpenFileTable)
        : OpenFileTable * OpenFileDescription option
        =
        match Map.tryFind id table.Entries with
        | None -> table, None
        | Some entry ->
            if entry.Descriptors > 0 || entry.Holds > 0 then
                table, None
            else
                destroy id table, Some entry.Description

    /// May two *different* open file descriptions on one file hold these two
    /// locks at the same time? Symmetric, so `checkInvariants` can apply it to
    /// an unordered pair.
    let private locksConflict (a : FlockMode) (b : FlockMode) : bool =
        match a, b with
        | FlockMode.Shared, FlockMode.Shared -> false
        | _, _ -> true

    /// Would an `flock` acquisition of `mode`, by the open file description
    /// `requester` onto `object`, have to wait? True exactly when some *other*
    /// description on the machine naming `object` holds a lock that could not be
    /// held alongside it.
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
        (table : OpenFileTable)
        : bool
        =
        table.Entries
        |> Map.exists (fun otherId (other : OpenFileEntry) ->
            otherId <> requester
            // Identity, not the whole description: two descriptions on one
            // file contend however far apart their offsets are.
            && OpenFileDescription.object otherId other.Description = object
            && (
                match other.Description.Flock with
                | None -> false
                | Some held -> locksConflict mode held
            )
        )

    /// `flock(2)` on the open file description directly, for a caller that holds
    /// one rather than a descriptor.
    ///
    /// The primitive: `FileDescriptorRegistry.flock` is this with a descriptor
    /// resolved first, and everything that docstring says about conversion,
    /// contention and the dropped old lock is decided here.
    ///
    /// A caller finishing a *parked* acquisition wants this rather than the
    /// by-fd version, and not as a convenience: descriptor numbers are reused as
    /// soon as they are free, so the number a waiter parked on can name an
    /// entirely different object by the time the lock becomes available.
    ///
    /// Loudly partial in `id`, which no process can reach: a
    /// description a client still holds an identity for is one it must not have
    /// let `close` destroy.
    let internal flockOn
        (id : OpenFileDescriptionId)
        (request : FlockRequest)
        (table : OpenFileTable)
        : OpenFileTable * FlockError option
        =
        let description =
            match tryFind id table with
            | Some description -> description
            | None ->
                failwith
                    $"open file description %O{id} is not present in the table (this is a bug in the caller of OpenFileTable.flockOn, which holds the identity of a description it let close destroy)"

        let withFlock (flock : FlockMode option) : OpenFileTable =
            table
            |> mapDescription
                "flockOn"
                id
                (fun description ->
                    { description with
                        Flock = flock
                    }
                )

        match request with
        | FlockRequest.Release -> withFlock None, None
        | FlockRequest.Acquire mode ->

        let blocked =
            flockConflicts (OpenFileDescription.object id description) id mode table

        if blocked then
            // The old lock is gone either way — see the note on
            // `FileDescriptorRegistry.flock`.
            withFlock None, Some FlockError.WouldBlock
        else
            withFlock (Some mode), None

    /// Mark the kqueue the open file description `kqueue` names as drained
    /// (see `KqueueState.Drained`). Loudly partial on a dead or non-kqueue
    /// description: the caller has just resolved it as a kqueue.
    let drainKqueue (kqueue : OpenFileDescriptionId) (table : OpenFileTable) : OpenFileTable =
        match tryFind kqueue table with
        | Some {
                   Target = OpenFileTarget.Kqueue state
               } ->
            table
            |> mapDescription
                "drainKqueue"
                kqueue
                (fun description ->
                    { description with
                        Target =
                            OpenFileTarget.Kqueue
                                { state with
                                    Drained = true
                                }
                    }
                )
        | other ->
            failwith
                $"drainKqueue: %O{kqueue} names %A{other} rather than a live kqueue; the caller resolved it as one moments ago (this is a bug in the caller of OpenFileTable.drainKqueue)."

    /// Replace the state of the kqueue the open file description `kqueue`
    /// names with `state`. Loudly partial on a dead or non-kqueue description:
    /// the caller has just resolved it as a kqueue.
    ///
    /// Checks nothing about `state`; `FileDescriptorRegistry.checkInvariants`
    /// states what a kqueue's state must satisfy.
    let setKqueueState (kqueue : OpenFileDescriptionId) (state : KqueueState) (table : OpenFileTable) : OpenFileTable =
        match tryFind kqueue table with
        | Some {
                   Target = OpenFileTarget.Kqueue _
               } ->
            table
            |> mapDescription
                "setKqueueState"
                kqueue
                (fun description ->
                    { description with
                        Target = OpenFileTarget.Kqueue state
                    }
                )
        | other ->
            failwith
                $"setKqueueState: %O{kqueue} names %A{other} rather than a live kqueue; the caller resolved it as one moments ago (this is a bug in the caller of OpenFileTable.setKqueueState)."

    /// Rewrite the state of the epoll instance `epollId` names. Loudly partial
    /// on a dead or non-epoll description: every caller resolved it as an
    /// epoll instance moments ago, so either means it wrote against a different
    /// table than the one it read. `operation` names the caller for that message.
    let private mapEpollState
        (operation : string)
        (epollId : OpenFileDescriptionId)
        (f : EpollState -> EpollState)
        (table : OpenFileTable)
        : OpenFileTable
        =
        match tryFind epollId table with
        | None ->
            failwith
                $"%s{operation}: %O{epollId} names no live open file description; the caller resolved it moments ago, so this is a bug in the caller of OpenFileTable.%s{operation}."
        | Some description ->

        match description.Target with
        | OpenFileTarget.Kqueue _
        | OpenFileTarget.File _
        | OpenFileTarget.Directory _
        | OpenFileTarget.Socket _
        | OpenFileTarget.CharacterDevice _
        | OpenFileTarget.Pipe _ ->
            failwith
                $"%s{operation}: %O{epollId} is not an epoll instance; the caller resolved it as one moments ago, so this is a bug in the caller of OpenFileTable.%s{operation}."
        | OpenFileTarget.Epoll epollState ->

        table
        |> mapDescription
            operation
            epollId
            (fun description ->
                { description with
                    Target = OpenFileTarget.Epoll (f epollState)
                }
            )

    /// Record `registration` under `key` in the interest table of the epoll instance
    /// `epollId` names: the table half of a committed `EPOLL_CTL_ADD`.
    ///
    /// The key is epoll's own, the target's (fd number, open file description)
    /// pair. Loudly partial on a key already registered, which `epoll_ctl`
    /// answers `EEXIST` for before it reaches the table; that answer is the
    /// caller's (`UnixPoll.epollCtl`).
    let internal addEpollRegistration
        (epollId : OpenFileDescriptionId)
        (key : int * OpenFileDescriptionId)
        (registration : EpollRegistration)
        (table : OpenFileTable)
        : OpenFileTable
        =
        table
        |> mapEpollState
            "addEpollRegistration"
            epollId
            (fun epollState ->
                if Map.containsKey key epollState.Registrations then
                    failwith
                        $"addEpollRegistration: %A{key} is already registered with epoll instance %O{epollId}, which epoll_ctl answers EEXIST for (this is a bug in the caller of OpenFileTable.addEpollRegistration)."

                { epollState with
                    Registrations = Map.add key registration epollState.Registrations
                }
            )

    /// Replace the stored event mask and data of the registration under `key`:
    /// the table half of a committed `EPOLL_CTL_MOD`. The registration keeps
    /// its `RegisteredAt`, and a pending entry keeps its place in the ready
    /// list (measured, `order3.c` row L).
    ///
    /// Loudly partial on a key not registered, which `epoll_ctl` answers
    /// `ENOENT` for before it reaches the table.
    let internal modifyEpollRegistration
        (epollId : OpenFileDescriptionId)
        (key : int * OpenFileDescriptionId)
        (events : uint32)
        (data : uint64)
        (table : OpenFileTable)
        : OpenFileTable
        =
        table
        |> mapEpollState
            "modifyEpollRegistration"
            epollId
            (fun epollState ->
                match Map.tryFind key epollState.Registrations with
                | None ->
                    failwith
                        $"modifyEpollRegistration: %A{key} is not registered with epoll instance %O{epollId}, which epoll_ctl answers ENOENT for (this is a bug in the caller of OpenFileTable.modifyEpollRegistration)."
                | Some existing ->
                    { epollState with
                        Registrations =
                            Map.add
                                key
                                { existing with
                                    Events = events
                                    Data = data
                                }
                                epollState.Registrations
                    }
            )

    /// Remove the registration under `key`, and its pending entry if it has
    /// one: the table half of a committed `EPOLL_CTL_DEL`.
    ///
    /// Loudly partial on a key not registered, which `epoll_ctl` answers
    /// `ENOENT` for before it reaches the table.
    let internal removeEpollRegistration
        (epollId : OpenFileDescriptionId)
        (key : int * OpenFileDescriptionId)
        (table : OpenFileTable)
        : OpenFileTable
        =
        table
        |> mapEpollState
            "removeEpollRegistration"
            epollId
            (fun epollState ->
                if not (Map.containsKey key epollState.Registrations) then
                    failwith
                        $"removeEpollRegistration: %A{key} is not registered with epoll instance %O{epollId}, which epoll_ctl answers ENOENT for (this is a bug in the caller of OpenFileTable.removeEpollRegistration)."

                {
                    Registrations = Map.remove key epollState.Registrations
                    Ready = epollState.Ready |> List.filter (fun k -> k <> key)
                }
            )

    /// Append `key` to the ready list of the epoll instance `epollId` names. The caller
    /// has decided the entry belongs there (an ADD/MOD found the target ready,
    /// or the driver signalled it); this only performs the append, and it is
    /// loudly partial on a key that is not registered or is already pending —
    /// both would mean the caller's decision was made against a different
    /// table than the one being written.
    let internal appendEpollReady
        (epollId : OpenFileDescriptionId)
        (key : int * OpenFileDescriptionId)
        (table : OpenFileTable)
        : OpenFileTable
        =
        table
        |> mapEpollState
            "appendEpollReady"
            epollId
            (fun epollState ->
                if not (Map.containsKey key epollState.Registrations) then
                    failwith
                        $"appendEpollReady: %A{key} is not registered with epoll instance %O{epollId}, so it cannot become pending on it (this is a bug in the caller of OpenFileTable.appendEpollReady)."

                if List.contains key epollState.Ready then
                    failwith
                        $"appendEpollReady: %A{key} is already pending on epoll instance %O{epollId}; a pending entry keeps its place rather than being re-queued, so the caller should not have asked (this is a bug in the caller of OpenFileTable.appendEpollReady)."

                { epollState with
                    Ready = epollState.Ready @ [ key ]
                }
            )

    /// Replace the ready list of the epoll instance `epollId` names — delivery's
    /// write-back once a walk has consumed a prefix. Loudly partial on a
    /// dead or non-epoll description, on an entry the interest table does not
    /// register, and on a duplicate: the caller derived `ready` from the
    /// epoll instance's own state moments ago, so any of those means it wrote against
    /// a different table than the one it read.
    let internal setEpollReady
        (epollId : OpenFileDescriptionId)
        (ready : (int * OpenFileDescriptionId) list)
        (table : OpenFileTable)
        : OpenFileTable
        =
        table
        |> mapEpollState
            "setEpollReady"
            epollId
            (fun epollState ->
                for key in ready do
                    if not (Map.containsKey key epollState.Registrations) then
                        failwith
                            $"setEpollReady: %A{key} is not registered with epoll instance %O{epollId} (this is a bug in the caller of OpenFileTable.setEpollReady, which derived the list from a different table)."

                if List.length (List.distinct ready) <> List.length ready then
                    failwith
                        $"setEpollReady: the ready list for epoll instance %O{epollId} repeats an entry (this is a bug in the caller of OpenFileTable.setEpollReady, which derived the list from a different table)."

                { epollState with
                    Ready = ready
                }
            )

    /// The driver signalled every description in `naming` (all of one
    /// socket's descriptions): on every epoll instance on the machine, each
    /// registration targeting one of them becomes pending unless it already is.
    /// `wakeKey` is what the waker carried, in Linux's `<sys/epoll.h>`
    /// numbering, and the two kinds are both measured:
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
    let internal signalEpollInstances
        (naming : Set<OpenFileDescriptionId>)
        (wakeKey : uint32 option)
        (table : OpenFileTable)
        : OpenFileTable
        =
        let entries =
            table.Entries
            |> Map.map (fun _ entry ->
                match entry.Description.Target with
                | OpenFileTarget.Kqueue _
                | OpenFileTarget.File _
                | OpenFileTarget.Directory _
                | OpenFileTarget.Socket _
                | OpenFileTarget.CharacterDevice _
                | OpenFileTarget.Pipe _ -> entry
                | OpenFileTarget.Epoll epollState ->
                    let entering =
                        epollState.Registrations
                        |> Map.toList
                        |> List.filter (fun ((_, targetId as key), registration) ->
                            Set.contains targetId naming
                            && not (List.contains key epollState.Ready)
                            && (
                                match wakeKey with
                                | None -> true
                                | Some key -> key &&& registration.Events <> 0u
                            )
                        )
                        |> List.sortByDescending (fun (_, registration) -> registration.RegisteredAt)
                        |> List.map fst

                    match entering with
                    | [] -> entry
                    | entering ->
                        { entry with
                            Description =
                                { entry.Description with
                                    Target =
                                        OpenFileTarget.Epoll
                                            { epollState with
                                                Ready = epollState.Ready @ entering
                                            }
                                }
                        }
            )

        { table with
            Entries = entries
        }

    /// Every way in which `table` fails to be the open file descriptions of a
    /// machine whose descriptor tables are `descriptorTables`, all of them:
    /// its own rules, and each description's count of the descriptors naming
    /// it against those tables.
    ///
    /// The rules that relate a descriptor *number* to a description are one
    /// process's, so they are `FileDescriptorRegistry.checkInvariants`'s.
    let checkInvariants
        (descriptorTables : DescriptorTable list)
        (table : OpenFileTable)
        : FileDescriptorRegistryDefect list
        =
        let descriptions = descriptions table

        let naming =
            descriptorTables
            |> List.collect (fun descriptors ->
                descriptors.Fds |> Map.toList |> List.map (fun (_, entry) -> entry.Description)
            )
            |> List.countBy id
            |> Map.ofList

        let counts =
            table.Entries
            |> Map.toList
            |> List.choose (fun (id, entry) ->
                let actual = Map.tryFind id naming |> Option.defaultValue 0

                if actual = entry.Descriptors then
                    None
                else
                    Some (FileDescriptorRegistryDefect.DescriptorCountMismatch (id, entry.Descriptors, actual))
            )

        let freshness =
            descriptions
            |> Map.toList
            |> List.map fst
            |> List.filter (fun id -> id >= table.NextId)
            |> List.map (fun id -> FileDescriptorRegistryDefect.NextIdNotFresh (table.NextId, id))

        let negativeOffsets =
            descriptions
            |> Map.toList
            |> List.choose (fun (id, description) ->
                match description.Target with
                | OpenFileTarget.Kqueue _
                | OpenFileTarget.Epoll _
                | OpenFileTarget.Socket _
                | OpenFileTarget.CharacterDevice _
                | OpenFileTarget.Pipe _ -> None
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
            descriptions
            |> Map.toList
            |> List.choose (fun (id, description) ->
                match description.Target with
                | OpenFileTarget.Directory _ when FileAccessMode.permitsWrite description.AccessMode ->
                    Some (FileDescriptorRegistryDefect.WritableDirectory id)
                | OpenFileTarget.Directory _
                | OpenFileTarget.Kqueue _
                | OpenFileTarget.Epoll _
                | OpenFileTarget.Socket _
                | OpenFileTarget.CharacterDevice _
                | OpenFileTarget.Pipe _
                | OpenFileTarget.File _ -> None
            )

        let locked =
            descriptions
            |> Map.toList
            |> List.choose (fun (id, description) ->
                description.Flock
                |> Option.map (fun mode -> id, OpenFileDescription.object id description, mode)
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
            descriptions
            |> Map.toList
            |> List.choose (fun (id, description) ->
                match description.Target with
                | OpenFileTarget.Kqueue _
                | OpenFileTarget.Epoll _
                | OpenFileTarget.File _
                | OpenFileTarget.Directory _
                | OpenFileTarget.CharacterDevice _
                | OpenFileTarget.Pipe _ -> None
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
            descriptions
            |> Map.toList
            |> List.collect (fun (epollId, description) ->
                match description.Target with
                | OpenFileTarget.Kqueue _
                | OpenFileTarget.File _
                | OpenFileTarget.Directory _
                | OpenFileTarget.Socket _
                | OpenFileTarget.CharacterDevice _
                | OpenFileTarget.Pipe _ -> []
                | OpenFileTarget.Epoll epollState ->
                    epollState.Registrations
                    |> Map.toList
                    |> List.choose (fun ((_, targetId), _) ->
                        if Map.containsKey targetId descriptions then
                            None
                        else
                            Some (FileDescriptorRegistryDefect.EpollRegistrationTargetDead (epollId, targetId))
                    )
            )

        let readyEntries =
            descriptions
            |> Map.toList
            |> List.collect (fun (epollId, description) ->
                match description.Target with
                | OpenFileTarget.Kqueue _
                | OpenFileTarget.File _
                | OpenFileTarget.Directory _
                | OpenFileTarget.Socket _
                | OpenFileTarget.CharacterDevice _
                | OpenFileTarget.Pipe _ -> []
                | OpenFileTarget.Epoll epollState ->
                    let unregistered =
                        epollState.Ready
                        |> List.choose (fun (fd, targetId as key) ->
                            if Map.containsKey key epollState.Registrations then
                                None
                            else
                                Some (
                                    FileDescriptorRegistryDefect.EpollReadyEntryUnregistered (epollId, fd, targetId)
                                )
                        )

                    let duplicated =
                        epollState.Ready
                        |> List.countBy id
                        |> List.choose (fun ((fd, targetId), count) ->
                            if count > 1 then
                                Some (FileDescriptorRegistryDefect.EpollReadyEntryDuplicated (epollId, fd, targetId))
                            else
                                None
                        )

                    unregistered @ duplicated
            )

        let kqueueEntries =
            descriptions
            |> Map.toList
            |> List.collect (fun (kqueue, description) ->
                match description.Target with
                | OpenFileTarget.Epoll _
                | OpenFileTarget.File _
                | OpenFileTarget.Directory _
                | OpenFileTarget.Socket _
                | OpenFileTarget.CharacterDevice _
                | OpenFileTarget.Pipe _ -> []
                | OpenFileTarget.Kqueue state ->
                    let unregistered =
                        state.Active
                        |> List.choose (fun (fd, filter as key) ->
                            if Map.containsKey key state.Registrations then
                                None
                            else
                                Some (FileDescriptorRegistryDefect.KqueueActiveEntryUnregistered (kqueue, fd, filter))
                        )

                    let duplicated =
                        state.Active
                        |> List.countBy id
                        |> List.choose (fun ((fd, filter), count) ->
                            if count > 1 then
                                Some (FileDescriptorRegistryDefect.KqueueActiveEntryDuplicated (kqueue, fd, filter))
                            else
                                None
                        )

                    unregistered @ duplicated
            )

        counts
        @ freshness
        @ negativeOffsets
        @ writableDirectories
        @ conflicting
        @ duplicateSockets
        @ deadRegistrations
        @ readyEntries
        @ kqueueEntries

[<RequireQualifiedAccess>]
module FileDescriptorRegistry =
    /// A table entry naming `id`, with neither descriptor flag.
    let private unflagged (id : OpenFileDescriptionId) : DescriptorEntry =
        {
            Description = id
            Flags = DescriptorFlags.none
        }

    /// Which description `fd` names in `fds`, if `fd` is live.
    let private named (fd : int) (fds : Map<int, DescriptorEntry>) : OpenFileDescriptionId option =
        Map.tryFind fd fds |> Option.map (fun entry -> entry.Description)

    /// The process's descriptor table `descriptors`, read against the machine's
    /// open file descriptions `openFiles`.
    let internal ofTables (descriptors : DescriptorTable) (openFiles : OpenFileTable) : FileDescriptorRegistry =
        {
            Descriptors = descriptors
            OpenFiles = openFiles
        }

    /// The process's descriptor table.
    let internal descriptorTable (registry : FileDescriptorRegistry) : DescriptorTable = registry.Descriptors

    /// The machine's open file descriptions, which `OpenFileTable`'s queries
    /// read: every one the process's descriptors name, and any others the
    /// machine holds.
    let openFiles (registry : FileDescriptorRegistry) : OpenFileTable = registry.OpenFiles

    /// Rewrite the machine's open file descriptions, leaving the descriptor
    /// table as it is.
    let internal mapOpenFiles
        (f : OpenFileTable -> OpenFileTable)
        (registry : FileDescriptorRegistry)
        : FileDescriptorRegistry
        =
        { registry with
            OpenFiles = f registry.OpenFiles
        }

    let private withFds (fds : Map<int, DescriptorEntry>) (registry : FileDescriptorRegistry) : FileDescriptorRegistry =
        { registry with
            Descriptors =
                {
                    Fds = fds
                }
        }

    /// Which description `fd` names, if `fd` is live. Callers that need to know
    /// whether two descriptors share a description — rather than merely name
    /// equal ones — must compare these rather than the payloads.
    let tryFindId (fd : int) (registry : FileDescriptorRegistry) : OpenFileDescriptionId option =
        named fd registry.Descriptors.Fds

    /// The description `fd` names *and* its identity, if `fd` is live.
    ///
    /// For callers that need both, which is otherwise two lookups whose results
    /// could not be shown to agree: `UnixPoll.epollWait` keys the waiter it
    /// parks on the identity, while which answer it gives at all depends on the
    /// target.
    let tryFindWithId
        (fd : int)
        (registry : FileDescriptorRegistry)
        : (OpenFileDescriptionId * OpenFileDescription) option
        =
        named fd registry.Descriptors.Fds
        |> Option.map (fun id ->
            match OpenFileTable.tryFind id registry.OpenFiles with
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
        tryFindWithId fd registry
        |> Option.map (fun (id, description) -> OpenFileDescription.object id description)

    /// What `fd` refers to and where in it, if `fd` is live. For the callers
    /// that move or consume the file offset.
    let tryFindTarget (fd : int) (registry : FileDescriptorRegistry) : OpenFileTarget option =
        tryFind fd registry |> Option.map (fun description -> description.Target)

    /// Every live file descriptor, and the description each names.
    let fds (registry : FileDescriptorRegistry) : Map<int, OpenFileDescriptionId> =
        registry.Descriptors.Fds |> Map.map (fun _ entry -> entry.Description)

    /// The flags of the descriptor `fd`, if `fd` is live: what
    /// `fcntl(F_GETFD)` reports.
    let tryFindFlags (fd : int) (registry : FileDescriptorRegistry) : DescriptorFlags option =
        Map.tryFind fd registry.Descriptors.Fds |> Option.map (fun entry -> entry.Flags)

    /// Lowest integer at or above `minimum`, which must be non-negative, not
    /// currently used as a file descriptor; `None` if every one up to
    /// `Int32.MaxValue` is. O(n) in the number of live fds; process fd tables
    /// are small.
    let private lowestFreeAtOrAbove (minimum : int) (fds : Map<int, DescriptorEntry>) : int option =
        if minimum < 0 then
            failwith
                $"FileDescriptorRegistry.lowestFreeAtOrAbove: minimum %d{minimum} is negative, and no descriptor is (this is a bug in this library)."

        let rec scan (candidate : int) =
            if not (Map.containsKey candidate fds) then Some candidate
            elif candidate = System.Int32.MaxValue then None
            else scan (candidate + 1)

        scan minimum

    /// Whether `count` descriptors, the lowest free at or above `minimum`, all
    /// lie below `bound`: the room a call that makes them needs. The refusal
    /// names the first that does not. Every syscall that makes a descriptor
    /// asks this, with `SimulatedUnixPlatform.descriptorBound`, before it
    /// allocates.
    let internal room
        (bound : int)
        (minimum : int)
        (count : int)
        (registry : FileDescriptorRegistry)
        : Result<unit, DescriptorLimitRefusal>
        =
        let rec find (candidate : int) (left : int) : Result<unit, DescriptorLimitRefusal> =
            if candidate >= bound then
                Error
                    {
                        Descriptor = candidate
                        Bound = bound
                    }
            elif Map.containsKey candidate registry.Descriptors.Fds then
                find (candidate + 1) left
            elif left = 1 then
                Ok ()
            else
                find (candidate + 1) (left - 1)

        if count < 1 || minimum < 0 then
            failwith
                $"FileDescriptorRegistry.room: asked for %d{count} descriptors from %d{minimum} (this is a bug in this library)."

        find minimum count

    /// Lowest non-negative integer not currently used as a file descriptor.
    let private lowestFree (fds : Map<int, DescriptorEntry>) : int =
        match lowestFreeAtOrAbove 0 fds with
        | Some fd -> fd
        | None ->
            failwith
                "FileDescriptorRegistry.lowestFree: every non-negative int is a live descriptor, which no table this library builds can reach."

    /// Make the free descriptor `newFd` name the live description `id`, with
    /// `flags`, counting it as one more descriptor naming `id`.
    let private install
        (newFd : int)
        (id : OpenFileDescriptionId)
        (flags : DescriptorFlags)
        (registry : FileDescriptorRegistry)
        : FileDescriptorRegistry
        =
        { registry with
            Descriptors =
                {
                    Fds =
                        Map.add
                            newFd
                            {
                                Description = id
                                Flags = flags
                            }
                            registry.Descriptors.Fds
                }
            OpenFiles = OpenFileTable.retain id registry.OpenFiles
        }

    /// A fresh open file description, `description`, and the lowest
    /// non-negative descriptor not in use to point at it, with neither
    /// descriptor flag.
    let private createDescription
        (description : OpenFileDescription)
        (registry : FileDescriptorRegistry)
        : int * FileDescriptorRegistry
        =
        let id, openFiles = OpenFileTable.create description registry.OpenFiles
        let fd = lowestFree registry.Descriptors.Fds

        fd,
        { registry with
            Descriptors =
                {
                    Fds = Map.add fd (unflagged id) registry.Descriptors.Fds
                }
            OpenFiles = openFiles
        }

    /// A descriptor table holding exactly the descriptors in `ends`, each naming
    /// an open file description of its own, fresh in `openFiles`, onto the given
    /// end of the given pipe: the read end opened `O_RDONLY` and the write end
    /// `O_WRONLY`, as `pipe(2)` opens them, and neither `O_NONBLOCK`.
    ///
    /// This is the table a process inherits from a launcher that gave it each
    /// of those descriptors onto a pipe of its own; `UnixSystem.initial` is the
    /// caller, and mints the pipes. It is the only way to build a table with
    /// descriptors at chosen numbers, which no syscall can do.
    let internal ofLaunchedPipes
        (ends : Map<int, PipeId * PipeEnd>)
        (openFiles : OpenFileTable)
        : FileDescriptorRegistry
        =
        let empty =
            {
                Descriptors =
                    {
                        Fds = Map.empty
                    }
                OpenFiles = openFiles
            }

        ends
        |> Map.fold
            (fun (registry : FileDescriptorRegistry) (fd : int) (pipeId : PipeId, pipeEnd : PipeEnd) ->
                if fd < 0 then
                    failwith
                        $"FileDescriptorRegistry.ofLaunchedPipes: descriptor %d{fd} is negative, which no descriptor is (this is a bug in the caller, which should have refused it)."

                let accessMode =
                    match pipeEnd with
                    | PipeEnd.Read -> FileAccessMode.ReadOnly
                    | PipeEnd.Write -> FileAccessMode.WriteOnly

                let id, openFiles =
                    OpenFileTable.create
                        {
                            Target = OpenFileTarget.Pipe (pipeId, pipeEnd)
                            AccessMode = accessMode
                            NonBlocking = false
                            Flock = None
                            Status = OpenFileStatus.none
                        }
                        registry.OpenFiles

                { registry with
                    Descriptors =
                        {
                            Fds = Map.add fd (unflagged id) registry.Descriptors.Fds
                        }
                    OpenFiles = openFiles
                }
            )
            empty

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
        match named oldFd registry.Descriptors.Fds with
        | None -> Error FileDescriptorDupError.BadFd
        | Some id ->
            let newFd = lowestFree registry.Descriptors.Fds
            Ok (newFd, install newFd id DescriptorFlags.none registry)

    /// The `fcntl(F_DUPFD)` half of the table: a new descriptor naming the
    /// description `oldFd` names, the lowest not in use at or above `minimum`,
    /// with `flags`. `None` when every descriptor from `minimum` to
    /// `Int32.MaxValue` is in use.
    ///
    /// Partial: `oldFd` must be live and `minimum` non-negative, which the
    /// caller has already answered EBADF and EINVAL for.
    let internal dupAtOrAbove
        (oldFd : int)
        (minimum : int)
        (flags : DescriptorFlags)
        (registry : FileDescriptorRegistry)
        : (int * FileDescriptorRegistry) option
        =
        match named oldFd registry.Descriptors.Fds with
        | None ->
            failwith
                $"FileDescriptorRegistry.dupAtOrAbove: fd %d{oldFd} is not live (this is a bug in the caller, which should have answered EBADF)."
        | Some id ->
            lowestFreeAtOrAbove minimum registry.Descriptors.Fds
            |> Option.map (fun newFd -> newFd, install newFd id flags registry)

    /// The installing half of `dup2(2)`: make `newFd`, which must be free and
    /// non-negative, name the description `oldFd` names, with `flags`.
    ///
    /// Partial: the caller has answered EBADF for a dead `oldFd` or a negative
    /// `newFd`, and closed `newFd` if it was open.
    let internal installAt
        (oldFd : int)
        (newFd : int)
        (flags : DescriptorFlags)
        (registry : FileDescriptorRegistry)
        : FileDescriptorRegistry
        =
        if newFd < 0 then
            failwith
                $"FileDescriptorRegistry.installAt: target %d{newFd} is negative (this is a bug in the caller, which should have answered EBADF)."

        if Map.containsKey newFd registry.Descriptors.Fds then
            failwith
                $"FileDescriptorRegistry.installAt: target %d{newFd} is live (this is a bug in the caller, which should have closed it first)."

        match named oldFd registry.Descriptors.Fds with
        | None ->
            failwith
                $"FileDescriptorRegistry.installAt: fd %d{oldFd} is not live (this is a bug in the caller, which should have answered EBADF)."
        | Some id -> install newFd id flags registry

    /// Replace the flags of the descriptor `fd`: the table half of
    /// `fcntl(F_SETFD)`. Partial: the caller has answered EBADF for a dead `fd`.
    let internal setFlags
        (fd : int)
        (flags : DescriptorFlags)
        (registry : FileDescriptorRegistry)
        : FileDescriptorRegistry
        =
        match Map.tryFind fd registry.Descriptors.Fds with
        | None ->
            failwith
                $"FileDescriptorRegistry.setFlags: fd %d{fd} is not live (this is a bug in the caller, which should have answered EBADF)."
        | Some entry ->
            withFds
                (Map.add
                    fd
                    { entry with
                        Flags = flags
                    }
                    registry.Descriptors.Fds)
                registry

    /// Remove a descriptor from the table of the process `owner`, destroying the
    /// description it named if nothing references that description any more:
    /// no other descriptor, in this process or any other, names it, and no
    /// syscall in flight holds it (`OpenFileTable.holdCount`). Mirrors
    /// `close(2)`: returns `Error BadFd` (= `EBADF`) when `fd` is not currently
    /// live.
    ///
    /// Closing one descriptor of a `dup` pair leaves the other's description
    /// intact — true of everything this library models, though not of POSIX in
    /// general (see the record-lock note on `FileDescriptorRegistry`).
    ///
    /// The descriptor-table half of `close(2)`, and only that half: it drops
    /// the descriptor, every registration made through it in a kqueue `owner`
    /// holds (`KqueueState.Owner`), and, if it
    /// was the last reference, the description, and it releases nothing that
    /// description referenced. `UnixDescriptor.close` is the syscall, and the
    /// one caller; a client that wants `close(2)` wants
    /// that. The in-house property tests drive close+dup cycles directly
    /// against this function to exercise the `lowestFree` invariant against
    /// the gap structure that closing produces.
    ///
    /// Reports the description it destroyed, if it destroyed one: closing a
    /// `dup(2)` of a live descriptor destroys nothing and answers `None`, and
    /// so does closing the last descriptor onto a description a call in flight
    /// still holds. The caller needs this because a description can
    /// be the last reference to a *kernel object* whose lifetime is decided
    /// elsewhere — `UnixMachineState.Sockets` is the one that exists today —
    /// and this registry cannot reach that state to clean it up itself.
    let internal dropDescriptor
        (owner : ProcessId)
        (fd : int)
        (registry : FileDescriptorRegistry)
        : Result<FileDescriptorRegistry * OpenFileDescription option, FileDescriptorCloseError>
        =
        match named fd registry.Descriptors.Fds with
        | None -> Error FileDescriptorCloseError.BadFd
        | Some id ->
            // Every kqueue registration made through this descriptor goes with
            // it, queued or not, whether or not something else keeps the
            // description alive: Darwin keys a registration by the descriptor
            // number (measured, `kevent-register.c` section G). The number is
            // one in the kqueue's owner's table, so another process's kqueue
            // registers nothing through this descriptor, whatever numbers it
            // holds.
            let openFiles =
                (registry.OpenFiles, OpenFileTable.toSeq registry.OpenFiles)
                ||> Seq.fold (fun openFiles (kqueue, description) ->
                    match description.Target with
                    | OpenFileTarget.Kqueue state when
                        state.Owner = owner
                        && state.Registrations |> Map.exists (fun (registeredFd, _) _ -> registeredFd = fd)
                        ->
                        OpenFileTable.setKqueueState
                            kqueue
                            { state with
                                Registrations =
                                    state.Registrations
                                    |> Map.filter (fun (registeredFd, _) _ -> registeredFd <> fd)
                                Active = state.Active |> List.filter (fun (activeFd, _) -> activeFd <> fd)
                            }
                            openFiles
                    | OpenFileTarget.Kqueue _
                    | OpenFileTarget.Epoll _
                    | OpenFileTarget.File _
                    | OpenFileTarget.Directory _
                    | OpenFileTarget.Socket _
                    | OpenFileTarget.CharacterDevice _
                    | OpenFileTarget.Pipe _ -> openFiles
                )

            // Present by `DanglingFd`: a live descriptor names a live
            // description.
            if (OpenFileTable.tryFind id openFiles).IsNone then
                failwith
                    $"FileDescriptorRegistry.dropDescriptor: file descriptor %d{fd} names open file description %O{id}, which is not present in the table (this is a bug in this library: every descriptor names a description in the table)"

            let openFiles, destroyed =
                OpenFileTable.release id openFiles |> OpenFileTable.destroyIfUnreferenced id

            Ok (
                {
                    Descriptors =
                        {
                            Fds = Map.remove fd registry.Descriptors.Fds
                        }
                    OpenFiles = openFiles
                },
                destroyed
            )

    /// Mirrors the descriptor half of `open(2)`: allocate a *fresh* open file
    /// description naming `inode`, and the lowest non-negative descriptor not
    /// in use to point at it.
    ///
    /// Fresh, unlike `dup`: two `open` calls on one path give two descriptions,
    /// which is why they can hold separate offsets and separate `flock` locks.
    ///
    /// The offset starts at 0 for *every* flag, not merely the ones
    /// `UnixNamespace.openPath` models. `O_APPEND` is no exception: measured on both platforms, a
    /// descriptor opened `O_WRONLY | O_APPEND` on a five-byte file reports 0
    /// from `lseek(0, SEEK_CUR)` immediately afterwards, and only reaches 6
    /// after a one-byte write. The flag repositions to the end before each
    /// individual *write*, not at open time, so when the write path lands it
    /// belongs there.
    ///
    /// Total — there is no failure mode at this level. Whether the path
    /// resolves, whether the flags are ones this library honours, and whether the
    /// process may open the file at all are decided before this is reached, and
    /// so is whether a descriptor below the bound is free
    /// (`SimulatedUnixPlatform.descriptorBound`), which `UnixSystem.checkInvariants`
    /// holds every descriptor to.
    let openFile
        (inode : InodeNumber)
        (accessMode : FileAccessMode)
        (registry : FileDescriptorRegistry)
        : int * FileDescriptorRegistry
        =
        createDescription
            {
                Target = OpenFileTarget.File (inode, 0L)
                AccessMode = accessMode
                // `UnixNamespace.openPath` refuses `O_NONBLOCK`, so
                // every modelled open starts blocking.
                NonBlocking = false
                // `open(2)` never takes a lock.
                Flock = None
                Status = OpenFileStatus.none
            }
            registry

    /// Mirrors the descriptor half of `open(2)` on a character device's node:
    /// allocate a fresh open file description of `device`, the device the node
    /// at `inode` stands for, with `accessMode`, and the lowest non-negative
    /// descriptor not in use to point at it.
    ///
    /// Total, for the reasons `openFile` is; whether the process may open the
    /// node is decided before this is reached.
    let internal openCharacterDevice
        (inode : InodeNumber)
        (device : CharacterDevice)
        (accessMode : FileAccessMode)
        (registry : FileDescriptorRegistry)
        : int * FileDescriptorRegistry
        =
        createDescription
            {
                Target = OpenFileTarget.CharacterDevice (inode, device)
                AccessMode = accessMode
                NonBlocking = false
                Flock = None
                Status = OpenFileStatus.none
            }
            registry

    /// Mirrors the descriptor half of `open(2)` on a directory: allocate a
    /// fresh open file description positioned at the start of `inode`'s
    /// entries, open for reading, and the lowest non-negative descriptor not in
    /// use to point at it.
    ///
    /// Total, for the reasons `openFile` is; whether `inode` is a directory the
    /// process may read is decided before this is reached.
    let internal openDirectory
        (inode : InodeNumber)
        (registry : FileDescriptorRegistry)
        : int * FileDescriptorRegistry
        =
        createDescription
            {
                Target = OpenFileTarget.Directory (inode, DirectoryPosition.Cursor DirectoryCursor.Start)
                AccessMode = FileAccessMode.ReadOnly
                NonBlocking = false
                Flock = None
                Status = OpenFileStatus.none
            }
            registry

    /// A fresh, blocking, read-write open file description naming `target`,
    /// and the lowest non-negative descriptor not in use to point at it.
    let private createAnonymous
        (target : OpenFileTarget)
        (registry : FileDescriptorRegistry)
        : int * FileDescriptorRegistry
        =
        createDescription
            {
                Target = target
                AccessMode = FileAccessMode.ReadWrite
                NonBlocking = false
                Flock = None
                Status = OpenFileStatus.none
            }
            registry

    /// Mirrors `epoll_create1(2)`: allocate a fresh open file description
    /// naming a new epoll instance with nothing registered, and the lowest
    /// non-negative descriptor not in use to point at it.
    ///
    /// Fresh, like `openFile` and unlike `dup`: two `epoll_create1` calls give
    /// two instances, which is what makes them separately identifiable.
    ///
    /// The access mode is `ReadWrite`, and that is load-bearing rather than
    /// cosmetic: `UnixReadWrite.read` checks `FileAccessMode.permitsRead` before
    /// it looks at the target kind and answers `EBADF` if it fails, whereas a
    /// real instance answers `EINVAL` — measured. Linux opens the underlying
    /// anonymous file `O_RDWR`.
    ///
    /// Total, like `openFile` and for the same reason.
    let createEpoll (registry : FileDescriptorRegistry) : int * FileDescriptorRegistry =
        createAnonymous
            (OpenFileTarget.Epoll
                {
                    Registrations = Map.empty
                    Ready = []
                })
            registry

    /// Mirrors `kqueue(2)`: allocate a fresh open file description naming a
    /// new kqueue, and the lowest non-negative descriptor not in use to point
    /// at it.
    ///
    /// The access mode is `ReadWrite`, for the reason `createEpoll`'s is: a
    /// real kqueue answers `ENXIO` to `read(2)` and `write(2)`, never `EBADF`
    /// (measured). Measured, Darwin's kqueue is `O_RDWR`, not `O_NONBLOCK`. Its
    /// descriptor flags are `UnixKqueue.kqueue`'s to set.
    ///
    /// Total, like `createEpoll`.
    let createKqueue (owner : ProcessId) (registry : FileDescriptorRegistry) : int * FileDescriptorRegistry =
        createAnonymous
            (OpenFileTarget.Kqueue
                {
                    Owner = owner
                    Drained = false
                    Registrations = Map.empty
                    Active = []
                })
            registry

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
    /// cosmetic, for the reason `createEpoll`'s is:
    /// `UnixReadWrite.read` and `UnixReadWrite.write` test the access mode before
    /// they look at the target, so anything narrower would answer EBADF where a
    /// real socket gives its own answer instead (measured on one with no peer:
    /// ENOTCONN, EINVAL, EPIPE, EDESTADDRREQ, EAGAIN or a block, never EBADF).
    ///
    /// Total, like `openFile` and `createEpoll`: there is no resource a socket
    /// could exhaust, and the bound is the caller's to check.
    let createSocket (socketId : SocketId) (registry : FileDescriptorRegistry) : int * FileDescriptorRegistry =
        createDescription
            {
                Target = OpenFileTarget.Socket socketId
                AccessMode = FileAccessMode.ReadWrite
                // A `socket(2)` asked for `SOCK_NONBLOCK` sets it
                // afterwards, through `setNonBlocking`, and one asked
                // for `SOCK_CLOEXEC` sets its flag through `setFlags`.
                NonBlocking = false
                // `socket(2)` takes no lock, exactly as `open(2)` does not.
                Flock = None
                Status = OpenFileStatus.none
            }
            registry

    /// Allocate the two open file descriptions of a new pipe, the read end
    /// `O_RDONLY` and the write end `O_WRONLY`, and a descriptor onto each: the
    /// lowest one not in use for the read end, then the lowest not in use after
    /// that for the write end. Both descriptions carry `O_NONBLOCK` exactly when
    /// `nonBlocking` is set.
    ///
    /// Measured on both flavours: with 0, 1 and 2 open, `pipe(2)` returns 3 and
    /// 4; with 3 then closed, a second `pipe(2)` returns 3 and 5.
    ///
    /// `pipeId` is minted by the caller, because the pipe it names lives in the
    /// pipe table rather than here; `UnixPipe.pipe2` is the one caller.
    ///
    /// Total, like `openFile`; the caller checks that both descriptors lie
    /// below the bound.
    let internal createPipe
        (pipeId : PipeId)
        (nonBlocking : bool)
        (registry : FileDescriptorRegistry)
        : (int * int) * FileDescriptorRegistry
        =
        let add (pipeEnd : PipeEnd) (accessMode : FileAccessMode) (registry : FileDescriptorRegistry) =
            createDescription
                {
                    Target = OpenFileTarget.Pipe (pipeId, pipeEnd)
                    AccessMode = accessMode
                    NonBlocking = nonBlocking
                    // `pipe(2)` takes no lock.
                    Flock = None
                    Status = OpenFileStatus.none
                }
                registry

        let readFd, registry = add PipeEnd.Read FileAccessMode.ReadOnly registry
        let writeFd, registry = add PipeEnd.Write FileAccessMode.WriteOnly registry
        (readFd, writeFd), registry

    /// Mirrors `flock(2)`.
    ///
    /// The lock belongs to the open file description `fd` names, so two
    /// descriptors from one `dup(2)` share a single lock (releasing through
    /// either releases it), while two separate `open(2)` calls on one path hold
    /// two and therefore contend. That contention is the mechanism behind
    /// `FileShare` on Unix, and it works *within* one process, so a
    /// single-threaded process can observe it.
    ///
    /// Contention is between descriptions naming the same `OpenFileObject`. For
    /// a pipe launched by `ofLaunchedPipes` that set is the one description,
    /// whose pipe nothing else names, and `dup` shares rather than copies, so
    /// `flock` on one succeeds and conflicts with nothing. That is what Linux
    /// does (measured: `flock` on a pipe returns 0).
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
    let internal flock
        (fd : int)
        (request : FlockRequest)
        (registry : FileDescriptorRegistry)
        : FileDescriptorRegistry * FlockError option
        =
        match named fd registry.Descriptors.Fds with
        | None -> registry, Some FlockError.BadFd
        | Some id ->
            let openFiles, error = OpenFileTable.flockOn id request registry.OpenFiles

            { registry with
                OpenFiles = openFiles
            },
            error

    /// The description `fd` names and its identity, for an operation named
    /// `operation` whose caller has already answered EBADF for a dead `fd`.
    let private liveDescription
        (operation : string)
        (fd : int)
        (registry : FileDescriptorRegistry)
        : OpenFileDescriptionId * OpenFileDescription
        =
        match tryFindWithId fd registry with
        | Some found -> found
        | None ->
            failwith
                $"%s{operation}: fd %d{fd} is not a live file descriptor (this is a bug in the caller of FileDescriptorRegistry.%s{operation}, which should have answered EBADF)."

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
    let internal setOffset (fd : int) (offset : int64) (registry : FileDescriptorRegistry) : FileDescriptorRegistry =
        if offset < 0L then
            failwith
                $"setOffset: fd %d{fd} was asked to move to offset %d{offset}, which is negative. No kernel permits a negative file offset; the caller must reject this as EINVAL before storing it (this is a bug in the caller of FileDescriptorRegistry.setOffset)."

        let id, description = liveDescription "setOffset" fd registry

        match description.Target with
        | OpenFileTarget.Epoll _ ->
            failwith
                $"setOffset: fd %d{fd} names an epoll instance, which holds no file offset — Linux's lseek on one is noop_llseek (this is a bug in the caller of FileDescriptorRegistry.setOffset, which should have answered without moving a position)."
        | OpenFileTarget.Kqueue _ ->
            failwith
                $"setOffset: fd %d{fd} names a kqueue, which holds no file offset — Darwin's lseek on one is ESPIPE (this is a bug in the caller of FileDescriptorRegistry.setOffset, which should have answered ESPIPE)."
        | OpenFileTarget.Socket socketId ->
            failwith
                $"setOffset: fd %d{fd} names socket %O{socketId}, which holds no file offset on either platform — `lseek` on a socket is ESPIPE on both (this is a bug in the caller of FileDescriptorRegistry.setOffset, which should have answered ESPIPE)."
        | OpenFileTarget.Pipe (pipeId, pipeEnd) ->
            failwith
                $"setOffset: fd %d{fd} names the %O{pipeEnd} end of pipe %O{pipeId}, which holds no file offset — `lseek` on a pipe is ESPIPE on both flavours (this is a bug in the caller of FileDescriptorRegistry.setOffset, which should have answered ESPIPE)."
        | OpenFileTarget.Directory (inode, _) ->
            failwith
                $"setOffset: fd %d{fd} names directory %O{inode}, whose position is not a byte offset (this is a bug in the caller of FileDescriptorRegistry.setOffset, which should have called setDirectoryPosition)."
        | OpenFileTarget.CharacterDevice (inode, device) ->
            failwith
                $"setOffset: fd %d{fd} names %O{device} at inode %O{inode}, which keeps no offset: its `lseek` answers 0 and moves nothing (this is a bug in the caller of FileDescriptorRegistry.setOffset)."
        | OpenFileTarget.File (inode, _) ->

        registry
        |> mapOpenFiles (
            OpenFileTable.mapDescription
                "setOffset"
                id
                (fun description ->
                    { description with
                        Target = OpenFileTarget.File (inode, offset)
                    }
                )
        )

    /// Move the position of the directory description `fd` names, which every
    /// descriptor `dup(2)` made for it shares.
    ///
    /// Partial in the descriptor, like `setOffset`: the caller has already
    /// answered EBADF for a dead fd, and has established that `fd` names a
    /// directory.
    let internal setDirectoryPosition
        (fd : int)
        (position : DirectoryPosition)
        (registry : FileDescriptorRegistry)
        : FileDescriptorRegistry
        =
        let id, description = liveDescription "setDirectoryPosition" fd registry

        match description.Target with
        | OpenFileTarget.Directory (inode, _) ->
            registry
            |> mapOpenFiles (
                OpenFileTable.mapDescription
                    "setDirectoryPosition"
                    id
                    (fun description ->
                        { description with
                            Target = OpenFileTarget.Directory (inode, position)
                        }
                    )
            )
        | OpenFileTarget.File _
        | OpenFileTarget.Kqueue _
        | OpenFileTarget.Epoll _
        | OpenFileTarget.Socket _
        | OpenFileTarget.CharacterDevice _
        | OpenFileTarget.Pipe _ ->
            failwith
                $"setDirectoryPosition: fd %d{fd} names %O{description.Target}, which is not a directory (this is a bug in the caller of FileDescriptorRegistry.setDirectoryPosition)."

    /// Mirrors the `O_NONBLOCK` half of `fcntl(F_SETFL)`: record whether this
    /// description's transfers should refuse to block. On the description, so
    /// shared with every descriptor `dup(2)` has produced for it.
    ///
    /// Like `setOffset`, *partial* in the descriptor: the caller
    /// (`UnixDescriptor.fcntl`) has already answered `EBADF` for
    /// a dead fd. Every target stores the flag, an epoll instance and a kqueue
    /// included:
    /// measured on both flavours, `F_SETFL` genuinely toggles the bit there
    /// (even on Darwin, where the call also reports ENOTTY — the caller's
    /// business, not this store's), and no modelled wait consults it, because
    /// `epoll_wait` and `kevent` block per their own timeout argument rather
    /// than per the descriptor's flags.
    let internal setNonBlocking (fd : int) (value : bool) (registry : FileDescriptorRegistry) : FileDescriptorRegistry =
        let id, _ = liveDescription "setNonBlocking" fd registry

        registry
        |> mapOpenFiles (
            OpenFileTable.mapDescription
                "setNonBlocking"
                id
                (fun description ->
                    { description with
                        NonBlocking = value
                    }
                )
        )

    // The rules relating `registry`'s descriptor numbers to the descriptions:
    // no descriptor names a missing description (the first list), and every
    // registration of a kqueue `ownsKqueue` admits is made through an open
    // descriptor onto a socket (the second).
    let private tableDefects
        (ownsKqueue : KqueueState -> bool)
        (registry : FileDescriptorRegistry)
        : FileDescriptorRegistryDefect list * FileDescriptorRegistryDefect list
        =
        let descriptions = OpenFileTable.descriptions registry.OpenFiles

        let dangling =
            fds registry
            |> Map.toList
            |> List.filter (fun (_, id) -> not (Map.containsKey id descriptions))
            |> List.map FileDescriptorRegistryDefect.DanglingFd

        let kqueueRegistrations =
            descriptions
            |> Map.toList
            |> List.collect (fun (kqueue, description) ->
                match description.Target with
                | OpenFileTarget.Epoll _
                | OpenFileTarget.File _
                | OpenFileTarget.Directory _
                | OpenFileTarget.Socket _
                | OpenFileTarget.CharacterDevice _
                | OpenFileTarget.Pipe _ -> []
                | OpenFileTarget.Kqueue state when not (ownsKqueue state) -> []
                | OpenFileTarget.Kqueue state ->
                    state.Registrations
                    |> Map.toList
                    |> List.choose (fun ((fd, filter), _) ->
                        match named fd registry.Descriptors.Fds with
                        | None ->
                            Some (
                                FileDescriptorRegistryDefect.KqueueRegistrationThroughClosedDescriptor (
                                    kqueue,
                                    fd,
                                    filter
                                )
                            )
                        | Some id ->
                            match Map.tryFind id descriptions with
                            | Some {
                                       Target = OpenFileTarget.Socket _
                                   }
                            // A dangling descriptor is `DanglingFd`'s to report.
                            | None -> None
                            | Some other ->
                                Some (
                                    FileDescriptorRegistryDefect.KqueueRegistrationNotOnSocket (
                                        kqueue,
                                        fd,
                                        filter,
                                        other.Target
                                    )
                                )
                    )
            )

        dangling, kqueueRegistrations

    /// Every way in which `registry` fails to be a descriptor table a kernel
    /// could produce, with the open file descriptions it names. Empty for any
    /// registry built out of `ofLaunchedPipes`, `dup` and `close`; the property
    /// tests assert exactly that.
    ///
    /// Includes `OpenFileTable.checkInvariants` of the machine's descriptions
    /// against this process's descriptor table alone, which holds every
    /// descriptor on a machine running this one process.
    ///
    /// Whether a description is still referenced, and whether its holds are
    /// those the parks name, are not among them: the parks are the tasks',
    /// which a registry does not hold, so those are `UnixSystem.checkInvariants`'s
    /// `UnreferencedDescription` and `HoldCountMismatch`.
    let checkInvariants (registry : FileDescriptorRegistry) : FileDescriptorRegistryDefect list =
        let dangling, kqueueRegistrations = tableDefects (fun _ -> true) registry

        dangling
        @ OpenFileTable.checkInvariants [ registry.Descriptors ] registry.OpenFiles
        @ kqueueRegistrations

    /// Every way in which `registry`'s descriptor table, the table of the
    /// process `owner`, fails to be one a kernel could produce over the
    /// machine's open file descriptions: a descriptor naming no description,
    /// and a registration of a kqueue `owner` owns made through a descriptor of
    /// its table that is closed or names no socket.
    ///
    /// For a machine holding several processes, where `checkInvariants` would
    /// count only this table's descriptors against each description: the
    /// counts, and the rest of `OpenFileTable.checkInvariants`, are the
    /// machine's, read against every process's table.
    let checkDescriptorTableInvariants
        (owner : ProcessId)
        (registry : FileDescriptorRegistry)
        : FileDescriptorRegistryDefect list
        =
        let dangling, kqueueRegistrations =
            tableDefects (fun state -> state.Owner = owner) registry

        dangling @ kqueueRegistrations

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
        /// A registry whose descriptor table is `fds` and whose open file
        /// descriptions are `descriptions`, each counting the descriptors in
        /// `fds` that name it and no holds, with `nextId` the identity the next
        /// one gets.
        let ofParts
            (fds : Map<int, OpenFileDescriptionId>)
            (descriptions : Map<OpenFileDescriptionId, OpenFileDescription>)
            (nextId : OpenFileDescriptionId)
            : FileDescriptorRegistry
            =
            let naming = fds |> Map.toList |> List.countBy snd |> Map.ofList

            {
                Descriptors =
                    {
                        Fds = fds |> Map.map (fun _ id -> unflagged id)
                    }
                OpenFiles =
                    {
                        Entries =
                            descriptions
                            |> Map.map (fun id description ->
                                {
                                    Description = description
                                    Descriptors = Map.tryFind id naming |> Option.defaultValue 0
                                    Holds = 0
                                }
                            )
                        NextId = nextId
                    }
            }

        /// Rewrite one description in place, however unsoundly. Partial: the
        /// id must be live.
        let mapDescription
            (id : OpenFileDescriptionId)
            (f : OpenFileDescription -> OpenFileDescription)
            (registry : FileDescriptorRegistry)
            : FileDescriptorRegistry
            =
            registry
            |> mapOpenFiles (OpenFileTable.mapDescription "Unchecked.mapDescription" id f)

        /// Record `count` as the number of descriptors naming the description
        /// `id`, however many do. Partial: the id must be live.
        let setDescriptorCount
            (id : OpenFileDescriptionId)
            (count : int)
            (registry : FileDescriptorRegistry)
            : FileDescriptorRegistry
            =
            { registry with
                OpenFiles =
                    { registry.OpenFiles with
                        Entries =
                            Map.add
                                id
                                { Map.find id registry.OpenFiles.Entries with
                                    Descriptors = count
                                }
                                registry.OpenFiles.Entries
                    }
            }

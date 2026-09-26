namespace WoofWare.PosixKernel

/// The readiness a descriptor presents to a Linux-flavoured waiter, in Linux's
/// own numbering: what `poll(2)` reports to a request of every bit, and what
/// `epoll_wait(2)` reports to a registration of every condition.
///
/// One mask serves both waiters, because both take it from the file's own
/// `->poll` handler. Measured through each: `poll-alphabet.c` polled every
/// state below with all 65536 request masks, and `epoll-ctl.c` registered each
/// with 21024 event masks, both on Linux 6.18.5
/// (docs/plans/2026-08-23-posix-kernel-extraction). Every answer of either was
/// this mask restricted to what was asked, plus `ERR` and `HUP`, and the two
/// waiters presented the same mask for every state. The readiness bits of
/// `<sys/epoll.h>` and `<poll.h>` share their numbering, which is why one
/// `uint32` states both.
///
/// Answers for the socket phases `UnixMachineState.socketReadinessLevel`
/// answers, the launch shape's standard streams, and regular files and
/// directories (which epoll will not register, but `poll` answers). A socket
/// event port is refused: what either waiter reports for one is not modelled.
[<RequireQualifiedAccess>]
module LinuxReadiness =

    /// The mask the descriptor `targetId` names presents right now.
    let ofDescription<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (targetId : OpenFileDescriptionId)
        (system : UnixSystem<'Task, 'Handler>)
        : uint32
        =
        match Map.tryFind targetId (FileDescriptorRegistry.descriptions system.Process.FileDescriptors) with
        | None ->
            failwith
                $"LinuxReadiness.ofDescription: %O{targetId} names no live open file description. Both waiters resolve their descriptor first, and FileDescriptorRegistry.dropDescriptor sweeps destroyed descriptions out of every interest table, so this is a bug in this library."
        | Some description ->

        match description.Target with
        | OpenFileTarget.Socket socketId ->
            // The five conditions are the socket's own. The handler also sets
            // the `*NORM` and `*BAND` bits, and measurement pins each to a
            // condition the level already holds: every measured socket
            // presents RDNORM exactly when it presents IN, and WRNORM exactly
            // when OUT.
            let level = UnixMachineState.socketReadinessLevel socketId system.Machine
            let socket = UnixMachineState.socket socketId system.Machine

            // WRBAND is the one bit that depends on more than the level.
            // Measured, it rides with OUT on UDP (IPv4 and IPv6, with and
            // without a peer) and on every Unix-domain socket (stream,
            // datagram, raw and seqpacket, fresh), and never on TCP (idle,
            // listening, established with the peer alive or gone, refused) --
            // `tcp_poll` sets OUT|WRNORM, where `datagram_poll` and the
            // Unix-domain handlers set OUT|WRNORM|WRBAND.
            let writeBand =
                match socket.Domain, socket.Kind with
                | SocketDomain.Unix, _ -> true
                | SocketDomain.InterNetwork, SocketKind.Datagram
                | SocketDomain.InterNetworkV6, SocketKind.Datagram -> true
                | SocketDomain.InterNetwork, SocketKind.Stream
                | SocketDomain.InterNetworkV6, SocketKind.Stream -> false
                | SocketDomain.InterNetwork, (SocketKind.Raw | SocketKind.SeqPacket)
                | SocketDomain.InterNetworkV6, (SocketKind.Raw | SocketKind.SeqPacket) ->
                    failwith
                        $"LinuxReadiness.ofDescription: socket %O{socketId} is %O{socket.Kind} in %O{socket.Domain}, which this kernel never creates (an IP raw socket needs CAP_NET_RAW, and nothing here creates SCTP), so what a waiter reports for it is unmeasured (this is a bug in the caller's state construction)."

            // No modelled socket presents PRI, RDBAND or MSG. Nor
            // POLL_BUSY_LOOP (0x8000), which a socket's handler adds only
            // while busy polling is enabled for it -- `SO_BUSY_POLL`, which
            // nothing here sets, or the `net.core.busy_read` sysctl, which this
            // library takes to be at its default of 0.
            (if level.In then
                 EpollEvents.In ||| EpollEvents.RdNorm
             else
                 0u)
            ||| (if level.Out then
                     EpollEvents.Out
                     ||| EpollEvents.WrNorm
                     ||| (if writeBand then EpollEvents.WrBand else 0u)
                 else
                     0u)
            ||| (if level.RdHup then EpollEvents.RdHup else 0u)
            ||| (if level.Hup then EpollEvents.Hup else 0u)
            ||| (if level.Err then EpollEvents.Err else 0u)
        | OpenFileTarget.File _ ->
            // Measured through `poll`: a regular file answers IN|OUT|RDNORM|
            // WRNORM at every offset, empty or not, and under every access
            // mode, and a directory answers the same. Files have no `->poll`
            // handler, so `vfs_poll` reports `DEFAULT_POLLMASK` for them. The
            // same missing handler is why `epoll_ctl` answers EPERM for one, so
            // only `poll` asks.
            EpollEvents.In ||| EpollEvents.Out ||| EpollEvents.RdNorm ||| EpollEvents.WrNorm
        | OpenFileTarget.StandardStream FileDescriptorRole.StandardInput ->
            // The launch shape this library models (measured, `pipes.c`):
            // stdin is the read end of a pipe whose write end the launcher
            // closed -- the same claim `UnixReadWrite.read`'s immediate
            // end-of-file makes -- which presents HUP alone.
            EpollEvents.Hup
        | OpenFileTarget.StandardStream FileDescriptorRole.StandardOutput
        | OpenFileTarget.StandardStream FileDescriptorRole.StandardError ->
            // Write ends of pipes with space and a live reader. No WRBAND:
            // a pipe's handler does not set it.
            EpollEvents.Out ||| EpollEvents.WrNorm
        | OpenFileTarget.SocketEventPort _ ->
            failwith
                $"LinuxReadiness.ofDescription: %O{targetId} is a socket event port, and what a waiter reports for one is not modelled. `poll` refuses such an entry and `epoll_ctl` refuses to nest one, both before reaching here (this is a bug in this library)."

/// What a socket event port -- an `epoll` instance, or a `kqueue` -- would
/// report if a wait on it were re-polled now, and what draining one does.
///
/// The *consumer* half of the port model. The producer half -- seeding the
/// pending list when a registration is added or modified, and signalling a
/// registration when its target's level changes -- belongs to the operations
/// that make those changes: `UnixPoll.epollCtl`, and the socket operations in
/// `UnixConnection`.
[<RequireQualifiedAccess>]
module SocketEventPort =

    /// Each pending entry of the port, in delivery order, with what it would
    /// report if `epoll_wait` re-polled it right now: the target's current
    /// readiness restricted to the registration's stored mask.
    let private annotatedReady<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (portState : SocketEventPortState)
        (system : UnixSystem<'Task, 'Handler>)
        : ((int * OpenFileDescriptionId) * EpollRegistration * uint32) list
        =
        portState.Ready
        |> List.map (fun (_, targetId as key) ->
            let registration =
                match Map.tryFind key portState.Registrations with
                | Some registration -> registration
                | None ->
                    failwith
                        $"SocketEventPort.annotatedReady: pending entry %A{key} has no registration. FileDescriptorRegistryDefect.SocketEventReadyEntryUnregistered exists to make this unreachable, so the system breaks UnixSystem.checkInvariants: this is a bug in this library, or in a caller that assembled the state by hand."

            let reported = LinuxReadiness.ofDescription targetId system &&& registration.Events

            key, registration, reported
        )

    /// Whether an `epoll_wait` on the port `portId` names would return at
    /// least one event right now — the wake condition a parked waiter is
    /// polled against, and by construction the same question `drain` answers,
    /// because both read the same annotated walk.
    ///
    /// Loudly partial in `portId`, exactly as a parked `flock`'s wake condition
    /// is: this library's descriptor table models no reference from a waiter to
    /// what it waits on, so a client that parks a task on a port must stop that
    /// port being destroyed while it waits — which is what `close`'s port
    /// refusal does. Asking about a port that has gone is that obligation being
    /// broken, and neither answer is honest: `true` wakes the waiter into an
    /// `EBADF` no kernel produces, and `false` sleeps for ever.
    let hasDeliverableEvent<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (portId : OpenFileDescriptionId)
        (system : UnixSystem<'Task, 'Handler>)
        : bool
        =
        match Map.tryFind portId (FileDescriptorRegistry.descriptions system.Process.FileDescriptors) with
        | None ->
            failwith
                $"SocketEventPort.hasDeliverableEvent: %O{portId} names no live open file description, so a task parked on a wait for it has had that description closed underneath it. This library's table models no reference from a waiter to what it waits on, so a client that parks must refuse such a close (as `close` does)."
        | Some description ->

        match description.Target with
        | OpenFileTarget.StandardStream _
        | OpenFileTarget.File _
        | OpenFileTarget.Socket _ ->
            failwith
                $"SocketEventPort.hasDeliverableEvent: %O{portId} is not a socket event port, so no wait can be parked on it (this is a bug in the caller of SocketEventPort.hasDeliverableEvent)."
        | OpenFileTarget.SocketEventPort portState ->
            annotatedReady portState system
            |> List.exists (fun (_, _, reported) -> reported <> 0u)

    /// Drain the port as one `epoll_wait(maxevents = maxCount)` would: walk
    /// the pending entries in order, re-polling each; report the ones whose
    /// re-poll is nonempty, silently drop the stale ones, and stop once
    /// `maxCount` events are reported — every walked entry is consumed, and
    /// the entries the stop spared stay pending in order (measured,
    /// `order2.c` row J).
    ///
    /// Returns the reported rows -- each the registration's `Data` and the
    /// `events` `epoll_wait` writes for it, in Linux's `<sys/epoll.h>`
    /// numbering (`EpollEvents`) -- and the system with the walked entries
    /// consumed.
    ///
    /// Loudly partial in `portId`: callers hold a live port description in
    /// hand.
    let drain<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (portId : OpenFileDescriptionId)
        (maxCount : int)
        (system : UnixSystem<'Task, 'Handler>)
        : (uint64 * uint32) list * UnixSystem<'Task, 'Handler>
        =
        if maxCount <= 0 then
            failwith
                $"SocketEventPort.drain: maxCount %d{maxCount} is not positive; epoll answers EINVAL for it before reaching the ready list, so this is a bug in the caller of SocketEventPort.drain."

        match Map.tryFind portId (FileDescriptorRegistry.descriptions system.Process.FileDescriptors) with
        | None ->
            failwith
                $"SocketEventPort.drain: %O{portId} names no live open file description (this is a bug in the caller of SocketEventPort.drain)."
        | Some description ->

        match description.Target with
        | OpenFileTarget.StandardStream _
        | OpenFileTarget.File _
        | OpenFileTarget.Socket _ ->
            failwith
                $"SocketEventPort.drain: %O{portId} is not a socket event port (this is a bug in the caller of SocketEventPort.drain)."
        | OpenFileTarget.SocketEventPort portState ->

        let rec walk
            (delivered : (uint64 * uint32) list)
            (remaining : ((int * OpenFileDescriptionId) * EpollRegistration * uint32) list)
            : (uint64 * uint32) list * (int * OpenFileDescriptionId) list
            =
            match remaining with
            | [] -> List.rev delivered, []
            | (_, registration, reported) :: rest ->
                if List.length delivered = maxCount then
                    List.rev delivered, remaining |> List.map (fun (key, _, _) -> key)
                elif reported = 0u then
                    walk delivered rest
                else
                    walk ((registration.Data, reported) :: delivered) rest

        let delivered, surviving = walk [] (annotatedReady portState system)

        delivered,
        { system with
            Process =
                { system.Process with
                    FileDescriptors =
                        FileDescriptorRegistry.setSocketEventReady portId surviving system.Process.FileDescriptors
                }
        }

/// One entry of a `poll(2)` call, as its caller supplied it: `struct pollfd`'s
/// `fd` and `events`, without the `revents` the kernel writes back.
type PollEntry =
    {
        /// The descriptor to poll. A negative one is not an error: measured on
        /// both kernels, it is ignored, reports nothing, and does not count
        /// towards the return value.
        Fd : int
        /// What the caller asked about: `events`, as raw bits in the simulated
        /// flavour's own `<poll.h>` numbering. `POLLERR`, `POLLHUP` and
        /// `POLLNVAL` are reported whether or not they appear here, so a caller
        /// may leave them out and still be told about them.
        Events : int16
    }

/// Why this kernel will not answer a `poll`.
///
/// Distinct from an errno: an errno is an answer, and these are the inputs for
/// which this library has measured what real kernels do and found no single
/// answer to give.
[<RequireQualifiedAccess>]
type PollRefusal =
    /// This kernel models `poll(2)` for one flavour only, and it is not this
    /// one.
    ///
    /// Darwin's answer is not one level masked by the request, as Linux's is:
    /// what it reports depends on which bits were asked for together. A request
    /// carrying `POLLEXTEND`, `POLLATTRIB`, `POLLNLINK` or `POLLWRITE` on a
    /// socket answers `POLLNVAL`; a TCP socket whose peer has closed answers
    /// `POLLOUT` to a request for `POLLOUT` but `POLLIN|POLLHUP` to a request
    /// for `POLLIN|POLLOUT`; a request of 0 reports nothing, even for a
    /// descriptor that is not open. So it is a second model rather than an
    /// extra column.
    | UnmodelledFlavour of flavour : SimulatedUnixFlavour
    /// The entry names a socket event port, which this kernel does not answer
    /// `poll(2)` for.
    ///
    /// Reachable in a way epoll's equivalent is not: `epoll_ctl` screens the
    /// targets it will accept, and `poll(2)` accepts any descriptor.
    | UnmodelledTarget of fd : int
    /// No entry carries anything and the timeout is not zero, so a real `poll`
    /// sleeps here until a descriptor becomes ready or the timeout expires.
    ///
    /// Not `SyscallOutcome.WouldBlock`, for the reason `accept`'s `WouldPark` is
    /// not: blocking is an outcome only where there is a `WakeCondition` to hand
    /// back, and this library has none carrying a poll's captured entry set and
    /// its deadline.
    ///
    /// Every *other* case is answerable whatever the timeout, which is measured
    /// rather than assumed: an entry carrying anything at all -- a requested
    /// `IN`/`OUT`, an unrequested `HUP`, or `NVAL` -- makes a real poll return
    /// immediately at any timeout.
    | WouldPark of timeoutMilliseconds : int

[<RequireQualifiedAccess>]
module PollRefusal =
    /// What this kernel knows about why it cannot answer. The client supplies
    /// its own half -- which entry point asked, and what it should do instead.
    let describe (refusal : PollRefusal) : string =
        match refusal with
        | PollRefusal.UnmodelledFlavour flavour ->
            $"this kernel is %O{flavour}-flavoured, and `poll(2)` is modelled here for Linux only. Darwin's answer is not one level masked by the request: it registers a kqueue filter per group of requested bits, so which bits were asked together decides what is reported (a vnode bit on a socket answers POLLNVAL, a reported HUP suppresses OUT, and a request of 0 reports nothing even for a descriptor that is not open). Model that before polling under this flavour."
        | PollRefusal.UnmodelledTarget fd ->
            $"fd %d{fd} names a socket event port, which this kernel does not answer `poll(2)` for. Linux answers it by re-polling the port's ready list, as `epoll_wait` does, and what that walk leaves in the list is unmeasured; model that before answering."
        | PollRefusal.WouldPark timeoutMilliseconds ->
            $"no entry carries anything and the timeout is %d{timeoutMilliseconds}ms, so a real `poll(2)` would sleep. This library models no parked poll: `WakeCondition` has no case carrying a poll's entry set and its deadline, so a park here would never end. A poll with anything already ready is answered at any timeout; only this case needs the park."

/// What a wait for socket events settles before it can either deliver or sleep:
/// `epoll_wait(2)`'s screens under one flavour, `kevent(2)`'s under the other.
///
/// Five of the eight measured rows differ between the two, so this is a
/// flavour-branching ladder throughout rather than in one place -- which is why
/// it is a kernel answer rather than something a client can assemble from parts.
[<RequireQualifiedAccess>]
type SocketWaitAdmission =
    /// The syscall was reached and failed. A client that keeps a last-error slot
    /// records this errno, and one whose foreign-function layer writes a
    /// sentinel through the caller's count does that too.
    | Failed of error : UnixError
    /// Answered with no events, having neither consulted the port nor slept.
    ///
    /// The one input on which the flavours disagree about whether the call
    /// blocks at all: measured, `kevent(kq, NULL, 0, evs, 0, NULL)` returns 0
    /// immediately where `epoll_wait` with `maxevents == 0` is EINVAL.
    | NoEvents
    /// The call reaches the port: take up to `maxEvents` events off it, and
    /// sleep if that delivers nothing.
    | DeliverOrWait of port : OpenFileDescriptionId * maxEvents : int

/// Why this kernel will not answer a wait for socket events.
[<RequireQualifiedAccess>]
type SocketWaitRefusal =
    /// The buffer reached this platform's up-front address screen and has no
    /// address to screen.
    ///
    /// Only one flavour has such a screen -- Darwin's `kevent` checks no buffer
    /// at all, and a wait that never delivers never copies -- so this is
    /// reachable under Linux alone.
    | Buffer of BufferRefusal

[<RequireQualifiedAccess>]
module SocketWaitRefusal =
    /// What this kernel knows about why it cannot answer. The client supplies
    /// its own half -- which entry point asked, and what it actually passed.
    let describe (refusal : SocketWaitRefusal) : string =
        match refusal with
        | SocketWaitRefusal.Buffer refusal -> BufferRefusal.describe refusal

/// The `event` argument of `epoll_ctl(2)`, as the kernel's copy-in finds it.
///
/// The caller classifies it, because only the caller knows what its memory
/// holds; the consequence is the kernel's, which is `EFAULT` for any operation
/// but `EPOLL_CTL_DEL` (which never reads it) ahead of every other check.
[<RequireQualifiedAccess>]
type EpollEventArgument =
    /// The pointer names a whole readable `struct epoll_event`, holding these.
    /// `events` is in Linux's `<sys/epoll.h>` numbering (`EpollEvents`), and
    /// may carry any bits: `epoll_ctl` rejects none of them outright.
    | Readable of events : uint32 * data : uint64
    /// The pointer is null, or does not name a whole readable
    /// `struct epoll_event`.
    | Unreadable

/// Why `epoll_ctl(2)` failed, in the order Linux decides them.
///
/// Measured on Linux 6.18.5 by `epoll-ctl.c`
/// (docs/plans/2026-08-23-posix-kernel-extraction), which tried every
/// combination of seven kinds of `epfd`, fourteen kinds of target, nine
/// operation values, a null and a real event pointer, and 36 event masks;
/// each adjacent pair below is separated by an input that provokes exactly one
/// of the two.
[<RequireQualifiedAccess>]
type EpollCtlError =
    /// The event could not be read, for an operation that reads one (every
    /// operation value but `EPOLL_CTL_DEL`, including unrecognised ones);
    /// `EFAULT`. First of everything.
    | EventUnreadable
    /// `epfd` is not a live descriptor; `EBADF`.
    | BadPortFd
    /// The target fd is not a live descriptor; `EBADF`.
    | BadTargetFd
    /// The target supports no poll -- a regular file or a directory; `EPERM`.
    /// Ahead of the not-a-port check, so a file as both port and target is
    /// `EPERM`.
    | TargetNotPollable
    /// `epfd` is not an epoll instance, or `epfd` and the target name the same
    /// open file description (a `dup` of the port included); `EINVAL`.
    | NotAnEventPort
    /// The event carries `EPOLLEXCLUSIVE` where it is not permitted: on
    /// `EPOLL_CTL_MOD`, or on `EPOLL_CTL_ADD` with an epoll instance as the
    /// target or with any bit outside `EPOLLIN`, `EPOLLOUT`, `EPOLLERR`,
    /// `EPOLLHUP`, `EPOLLWAKEUP`, `EPOLLET` and `EPOLLEXCLUSIVE` itself;
    /// `EINVAL`. Ahead of the table, so it wins over `EEXIST` and `ENOENT`.
    | ExclusiveNotPermitted
    /// `EPOLL_CTL_ADD` of a target already registered; `EEXIST`.
    | AlreadyRegistered
    /// `EPOLL_CTL_MOD` or `EPOLL_CTL_DEL` of a target not registered;
    /// `ENOENT`.
    | NotRegistered
    /// The operation is none of `EPOLL_CTL_ADD`, `EPOLL_CTL_DEL` and
    /// `EPOLL_CTL_MOD`; `EINVAL`. Last: every check above still applies to
    /// it, the copy-in included.
    | UnrecognisedOperation

[<RequireQualifiedAccess>]
module EpollCtlError =
    /// The errno `epoll_ctl(2)` answers for this failure.
    ///
    /// Not injective: two failures share `EBADF` and three share `EINVAL`, so a
    /// client wanting to know *which* keeps the case.
    let toErrno (error : EpollCtlError) : UnixError =
        match error with
        | EpollCtlError.EventUnreadable -> UnixError.EFAULT
        | EpollCtlError.BadPortFd
        | EpollCtlError.BadTargetFd -> UnixError.EBADF
        | EpollCtlError.TargetNotPollable -> UnixError.EPERM
        | EpollCtlError.NotAnEventPort
        | EpollCtlError.ExclusiveNotPermitted
        | EpollCtlError.UnrecognisedOperation -> UnixError.EINVAL
        | EpollCtlError.AlreadyRegistered -> UnixError.EEXIST
        | EpollCtlError.NotRegistered -> UnixError.ENOENT

/// What `epoll_ctl(2)` answered.
[<RequireQualifiedAccess>]
type EpollCtlAnswer =
    /// Applied. The system this rides with carries the new interest table, and
    /// the ready list if the change made the target pending.
    | Changed
    /// `epoll_ctl(2)` failed. Nothing changed: the system comes back as it
    /// was.
    | Failed of reason : EpollCtlError

/// Why this kernel will not answer an `epoll_ctl(2)`.
///
/// Distinct from a `Failed` answer: that is `epoll_ctl(2)` refusing, and this is
/// this library having nothing to say. Every refusal but `UnmodelledFlavour`
/// is given only where the real call would succeed, having passed every check
/// that could fail it; so a call that would fail is answered with its failure
/// whatever it asks for.
[<RequireQualifiedAccess>]
type EpollCtlRefusal =
    /// This kernel models `epoll_ctl` for one flavour only, and it is not this
    /// one.
    ///
    /// kqueue's model is *structurally* different rather than differently
    /// numbered: registration is per `(ident, filter)`, a re-`ADD` silently
    /// replaces where epoll answers `EEXIST`, a regular file registers where
    /// epoll answers `EPERM`, and a `DEL` of a dead target answers `ENOENT`
    /// where epoll answers `EBADF`. Each of those is measured only far enough to
    /// know that it diverges, which is not far enough to model the state a call
    /// leaves behind.
    | UnmodelledFlavour of flavour : SimulatedUnixFlavour
    /// An `EPOLL_CTL_ADD` whose target is itself an epoll instance.
    ///
    /// Linux accepts one, subject to a loop check and a nesting depth of at
    /// most four ports (`ELOOP` beyond either). This library does not model
    /// what a nested port reports or how a wake propagates through it.
    | NestedPort of targetFd : int
    /// The event carries `EPOLLEXCLUSIVE`, which changes which of several ports
    /// on one target a wake reaches. This library's wakes reach every port.
    | Exclusive
    /// The event carries `EPOLLONESHOT`, which disarms the registration once it
    /// has reported. This library's registrations stay armed.
    | OneShot
    /// The event carries `EPOLLWAKEUP`. The kernel keeps it only for a caller
    /// with `CAP_BLOCK_SUSPEND` on a kernel built with power management, and
    /// silently clears it otherwise; this library models neither.
    | WakeUp
    /// The event lacks `EPOLLET`, asking to be level-triggered. This port
    /// models edge-triggered registrations only: a wait consumes every entry it
    /// walks and never re-arms a still-ready one, so a wait after a partly
    /// drained level would sleep where a real `epoll_wait` returns again.
    | LevelTriggered

[<RequireQualifiedAccess>]
module EpollCtlRefusal =
    /// What this kernel knows about why it will not answer. The client supplies
    /// its own half -- which of its entry points was asked, and on whose
    /// behalf.
    let describe (refusal : EpollCtlRefusal) : string =
        match refusal with
        | EpollCtlRefusal.UnmodelledFlavour flavour ->
            $"this kernel is %O{flavour}-flavoured, and epoll_ctl is modelled here for Linux only. kqueue's semantics -- per-filter state, a silently-replacing ADD, file targets succeeding -- are unmeasured beyond the fact that they diverge from epoll's, and the return codes alone are not a model of the state a call leaves behind. Measure them before answering."
        | EpollCtlRefusal.NestedPort targetFd ->
            $"fd %d{targetFd} is itself an epoll instance. Linux would register it (subject to a loop check and a nesting depth of four, both ELOOP), but what a nested port reports and how a wake propagates through one are not modelled."
        | EpollCtlRefusal.Exclusive ->
            "the event carries EPOLLEXCLUSIVE, and the registration would succeed. An exclusive registration changes which of several ports on one target a wake reaches, and this library's wakes reach every port. Model wake-one delivery before answering."
        | EpollCtlRefusal.OneShot ->
            "the event carries EPOLLONESHOT, and the registration would succeed. A one-shot registration disarms once it has reported until a MOD re-arms it, and this library's registrations stay armed. Model disarming before answering."
        | EpollCtlRefusal.WakeUp ->
            "the event carries EPOLLWAKEUP, and the registration would succeed. The kernel keeps the bit only for a caller with CAP_BLOCK_SUSPEND on a kernel built with power management, clearing it silently otherwise, and this library models neither capabilities nor wakeup sources."
        | EpollCtlRefusal.LevelTriggered ->
            "the event lacks EPOLLET, asking to be level-triggered, and the registration would succeed. This port models edge-triggered registrations only: the ready list is consumed as it is drained and a still-ready entry is never re-armed, so a wait after a partly drained level would sleep where a real epoll_wait returns again. Register with EPOLLET, or model level-triggering before answering."

[<RequireQualifiedAccess>]
module UnixPoll =

    /// Everything a wait for socket events settles before it consults the port:
    /// `epoll_wait(2)`'s four screens or `kevent(2)`'s two, in the order each
    /// kernel applies them. See `SocketWaitAdmission`.
    ///
    /// `maxEvents` must not be negative. Neither kernel is ever asked one -- a
    /// foreign-function layer that reads it out of a caller's cell screens it
    /// there -- so a caller that has not is asking a question this library has no
    /// answer for.
    ///
    /// Each ordering is measured, on Linux 6.18.5 and Darwin 25.6.0, rather than
    /// read off the kernel sources: the widely-reproduced `do_epoll_wait` listing
    /// checks `maxevents` and `access_ok` *before* `fdget`, and current kernels
    /// do not.
    ///
    /// Changes nothing: everything a wait does before it reaches the port is a
    /// question.
    let admitSocketWait<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (fd : int)
        (maxEvents : int)
        (buffer : UserBuffer)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SocketWaitAdmission, SocketWaitRefusal>
        =
        if maxEvents < 0 then
            failwith
                $"UnixPoll.admitSocketWait: maxEvents %d{maxEvents} is negative, which neither kernel is ever asked -- the layer that reads it out of the caller's cell answers for a negative itself. Screen this in the client (this is a bug in the caller)."

        let openFile =
            FileDescriptorRegistry.tryFindWithId fd system.Process.FileDescriptors

        match SimulatedUnixPlatform.flavour system.Machine.UnixPlatform with
        | SimulatedUnixFlavour.Linux ->
            // Measured on 6.18.5, each adjacent pair separated by an input that
            // provokes exactly one of the two: descriptor, then `maxevents`,
            // then the buffer, then is-it-an-epoll-instance.
            match openFile with
            | None -> Ok (SocketWaitAdmission.Failed UnixError.EBADF)
            | Some (port, description) ->

            // The kernel's predicate is `maxevents <= 0 || maxevents > EP_MAX_EVENTS`.
            // Zero is the only non-positive value that reaches here, negatives
            // having been screened by the caller.
            if maxEvents = 0 || maxEvents > LinuxEpollLimits.MaxEvents then
                Ok (SocketWaitAdmission.Failed UnixError.EINVAL)
            else

            // The byte range `access_ok(events, maxevents * sizeof(struct
            // epoll_event))` screens. This multiplication is safe only *below*
            // the cap just applied, which is what `EP_MAX_EVENTS` exists for: it
            // is `INT_MAX / EventSize`, so every count that reaches here has a
            // product inside `int32`.
            let bufferExtent = uint64 maxEvents * uint64 LinuxEpollLimits.EventSize

            // Not a mappedness check. On 64-bit Linux `access_ok` only rejects
            // ranges reaching into the kernel half, so a merely-unmapped
            // userspace address passes and the wait then blocks, faulting at
            // delivery -- which is why this must not eagerly demand that the
            // buffer be real before sleeping.
            match
                UserBufferCheck.faultsBeforeOperationFor
                    (UnixMachineState.userBufferCheck system.Machine)
                    buffer
                    bufferExtent
            with
            | Error refusal -> Error (SocketWaitRefusal.Buffer refusal)
            | Ok true -> Ok (SocketWaitAdmission.Failed UnixError.EFAULT)
            | Ok false ->

            match description.Target with
            | OpenFileTarget.StandardStream _
            | OpenFileTarget.File _
            | OpenFileTarget.Socket _ ->
                // A live descriptor onto the wrong kind of object. EINVAL is
                // epoll's own answer for it, and it is the last of the four
                // screens -- behind the buffer, which is why an unmappable
                // buffer on a non-port descriptor is EFAULT rather than this.
                //
                // A socket is measured to be exactly like the other two here
                // rather than assumed to be: `epoll_wait` on a socket fd is
                // EINVAL, and EFAULT still wins ahead of it for an unmappable
                // buffer.
                Ok (SocketWaitAdmission.Failed UnixError.EINVAL)
            | OpenFileTarget.SocketEventPort _ -> Ok (SocketWaitAdmission.DeliverOrWait (port, maxEvents))
        | SimulatedUnixFlavour.Darwin ->
            // Measured on 25.6.0, and flatter: `kevent` resolves the descriptor
            // before its `nevents == 0` early return, has no "wrong kind of
            // object" answer to give, and screens no buffer at all -- so the
            // whole ladder is one question about the descriptor followed by one
            // about the count.
            match openFile with
            | None -> Ok (SocketWaitAdmission.Failed UnixError.EBADF)
            | Some (port, description) ->

            match description.Target with
            | OpenFileTarget.StandardStream _
            | OpenFileTarget.File _
            | OpenFileTarget.Socket _ ->
                // EBADF, where epoll says EINVAL: kqueue folds "not a kqueue"
                // into "bad descriptor". Measured on a socket too, and for both
                // a zero and a non-zero event count.
                Ok (SocketWaitAdmission.Failed UnixError.EBADF)
            | OpenFileTarget.SocketEventPort portState ->

            if maxEvents = 0 then
                Ok SocketWaitAdmission.NoEvents
            else

            // No buffer screen, so an unmappable buffer sleeps here rather than
            // faulting: `UserBufferCheck.AtCopyTime` is Darwin's answer, and a
            // wait that never delivers an event never copies anything.
            //
            // The port is empty by construction on this flavour -- the Darwin
            // registration arm refuses every change, so nothing can ever become
            // deliverable -- which is what makes it faithful to hand this to the
            // same delivery walk epoll uses and have it sleep. The assertion ties
            // those two facts together rather than leaving the second to be
            // rediscovered.
            if not (Map.isEmpty portState.Registrations) then
                failwith
                    $"UnixPoll.admitSocketWait: a Darwin-flavoured kernel holds %d{Map.count portState.Registrations} socket event registrations, but the Darwin registration arm refuses every change (this is a bug in the caller's state construction)."

            Ok (SocketWaitAdmission.DeliverOrWait (port, maxEvents))

    /// `epoll_ctl(2)`: apply `op` to the interest table of the epoll instance
    /// `epfd` names, for the target `fd` names, with the `event` the caller
    /// passed.
    ///
    /// `op` is the raw operation value, in Linux's numbering: `EPOLL_CTL_ADD`
    /// is 1, `EPOLL_CTL_DEL` 2 and `EPOLL_CTL_MOD` 3, and every other value is
    /// answered as Linux answers it. `event` is what the kernel's copy-in finds
    /// (see `EpollEventArgument`); it is not read for `EPOLL_CTL_DEL`.
    ///
    /// Under the Linux flavour every failure is answered, in Linux's order (see
    /// `EpollCtlError`), and so is every `EPOLL_CTL_DEL` and every `ADD` or
    /// `MOD` of an edge-triggered registration without `EPOLLEXCLUSIVE`,
    /// `EPOLLONESHOT` or `EPOLLWAKEUP`, whatever other bits it carries. The
    /// registration stores the caller's `events` with `EPOLLERR` and `EPOLLHUP`
    /// added, and a wait reports the target's readiness masked by that (see
    /// `LinuxReadiness`); an `ADD` or `MOD` whose target is ready under the new
    /// mask makes the registration pending at once, and a `MOD` of an entry
    /// already pending leaves its place alone. An `ADD` or `MOD` that would
    /// succeed with one of the modes this library does not model, and an
    /// `ADD` of another epoll instance, are refused (see `EpollCtlRefusal`).
    /// Under the Darwin flavour every call is refused: kqueue is not epoll.
    let epollCtl<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (epfd : int)
        (op : int)
        (fd : int)
        (event : EpollEventArgument)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<EpollCtlAnswer * UnixSystem<'Task, 'Handler>, EpollCtlRefusal>
        =
        match SimulatedUnixPlatform.flavour system.Machine.UnixPlatform with
        | SimulatedUnixFlavour.Darwin -> Error (EpollCtlRefusal.UnmodelledFlavour SimulatedUnixFlavour.Darwin)
        | SimulatedUnixFlavour.Linux ->

        let failed (error : EpollCtlError) =
            Ok (EpollCtlAnswer.Failed error, system)

        // Linux's `EPOLL_CTL_*` values.
        let add = 1
        let del = 2
        let modify = 3

        // Measured on 6.18.5 (`epoll-ctl.c`), each step separated from the
        // next by an input that provokes exactly one of the two. The copy-in
        // comes first, and is skipped for DEL alone: an unrecognised op reads
        // the event too.
        let readEvent =
            if op = del then
                Ok (0u, 0UL)
            else
                match event with
                | EpollEventArgument.Readable (events, data) -> Ok (events, data)
                | EpollEventArgument.Unreadable -> Error ()

        match readEvent with
        | Error () -> failed EpollCtlError.EventUnreadable
        | Ok (events, data) ->

        let registry = system.Process.FileDescriptors

        match FileDescriptorRegistry.tryFindWithId epfd registry with
        | None -> failed EpollCtlError.BadPortFd
        | Some (portId, portDescription) ->

        match FileDescriptorRegistry.tryFindWithId fd registry with
        | None -> failed EpollCtlError.BadTargetFd
        | Some (targetId, targetDescription) ->

        // `file_can_poll`: the target must have a `->poll` handler, which a
        // regular file and a directory lack.
        match targetDescription.Target with
        | OpenFileTarget.File _ -> failed EpollCtlError.TargetNotPollable
        | OpenFileTarget.StandardStream _
        | OpenFileTarget.SocketEventPort _
        | OpenFileTarget.Socket _ ->

        // One kernel test, `f.file == tf.file || !is_file_epoll(f.file)`, so
        // one answer: a `dup` of the port as target is this, not success.
        let portState =
            match portDescription.Target with
            | OpenFileTarget.SocketEventPort portState when portId <> targetId -> Some portState
            | OpenFileTarget.SocketEventPort _
            | OpenFileTarget.StandardStream _
            | OpenFileTarget.File _
            | OpenFileTarget.Socket _ -> None

        match portState with
        | None -> failed EpollCtlError.NotAnEventPort
        | Some portState ->

        let targetIsPort =
            match targetDescription.Target with
            | OpenFileTarget.SocketEventPort _ -> true
            | OpenFileTarget.StandardStream _
            | OpenFileTarget.File _
            | OpenFileTarget.Socket _ -> false

        // The EPOLLEXCLUSIVE screen, ahead of the table (so ahead of EEXIST
        // and ENOENT) and applied to ADD and MOD only: 20000 random masks for
        // each of six (op, table state) rows found no exception to it.
        let exclusivePermitted =
            EpollEvents.In
            ||| EpollEvents.Out
            ||| EpollEvents.Err
            ||| EpollEvents.Hup
            ||| EpollEvents.WakeUp
            ||| EpollEvents.EdgeTriggered
            ||| EpollEvents.Exclusive

        let exclusive = events &&& EpollEvents.Exclusive <> 0u

        if
            exclusive
            && (op = modify
                || op = add && (targetIsPort || events &&& ~~~exclusivePermitted <> 0u))
        then
            failed EpollCtlError.ExclusiveNotPermitted
        // Where Linux runs its loop and depth checks, which cannot fail on any
        // table this library builds (it never holds a nested port) but which
        // precede EEXIST in the kernel.
        elif op = add && targetIsPort then
            Error (EpollCtlRefusal.NestedPort fd)
        else

        let key = fd, targetId
        let registered = Map.containsKey key portState.Registrations

        // The modes this port does not model, refused only where the call
        // would otherwise commit.
        let unmodelledMode : EpollCtlRefusal option =
            if exclusive then
                Some EpollCtlRefusal.Exclusive
            elif events &&& EpollEvents.OneShot <> 0u then
                Some EpollCtlRefusal.OneShot
            elif events &&& EpollEvents.WakeUp <> 0u then
                Some EpollCtlRefusal.WakeUp
            elif events &&& EpollEvents.EdgeTriggered = 0u then
                Some EpollCtlRefusal.LevelTriggered
            else
                None

        // `epoll_ctl` forces these two into every stored mask (measured through
        // `/proc/self/fdinfo`, `fdinfo.c`).
        let stored = events ||| EpollEvents.Err ||| EpollEvents.Hup

        let withRegistry (registry : FileDescriptorRegistry) (system : UnixSystem<'Task, 'Handler>) =
            { system with
                Process =
                    { system.Process with
                        FileDescriptors = registry
                    }
            }

        // An ADD or MOD whose target is ready under the new mask makes the
        // registration pending at that moment (measured rows E, I and K), and
        // a MOD of an entry already pending leaves its place alone (row L).
        let pendIfReady (system : UnixSystem<'Task, 'Handler>) : UnixSystem<'Task, 'Handler> =
            let alreadyPending = List.contains key portState.Ready

            if
                not alreadyPending
                && LinuxReadiness.ofDescription targetId system &&& stored <> 0u
            then
                withRegistry
                    (FileDescriptorRegistry.appendSocketEventReady portId key system.Process.FileDescriptors)
                    system
            else
                system

        if op = add then
            if registered then
                failed EpollCtlError.AlreadyRegistered
            else

            match unmodelledMode with
            | Some refusal -> Error refusal
            | None ->

            let ordinal = system.Machine.NextSocketEventRegistrationOrdinal

            let registration =
                {
                    Events = stored
                    Data = data
                    RegisteredAt = ordinal
                }

            let system =
                { withRegistry (FileDescriptorRegistry.addEpollRegistration portId key registration registry) system with
                    Machine =
                        { system.Machine with
                            NextSocketEventRegistrationOrdinal = ordinal + 1L
                        }
                }

            Ok (EpollCtlAnswer.Changed, pendIfReady system)
        elif op = del then
            if registered then
                Ok (
                    EpollCtlAnswer.Changed,
                    withRegistry (FileDescriptorRegistry.removeEpollRegistration portId key registry) system
                )
            else
                failed EpollCtlError.NotRegistered
        elif op = modify then
            if not registered then
                failed EpollCtlError.NotRegistered
            else

            match unmodelledMode with
            | Some refusal -> Error refusal
            | None ->

            // No stored mask here carries EPOLLEXCLUSIVE, whose MOD the kernel
            // would answer EINVAL, because an exclusive ADD is refused.
            let system =
                withRegistry (FileDescriptorRegistry.modifyEpollRegistration portId key stored data registry) system

            Ok (EpollCtlAnswer.Changed, pendIfReady system)
        else
            failed EpollCtlError.UnrecognisedOperation

    // Linux's `<poll.h>` numbering: the bits a Linux-flavoured `poll(2)` reads
    // in `events` and writes in `revents`.
    //
    // Measured 2026-09-23 on Linux 6.18.5 aarch64 (glibc 2.41) by
    // `docs/plans/2026-08-23-posix-kernel-extraction/poll-alphabet.c`, which
    // printed the header and then polled every object and state
    // `LinuxReadiness.ofDescription` answers for with all 65536 request masks
    // at timeout 0. On every object, every answer was `level & (events |
    // POLLERR | POLLHUP)` and `rv` counted exactly the entries with a non-zero
    // `revents`; a descriptor that is not open answered `POLLNVAL` alone to all
    // 65536; no request failed.
    //
    // No modelled object presents `POLLPRI`, `POLLRDBAND` or `POLLMSG`.
    // `POLLREMOVE` (0x1000), the unassigned
    // 0x0800 and the kernel-internal 0x4000 and 0x8000 were never reported
    // either, which is the shape of `do_pollfd`: it reads a request through
    // `demangle_poll`, which maps only the named bits other than `POLLREMOVE`,
    // so the rest never reach the filter the level is masked by. That held
    // for 0x8000 even on a socket with `SO_BUSY_POLL` set, the one case in
    // which a socket's own poll handler adds that bit to its mask.
    let private linuxPollErr : int16 = 0x0008s
    let private linuxPollHup : int16 = 0x0010s
    let private linuxPollNval : int16 = 0x0020s

    /// `poll(2)`: what each entry reports right now, and how many entries carry
    /// anything.
    ///
    /// Each entry's `Events`, and each `revents` answered for it, is the raw
    /// bits in the simulated flavour's own `<poll.h>` numbering. Under the Linux
    /// flavour every bit is answered as a real kernel answers it: each named
    /// bit is reported when the descriptor presents it and the entry asked for
    /// it, `POLLERR` and `POLLHUP` whether asked for or not, and `POLLNVAL`
    /// alone for a descriptor that is not open. A bit Linux does not read
    /// (`POLLREMOVE`, 0x0800, 0x4000 and 0x8000) is ignored, as it is there.
    /// Under the Darwin flavour every poll is refused.
    ///
    /// The count is `poll(2)`'s own return value, and it is neither the number
    /// of entries nor the number of *conditions*: it counts entries carrying
    /// something. Derivable from the list, and answered here so that no client
    /// re-derives a kernel rule.
    ///
    /// `milliseconds` is read as `poll(2)` reads it -- zero means "answer now",
    /// and every other value means "sleep until something happens", negative
    /// included. A foreign-function layer that screens some negative values
    /// itself does that before calling.
    ///
    /// Changes nothing and returns no system: a `poll` asks.
    let poll<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (entries : PollEntry list)
        (milliseconds : int)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<int16 list * int, PollRefusal>
        =
        // Ahead of the entries, and so ahead of an empty entry list too: a
        // zero-entry poll answers `rv = 0` identically on both flavours and
        // consults no readiness at all, but answering that one row would be a
        // branch reachable only from a flavour whose every other row refuses.
        //
        // Darwin's alphabet (`<poll.h>` on 25.6.0 arm64, 2026-09-23): the six
        // shared bits, POLLRDNORM 0x40, POLLRDBAND 0x80, POLLWRNORM = POLLOUT,
        // POLLWRBAND 0x100, POLLEXTEND 0x200, POLLATTRIB 0x400, POLLNLINK 0x800,
        // POLLWRITE 0x1000. `poll-alphabet.c`'s full sweep there finds no
        // request that fails, and no object for which the answer is one level
        // masked by the request: `poll` registers EVFILT_READ for any of
        // IN/RDNORM/PRI/RDBAND/HUP, EVFILT_WRITE for any of OUT/WRNORM/WRBAND,
        // and EVFILT_VNODE for any of the four vnode bits, and a filter the
        // descriptor cannot take turns the whole entry into POLLNVAL (a vnode
        // bit on every socket, pipe and kqueue; a read or write bit on a
        // directory; a write bit on a kqueue; any of them on a descriptor that
        // is not open). ERR and NVAL, and 0x2000..0x8000, register nothing, so
        // a request of only those reports nothing at all.
        match SimulatedUnixPlatform.flavour system.Machine.UnixPlatform with
        | SimulatedUnixFlavour.Darwin -> Error (PollRefusal.UnmodelledFlavour SimulatedUnixFlavour.Darwin)
        | SimulatedUnixFlavour.Linux ->

        let reportOne (entry : PollEntry) : Result<int16, PollRefusal> =
            if entry.Fd < 0 then
                // Measured on both kernels: a negative descriptor is ignored,
                // reports nothing, and does not count towards the return value.
                // It is not an error and not NVAL.
                Ok 0s
            else

            match FileDescriptorRegistry.tryFindWithId entry.Fd system.Process.FileDescriptors with
            | None ->
                // POLLNVAL is a statement about the entry, not a readiness
                // level: measured, it is reported alone, whatever was asked
                // for, `events = 0` included.
                Ok linuxPollNval
            | Some (descriptionId, description) ->

            match description.Target with
            // Measured on Linux (`poll-alphabet.c`): POLLIN|POLLRDNORM when an
            // event is deliverable, nothing otherwise, under the same
            // `level & (events | POLLERR | POLLHUP)` rule. Refused anyway,
            // because the kernel computes that level by re-polling the ready
            // list, and whether the walk drops a stale entry, as `drain` does,
            // is unmeasured.
            | OpenFileTarget.SocketEventPort _ -> Error (PollRefusal.UnmodelledTarget entry.Fd)
            | OpenFileTarget.Socket _
            | OpenFileTarget.File _
            | OpenFileTarget.StandardStream _ ->
                // `do_pollfd`'s own shape: the level, filtered by the request
                // with POLLERR and POLLHUP added whatever was asked.
                // The level's bits all lie below 0x10000, where `<poll.h>`
                // and `<sys/epoll.h>` share their numbering.
                int16 (LinuxReadiness.ofDescription descriptionId system)
                &&& (entry.Events ||| linuxPollErr ||| linuxPollHup)
                |> Ok

        // In list order, stopping at the first entry that cannot be answered:
        // a real `poll` inspects its entries in order, so that is the entry a
        // refusal names.
        let rec report (remaining : PollEntry list) (acc : int16 list) : Result<int16 list, PollRefusal> =
            match remaining with
            | [] -> Ok (List.rev acc)
            | entry :: rest ->
                match reportOne entry with
                | Error refusal -> Error refusal
                | Ok events -> report rest (events :: acc)

        let reported = report entries []

        match reported with
        | Error refusal -> Error refusal
        | Ok reported ->

        let triggered =
            reported |> List.filter (fun revents -> revents <> 0s) |> List.length

        if triggered = 0 && milliseconds <> 0 then
            Error (PollRefusal.WouldPark milliseconds)
        else
            Ok (reported, triggered)

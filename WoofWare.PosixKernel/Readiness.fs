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
/// answers, both ends of a pipe, and regular files, directories and character
/// devices (which epoll will not register, but `poll` answers). An epoll
/// instance is refused: what either waiter reports for one is not modelled.
[<RequireQualifiedAccess>]
module LinuxReadiness =

    /// The mask the descriptor `targetId` names presents right now.
    let ofDescription<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (targetId : OpenFileDescriptionId)
        (system : UnixSystem<'Task, 'Handler>)
        : uint32
        =
        match OpenFileTable.tryFind targetId system.Machine.OpenFiles with
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
            // datagram, seqpacket, and Linux's raw request, which makes a
            // datagram socket; all fresh), and never on TCP (idle,
            // listening, established with the peer alive or gone, refused) --
            // `tcp_poll` sets OUT|WRNORM, where `datagram_poll` and the
            // Unix-domain handlers set OUT|WRNORM|WRBAND.
            let writeBand =
                match socket.Domain, socket.Kind with
                | SocketDomain.Unix, _ -> true
                | SocketDomain.Inet, SocketKind.Datagram
                | SocketDomain.Inet6, SocketKind.Datagram -> true
                | SocketDomain.Inet, SocketKind.Stream
                | SocketDomain.Inet6, SocketKind.Stream -> false
                | SocketDomain.Inet, SocketKind.SeqPacket
                | SocketDomain.Inet6, SocketKind.SeqPacket ->
                    failwith
                        $"LinuxReadiness.ofDescription: socket %O{socketId} is %O{socket.Kind} in %O{socket.Domain}, which this kernel never creates (nothing here creates SCTP), so what a waiter reports for it is unmeasured (this is a bug in the caller's state construction)."

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
        | OpenFileTarget.File _
        | OpenFileTarget.Directory _
        | OpenFileTarget.CharacterDevice _ ->
            // Measured through `poll`: a regular file answers IN|OUT|RDNORM|
            // WRNORM at every offset, empty or not, and under every access
            // mode, and a directory answers the same. Files have no `->poll`
            // handler, so `vfs_poll` reports `DEFAULT_POLLMASK` for them. The
            // same missing handler is why `epoll_ctl` answers EPERM for one, so
            // only `poll` asks.
            //
            // `/dev/null` and `/dev/urandom` have none either, and answer the
            // same mask under every access mode (`devices.c`, POLL rows).
            // `/dev/random` is the device that does, and is not modelled.
            EpollEvents.In ||| EpollEvents.Out ||| EpollEvents.RdNorm ||| EpollEvents.WrNorm
        | OpenFileTarget.Pipe (pipeId, PipeEnd.Read) ->
            // Measured on Linux 6.18.5 (`pipe-states.c` in
            // docs/plans/2026-08-23-posix-kernel-extraction, and the live
            // comparison in `TestPipeAgainstHost`): IN|RDNORM while
            // the pipe holds anything, and HUP once no write end is open, data
            // or none. No PRI or RDBAND: a pipe's handler sets neither.
            let pipe = UnixMachineState.pipe pipeId system.Machine

            (if PipeBuffer.readable pipe.Buffer then
                 EpollEvents.In ||| EpollEvents.RdNorm
             else
                 0u)
            ||| (if UnixMachineState.pipeEndOpen pipeId pipe PipeEnd.Write system.Machine then
                     0u
                 else
                     EpollEvents.Hup)
        | OpenFileTarget.Pipe (pipeId, PipeEnd.Write) ->
            // Measured likewise: OUT|WRNORM while a slot is free (see
            // `PipeBuffer.writable`), and ERR once no read end is open, whether
            // or not a slot is. No WRBAND.
            let pipe = UnixMachineState.pipe pipeId system.Machine

            (if PipeBuffer.writable pipe.Buffer then
                 EpollEvents.Out ||| EpollEvents.WrNorm
             else
                 0u)
            ||| (if UnixMachineState.pipeEndOpen pipeId pipe PipeEnd.Read system.Machine then
                     0u
                 else
                     EpollEvents.Err)
        | OpenFileTarget.Epoll _ ->
            failwith
                $"LinuxReadiness.ofDescription: %O{targetId} is an epoll instance, and what a waiter reports for one is not modelled. `poll` refuses such an entry and `epoll_ctl` refuses to nest one, both before reaching here (this is a bug in this library)."
        | OpenFileTarget.Kqueue _ ->
            failwith
                $"LinuxReadiness.ofDescription: %O{targetId} is a kqueue, which only Darwin has, and Linux's readiness is asked only of a Linux-flavoured kernel (this is a bug in the caller's state construction)."

/// What an `epoll` instance would report if a wait on it were re-polled now,
/// and what draining one does.
///
/// The *consumer* half of the epoll model. The producer half -- seeding the
/// pending list when a registration is added or modified, and signalling a
/// registration when its target's level changes -- belongs to the operations
/// that make those changes: `UnixPoll.epollCtl`, and the socket operations in
/// `UnixConnection`.
[<RequireQualifiedAccess>]
module EpollReadyList =

    /// Each pending entry of the epoll instance, in delivery order, with what it would
    /// report if `epoll_wait` re-polled it right now: the target's current
    /// readiness restricted to the registration's stored mask.
    let private annotatedReady<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (epollState : EpollState)
        (system : UnixSystem<'Task, 'Handler>)
        : ((int * OpenFileDescriptionId) * EpollRegistration * uint32) list
        =
        epollState.Ready
        |> List.map (fun (_, targetId as key) ->
            let registration =
                match Map.tryFind key epollState.Registrations with
                | Some registration -> registration
                | None ->
                    failwith
                        $"EpollReadyList.annotatedReady: pending entry %A{key} has no registration. FileDescriptorRegistryDefect.EpollReadyEntryUnregistered exists to make this unreachable, so the system breaks UnixSystem.checkInvariants: this is a bug in this library, or in a caller that assembled the state by hand."

            let reported = LinuxReadiness.ofDescription targetId system &&& registration.Events

            key, registration, reported
        )

    /// Whether an `epoll_wait` on the epoll instance `epollId` names would return at
    /// least one event right now — the wake condition a parked waiter is
    /// polled against, and by construction the same question `drain` answers,
    /// because both read the same annotated walk.
    ///
    /// Loudly partial in `epollId`, exactly as a parked `flock`'s wake condition
    /// is: a task parked on an epoll instance holds it until its call returns, so the
    /// epoll instance cannot have gone while something waits on it. Asking about an epoll instance
    /// that has gone means a park was ended or the epoll instance destroyed some other
    /// way, and neither answer is honest: `true` wakes the waiter into an
    /// `EBADF` no kernel produces, and `false` sleeps for ever.
    let hasDeliverableEvent<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (epollId : OpenFileDescriptionId)
        (system : UnixSystem<'Task, 'Handler>)
        : bool
        =
        match OpenFileTable.tryFind epollId system.Machine.OpenFiles with
        | None ->
            failwith
                $"EpollReadyList.hasDeliverableEvent: %O{epollId} names no live open file description, but a task waits on it, and a park holds what it waits on until the call returns (this is a bug in this library, or in a caller that ended a park without its finishing call or assembled the state by hand)."
        | Some description ->

        match description.Target with
        | OpenFileTarget.File _
        | OpenFileTarget.Directory _
        | OpenFileTarget.Socket _
        | OpenFileTarget.CharacterDevice _
        | OpenFileTarget.Pipe _
        | OpenFileTarget.Kqueue _ ->
            failwith
                $"EpollReadyList.hasDeliverableEvent: %O{epollId} is not an epoll instance, so no wait can be parked on it (this is a bug in the caller of EpollReadyList.hasDeliverableEvent)."
        | OpenFileTarget.Epoll epollState ->
            annotatedReady epollState system
            |> List.exists (fun (_, _, reported) -> reported <> 0u)

    /// Drain the epoll instance as one `epoll_wait(maxevents = maxCount)` would: walk
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
    /// Loudly partial in `epollId`: callers hold a live epoll instance's description in
    /// hand.
    let drain<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (epollId : OpenFileDescriptionId)
        (maxCount : int)
        (system : UnixSystem<'Task, 'Handler>)
        : (uint64 * uint32) list * UnixSystem<'Task, 'Handler>
        =
        if maxCount <= 0 then
            failwith
                $"EpollReadyList.drain: maxCount %d{maxCount} is not positive; epoll answers EINVAL for it before reaching the ready list, so this is a bug in the caller of EpollReadyList.drain."

        match OpenFileTable.tryFind epollId system.Machine.OpenFiles with
        | None ->
            failwith
                $"EpollReadyList.drain: %O{epollId} names no live open file description (this is a bug in the caller of EpollReadyList.drain)."
        | Some description ->

        match description.Target with
        | OpenFileTarget.File _
        | OpenFileTarget.Directory _
        | OpenFileTarget.Socket _
        | OpenFileTarget.CharacterDevice _
        | OpenFileTarget.Pipe _
        | OpenFileTarget.Kqueue _ ->
            failwith
                $"EpollReadyList.drain: %O{epollId} is not an epoll instance (this is a bug in the caller of EpollReadyList.drain)."
        | OpenFileTarget.Epoll epollState ->

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

        let delivered, surviving = walk [] (annotatedReady epollState system)

        delivered, UnixSystemState.mapOpenFiles (OpenFileTable.setEpollReady epollId surviving) system

/// What a kqueue filter reports of a descriptor on which it is ready. `data`
/// is what the event's `data` field holds: for `EVFILT_READ` of a listener
/// the connections queued, of any other socket the bytes waiting, and for
/// `EVFILT_WRITE` the free space in the socket's send buffer
/// (`DarwinReadiness.sendBufferSpace`).
[<RequireQualifiedAccess>]
type KqueueFilterReport =
    /// The filter is ready, and reports `data`.
    | Ready of data : int64
    /// The filter is ready and reports `EV_EOF`: the socket can receive no
    /// more, or (for `EVFILT_WRITE`) send no more. Its `fflags` hold the
    /// socket's pending error, or 0 when there is none.
    | EndOfFile of data : int64 * pendingError : UnixError option

/// The readiness a Darwin-flavoured socket or pipe presents to a kqueue
/// filter.
///
/// Answers for the stream sockets of `AF_INET` and `AF_INET6` (see
/// `modelsSocket`), and for both ends of a pipe.
[<RequireQualifiedAccess>]
module DarwinReadiness =

    /// Whether this kernel models what a kqueue filter reports of `socket`: a
    /// stream socket of `AF_INET` or `AF_INET6`.
    ///
    /// A datagram socket's readiness is measured but not modelled, because
    /// what activates its filters is not: Darwin's datagram `connect` activates
    /// `EVFILT_WRITE`, where nothing in Linux's does. A Unix-domain socket's is
    /// not measured.
    let modelsSocket (socket : SocketDescription) : bool =
        match socket.Domain, socket.Kind with
        | SocketDomain.Inet, SocketKind.Stream
        | SocketDomain.Inet6, SocketKind.Stream -> true
        | SocketDomain.Unix, _
        | _, SocketKind.Datagram
        | _, SocketKind.SeqPacket -> false

    // The send buffer's cap on a TCP socket that has never connected:
    // `tcp_attach` sets `sb_preconn_hiwat` to this constant, and only a
    // completed connect clears it.
    let private preconnectSendSpace : int64 = 2048L

    /// The free space in the send buffer of the TCP socket `socket`, in bytes:
    /// what `EVFILT_WRITE` reports as its event's `data`.
    ///
    /// This kernel has no send path, so nothing is ever queued and the space
    /// is the buffer's whole size; and `SO_SNDBUF` is refused, so only the
    /// socket's creation and its handshake set that size. A connected
    /// socket's buffer, at either end, is the machine's `TcpSendSpace` rounded
    /// up on the handshake to whole loopback segments, and capped at
    /// `kern.ipc.maxsockbuf`: 146988 over IPv4 and 146808 over IPv6 at the
    /// default 131072. A refused socket's is capped at 2048 until a connect
    /// completes, which on Darwin none ever will. Measured on Darwin 27.0.0,
    /// and explained from XNU's source, in `kevent-write-data.c`.
    ///
    /// Loudly partial: the machine must be Darwin-flavoured, with a
    /// `TcpSendSpace` that `UnixBootImage.withTcpSendSpace` admits, and the
    /// socket one `modelsSocket` admits, connected or refused, which are the
    /// states in which its WRITE filter is ready.
    let sendBufferSpace (socket : SocketDescription) (machine : UnixMachineState) : int64 =
        match SimulatedUnixPlatform.flavour machine.UnixPlatform with
        | SimulatedUnixFlavour.Linux ->
            failwith
                "DarwinReadiness.sendBufferSpace: the machine is Linux-flavoured, and this is Darwin's send buffer (this is a bug in the caller: a kqueue exists only on Darwin)."
        | SimulatedUnixFlavour.Darwin -> ()

        let sendSpace = machine.TcpSendSpace

        if
            sendSpace < UnixMachineState.darwinLoopbackSendPipe
            || sendSpace > UnixMachineState.darwinSocketBufferMax
        then
            failwith
                $"DarwinReadiness.sendBufferSpace: the machine's TcpSendSpace is %d{sendSpace}, which UnixBootImage.withTcpSendSpace refuses on Darwin (this is a bug in a caller that assembled the machine by hand)."

        if not (modelsSocket socket) then
            failwith
                $"DarwinReadiness.sendBufferSpace: the socket is %O{socket.Kind} in %O{socket.Domain}, whose send buffer this kernel does not model (this is a bug in the caller)."

        match socket.Phase with
        | SocketPhase.Established _ -> int64 (TcpBufferSizing.darwinSendBuffer sendSpace socket.Domain)
        // Darwin's rule is the lesser of the two, though the cap always wins
        // while `withTcpSendSpace` admits nothing below 49152.
        | SocketPhase.Refused _ -> min (int64 sendSpace) preconnectSendSpace
        | SocketPhase.Idle
        | SocketPhase.Listening _ ->
            failwith
                $"DarwinReadiness.sendBufferSpace: the socket is %A{socket.Phase}, whose WRITE filter is never ready, so nothing reports its send buffer (this is a bug in the caller)."
        | SocketPhase.EstablishedPendingReport _ ->
            failwith
                "DarwinReadiness.sendBufferSpace: the socket is in EstablishedPendingReport, which only a Linux-flavoured connect enters (this is a bug in the caller's state construction)."
        | SocketPhase.DatagramPeer _ ->
            failwith
                "DarwinReadiness.sendBufferSpace: a stream socket holds a datagram peer, which this kernel's socket invariants forbid (this is a bug in the caller's state construction)."

    /// What the kqueue filter `filter` reports of the socket `socketId` right
    /// now, or `None` when the filter is not ready.
    ///
    /// The socket must be one `modelsSocket` admits.
    let ofSocket
        (filter : KqueueFilter)
        (socketId : SocketId)
        (machine : UnixMachineState)
        : KqueueFilterReport option
        =
        let socket = UnixMachineState.socket socketId machine

        if not (modelsSocket socket) then
            failwith
                $"DarwinReadiness.ofSocket: socket %O{socketId} is %O{socket.Kind} in %O{socket.Domain}, which `kevent` never registers a filter on (this is a bug in this library, or in a caller that assembled the state by hand)."

        // Every row measured on Darwin 27.0.0 arm64 (`kevent-register.c`,
        // sections P and X, in IPv4 and IPv6 alike), with or without EV_CLEAR.
        // No receive path is modelled, so no byte is ever waiting to be read:
        // a connected socket's READ is ready only once nothing more can come.
        let pendingError (error : RefusalError) : UnixError option =
            match error with
            | RefusalError.Pending -> Some UnixError.ECONNREFUSED
            | RefusalError.Reported -> None

        match socket.Phase, filter with
        | SocketPhase.Listening listenState, KqueueFilter.Read ->
            // Ready while a connection is queued, reporting how many are.
            match listenState.Queue with
            | [] -> None
            | queue -> Some (KqueueFilterReport.Ready (int64 (List.length queue)))
        | SocketPhase.Listening _, KqueueFilter.Write -> None
        // Bound or not: a socket that is not connected can neither be read nor
        // written.
        | SocketPhase.Idle, _ -> None
        | SocketPhase.Established (connectionId, _), KqueueFilter.Read ->
            if UnixMachineState.peerOpen socketId connectionId machine then
                None
            else
                // The peer's FIN: EV_EOF, no error, and nothing waiting.
                Some (KqueueFilterReport.EndOfFile (0L, None))
        // Writable whether or not the peer has gone, and without EV_EOF.
        | SocketPhase.Established _, KqueueFilter.Write ->
            Some (KqueueFilterReport.Ready (sendBufferSpace socket machine))
        // Both filters report EV_EOF once a connect is refused, with the error
        // in `fflags` until an `SO_ERROR` read takes it (measured: ECONNREFUSED,
        // then 0). The WRITE filter's data is still the send buffer's free space.
        | SocketPhase.Refused error, KqueueFilter.Read -> Some (KqueueFilterReport.EndOfFile (0L, pendingError error))
        | SocketPhase.Refused error, KqueueFilter.Write ->
            Some (KqueueFilterReport.EndOfFile (sendBufferSpace socket machine, pendingError error))
        | SocketPhase.EstablishedPendingReport _, _ ->
            failwith
                $"DarwinReadiness.ofSocket: socket %O{socketId} is in EstablishedPendingReport, which only a Linux-flavoured connect enters (this is a bug in the caller's state construction)."
        | SocketPhase.DatagramPeer _, _ ->
            failwith
                $"DarwinReadiness.ofSocket: stream socket %O{socketId} holds a datagram peer, which this kernel's socket invariants forbid (this is a bug in the caller's state construction)."

    /// What the kqueue filter `filter`, registered through a descriptor onto
    /// `pipeEnd` of the pipe `pipeId`, reports right now, or `None` when the
    /// filter is not ready.
    ///
    /// Either filter registers on either end. Measured on Darwin 27.0.0
    /// (`pipe-activation.c` and `poll-darwin.c`): `EVFILT_READ` on the read
    /// end is ready while the pipe holds anything, reporting how much;
    /// `EVFILT_WRITE` on the write end is ready while it is writable (see
    /// `PipeBuffer.writable`), reporting the free space
    /// (`PipeBuffer.darwinWriteSpace`); the other two are ready only once the
    /// pipe has lost an end. Once either end has no descriptor left, every
    /// filter on the surviving end is ready with `EV_EOF`, reporting 0, but a
    /// read end's `EVFILT_READ`, which still reports what the pipe holds.
    let ofPipe<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (filter : KqueueFilter)
        (pipeId : PipeId)
        (pipeEnd : PipeEnd)
        (system : UnixSystem<'Task, 'Handler>)
        : KqueueFilterReport option
        =
        let pipe = UnixMachineState.pipe pipeId system.Machine

        let otherEnd =
            match pipeEnd with
            | PipeEnd.Read -> PipeEnd.Write
            | PipeEnd.Write -> PipeEnd.Read

        let ended = not (UnixMachineState.pipeEndOpen pipeId pipe otherEnd system.Machine)

        let held = int64 (PipeBuffer.held pipe.Buffer)

        match pipeEnd, filter with
        | PipeEnd.Read, KqueueFilter.Read ->
            if ended then
                Some (KqueueFilterReport.EndOfFile (held, None))
            elif held > 0L then
                Some (KqueueFilterReport.Ready (held))
            else
                None
        | PipeEnd.Write, KqueueFilter.Write ->
            if ended then
                Some (KqueueFilterReport.EndOfFile (0L, None))
            elif PipeBuffer.writable pipe.Buffer then
                Some (KqueueFilterReport.Ready ((int64 (PipeBuffer.darwinWriteSpace pipe.Buffer))))
            else
                None
        | PipeEnd.Read, KqueueFilter.Write
        | PipeEnd.Write, KqueueFilter.Read ->
            if ended then
                Some (KqueueFilterReport.EndOfFile (0L, None))
            else
                None

/// One event a kqueue reports: the registration that reported it, and what its
/// filter reported.
type KqueueReport =
    {
        /// The descriptor the registration was made through.
        Fd : int
        /// The registration's filter.
        Filter : KqueueFilter
        /// The registration, as it stood when it reported.
        Registration : KqueueRegistration
        /// What the filter reported.
        Report : KqueueFilterReport
    }

/// Darwin's kqueue as its registrations see it: activating them when something
/// happens to a socket, and reporting them to a wait.
///
/// A registration is activated when something happens that the real kernel
/// wakes its filter for, and only if its filter is then ready; an activated
/// registration waits in its kqueue's `KqueueState.Active` queue until a wait
/// reports it. A wait reads each filter again as it reaches it.
[<RequireQualifiedAccess>]
module KqueueQueue =

    /// The kqueue state of the open file description `kqueue`. Loudly partial:
    /// every caller has just resolved it as a live kqueue.
    let private stateOf<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (operation : string)
        (kqueue : OpenFileDescriptionId)
        (system : UnixSystem<'Task, 'Handler>)
        : KqueueState
        =
        match OpenFileTable.tryFind kqueue system.Machine.OpenFiles with
        | Some {
                   Target = OpenFileTarget.Kqueue state
               } -> state
        | other ->
            failwith
                $"KqueueQueue.%s{operation}: %O{kqueue} names %A{other} rather than a live kqueue (this is a bug in the caller, which resolved it as one)."

    let private withState<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (kqueue : OpenFileDescriptionId)
        (state : KqueueState)
        (system : UnixSystem<'Task, 'Handler>)
        : UnixSystem<'Task, 'Handler>
        =
        UnixSystemState.mapOpenFiles (OpenFileTable.setKqueueState kqueue state) system

    /// What the registration `registration`, of the filter `filter`, would
    /// report were a wait to reach it now: read off the socket it is attached
    /// to (`KqueueRegistration.Socket`), so from any process's view.
    let private reportOf
        (filter : KqueueFilter)
        (registration : KqueueRegistration)
        (machine : UnixMachineState)
        : KqueueFilterReport option
        =
        DarwinReadiness.ofSocket filter registration.Socket machine

    /// The registration `key` of the kqueue `kqueue`, whose state is `state`.
    /// Loudly partial: every caller read `key` from the kqueue's registrations
    /// or from its queue, which is a subset of them.
    let private registrationOf
        (kqueue : OpenFileDescriptionId)
        (state : KqueueState)
        (key : int * KqueueFilter)
        : KqueueRegistration
        =
        match Map.tryFind key state.Registrations with
        | Some registration -> registration
        | None ->
            failwith
                $"KqueueQueue: kqueue %O{kqueue} queues %A{key}, which it does not register. FileDescriptorRegistryDefect.KqueueActiveEntryUnregistered exists to make this unreachable, so the system breaks UnixSystem.checkInvariants (this is a bug in this library, or in a caller that assembled the state by hand)."

    /// Activate the registration `key` of the kqueue `kqueue`: queue it at the
    /// tail if its filter is ready now and it is not queued already. What an
    /// `EV_ADD` does, of a new registration and of an existing one alike.
    let activateRegistration<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (kqueue : OpenFileDescriptionId)
        (key : int * KqueueFilter)
        (system : UnixSystem<'Task, 'Handler>)
        : UnixSystem<'Task, 'Handler>
        =
        let state = stateOf "activateRegistration" kqueue system

        let registration =
            match Map.tryFind key state.Registrations with
            | Some registration -> registration
            | None ->
                failwith
                    $"KqueueQueue.activateRegistration: kqueue %O{kqueue} does not register %A{key} (this is a bug in the caller, which has just added it)."

        if
            List.contains key state.Active
            || Option.isNone (reportOf (snd key) registration system.Machine)
        then
            system
        else
            withState
                kqueue
                { state with
                    Active = state.Active @ [ key ]
                }
                system

    /// The registrations attached to the socket `socketId` among
    /// `registrations` (each with the socket it is attached to and the ordinal
    /// it was first made at) that an event waking each filter of `filters`, in
    /// that order, activates: each whose filter is ready now and which is not
    /// in `active` already. Of several registrations of the socket for one
    /// filter, made through different descriptors onto it, the
    /// newest-registered comes first.
    let private entering
        (socketId : SocketId)
        (filters : KqueueFilter list)
        (registrations : ((int * KqueueFilter) * SocketId option * int64) list)
        (active : (int * KqueueFilter) list)
        (machine : UnixMachineState)
        : (int * KqueueFilter) list
        =
        // Measured on Darwin 27.0.0 (`kevent-register.c`): one event activates
        // WRITE before READ whatever order they were registered in (O2), and a
        // socket's registrations through a descriptor and its dup newest first
        // (O6).
        filters
        |> List.collect (fun filter ->
            registrations
            |> List.filter (fun ((_, registered), socket, _) -> registered = filter && socket = Some socketId)
            |> List.sortByDescending (fun (_, _, registeredAt) -> registeredAt)
            |> List.map (fun (key, _, _) -> key)
        )
        |> List.filter (fun (_, filter as key) ->
            not (List.contains key active)
            && Option.isSome (DarwinReadiness.ofSocket filter socketId machine)
        )

    /// Something happened to the socket `socketId` that wakes each filter of
    /// `filters`, in that order: in every kqueue on the machine, and in the
    /// kqueue of every Darwin `poll` asleep on it (`PollQueue`), whichever
    /// process each belongs to, queue each registration attached to the socket
    /// for that filter whose filter is ready now and which is not queued
    /// already. Of several registrations of the socket for one filter, made
    /// through different descriptors onto it, the newest-registered is queued
    /// first.
    ///
    /// A registration is reached through the socket it is attached to
    /// (`KqueueRegistration.Socket`, `PollRegistration.Socket`), as XNU reaches
    /// a knote through the socket's own list, so no descriptor table is read:
    /// an event one process's call causes activates another's registrations
    /// as its own.
    let activate (socketId : SocketId) (filters : KqueueFilter list) (machine : UnixMachineState) : UnixMachineState =
        let openFiles =
            (machine.OpenFiles, OpenFileTable.toSeq machine.OpenFiles |> Seq.toList)
            ||> List.fold (fun openFiles (kqueue, description) ->
                match description.Target with
                | OpenFileTarget.Kqueue state ->
                    let registrations =
                        state.Registrations
                        |> Map.toList
                        |> List.map (fun (key, registration) ->
                            key, Some registration.Socket, registration.RegisteredAt
                        )

                    match entering socketId filters registrations state.Active machine with
                    | [] -> openFiles
                    | entering ->
                        OpenFileTable.setKqueueState
                            kqueue
                            { state with
                                Active = state.Active @ entering
                            }
                            openFiles
                | OpenFileTarget.Epoll _
                | OpenFileTarget.File _
                | OpenFileTarget.Directory _
                | OpenFileTarget.Socket _
                | OpenFileTarget.CharacterDevice _
                | OpenFileTarget.Pipe _ -> openFiles
            )

        let pollQueues =
            machine.PollQueues
            |> Map.map (fun _ queue ->
                let registrations =
                    queue.Registrations
                    |> Map.toList
                    |> List.map (fun (key, registration) -> key, registration.Socket, int64 registration.RegisteredAt)

                match entering socketId filters registrations queue.Active machine with
                | [] -> queue
                | entering ->
                    { queue with
                        Active = queue.Active @ entering
                    }
            )

        { machine with
            OpenFiles = openFiles
            PollQueues = pollQueues
        }

    /// Whether a wait on the kqueue `kqueue` would report at least one event
    /// right now: the question a task waiting in `kevent` on it is polled
    /// against, and the one `drain` answers by reporting.
    ///
    /// Loudly partial in `kqueue`, as `EpollReadyList.hasDeliverableEvent` is
    /// in its epoll instance: a task parked on a kqueue holds it until its call returns,
    /// so the kqueue cannot have gone while something waits on it.
    let hasDeliverableEvent<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (kqueue : OpenFileDescriptionId)
        (system : UnixSystem<'Task, 'Handler>)
        : bool
        =
        match OpenFileTable.tryFind kqueue system.Machine.OpenFiles with
        | None ->
            failwith
                $"KqueueQueue.hasDeliverableEvent: %O{kqueue} names no live open file description, but a task waits on it, and a park holds what it waits on until the call returns (this is a bug in this library, or in a caller that ended a park without its finishing call or assembled the state by hand)."
        | Some {
                   Target = OpenFileTarget.Kqueue state
               } ->
            state.Active
            |> List.exists (fun (_, filter as key) ->
                Option.isSome (reportOf filter (registrationOf kqueue state key) system.Machine)
            )
        | Some other ->
            failwith
                $"KqueueQueue.hasDeliverableEvent: %O{kqueue} names %A{other.Target} rather than a kqueue, so no wait can be parked on it (this is a bug in the caller)."

    /// Report up to `maxCount` events from the kqueue `kqueue`, as one wait
    /// does: walk the queue in order, reading each registration's filter
    /// again. One no longer ready leaves the queue and reports nothing. One
    /// that reports leaves the queue if it was added with `EV_CLEAR`, and
    /// otherwise goes back to the tail, behind the entries the walk did not
    /// reach. The walk stops once `maxCount` events are reported, and the
    /// entries it did not reach stay queued in order.
    ///
    /// Returns the reports in order, and the system with the queue as the walk
    /// left it. Loudly partial: `kqueue` must be a live kqueue, and `maxCount`
    /// positive.
    let drain<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (kqueue : OpenFileDescriptionId)
        (maxCount : int)
        (system : UnixSystem<'Task, 'Handler>)
        : KqueueReport list * UnixSystem<'Task, 'Handler>
        =
        if maxCount <= 0 then
            failwith
                $"KqueueQueue.drain: maxCount %d{maxCount} is not positive; kevent returns at once before reaching the queue for it, so this is a bug in the caller."

        let state = stateOf "drain" kqueue system

        // Measured on Darwin 27.0.0 (`kevent-register.c`): stale entries are
        // dropped (P1.8, P4.7); a level registration goes back behind what the
        // walk did not reach (O4, O5); room for fewer stops the walk (R14, O5).
        let rec walk
            (reported : KqueueReport list)
            (requeued : (int * KqueueFilter) list)
            (remaining : (int * KqueueFilter) list)
            : KqueueReport list * (int * KqueueFilter) list
            =
            match remaining with
            | _ when List.length reported = maxCount -> List.rev reported, remaining @ List.rev requeued
            | [] -> List.rev reported, List.rev requeued
            | (fd, filter as key) :: rest ->
                let registration = registrationOf kqueue state key

                match reportOf filter registration system.Machine with
                | None -> walk reported requeued rest
                | Some report ->
                    let reported =
                        {
                            Fd = fd
                            Filter = filter
                            Registration = registration
                            Report = report
                        }
                        :: reported

                    if registration.Clear then
                        walk reported requeued rest
                    else
                        walk reported (key :: requeued) rest

        let reported, active = walk [] [] state.Active

        reported,
        withState
            kqueue
            { state with
                Active = active
            }
            system

/// Something that happened to a socket, which a task waiting in `epoll_wait`
/// or `kevent` may be waiting for.
///
/// The producers are a measured set, not "anything that writes the socket
/// table": a datagram re-target or dissolve, `bind(2)`, an `accept(2)`, an
/// `SO_ERROR` read, and the completion-reporting connect signal nothing at all
/// (`order3.c` rows N, O, P on Linux; `kevent-register.c` sections P and X on
/// Darwin).
[<RequireQualifiedAccess>]
type SocketWake =
    /// A completed connection joined the listening socket's accept queue.
    | AcceptQueuePush
    /// A connect on the socket resolved: it completed, or it was refused.
    | ConnectResolved
    /// Linux's connect delivered a pending refusal and reset the socket to
    /// idle. Darwin never does.
    | RefusalReset
    /// The other end of the socket's connection closed, which delivers its FIN.
    | PeerFin

[<RequireQualifiedAccess>]
module SocketWake =

    /// The key an epoll wake for `wake` carries, in Linux's `<sys/epoll.h>`
    /// numbering, or `None` for an unkeyed wake (see
    /// `OpenFileTable.signalEpollInstances`).
    let epollKey (wake : SocketWake) : uint32 option =
        match wake with
        // A data-ready wake, keyed with what `sock_def_readable` passes its
        // waiters, so a registration whose stored mask misses all four is
        // never queued (measured, `order6.c`), and one asking only for
        // `EPOLLPRI` or `EPOLLRDBAND` is queued although a listener never
        // reports either (measured, the WAKE section of `epoll-ctl.c`).
        | SocketWake.AcceptQueuePush ->
            Some (EpollEvents.In ||| EpollEvents.Pri ||| EpollEvents.RdNorm ||| EpollEvents.RdBand)
        // State changes, which queue every registration regardless of interest:
        // the entry keeps the wake's position through a later interest change,
        // and delivery's re-poll does the filtering (measured, `order8.c`,
        // `order9.c`).
        | SocketWake.ConnectResolved
        | SocketWake.RefusalReset
        | SocketWake.PeerFin -> None

    /// The kqueue filters `wake` activates, in the order it activates them.
    let kqueueFilters (wake : SocketWake) : KqueueFilter list =
        // Measured on Darwin 27.0.0 (`kevent-register.c`, section P): a queued
        // connection activates the listener's READ (P1); a connect completing
        // its WRITE (P2), and a refusal its WRITE and then its READ (P3, O2);
        // the peer's FIN its READ alone, though its WRITE is ready too (P5,
        // P6). A completing connect's READ is never ready, so whether it is
        // activated is not observable.
        match wake with
        | SocketWake.AcceptQueuePush -> [ KqueueFilter.Read ]
        | SocketWake.ConnectResolved -> [ KqueueFilter.Write ; KqueueFilter.Read ]
        | SocketWake.PeerFin -> [ KqueueFilter.Read ]
        // Only Linux resets a refused socket, and Linux has no kqueue.
        | SocketWake.RefusalReset -> []

    /// `wake` happened to the socket `socketId`: signal every epoll
    /// registration of it on the machine, and activate every kqueue
    /// registration attached to it, including those of every Darwin `poll`
    /// asleep (see `KqueueQueue.activate`), whichever process made each.
    ///
    /// An epoll registration is reached through the description it names,
    /// as Linux reaches an epitem through the file's wait queue, and a kqueue
    /// registration through the socket it is attached to, as XNU reaches a
    /// knote through the socket's own list; so no descriptor table is read.
    ///
    /// Called with the socket already in the state the event left it in,
    /// since a kqueue registration is activated only if its filter is then
    /// ready.
    let signal<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (socketId : SocketId)
        (wake : SocketWake)
        (system : UnixSystem<'Task, 'Handler>)
        : UnixSystem<'Task, 'Handler>
        =
        let system =
            UnixSystemState.mapOpenFiles
                (OpenFileTable.signalEpollInstances
                    (UnixMachineState.descriptionsNamingSocket socketId system.Machine)
                    (epollKey wake))
                system

        { system with
            Machine = KqueueQueue.activate socketId (kqueueFilters wake) system.Machine
        }

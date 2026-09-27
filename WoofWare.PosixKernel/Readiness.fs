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
/// answers, the launch shape's standard streams, both ends of a pipe, and
/// regular files and directories (which epoll will not register, but `poll`
/// answers). A socket
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
        | OpenFileTarget.Directory _ ->
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
            ||| (if UnixProcessState.pipeEndOpen pipeId PipeEnd.Write system.Process then
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
            ||| (if UnixProcessState.pipeEndOpen pipeId PipeEnd.Read system.Process then
                     0u
                 else
                     EpollEvents.Err)
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
        | OpenFileTarget.Directory _
        | OpenFileTarget.Socket _
        | OpenFileTarget.Pipe _ ->
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
        | OpenFileTarget.Directory _
        | OpenFileTarget.Socket _
        | OpenFileTarget.Pipe _ ->
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

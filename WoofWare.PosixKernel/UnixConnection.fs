namespace WoofWare.PosixKernel

open System.Collections.Immutable

/// One `connect(2)` call's answer: it completed, or it failed with the errno
/// the syscall left. EINPROGRESS is a `Failed` like any other -- a caller
/// reports it as it reports any other errno -- and the outcome it defers is
/// already latched on the socket's phase.
[<RequireQualifiedAccess>]
type ConnectOutcome =
    | Completed
    | Failed of UnixError

/// What became of an `accept(2)` this kernel could answer.
[<RequireQualifiedAccess>]
type AcceptOutcome =
    /// The call failed with this errno, and nothing about the listener changed.
    /// The accept queue in particular is untouched: measured on both flavours,
    /// a failed `accept` leaves a queued connection queued.
    ///
    /// Answered by `finishAccept`, the task is no longer parked in the system
    /// this rides with.
    | Failed of error : UnixError
    /// A connection was dequeued and a socket materialised onto it. `fd` is the
    /// descriptor that socket is open on.
    ///
    /// The kernel wrote `copiedOut` to the start of the caller's address buffer
    /// and `reportedLength` to its length cell: the client's address as this
    /// platform's `struct sockaddr_in`, cut to the length the caller declared
    /// and so possibly empty, and that structure's untruncated size. As for
    /// `getsockname`, the declared length bounds what is written and not what
    /// is reported: a call declaring 8 writes eight bytes and still reports 16.
    | Accepted of fd : int * copiedOut : ImmutableArray<byte> * reportedLength : int
    /// The call failed with this errno after it had taken the oldest connection
    /// off the accept queue, and the connection is gone: its server end is
    /// closed as `close` closes an accepted socket, so the client sees an
    /// orderly shutdown and its registrations are signalled. No descriptor is
    /// allocated. The system that rides with this records all of that.
    ///
    /// Linux's answer, `EINVAL`, for a negative declared length, which it reads
    /// only once it holds the connection.
    | DroppedConnection of error : UnixError
    /// The call did not return: the listener is blocking and its accept queue
    /// is empty. The calling task is parked, and sleeps until
    /// `WakeCondition.satisfied` of this condition is non-empty and
    /// `UnixWait.wakes` wakes it; then `UnixConnection.finishAccept` finishes
    /// the call.
    | WouldBlock of WakeCondition
    /// The call was asleep, a signal with a handler interrupted it, and the call
    /// restarts (`SyscallInterruption.Restart`): it never returns. The task is
    /// no longer parked. Once the handlers have run, the client issues the
    /// `accept` again with the arguments it was first made with.
    ///
    /// Only `finishAccept` answers this.
    | Restarts

/// Why this kernel will not answer an `accept`.
///
/// Distinct from an errno: an errno is an answer, and these are the inputs for
/// which this library has measured what real kernels do and found no single
/// answer to give.
[<RequireQualifiedAccess>]
type AcceptRefusal =
    /// A descriptor the call would make lies at or above the bound this kernel
    /// assumes the process's `RLIMIT_NOFILE` reaches.
    | DescriptorLimit of DescriptorLimitRefusal
    /// The descriptor is a socket in a domain whose addresses this kernel does
    /// not model, so there is no peer address to report even if the accept
    /// itself would succeed.
    | UnmodelledDomain of socket : SocketId * domain : SocketDomain
    /// The descriptor is a socket of a kind whose `accept(2)` answer is
    /// unmeasured. `SOCK_SEQPACKET` does accept connections, so answering
    /// rather than measuring would be the difference between an answer and a
    /// state change.
    | UnmeasuredKind of socket : SocketId * kind : SocketKind
    /// The accept would succeed and copy the peer address out, but the
    /// destination is one this library has no answer for: its bytes cannot be
    /// produced, or it is not an address at all.
    ///
    /// Reached only once a connection has been selected, which is what makes it
    /// worth distinguishing from `UnmeasuredCopyOutFault` beside it: here the
    /// kernel *would* have succeeded and dequeued, and it is the client that
    /// cannot represent the transfer.
    | Buffer of BufferRefusal
    /// The accept would succeed and copy the peer address out, but the
    /// destination is unmapped, so the copy faults.
    ///
    /// `getsockname` answers EFAULT for this and `accept` cannot, which is the
    /// whole reason the case exists: by the time the fault happens a connection
    /// has been taken off the queue. Measured, Linux stores the untruncated
    /// length in the caller's cell, answers EFAULT and loses the connection,
    /// while Darwin ignores the fault and succeeds; neither is an outcome this
    /// library can return, since neither a failure that writes the cell nor a
    /// success whose address the caller must not write is one. A NULL
    /// destination (`Unmapped 0UL`) is refused here too, although no kernel
    /// faults on it: both skip the copy and the length cell, which this
    /// library has no outcome for either.
    | UnmeasuredCopyOutFault of listener : SocketId
    /// The accept was asleep and a signal is pending for the task, and this
    /// library will not say how the signal ends it.
    | Interruption of SyscallInterruptionRefusal
    /// The accept slept on a listener whose last descriptor closed meanwhile,
    /// so the listener goes as the call returns, and what that does to what is
    /// left in its queue is unmeasured.
    | Release of DescriptionReleaseRefusal
    /// The accept would sleep on a listener a close has drained
    /// (`ListenState.Drained`): its queue is empty and the description it was
    /// made through blocking.
    ///
    /// Darwin answers such a sleep with `ECONNABORTED` once anything wakes it,
    /// and a connection wakes one sleeper and stays queued, so a second sleeper
    /// sleeps on through it. This kernel wakes a sleeping accept while a
    /// connection is queued, which would wake that second sleeper too.
    | DarwinDrainedListener of listener : SocketId

[<RequireQualifiedAccess>]
module AcceptRefusal =
    /// What this kernel knows about why it cannot answer. The client supplies
    /// its own half -- which entry point, which descriptor, and how a caller
    /// could have come by such a socket or such a buffer.
    let describe (refusal : AcceptRefusal) : string =
        match refusal with
        | AcceptRefusal.DescriptorLimit refusal -> DescriptorLimitRefusal.describe refusal
        | AcceptRefusal.UnmodelledDomain (socket, domain) ->
            $"the descriptor is socket %O{socket}, whose domain is %O{domain}. This kernel models a peer address only for IPv4: an IPv6 socket's is sixteen bytes of address plus a scope id, and a Unix-domain socket's is a *path* in the filesystem rather than a transport endpoint. Neither is a wider version of what is modelled here, so there is nothing to truncate or widen into an answer."
        | AcceptRefusal.UnmeasuredKind (socket, kind) ->
            $"the descriptor is socket %O{socket}, which is a %O{kind} socket, and what `accept(2)` answers for one is unmeasured. Measure it rather than guessing: SOCK_SEQPACKET does accept connections, so a guess of EOPNOTSUPP there would be a wrong answer rather than an approximate one."
        | AcceptRefusal.Buffer refusal -> BufferRefusal.describe refusal
        | AcceptRefusal.UnmeasuredCopyOutFault listener ->
            $"socket %O{listener} has a connection to hand over, so this call takes it off the queue and copies the peer address out -- but the destination is unmapped, so that copy faults. Measured, Linux stores the untruncated length in the caller's length cell, answers EFAULT and loses the connection, while Darwin ignores the fault and succeeds; this kernel's accept has no outcome for either. (A NULL destination is not copied to at all, and neither is the length cell; that has no outcome here either.)"
        | AcceptRefusal.Interruption refusal -> SyscallInterruptionRefusal.describe refusal
        | AcceptRefusal.Release refusal ->
            $"the accept slept on a listener no descriptor names any more, which goes as the call returns: %s{DescriptionReleaseRefusal.describe refusal}"
        | AcceptRefusal.DarwinDrainedListener listener ->
            $"socket %O{listener} is a listener on which a close of the descriptor an accept was asleep through has ended every accept, and this accept would sleep on it. Measured on Darwin (close-ends-call.c section A7), such a sleep answers ECONNABORTED as soon as anything wakes it, a connection or a signal, and one connection wakes one such sleeper, the connection staying queued, so a second sleeper sleeps on through it. This kernel wakes a sleeping accept for as long as a connection is queued, so it would wake every such sleeper for one connection."

/// Why this kernel will not answer a `connect(2)` at all: the call reached an
/// input whose real answer is unmeasured, or a state this library does not
/// model. The client decides what a refusal means for it; nothing here is
/// recoverable by retrying the same call.
[<RequireQualifiedAccess>]
type ConnectRefusal =
    /// The sockaddr copy could not be admitted.
    | Copy of SockaddrCopyRefusal
    /// A seqpacket socket, whose `connect(2)` is unmeasured.
    | UnmeasuredKind of socket : SocketId * kind : SocketKind
    /// The socket has no concrete source address and the destination is not
    /// loopback, and which source a kernel resolves for it is unmeasured.
    /// `boundToWildcard` says whether the socket was bound to the wildcard or
    /// not bound at all.
    | SourceForNonLoopbackDestination of socket : SocketId * destination : InternetEndpoint * boundToWildcard : bool
    /// The implicit bind found every port in the ephemeral range taken, and
    /// what a kernel answers then is unmeasured.
    | EphemeralPortsExhausted of range : uint16 * uint16
    /// The destination is not an address of this machine, and this library
    /// models no network to carry a packet anywhere else.
    | DestinationNotLocal of destination : InternetEndpoint * kind : SocketKind
    /// The listener's accept queue is at its measured capacity: a real kernel
    /// leaves the SYN unanswered and the client retries on a timer.
    | AcceptQueueFull of listener : SocketId * destination : InternetEndpoint * queued : int
    /// The resolved source equals the destination while a listener matched,
    /// which only a reuse-bound client can engineer; unmeasured.
    | SelfTuple of endpoint : InternetEndpoint
    /// A connection between this source and destination already exists, or,
    /// under Darwin, another datagram socket is connected from this source to
    /// this destination, and how a kernel refuses the duplicate four-tuple is
    /// unmeasured.
    | DuplicateFourTuple of source : InternetEndpoint * destination : InternetEndpoint
    /// The destination is the socket's own bound address with nothing
    /// listening: TCP simultaneous open, which is unmodelled.
    | SimultaneousOpen of destination : InternetEndpoint
    /// Darwin drops the SYN to a bound but unlistened port, and the connect
    /// pends on a retransmission schedule this library cannot honour.
    | DarwinSynDropped of destination : InternetEndpoint
    /// `AF_UNSPEC` on a Linux stream socket in a phase other than idle, whose
    /// `tcp_disconnect` consequences are unmeasured.
    | LinuxUnspecOnPhase of socket : SocketId * phase : SocketPhase
    /// A datagram connect to the broadcast address or a multicast group that
    /// would succeed. This library models no group membership and no
    /// interface to broadcast on, so it does not record such a peer.
    | DatagramGroupDestination of socket : SocketId * destination : InternetEndpoint

[<RequireQualifiedAccess>]
module ConnectRefusal =
    /// What this kernel knows about why it cannot answer. The client supplies
    /// its own half -- which entry point, and which caller could have asked.
    let describe (refusal : ConnectRefusal) : string =
        match refusal with
        | ConnectRefusal.Copy refusal -> SockaddrCopyRefusal.describe refusal
        | ConnectRefusal.UnmeasuredKind (socket, kind) ->
            $"socket %O{socket} is a %O{kind} socket, and what connect(2) does for one is unmeasured, so measure it rather than guessing."
        | ConnectRefusal.SourceForNonLoopbackDestination (socket, destination, boundToWildcard) ->
            if boundToWildcard then
                $"socket %O{socket} is bound to the wildcard and is connecting to %s{InternetEndpoint.toString destination}, and which source address a kernel resolves the wildcard to for a destination other than 127.0.0.1 is unmeasured. Bind to a concrete address first, or connect to 127.0.0.1."
            else
                $"socket %O{socket} is unbound and is connecting to %s{InternetEndpoint.toString destination}, and which source address a kernel picks for a destination other than 127.0.0.1 is unmeasured. Bind the socket first, or connect to 127.0.0.1."
        | ConnectRefusal.EphemeralPortsExhausted (low, high) ->
            $"every port in the ephemeral range %d{low}-%d{high} is taken, so this implicit bind has no answer. Widen the machine's EphemeralPortRange, or measure what a real kernel says here."
        | ConnectRefusal.DestinationNotLocal (destination, kind) ->
            let carried =
                match kind with
                | SocketKind.Stream -> "a SYN"
                | SocketKind.Datagram -> "a datagram"
                | SocketKind.SeqPacket -> "a packet"

            $"destination %s{InternetEndpoint.toString destination} is not a local address of this simulated machine, and this library models no network to carry %s{carried} anywhere else. Add the address to the kernel's LocalAddresses/LocalRoutes if it should be local, or connect to loopback."
        | ConnectRefusal.AcceptQueueFull (listener, destination, queued) ->
            $"the accept queue of listener %O{listener} at %s{InternetEndpoint.toString destination} already holds %d{queued} connections, its measured capacity. A real kernel leaves this SYN unanswered and the client retries on a timer -- timing this library cannot honour deterministically -- so this connect has no faithful answer. Accept from the listener before connecting again, or listen with a larger backlog."
        | ConnectRefusal.SelfTuple endpoint ->
            $"the resolved source %s{InternetEndpoint.toString endpoint} equals the destination, with a listener present. What a real kernel does with this self-tuple (plausibly EINVAL on Darwin, a completed self-connect on Linux) is unmeasured, so measure it rather than guessing."
        | ConnectRefusal.DuplicateFourTuple (source, destination) ->
            $"a connection from %s{InternetEndpoint.toString source} to %s{InternetEndpoint.toString destination} already exists, and a real kernel refuses a duplicate four-tuple in ways that are unmeasured (plausibly EADDRINUSE at connect time). Measure it rather than guessing."
        | ConnectRefusal.SimultaneousOpen destination ->
            $"destination %s{InternetEndpoint.toString destination} is this socket's own bound address and nothing is listening there. A real kernel can complete this as a TCP simultaneous open -- connecting the socket to itself -- which this library does not model."
        | ConnectRefusal.DarwinSynDropped destination ->
            $"destination %s{InternetEndpoint.toString destination} is bound but nothing is listening there, and Darwin *drops* such a SYN rather than answering RST: the connect pends on the client's retransmission schedule (a blocking one was measured to stall into ETIMEDOUT), which this library cannot honour deterministically. Listen on the destination socket, or connect to a fully closed port."
        | ConnectRefusal.LinuxUnspecOnPhase (socket, phase) ->
            $"AF_UNSPEC on stream socket %O{socket} in %A{phase} under Linux runs tcp_disconnect, whose consequences for this phase (a connected socket's peer, a listener's queue) are unmeasured and unmodelled."
        | ConnectRefusal.DatagramGroupDestination (socket, destination) ->
            $"datagram socket %O{socket} would connect to %s{InternetEndpoint.toString destination}, a broadcast or multicast destination. This kernel models no group membership and no interface to broadcast on, so it does not record such a peer. Model multicast and broadcast before connecting to either."

[<RequireQualifiedAccess>]
module UnixConnection =

    /// `connect(2)` past the descriptor screens and the copy-in: the
    /// per-flavour ladder over the socket's phase, the declared length, the
    /// sockaddr's family, and the destination, for the sockaddr `copied`
    /// decodes to.
    ///
    /// Every answered row is measured (`connect_probe.c` and successors,
    /// 2026-08-21; docs/plans/2026-08-21-socket-connect.md holds the table);
    /// a `ConnectRefusal` names an unmeasured or unmodellable input, and a
    /// throw is a bug in this library or in the caller's state construction.
    let internal connectDecoded<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (socketId : SocketId)
        (nonBlocking : bool)
        (declaredLength : uint32)
        (copied : CopiedInternetSockaddr)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<ConnectOutcome * UnixSystem<'Task, 'Handler>, ConnectRefusal>
        =
        let family = copied.Family
        let destination = copied.Endpoint
        let sock = UnixMachineState.socket socketId system.Machine
        let platform = system.Machine.UnixPlatform
        let flavour = SimulatedUnixPlatform.flavour platform
        let exactSize = SimulatedUnixPlatform.internetSocketAddressSize

        // connect(2) copies the sockaddr in through the same helpers bind(2)
        // uses (Linux's move_addr_to_kernel, Darwin's getsockaddr), and the
        // measured lengths agree with bind's rule exactly: Linux takes 16
        // through 128 and answers EINVAL outside, Darwin takes exactly 16,
        // EINVAL otherwise and ENAMETOOLONG past 255. So the verdict function
        // is shared.
        let lengthVerdict =
            SimulatedUnixPlatform.bindAddressLength platform exactSize declaredLength

        let declaredLength = int declaredLength

        let fail (error : UnixError) : Result<ConnectOutcome * UnixSystem<'Task, 'Handler>, ConnectRefusal> =
            Ok (ConnectOutcome.Failed error, system)

        let failed
            (error : UnixError)
            (system : UnixSystem<'Task, 'Handler>)
            : Result<ConnectOutcome * UnixSystem<'Task, 'Handler>, ConnectRefusal>
            =
            Ok (ConnectOutcome.Failed error, system)

        let completed
            (system : UnixSystem<'Task, 'Handler>)
            : Result<ConnectOutcome * UnixSystem<'Task, 'Handler>, ConnectRefusal>
            =
            Ok (ConnectOutcome.Completed, system)

        let withPhase (phase : SocketPhase) (system : UnixSystem<'Task, 'Handler>) : UnixSystem<'Task, 'Handler> =
            { system with
                Machine =
                    { system.Machine with
                        Sockets =
                            Map.add
                                socketId
                                { sock with
                                    Phase = phase
                                }
                                system.Machine.Sockets
                    }
            }

        // A wildcard destination: Linux aims at the socket's own local
        // address when it has a concrete one, and at loopback otherwise; Darwin
        // aims at loopback. Measured for stream and datagram sockets alike
        // (`sockaddr-dgram-reconnect.c`, and `sockaddr-dgram-connect.c`, D, for
        // a socket with no concrete address).
        let resolveWildcard (dest : InternetEndpoint) : InternetEndpoint =
            if dest.Address <> InternetEndpoint.WildcardAddress then
                dest
            else

            let address =
                match flavour, sock.Binding with
                | SimulatedUnixFlavour.Linux, Some binding when
                    binding.Endpoint.Address <> InternetEndpoint.WildcardAddress
                    ->
                    binding.Endpoint.Address
                | SimulatedUnixFlavour.Linux, _
                | SimulatedUnixFlavour.Darwin, _ -> InternetEndpoint.LoopbackAddress

            { dest with
                Address = address
            }

        let destinationIsLocal (address : uint32) : bool =
            List.contains address system.Machine.LocalAddresses
            || system.Machine.LocalRoutes |> List.exists (Ipv4Prefix.contains address)

        // What a refusal delivery leaves in the socket's binding. Measured
        // for all three provenances (implicit, bind(2) to 127.0.0.1, bind(2)
        // to 0.0.0.0): Darwin keeps the resolved source; Linux's reset
        // reverts the address to whatever bind(2) locked — the wildcard when
        // the address only ever came from source resolution — while keeping
        // the port.
        let bindingAfterRefusalDelivery (flavour : SimulatedUnixFlavour) (binding : SocketBinding) : SocketBinding =
            match flavour with
            | SimulatedUnixFlavour.Darwin -> binding
            | SimulatedUnixFlavour.Linux ->
                { binding with
                    Endpoint =
                        { binding.Endpoint with
                            Address = binding.LockedAddress |> Option.defaultValue InternetEndpoint.WildcardAddress
                        }
                }

        // connect(2)'s implicit bind, when the socket has no local address
        // yet: loopback source, ephemeral port, the same conflict rule as
        // bind(2)'s own port-0 path. The source address for a non-loopback
        // destination is the route's preferred source, which is unmeasured,
        // so that input is refused. `current` is the binding the source is
        // resolved from, which is the socket's own unless the connect has
        // already reset it.
        let ensureBoundFrom
            (current : SocketBinding option)
            (dest : InternetEndpoint)
            (system : UnixSystem<'Task, 'Handler>)
            : Result<SocketBinding * UnixSystem<'Task, 'Handler>, ConnectRefusal>
            =
            match current with
            | Some binding when binding.Endpoint.Port = 0us ->
                // Half-bound: a datagram dissolve kept the locked address and
                // dropped the port. Linux's `inet_autobind` gives it a port and
                // keeps the address -- measured, `127.0.0.1:0` connects from
                // `127.0.0.1:<ephemeral>`. Only a concrete locked address is
                // ever left half-bound, so the wildcard case does not arise.
                let candidate (port : uint16) : SocketBinding =
                    { binding with
                        Endpoint =
                            { binding.Endpoint with
                                Port = port
                            }
                    }

                match
                    UnixMachineState.allocateEphemeralPort
                        (EphemeralPortUse.ConnectTo dest)
                        socketId
                        sock
                        candidate
                        system.Machine
                with
                | Some (binding, machine) ->
                    Ok (
                        binding,
                        { system with
                            Machine = machine
                        }
                    )
                | None -> Error (ConnectRefusal.EphemeralPortsExhausted system.Machine.EphemeralPortRange)
            | Some binding when binding.Endpoint.Address <> InternetEndpoint.WildcardAddress -> Ok (binding, system)
            | Some binding ->
                // A client bound to the wildcard gets a concrete source
                // address at connect — measured on both kernels, TCP and UDP
                // alike: the address becomes 127.0.0.1 for a loopback
                // destination and the port is kept, and getsockname reports
                // the rewrite afterwards, so the *binding* itself changes
                // rather than merely the connection's record of it. Which
                // source a kernel picks for any other destination is
                // unmeasured.
                if dest.Address <> InternetEndpoint.LoopbackAddress then
                    Error (ConnectRefusal.SourceForNonLoopbackDestination (socketId, dest, true))
                else

                Ok (
                    { binding with
                        Endpoint =
                            { binding.Endpoint with
                                Address = InternetEndpoint.LoopbackAddress
                            }
                    },
                    system
                )
            | None ->

            if dest.Address <> InternetEndpoint.LoopbackAddress then
                Error (ConnectRefusal.SourceForNonLoopbackDestination (socketId, dest, false))
            else

            let candidate (port : uint16) : SocketBinding =
                {
                    Endpoint = InternetEndpoint.ofParts InternetEndpoint.LoopbackAddress port
                    // No bind(2) ran: a Linux refusal delivery reverts the
                    // address all the way to the wildcard.
                    LockedAddress = None
                    LockedPort = false
                }

            match
                UnixMachineState.allocateEphemeralPort
                    (EphemeralPortUse.ConnectTo dest)
                    socketId
                    sock
                    candidate
                    system.Machine
            with
            | Some (binding, machine) ->
                Ok (
                    binding,
                    { system with
                        Machine = machine
                    }
                )
            | None -> Error (ConnectRefusal.EphemeralPortsExhausted system.Machine.EphemeralPortRange)


        let ensureBound
            (dest : InternetEndpoint)
            (system : UnixSystem<'Task, 'Handler>)
            : Result<SocketBinding * UnixSystem<'Task, 'Handler>, ConnectRefusal>
            =
            ensureBoundFrom sock.Binding dest system

        // The established/refused attempt, shared by both flavours once the
        // per-flavour screens have let an idle stream socket through.
        let attemptStream
            (dest : InternetEndpoint)
            : Result<ConnectOutcome * UnixSystem<'Task, 'Handler>, ConnectRefusal>
            =
            let dest = resolveWildcard dest

            if not (destinationIsLocal dest.Address) then
                Error (ConnectRefusal.DestinationNotLocal (dest, SocketKind.Stream))
            else

            let listeners =
                system.Machine.Sockets
                |> Map.toList
                |> List.choose (fun (otherId, other) ->
                    match other.Phase with
                    | SocketPhase.Listening listenState ->
                        match other.Binding with
                        | Some binding when
                            other.Kind = SocketKind.Stream
                            && binding.Endpoint.Port = dest.Port
                            && (binding.Endpoint.Address = dest.Address
                                || InternetEndpoint.isWildcard binding.Endpoint)
                            ->
                            Some (otherId, other, listenState, binding)
                        | _ -> None
                    | _ -> None
                )

            // A specific-address listener beats the wildcard — both kernels'
            // documented most-specific-match rule. The pair can only coexist
            // under SO_REUSEADDR, so the preference is the documented rule
            // rather than a measured one.
            let listener =
                match
                    listeners
                    |> List.tryFind (fun (_, _, _, binding) -> not (InternetEndpoint.isWildcard binding.Endpoint))
                with
                | Some found -> Some found
                | None -> List.tryHead listeners

            match listener with
            | Some (listenerId, listenerSocket, listenState, _) ->
                // Int64, so that the Linux `+ 1` cannot wrap when the
                // configured somaxconn is itself Int32.MaxValue.
                let capacity : int64 =
                    match flavour with
                    | SimulatedUnixFlavour.Linux ->
                        // Measured, with the sysctl set to 3 to bring the
                        // boundary in reach: listen(0) admits 1, listen(1)
                        // admits 2, listen(5) admits 6, and listen(-1) and
                        // listen(INT_MAX) both admit somaxconn + 1 — the
                        // kernel compares the backlog *unsigned* against
                        // somaxconn and clamps, and the queue then admits
                        // one more than the clamped value. The clamp also
                        // keeps the `+ 1` from overflowing on the
                        // Int32.MaxValue a parameterless Socket.Listen()
                        // passes.
                        let clamped =
                            if listenState.Backlog < 0 || listenState.Backlog > system.Machine.SoMaxConn then
                                system.Machine.SoMaxConn
                            else
                                listenState.Backlog

                        int64 clamped + 1L
                    | SimulatedUnixFlavour.Darwin ->
                        // Measured at the default sysctl of 128: listen(1)
                        // admits 1, listen(5) admits 5, and listen(0),
                        // listen(-1) and listen(INT_MAX) all admit exactly
                        // somaxconn — a non-positive or over-large backlog
                        // clamps to somaxconn, and the queue admits exactly
                        // the clamped value.
                        if listenState.Backlog <= 0 || listenState.Backlog > system.Machine.SoMaxConn then
                            int64 system.Machine.SoMaxConn
                        else
                            int64 listenState.Backlog

                if int64 (List.length listenState.Queue) >= capacity then
                    Error (ConnectRefusal.AcceptQueueFull (listenerId, dest, List.length listenState.Queue))
                else

                match ensureBound dest system with
                | Error refusal -> Error refusal
                | Ok (clientBinding, system) ->

                // Two corners a REUSEADDR-bound client can engineer, each
                // refused because the real answer is unmeasured.
                if clientBinding.Endpoint = dest then
                    // A wildcard listener at P beside a reuse-bound client at
                    // 127.0.0.1:P, connecting to 127.0.0.1:P: source equals
                    // destination even though a listener matched.
                    Error (ConnectRefusal.SelfTuple clientBinding.Endpoint)
                elif
                    system.Machine.Connections
                    |> Map.exists (fun _ connection ->
                        // In either orientation: a connection's endpoint
                        // pair occupies the tuple from both ends.
                        (connection.ClientAddress = clientBinding.Endpoint
                         && connection.ServerAddress = dest)
                        || (connection.ClientAddress = dest
                            && connection.ServerAddress = clientBinding.Endpoint)
                    )
                then
                    // Established tuples are unique in a real kernel; a second
                    // identical (source, destination) pair — two clients
                    // reuse-bound to one source endpoint, connecting to one
                    // listener — is refused there (plausibly EADDRINUSE),
                    // which is unmeasured.
                    Error (ConnectRefusal.DuplicateFourTuple (clientBinding.Endpoint, dest))
                else

                let connectionId = system.Machine.NextConnectionId
                let (ConnectionId rawConnectionId) = connectionId

                let tcpConnection =
                    {
                        ClientAddress = clientBinding.Endpoint
                        ServerAddress = dest
                    }

                let clientPhase =
                    if not nonBlocking then
                        SocketPhase.Established (connectionId, ConnectionEnd.Client)
                    else
                        match flavour with
                        | SimulatedUnixFlavour.Linux ->
                            // The next connect reports the completion with
                            // one SUCCESS (measured), which is what this
                            // phase defers.
                            SocketPhase.EstablishedPendingReport connectionId
                        | SimulatedUnixFlavour.Darwin ->
                            // Darwin's retry answers EISCONN directly
                            // (measured), so nothing is deferred.
                            SocketPhase.Established (connectionId, ConnectionEnd.Client)

                let system =
                    { system with
                        Machine =
                            { system.Machine with
                                Sockets =
                                    system.Machine.Sockets
                                    |> Map.add
                                        socketId
                                        { sock with
                                            Binding = Some clientBinding
                                            Phase = clientPhase
                                        }
                                    |> Map.add
                                        listenerId
                                        { listenerSocket with
                                            Phase =
                                                SocketPhase.Listening
                                                    { listenState with
                                                        // Oldest first: accept(2)
                                                        // dequeues the head.
                                                        Queue = listenState.Queue @ [ connectionId ]
                                                    }
                                        }
                                Connections = Map.add connectionId tcpConnection system.Machine.Connections
                                NextConnectionId = ConnectionId (rawConnectionId + 1L)
                            }
                    }

                // The two edges this call raises, in the measured order
                // (`order7.c`, three runs): the client's completion enters
                // the ready list *before* the listener's accept edge — the
                // client processes the SYN-ACK and becomes writable before
                // its final ACK puts the child on the accept queue. The
                // client's phase resolves in this call whether or not the
                // syscall's own answer is deferred to EINPROGRESS.
                let system =
                    system
                    |> SocketWake.signal socketId SocketWake.ConnectResolved
                    |> SocketWake.signal listenerId SocketWake.AcceptQueuePush

                if nonBlocking then
                    // The syscall itself still answers EINPROGRESS —
                    // measured on both kernels, even on loopback — and the
                    // completion is what the phase above latches.
                    failed UnixError.EINPROGRESS system
                else
                    completed system
            | None ->
                // The client's own endpoint with no listener behind it is
                // TCP simultaneous open: a real kernel can complete it,
                // connecting the socket to itself. Unmodelled.
                match sock.Binding with
                | Some binding when
                    binding.Endpoint.Port = dest.Port
                    && InternetEndpoint.addressesOverlap binding.Endpoint dest
                    ->
                    Error (ConnectRefusal.SimultaneousOpen dest)
                | _ ->

                match flavour with
                | SimulatedUnixFlavour.Darwin when
                    system.Machine.Sockets
                    |> Map.exists (fun otherId other ->
                        otherId <> socketId
                        && other.Kind = SocketKind.Stream
                        // Only a bound-but-unconnected socket makes Darwin
                        // drop the SYN. A port held by established ends
                        // (their pcbs are keyed by the full peer tuple) or
                        // by a refused socket answers RST — measured, both
                        // refuse like a closed port.
                        && (
                            match other.Phase with
                            | SocketPhase.Idle -> true
                            | _ -> false
                        )
                        && (
                            match other.Binding with
                            | Some binding ->
                                binding.Endpoint.Port = dest.Port
                                && InternetEndpoint.addressesOverlap binding.Endpoint dest
                            | None -> false
                        )
                    )
                    ->
                    Error (ConnectRefusal.DarwinSynDropped dest)
                | _ ->

                // The implicit bind happens before the SYN, so a refused
                // socket has a concrete local endpoint too — measured,
                // getsockname reports 127.0.0.1 and a nonzero port while the
                // refusal is pending, on both kernels.
                match ensureBound dest system with
                | Error refusal -> Error refusal
                | Ok (binding, system) ->

                if not nonBlocking then
                    // The refusal is delivered inline, and the socket's fate
                    // diverges by flavour: measured, a Linux retry is a fresh
                    // attempt and a Darwin one answers EISCONN forever.
                    let phase =
                        match flavour with
                        | SimulatedUnixFlavour.Linux -> SocketPhase.Idle
                        | SimulatedUnixFlavour.Darwin -> SocketPhase.Refused RefusalError.Reported

                    let system =
                        { system with
                            Machine =
                                { system.Machine with
                                    Sockets =
                                        Map.add
                                            socketId
                                            { sock with
                                                Binding = Some (bindingAfterRefusalDelivery flavour binding)
                                                Phase = phase
                                            }
                                            system.Machine.Sockets
                                }
                        }

                    // The error's arrival and its reset both signal
                    // (measured separately for the deferred path, `order3.c`
                    // row M); inline delivery collapses them into this one
                    // state change, so one signal carries both.
                    let system = SocketWake.signal socketId SocketWake.ConnectResolved system

                    failed UnixError.ECONNREFUSED system
                else
                    // EINPROGRESS now, with ECONNREFUSED pending. On Linux the
                    // first later connect delivers it, unless an SO_ERROR read
                    // has taken it first. Darwin's connect never delivers it
                    // (measured).
                    let system =
                        { system with
                            Machine =
                                { system.Machine with
                                    Sockets =
                                        Map.add
                                            socketId
                                            { sock with
                                                Binding = Some binding
                                                Phase = SocketPhase.Refused RefusalError.Pending
                                            }
                                            system.Machine.Sockets
                                }
                        }

                    // The error's arrival signals the client (measured,
                    // `order3.c` row M: the 0x201d edge).
                    let system = SocketWake.signal socketId SocketWake.ConnectResolved system

                    failed UnixError.EINPROGRESS system

        match sock.Kind with
        | SocketKind.SeqPacket -> Error (ConnectRefusal.UnmeasuredKind (socketId, sock.Kind))
        | SocketKind.Stream ->
            // The copy layer answers before any socket state on both
            // flavours: Linux's move_addr_to_kernel rejects an oversized
            // sockaddr and Darwin's getsockaddr rejects both bounds, each in
            // the syscall layer ahead of the protocol's own checks.
            match lengthVerdict with
            | BindLengthVerdict.RejectedBeforeCopy error -> fail error
            | BindLengthVerdict.Accepted
            | BindLengthVerdict.Invalid ->

            match family with
            | None ->
                // Too short to carry the family: EINVAL on both — Linux in
                // inet_stream_connect's first screen, Darwin in getsockaddr.
                fail UnixError.EINVAL
            | Some family ->

            match flavour with
            | SimulatedUnixFlavour.Linux ->
                // inet_stream_connect's order: the AF_UNSPEC branch, then
                // the state machine, then tcp_v4_connect's length and family
                // checks. Measured where a caller reaches it; the state arms'
                // precedence over the argument checks is the pinned source's.
                if family = 0 then
                    match sock.Phase with
                    | SocketPhase.Idle ->
                        // Measured: an accepted no-op, and the socket stays
                        // usable.
                        completed system
                    | phase -> Error (ConnectRefusal.LinuxUnspecOnPhase (socketId, phase))
                else

                match sock.Phase with
                | SocketPhase.EstablishedPendingReport connectionId ->
                    // The one completion-reporting SUCCESS (measured). The
                    // destination is ignored, as the state transition is.
                    completed (withPhase (SocketPhase.Established (connectionId, ConnectionEnd.Client)) system)
                | SocketPhase.Refused error ->
                    // Deliver the latched refusal once, then reset: the next
                    // connect is a fresh attempt, and the source address the
                    // pending attempt resolved reverts to whatever bind(2)
                    // locked (both measured). A refusal an `SO_ERROR` read has
                    // already taken resets the same way, answering
                    // ECONNABORTED for it (measured, wherever the connect is
                    // aimed).
                    let answer =
                        match error with
                        | RefusalError.Pending -> UnixError.ECONNREFUSED
                        | RefusalError.Reported -> UnixError.ECONNABORTED

                    let system =
                        { system with
                            Machine =
                                { system.Machine with
                                    Sockets =
                                        Map.add
                                            socketId
                                            { sock with
                                                Binding =
                                                    sock.Binding
                                                    |> Option.map (
                                                        bindingAfterRefusalDelivery SimulatedUnixFlavour.Linux
                                                    )
                                                Phase = SocketPhase.Idle
                                            }
                                            system.Machine.Sockets
                                }
                        }

                    // The reset signals: a registered client whose error edge
                    // was already consumed sees a fresh OUT|HUP edge after
                    // the delivering connect (measured, `order3.c` row M, and
                    // `consumed-epoll.c` R2 for the aborting one).
                    let system = SocketWake.signal socketId SocketWake.RefusalReset system

                    failed answer system
                | SocketPhase.Established _ -> fail UnixError.EISCONN
                | SocketPhase.Listening _ ->
                    // Measured: Linux answers a connect on the listening
                    // socket itself with EISCONN, where Darwin answers
                    // EOPNOTSUPP.
                    fail UnixError.EISCONN
                | SocketPhase.DatagramPeer _ ->
                    failwith
                        "UnixConnection.connectSocket: a stream socket holds SocketPhase.DatagramPeer. this kernel's socket invariants forbid that pairing, so this is a bug in the caller's state construction."
                | SocketPhase.Idle ->

                match lengthVerdict with
                | BindLengthVerdict.Invalid -> fail UnixError.EINVAL
                | BindLengthVerdict.RejectedBeforeCopy _
                | BindLengthVerdict.Accepted ->

                if family <> SimulatedUnixPlatform.internetAddressFamily then
                    fail UnixError.EAFNOSUPPORT
                else

                match destination with
                // Measured (`sockaddr-dgram-connect.c`, D and M): a broadcast
                // or multicast destination is ENETUNREACH, at every length
                // from 16 and whatever the port, before anything binds.
                | Some dest when SimulatedUnixPlatform.isBroadcastOrMulticast dest.Address -> fail UnixError.ENETUNREACH
                | Some dest -> attemptStream dest
                | None ->
                    failwith
                        "UnixConnection.connectSocket: the declared length passed the AF_INET verdict but the copy held no destination, though a length that passes it reaches `sin_addr` (this is a bug in this library)."
            | SimulatedUnixFlavour.Darwin ->
                // The state arms answer first — measured: a refused socket's
                // EISCONN beats a good destination, AF_UNSPEC and an
                // oversized sockaddr, and a connected socket's beats
                // AF_UNSPEC.
                match sock.Phase with
                | SocketPhase.EstablishedPendingReport _ ->
                    failwith
                        "UnixConnection.connectSocket: a stream socket is in SocketPhase.EstablishedPendingReport under the Darwin flavour, which never constructs it (its retry answers EISCONN directly). This is a bug in this library, or in a caller that assembled the state by hand."
                | SocketPhase.Refused _ ->
                    // Measured, whatever the destination, and whether or not
                    // the refusal is still pending: connect never delivers it.
                    fail UnixError.EISCONN
                | SocketPhase.Established _ ->
                    // Measured, including against an AF_UNSPEC destination.
                    fail UnixError.EISCONN
                | SocketPhase.Listening _ ->
                    // Measured: EOPNOTSUPP, where Linux answers EISCONN, for
                    // AF_INET and AF_UNSPEC alike (`sockaddr-connect-ladder.c`,
                    // U and Z).
                    fail UnixError.EOPNOTSUPP
                | SocketPhase.DatagramPeer _ ->
                    failwith
                        "UnixConnection.connectSocket: a stream socket holds SocketPhase.DatagramPeer. this kernel's socket invariants forbid that pairing, so this is a bug in the caller's state construction."
                | SocketPhase.Idle ->

                // Measured (`sockaddr-connect-ladder.c`, G and U): the family
                // is judged before the length, so any family but AF_UNSPEC and
                // AF_INET is EAFNOSUPPORT at every length the copy takes, and
                // binds nothing. AF_UNSPEC is then read exactly as AF_INET.
                if family <> 0 && family <> SimulatedUnixPlatform.internetAddressFamily then
                    fail UnixError.EAFNOSUPPORT
                else

                // A broadcast or multicast destination is EAFNOSUPPORT too,
                // judged with the family and binding nothing: for AF_INET
                // before the length, reading `sin_addr` with every byte past
                // the copy as zero, and for AF_UNSPEC only at 16. Measured
                // (`sockaddr-dgram-connect.c`, D and M), the rule
                // `SimulatedUnixPlatform.bindGroupAddressRule` states for a
                // stream socket's `bind(2)`.
                let groupDestination =
                    SimulatedUnixPlatform.isBroadcastOrMulticast copied.ZeroFilledAddress
                    && (family = SimulatedUnixPlatform.internetAddressFamily
                        || lengthVerdict = BindLengthVerdict.Accepted)

                if groupDestination then
                    fail UnixError.EAFNOSUPPORT
                else

                // From here a socket with no address is bound to the wildcard
                // and an ephemeral port before the length or the port is
                // judged, and a failure keeps that binding: measured on a
                // fresh socket, EINVAL at a length other than 16 and
                // EADDRNOTAVAIL for port 0 each leave `0.0.0.0:<ephemeral>`.
                // A connect that gets past both resolves its source as
                // `attemptStream` does.
                let failBound
                    (error : UnixError)
                    : Result<ConnectOutcome * UnixSystem<'Task, 'Handler>, ConnectRefusal>
                    =
                    match sock.Binding with
                    | Some _ -> fail error
                    | None ->
                        let candidate (port : uint16) : SocketBinding =
                            {
                                Endpoint = InternetEndpoint.ofParts InternetEndpoint.WildcardAddress port
                                // No bind(2) ran.
                                LockedAddress = None
                                LockedPort = false
                            }

                        match
                            UnixMachineState.allocateEphemeralPort
                                EphemeralPortUse.Reserve
                                socketId
                                sock
                                candidate
                                system.Machine
                        with
                        | None -> Error (ConnectRefusal.EphemeralPortsExhausted system.Machine.EphemeralPortRange)
                        | Some (binding, machine) ->
                            failed
                                error
                                { system with
                                    Machine =
                                        { machine with
                                            Sockets =
                                                Map.add
                                                    socketId
                                                    { sock with
                                                        Binding = Some binding
                                                    }
                                                    machine.Sockets
                                        }
                                }

                match lengthVerdict with
                | BindLengthVerdict.Invalid -> failBound UnixError.EINVAL
                | BindLengthVerdict.RejectedBeforeCopy _
                | BindLengthVerdict.Accepted ->

                match destination with
                // Measured (`sockaddr-connect-ladder.c`, Z): a port of 0 is
                // EADDRNOTAVAIL whatever the address, before the address is
                // looked up -- to 0.0.0.0, 127.0.0.1, 127.0.0.2 and 8.8.8.8
                // alike.
                | Some dest when dest.Port = 0us -> failBound UnixError.EADDRNOTAVAIL
                | Some dest -> attemptStream dest
                | None ->
                    failwith
                        "UnixConnection.connectSocket: the declared length passed the AF_INET verdict but the copy held no destination, though a length that passes it reaches `sin_addr` (this is a bug in this library)."
        | SocketKind.Datagram ->
            match lengthVerdict with
            | BindLengthVerdict.RejectedBeforeCopy error -> fail error
            | BindLengthVerdict.Accepted
            | BindLengthVerdict.Invalid ->

            match family with
            | None -> fail UnixError.EINVAL
            | Some family ->

            match sock.Phase with
            | SocketPhase.Idle
            | SocketPhase.DatagramPeer _ -> ()
            | phase ->
                failwith
                    $"UnixConnection.connectSocket: a datagram socket holds %A{phase}. this kernel's socket invariants forbid that pairing, so this is a bug in the caller's state construction."

            // Darwin disconnects a connected datagram socket before it judges
            // anything the copy holds, so every connect whose length the copy
            // takes leaves it disconnected, whatever it answers (a success then
            // connects it again): its peer goes and its local address reverts
            // to the wildcard, port kept, whatever `bind(2)` locked --
            // `127.0.0.1:5556` reads back `0.0.0.0:5556`. Measured
            // (`sockaddr-connect-ladder.c`, U, and `sockaddr-dgram-connect.c`,
            // L and D).
            let disconnectedFirst : UnixSystem<'Task, 'Handler> =
                match flavour, sock.Phase, sock.Binding with
                | SimulatedUnixFlavour.Darwin, SocketPhase.DatagramPeer _, Some binding ->
                    { system with
                        Machine =
                            { system.Machine with
                                Sockets =
                                    Map.add
                                        socketId
                                        { sock with
                                            Binding =
                                                Some
                                                    { binding with
                                                        Endpoint =
                                                            { binding.Endpoint with
                                                                Address = InternetEndpoint.WildcardAddress
                                                            }
                                                    }
                                            Phase = SocketPhase.Idle
                                        }
                                        system.Machine.Sockets
                            }
                    }
                | SimulatedUnixFlavour.Darwin, SocketPhase.DatagramPeer _, None ->
                    failwith
                        "UnixConnection.connectSocket: a datagram socket holds a peer but no binding; connect binds before it records the peer, so this is a bug in this library, or in a caller that assembled the state by hand."
                | _ -> system

            if family = 0 then
                match flavour with
                | SimulatedUnixFlavour.Linux ->
                    // Linux's `udp_disconnect`, connected or not, at every
                    // length the copy takes from 2 (measured both ways,
                    // `docs/probes/udp-connect/dissolve.py`, and at every
                    // length by `sockaddr-connect-ladder.c`, U): the
                    // peer filter goes; the address reverts to the wildcard
                    // unless `bind(2)` locked a concrete one; the port is
                    // dropped unless `bind(2)` chose it. So `0.0.0.0:5555`
                    // dissolves to itself, `127.0.0.1:0`-bound-then-connected
                    // to `127.0.0.1:0` (half-bound: it rebinds freely, and a
                    // later connect keeps the address), and an implicit or
                    // `0.0.0.0:0` binding to nothing at all.
                    match sock.Phase, sock.Binding with
                    | SocketPhase.DatagramPeer _, None ->
                        failwith
                            "UnixConnection.connectSocket: a datagram socket holds a peer but no binding; connect binds before it records the peer, so this is a bug in this library, or in a caller that assembled the state by hand."
                    | _, None ->
                        // Nothing to dissolve and nothing bound: the accepted
                        // no-op (measured).
                        completed system
                    | _, Some binding ->
                        let address =
                            match binding.LockedAddress with
                            | Some locked when locked <> InternetEndpoint.WildcardAddress -> Some locked
                            | Some _
                            | None -> None

                        let port =
                            if binding.LockedPort then
                                Some binding.Endpoint.Port
                            else
                                None

                        let dissolved =
                            match address, port with
                            | None, None -> None
                            | _ ->
                                Some
                                    { binding with
                                        Endpoint =
                                            InternetEndpoint.ofParts
                                                (address |> Option.defaultValue InternetEndpoint.WildcardAddress)
                                                (port |> Option.defaultValue 0us)
                                    }

                        completed
                            { system with
                                Machine =
                                    { system.Machine with
                                        Sockets =
                                            Map.add
                                                socketId
                                                { sock with
                                                    Binding = dissolved
                                                    Phase = SocketPhase.Idle
                                                }
                                                system.Machine.Sockets
                                    }
                            }
                | SimulatedUnixFlavour.Darwin ->
                    // Measured (`sockaddr-connect-ladder.c`, U): EINVAL at
                    // every length the copy takes but 16, EAFNOSUPPORT at 16,
                    // and the disconnect below happens either way.
                    let error =
                        match lengthVerdict with
                        | BindLengthVerdict.Accepted -> UnixError.EAFNOSUPPORT
                        | BindLengthVerdict.Invalid -> UnixError.EINVAL
                        | BindLengthVerdict.RejectedBeforeCopy _ ->
                            failwith
                                "UnixConnection.connectSocket: a length the copy rejects outright reached the datagram AF_UNSPEC rule, though the datagram arm answers it first (this is a bug in this library)."

                    // The answer with and without a peer set (measured), but
                    // not before the disconnect has happened.
                    failed error disconnectedFirst
            else

            // Linux binds a datagram socket with no port to the wildcard and an
            // ephemeral port before it judges the length or the family, for
            // every family but AF_UNSPEC, and a failure keeps that binding;
            // Darwin binds nothing, and disconnects first. Measured
            // (`sockaddr-dgram-connect.c`, B, D and L).
            let failPrepared
                (error : UnixError)
                : Result<ConnectOutcome * UnixSystem<'Task, 'Handler>, ConnectRefusal>
                =
                match flavour with
                | SimulatedUnixFlavour.Darwin -> failed error disconnectedFirst
                | SimulatedUnixFlavour.Linux ->
                    match sock.Binding with
                    | Some binding when binding.Endpoint.Port <> 0us -> fail error
                    | existing ->
                        // A half-bound socket keeps its address and gains a
                        // port, as `inet_autobind` does for it on success.
                        let candidate (port : uint16) : SocketBinding =
                            match existing with
                            | Some binding ->
                                { binding with
                                    Endpoint =
                                        { binding.Endpoint with
                                            Port = port
                                        }
                                }
                            | None ->
                                {
                                    Endpoint = InternetEndpoint.ofParts InternetEndpoint.WildcardAddress port
                                    // No bind(2) ran.
                                    LockedAddress = None
                                    LockedPort = false
                                }

                        match
                            UnixMachineState.allocateEphemeralPort
                                EphemeralPortUse.Reserve
                                socketId
                                sock
                                candidate
                                system.Machine
                        with
                        | None -> Error (ConnectRefusal.EphemeralPortsExhausted system.Machine.EphemeralPortRange)
                        | Some (binding, machine) ->
                            failed
                                error
                                { system with
                                    Machine =
                                        { machine with
                                            Sockets =
                                                Map.add
                                                    socketId
                                                    { sock with
                                                        Binding = Some binding
                                                    }
                                                    machine.Sockets
                                        }
                                }

            match lengthVerdict with
            | BindLengthVerdict.Invalid -> failPrepared UnixError.EINVAL
            | BindLengthVerdict.RejectedBeforeCopy _
            | BindLengthVerdict.Accepted ->

            if family <> SimulatedUnixPlatform.internetAddressFamily then
                failPrepared UnixError.EAFNOSUPPORT
            else

            match destination with
            | None ->
                failwith
                    "UnixConnection.connectSocket: the declared length passed the AF_INET verdict but the copy held no destination, though a length that passes it reaches `sin_addr` (this is a bug in this library)."
            // Measured (`sockaddr-dgram-connect.c`, D): Darwin answers a port
            // of 0 with EADDRNOTAVAIL whatever the address, before it looks
            // the address up. Linux connects it, to no peer `getpeername(2)`
            // can read.
            | Some dest when dest.Port = 0us && flavour = SimulatedUnixFlavour.Darwin ->
                failPrepared UnixError.EADDRNOTAVAIL
            | Some dest ->

            let dest = resolveWildcard dest

            if SimulatedUnixPlatform.isBroadcastOrMulticast dest.Address then
                // Measured (`sockaddr-dgram-connect.c`, D and M): Linux answers
                // the broadcast address with EACCES, `SO_BROADCAST` being off
                // (this library models no way to set it); every other such
                // connect succeeds, aimed at a group or a broadcast this
                // library cannot carry.
                if flavour = SimulatedUnixFlavour.Linux && dest.Address = System.UInt32.MaxValue then
                    failPrepared UnixError.EACCES
                else
                    Error (ConnectRefusal.DatagramGroupDestination (socketId, dest))
            elif not (destinationIsLocal dest.Address) then
                Error (ConnectRefusal.DestinationNotLocal (dest, SocketKind.Datagram))
            else

            // A datagram connect is a peer filter, not a handshake: it
            // succeeds with nothing at the destination and a re-connect
            // re-targets, both measured. It binds implicitly just as a
            // stream connect does. On Darwin the source is resolved from the
            // binding the disconnect left, so a connected socket bound to an
            // interface address connects to loopback from 127.0.0.1, where
            // Linux keeps the interface address (`sockaddr-dgram-reconnect.c`).
            let current = (UnixMachineState.socket socketId disconnectedFirst.Machine).Binding

            match ensureBoundFrom current dest disconnectedFirst with
            | Error refusal -> Error refusal
            | Ok (binding, system) ->

            // Another datagram socket already holding this very source and
            // peer. Linux admits it: two sockets sharing a port through
            // SO_REUSEADDR both connect to one peer (`sockaddr-dgram-duplicate.c`).
            // Darwin reaches it only through a reconnect whose source is
            // resolved afresh, and how it refuses that is unmeasured.
            let duplicate =
                flavour = SimulatedUnixFlavour.Darwin
                && system.Machine.Sockets
                   |> Map.exists (fun otherId other ->
                       otherId <> socketId
                       && other.Kind = SocketKind.Datagram
                       && other.Phase = SocketPhase.DatagramPeer dest
                       && (other.Binding |> Option.map (fun b -> b.Endpoint)) = Some binding.Endpoint
                   )

            if duplicate then
                Error (ConnectRefusal.DuplicateFourTuple (binding.Endpoint, dest))
            else

            let system =
                { system with
                    Machine =
                        { system.Machine with
                            Sockets =
                                Map.add
                                    socketId
                                    { sock with
                                        Binding = Some binding
                                        Phase = SocketPhase.DatagramPeer dest
                                    }
                                    system.Machine.Sockets
                        }
                }

            completed system

    /// `connect(2)` on the socket `socketId` past the descriptor screens: the
    /// ladder `connect` runs once it has looked the descriptor up, through a
    /// description whose `O_NONBLOCK` is `nonBlocking`. For a client that wants
    /// to put a kernel into a state where a connection is pending, established
    /// or refused; a syscall goes through `connect`.
    ///
    /// `copied` is the bytes a copy-in of `declaredLength` takes from real
    /// storage: exactly as many as `admitSockaddrCopy` answers `Transfer` with
    /// for a mapped buffer, and none where it rejects the length before the
    /// copy. Passing any other number is refused, as a bug in the caller.
    let connectSocket<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (socketId : SocketId)
        (nonBlocking : bool)
        (declaredLength : uint32)
        (copied : ImmutableArray<byte>)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<ConnectOutcome * UnixSystem<'Task, 'Handler>, ConnectRefusal>
        =
        let platform = system.Machine.UnixPlatform

        UnixSocket.requireCopied
            "UnixConnection.connectSocket"
            (UnixSocket.mappedCopyLength platform declaredLength)
            copied

        connectDecoded
            socketId
            nonBlocking
            declaredLength
            (SimulatedUnixPlatform.decodeInternetSockaddr platform copied)
            system

    /// `connect(2)`: point `fd` at the address in its sockaddr, or ask what
    /// pointing it there would answer.
    ///
    /// `copied` is the bytes the kernel copies in: exactly as many as
    /// `admitSockaddrCopy` answered `Transfer` with, from the start of the
    /// caller's buffer, or none where it answered `Answered`. Passing any other
    /// number is refused, as a bug in the caller. The kernel reads the family,
    /// port and address out of them itself.
    let connect<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (fd : int)
        (destination : UserBuffer)
        (declaredLength : uint32)
        (copied : ImmutableArray<byte>)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<ConnectOutcome * UnixSystem<'Task, 'Handler>, ConnectRefusal>
        =
        match UnixSocket.admitSockaddrCopy SockaddrCopySyscall.Connect fd destination declaredLength system with
        | Error refusal -> Error (ConnectRefusal.Copy refusal)
        | Ok (SockaddrCopyAdmission.Answered error) ->
            UnixSocket.requireCopied "UnixConnection.connect" 0 copied
            Ok (ConnectOutcome.Failed error, system)
        | Ok (SockaddrCopyAdmission.Transfer length) ->

        UnixSocket.requireCopied "UnixConnection.connect" length copied

        // `admitSockaddrCopy` reached the copy, so the descriptor is a live IPv4
        // socket; nothing between there and here could have changed that.
        let socketId =
            match FileDescriptorRegistry.tryFindTarget fd (UnixSystemState.fileDescriptors system) with
            | Some (OpenFileTarget.Socket socketId) -> socketId
            | other ->
                failwith
                    $"UnixConnection.connect: fd %d{fd} names %A{other}, yet the admission above reached the sockaddr copy, which only a socket does (this is a bug in this library)."

        // `O_NONBLOCK` is a fact about the open file description `fd` came
        // through, not about the socket, so a connect through a `dup` of a
        // non-blocking socket pends too.
        let nonBlocking =
            match FileDescriptorRegistry.tryFind fd (UnixSystemState.fileDescriptors system) with
            | Some description -> description.NonBlocking
            | None ->
                failwith
                    $"UnixConnection.connect: fd %d{fd} resolved to a socket a line above and nothing here closes it (this is a bug in this library)."

        connectDecoded
            socketId
            nonBlocking
            declaredLength
            (SimulatedUnixPlatform.decodeInternetSockaddr system.Machine.UnixPlatform copied)
            system

    /// Dequeue the oldest completed connection from `socketId`'s accept queue
    /// and materialise the server-side socket onto it: a fresh socket, bound at
    /// the connection's server address, on a fresh **blocking** descriptor.
    /// Answers the new fd and the connection, whose `ClientAddress` is what
    /// `accept(2)` reports as the peer.
    ///
    /// Blocking unconditionally, which is not the whole of `accept(2)`: on a
    /// flavour where the accepted socket inherits `O_NONBLOCK`, it inherits it
    /// from the *description the call was made through*, and a `SocketId` does
    /// not name one. `accept` applies that, having the descriptor.
    ///
    /// The state transition on its own, without the entry point's screens, for a
    /// client that wants to put a kernel into a state where a connection has
    /// been accepted. `accept` is what a syscall goes through.
    ///
    /// Partial: `socketId` must be a listening socket with a non-empty queue,
    /// and a descriptor below the bound (`SimulatedUnixPlatform.descriptorBound`)
    /// must be free. `accept` answers EAGAIN (or parks) for an empty one, and
    /// EINVAL/EOPNOTSUPP for a socket that is not a listening stream socket, and
    /// refuses a full table, so reaching this in any other state is a bug in
    /// the caller.
    let acceptConnection<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (socketId : SocketId)
        (system : UnixSystem<'Task, 'Handler>)
        : int * TcpConnection * UnixSystem<'Task, 'Handler>
        =
        match
            FileDescriptorRegistry.room
                (SimulatedUnixPlatform.descriptorBound system.Machine.UnixPlatform)
                0
                1
                (UnixSystemState.fileDescriptors system)
        with
        | Error refusal ->
            failwith
                $"UnixConnection.acceptConnection: %s{DescriptorLimitRefusal.describe refusal} `accept` refuses this before reaching here (this is a bug in the caller)."
        | Ok () ->

        let listener = UnixMachineState.socket socketId system.Machine

        match listener.Phase with
        | SocketPhase.Listening ({
                                     Queue = connectionId :: rest
                                 } as listenState) ->
            let tcpConnection = UnixMachineState.connection connectionId system.Machine
            let acceptedId = system.Machine.NextSocketId
            let (SocketId rawAcceptedId) = acceptedId

            let fd, registry =
                FileDescriptorRegistry.createSocket acceptedId (UnixSystemState.fileDescriptors system)

            let accepted =
                {
                    Domain = listener.Domain
                    Kind = SocketKind.Stream
                    Protocol = listener.Protocol
                    Binding =
                        Some
                            {
                                Endpoint = tcpConnection.ServerAddress
                                // Nothing reads this on an accepted socket:
                                // its phase is Established for life, so no
                                // refusal delivery can ever revert it.
                                LockedAddress = None
                                LockedPort = false
                            }
                    // Both kernels copy the listener's socket options onto
                    // the new socket when the connection completes
                    // (inet_csk_clone_lock; sonewconn), not at accept.
                    // Reading the listener now gives the same value because
                    // `UnixSocket.setsockopt` refuses to change it while
                    // connections are queued.
                    ReuseAddress = listener.ReuseAddress
                    Phase = SocketPhase.Established (connectionId, ConnectionEnd.Server)
                }

            fd,
            tcpConnection,
            { system with
                Machine =
                    { system.Machine with
                        Sockets =
                            system.Machine.Sockets
                            |> Map.add acceptedId accepted
                            |> Map.add
                                socketId
                                { listener with
                                    Phase =
                                        SocketPhase.Listening
                                            { listenState with
                                                Queue = rest
                                            }
                                }
                        NextSocketId = SocketId (rawAcceptedId + 1L)
                    }
            }
            |> UnixSystemState.withFileDescriptors registry
        | SocketPhase.Listening {
                                    Queue = []
                                } ->
            failwith
                "UnixConnection.acceptConnection: the accept queue is empty; `accept` answers EAGAIN (or parks) before reaching this (this is a bug in the caller)."
        | phase ->
            failwith
                $"UnixConnection.acceptConnection: socket %O{socketId} is in %A{phase}, not listening; `accept` screens this (this is a bug in the caller)."

    /// Take the oldest connection off `socketId`'s accept queue and close its
    /// server end at once, without a descriptor: the state `acceptConnection`
    /// followed by `close` of the accepted descriptor would leave, bar the
    /// identities those would spend.
    let private dropConnection<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (socketId : SocketId)
        (system : UnixSystem<'Task, 'Handler>)
        : UnixSystem<'Task, 'Handler>
        =
        let listener = UnixMachineState.socket socketId system.Machine

        let connectionId, listenState =
            match listener.Phase with
            | SocketPhase.Listening ({
                                         Queue = connectionId :: rest
                                     } as listenState) ->
                connectionId,
                { listenState with
                    Queue = rest
                }
            | phase ->
                failwith
                    $"UnixConnection.dropConnection: socket %O{socketId} is in %A{phase}, not listening with a connection queued; `accept` screens this (this is a bug in this library)."

        let sockets =
            Map.add
                socketId
                { listener with
                    Phase = SocketPhase.Listening listenState
                }
                system.Machine.Sockets

        // The client end, if the client has not closed it, is the only socket
        // left referencing the connection: an accept queue holds a connection
        // once, and only this one held it. Closing the server end leaves the
        // client half-closed, and the connection lives while the client does.
        let clients =
            sockets
            |> Map.toList
            |> List.choose (fun (survivorId, survivor) ->
                match survivor.Phase with
                | SocketPhase.Established (c, _)
                | SocketPhase.EstablishedPendingReport c when c = connectionId -> Some survivorId
                | _ -> None
            )

        let connections =
            if List.isEmpty clients then
                Map.remove connectionId system.Machine.Connections
            else
                system.Machine.Connections

        let system =
            { system with
                Machine =
                    { system.Machine with
                        Sockets = sockets
                        Connections = connections
                    }
            }

        // The FIN's edge, raised once the tables reflect the close, as `close`
        // raises it.
        (system, clients)
        ||> List.fold (fun system client -> SocketWake.signal client SocketWake.PeerFin system)

    /// Hand the oldest connection on `socketId`'s queue over to the caller: the
    /// half of `accept(2)` that follows the choice of a connection, shared by a
    /// call that finds one at once and a parked call that finds one on waking.
    ///
    /// `nonBlocking` is the listening description's `O_NONBLOCK` as the
    /// connection is handed over, which is what Darwin's accepted socket
    /// inherits (measured, `blocking-accept.c` section F: a flag set while the
    /// call slept was inherited).
    let private handOver<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (socketId : SocketId)
        (nonBlocking : bool)
        (destination : UserBuffer)
        (declaredLength : uint32)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<AcceptOutcome * UnixSystem<'Task, 'Handler>, AcceptRefusal>
        =
        // A connection is there to take, which needs a descriptor below the
        // limit: at a limit of the bound, EMFILE (measured, `fcntl-dup.c`,
        // LIMIT rows). A Linux accept that reaches here from its park took its
        // descriptor before it slept, which this kernel does not hold for it,
        // so with none left now it is refused all the same.
        match
            FileDescriptorRegistry.room
                (SimulatedUnixPlatform.descriptorBound system.Machine.UnixPlatform)
                0
                1
                (UnixSystemState.fileDescriptors system)
        with
        | Error refusal -> Error (AcceptRefusal.DescriptorLimit refusal)
        | Ok () ->

        let reportedLength = SimulatedUnixPlatform.internetSocketAddressSize

        // Measured (`socket-address-length.c`): Linux reads the length cell as
        // an `int` only once it holds the connection, and answers EINVAL for a
        // negative one without touching the destination -- so the connection
        // is lost. Unless the destination is NULL, when it reads no length at
        // all; that is refused below, as for any other length.
        let linuxNegative =
            SimulatedUnixPlatform.flavour system.Machine.UnixPlatform = SimulatedUnixFlavour.Linux
            && int declaredLength < 0

        match linuxNegative, destination with
        | true, UserBuffer.Addressless ->
            // Whether the kernel reads the length at all turns on whether the
            // address is NULL, which a client with no number for it cannot say.
            Error (AcceptRefusal.Buffer BufferRefusal.AddresslessAtScreen)
        | true, UserBuffer.Mapped
        | true, UserBuffer.Opaque ->
            Ok (AcceptOutcome.DroppedConnection UnixError.EINVAL, dropConnection socketId system)
        | true, UserBuffer.Unmapped address when address <> 0UL ->
            Ok (AcceptOutcome.DroppedConnection UnixError.EINVAL, dropConnection socketId system)
        | true, UserBuffer.Unmapped _
        | false, _ ->

        // The destination is screened after the queue and before the dequeue,
        // which is the only place it can go: there is nothing to copy out until
        // a connection has been selected. A call that writes nothing never looks
        // at it at all.
        let destinationRefusal =
            if declaredLength = 0u then
                None
            else
                match destination with
                | UserBuffer.Mapped -> None
                | UserBuffer.Opaque -> Some (AcceptRefusal.Buffer BufferRefusal.OpaqueAtTransfer)
                | UserBuffer.Addressless -> Some (AcceptRefusal.Buffer BufferRefusal.AddresslessAtTransfer)
                | UserBuffer.Unmapped _ -> Some (AcceptRefusal.UnmeasuredCopyOutFault socketId)

        match destinationRefusal with
        | Some refusal -> Error refusal
        | None ->

        let acceptedFd, connection, system = acceptConnection socketId system

        // `O_NONBLOCK` inheritance is the flavour's answer rather than this
        // kernel's convenience: Darwin's `accept(2)` copies the listening
        // description's flag onto the accepted socket and Linux's does not
        // (measured; see `acceptedSocketInheritsNonBlocking`). It is inherited
        // from the description the call was made through, so a `dup` of a
        // non-blocking listener passes the flag on too.
        let system =
            if
                nonBlocking
                && SimulatedUnixPlatform.acceptedSocketInheritsNonBlocking system.Machine.UnixPlatform
            then
                UnixSystemState.withFileDescriptors
                    (FileDescriptorRegistry.setNonBlocking acceptedFd true (UnixSystemState.fileDescriptors system))
                    system
            else
                system

        let copiedOut =
            SimulatedUnixPlatform.copyOutInternetSockaddr
                system.Machine.UnixPlatform
                connection.ClientAddress
                declaredLength

        Ok (AcceptOutcome.Accepted (acceptedFd, copiedOut, reportedLength), system)

    /// `accept(2)`, made by `task`: take the oldest completed connection off
    /// `fd`'s accept queue and hand back a descriptor onto the server side of
    /// it; or, for a blocking listener with nothing queued, sleep until a
    /// connection arrives.
    ///
    /// `destination` is where the peer address would be copied out, and
    /// `declaredLength` how much of it may be written. As for `getsockname`, the
    /// declared length **does not bound what is reported**: a call declaring 8
    /// writes eight bytes and still reports 16.
    ///
    /// `declaredLength` is the 32-bit word the caller read out of its length
    /// cell. Linux reads it as an `int`, and only once it holds a connection:
    /// a negative one fails with `EINVAL` after the connection has been taken
    /// off the queue, which loses it (`AcceptOutcome.DroppedConnection`).
    /// Darwin reads it as the `socklen_t` it is, so no length is an error there.
    ///
    /// A call that writes nothing never looks at `destination`: at a declared
    /// length of zero every buffer succeeds, including one naming no storage.
    ///
    /// Every `AcceptOutcome.Failed` leaves the listener exactly as it was, the
    /// queue included.
    ///
    /// A listener whose description is blocking and whose queue is empty parks
    /// `task` until a connection is queued, and the call is finished with
    /// `finishAccept`. It waits for ever: `SO_RCVTIMEO`, which bounds such a
    /// wait on Linux (and not on Darwin), is an option `setsockopt` refuses to
    /// set. Of several tasks parked on one listener, a connection wakes the one
    /// that parked first (see `UnixWait.wakes`). The destination is not looked
    /// at before the call sleeps.
    ///
    /// Under Darwin, a close of the descriptor a sleeping accept was made
    /// through ends every accept asleep on the listener, through that
    /// descriptor or another, and leaves the listener drained
    /// (`ListenState.Drained`); see `UnixDescriptor.close`. A later accept on
    /// it that finds a connection queued takes it, and a non-blocking one that
    /// finds none answers `EAGAIN`; one that would sleep is refused
    /// (`AcceptRefusal.DarwinDrainedListener`).
    ///
    /// The accepted descriptor inherits `O_NONBLOCK` from the description this
    /// call was made through, on the flavours whose kernels do that: see
    /// `SimulatedUnixPlatform.acceptedSocketInheritsNonBlocking`. A client whose
    /// own sockets want one answer on every platform clears it itself.
    ///
    /// `task` must not already be parked.
    let accept<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (task : 'Task)
        (fd : int)
        (destination : UserBuffer)
        (declaredLength : uint32)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<AcceptOutcome * UnixSystem<'Task, 'Handler>, AcceptRefusal>
        =
        match UnixTaskTable.parkedFor task system.Tasks with
        | Some parked ->
            failwith
                $"UnixConnection.accept: task %O{task} is parked in %A{parked}, and is issuing an accept. A task blocks in one syscall at a time; a parked accept is finished with `finishAccept` (this is a bug in the client)."
        | None ->

        // The descriptor is classified before the destination is looked at, and
        // before the accept queue is: measured on both flavours, a closed
        // descriptor answers EBADF and a non-socket ENOTSOCK whatever the
        // destination and whatever the listener would have said.
        match FileDescriptorRegistry.tryFindWithId fd (UnixSystemState.fileDescriptors system) with
        | None -> Ok (AcceptOutcome.Failed UnixError.EBADF, system)
        | Some (descriptionId, description) ->

        // Measured (`fcntl-dup.c`, LIMIT rows): Linux takes the new descriptor
        // right after EBADF, so with none left below the limit even a file's
        // ENOTSOCK and an empty listener's EAGAIN are EMFILE. Darwin takes it
        // only with a connection to hand over (`handOver`).
        let linuxRoom =
            match SimulatedUnixPlatform.flavour system.Machine.UnixPlatform with
            | SimulatedUnixFlavour.Darwin -> Ok ()
            | SimulatedUnixFlavour.Linux ->
                FileDescriptorRegistry.room
                    (SimulatedUnixPlatform.descriptorBound system.Machine.UnixPlatform)
                    0
                    1
                    (UnixSystemState.fileDescriptors system)

        match linuxRoom with
        | Error refusal -> Error (AcceptRefusal.DescriptorLimit refusal)
        | Ok () ->

        match description.Target with
        | OpenFileTarget.File _
        | OpenFileTarget.Directory _
        | OpenFileTarget.CharacterDevice _
        | OpenFileTarget.Pipe _
        | OpenFileTarget.Kqueue _
        | OpenFileTarget.Epoll _ -> Ok (AcceptOutcome.Failed UnixError.ENOTSOCK, system)
        | OpenFileTarget.Socket socketId ->

        let socket = UnixMachineState.socket socketId system.Machine

        match socket.Domain with
        | SocketDomain.Inet6
        | SocketDomain.Unix -> Error (AcceptRefusal.UnmodelledDomain (socketId, socket.Domain))
        | SocketDomain.Inet ->

        match socket.Kind with
        | SocketKind.Datagram ->
            // The kind check beats the listening check: measured on both, a
            // datagram socket -- which is also "not listening" -- answers
            // EOPNOTSUPP, blocking or not.
            Ok (AcceptOutcome.Failed UnixError.EOPNOTSUPP, system)
        | SocketKind.SeqPacket -> Error (AcceptRefusal.UnmeasuredKind (socketId, socket.Kind))
        | SocketKind.Stream ->

        match socket.Phase with
        | SocketPhase.DatagramPeer _ ->
            failwith
                $"UnixConnection.accept: socket %O{socketId} is a stream socket holding SocketPhase.DatagramPeer, a pairing this kernel's socket invariants forbid (this is a bug in the caller's state construction)."
        | SocketPhase.Idle
        | SocketPhase.EstablishedPendingReport _
        | SocketPhase.Established _
        | SocketPhase.Refused _ ->
            // ...and the listening check beats blocking behaviour: measured on
            // both, a *blocking* non-listening socket answers EINVAL
            // immediately rather than parking. Measured for idle sockets, bound
            // or not; the other non-listening phases share the answer because it
            // is the same kernel test (Linux's TCP_LISTEN check, Darwin's
            // SO_ACCEPTCONN check).
            Ok (AcceptOutcome.Failed UnixError.EINVAL, system)
        | SocketPhase.Listening listenState ->

        match listenState.Queue with
        | [] ->
            // `O_NONBLOCK` is a fact about the open file description `fd` came
            // through, not about the socket, so an accept through a `dup` of a
            // non-blocking listener answers EAGAIN too. Measured on Darwin
            // (close-ends-call.c section A7a), a drained listener's is EAGAIN
            // too.
            if description.NonBlocking then
                Ok (AcceptOutcome.Failed UnixError.EAGAIN, system)
            elif listenState.Drained then
                Error (AcceptRefusal.DarwinDrainedListener socketId)
            else
                let parked =
                    ParkedSyscall.Accept
                        {
                            Listener = SleepTarget.Waiting (descriptionId, fd)
                            Destination = destination
                            DeclaredLength = declaredLength
                        }

                Ok (AcceptOutcome.WouldBlock (WakeCondition.ofPark parked), UnixWait.park task parked system)
        | _ :: _ -> handOver socketId description.NonBlocking destination declaredLength system

    let private finishAcceptHolding<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (task : 'Task)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<AcceptOutcome * UnixSystem<'Task, 'Handler>, AcceptRefusal>
        =
        let parked =
            match UnixTaskTable.parkedFor task system.Tasks with
            | Some (ParkedSyscall.Accept parked) -> parked
            | Some other ->
                failwith
                    $"UnixConnection.finishAccept: task %O{task} is parked in %A{other}, not in an accept, so there is no accept to finish (this is a bug in the client)."
            | None ->
                failwith
                    $"UnixConnection.finishAccept: task %O{task} is not parked, so there is no accept to finish. Only a task `accept` answered `WouldBlock` finishes here (this is a bug in the client)."

        match parked.Listener with
        | SleepTarget.EndedByClose _ ->
            // Measured on Darwin 27.0.0 (`close-ends-call.c`, sections A1-A4):
            // ECONNABORTED, whatever is queued and whatever signal is pending,
            // since a woken accept on a drained listener answers it whatever
            // woke it (section A7).
            Ok (AcceptOutcome.Failed UnixError.ECONNABORTED, (UnixParkState.unpark task system))
        | SleepTarget.Waiting (listenerId, _) ->

        let description =
            match OpenFileTable.tryFind listenerId system.Machine.OpenFiles with
            | Some description -> description
            | None ->
                failwith
                    $"UnixConnection.finishAccept: task %O{task}'s accept waits on open file description %O{listenerId}, which is not in the table, but a park holds its description until the call returns (this is a bug in this library, or in a caller that ended a park without its finishing call or assembled the state by hand)."

        let socketId =
            match description.Target with
            | OpenFileTarget.Socket socketId -> socketId
            | OpenFileTarget.File _
            | OpenFileTarget.Directory _
            | OpenFileTarget.CharacterDevice _
            | OpenFileTarget.Pipe _
            | OpenFileTarget.Kqueue _
            | OpenFileTarget.Epoll _ ->
                failwith
                    $"UnixConnection.finishAccept: task %O{task}'s accept waits on open file description %O{listenerId}, which names %A{description.Target} rather than a socket. `accept` parks only on a listening socket (this is a bug in the caller that recorded the park)."

        let finished = (UnixParkState.unpark task system)

        match (UnixMachineState.socket socketId system.Machine).Phase with
        | SocketPhase.Listening {
                                    Drained = true
                                } ->
            failwith
                $"UnixConnection.finishAccept: task %O{task}'s accept sleeps on socket %O{socketId}, which a close has drained. The close that drains a listener ends every accept asleep on it, and `accept` refuses to sleep on a drained one (this is a bug in this library, or in a caller that assembled the state by hand)."
        | SocketPhase.Listening {
                                    Queue = _ :: _
                                } ->
            // Measured on Linux 6.18.5 (`signal-interrupt-requeue.c`, section
            // D): a queued connection beats a pending signal. Darwin answers
            // whichever came first, which `beforeCompleting` refuses.
            match SyscallInterruption.beforeCompleting task system with
            | Error refusal -> Error (AcceptRefusal.Interruption refusal)
            | Ok () -> handOver socketId description.NonBlocking parked.Destination parked.DeclaredLength finished
        | SocketPhase.Listening {
                                    Queue = []
                                } ->
            // Measured on Linux 6.18.5 and Darwin 27.0.0
            // (`signal-interrupt-requeue.c`, section B): an accepter that a
            // signal ends, restarted or calling again on EINTR, returns after
            // every accepter that parked before it, so it has left the queue.
            match SyscallInterruption.ofPark task system with
            | Error refusal -> Error (AcceptRefusal.Interruption refusal)
            | Ok (Some SyscallInterruption.Eintr) -> Ok (AcceptOutcome.Failed UnixError.EINTR, finished)
            | Ok (Some SyscallInterruption.Restart) -> Ok (AcceptOutcome.Restarts, finished)
            | Ok None ->
                let parkedAgain = ParkedSyscall.Accept parked
                Ok (AcceptOutcome.WouldBlock (WakeCondition.ofPark parkedAgain), UnixWait.park task parkedAgain system)
        | phase ->
            failwith
                $"UnixConnection.finishAccept: task %O{task}'s accept waits on socket %O{socketId}, which is in %A{phase} rather than listening. Nothing takes a live listener out of listening, so the park was recorded on a socket that was never one (this is a bug in the caller that recorded it)."

    /// Finish the `accept` `task` is parked in: look at the listener's queue
    /// again, as a woken real accept does, and answer.
    ///
    /// Hands over the oldest connection on the queue, copying its peer address
    /// out to the destination the call was entered with; see `accept` for the
    /// destinations that refuses. A queue found empty again (another caller
    /// took the connection) parks the task again, behind every other park,
    /// which on a real kernel puts it at the back of the listener's queue of
    /// accepters. So does a listener whose description has become non-blocking
    /// while the call slept: measured on both flavours (`blocking-accept.c`
    /// section F), a sleeping accept is not woken by that, and goes on waiting.
    ///
    /// On the flavours whose accepted socket inherits `O_NONBLOCK`, it inherits
    /// the listening description's flag as it stands when the call finishes.
    ///
    /// With the queue empty, a signal with a handler pending for the task ends
    /// the call: `Restarts` if every handler that runs was installed with
    /// `SA_RESTART`, and `Failed EINTR` if none was. Either way the task leaves
    /// the listener's queue of accepters, and issuing the call again puts it at
    /// the back. An answer, `Restarts` included, clears the park.
    ///
    /// The sleeping call holds the listener, so it outlives its last descriptor
    /// until the call returns. A listener no descriptor names goes as the call
    /// answers, and if a connection whose client is open is left in its queue,
    /// what that does to the client is unmeasured and the finish refuses
    /// (`AcceptRefusal.Release`).
    ///
    /// An accept a close has ended (`SleepTarget.EndedByClose`, under Darwin)
    /// answers `ECONNABORTED`, whatever is queued and whatever signal is
    /// pending, and leaves the queue as it is.
    ///
    /// `task` must be parked in an accept.
    let finishAccept<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (task : 'Task)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<AcceptOutcome * UnixSystem<'Task, 'Handler>, AcceptRefusal>
        =
        let held =
            match UnixTaskTable.parkedFor task system.Tasks with
            | Some parked -> ParkedSyscall.descriptions parked
            | None -> []

        // The call's reference to the listener goes as it returns, and with it
        // the listener, if no descriptor names it any more
        // (`open-file-references.c` section A1).
        match finishAcceptHolding task system with
        | Error refusal -> Error refusal
        | Ok (outcome, after) ->
            match ObjectLifetime.releaseUnreferenced held after with
            | Ok released -> Ok (outcome, released)
            | Error refusal -> Error (AcceptRefusal.Release refusal)

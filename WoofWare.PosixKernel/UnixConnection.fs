namespace WoofWare.PosixKernel

/// One `connect(2)` call's answer: it completed, or it failed with the errno
/// the syscall left. EINPROGRESS is a `Failed` like any other -- a caller
/// reports it as it reports any other errno -- and the outcome it defers is
/// already latched on the socket's phase.
[<RequireQualifiedAccess>]
type ConnectOutcome =
    | Completed
    | Failed of UnixError

/// What an `accept(2)` answered.
[<RequireQualifiedAccess>]
type AcceptAnswer =
    /// The call failed with this errno, and nothing about the listener changed.
    /// The accept queue in particular is untouched: measured on both flavours,
    /// a failed `accept` leaves a queued connection queued.
    | Failed of error : UnixError
    /// A connection was dequeued and a socket materialised onto it. `fd` is the
    /// descriptor that socket is open on.
    ///
    /// `peer` is the client's address, which is what `accept(2)` copies out, and
    /// `reportedLength` what the caller's length cell is owed. As for
    /// `getsockname`, the declared length bounds what a client *writes* and not
    /// what is reported: a call declaring 8 writes eight bytes of the encoded
    /// address and still reports 16.
    | Accepted of fd : int * peer : InternetEndpoint * reportedLength : int

/// Why this kernel will not answer an `accept`.
///
/// Distinct from an errno: an errno is an answer, and these are the inputs for
/// which this library has measured what real kernels do and found no single
/// answer to give.
[<RequireQualifiedAccess>]
type AcceptRefusal =
    /// The descriptor is a socket in a domain whose addresses this kernel does
    /// not model, so there is no peer address to report even if the accept
    /// itself would succeed.
    | UnmodelledDomain of socket : SocketId * domain : SocketDomain
    /// The descriptor is a socket of a kind whose `accept(2)` answer is
    /// unmeasured. `SOCK_SEQPACKET` does accept connections and `SOCK_RAW`
    /// plausibly answers EOPNOTSUPP, but neither has been measured, and the
    /// difference between them is the difference between an answer and a state
    /// change.
    | UnmeasuredKind of socket : SocketId * kind : SocketKind
    /// `listener` is a *blocking* listening socket with an empty accept queue,
    /// which a real kernel sleeps in until a connection arrives.
    ///
    /// Not `SyscallOutcome.WouldBlock`, and the difference is the point:
    /// blocking is an outcome only where there is a `WakeCondition` to hand
    /// back, and this kernel has none for the accept side. Nothing wakes such a
    /// sleeper, so parking one would be a deadlock rather than a park.
    | WouldPark of listener : SocketId
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
    /// has been taken off the queue, and whether a real kernel loses it or
    /// leaves it queued is unmeasured. Neither answer is available, so there is
    /// none to give.
    | UnmeasuredCopyOutFault of listener : SocketId

[<RequireQualifiedAccess>]
module AcceptRefusal =
    /// What this kernel knows about why it cannot answer. The client supplies
    /// its own half -- which entry point, which descriptor, and how a caller
    /// could have come by such a socket or such a buffer.
    let describe (refusal : AcceptRefusal) : string =
        match refusal with
        | AcceptRefusal.UnmodelledDomain (socket, domain) ->
            $"the descriptor is socket %O{socket}, whose domain is %O{domain}. This kernel models a peer address only for IPv4: an IPv6 socket's is sixteen bytes of address plus a scope id, and a Unix-domain socket's is a *path* in the filesystem rather than a transport endpoint. Neither is a wider version of what is modelled here, so there is nothing to truncate or widen into an answer."
        | AcceptRefusal.UnmeasuredKind (socket, kind) ->
            $"the descriptor is socket %O{socket}, which is a %O{kind} socket, and what `accept(2)` answers for one is unmeasured. Measure it rather than guessing: SOCK_SEQPACKET does accept connections, so a guess of EOPNOTSUPP there would be a wrong answer rather than an approximate one."
        | AcceptRefusal.WouldPark listener ->
            $"socket %O{listener} is a blocking listener with an empty accept queue, which a real kernel sleeps in. Nothing in this kernel delivers a connection to a sleeping accepter, so a park here would never end. Complete a connect before the accept, or make the listener non-blocking."
        | AcceptRefusal.Buffer refusal -> BufferRefusal.describe refusal
        | AcceptRefusal.UnmeasuredCopyOutFault listener ->
            $"socket %O{listener} has a connection to hand over, so this call succeeds and copies the peer address out -- but the destination is unmapped, so that copy faults. Whether a real kernel loses the connection when it faults, having already taken it off the queue, is unmeasured, so EFAULT is not available here as it is for `getsockname`."

[<RequireQualifiedAccess>]
module UnixConnection =

    /// The `Process` half, mapped. Here because `connectSocket` below signals
    /// through it in four places and spelling the record update out each time
    /// would bury what those four lines are doing.
    let private mapProcess<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (f : UnixProcessState<'Task, 'Handler> -> UnixProcessState<'Task, 'Handler>)
        (system : UnixSystem<'Task, 'Handler>)
        : UnixSystem<'Task, 'Handler>
        =
        { system with
            Process = f system.Process
        }

    /// A *data-ready* wake on `socketId` — the accept-queue push is the one
    /// modelled producer. Keyed: the producer signals synchronously with the
    /// state change, so the socket's new level is the signalled mask, and a
    /// registration whose interest misses it entirely is never queued
    /// (measured, `order6.c`). Lazy so the level is computed only when a
    /// registration actually targets the socket.
    ///
    /// The producers are a measured set, not "anything that writes the
    /// socket table": a datagram re-target or dissolve, `bind(2)`, and the
    /// completion-reporting connect measurably signal nothing at all
    /// (`order3.c` rows N, O, P).
    let signalSocketDataReady<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (socketId : SocketId)
        (system : UnixSystem<'Task, 'Handler>)
        : UnixSystem<'Task, 'Handler>
        =
        { system with
            Process =
                { system.Process with
                    FileDescriptors =
                        FileDescriptorRegistry.signalSocketEventPorts
                            (UnixProcessState.descriptionsNamingSocket socketId system.Process)
                            (Some (lazy (UnixMachineState.socketReadinessLevel socketId system.Machine)))
                            system.Process.FileDescriptors
                }
        }

    /// `connect(2)` past the wrapper's screens and the copy-in faults, which
    /// stay with the caller (they are about the client's memory, which this library
    /// cannot see): the per-flavour ladder over the socket's phase, the
    /// declared length, the sockaddr family, and the destination.
    ///
    /// `family` (the *platform* family number) and `destination` are `None`
    /// when the declared length does not reach the field — this function only
    /// ever answers for an unreadable field, never reads one.
    ///
    /// Every answered row is measured (`connect_probe.c` and successors,
    /// 2026-08-21; docs/plans/2026-08-21-socket-connect.md holds the table);
    /// the failwiths name the unmeasured or unmodellable inputs.
    let connectSocket<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (socketId : SocketId)
        (nonBlocking : bool)
        (declaredLength : int)
        (family : int option)
        (destination : InternetEndpoint option)
        (system : UnixSystem<'Task, 'Handler>)
        : ConnectOutcome * UnixSystem<'Task, 'Handler>
        =
        let sock = UnixMachineState.socket socketId system.Machine
        let platform = system.Machine.UnixPlatform
        let flavour = SimulatedUnixPlatform.flavour platform
        let exactSize = (SimulatedUnixPlatform.socketAddressSizes platform).InterNetwork

        // connect(2) copies the sockaddr in through the same helpers bind(2)
        // uses (Linux's move_addr_to_kernel, Darwin's getsockaddr), and the
        // measured lengths agree with bind's rule exactly: Linux takes 16
        // through 128 and answers EINVAL outside, Darwin takes exactly 16,
        // EINVAL otherwise and ENAMETOOLONG past 255. So the verdict function
        // is shared.
        let lengthVerdict =
            SimulatedUnixPlatform.bindAddressLength platform exactSize declaredLength

        let fail (error : UnixError) : ConnectOutcome * UnixSystem<'Task, 'Handler> =
            ConnectOutcome.Failed error, system

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
        // so that input is refused.
        let ensureBound
            (dest : InternetEndpoint)
            (system : UnixSystem<'Task, 'Handler>)
            : SocketBinding * UnixSystem<'Task, 'Handler>
            =
            match sock.Binding with
            | Some binding when binding.Endpoint.Address <> InternetEndpoint.WildcardAddress -> binding, system
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
                    failwith
                        $"UnixConnection.connectSocket: a socket bound to the wildcard is connecting to %s{InternetEndpoint.toString dest}, and which source address a kernel resolves the wildcard to for a destination other than 127.0.0.1 is unmeasured. Bind to a concrete address first, or connect to 127.0.0.1."

                { binding with
                    Endpoint =
                        { binding.Endpoint with
                            Address = InternetEndpoint.LoopbackAddress
                        }
                },
                system
            | None ->

            if dest.Address <> InternetEndpoint.LoopbackAddress then
                failwith
                    $"UnixConnection.connectSocket: an unbound socket is connecting to %s{InternetEndpoint.toString dest}, and which source address a kernel picks for a destination other than 127.0.0.1 is unmeasured. Bind the socket first, or connect to 127.0.0.1."

            let candidate (port : uint16) : SocketBinding =
                {
                    Endpoint = InternetEndpoint.ofParts InternetEndpoint.LoopbackAddress port
                    // No bind(2) ran: a Linux refusal delivery reverts the
                    // address all the way to the wildcard.
                    LockedAddress = None
                }

            let acceptable (port : uint16) : bool =
                not (
                    system.Machine.Sockets
                    |> Map.exists (fun otherId other ->
                        if otherId = socketId then
                            false
                        else

                        match other.Binding with
                        | None -> false
                        | Some existing ->
                            other.Kind = sock.Kind
                            && SimulatedUnixPlatform.bindConflict
                                platform
                                existing
                                other.ReuseAddress
                                other.Phase
                                (candidate port)
                                sock.ReuseAddress
                    )
                )
                // A connection can outlive the socket that opened it (a
                // closed client whose connection sits queued or accepted),
                // and its four-tuple stays occupied for this destination
                // even though no socket holds the port any more. A real
                // kernel's connect-time port selection skips such tuples,
                // so the allocator must too, in either orientation.
                && not (
                    system.Machine.Connections
                    |> Map.exists (fun _ connection ->
                        let endpoint = (candidate port).Endpoint

                        (connection.ClientAddress = endpoint && connection.ServerAddress = dest)
                        || (connection.ClientAddress = dest && connection.ServerAddress = endpoint)
                    )
                )

            match UnixMachineState.allocateEphemeralPort acceptable system.Machine with
            | Some (port, machine) ->
                candidate port,
                { system with
                    Machine = machine
                }
            | None ->
                let low, high = system.Machine.EphemeralPortRange

                failwith
                    $"UnixConnection.connectSocket: every port in the ephemeral range %d{low}-%d{high} is taken, so this implicit bind has no answer. Widen the machine's EphemeralPortRange, or measure what a real kernel says here."

        // The established/refused attempt, shared by both flavours once the
        // per-flavour screens have let an idle stream socket through.
        let attemptStream (dest : InternetEndpoint) : ConnectOutcome * UnixSystem<'Task, 'Handler> =
            // A wildcard destination means loopback: measured on both,
            // connect to 0.0.0.0:port reaches a loopback listener.
            let dest =
                if dest.Address = InternetEndpoint.WildcardAddress then
                    { dest with
                        Address = InternetEndpoint.LoopbackAddress
                    }
                else
                    dest

            if not (destinationIsLocal dest.Address) then
                failwith
                    $"UnixConnection.connectSocket: destination %s{InternetEndpoint.toString dest} is not a local address of this simulated machine, and PawPrint models no network to carry a SYN anywhere else. Add the address to the kernel's LocalAddresses/LocalRoutes if it should be local, or connect to loopback."

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
            // under SO_REUSEADDR, which no current guest exercises, so the
            // preference has no observer today and is recorded for when it
            // does.
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
                    failwith
                        $"UnixConnection.connectSocket: the accept queue of the listener at %s{InternetEndpoint.toString dest} already holds %d{List.length listenState.Queue} connections, its measured capacity. A real kernel leaves this SYN unanswered and the client retries on a timer — timing PawPrint cannot honour deterministically — so this connect has no faithful answer. Accept from the listener before connecting again, or listen with a larger backlog."

                let clientBinding, system = ensureBound dest system

                // Two corners a REUSEADDR-bound client can engineer, each
                // refused because the real answer is unmeasured (no managed
                // path reaches either: managed clients connect from fresh
                // ephemeral ports).
                if clientBinding.Endpoint = dest then
                    // A wildcard listener at P beside a reuse-bound client at
                    // 127.0.0.1:P, connecting to 127.0.0.1:P: source equals
                    // destination even though a listener matched.
                    failwith
                        $"UnixConnection.connectSocket: the resolved source %s{InternetEndpoint.toString clientBinding.Endpoint} equals the destination, with a listener present. What a real kernel does with this self-tuple (plausibly EINVAL on Darwin, a completed self-connect on Linux) is unmeasured, so measure it rather than guessing."

                if
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
                    failwith
                        $"UnixConnection.connectSocket: a connection from %s{InternetEndpoint.toString clientBinding.Endpoint} to %s{InternetEndpoint.toString dest} already exists, and a real kernel refuses a duplicate four-tuple in ways that are unmeasured (plausibly EADDRINUSE at connect time). Measure it rather than guessing."

                let connectionId = system.Machine.NextConnectionId
                let (ConnectionId rawConnectionId) = connectionId

                let tcpConnection =
                    {
                        ClientAddress = clientBinding.Endpoint
                        ServerAddress = dest
                    }

                let clientPhase =
                    if not nonBlocking then
                        SocketPhase.Established connectionId
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
                            SocketPhase.Established connectionId

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
                    |> mapProcess (UnixProcessState.signalSocketStateChange socketId)
                    |> signalSocketDataReady listenerId

                if nonBlocking then
                    // The syscall itself still answers EINPROGRESS —
                    // measured on both kernels, even on loopback — and the
                    // completion is what the phase above latches.
                    ConnectOutcome.Failed UnixError.EINPROGRESS, system
                else
                    ConnectOutcome.Completed, system
            | None ->
                // The client's own endpoint with no listener behind it is
                // TCP simultaneous open: a real kernel can complete it,
                // connecting the socket to itself. Unmodelled.
                match sock.Binding with
                | Some binding when
                    binding.Endpoint.Port = dest.Port
                    && InternetEndpoint.addressesOverlap binding.Endpoint dest
                    ->
                    failwith
                        $"UnixConnection.connectSocket: destination %s{InternetEndpoint.toString dest} is this socket's own bound address and nothing is listening there. A real kernel can complete this as a TCP simultaneous open — connecting the socket to itself — which PawPrint does not model."
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
                    failwith
                        $"UnixConnection.connectSocket: destination %s{InternetEndpoint.toString dest} is bound but nothing is listening there, and Darwin *drops* such a SYN rather than answering RST: the connect pends on the client's retransmission schedule (a blocking one was measured to stall into ETIMEDOUT), which PawPrint cannot honour deterministically. Listen on the destination socket, or connect to a fully closed port."
                | _ ->

                // The implicit bind happens before the SYN, so a refused
                // socket has a concrete local endpoint too — measured,
                // getsockname reports 127.0.0.1 and a nonzero port while the
                // refusal is pending, on both kernels.
                let binding, system = ensureBound dest system

                if not nonBlocking then
                    // The refusal is delivered inline, and the socket's fate
                    // diverges by flavour exactly as for the deferred
                    // delivery below: measured, a Linux retry is a fresh
                    // attempt and a Darwin one answers EINVAL forever.
                    let phase =
                        match flavour with
                        | SimulatedUnixFlavour.Linux -> SocketPhase.Idle
                        | SimulatedUnixFlavour.Darwin -> SocketPhase.Dead

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
                    let system = mapProcess (UnixProcessState.signalSocketStateChange socketId) system

                    ConnectOutcome.Failed UnixError.ECONNREFUSED, system
                else
                    // EINPROGRESS now; the first later connect delivers
                    // ECONNREFUSED. Measured on both — with no SO_ERROR read
                    // in between, which would consume the pending error and
                    // change these answers; GetSocketErrorOption is not
                    // modelled yet, so only this path is reachable.
                    let system =
                        { system with
                            Machine =
                                { system.Machine with
                                    Sockets =
                                        Map.add
                                            socketId
                                            { sock with
                                                Binding = Some binding
                                                Phase = SocketPhase.RefusedPendingDelivery
                                            }
                                            system.Machine.Sockets
                                }
                        }

                    // The error's arrival signals the client (measured,
                    // `order3.c` row M: the 0x201d edge).
                    let system = mapProcess (UnixProcessState.signalSocketStateChange socketId) system

                    ConnectOutcome.Failed UnixError.EINPROGRESS, system

        match sock.Kind with
        | SocketKind.Raw
        | SocketKind.SeqPacket ->
            failwith
                $"UnixConnection.connectSocket: socket %O{socketId} is a %O{sock.Kind} socket, and what connect(2) does for one is unmeasured, so measure it rather than guessing."
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
                // checks. Measured where a guest reaches it; the state arms'
                // precedence over the argument checks is the pinned source's.
                if family = 0 then
                    match sock.Phase with
                    | SocketPhase.Idle ->
                        // Measured: an accepted no-op, and the socket stays
                        // usable.
                        ConnectOutcome.Completed, system
                    | phase ->
                        failwith
                            $"UnixConnection.connectSocket: AF_UNSPEC on a stream socket in %A{phase} under Linux runs tcp_disconnect, whose consequences for this phase (a connected socket's peer, a listener's queue) are unmeasured and unmodelled."
                else

                match sock.Phase with
                | SocketPhase.EstablishedPendingReport connectionId ->
                    // The one completion-reporting SUCCESS (measured). The
                    // destination is ignored, as the state transition is.
                    ConnectOutcome.Completed, withPhase (SocketPhase.Established connectionId) system
                | SocketPhase.RefusedPendingDelivery ->
                    // Deliver the latched refusal once, then reset: the next
                    // connect is a fresh attempt, and the source address the
                    // pending attempt resolved reverts to whatever bind(2)
                    // locked (both measured).
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
                    // the delivering connect (measured, `order3.c` row M).
                    let system = mapProcess (UnixProcessState.signalSocketStateChange socketId) system

                    ConnectOutcome.Failed UnixError.ECONNREFUSED, system
                | SocketPhase.Dead ->
                    failwith
                        "UnixConnection.connectSocket: a stream socket is in SocketPhase.Dead under the Linux flavour, which only Darwin's refusal delivery produces. This is an interpreter bug."
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
                | Some dest -> attemptStream dest
                | None ->
                    failwith
                        "UnixConnection.connectSocket: the declared length passed the AF_INET verdict but the destination was not supplied; the caller reads it whenever the length reaches it. This is an interpreter bug."
            | SimulatedUnixFlavour.Darwin ->
                // The state arms answer first — measured three ways: the
                // dead latch beats a good destination, EISCONN beats
                // AF_UNSPEC, and the refusal delivery beats a changed
                // destination.
                match sock.Phase with
                | SocketPhase.EstablishedPendingReport _ ->
                    failwith
                        "UnixConnection.connectSocket: a stream socket is in SocketPhase.EstablishedPendingReport under the Darwin flavour, which never constructs it (its retry answers EISCONN directly). This is an interpreter bug."
                | SocketPhase.RefusedPendingDelivery ->
                    // Deliver once; the socket is then dead (measured).
                    ConnectOutcome.Failed UnixError.ECONNREFUSED, withPhase SocketPhase.Dead system
                | SocketPhase.Dead ->
                    // Measured, whatever the destination.
                    fail UnixError.EINVAL
                | SocketPhase.Established _ ->
                    // Measured, including against an AF_UNSPEC destination.
                    fail UnixError.EISCONN
                | SocketPhase.Listening _ ->
                    if family = 0 then
                        failwith
                            "UnixConnection.connectSocket: AF_UNSPEC on a listening stream socket under Darwin is unmeasured (the measured EOPNOTSUPP row used an AF_INET destination), so measure it rather than extrapolating."
                    else
                        // Measured: EOPNOTSUPP, where Linux answers EISCONN.
                        fail UnixError.EOPNOTSUPP
                | SocketPhase.DatagramPeer _ ->
                    failwith
                        "UnixConnection.connectSocket: a stream socket holds SocketPhase.DatagramPeer. this kernel's socket invariants forbid that pairing, so this is a bug in the caller's state construction."
                | SocketPhase.Idle ->

                if family = 0 then
                    // Measured at the exact sockaddr_in length:
                    // EADDRNOTAVAIL, and the socket stays usable. Other
                    // lengths are unmeasured.
                    if declaredLength <> exactSize then
                        failwith
                            $"UnixConnection.connectSocket: AF_UNSPEC with a declared length of %d{declaredLength} on an idle Darwin stream socket is unmeasured (only %d{exactSize} is), so measure it rather than guessing."
                    else
                        fail UnixError.EADDRNOTAVAIL
                else

                match lengthVerdict with
                | BindLengthVerdict.Invalid -> fail UnixError.EINVAL
                | BindLengthVerdict.RejectedBeforeCopy _
                | BindLengthVerdict.Accepted ->

                if family <> SimulatedUnixPlatform.internetAddressFamily then
                    fail UnixError.EAFNOSUPPORT
                else

                match destination with
                | Some dest -> attemptStream dest
                | None ->
                    failwith
                        "UnixConnection.connectSocket: the declared length passed the AF_INET verdict but the destination was not supplied; the caller reads it whenever the length reaches it. This is an interpreter bug."
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

            if family = 0 then
                match flavour with
                | SimulatedUnixFlavour.Linux ->
                    if declaredLength < exactSize then
                        failwith
                            $"UnixConnection.connectSocket: AF_UNSPEC with a declared length of %d{declaredLength} on a Linux datagram socket is unmeasured (only %d{exactSize} and above are), so measure it rather than guessing."
                    else

                    // Measured with and without a peer set: dissolves the
                    // filter and answers SUCCESS. The dissolve also unbinds
                    // what connect resolved — unlike TCP's reset, the *port*
                    // is dropped too (probe8: getsockname reads 0.0.0.0:0
                    // afterwards for an implicitly bound socket and for one
                    // whose bind(2) gave the wildcard), so a socket with no
                    // locked concrete address ends up fully unbound and the
                    // next connect binds afresh. A locked concrete address
                    // was measured to survive with the port zeroed —
                    // 127.0.0.1:0 — but whether a bind(2)-chosen port would
                    // also drop, and how such a half-bound socket rebinds,
                    // is unmeasured, so that provenance is refused.
                    match sock.Phase with
                    | SocketPhase.DatagramPeer _ ->
                        let binding =
                            match sock.Binding with
                            | None ->
                                failwith
                                    "UnixConnection.connectSocket: a datagram socket holds a peer but no binding; connect binds before it records the peer, so this is an interpreter bug."
                            | Some binding ->
                                match binding.LockedAddress with
                                | None -> None
                                | Some locked when locked = InternetEndpoint.WildcardAddress -> None
                                | Some _ ->
                                    failwith
                                        $"UnixConnection.connectSocket: AF_UNSPEC on a datagram socket whose bind(2) locked %s{InternetEndpoint.toString binding.Endpoint}'s address is only measured for a kernel-chosen port (the address survives, the port zeroes); what survives a bind(2)-chosen port, and how the half-bound socket rebinds, is unmeasured. Measure it rather than guessing."

                        ConnectOutcome.Completed,
                        { system with
                            Machine =
                                { system.Machine with
                                    Sockets =
                                        Map.add
                                            socketId
                                            { sock with
                                                Binding = binding
                                                Phase = SocketPhase.Idle
                                            }
                                            system.Machine.Sockets
                                }
                        }
                    | _ ->

                    match sock.Binding with
                    | None ->
                        // No peer to dissolve and nothing bound: the
                        // accepted no-op (measured).
                        ConnectOutcome.Completed, system
                    | Some _ ->
                        failwith
                            "UnixConnection.connectSocket: AF_UNSPEC on a bound but unconnected Linux datagram socket is unmeasured (whether the dissolve drops the binding as it does for a connected one), so measure it rather than guessing."
                | SimulatedUnixFlavour.Darwin ->
                    if declaredLength <> exactSize then
                        failwith
                            $"UnixConnection.connectSocket: AF_UNSPEC with a declared length of %d{declaredLength} on a Darwin datagram socket is unmeasured (only %d{exactSize} is), so measure it rather than guessing."
                    else
                        // Measured with and without a peer set.
                        fail UnixError.EAFNOSUPPORT
            else

            match lengthVerdict with
            | BindLengthVerdict.Invalid -> fail UnixError.EINVAL
            | BindLengthVerdict.RejectedBeforeCopy _
            | BindLengthVerdict.Accepted ->

            if family <> SimulatedUnixPlatform.internetAddressFamily then
                fail UnixError.EAFNOSUPPORT
            else

            match destination with
            | None ->
                failwith
                    "UnixConnection.connectSocket: the declared length passed the AF_INET verdict but the destination was not supplied; the caller reads it whenever the length reaches it. This is an interpreter bug."
            | Some dest ->

            if dest.Address = InternetEndpoint.WildcardAddress then
                failwith
                    "UnixConnection.connectSocket: a datagram connect to 0.0.0.0 is unmeasured (the kernels remap it, but which address the peer filter then holds was not probed), so measure it rather than guessing."
            elif not (destinationIsLocal dest.Address) then
                failwith
                    $"UnixConnection.connectSocket: destination %s{InternetEndpoint.toString dest} is not a local address of this simulated machine, and PawPrint models no network to carry a datagram anywhere else. Add the address to the kernel's LocalAddresses/LocalRoutes if it should be local, or connect to loopback."
            else

            // A datagram connect is a peer filter, not a handshake: it
            // succeeds with nothing at the destination and a re-connect
            // re-targets, both measured. It binds implicitly just as a
            // stream connect does.
            let binding, system = ensureBound dest system

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

            ConnectOutcome.Completed, system

    /// `connect(2)`: point `fd` at `endpoint`, or ask what pointing it there
    /// would answer.
    ///
    /// `family` is the *platform's* family number as it was found in the
    /// caller's sockaddr, and `endpoint` the address and port found there. Both
    /// must be exactly what `admitSockaddrCopy` asked for: a `SockaddrCopyFields` of
    /// `Nothing` means neither, `Family` the family alone, `FamilyAndEndpoint`
    /// both. Supplying less than that is refused rather than answered, because
    /// this kernel's answer for an *unreadable* field is measured and different
    /// from its answer for a field nobody bothered to read.
    ///
    /// The screens `admitSockaddrCopy` performs are performed again here, so a caller
    /// that already has the fields need not have asked; they are pure, and they
    /// agree.
    let connect<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (fd : int)
        (destination : UserBuffer)
        (declaredLength : int)
        (family : int option)
        (endpoint : InternetEndpoint option)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<ConnectOutcome * UnixSystem<'Task, 'Handler>, SockaddrCopyRefusal>
        =
        match UnixSocket.admitSockaddrCopy fd destination declaredLength system with
        | Error refusal -> Error refusal
        | Ok (SockaddrCopyAdmission.Answered error) -> Ok (ConnectOutcome.Failed error, system)
        | Ok (SockaddrCopyAdmission.Transfer (_, fields)) ->

        SockaddrCopyFields.checkSupplied "UnixConnection.connect" fields family endpoint

        // `admitSockaddrCopy` reached the copy, so the descriptor is a live IPv4
        // socket; nothing between there and here could have changed that.
        let socketId =
            match FileDescriptorRegistry.tryFindTarget fd system.Process.FileDescriptors with
            | Some (OpenFileTarget.Socket socketId) -> socketId
            | other ->
                failwith
                    $"UnixConnection.connect: fd %d{fd} names %A{other}, yet the admission above reached the sockaddr copy, which only a socket does (this is an interpreter bug)."

        // `O_NONBLOCK` is a fact about the open file description `fd` came
        // through, not about the socket, so a connect through a `dup` of a
        // non-blocking socket pends too.
        let nonBlocking =
            match FileDescriptorRegistry.tryFind fd system.Process.FileDescriptors with
            | Some description -> description.NonBlocking
            | None ->
                failwith
                    $"UnixConnection.connect: fd %d{fd} resolved to a socket a line above and nothing here closes it (this is an interpreter bug)."

        Ok (connectSocket socketId nonBlocking declaredLength family endpoint system)

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
    /// Partial: `socketId` must be a listening socket with a non-empty queue.
    /// `accept` answers EAGAIN (or refuses to park) for an empty one, and
    /// EINVAL/EOPNOTSUPP for a socket that is not a listening stream socket, so
    /// reaching this in any other state is a bug in the caller.
    let acceptConnection<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (socketId : SocketId)
        (system : UnixSystem<'Task, 'Handler>)
        : int * TcpConnection * UnixSystem<'Task, 'Handler>
        =
        let listener = UnixMachineState.socket socketId system.Machine

        match listener.Phase with
        | SocketPhase.Listening ({
                                     Queue = connectionId :: rest
                                 } as listenState) ->
            let tcpConnection = UnixMachineState.connection connectionId system.Machine
            let acceptedId = system.Machine.NextSocketId
            let (SocketId rawAcceptedId) = acceptedId

            let fd, registry =
                FileDescriptorRegistry.createSocket acceptedId system.Process.FileDescriptors

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
                            }
                    // Both kernels copy the listener's socket options onto
                    // the accepted socket (inet_csk_clone_lock; sonewconn),
                    // and this flag's one modelled effect is bind-conflict
                    // admission.
                    ReuseAddress = listener.ReuseAddress
                    Phase = SocketPhase.Established connectionId
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
                Process =
                    { system.Process with
                        FileDescriptors = registry
                    }
            }
        | SocketPhase.Listening {
                                    Queue = []
                                } ->
            failwith
                "UnixConnection.acceptConnection: the accept queue is empty; `accept` answers EAGAIN (or refuses to park) before reaching this (this is a bug in the caller)."
        | phase ->
            failwith
                $"UnixConnection.acceptConnection: socket %O{socketId} is in %A{phase}, not listening; `accept` screens this (this is a bug in the caller)."

    /// `accept(2)`: take the oldest completed connection off `fd`'s accept queue
    /// and hand back a descriptor onto the server side of it.
    ///
    /// `destination` is where the peer address would be copied out, and
    /// `declaredLength` how much of it may be written. As for `getsockname`, the
    /// declared length **does not bound what is reported**: a call declaring 8
    /// writes eight bytes and still reports 16. It must not be negative -- a
    /// kernel never sees one, because a foreign-function layer that casts it to
    /// `socklen_t` would make the bound `SIZE_MAX` rather than passing it on --
    /// so a caller that has not screened it is asking a question no kernel this
    /// library models was ever asked.
    ///
    /// A call that writes nothing never looks at `destination`: at a declared
    /// length of zero every buffer succeeds, including one naming no storage.
    ///
    /// Every failure leaves the listener exactly as it was, the queue included,
    /// which is why the failing arms hand back the system they were given.
    ///
    /// The accepted descriptor inherits `O_NONBLOCK` from the description this
    /// call was made through, on the flavours whose kernels do that: see
    /// `SimulatedUnixPlatform.acceptedSocketInheritsNonBlocking`. A client whose
    /// own sockets want one answer on every platform clears it itself, which is
    /// what CoreCLR's `SystemNative_Accept` does.
    let accept<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (fd : int)
        (destination : UserBuffer)
        (declaredLength : int)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<AcceptAnswer * UnixSystem<'Task, 'Handler>, AcceptRefusal>
        =
        if declaredLength < 0 then
            failwith
                $"UnixConnection.accept: declared length %d{declaredLength} is negative, which no kernel is ever asked -- a shim that casts it to `socklen_t` makes the bound SIZE_MAX rather than passing it on. Screen this in the client (this is a bug in the caller)."

        // The descriptor is classified before the destination is looked at, and
        // before the accept queue is: measured on both flavours, a closed
        // descriptor answers EBADF and a non-socket ENOTSOCK whatever the
        // destination and whatever the listener would have said.
        match FileDescriptorRegistry.tryFind fd system.Process.FileDescriptors with
        | None -> Ok (AcceptAnswer.Failed UnixError.EBADF, system)
        | Some description ->

        match description.Target with
        | OpenFileTarget.File _
        | OpenFileTarget.StandardStream _
        | OpenFileTarget.SocketEventPort _ -> Ok (AcceptAnswer.Failed UnixError.ENOTSOCK, system)
        | OpenFileTarget.Socket socketId ->

        let socket = UnixMachineState.socket socketId system.Machine

        match socket.Domain with
        | SocketDomain.InterNetworkV6
        | SocketDomain.Unix -> Error (AcceptRefusal.UnmodelledDomain (socketId, socket.Domain))
        | SocketDomain.InterNetwork ->

        match socket.Kind with
        | SocketKind.Datagram ->
            // The kind check beats the listening check: measured on both, a
            // datagram socket -- which is also "not listening" -- answers
            // EOPNOTSUPP, blocking or not.
            Ok (AcceptAnswer.Failed UnixError.EOPNOTSUPP, system)
        | SocketKind.Raw
        | SocketKind.SeqPacket -> Error (AcceptRefusal.UnmeasuredKind (socketId, socket.Kind))
        | SocketKind.Stream ->

        match socket.Phase with
        | SocketPhase.DatagramPeer _ ->
            failwith
                $"UnixConnection.accept: socket %O{socketId} is a stream socket holding SocketPhase.DatagramPeer, a pairing this kernel's socket invariants forbid (this is a bug in the caller's state construction)."
        | SocketPhase.Idle
        | SocketPhase.EstablishedPendingReport _
        | SocketPhase.Established _
        | SocketPhase.RefusedPendingDelivery
        | SocketPhase.Dead ->
            // ...and the listening check beats blocking behaviour: measured on
            // both, a *blocking* non-listening socket answers EINVAL
            // immediately rather than parking. Measured for idle sockets, bound
            // or not; the other non-listening phases share the answer because it
            // is the same kernel test (Linux's TCP_LISTEN check, Darwin's
            // SO_ACCEPTCONN check).
            Ok (AcceptAnswer.Failed UnixError.EINVAL, system)
        | SocketPhase.Listening listenState ->

        match listenState.Queue with
        | [] ->
            // `O_NONBLOCK` is a fact about the open file description `fd` came
            // through, not about the socket, so an accept through a `dup` of a
            // non-blocking listener answers EAGAIN too.
            if description.NonBlocking then
                Ok (AcceptAnswer.Failed UnixError.EAGAIN, system)
            else
                Error (AcceptRefusal.WouldPark socketId)
        | _ :: _ ->

        let reportedLength =
            (SimulatedUnixPlatform.socketAddressSizes system.Machine.UnixPlatform).InterNetwork

        // The destination is screened after the queue and before the dequeue,
        // which is the only place it can go: there is nothing to copy out until
        // a connection has been selected. A call that writes nothing never looks
        // at it at all.
        let destinationRefusal =
            if declaredLength = 0 then
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
        // from the description this call was made through, so a `dup` of a
        // non-blocking listener passes the flag on too.
        let system =
            if
                description.NonBlocking
                && SimulatedUnixPlatform.acceptedSocketInheritsNonBlocking system.Machine.UnixPlatform
            then
                { system with
                    Process =
                        { system.Process with
                            FileDescriptors =
                                FileDescriptorRegistry.setNonBlocking acceptedFd true system.Process.FileDescriptors
                        }
                }
            else
                system

        Ok (AcceptAnswer.Accepted (acceptedFd, connection.ClientAddress, reportedLength), system)

namespace WoofWare.PawPrint.Test

open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PosixKernel

/// The socket readiness delivery, row by measured row: every claim here is a
/// row of `docs/plans/2026-08-21-socket-readiness-wake.md`'s tables (probes
/// `et.c`/`order2.c`/`order3.c`/`order4.c`, Linux 6.18.5). Guests can see the
/// delivered batches; what they cannot see is the ready list itself — which
/// entries are pending, what consumption removed, where truncation stopped —
/// so those are pinned here.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestSocketEventDelivery =

    let private loopback (port : uint16) : InternetEndpoint =
        InternetEndpoint.ofParts InternetEndpoint.LoopbackAddress port

    let private inetFamily : int option =
        Some SimulatedUnixPlatform.internetAddressFamily

    /// All five interest bits: READ|WRITE|READCLOSE|CLOSE|ERROR.
    let private allInterest : int = 0x1F

    let private addPort (kernel : EmulatedKernel) : int * OpenFileDescriptionId * EmulatedKernel =
        let fd, registry =
            FileDescriptorRegistry.createSocketEventPort kernel.FileDescriptors

        let portId =
            match FileDescriptorRegistry.tryFindId fd registry with
            | Some id -> id
            | None -> failwith "port fd not live"

        fd,
        portId,
        { kernel with
            FileDescriptors = registry
        }

    let private addStream (kernel : EmulatedKernel) : int * SocketId * EmulatedKernel =
        let fd, kernel =
            EmulatedKernel.createSocket SocketDomain.InterNetwork SocketKind.Stream SocketProtocol.Tcp kernel

        let socketId =
            match FileDescriptorRegistry.tryFind fd kernel.FileDescriptors with
            | Some description ->
                match description.Target with
                | OpenFileTarget.Socket socketId -> socketId
                | other -> failwith $"expected a socket target, got %O{other}"
            | None -> failwith "socket fd not live"

        fd, socketId, kernel

    /// A listening stream socket at loopback:`port`, backlog 8, empty queue.
    let private addListener (port : uint16) (kernel : EmulatedKernel) : int * SocketId * EmulatedKernel =
        let fd, socketId, kernel = addStream kernel

        let kernel =
            { kernel with
                Machine =
                    { kernel.Machine with
                        Sockets =
                            kernel.Sockets
                            |> Map.add
                                socketId
                                { EmulatedKernel.socket socketId kernel with
                                    Binding =
                                        Some
                                            {
                                                Endpoint = loopback port
                                                LockedAddress = None
                                            }
                                    Phase =
                                        SocketPhase.Listening
                                            {
                                                Backlog = 8
                                                Queue = []
                                            }
                                }
                    }
            }

        fd, socketId, kernel

    let private register (portFd : int) (targetFd : int) (data : uint64) (kernel : EmulatedKernel) : EmulatedKernel =
        match
            EmulatedKernel.changeSocketEventRegistration
                portFd
                targetFd
                (SocketEventRegistrationChange.Add (SocketEventInterest.ofBits "test" allInterest, data))
                kernel
        with
        | Ok kernel -> kernel
        | Error error -> failwith $"registration failed: %O{error}"

    let private connect
        (client : SocketId)
        (nonBlocking : bool)
        (dest : InternetEndpoint)
        (kernel : EmulatedKernel)
        : EmulatedKernel.ConnectOutcome * EmulatedKernel
        =
        EmulatedKernel.connectSocket client nonBlocking 16 inetFamily (Some dest) kernel

    /// The delivered rows' `Data` fields, so a test can assert order without
    /// restating every mask.
    let private dataOf (rows : (uint64 * ReadinessLevel) list) : uint64 list = rows |> List.map fst

    let private readyOf
        (portId : OpenFileDescriptionId)
        (kernel : EmulatedKernel)
        : (int * OpenFileDescriptionId) list
        =
        match Map.tryFind portId (FileDescriptorRegistry.descriptions kernel.FileDescriptors) with
        | Some description ->
            match description.Target with
            | OpenFileTarget.SocketEventPort portState -> portState.Ready
            | other -> failwith $"not a port: %O{other}"
        | None -> failwith "port description not live"

    let private assertSound (kernel : EmulatedKernel) : unit =
        EmulatedKernel.checkInvariants kernel |> shouldEqual []
        FileDescriptorRegistry.checkInvariants kernel.FileDescriptors |> shouldEqual []

    // --- rows A-E: what an edge is ---

    /// Row A: an edge whose level has gone away by delivery time reports
    /// nothing, and is consumed — epoll re-polls at delivery.
    [<Test>]
    let ``a stale edge delivers nothing and is consumed`` () : unit =
        let portFd, portId, kernel = addPort EmulatedKernel.initial
        let listenerFd, listenerId, kernel = addListener 5000us kernel
        let _, clientId, kernel = addStream kernel
        let kernel = register portFd listenerFd 7UL kernel
        let _, kernel = connect clientId false (loopback 5000us) kernel

        readyOf portId kernel |> List.length |> shouldEqual 1

        let _, _, kernel = EmulatedKernel.acceptConnection listenerId kernel

        let delivered, kernel = EmulatedKernel.deliverSocketEvents portId 8 kernel
        delivered |> shouldEqual []
        readyOf portId kernel |> shouldEqual []
        assertSound kernel

    /// Rows B and D: a live edge reports once and only once (B), and a second
    /// connect onto an already-nonempty, already-reported queue is a fresh
    /// edge that reports again (D).
    [<Test>]
    let ``a live edge reports once, and a further connect re-arms the reported queue`` () : unit =
        let portFd, portId, kernel = addPort EmulatedKernel.initial
        let listenerFd, _, kernel = addListener 5000us kernel
        let _, c1, kernel = addStream kernel
        let _, c2, kernel = addStream kernel
        let kernel = register portFd listenerFd 7UL kernel
        let _, kernel = connect c1 false (loopback 5000us) kernel

        let delivered, kernel = EmulatedKernel.deliverSocketEvents portId 8 kernel

        delivered
        |> shouldEqual
            [
                7UL,
                { ReadinessLevel.none with
                    In = true
                }
            ]

        // B: the level is still high (queue nonempty), and nothing reports.
        let delivered, kernel = EmulatedKernel.deliverSocketEvents portId 8 kernel
        delivered |> shouldEqual []

        // D: the second connect is a fresh signal even though the reported
        // mask never changed.
        let _, kernel = connect c2 false (loopback 5000us) kernel
        let delivered, kernel = EmulatedKernel.deliverSocketEvents portId 8 kernel
        dataOf delivered |> shouldEqual [ 7UL ]
        assertSound kernel

    /// Row C: a drain and a refill entirely between two deliveries reports —
    /// the refill is the edge, whatever the mask did in between.
    [<Test>]
    let ``a drop-then-rise between deliveries reports`` () : unit =
        let portFd, portId, kernel = addPort EmulatedKernel.initial
        let listenerFd, listenerId, kernel = addListener 5000us kernel
        let _, c1, kernel = addStream kernel
        let _, c2, kernel = addStream kernel
        let kernel = register portFd listenerFd 7UL kernel
        let _, kernel = connect c1 false (loopback 5000us) kernel
        let delivered, kernel = EmulatedKernel.deliverSocketEvents portId 8 kernel
        dataOf delivered |> shouldEqual [ 7UL ]

        let _, _, kernel = EmulatedKernel.acceptConnection listenerId kernel
        let _, kernel = connect c2 false (loopback 5000us) kernel

        let delivered, kernel = EmulatedKernel.deliverSocketEvents portId 8 kernel
        dataOf delivered |> shouldEqual [ 7UL ]
        assertSound kernel

    /// Row E: an ADD of an already-ready target is pending from the ADD, with
    /// no signal ever having reached the registration.
    [<Test>]
    let ``an ADD of a ready target is pending immediately`` () : unit =
        let listenerFd, _, kernel = addListener 5000us EmulatedKernel.initial
        let _, c1, kernel = addStream kernel
        let _, kernel = connect c1 false (loopback 5000us) kernel

        // The port did not exist when the edge arrived.
        let portFd, portId, kernel = addPort kernel
        let kernel = register portFd listenerFd 9UL kernel

        EmulatedKernel.hasDeliverableSocketEvents portId kernel |> shouldEqual true
        let delivered, kernel = EmulatedKernel.deliverSocketEvents portId 8 kernel
        dataOf delivered |> shouldEqual [ 9UL ]
        assertSound kernel

    /// The complement of row E: an ADD of a target that is not ready pends
    /// nothing, which is what keeps the no-spurious-wake guests parked.
    [<Test>]
    let ``an ADD of an unready target pends nothing`` () : unit =
        let portFd, portId, kernel = addPort EmulatedKernel.initial
        let listenerFd, _, kernel = addListener 5000us kernel
        let kernel = register portFd listenerFd 9UL kernel

        readyOf portId kernel |> shouldEqual []
        EmulatedKernel.hasDeliverableSocketEvents portId kernel |> shouldEqual false
        assertSound kernel

    // --- rows F-J, R: order ---

    /// Rows F/G: the batch is in edge-arrival order, not fd or registration
    /// order.
    [<Test>]
    let ``the batch is in edge-arrival order`` () : unit =
        for firstIsL1 in [ true ; false ] do
            let portFd, portId, kernel = addPort EmulatedKernel.initial
            let l1Fd, _, kernel = addListener 5001us kernel
            let l2Fd, _, kernel = addListener 5002us kernel
            let _, c1, kernel = addStream kernel
            let _, c2, kernel = addStream kernel
            let kernel = register portFd l1Fd 1UL kernel
            let kernel = register portFd l2Fd 2UL kernel

            let first, second = if firstIsL1 then 5001us, 5002us else 5002us, 5001us
            let _, kernel = connect c1 false (loopback first) kernel
            let _, kernel = connect c2 false (loopback second) kernel

            let delivered, kernel = EmulatedKernel.deliverSocketEvents portId 8 kernel

            dataOf delivered
            |> shouldEqual (if firstIsL1 then [ 1UL ; 2UL ] else [ 2UL ; 1UL ])

            assertSound kernel

    /// Row H: a re-signal of an entry already pending does not move it.
    [<Test>]
    let ``a re-signal does not move a pending entry`` () : unit =
        let portFd, portId, kernel = addPort EmulatedKernel.initial
        let l1Fd, _, kernel = addListener 5001us kernel
        let l2Fd, _, kernel = addListener 5002us kernel
        let _, c1, kernel = addStream kernel
        let _, c2, kernel = addStream kernel
        let _, c3, kernel = addStream kernel
        let kernel = register portFd l1Fd 1UL kernel
        let kernel = register portFd l2Fd 2UL kernel

        let _, kernel = connect c1 false (loopback 5002us) kernel
        let _, kernel = connect c2 false (loopback 5001us) kernel
        let _, kernel = connect c3 false (loopback 5002us) kernel

        let delivered, kernel = EmulatedKernel.deliverSocketEvents portId 8 kernel
        dataOf delivered |> shouldEqual [ 2UL ; 1UL ]
        assertSound kernel

    /// Row I: an ADD-of-ready enters at ADD time, not at its old edge's time.
    [<Test>]
    let ``an ADD of a ready target enters at ADD time`` () : unit =
        let portFd, portId, kernel = addPort EmulatedKernel.initial
        let l1Fd, _, kernel = addListener 5001us kernel
        let l2Fd, _, kernel = addListener 5002us kernel
        let _, c1, kernel = addStream kernel
        let _, c2, kernel = addStream kernel
        let kernel = register portFd l1Fd 1UL kernel

        // l2 becomes ready while unregistered, then l1's edge arrives, then
        // l2 is added: l2 reports at its ADD's place, behind l1.
        let _, kernel = connect c1 false (loopback 5002us) kernel
        let _, kernel = connect c2 false (loopback 5001us) kernel
        let kernel = register portFd l2Fd 2UL kernel

        let delivered, kernel = EmulatedKernel.deliverSocketEvents portId 8 kernel
        dataOf delivered |> shouldEqual [ 1UL ; 2UL ]
        assertSound kernel

    /// Row J: truncation delivers the prefix, the suffix stays pending in
    /// order, and a drained port reports nothing further.
    [<Test>]
    let ``truncation keeps the suffix pending in order`` () : unit =
        let portFd, portId, kernel = addPort EmulatedKernel.initial
        let l1Fd, _, kernel = addListener 5001us kernel
        let l2Fd, _, kernel = addListener 5002us kernel
        let l3Fd, _, kernel = addListener 5003us kernel
        let _, c1, kernel = addStream kernel
        let _, c2, kernel = addStream kernel
        let _, c3, kernel = addStream kernel
        let kernel = register portFd l1Fd 1UL kernel
        let kernel = register portFd l2Fd 2UL kernel
        let kernel = register portFd l3Fd 3UL kernel
        let _, kernel = connect c1 false (loopback 5001us) kernel
        let _, kernel = connect c2 false (loopback 5002us) kernel
        let _, kernel = connect c3 false (loopback 5003us) kernel

        let delivered, kernel = EmulatedKernel.deliverSocketEvents portId 2 kernel
        dataOf delivered |> shouldEqual [ 1UL ; 2UL ]
        readyOf portId kernel |> List.length |> shouldEqual 1

        let delivered, kernel = EmulatedKernel.deliverSocketEvents portId 8 kernel
        dataOf delivered |> shouldEqual [ 3UL ]

        let delivered, kernel = EmulatedKernel.deliverSocketEvents portId 8 kernel
        delivered |> shouldEqual []
        assertSound kernel

    /// Row R: one signal reaching two registrations of the same socket (a
    /// `dup`) delivers them newest-registered first, whichever fd is which.
    [<Test>]
    let ``same-signal ties deliver newest-registered first`` () : unit =
        for originalFirst in [ true ; false ] do
            let portFd, portId, kernel = addPort EmulatedKernel.initial
            let listenerFd, _, kernel = addListener 5000us kernel
            let _, c1, kernel = addStream kernel

            let dupFd, kernel =
                match FileDescriptorRegistry.dup listenerFd kernel.FileDescriptors with
                | Ok (fd, registry) ->
                    fd,
                    { kernel with
                        FileDescriptors = registry
                    }
                | Error error -> failwith $"dup failed: %O{error}"

            let kernel =
                if originalFirst then
                    kernel |> register portFd listenerFd 1UL |> register portFd dupFd 2UL
                else
                    kernel |> register portFd dupFd 2UL |> register portFd listenerFd 1UL

            let _, kernel = connect c1 false (loopback 5000us) kernel

            let delivered, kernel = EmulatedKernel.deliverSocketEvents portId 8 kernel

            dataOf delivered
            |> shouldEqual (if originalFirst then [ 2UL ; 1UL ] else [ 1UL ; 2UL ])

            assertSound kernel

    // --- rows K-L: MOD ---

    /// Row K: a MOD of a consumed, still-ready target re-arms it.
    [<Test>]
    let ``MOD of a consumed ready target re-arms`` () : unit =
        let portFd, portId, kernel = addPort EmulatedKernel.initial
        let listenerFd, _, kernel = addListener 5000us kernel
        let _, c1, kernel = addStream kernel
        let kernel = register portFd listenerFd 7UL kernel
        let _, kernel = connect c1 false (loopback 5000us) kernel
        let delivered, kernel = EmulatedKernel.deliverSocketEvents portId 8 kernel
        dataOf delivered |> shouldEqual [ 7UL ]

        let kernel =
            match
                EmulatedKernel.changeSocketEventRegistration
                    portFd
                    listenerFd
                    (SocketEventRegistrationChange.Modify (SocketEventInterest.ofBits "test" allInterest, 7UL))
                    kernel
            with
            | Ok kernel -> kernel
            | Error error -> failwith $"modify failed: %O{error}"

        let delivered, kernel = EmulatedKernel.deliverSocketEvents portId 8 kernel
        dataOf delivered |> shouldEqual [ 7UL ]
        assertSound kernel

    /// Row L: a MOD of an entry already pending leaves its place alone.
    [<Test>]
    let ``MOD of a pending entry does not move it`` () : unit =
        let portFd, portId, kernel = addPort EmulatedKernel.initial
        let l1Fd, _, kernel = addListener 5001us kernel
        let l2Fd, _, kernel = addListener 5002us kernel
        let _, c1, kernel = addStream kernel
        let _, c2, kernel = addStream kernel
        let kernel = register portFd l1Fd 1UL kernel
        let kernel = register portFd l2Fd 2UL kernel
        let _, kernel = connect c1 false (loopback 5002us) kernel
        let _, kernel = connect c2 false (loopback 5001us) kernel

        let kernel =
            match
                EmulatedKernel.changeSocketEventRegistration
                    portFd
                    l2Fd
                    (SocketEventRegistrationChange.Modify (SocketEventInterest.ofBits "test" allInterest, 2UL))
                    kernel
            with
            | Ok kernel -> kernel
            | Error error -> failwith $"modify failed: %O{error}"

        let delivered, kernel = EmulatedKernel.deliverSocketEvents portId 8 kernel
        dataOf delivered |> shouldEqual [ 2UL ; 1UL ]
        assertSound kernel

    // --- row M and interest filtering: the refusal lifecycle ---

    /// Row M: the error's arrival signals, delivery reports the full refusal
    /// level restricted by interest, and the delivering connect's reset
    /// signals again with the idle level.
    [<Test>]
    let ``a refusal delivers its error level once, and the reset re-signals`` () : unit =
        let portFd, portId, kernel = addPort EmulatedKernel.initial
        let clientFd, clientId, kernel = addStream kernel
        let kernel = register portFd clientFd 5UL kernel

        // Consume the idle OUT|HUP edge the ADD-of-ready queued.
        let delivered, kernel = EmulatedKernel.deliverSocketEvents portId 8 kernel

        delivered
        |> shouldEqual
            [
                5UL,
                { ReadinessLevel.none with
                    Out = true
                    Hup = true
                }
            ]

        // Nothing listens at 5999: the refusal latches and signals.
        let outcome, kernel = connect clientId true (loopback 5999us) kernel

        outcome
        |> shouldEqual (EmulatedKernel.ConnectOutcome.Failed UnixError.EINPROGRESS)

        let delivered, kernel = EmulatedKernel.deliverSocketEvents portId 8 kernel

        delivered
        |> shouldEqual
            [
                5UL,
                {
                    In = true
                    Out = true
                    RdHup = true
                    Hup = true
                    Err = true
                }
            ]

        let delivered, kernel = EmulatedKernel.deliverSocketEvents portId 8 kernel
        delivered |> shouldEqual []

        // The delivering connect resets the socket, and the reset signals.
        let outcome, kernel = connect clientId true (loopback 5999us) kernel

        outcome
        |> shouldEqual (EmulatedKernel.ConnectOutcome.Failed UnixError.ECONNREFUSED)

        let delivered, kernel = EmulatedKernel.deliverSocketEvents portId 8 kernel

        delivered
        |> shouldEqual
            [
                5UL,
                { ReadinessLevel.none with
                    Out = true
                    Hup = true
                }
            ]

        assertSound kernel

    // --- registration bookkeeping ---

    /// A DEL takes the pending entry with the registration, so a later
    /// delivery cannot report from a key the table no longer holds.
    [<Test>]
    let ``removing a registration removes its pending entry`` () : unit =
        let portFd, portId, kernel = addPort EmulatedKernel.initial
        let listenerFd, _, kernel = addListener 5000us kernel
        let _, c1, kernel = addStream kernel
        let kernel = register portFd listenerFd 7UL kernel
        let _, kernel = connect c1 false (loopback 5000us) kernel
        readyOf portId kernel |> List.length |> shouldEqual 1

        let kernel =
            match
                EmulatedKernel.changeSocketEventRegistration
                    portFd
                    listenerFd
                    SocketEventRegistrationChange.Remove
                    kernel
            with
            | Ok kernel -> kernel
            | Error error -> failwith $"remove failed: %O{error}"

        readyOf portId kernel |> shouldEqual []
        EmulatedKernel.hasDeliverableSocketEvents portId kernel |> shouldEqual false
        assertSound kernel

    /// Closing the registered target's last descriptor sweeps its pending
    /// entry with its registration (`eventpoll_release`).
    [<Test>]
    let ``closing the registered target sweeps its pending entry`` () : unit =
        let portFd, portId, kernel = addPort EmulatedKernel.initial
        let listenerFd, _, kernel = addListener 5000us kernel
        let clientFd, c1, kernel = addStream kernel
        let kernel = register portFd listenerFd 7UL kernel
        let _, kernel = connect c1 false (loopback 5000us) kernel
        readyOf portId kernel |> List.length |> shouldEqual 1

        // The client goes first: a listener close over a live queued client
        // refuses (the RST would leave the client unmeasurable), and the
        // queued connection survives its client, so the pending entry is
        // still there to sweep.
        let kernel =
            match EmulatedKernel.closeFd clientFd kernel with
            | Ok kernel -> kernel
            | Error error -> failwith $"close failed: %O{error}"

        readyOf portId kernel |> List.length |> shouldEqual 1

        let kernel =
            match EmulatedKernel.closeFd listenerFd kernel with
            | Ok kernel -> kernel
            | Error error -> failwith $"close failed: %O{error}"

        readyOf portId kernel |> shouldEqual []
        EmulatedKernel.hasDeliverableSocketEvents portId kernel |> shouldEqual false
        assertSound kernel

    /// A failed ADD (EEXIST here) leaves the ordinal counter exactly as it
    /// found it: a failed `epoll_ctl` changes no kernel state.
    [<Test>]
    let ``a failed ADD does not consume an ordinal`` () : unit =
        let portFd, _, kernel = addPort EmulatedKernel.initial
        let listenerFd, _, kernel = addListener 5000us kernel
        let kernel = register portFd listenerFd 7UL kernel
        let before = kernel.NextSocketEventRegistrationOrdinal

        match
            EmulatedKernel.changeSocketEventRegistration
                portFd
                listenerFd
                (SocketEventRegistrationChange.Add (SocketEventInterest.ofBits "test" allInterest, 8UL))
                kernel
        with
        | Ok _ -> failwith "expected EEXIST"
        | Error error -> error |> shouldEqual SocketEventRegistrationError.AlreadyRegistered

        before |> shouldEqual kernel.NextSocketEventRegistrationOrdinal

    // --- the peer-close edge ---

    /// The peer's FIN: closing the peer of a registered established socket
    /// signals the survivor, whose level becomes the measured half-closed
    /// IN|OUT|RDHUP (`order3.c` row Q).
    [<Test>]
    let ``closing the peer signals the registered survivor with the half-closed level`` () : unit =
        let portFd, portId, kernel = addPort EmulatedKernel.initial
        let _, listenerId, kernel = addListener 5000us kernel
        let clientFd, clientId, kernel = addStream kernel
        let _, kernel = connect clientId false (loopback 5000us) kernel
        let serverFd, _, kernel = EmulatedKernel.acceptConnection listenerId kernel
        let kernel = register portFd clientFd 5UL kernel

        // Consume the ADD-of-ready edge (established, live peer: OUT).
        let delivered, kernel = EmulatedKernel.deliverSocketEvents portId 8 kernel

        delivered
        |> shouldEqual
            [
                5UL,
                { ReadinessLevel.none with
                    Out = true
                }
            ]

        let kernel =
            match EmulatedKernel.closeFd serverFd kernel with
            | Ok kernel -> kernel
            | Error error -> failwith $"close failed: %O{error}"

        let delivered, kernel = EmulatedKernel.deliverSocketEvents portId 8 kernel

        delivered
        |> shouldEqual
            [
                5UL,
                { ReadinessLevel.none with
                    In = true
                    Out = true
                    RdHup = true
                }
            ]

        assertSound kernel

    /// The FIN is a *state-change* wake, unkeyed (measured, `order8.c`): it
    /// pends even a registration whose CLOSE|ERROR-only interest the
    /// half-closed level misses, the entry keeps the FIN's position through
    /// a later interest change, and delivery's re-poll is what filters — so
    /// nothing is deliverable until the interest widens, and once it does
    /// the entry delivers ahead of newer edges.
    [<Test>]
    let ``a peer close pends a CLOSE-and-ERROR-only interest, which delivery filters until widened`` () : unit =
        let portFd, portId, kernel = addPort EmulatedKernel.initial
        let _, listenerId, kernel = addListener 5000us kernel
        let clientFd, clientId, kernel = addStream kernel
        let _, kernel = connect clientId false (loopback 5000us) kernel
        let serverFd, _, kernel = EmulatedKernel.acceptConnection listenerId kernel

        let kernel =
            match
                EmulatedKernel.changeSocketEventRegistration
                    portFd
                    clientFd
                    (SocketEventRegistrationChange.Add (SocketEventInterest.ofBits "test" 0x18, 5UL))
                    kernel
            with
            | Ok kernel -> kernel
            | Error error -> failwith $"registration failed: %O{error}"

        readyOf portId kernel |> shouldEqual []

        let kernel =
            match EmulatedKernel.closeFd serverFd kernel with
            | Ok kernel -> kernel
            | Error error -> failwith $"close failed: %O{error}"

        // Pending — the unkeyed wake queued it — but not deliverable: the
        // re-poll reports nothing under CLOSE|ERROR against a level with
        // neither.
        readyOf portId kernel |> List.length |> shouldEqual 1
        EmulatedKernel.hasDeliverableSocketEvents portId kernel |> shouldEqual false

        // A newer edge elsewhere, then the widening MOD: the FIN's entry
        // keeps its earlier position and delivers first (`order8.c`).
        let listener2Fd, _, kernel = addListener 5001us kernel
        let _, c2, kernel = addStream kernel
        let kernel = register portFd listener2Fd 9UL kernel
        let _, kernel = connect c2 false (loopback 5001us) kernel

        let kernel =
            match
                EmulatedKernel.changeSocketEventRegistration
                    portFd
                    clientFd
                    (SocketEventRegistrationChange.Modify (SocketEventInterest.ofBits "test" allInterest, 5UL))
                    kernel
            with
            | Ok kernel -> kernel
            | Error error -> failwith $"modify failed: %O{error}"

        let delivered, kernel = EmulatedKernel.deliverSocketEvents portId 8 kernel
        dataOf delivered |> shouldEqual [ 5UL ; 9UL ]
        assertSound kernel

    /// An unregistered peer close proceeds, and a later ADD of the survivor
    /// finds it ready at the half-closed level.
    [<Test>]
    let ``registering a survivor after an unwatched peer close pends the half-closed level`` () : unit =
        let portFd, portId, kernel = addPort EmulatedKernel.initial
        let _, listenerId, kernel = addListener 5000us kernel
        let clientFd, clientId, kernel = addStream kernel
        let _, kernel = connect clientId false (loopback 5000us) kernel
        let serverFd, _, kernel = EmulatedKernel.acceptConnection listenerId kernel

        let kernel =
            match EmulatedKernel.closeFd serverFd kernel with
            | Ok kernel -> kernel
            | Error error -> failwith $"close failed: %O{error}"

        assertSound kernel

        let kernel = register portFd clientFd 5UL kernel
        let delivered, kernel = EmulatedKernel.deliverSocketEvents portId 8 kernel

        delivered
        |> shouldEqual
            [
                5UL,
                { ReadinessLevel.none with
                    In = true
                    Out = true
                    RdHup = true
                }
            ]

        assertSound kernel

    /// A dying listener RSTs its unaccepted queue entries' clients, leaving
    /// them in an unmeasured state a later registration could not answer for
    /// — so the close refuses whenever a live client would be left behind,
    /// registered or not (an unregistered survivor would otherwise be
    /// indistinguishable from a cleanly FIN'd peer at its next ADD).
    [<Test>]
    let ``closing a listener with a live queued client refuses`` () : unit =
        let listenerFd, _, kernel = addListener 5000us EmulatedKernel.initial
        let _, clientId, kernel = addStream kernel
        let _, kernel = connect clientId false (loopback 5000us) kernel

        let exc =
            Assert.Throws<System.Exception> (fun () -> EmulatedKernel.closeFd listenerFd kernel |> ignore)

        exc.Message |> shouldContainText "RSTs the unaccepted client"

    /// The connect's two edges enter in the measured order (`order7.c`): the
    /// client's completion before the listener's accept edge.
    [<Test>]
    let ``a connect's edges enter client-first`` () : unit =
        let portFd, portId, kernel = addPort EmulatedKernel.initial
        let listenerFd, _, kernel = addListener 5000us kernel
        let clientFd, clientId, kernel = addStream kernel
        let kernel = register portFd listenerFd 2UL kernel
        let kernel = register portFd clientFd 1UL kernel

        // Consume the client's idle ADD-of-ready edge so only the connect's
        // pair remains.
        let delivered, kernel = EmulatedKernel.deliverSocketEvents portId 8 kernel
        dataOf delivered |> shouldEqual [ 1UL ]

        let _, kernel = connect clientId false (loopback 5000us) kernel

        let delivered, kernel = EmulatedKernel.deliverSocketEvents portId 8 kernel
        dataOf delivered |> shouldEqual [ 1UL ; 2UL ]
        assertSound kernel

    /// The completion of a connect signals the client it resolved on
    /// (`order3.c` row N's "completion arrived" edge): a pre-registered,
    /// edge-consumed client re-reports once established.
    [<Test>]
    let ``a connect's completion signals the registered client`` () : unit =
        let portFd, portId, kernel = addPort EmulatedKernel.initial
        let _, _, kernel = addListener 5000us kernel
        let clientFd, clientId, kernel = addStream kernel
        let kernel = register portFd clientFd 6UL kernel

        // Consume the idle OUT|HUP edge the ADD-of-ready queued.
        let delivered, kernel = EmulatedKernel.deliverSocketEvents portId 8 kernel
        dataOf delivered |> shouldEqual [ 6UL ]

        let outcome, kernel = connect clientId false (loopback 5000us) kernel
        outcome |> shouldEqual EmulatedKernel.ConnectOutcome.Completed

        let delivered, kernel = EmulatedKernel.deliverSocketEvents portId 8 kernel

        delivered
        |> shouldEqual
            [
                6UL,
                { ReadinessLevel.none with
                    Out = true
                }
            ]

        assertSound kernel

    /// A *blocking* connect's refusal delivers inline, and the collapsed
    /// arrival-plus-reset is still a signal: the registered client
    /// re-reports its post-reset idle level.
    [<Test>]
    let ``an inline refusal signals the registered client`` () : unit =
        let portFd, portId, kernel = addPort EmulatedKernel.initial
        let clientFd, clientId, kernel = addStream kernel
        let kernel = register portFd clientFd 6UL kernel
        let delivered, kernel = EmulatedKernel.deliverSocketEvents portId 8 kernel
        dataOf delivered |> shouldEqual [ 6UL ]

        let outcome, kernel = connect clientId false (loopback 5999us) kernel

        outcome
        |> shouldEqual (EmulatedKernel.ConnectOutcome.Failed UnixError.ECONNREFUSED)

        let delivered, kernel = EmulatedKernel.deliverSocketEvents portId 8 kernel

        delivered
        |> shouldEqual
            [
                6UL,
                { ReadinessLevel.none with
                    Out = true
                    Hup = true
                }
            ]

        assertSound kernel

    /// A signal that misses a registration's interest never queues it
    /// (measured, `order6.c`: an IN edge at a WRITE-only registration leaves
    /// no trace), and a later MOD to an interest the level meets enqueues
    /// fresh at MOD time — behind everything queued since the missed edge.
    [<Test>]
    let ``a signal missing the interest leaves no trace, and a later MOD enqueues fresh`` () : unit =
        let portFd, portId, kernel = addPort EmulatedKernel.initial
        let l1Fd, _, kernel = addListener 5001us kernel
        let l2Fd, _, kernel = addListener 5002us kernel
        let _, c1, kernel = addStream kernel
        let _, c2, kernel = addStream kernel

        // l1 watches WRITE alone: a queued connection raises only IN, and a
        // listener reports no ERR or HUP.
        let kernel =
            match
                EmulatedKernel.changeSocketEventRegistration
                    portFd
                    l1Fd
                    (SocketEventRegistrationChange.Add (SocketEventInterest.ofBits "test" 0x2, 1UL))
                    kernel
            with
            | Ok kernel -> kernel
            | Error error -> failwith $"registration failed: %O{error}"

        let kernel = register portFd l2Fd 2UL kernel

        let _, kernel = connect c1 false (loopback 5001us) kernel
        readyOf portId kernel |> shouldEqual []
        EmulatedKernel.hasDeliverableSocketEvents portId kernel |> shouldEqual false

        let _, kernel = connect c2 false (loopback 5002us) kernel

        let kernel =
            match
                EmulatedKernel.changeSocketEventRegistration
                    portFd
                    l1Fd
                    (SocketEventRegistrationChange.Modify (SocketEventInterest.ofBits "test" allInterest, 1UL))
                    kernel
            with
            | Ok kernel -> kernel
            | Error error -> failwith $"modify failed: %O{error}"

        let delivered, kernel = EmulatedKernel.deliverSocketEvents portId 8 kernel
        dataOf delivered |> shouldEqual [ 2UL ; 1UL ]
        assertSound kernel

    /// The walk re-applies the interest at delivery: an entry queued under a
    /// wide interest and then narrowed while pending keeps its place (row L)
    /// but reports nothing, and is consumed silently.
    [<Test>]
    let ``an interest narrowed while pending is dropped at delivery`` () : unit =
        let portFd, portId, kernel = addPort EmulatedKernel.initial
        let listenerFd, _, kernel = addListener 5000us kernel
        let _, c1, kernel = addStream kernel
        let kernel = register portFd listenerFd 6UL kernel
        let _, kernel = connect c1 false (loopback 5000us) kernel
        readyOf portId kernel |> List.length |> shouldEqual 1

        let kernel =
            match
                EmulatedKernel.changeSocketEventRegistration
                    portFd
                    listenerFd
                    (SocketEventRegistrationChange.Modify (SocketEventInterest.ofBits "test" 0x2, 6UL))
                    kernel
            with
            | Ok kernel -> kernel
            | Error error -> failwith $"modify failed: %O{error}"

        readyOf portId kernel |> List.length |> shouldEqual 1
        EmulatedKernel.hasDeliverableSocketEvents portId kernel |> shouldEqual false

        let delivered, kernel = EmulatedKernel.deliverSocketEvents portId 8 kernel
        delivered |> shouldEqual []
        readyOf portId kernel |> shouldEqual []
        assertSound kernel

    /// Row S: a MOD before the tie-making edge does not move the entry's
    /// place — `RegisteredAt` survives the MOD.
    [<Test>]
    let ``MOD does not move a registration's place in a same-signal tie`` () : unit =
        let portFd, portId, kernel = addPort EmulatedKernel.initial
        let listenerFd, _, kernel = addListener 5000us kernel
        let _, c1, kernel = addStream kernel

        let dupFd, kernel =
            match FileDescriptorRegistry.dup listenerFd kernel.FileDescriptors with
            | Ok (fd, registry) ->
                fd,
                { kernel with
                    FileDescriptors = registry
                }
            | Error error -> failwith $"dup failed: %O{error}"

        let kernel = kernel |> register portFd listenerFd 1UL |> register portFd dupFd 2UL

        // Consume the two entries the not-ready listener never queued —
        // nothing pends yet, so nothing to consume; MOD the older entry.
        let kernel =
            match
                EmulatedKernel.changeSocketEventRegistration
                    portFd
                    listenerFd
                    (SocketEventRegistrationChange.Modify (SocketEventInterest.ofBits "test" allInterest, 1UL))
                    kernel
            with
            | Ok kernel -> kernel
            | Error error -> failwith $"modify failed: %O{error}"

        let _, kernel = connect c1 false (loopback 5000us) kernel

        let delivered, kernel = EmulatedKernel.deliverSocketEvents portId 8 kernel
        dataOf delivered |> shouldEqual [ 2UL ; 1UL ]
        assertSound kernel

    /// The close-time retention rule, flavour by flavour (both measured, see
    /// `SocketEventWaitSurvivesCloseLinux.cs`): under Linux a dup-survived
    /// close of an in-flight-waited port proceeds — the wait holds the
    /// description — and only destroying the description refuses; under
    /// Darwin, where kevent ends the wait with an unmeasured error, any such
    /// close refuses.
    [<Test>]
    let ``closing a descriptor of an in-flight-waited port follows the measured flavour split`` () : unit =
        let build () =
            let portFd, portId, kernel = addPort EmulatedKernel.initial

            let dupFd, kernel =
                match FileDescriptorRegistry.dup portFd kernel.FileDescriptors with
                | Ok (fd, registry) ->
                    fd,
                    { kernel with
                        FileDescriptors = registry
                    }
                | Error error -> failwith $"dup failed: %O{error}"

            let kernel =
                { kernel with
                    ParkedSocketWaits =
                        Map.ofList
                            [
                                ThreadId 1,
                                {
                                    Port = portId
                                    MaxEvents = 8
                                }
                            ]
                }

            portFd, dupFd, kernel

        // Linux: the dup-survived close proceeds...
        let portFd, dupFd, kernel = build ()

        let kernel =
            match EmulatedKernel.closeFd dupFd kernel with
            | Ok kernel -> kernel
            | Error error -> failwith $"close failed: %O{error}"

        // ...and destroying the description refuses.
        let exc =
            Assert.Throws<System.Exception> (fun () -> EmulatedKernel.closeFd portFd kernel |> ignore)

        exc.Message |> shouldContainText "Implement port retention"

        // Darwin: even the dup-survived close refuses.
        let _, dupFd, kernel = build ()

        let kernel =
            { kernel with
                Machine =
                    { kernel.Machine with
                        UnixPlatform = SimulatedUnixPlatform.macOsArm64
                    }
            }

        let exc =
            Assert.Throws<System.Exception> (fun () -> EmulatedKernel.closeFd dupFd kernel |> ignore)

        exc.Message |> shouldContainText "closing a kqueue out from under a waiter"

    // --- forged invariants ---

    /// `checkInvariants` rejects an ordinal at or above the counter, and a
    /// duplicated ordinal.
    [<Test>]
    let ``checkInvariants rejects stale and duplicated registration ordinals`` () : unit =
        let portFd, portId, kernel = addPort EmulatedKernel.initial
        let l1Fd, _, kernel = addListener 5001us kernel
        let l2Fd, _, kernel = addListener 5002us kernel
        let kernel = register portFd l1Fd 1UL kernel
        let kernel = register portFd l2Fd 2UL kernel
        assertSound kernel

        let withOrdinals (first : int64) (second : int64) (counter : int64) : EmulatedKernel =
            let descriptions = FileDescriptorRegistry.descriptions kernel.FileDescriptors

            let portState =
                match (Map.find portId descriptions).Target with
                | OpenFileTarget.SocketEventPort portState -> portState
                | other -> failwith $"not a port: %O{other}"

            let ordinals = [ first ; second ]

            let rewritten =
                portState.Registrations
                |> Map.toList
                |> List.sortBy (fun (_, registration) -> registration.RegisteredAt)
                |> List.mapi (fun i (key, registration) ->
                    key,
                    { registration with
                        RegisteredAt = ordinals.[i]
                    }
                )
                |> Map.ofList

            { kernel with
                FileDescriptors =
                    FileDescriptorRegistry.Unchecked.mapDescription
                        portId
                        (fun description ->
                            { description with
                                Target =
                                    OpenFileTarget.SocketEventPort
                                        { portState with
                                            Registrations = rewritten
                                        }
                            }
                        )
                        kernel.FileDescriptors
                Machine =
                    { kernel.Machine with
                        NextSocketEventRegistrationOrdinal = counter
                    }
            }

        EmulatedKernel.checkInvariants (withOrdinals 0L 5L 2L)
        |> shouldEqual [ EmulatedKernelDefect.SocketEventRegistrationOrdinalNotFresh (2L, portId, 5L) ]

        EmulatedKernel.checkInvariants (withOrdinals 0L 0L 2L)
        |> shouldEqual [ EmulatedKernelDefect.DuplicateSocketEventRegistrationOrdinal 0L ]

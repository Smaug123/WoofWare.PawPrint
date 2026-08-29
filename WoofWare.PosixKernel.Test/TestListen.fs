namespace WoofWare.PosixKernel.Test

open System.Collections.Immutable
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `UnixSystem.listen`.
///
/// It takes no buffer and has no shim screens of its own, so unlike `bind` there
/// is nothing here that belongs to a caller: every row is `listen(2)`. The two
/// that only this tier can reach are the re-screen, which one flavour performs
/// and the other does not, and the implicit bind's exhaustion.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestListen =

    let private context : string = "TestListen"

    let private epoch : UnixTimestamp = UnixTimestamp.ofMillisecondsSinceEpoch 0L

    /// A simulated process on the flavour asked for, before anything has
    /// happened to it.
    let private systemOn (platform : SimulatedUnixPlatform) : UnixSystem<int, string> = UnixSystem.initial platform


    let private platforms : SimulatedUnixPlatform list =
        [ SimulatedUnixPlatform.linuxX64 ; SimulatedUnixPlatform.macOsArm64 ]

    let private loopback (port : uint16) : InternetEndpoint =
        InternetEndpoint.ofParts InternetEndpoint.LoopbackAddress port

    let private socketWith
        (kind : SocketKind)
        (binding : SocketBinding option)
        (reuse : bool)
        (phase : SocketPhase)
        : SocketDescription
        =
        {
            Domain = SocketDomain.InterNetwork
            Kind = kind
            Protocol =
                if kind = SocketKind.Stream then
                    SocketProtocol.Tcp
                else
                    SocketProtocol.Udp
            Binding = binding
            ReuseAddress = reuse
            Phase = phase
        }

    let private withSocket
        (socketId : SocketId)
        (socket : SocketDescription)
        (system : UnixSystem<int, string>)
        : int * UnixSystem<int, string>
        =
        let fd, registry =
            FileDescriptorRegistry.createSocket socketId system.Process.FileDescriptors

        let (SocketId raw) = socketId
        let (SocketId next) = system.Machine.NextSocketId

        fd,
        { system with
            Machine =
                { system.Machine with
                    Sockets = Map.add socketId socket system.Machine.Sockets
                    NextSocketId = SocketId (max next (raw + 1L))
                }
            Process =
                { system.Process with
                    FileDescriptors = registry
                }
        }

    let private boundAt (endpoint : InternetEndpoint) : SocketBinding option =
        Some
            {
                Endpoint = endpoint
                LockedAddress = Some endpoint.Address
            }

    let private listenOrFail (fd : int) (backlog : int) (system : UnixSystem<int, string>) =
        match UnixSystem.listen fd backlog system with
        | Ok result -> result
        | Error refusal -> failwith $"expected an answer, got a refusal: %s{ListenRefusal.describe refusal}"

    let private phaseOf (socketId : SocketId) (system : UnixSystem<int, string>) : SocketPhase =
        (UnixMachineState.socket socketId system.Machine).Phase

    // ------------------------------------------------------------------
    // The happy path
    // ------------------------------------------------------------------

    [<TestCaseSource(nameof platforms)>]
    let ``a bound socket listens where it is bound`` (platform : SimulatedUnixPlatform) : unit =
        let fd, system =
            withSocket
                (SocketId 0L)
                (socketWith SocketKind.Stream (boundAt (loopback 5000us)) false SocketPhase.Idle)
                (systemOn platform)

        let answer, system = listenOrFail fd 8 system
        answer |> shouldEqual (ListenAnswer.Listening (loopback 5000us))

        match phaseOf (SocketId 0L) system with
        | SocketPhase.Listening listenState ->
            listenState.Backlog |> shouldEqual 8
            listenState.Queue |> shouldEqual []
        | other -> failwith $"expected Listening, got %A{other}"

    /// `listen(2)` on an unbound socket binds it to the wildcard and an
    /// ephemeral port. Measured on both — and note it does *not* go through
    /// `bind(2)`, so nothing is locked and no `SO_REUSEADDR` is set, both of
    /// which a later bind can see.
    [<TestCaseSource(nameof platforms)>]
    let ``an unbound socket is bound to the wildcard`` (platform : SimulatedUnixPlatform) : unit =
        let fd, system =
            withSocket (SocketId 0L) (socketWith SocketKind.Stream None false SocketPhase.Idle) (systemOn platform)

        let answer, system = listenOrFail fd 8 system

        let where =
            match answer with
            | ListenAnswer.Listening where -> where
            | ListenAnswer.Failed error -> failwith $"expected a listen, got %O{error}"

        where.Address |> shouldEqual InternetEndpoint.WildcardAddress

        let low, high = system.Machine.EphemeralPortRange
        (where.Port >= low && where.Port <= high) |> shouldEqual true

        let socket = UnixMachineState.socket (SocketId 0L) system.Machine

        socket.Binding
        |> shouldEqual (
            Some
                {
                    Endpoint = where
                    LockedAddress = None
                }
        )

        socket.ReuseAddress |> shouldEqual false

    /// Every backlog is accepted — measured, 0, -1 and INT_MAX all succeed on
    /// both — and the number is stored verbatim, because the accept-queue
    /// capacity a later connect enforces is derived from it per flavour.
    [<TestCaseSource(nameof platforms)>]
    let ``every backlog is accepted and stored verbatim`` (platform : SimulatedUnixPlatform) : unit =
        for backlog in [ 0 ; -1 ; System.Int32.MaxValue ; System.Int32.MinValue ] do
            let fd, system =
                withSocket
                    (SocketId 0L)
                    (socketWith SocketKind.Stream (boundAt (loopback 5000us)) false SocketPhase.Idle)
                    (systemOn platform)

            let _, system = listenOrFail fd backlog system

            match phaseOf (SocketId 0L) system with
            | SocketPhase.Listening listenState -> listenState.Backlog |> shouldEqual backlog
            | other -> failwith $"expected Listening, got %A{other}"

    /// A re-listen keeps the queue and updates the backlog, which is Linux's
    /// documented behaviour: `sk_max_ack_backlog` is simply re-assigned.
    [<TestCaseSource(nameof platforms)>]
    let ``a re-listen keeps the queue and updates the backlog`` (platform : SimulatedUnixPlatform) : unit =
        let queued = [ ConnectionId 7L ]

        let fd, system =
            withSocket
                (SocketId 0L)
                (socketWith
                    SocketKind.Stream
                    (boundAt (loopback 5000us))
                    false
                    (SocketPhase.Listening
                        {
                            Backlog = 8
                            Queue = queued
                        }))
                (systemOn platform)

        let _, system = listenOrFail fd 64 system

        match phaseOf (SocketId 0L) system with
        | SocketPhase.Listening listenState ->
            listenState.Backlog |> shouldEqual 64
            listenState.Queue |> shouldEqual queued
        | other -> failwith $"expected Listening, got %A{other}"

    // ------------------------------------------------------------------
    // Errnos
    // ------------------------------------------------------------------

    [<TestCaseSource(nameof platforms)>]
    let ``a descriptor that is not open is EBADF`` (platform : SimulatedUnixPlatform) : unit =
        listenOrFail 99 8 (systemOn platform)
        |> fst
        |> shouldEqual (ListenAnswer.Failed UnixError.EBADF)

    [<TestCaseSource(nameof platforms)>]
    let ``a descriptor that is not a socket is ENOTSOCK`` (platform : SimulatedUnixPlatform) : unit =
        let system = systemOn platform

        let fileFd, registry =
            FileDescriptorRegistry.openFile (InodeNumber 1L) FileAccessMode.ReadOnly system.Process.FileDescriptors

        let portFd, registry = FileDescriptorRegistry.createSocketEventPort registry

        let system =
            { system with
                Process =
                    { system.Process with
                        FileDescriptors = registry
                    }
            }

        for fd in [ 0 ; fileFd ; portFd ] do
            listenOrFail fd 8 system
            |> fst
            |> shouldEqual (ListenAnswer.Failed UnixError.ENOTSOCK)

    [<TestCaseSource(nameof platforms)>]
    let ``a datagram socket is EOPNOTSUPP`` (platform : SimulatedUnixPlatform) : unit =
        let fd, system =
            withSocket
                (SocketId 0L)
                (socketWith SocketKind.Datagram (boundAt (loopback 5000us)) false SocketPhase.Idle)
                (systemOn platform)

        listenOrFail fd 8 system
        |> fst
        |> shouldEqual (ListenAnswer.Failed UnixError.EOPNOTSUPP)

    // ------------------------------------------------------------------
    // The re-screen, which is the flavour split
    // ------------------------------------------------------------------

    /// On Linux two sockets carrying `SO_REUSEADDR` may share an endpoint right
    /// up until one of them listens, and the second `listen(2)` is then
    /// EADDRINUSE — `inet_csk_listen_start` calls `get_port` a second time.
    /// Darwin's `tcp_usr_listen` asks nothing of a socket that already has a
    /// port, so the same pair listens happily.
    [<Test>]
    let ``the binding re-screen is one flavour's only`` () : unit =
        let rows =
            [
                SimulatedUnixPlatform.linuxX64, Some UnixError.EADDRINUSE
                SimulatedUnixPlatform.macOsArm64, None
            ]

        for platform, expected in rows do
            // Two reuse-carrying sockets at the same endpoint, which `bind(2)`
            // admitted on both flavours — and the *first* has already listened.
            // That last part is the whole scenario: Linux's relaxation holds
            // only while nothing listens, so a pair that both merely bound does
            // not conflict, and it is the second `listen` that finds one.
            let _, system =
                withSocket
                    (SocketId 0L)
                    (socketWith
                        SocketKind.Stream
                        (boundAt (loopback 5000us))
                        true
                        (SocketPhase.Listening
                            {
                                Backlog = 8
                                Queue = []
                            }))
                    (systemOn platform)

            let second, system =
                withSocket
                    (SocketId 1L)
                    (socketWith SocketKind.Stream (boundAt (loopback 5000us)) true SocketPhase.Idle)
                    system

            match listenOrFail second 8 system |> fst, expected with
            | ListenAnswer.Failed error, Some expected -> error |> shouldEqual expected
            | ListenAnswer.Listening where, None -> where |> shouldEqual (loopback 5000us)
            | answer, expected -> failwith $"platform %O{platform}: got %A{answer}, expected %A{expected}"

    /// ...and the re-screen is not "any second listener": a socket alone at its
    /// endpoint listens on both flavours, so the row above is about the conflict
    /// rather than about listening twice. Nor does a pair that has merely
    /// *bound* conflict — Linux's relaxation holds until one of them listens,
    /// which is the row below.
    [<TestCaseSource(nameof platforms)>]
    let ``a socket alone at its endpoint always listens`` (platform : SimulatedUnixPlatform) : unit =
        let fd, system =
            withSocket
                (SocketId 0L)
                (socketWith SocketKind.Stream (boundAt (loopback 5000us)) true SocketPhase.Idle)
                (systemOn platform)

        listenOrFail fd 8 system
        |> fst
        |> shouldEqual (ListenAnswer.Listening (loopback 5000us))

    /// The other half of Linux's rule: while *neither* socket listens, two
    /// reuse-carrying bindings at one endpoint coexist, and the first `listen`
    /// finds no conflict. Without this row the one above would pass for a rule
    /// that refused any duplicate binding at all.
    [<Test>]
    let ``the first listener of a reuse-carrying pair is admitted`` () : unit =
        for platform in platforms do
            let _, system =
                withSocket
                    (SocketId 0L)
                    (socketWith SocketKind.Stream (boundAt (loopback 5000us)) true SocketPhase.Idle)
                    (systemOn platform)

            let first, system =
                withSocket
                    (SocketId 1L)
                    (socketWith SocketKind.Stream (boundAt (loopback 5000us)) true SocketPhase.Idle)
                    system

            listenOrFail first 8 system
            |> fst
            |> shouldEqual (ListenAnswer.Listening (loopback 5000us))

    // ------------------------------------------------------------------
    // Refusals
    // ------------------------------------------------------------------

    [<TestCaseSource(nameof platforms)>]
    let ``a socket in an unmodelled domain is refused`` (platform : SimulatedUnixPlatform) : unit =
        for domain in [ SocketDomain.InterNetworkV6 ; SocketDomain.Unix ] do
            let socket =
                { socketWith SocketKind.Stream None false SocketPhase.Idle with
                    Domain = domain
                }

            let fd, system = withSocket (SocketId 0L) socket (systemOn platform)

            UnixSystem.listen fd 8 system
            |> shouldEqual (Error (ListenRefusal.UnmodelledDomain (SocketId 0L, domain)))

    [<TestCaseSource(nameof platforms)>]
    let ``a socket of an unmeasured kind is refused`` (platform : SimulatedUnixPlatform) : unit =
        for kind in [ SocketKind.Raw ; SocketKind.SeqPacket ] do
            let fd, system =
                withSocket (SocketId 0L) (socketWith kind None false SocketPhase.Idle) (systemOn platform)

            UnixSystem.listen fd 8 system
            |> shouldEqual (Error (ListenRefusal.UnmeasuredKind (SocketId 0L, kind)))

    /// Only `Idle` and `Listening` have measured answers; the rest are refused
    /// rather than guessed at, EISCONN for a connected socket being only
    /// plausible.
    [<TestCaseSource(nameof platforms)>]
    let ``a stream socket in an unmeasured phase is refused`` (platform : SimulatedUnixPlatform) : unit =
        let phases =
            [
                SocketPhase.EstablishedPendingReport (ConnectionId 7L)
                SocketPhase.Established (ConnectionId 7L)
                SocketPhase.RefusedPendingDelivery
                SocketPhase.Dead
                SocketPhase.DatagramPeer (loopback 9000us)
            ]

        for phase in phases do
            let fd, system =
                withSocket
                    (SocketId 0L)
                    (socketWith SocketKind.Stream (boundAt (loopback 5000us)) false phase)
                    (systemOn platform)

            UnixSystem.listen fd 8 system
            |> shouldEqual (Error (ListenRefusal.UnmeasuredPhase (SocketId 0L, phase)))

    /// The kind screen precedes the phase screen: a *datagram* socket in one of
    /// those phases is answered EOPNOTSUPP rather than refused, which is what
    /// keeps `DatagramPeer` from being a refusal for the sockets that legitimately
    /// hold it.
    [<TestCaseSource(nameof platforms)>]
    let ``a datagram peer is answered rather than refused`` (platform : SimulatedUnixPlatform) : unit =
        let fd, system =
            withSocket
                (SocketId 0L)
                (socketWith SocketKind.Datagram None false (SocketPhase.DatagramPeer (loopback 9000us)))
                (systemOn platform)

        listenOrFail fd 8 system
        |> fst
        |> shouldEqual (ListenAnswer.Failed UnixError.EOPNOTSUPP)

    /// The implicit bind can run out of ports, and the library refuses rather
    /// than inventing an errno for it.
    [<Test>]
    let ``an exhausted ephemeral range is refused`` () : unit =
        let system = systemOn SimulatedUnixPlatform.linuxX64

        let system =
            { system with
                Machine =
                    { system.Machine with
                        EphemeralPortRange = (40000us, 40000us)
                        NextEphemeralPort = 40000us
                    }
            }

        // A socket already holding the only port in the range, at the wildcard
        // so that the implicit bind's candidate collides with it.
        let _, system =
            withSocket
                (SocketId 1L)
                (socketWith
                    SocketKind.Stream
                    (Some
                        {
                            Endpoint = InternetEndpoint.ofParts InternetEndpoint.WildcardAddress 40000us
                            LockedAddress = None
                        })
                    false
                    SocketPhase.Idle)
                system

        let fd, system =
            withSocket (SocketId 0L) (socketWith SocketKind.Stream None false SocketPhase.Idle) system

        UnixSystem.listen fd 8 system
        |> shouldEqual (Error (ListenRefusal.EphemeralPortsExhausted (40000us, 40000us)))

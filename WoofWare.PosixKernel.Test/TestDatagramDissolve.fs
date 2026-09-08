namespace WoofWare.PosixKernel.Test

open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `connect(AF_UNSPEC)` on a datagram socket, row by row against the measured
/// tables in `docs/probes/udp-connect/`: what it answers, what it leaves of
/// the local binding and the peer, and what the socket can do afterwards.
///
/// Linux's `udp_disconnect` drops the peer, reverts the address to the
/// wildcard unless `bind(2)` locked a concrete one, and drops the port unless
/// `bind(2)` chose it -- connected or not. Darwin answers EAFNOSUPPORT, but a
/// connected socket has already lost its peer and had its address reset to
/// the wildcard, port kept, by the time it does.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestDatagramDissolve =

    let private inetFamily : int option =
        Some SimulatedUnixPlatform.internetAddressFamily

    let private endpoint (address : uint32) (port : uint16) : InternetEndpoint = InternetEndpoint.ofParts address port

    let private loopback : uint32 = InternetEndpoint.LoopbackAddress
    let private wildcard : uint32 = InternetEndpoint.WildcardAddress

    let private systemOn (platform : SimulatedUnixPlatform) : UnixSystem<int, string> =
        let system : UnixSystem<int, string> = UnixSystem.initial platform

        { system with
            Machine = UnixMachineState.withEphemeralPortRange (40000us, 40009us) system.Machine
        }

    let private socketOf (fd : int) (system : UnixSystem<int, string>) : SocketId =
        match FileDescriptorRegistry.tryFindTarget fd system.Process.FileDescriptors with
        | Some (OpenFileTarget.Socket socketId) -> socketId
        | other -> failwith $"fd %d{fd} is %A{other}"

    let private bound (fd : int) (system : UnixSystem<int, string>) : SocketBinding option =
        (UnixMachineState.socket (socketOf fd system) system.Machine).Binding

    let private phase (fd : int) (system : UnixSystem<int, string>) : SocketPhase =
        (UnixMachineState.socket (socketOf fd system) system.Machine).Phase

    /// A datagram socket, bound as `bindTo` says (or left unbound), and
    /// connected to 127.0.0.1:9000 if `connected`.
    let private datagram
        (platform : SimulatedUnixPlatform)
        (bindTo : InternetEndpoint option)
        (connected : bool)
        : int * UnixSystem<int, string>
        =
        let fd, system =
            UnixSocket.createSocket SocketDomain.InterNetwork SocketKind.Datagram SocketProtocol.Udp (systemOn platform)

        let system =
            match bindTo with
            | None -> system
            | Some endpoint ->
                match UnixSocket.bind fd UserBuffer.Mapped 16 false inetFamily (Some endpoint) system with
                | Ok (BindAnswer.Bound _, system) -> system
                | other -> failwith $"bind to %s{InternetEndpoint.toString endpoint}: %A{other}"

        let system =
            if not connected then
                system
            else
                match
                    UnixConnection.connect fd UserBuffer.Mapped 16 inetFamily (Some (endpoint loopback 9000us)) system
                with
                | Ok (ConnectOutcome.Completed, system) -> system
                | other -> failwith $"connect: %A{other}"

        fd, system

    let private dissolve (fd : int) (system : UnixSystem<int, string>) : ConnectOutcome * UnixSystem<int, string> =
        match UnixConnection.connectSocket (socketOf fd system) false 16 (Some 0) None system with
        | Ok answer -> answer
        | Error refusal -> failwith $"dissolve refused: %s{ConnectRefusal.describe refusal}"

    /// The measured rows, as (how bound, connected) -> what is left. A port of
    /// 0 in `bindTo` is the kernel's choice, and the expectation's port then
    /// says whether *that* port survives (`KeepsPort`) or goes.
    [<RequireQualifiedAccess>]
    type private Left =
        | Unbound
        | At of address : uint32 * KeepsPort : bool * port : uint16 option

    let private linuxRows : (InternetEndpoint option * bool * Left) list =
        [
            Some (endpoint wildcard 5555us), true, Left.At (wildcard, true, Some 5555us)
            Some (endpoint wildcard 0us), true, Left.Unbound
            Some (endpoint loopback 5556us), true, Left.At (loopback, true, Some 5556us)
            Some (endpoint loopback 0us), true, Left.At (loopback, false, Some 0us)
            None, true, Left.Unbound
            Some (endpoint wildcard 5557us), false, Left.At (wildcard, true, Some 5557us)
            Some (endpoint loopback 5558us), false, Left.At (loopback, true, Some 5558us)
            None, false, Left.Unbound
        ]

    [<Test>]
    let ``Linux dissolves as udp_disconnect does, keeping a locked address and a chosen port`` () : unit =
        for bindTo, connected, left in linuxRows do
            let fd, system = datagram SimulatedUnixPlatform.linuxX64 bindTo connected
            let outcome, after = dissolve fd system

            outcome |> shouldEqual ConnectOutcome.Completed
            phase fd after |> shouldEqual SocketPhase.Idle
            UnixSystem.checkInvariants after |> shouldEqual []

            match left, bound fd after with
            | Left.Unbound, None -> ()
            | Left.At (address, _, Some port), Some binding ->
                binding.Endpoint.Address |> shouldEqual address
                binding.Endpoint.Port |> shouldEqual port
            | expected, actual ->
                failwith $"bind %A{bindTo}, connected %b{connected}: expected %A{expected}, got %A{actual}"

    [<Test>]
    let ``Darwin answers EAFNOSUPPORT, and a connected socket has lost its peer and its address by then`` () : unit =
        let rows =
            [
                Some (endpoint wildcard 5555us), true, Some (wildcard, 5555us)
                Some (endpoint loopback 5556us), true, Some (wildcard, 5556us)
                None, true, None
                Some (endpoint wildcard 5557us), false, Some (wildcard, 5557us)
                Some (endpoint loopback 5558us), false, Some (loopback, 5558us)
                None, false, None
            ]

        for bindTo, connected, expected in rows do
            let fd, system = datagram SimulatedUnixPlatform.macOsArm64 bindTo connected
            let outcome, after = dissolve fd system

            outcome |> shouldEqual (ConnectOutcome.Failed UnixError.EAFNOSUPPORT)
            phase fd after |> shouldEqual SocketPhase.Idle
            UnixSystem.checkInvariants after |> shouldEqual []

            match expected, bound fd after with
            | None, actual ->
                // Implicitly bound by the connect, or never bound: the port
                // a connect took is kept, the address reset.
                match connected, actual with
                | true, Some binding ->
                    binding.Endpoint.Address |> shouldEqual wildcard
                    binding.Endpoint.Port |> shouldNotEqual 0us
                | false, None -> ()
                | _ -> failwith $"bind None, connected %b{connected}: got %A{actual}"
            | Some (address, port), Some binding ->
                binding.Endpoint.Address |> shouldEqual address
                binding.Endpoint.Port |> shouldEqual port
            | Some expected, None ->
                failwith $"bind %A{bindTo}, connected %b{connected}: expected %A{expected}, got nothing"

    // ------------------------------------------------------------------
    // What a half-bound Linux socket does next
    // ------------------------------------------------------------------

    let private halfBound () : int * UnixSystem<int, string> =
        let fd, system =
            datagram SimulatedUnixPlatform.linuxX64 (Some (endpoint loopback 0us)) true

        let _, after = dissolve fd system
        (bound fd after).Value.Endpoint |> shouldEqual (endpoint loopback 0us)
        fd, after

    [<Test>]
    let ``a half-bound socket connects from its address on a fresh port`` () : unit =
        let fd, system = halfBound ()

        match UnixConnection.connect fd UserBuffer.Mapped 16 inetFamily (Some (endpoint loopback 9001us)) system with
        | Ok (ConnectOutcome.Completed, after) ->
            let binding = (bound fd after).Value
            binding.Endpoint.Address |> shouldEqual loopback
            binding.Endpoint.Port |> shouldNotEqual 0us
            UnixSystem.checkInvariants after |> shouldEqual []
        | other -> failwith $"connect from the half-bound socket: %A{other}"

    [<Test>]
    let ``a half-bound socket rebinds, to any address and either kind of port`` () : unit =
        for rebind in
            [
                endpoint loopback 0us
                endpoint wildcard 0us
                endpoint loopback 7777us
                endpoint wildcard 7778us
            ] do
            let fd, system = halfBound ()

            match UnixSocket.bind fd UserBuffer.Mapped 16 false inetFamily (Some rebind) system with
            | Ok (BindAnswer.Bound actual, after) ->
                actual.Address |> shouldEqual rebind.Address

                if rebind.Port <> 0us then
                    actual.Port |> shouldEqual rebind.Port
                else
                    actual.Port |> shouldNotEqual 0us

                UnixSystem.checkInvariants after |> shouldEqual []
            | other -> failwith $"rebind to %s{InternetEndpoint.toString rebind}: %A{other}"

    [<Test>]
    let ``a socket that kept its port cannot rebind, on either flavour`` () : unit =
        for platform in [ SimulatedUnixPlatform.linuxX64 ; SimulatedUnixPlatform.macOsArm64 ] do
            let fd, system = datagram platform (Some (endpoint wildcard 5555us)) true
            let _, after = dissolve fd system

            match UnixSocket.bind fd UserBuffer.Mapped 16 false inetFamily (Some (endpoint wildcard 0us)) after with
            | Ok (BindAnswer.Failed UnixError.EINVAL, _) -> ()
            | other -> failwith $"rebind after dissolve on %O{platform}: %A{other}"

    /// The port-0 invariant admits exactly the half-bound socket Linux's
    /// dissolve leaves: the same binding forged onto a Darwin system, or onto
    /// a socket still holding a peer, is the defect it always was.
    [<Test>]
    let ``the port-0 exception is exactly Linux's idle half-bound datagram`` () : unit =
        let halfBound : SocketBinding =
            {
                Endpoint = endpoint loopback 0us
                LockedAddress = Some loopback
                LockedPort = false
            }

        let forge (platform : SimulatedUnixPlatform) (phase : SocketPhase) : UnixSystem<int, string> =
            let fd, system =
                UnixSocket.createSocket
                    SocketDomain.InterNetwork
                    SocketKind.Datagram
                    SocketProtocol.Udp
                    (systemOn platform)

            let socketId = socketOf fd system

            { system with
                Machine =
                    { system.Machine with
                        Sockets =
                            system.Machine.Sockets
                            |> Map.add
                                socketId
                                { UnixMachineState.socket socketId system.Machine with
                                    Binding = Some halfBound
                                    Phase = phase
                                }
                    }
            }

        UnixSystem.checkInvariants (forge SimulatedUnixPlatform.linuxX64 SocketPhase.Idle)
        |> shouldEqual []

        UnixSystem.checkInvariants (forge SimulatedUnixPlatform.macOsArm64 SocketPhase.Idle)
        |> shouldEqual [ UnixSystemDefect.BoundToPortZero (SocketId 0L) ]

        UnixSystem.checkInvariants (
            forge SimulatedUnixPlatform.linuxX64 (SocketPhase.DatagramPeer (endpoint loopback 9000us))
        )
        |> shouldEqual [ UnixSystemDefect.BoundToPortZero (SocketId 0L) ]

    /// A half-bound socket reserves no port, so a second socket asking for
    /// `127.0.0.1:0` beside it is given a port rather than EADDRINUSE: two
    /// zero ports are not a collision.
    [<Test>]
    let ``a port-0 bind beside a half-bound socket is served`` () : unit =
        let _, system = halfBound ()

        let other, system =
            UnixSocket.createSocket SocketDomain.InterNetwork SocketKind.Datagram SocketProtocol.Udp system

        match UnixSocket.bind other UserBuffer.Mapped 16 false inetFamily (Some (endpoint loopback 0us)) system with
        | Ok (BindAnswer.Bound actual, after) ->
            actual.Address |> shouldEqual loopback
            actual.Port |> shouldNotEqual 0us
            UnixSystem.checkInvariants after |> shouldEqual []
        | other -> failwith $"bind beside the half-bound socket: %A{other}"

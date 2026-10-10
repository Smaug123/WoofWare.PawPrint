namespace WoofWare.PosixKernel.Test

open System.Collections.Immutable
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// Dual-mode IPv6 TCP sockets: what `TestDualModeMeasured`'s replay of the
/// probe cannot say -- which refusal each unmodelled input gets, the state a
/// connect leaves behind, and `connectSocket`, which the probe has no way to
/// call.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestDualMode =

    let private platforms : SimulatedUnixPlatform list =
        [ SimulatedUnixPlatform.linuxX64 ; SimulatedUnixPlatform.macOsArm64 ]

    let private fresh (platform : SimulatedUnixPlatform) : UnixSystem<int, string> =
        UnixSystem.initial platform
        |> Launched.boot UnixSystem.pipedStandardStreams 0 (CpuId 0)

    let private loopback (port : uint16) : InternetEndpoint =
        InternetEndpoint.ofParts InternetEndpoint.LoopbackAddress port

    let private socketIdOf (fd : int) (system : UnixSystem<int, string>) : SocketId =
        match FileDescriptorRegistry.tryFindTarget fd (UnixSystemState.fileDescriptors system) with
        | Some (OpenFileTarget.Socket socketId) -> socketId
        | other -> failwith $"fd %d{fd} names %A{other}"

    let private ipv6Only (system : UnixSystem<int, string>) (fd : int) (value : int) : UnixSystem<int, string> =
        let platform = system.Machine.UnixPlatform
        let level = SimulatedUnixPlatform.ipv6OptionLevel platform
        let name = SimulatedUnixPlatform.ipv6OnlyOption platform

        match
            UnixSocket.setsockopt
                fd
                level
                name
                UserBuffer.Mapped
                4u
                (Some (ImmutableArray.CreateRange (SimulatedUnixPlatform.encodeCInt platform value)))
                system
        with
        | Ok (SetSockOptAnswer.Set, system) -> system
        | other -> failwith $"IPV6_V6ONLY answered %A{other}"

    /// A listener at 127.0.0.1 and an IPv6 stream socket with `IPV6_V6ONLY`
    /// `v6only`: the listener's port, the socket's descriptor, and the system.
    let private listenerAndClient
        (platform : SimulatedUnixPlatform)
        (v6only : int)
        : uint16 * int * UnixSystem<int, string>
        =
        let system = fresh platform

        let listener, system =
            NewSocket.create SocketDomain.Inet SocketKind.Stream SocketProtocol.Tcp system

        let port, system =
            match CopyIn.bind listener UserBuffer.Mapped 16u (CopyIn.inet platform (loopback 0us)) system with
            | Ok (BindAnswer.Bound endpoint, system) -> endpoint.Port, system
            | other -> failwith $"listener bind answered %A{other}"

        let system =
            match UnixSocket.listen listener 8 system with
            | Ok (ListenAnswer.Listening _, system) -> system
            | other -> failwith $"listen answered %A{other}"

        let client, system =
            NewSocket.create SocketDomain.Inet6 SocketKind.Stream SocketProtocol.Tcp system

        port, client, ipv6Only system client v6only

    [<TestCaseSource(nameof platforms)>]
    let ``connectSocket connects a dual-mode socket over IPv4, as connect does``
        (platform : SimulatedUnixPlatform)
        : unit
        =
        let port, client, system = listenerAndClient platform 0
        let blob = CopyIn.inet6Mapped platform (loopback port)
        let socketId = socketIdOf client system

        let viaSocket =
            UnixConnection.connectSocket socketId false 28u (CopyIn.mapped platform 28u blob) system

        let viaDescriptor = CopyIn.connect client UserBuffer.Mapped 28u blob system
        viaSocket |> shouldEqual viaDescriptor

        match viaSocket with
        | Ok (ConnectOutcome.Completed, system) ->
            let socket = UnixMachineState.socket socketId system.Machine

            match socket.Addressing, socket.Phase with
            | SocketAddressing.Inet6DualMode (Some binding), SocketPhase.Established (connection, ConnectionEnd.Client) ->
                binding.Endpoint.Address |> shouldEqual InternetEndpoint.LoopbackAddress
                let connection = UnixMachineState.connection connection system.Machine
                connection.ServerAddress |> shouldEqual (loopback port)
                connection.ClientAddress |> shouldEqual binding.Endpoint
            | other -> failwith $"the client is %A{other}"
        | other -> failwith $"connect answered %A{other}"

    /// IPv4 is the only transport, so a dual-mode socket's connection has an
    /// IPv4 connection's buffers: measured on Darwin, whose buffers depend on
    /// the segment size (`docs/probes/dual-mode/`, G8).
    [<TestCaseSource(nameof platforms)>]
    let ``a dual-mode connection is sized as an IPv4 one`` (platform : SimulatedUnixPlatform) : unit =
        let transfer (connect : uint16 -> int -> UnixSystem<int, string> -> UnixSystem<int, string>) (v6 : bool) =
            let port, client, system = listenerAndClient platform 0

            let client, system =
                if v6 then
                    client, system
                else
                    NewSocket.create SocketDomain.Inet SocketKind.Stream SocketProtocol.Tcp system

            let system = connect port client system

            match (UnixMachineState.socket (socketIdOf client system) system.Machine).Phase with
            | SocketPhase.Established (connection, _) ->
                (UnixMachineState.connection connection system.Machine).Transfer
            | other -> failwith $"the client is %A{other}"

        let connectWith (blob : uint16 -> byte[]) (length : uint32) (port : uint16) (fd : int) system =
            match CopyIn.connect fd UserBuffer.Mapped length (blob port) system with
            | Ok (ConnectOutcome.Completed, system) -> system
            | other -> failwith $"connect answered %A{other}"

        let dual =
            transfer (connectWith (fun port -> CopyIn.inet6Mapped platform (loopback port)) 28u) true

        let v4 =
            transfer (connectWith (fun port -> CopyIn.inet platform (loopback port)) 16u) false

        dual |> shouldEqual v4

    [<TestCaseSource(nameof platforms)>]
    let ``a native IPv6 destination is refused`` (platform : SimulatedUnixPlatform) : unit =
        let _, client, system = listenerAndClient platform 0
        let address = Array.zeroCreate<byte> 16
        address.[15] <- 1uy

        let blob =
            CopyIn.blob6 platform (SimulatedUnixPlatform.internetV6AddressFamily platform) address 5000us

        match CopyIn.connect client UserBuffer.Mapped 28u blob system with
        | Error (ConnectRefusal.Ipv6Destination (socket, refused, 5000us)) ->
            socket |> shouldEqual (socketIdOf client system)
            refused |> shouldEqual (ImmutableArray.CreateRange address)
        | other -> failwith $"connect answered %A{other}"

    [<TestCaseSource(nameof platforms)>]
    let ``binding the wildcard or a native address is refused`` (platform : SimulatedUnixPlatform) : unit =
        let _, client, system = listenerAndClient platform 0
        let family = SimulatedUnixPlatform.internetV6AddressFamily platform

        for address in [ Array.zeroCreate<byte> 16 ; CopyIn.mappedAddress 0u ] do
            match CopyIn.bind client UserBuffer.Mapped 28u (CopyIn.blob6 platform family address 0us) system with
            | Error (BindRefusal.UnmodelledIpv6Address (_, refused)) ->
                refused |> shouldEqual (ImmutableArray.CreateRange address)
            | other -> failwith $"bind of %A{address} answered %A{other}"

    [<TestCaseSource(nameof platforms)>]
    let ``an IPv6 socket does not listen`` (platform : SimulatedUnixPlatform) : unit =
        let _, client, system = listenerAndClient platform 0

        match UnixSocket.listen client 8 system with
        | Error (ListenRefusal.Ipv6Listener socket) -> socket |> shouldEqual (socketIdOf client system)
        | other -> failwith $"listen answered %A{other}"

    [<TestCaseSource(nameof platforms)>]
    let ``an IPv6 datagram socket's addresses are refused`` (platform : SimulatedUnixPlatform) : unit =
        let system = fresh platform

        let fd, system =
            NewSocket.create SocketDomain.Inet6 SocketKind.Datagram SocketProtocol.Udp system

        let socketId = socketIdOf fd system
        let blob = CopyIn.inet6Mapped platform (loopback 5000us)

        CopyIn.connect fd UserBuffer.Mapped 28u blob system
        |> shouldEqual (
            Error (ConnectRefusal.Copy (SockaddrCopyRefusal.UnmodelledInet6Kind (socketId, SocketKind.Datagram)))
        )

        CopyIn.bind fd UserBuffer.Mapped 28u blob system
        |> shouldEqual (
            Error (BindRefusal.Copy (SockaddrCopyRefusal.UnmodelledInet6Kind (socketId, SocketKind.Datagram)))
        )

        UnixSocket.getsockname fd UserBuffer.Mapped 28u system
        |> shouldEqual (Error (GetSockNameRefusal.UnmodelledInet6Kind (socketId, SocketKind.Datagram)))

    [<Test>]
    let ``a Darwin IPv6 sockaddr short of sin6_addr's end is refused`` () : unit =
        let platform = SimulatedUnixPlatform.macOsArm64
        let port, client, system = listenerAndClient platform 0
        let socketId = socketIdOf client system
        let blob = CopyIn.inet6Mapped platform (loopback port)

        for length in [ 0u ; 2u ; 16u ; 23u ] do
            CopyIn.connect client UserBuffer.Mapped length blob system
            |> shouldEqual (
                Error (ConnectRefusal.Copy (SockaddrCopyRefusal.DarwinShortInet6Sockaddr (socketId, length)))
            )

        // From 24 it is answered.
        match CopyIn.connect client UserBuffer.Mapped 24u blob system with
        | Ok (ConnectOutcome.Completed, _) -> ()
        | other -> failwith $"connect at 24 answered %A{other}"

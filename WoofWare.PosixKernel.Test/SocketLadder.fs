namespace WoofWare.PosixKernel.Test

open System
open System.IO
open System.Reflection
open WoofWare.PosixKernel

/// The world the socket-ladder replays (`TestConnectLadderMeasured`,
/// `TestBindLadderMeasured`) make each call in, as the probes behind them
/// made theirs: a listener, a bound datagram socket and a port nothing holds,
/// and the socket under test in one of the states the probes swept.
[<AutoOpen>]
module SocketLadder =

    let listenerPort : uint16 = 5000us
    let datagramPeerPort : uint16 = 5001us
    let closedPort : uint16 = 5002us

    [<RequireQualifiedAccess>]
    type State =
        | Fresh
        | Bound
        | WildcardBound
        | Connected
        | Listening

    let parseState (text : string) : State =
        match text with
        | "fresh" -> State.Fresh
        | "bound" -> State.Bound
        | "wildbound" -> State.WildcardBound
        | "connected" -> State.Connected
        | "listening" -> State.Listening
        | other -> failwith $"unknown state %s{other}"

    let resource (name : string) : string[] =
        let assembly = Assembly.GetExecutingAssembly ()

        use stream =
            match assembly.GetManifestResourceStream $"WoofWare.PosixKernel.Test.%s{name}" with
            | null -> failwith $"embedded resource %s{name} not found"
            | stream -> stream

        use reader = new StreamReader (stream)
        reader.ReadToEnd().Split ('\n', StringSplitOptions.RemoveEmptyEntries)

    let kindOf (text : string) : SocketKind =
        match text with
        | "stream" -> SocketKind.Stream
        | "dgram" -> SocketKind.Datagram
        | other -> failwith $"unknown kind %s{other}"

    let loopback (port : uint16) =
        InternetEndpoint.ofParts InternetEndpoint.LoopbackAddress port

    /// The port a row's call was aimed at, by kind: a listener for a stream
    /// socket, a bound socket for a datagram one.
    let peerPort (kind : SocketKind) : uint16 =
        match kind with
        | SocketKind.Stream -> listenerPort
        | _ -> datagramPeerPort

    /// At most this many families out of a range, evenly spaced and always with
    /// both ends: the F rows span up to 65536 families.
    let familySample (low : int) (high : int) : int list =
        if high - low < 300 then
            [ low..high ]
        else
            let step = (high - low) / 64
            [ low..step..high ] @ [ high ] |> List.distinct

    /// The machine every call is made in: a listener at 127.0.0.1:5000, a
    /// datagram socket bound at 127.0.0.1:5001, and nothing at 5002; and the
    /// socket the call is made on, in `state`.
    let setUp (platform : SimulatedUnixPlatform) (kind : SocketKind) (state : State) : int * UnixSystem<int, string> =
        let system =
            UnixSystem.initial platform UnixSystem.pipedStandardStreams 0 (CpuId 0)
            |> UnixBootImage.withLocalAddresses UnixSystem.defaultLocalAddresses []
            |> UnixBootImage.boot

        let bindTo (fd : int) (endpoint : InternetEndpoint) (system : UnixSystem<int, string>) =
            match CopyIn.bind fd UserBuffer.Mapped 16u (CopyIn.inet platform endpoint) system with
            | Ok (BindAnswer.Bound _, system) -> system
            | other -> failwith $"setUp: bind answered %A{other}"

        let listen (fd : int) (system : UnixSystem<int, string>) =
            match UnixSocket.listen fd 128 system with
            | Ok (ListenAnswer.Listening _, system) -> system
            | other -> failwith $"setUp: listen answered %A{other}"

        let create (kind : SocketKind) (system : UnixSystem<int, string>) =
            let protocol =
                if kind = SocketKind.Stream then
                    SocketProtocol.Tcp
                else
                    SocketProtocol.Udp

            NewSocket.create SocketDomain.Inet kind protocol system

        let listener, system = create SocketKind.Stream system
        let system = system |> bindTo listener (loopback listenerPort) |> listen listener
        let peer, system = create SocketKind.Datagram system
        let system = bindTo peer (loopback datagramPeerPort) system

        let fd, system = create kind system

        let system =
            match state with
            | State.Fresh -> system
            | State.Bound -> bindTo fd (loopback 0us) system
            | State.WildcardBound -> bindTo fd (InternetEndpoint.ofParts 0u 0us) system
            | State.Connected ->
                match
                    CopyIn.connect fd UserBuffer.Mapped 16u (CopyIn.inet platform (loopback (peerPort kind))) system
                with
                | Ok (ConnectOutcome.Completed, system) -> system
                | other -> failwith $"setUp: connect answered %A{other}"
            | State.Listening -> system |> bindTo fd (loopback 0us) |> listen fd

        fd, system

    let socketOf (fd : int) (system : UnixSystem<int, string>) : SocketDescription =
        match FileDescriptorRegistry.tryFindTarget fd system.Process.FileDescriptors with
        | Some (OpenFileTarget.Socket socketId) -> UnixMachineState.socket socketId system.Machine
        | other -> failwith $"fd %d{fd} names %A{other}"

    let local (socket : SocketDescription) : InternetEndpoint =
        match socket.Binding with
        | Some binding -> binding.Endpoint
        | None -> InternetEndpoint.ofParts 0u 0us

    let dotted (address : uint32) : string =
        $"%d{address >>> 24}.%d{(address >>> 16) &&& 0xFFu}.%d{(address >>> 8) &&& 0xFFu}.%d{address &&& 0xFFu}"

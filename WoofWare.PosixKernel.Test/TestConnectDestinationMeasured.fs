namespace WoofWare.PosixKernel.Test

open System.Text.RegularExpressions
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `connect(2)` to the wildcard, to a port of 0, to an address this machine
/// does not hold, and to a broadcast or multicast address, against every row
/// `sockaddr-dgram-connect.c` measured on Linux 6.18.5 and Darwin 27.0.0,
/// replayed call by call in this kernel; with the probe's sweeps of a fresh and
/// a connected datagram socket's connect at every length.
///
/// Each row's answer, the local address afterwards and the peer
/// `getpeername(2)` reads are compared. Every row must be answered as
/// measured, except those this kernel refuses (`refused`), which are listed by
/// name and asserted exactly, so a row that changes either way is reported.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestConnectDestinationMeasured =

    /// One call the probe made, and what it saw.
    type private Call =
        {
            /// The probe's line, for a reader of a failure.
            Source : string
            Kind : SocketKind
            State : State
            Family : int
            Endpoint : InternetEndpoint
            Length : uint32
            /// What the probe printed after the colon: the errno's name or
            /// `OK`, the local address and its port's class, and the peer.
            Expected : string
        }

    let private address (text : string) : uint32 =
        match text with
        | "0.0.0.0" -> 0u
        | "127.0.0.1" -> InternetEndpoint.LoopbackAddress
        | "127.0.0.2" -> 0x7F000002u
        | "8.8.8.8" -> 0x08080808u
        | "224.0.0.1" -> 0xE0000001u
        | "255.255.255.255" -> System.UInt32.MaxValue
        | other -> failwith $"unknown address %s{other}"

    let private calls (flavour : string) : Call list =
        let rows = resource $"sockaddr-dgram-connect.%s{flavour}.txt"

        let d =
            Regex @"^D (?<kind>\w+)\s+(?<addr0>[\d.]+)\s+(?<port>0|P) (?<state>\w+)\s*: (?<rest>.+)$"

        let m =
            Regex
                @"^M (?<kind>\w+)\s+(?<fam>AF_INET|AF_UNSPEC)\s+(?<addr0>[\d.]+)\s+fresh len (?<lo>\d+)\.\.(?<hi>\d+): (?<rest>.+)$"

        let b =
            Regex @"^B dgram family=(?<fam>\d+)\s+fresh len (?<lo>\d+)\.\.(?<hi>\d+): (?<rest>.+)$"

        let l =
            Regex @"^L dgram family=(?<fam>\d+)\s+connected len (?<lo>\d+)\.\.(?<hi>\d+): (?<rest>.+)$"

        let lengths (m : Match) =
            [ uint32 m.Groups.["lo"].Value .. uint32 m.Groups.["hi"].Value ]

        let call line kind state family endpoint length (m : Match) =
            {
                Source = line
                Kind = kind
                State = state
                Family = family
                Endpoint = endpoint
                Length = length
                Expected = m.Groups.["rest"].Value
            }

        rows
        |> Array.toList
        |> List.collect (fun line ->
            let dm = d.Match line
            let mm = m.Match line
            let bm = b.Match line
            let lm = l.Match line

            if dm.Success then
                let kind = kindOf dm.Groups.["kind"].Value

                let port =
                    if dm.Groups.["port"].Value = "P" then
                        peerPort kind
                    else
                        0us

                [
                    call
                        line
                        kind
                        (parseState dm.Groups.["state"].Value)
                        SimulatedUnixPlatform.internetAddressFamily
                        (InternetEndpoint.ofParts (address dm.Groups.["addr0"].Value) port)
                        16u
                        dm
                ]
            elif mm.Success then
                let kind = kindOf mm.Groups.["kind"].Value

                let family =
                    if mm.Groups.["fam"].Value = "AF_INET" then
                        SimulatedUnixPlatform.internetAddressFamily
                    else
                        0

                lengths mm
                |> List.map (fun length ->
                    call
                        line
                        kind
                        State.Fresh
                        family
                        (InternetEndpoint.ofParts (address mm.Groups.["addr0"].Value) (peerPort kind))
                        length
                        mm
                )
            elif bm.Success then
                lengths bm
                |> List.map (fun length ->
                    call
                        line
                        SocketKind.Datagram
                        State.Fresh
                        (int bm.Groups.["fam"].Value)
                        (loopback datagramPeerPort)
                        length
                        bm
                )
            elif lm.Success then
                lengths lm
                |> List.map (fun length ->
                    call
                        line
                        SocketKind.Datagram
                        State.Connected
                        (int lm.Groups.["fam"].Value)
                        (loopback datagramPeerPort)
                        length
                        lm
                )
            elif line.StartsWith "#" then
                []
            else
                failwith $"unparsed probe line: %s{line}"
        )

    /// The peer `getpeername(2)` reads: a stream socket's connection's far
    /// end, or a datagram socket's peer filter with a port, since one of port
    /// 0 is one it answers ENOTCONN for.
    let private peerOf (system : UnixSystem<int, string>) (socket : SocketDescription) : InternetEndpoint option =
        match socket.Phase with
        | SocketPhase.Established connection
        | SocketPhase.EstablishedPendingReport connection ->
            Some (UnixMachineState.connection connection system.Machine).ServerAddress
        | SocketPhase.DatagramPeer endpoint when endpoint.Port <> 0us -> Some endpoint
        | _ -> None

    /// What this kernel answers for `call`, in the probe's words.
    let private replay (platform : SimulatedUnixPlatform) (call : Call) : string =
        let fd, system = setUp platform call.Kind call.State
        let beforeSocket = socketOf fd system
        let before = local beforeSocket
        let peerBefore = peerOf system beforeSocket

        match
            CopyIn.connect fd UserBuffer.Mapped call.Length (CopyIn.blob platform call.Family call.Endpoint) system
        with
        | Error refusal ->
            let name = (sprintf "%A" refusal).Split([| ' ' ; '\n' |]).[0]
            $"refused:%s{name}"
        | Ok (outcome, after) ->
            let answer =
                match outcome with
                | ConnectOutcome.Completed -> "OK"
                | ConnectOutcome.Failed error -> string error

            let socket = socketOf fd after
            let now = local socket

            let portClass =
                if now.Port = 0us then "0"
                elif now.Port = before.Port then "same"
                else "new"

            let peer =
                match peerOf after socket with
                | None -> "none"
                | Some peer when Some peer = peerBefore -> "old"
                | Some peer when peer.Port = call.Endpoint.Port -> $"%s{dotted peer.Address}:P"
                | Some peer -> $"%s{dotted peer.Address}:%d{peer.Port}"

            $"%s{answer} local=%s{dotted now.Address}:%s{portClass} peer=%s{peer}"

    /// The rows this kernel refuses rather than answers, by the probe line they
    /// came from, with the refusal: a connect that would reach beyond this
    /// machine, whose source address depends on the host's own routes and
    /// interfaces (`DestinationNotLocal`, and a SYN to 127.0.0.2 Darwin drops);
    /// and a datagram connect to a broadcast or multicast destination that
    /// would succeed (`DatagramGroupDestination`).
    let private refused (flavour : string) : (string * string) list =
        let notLocal = "refused:DestinationNotLocal"
        let group = "refused:DatagramGroupDestination"
        let states = [ "fresh" ; "bound" ; "wildbound" ; "connected" ]

        match flavour with
        | "linux" ->
            [
                for state in [ "fresh" ; "bound" ; "wildbound" ] do
                    $"D stream 127.0.0.2 0 %s{state}", notLocal
                    $"D stream 127.0.0.2 P %s{state}", notLocal
                    $"D stream 8.8.8.8 0 %s{state}", notLocal
                for state in states do
                    $"D dgram 127.0.0.2 0 %s{state}", notLocal
                    $"D dgram 127.0.0.2 P %s{state}", notLocal
                    $"D dgram 8.8.8.8 0 %s{state}", notLocal
                    $"D dgram 8.8.8.8 P %s{state}", notLocal
                    $"D dgram 224.0.0.1 0 %s{state}", group
                    $"D dgram 224.0.0.1 P %s{state}", group
                "M dgram AF_INET 224.0.0.1 fresh len 16..128", group
            ]
        | _ ->
            [
                for state in [ "fresh" ; "bound" ; "wildbound" ] do
                    $"D stream 127.0.0.2 P %s{state}", notLocal
                    $"D stream 8.8.8.8 P %s{state}", notLocal
                for state in states do
                    $"D dgram 127.0.0.2 P %s{state}", notLocal
                    $"D dgram 8.8.8.8 P %s{state}", notLocal
                    $"D dgram 224.0.0.1 P %s{state}", group
                    $"D dgram 255.255.255.255 P %s{state}", group
                "M dgram AF_INET 224.0.0.1 fresh len 16..16", group
                "M dgram AF_INET 255.255.255.255 fresh len 16..16", group
            ]

    let private check (flavour : string) (platform : SimulatedUnixPlatform) : unit =
        let all = calls flavour
        all |> List.isEmpty |> shouldEqual false

        let actual =
            all
            |> List.map (fun call -> call, replay platform call)
            |> List.filter (fun (call, answer) -> answer <> call.Expected)
            |> List.groupBy (fun (call, _) -> call.Source)
            |> List.map (fun (source, rows) ->
                // Everything before the answer, which every probe line
                // introduces with its first ": ", with its padding collapsed.
                Regex.Replace(source.Substring (0, source.IndexOf ": "), " +", " ").TrimEnd (),
                rows |> List.map snd |> List.distinct |> String.concat " | "
            )
            |> Set.ofList

        let allowed =
            refused flavour
            |> List.map (fun (line, answer) -> Regex.Replace (line, " +", " "), answer)
            |> Set.ofList

        if actual <> allowed then
            let show (rows : (string * string) list) =
                rows
                |> List.map (fun (line, answer) -> $"  %s{line}  =>  %s{answer}")
                |> String.concat "\n"

            let unexpected = Set.difference actual allowed |> Set.toList
            let missing = Set.difference allowed actual |> Set.toList

            failwith
                $"%s{flavour}: %d{List.length unexpected} probe lines answered otherwise than measured:\n%s{show unexpected}\n%d{List.length missing} refused lines now answered otherwise:\n%s{show missing}"

    [<Test>]
    let ``every Linux row is answered as measured`` () : unit =
        check "linux" SimulatedUnixPlatform.linuxX64

    [<Test>]
    let ``every Darwin row is answered as measured`` () : unit =
        check "darwin" SimulatedUnixPlatform.macOsArm64

    /// `sockaddr-dgram-reconnect.c`'s rows: a socket bound to an interface
    /// address connecting to 127.0.0.1 and to 0.0.0.0 -- a datagram socket,
    /// connected to a socket there before or not, and a stream socket. Darwin's
    /// disconnect resets a connected datagram socket's address, so it connects
    /// from 127.0.0.1; Linux keeps the interface address. A wildcard
    /// destination is the socket's own interface address on Linux and
    /// 127.0.0.1 on Darwin. The probe's interface address stands here as
    /// 10.0.0.5, an address the machine holds.
    let private reconnect (flavour : string) (platform : SimulatedUnixPlatform) : unit =
        let iface = 0x0A000005u

        let row =
            Regex
                @"^(?<kind>[RS]) (?<state>[\w-]+)\s+to (?<dest>[\d.]+)\s*: (?<ans>\w+) local=(?<addr>[\w.]+) port=(?<port>\w+) peer=(?<peer>[\w.]+)$"

        let rows =
            resource $"sockaddr-dgram-reconnect.%s{flavour}.txt"
            |> Array.toList
            |> List.filter (fun line -> not (line.StartsWith "#"))

        rows |> List.length |> shouldEqual 6

        let name (address : uint32) : string =
            if address = iface then
                "iface"
            elif address = InternetEndpoint.LoopbackAddress then
                "127.0.0.1"
            else
                dotted address

        for line in rows do
            let m = row.Match line

            if not m.Success then
                failwith $"unparsed probe line: %s{line}"

            let system =
                UnixSystem.initial<int, string> platform
                |> UnixBootImage.withLocalAddresses [ InternetEndpoint.LoopbackAddress ; iface ] []
                |> Launched.boot UnixSystem.pipedStandardStreams 0 (CpuId 0)

            let create (kind : SocketKind) (system : UnixSystem<int, string>) =
                let protocol =
                    if kind = SocketKind.Stream then
                        SocketProtocol.Tcp
                    else
                        SocketProtocol.Udp

                NewSocket.create SocketDomain.Inet kind protocol system

            let bindTo fd (endpoint : InternetEndpoint) system =
                match CopyIn.bind fd UserBuffer.Mapped 16u (CopyIn.inet platform endpoint) system with
                | Ok (BindAnswer.Bound bound, system) -> bound, system
                | other -> failwith $"%s{line}: bind answered %A{other}"

            let connectTo fd (endpoint : InternetEndpoint) system =
                CopyIn.connect fd UserBuffer.Mapped 16u (CopyIn.inet platform endpoint) system

            let kind =
                if m.Groups.["kind"].Value = "S" then
                    SocketKind.Stream
                else
                    SocketKind.Datagram

            // Somewhere to connect to at the interface address, and, at port
            // 6000 on the wildcard, a socket of the call's own kind.
            let peer, system = create SocketKind.Datagram system
            let peerAt, system = bindTo peer (InternetEndpoint.ofParts iface 0us) system
            let target, system = create kind system
            let _, system = bindTo target (InternetEndpoint.ofParts 0u 6000us) system

            let system =
                if kind = SocketKind.Stream then
                    match UnixSocket.listen target 8 system with
                    | Ok (ListenAnswer.Listening _, system) -> system
                    | other -> failwith $"%s{line}: listen answered %A{other}"
                else
                    system

            let fd, system = create kind system
            let bound, system = bindTo fd (InternetEndpoint.ofParts iface 0us) system

            let system =
                if m.Groups.["state"].Value = "connected" then
                    match connectTo fd peerAt system with
                    | Ok (ConnectOutcome.Completed, system) -> system
                    | other -> failwith $"%s{line}: the first connect answered %A{other}"
                else
                    system

            let destination =
                match m.Groups.["dest"].Value with
                | "127.0.0.1" -> InternetEndpoint.ofParts InternetEndpoint.LoopbackAddress 6000us
                | _ -> InternetEndpoint.ofParts 0u 6000us

            match connectTo fd destination system with
            | Ok (ConnectOutcome.Completed, after) ->
                m.Groups.["ans"].Value |> shouldEqual "OK"
                let socket = socketOf fd after
                let now = local socket

                let peerName =
                    match socket.Phase with
                    | SocketPhase.Established connection ->
                        name (UnixMachineState.connection connection after.Machine).ServerAddress.Address
                    | SocketPhase.DatagramPeer endpoint -> name endpoint.Address
                    | _ -> "none"

                (name now.Address, (if now.Port = bound.Port then "same" else "other"), peerName)
                |> shouldEqual (m.Groups.["addr"].Value, m.Groups.["port"].Value, m.Groups.["peer"].Value)
            | other -> failwith $"%s{line}: answered %A{other}"

    [<Test>]
    let ``a Linux socket bound to an interface address reconnects as measured`` () : unit =
        reconnect "linux" SimulatedUnixPlatform.linuxX64

    [<Test>]
    let ``a Darwin socket bound to an interface address reconnects as measured`` () : unit =
        reconnect "darwin" SimulatedUnixPlatform.macOsArm64

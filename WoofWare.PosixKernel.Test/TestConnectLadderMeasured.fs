namespace WoofWare.PosixKernel.Test

open System
open System.IO
open System.Reflection
open System.Text.RegularExpressions
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `connect(2)` against every row two probes measured on Linux 6.18.5 and
/// Darwin 27.0.0, replayed call by call in this kernel.
///
/// `sockaddr-decoding.c` (section F) swept every family against a handful of
/// lengths on fresh sockets, and `sockaddr-connect-ladder.c` (sections G, U
/// and Z) swept every length for AF_UNSPEC and a few families, in every socket
/// state, and recorded what the call left behind: the local address, and
/// whether a peer remains. Both outputs are embedded from beside the probes,
/// and each row's range is expanded into the calls it stands for.
///
/// Every row must be answered as measured, except those this kernel refuses on
/// purpose (`refusedOnPurpose`) and those it does not yet answer as measured
/// (`knownGaps`). Both lists are asserted exactly, so a row that changes
/// either way is reported.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestConnectLadderMeasured =

    let private listenerPort : uint16 = 5000us
    let private datagramPeerPort : uint16 = 5001us
    let private closedPort : uint16 = 5002us

    [<RequireQualifiedAccess>]
    type private State =
        | Fresh
        | Bound
        | WildcardBound
        | Connected
        | Listening

    let private parseState (text : string) : State =
        match text with
        | "fresh" -> State.Fresh
        | "bound" -> State.Bound
        | "wildbound" -> State.WildcardBound
        | "connected" -> State.Connected
        | "listening" -> State.Listening
        | other -> failwith $"unknown state %s{other}"

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
            /// The errno's name, or `OK`.
            Answer : string
            /// The local address afterwards and its port's class (`0`, `same`
            /// or `new`), where the probe recorded it.
            Left : (string * string * bool) option
        }

    let private resource (name : string) : string[] =
        let assembly = Assembly.GetExecutingAssembly ()

        use stream =
            match assembly.GetManifestResourceStream $"WoofWare.PosixKernel.Test.%s{name}" with
            | null -> failwith $"embedded resource %s{name} not found"
            | stream -> stream

        use reader = new StreamReader (stream)
        reader.ReadToEnd().Split ('\n', StringSplitOptions.RemoveEmptyEntries)

    let private kindOf (text : string) : SocketKind =
        match text with
        | "stream" -> SocketKind.Stream
        | "dgram" -> SocketKind.Datagram
        | other -> failwith $"unknown kind %s{other}"

    let private loopback (port : uint16) =
        InternetEndpoint.ofParts InternetEndpoint.LoopbackAddress port

    /// The port a row's call was aimed at, by kind: a listener for a stream
    /// socket, a bound socket for a datagram one.
    let private peerPort (kind : SocketKind) : uint16 =
        match kind with
        | SocketKind.Stream -> listenerPort
        | _ -> datagramPeerPort

    let private leftAfter (m : Match) : (string * string * bool) option =
        Some (m.Groups.["addr"].Value, m.Groups.["cls"].Value, m.Groups.["peer"].Value = "yes")

    /// At most this many families out of a range, evenly spaced and always with
    /// both ends: the F rows span up to 65536 families.
    let private familySample (low : int) (high : int) : int list =
        if high - low < 300 then
            [ low..high ]
        else
            let step = (high - low) / 64
            [ low..step..high ] @ [ high ] |> List.distinct

    let private calls (flavour : string) : Call list =
        let decodingRows = resource $"sockaddr-decoding.%s{flavour}.txt"
        let ladderRows = resource $"sockaddr-connect-ladder.%s{flavour}.txt"

        let f =
            Regex @"^F connect/(?<kind>\w+)\s+len=(?<len>\d+)\s+family (?<lo>\d+)\.\.(?<hi>\d+) \([^)]*\): (?<ans>\w+)$"

        let g =
            Regex
                @"^G stream family=(?<fam>\d+)\s+fresh\s+len (?<lo>\d+)\.\.(?<hi>\d+): (?<ans>\w+) local=(?<addr>[\d.]+):(?<cls>\w+) peer=(?<peer>\w+)$"

        let u =
            Regex
                @"^U (?<kind>\w+) AF_UNSPEC (?<dest>\S+)\s+(?<state>\w+)\s+len (?<lo>\d+)\.\.(?<hi>\d+): (?<ans>\w+) local=(?<addr>[\d.]+):(?<cls>\w+) peer=(?<peer>\w+)$"

        let z =
            Regex
                @"^Z (?<kind>\w+)\s+(?<fam>AF_INET|AF_UNSPEC)\s+(?<dest>\S+)\s+(?<state>\w+)\s*: (?<ans>\w+) local=(?<addr>[\d.]+):(?<cls>\w+) peer=(?<peer>\w+)$"

        let fromDecoding =
            decodingRows
            |> Array.toList
            |> List.collect (fun line ->
                let m = f.Match line

                if not m.Success then
                    []
                else

                let kind = kindOf m.Groups.["kind"].Value

                let endpoint =
                    match kind with
                    | SocketKind.Stream -> loopback closedPort
                    | _ -> loopback datagramPeerPort

                familySample (int m.Groups.["lo"].Value) (int m.Groups.["hi"].Value)
                |> List.map (fun family ->
                    {
                        Source = line
                        Kind = kind
                        State = State.Fresh
                        Family = family
                        Endpoint = endpoint
                        Length = uint32 m.Groups.["len"].Value
                        Answer = m.Groups.["ans"].Value
                        Left = None
                    }
                )
            )

        let fromLadder =
            ladderRows
            |> Array.toList
            |> List.collect (fun line ->
                let lengths (m : Match) =
                    [ uint32 m.Groups.["lo"].Value .. uint32 m.Groups.["hi"].Value ]

                let gm = g.Match line
                let um = u.Match line
                let zm = z.Match line

                if gm.Success then
                    lengths gm
                    |> List.map (fun length ->
                        {
                            Source = line
                            Kind = SocketKind.Stream
                            State = State.Fresh
                            Family = int gm.Groups.["fam"].Value
                            Endpoint = loopback closedPort
                            Length = length
                            Answer = gm.Groups.["ans"].Value
                            Left = leftAfter gm
                        }
                    )
                elif um.Success then
                    let kind = kindOf um.Groups.["kind"].Value

                    let endpoint =
                        if um.Groups.["dest"].Value = "zero" then
                            InternetEndpoint.ofParts 0u 0us
                        else
                            loopback (peerPort kind)

                    lengths um
                    |> List.map (fun length ->
                        {
                            Source = line
                            Kind = kind
                            State = parseState um.Groups.["state"].Value
                            Family = 0
                            Endpoint = endpoint
                            Length = length
                            Answer = um.Groups.["ans"].Value
                            Left = leftAfter um
                        }
                    )
                elif zm.Success then
                    let kind = kindOf zm.Groups.["kind"].Value

                    let endpoint =
                        match zm.Groups.["dest"].Value with
                        | "0.0.0.0:0" -> InternetEndpoint.ofParts 0u 0us
                        | "127.0.0.1:0" -> loopback 0us
                        | "0.0.0.0:lst" -> InternetEndpoint.ofParts 0u (peerPort kind)
                        | "127.0.0.1:lst" -> loopback (peerPort kind)
                        | "127.0.0.2:0" -> InternetEndpoint.ofParts 0x7F000002u 0us
                        | "8.8.8.8:0" -> InternetEndpoint.ofParts 0x08080808u 0us
                        | other -> failwith $"unknown destination %s{other}"

                    [
                        {
                            Source = line
                            Kind = kind
                            State = parseState zm.Groups.["state"].Value
                            Family =
                                if zm.Groups.["fam"].Value = "AF_INET" then
                                    SimulatedUnixPlatform.internetAddressFamily
                                else
                                    0
                            Endpoint = endpoint
                            Length = 16u
                            Answer = zm.Groups.["ans"].Value
                            Left = leftAfter zm
                        }
                    ]
                elif line.StartsWith "#" then
                    []
                else
                    failwith $"unparsed probe line: %s{line}"
            )

        fromDecoding @ fromLadder

    let private linuxCalls : Call list = calls "linux"
    let private darwinCalls : Call list = calls "darwin"

    /// The machine every call is made in: a listener at 127.0.0.1:5000, a
    /// datagram socket bound at 127.0.0.1:5001, and nothing at 5002; and the
    /// socket the call is made on, in `state`.
    let private setUp
        (platform : SimulatedUnixPlatform)
        (kind : SocketKind)
        (state : State)
        : int * UnixSystem<int, string>
        =
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

    let private socketOf (fd : int) (system : UnixSystem<int, string>) : SocketDescription =
        match FileDescriptorRegistry.tryFindTarget fd system.Process.FileDescriptors with
        | Some (OpenFileTarget.Socket socketId) -> UnixMachineState.socket socketId system.Machine
        | other -> failwith $"fd %d{fd} names %A{other}"

    let private local (socket : SocketDescription) : InternetEndpoint =
        match socket.Binding with
        | Some binding -> binding.Endpoint
        | None -> InternetEndpoint.ofParts 0u 0us

    /// Whether `getpeername(2)` would find a peer: a stream socket's connection,
    /// or a datagram socket's peer filter with a port, since a filter of port 0
    /// is one `getpeername` answers ENOTCONN for.
    let private hasPeer (socket : SocketDescription) : bool =
        match socket.Phase with
        | SocketPhase.Established _
        | SocketPhase.EstablishedPendingReport _ -> true
        | SocketPhase.DatagramPeer endpoint -> endpoint.Port <> 0us
        | _ -> false

    let private dotted (address : uint32) : string =
        $"%d{address >>> 24}.%d{(address >>> 16) &&& 0xFFu}.%d{(address >>> 8) &&& 0xFFu}.%d{address &&& 0xFFu}"

    /// What this kernel answers for `call`, in the probe's words.
    let private replay (platform : SimulatedUnixPlatform) (call : Call) : string =
        if call.Kind = SocketKind.Datagram && call.State = State.Listening then
            failwith "a datagram socket does not listen"

        let fd, system = setUp platform call.Kind call.State
        let before = local (socketOf fd system)

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

            match call.Left with
            | None -> answer
            | Some _ ->

            let socket = socketOf fd after
            let now = local socket

            let portClass =
                if now.Port = 0us then "0"
                elif now.Port = before.Port then "same"
                else "new"

            let peer = if hasPeer socket then "yes" else "no"
            $"%s{answer} local=%s{dotted now.Address}:%s{portClass} peer=%s{peer}"

    let private expected (call : Call) : string =
        match call.Left with
        | None -> call.Answer
        | Some (address, portClass, peer) ->
            let peer = if peer then "yes" else "no"
            $"%s{call.Answer} local=%s{address}:%s{portClass} peer=%s{peer}"

    /// The rows this kernel refuses rather than answers, by the probe line they
    /// came from: every call that line stands for is refused, with the case
    /// named. Each is a state whose consequences beyond the call's own answer
    /// are not measured.
    let private refusedOnPurpose (flavour : string) : (string * string) list =
        match flavour with
        | "linux" ->
            [
                // tcp_disconnect's effect on a connected socket's peer, and on
                // a listener's queue, is not measured.
                "U stream AF_UNSPEC 127.0.0.1:lst connected len 2..128", "refused:LinuxUnspecOnPhase"
                "U stream AF_UNSPEC zero          connected len 2..128", "refused:LinuxUnspecOnPhase"
                "U stream AF_UNSPEC 127.0.0.1:lst listening len 2..128", "refused:LinuxUnspecOnPhase"
                "U stream AF_UNSPEC zero          listening len 2..128", "refused:LinuxUnspecOnPhase"
                "Z stream AF_UNSPEC 0.0.0.0:0     connected", "refused:LinuxUnspecOnPhase"
                "Z stream AF_UNSPEC 0.0.0.0:0     listening", "refused:LinuxUnspecOnPhase"
                "Z stream AF_UNSPEC 127.0.0.1:0   connected", "refused:LinuxUnspecOnPhase"
                "Z stream AF_UNSPEC 127.0.0.1:0   listening", "refused:LinuxUnspecOnPhase"
                "Z stream AF_UNSPEC 0.0.0.0:lst   connected", "refused:LinuxUnspecOnPhase"
                "Z stream AF_UNSPEC 0.0.0.0:lst   listening", "refused:LinuxUnspecOnPhase"
                "Z stream AF_UNSPEC 127.0.0.1:lst connected", "refused:LinuxUnspecOnPhase"
                "Z stream AF_UNSPEC 127.0.0.1:lst listening", "refused:LinuxUnspecOnPhase"
            ]
        | _ -> []

    /// Rows this kernel does not yet answer as measured, by probe line, with
    /// what it answers instead: an AF_INET datagram `connect` to the wildcard
    /// address or to port 0. Gaps to close, each a row of the probe.
    let private knownGaps (flavour : string) : (string * string) list =
        let toWildcard =
            [
                for destination in [ "0.0.0.0:0" ; "0.0.0.0:lst" ] do
                    for state in [ "fresh" ; "bound" ; "wildbound" ; "connected" ] do
                        $"Z dgram AF_INET %s{destination} %s{state}", "refused:DatagramConnectToWildcard"
            ]

        match flavour with
        | "darwin" ->
            // Measured EADDRNOTAVAIL, and a connected socket's peer goes first.
            toWildcard
            @ [
                "Z dgram AF_INET 127.0.0.1:0 fresh", "OK local=127.0.0.1:new peer=no"
                "Z dgram AF_INET 127.0.0.1:0 bound", "OK local=127.0.0.1:same peer=no"
                "Z dgram AF_INET 127.0.0.1:0 wildbound", "OK local=127.0.0.1:same peer=no"
                "Z dgram AF_INET 127.0.0.1:0 connected", "OK local=127.0.0.1:same peer=no"
                for destination in [ "127.0.0.2:0" ; "8.8.8.8:0" ] do
                    for state in [ "fresh" ; "bound" ; "wildbound" ; "connected" ] do
                        $"Z dgram AF_INET %s{destination} %s{state}", "refused:DestinationNotLocal"
            ]
        | _ -> toWildcard

    /// Every mismatch, one per probe line, with the distinct answers this
    /// kernel gave across the calls that line stands for.
    let private mismatches (platform : SimulatedUnixPlatform) (all : Call list) : (string * string) list =
        all
        |> List.map (fun call -> call, replay platform call)
        |> List.filter (fun (call, actual) -> actual <> expected call)
        |> List.groupBy (fun (call, _) -> call.Source)
        |> List.map (fun (source, rows) ->
            // Everything before the answer, which every probe line introduces
            // with its first ": ", with its padding collapsed.
            let key =
                Regex.Replace(source.Substring (0, source.IndexOf ": "), " +", " ").TrimEnd ()

            key, (rows |> List.map snd |> List.distinct |> String.concat " | ")
        )

    let private check (flavour : string) (platform : SimulatedUnixPlatform) (all : Call list) : unit =
        let actual = mismatches platform all |> Set.ofList

        let allowed =
            refusedOnPurpose flavour @ knownGaps flavour
            |> List.map (fun (line, answer) -> Regex.Replace (line, " +", " "), answer)
            |> Set.ofList

        if actual <> allowed then
            let unexpected = Set.difference actual allowed |> Set.toList
            let missing = Set.difference allowed actual |> Set.toList

            let show (rows : (string * string) list) =
                rows
                |> List.map (fun (line, answer) -> $"  %s{line}  =>  %s{answer}")
                |> String.concat "\n"

            failwith
                $"%s{flavour}: %d{List.length unexpected} probe lines answered otherwise than measured:\n%s{show unexpected}\n%d{List.length missing} listed lines now answered as measured or differently:\n%s{show missing}"

    [<Test>]
    let ``every Linux row is answered as measured`` () : unit =
        linuxCalls |> List.isEmpty |> shouldEqual false
        check "linux" SimulatedUnixPlatform.linuxX64 linuxCalls

    [<Test>]
    let ``every Darwin row is answered as measured`` () : unit =
        darwinCalls |> List.isEmpty |> shouldEqual false
        check "darwin" SimulatedUnixPlatform.macOsArm64 darwinCalls

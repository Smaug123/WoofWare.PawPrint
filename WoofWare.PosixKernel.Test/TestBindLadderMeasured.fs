namespace WoofWare.PosixKernel.Test

open System.Text.RegularExpressions
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `bind(2)` against every row two probes measured on Linux 6.18.5 and Darwin
/// 27.0.0, replayed call by call in this kernel.
///
/// `sockaddr-decoding.c` (section F) swept every family against a handful of
/// lengths on fresh sockets, and `sockaddr-bind-ladder.c` (sections G, S, M and
/// Z) swept every length for a handful of families and for broadcast and
/// multicast addresses, in every socket state, and recorded the local address
/// each call left. Both outputs are embedded from beside the probes, and each
/// row's range is expanded into the calls it stands for.
///
/// Every row must be answered as measured, except those this kernel refuses on
/// purpose (`refusedOnPurpose`). That list is asserted exactly, so a row that
/// changes either way is reported.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestBindLadderMeasured =

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
            /// The local address afterwards and its port's class (`0`, `same`,
            /// `asked` or `new`), where the probe recorded it.
            Left : (string * string) option
        }

    let private address (text : string) : uint32 =
        match text with
        | "0.0.0.0" -> 0u
        | "127.0.0.1" -> InternetEndpoint.LoopbackAddress
        | "8.8.8.8" -> 0x08080808u
        | "224.0.0.1" -> 0xE0000001u
        | "255.255.255.255" -> System.UInt32.MaxValue
        | other -> failwith $"unknown address %s{other}"

    let private calls (flavour : string) (platform : SimulatedUnixPlatform) : Call list =
        let decodingRows = resource $"sockaddr-decoding.%s{flavour}.txt"
        let ladderRows = resource $"sockaddr-bind-ladder.%s{flavour}.txt"
        let inet6 = SimulatedUnixPlatform.internetV6AddressFamily platform

        let f =
            Regex @"^F bind/(?<kind>\w+)\s+len=(?<len>\d+)\s+family (?<lo>\d+)\.\.(?<hi>\d+) \([^)]*\): (?<ans>\w+)$"

        let gs =
            Regex
                @"^(?<sec>[GS]) (?<kind>\w+) family=(?<fam>\d+)\s+(?<state>\w+)\s+len (?<lo>\d+)\.\.(?<hi>\d+): (?<ans>\w+) local=(?<addr>[\d.]+):(?<cls>\w+)$"

        let m =
            Regex
                @"^M (?<kind>\w+) (?<fam>AF_INET|AF_UNSPEC) (?<addr0>[\d.]+)\s+(?<state>\w+)\s+len (?<lo>\d+)\.\.(?<hi>\d+): (?<ans>\w+) local=(?<addr>[\d.]+):(?<cls>\w+)$"

        let z =
            Regex
                @"^Z (?<kind>\w+)\s+(?<fam>AF_INET6|AF_INET|AF_UNSPEC)\s+(?<addr0>[\d.]+)\s+(?<port>0|P)\s+(?<state>\w+)\s*: (?<ans>\w+) local=(?<addr>[\d.]+):(?<cls>\w+)$"

        let familyNamed (name : string) : int =
            match name with
            | "AF_INET" -> SimulatedUnixPlatform.internetAddressFamily
            | "AF_UNSPEC" -> 0
            | "AF_INET6" -> inet6
            | other -> failwith $"unknown family %s{other}"

        let left (m : Match) =
            Some (m.Groups.["addr"].Value, m.Groups.["cls"].Value)

        let lengths (m : Match) =
            [ uint32 m.Groups.["lo"].Value .. uint32 m.Groups.["hi"].Value ]

        let fromDecoding =
            decodingRows
            |> Array.toList
            |> List.collect (fun line ->
                let fm = f.Match line

                if not fm.Success then
                    []
                else

                familySample (int fm.Groups.["lo"].Value) (int fm.Groups.["hi"].Value)
                |> List.map (fun family ->
                    {
                        Source = line
                        Kind = kindOf fm.Groups.["kind"].Value
                        State = State.Fresh
                        Family = family
                        Endpoint = loopback 0us
                        Length = uint32 fm.Groups.["len"].Value
                        Answer = fm.Groups.["ans"].Value
                        Left = None
                    }
                )
            )

        let fromLadder =
            ladderRows
            |> Array.toList
            |> List.collect (fun line ->
                let gsm = gs.Match line
                let mm = m.Match line
                let zm = z.Match line

                if gsm.Success then
                    lengths gsm
                    |> List.map (fun length ->
                        {
                            Source = line
                            Kind = kindOf gsm.Groups.["kind"].Value
                            State = parseState gsm.Groups.["state"].Value
                            Family = int gsm.Groups.["fam"].Value
                            Endpoint = loopback closedPort
                            Length = length
                            Answer = gsm.Groups.["ans"].Value
                            Left = left gsm
                        }
                    )
                elif mm.Success then
                    lengths mm
                    |> List.map (fun length ->
                        {
                            Source = line
                            Kind = kindOf mm.Groups.["kind"].Value
                            State = parseState mm.Groups.["state"].Value
                            Family = familyNamed mm.Groups.["fam"].Value
                            Endpoint = InternetEndpoint.ofParts (address mm.Groups.["addr0"].Value) closedPort
                            Length = length
                            Answer = mm.Groups.["ans"].Value
                            Left = left mm
                        }
                    )
                elif zm.Success then
                    [
                        {
                            Source = line
                            Kind = kindOf zm.Groups.["kind"].Value
                            State = parseState zm.Groups.["state"].Value
                            Family = familyNamed zm.Groups.["fam"].Value
                            Endpoint =
                                InternetEndpoint.ofParts
                                    (address zm.Groups.["addr0"].Value)
                                    (if zm.Groups.["port"].Value = "P" then closedPort else 0us)
                            Length = 16u
                            Answer = zm.Groups.["ans"].Value
                            Left = left zm
                        }
                    ]
                elif line.StartsWith "#" then
                    []
                else
                    failwith $"unparsed probe line: %s{line}"
            )

        fromDecoding @ fromLadder

    /// What this kernel answers for `call`, in the probe's words.
    let private replay (platform : SimulatedUnixPlatform) (call : Call) : string =
        let fd, system = setUp platform call.Kind call.State
        let before = local (socketOf fd system)

        match CopyIn.bind fd UserBuffer.Mapped call.Length (CopyIn.blob platform call.Family call.Endpoint) system with
        | Error refusal ->
            let name = (sprintf "%A" refusal).Split([| ' ' ; '\n' |]).[0]
            $"refused:%s{name}"
        | Ok (answer, after) ->
            let answer =
                match answer with
                | BindAnswer.Bound _ -> "OK"
                | BindAnswer.Failed error -> string error

            match call.Left with
            | None -> answer
            | Some _ ->

            let now = local (socketOf fd after)

            let portClass =
                if now.Port = 0us then "0"
                elif now.Port = before.Port then "same"
                elif now.Port = call.Endpoint.Port then "asked"
                else "new"

            $"%s{answer} local=%s{dotted now.Address}:%s{portClass}"

    let private expected (call : Call) : string =
        match call.Left with
        | None -> call.Answer
        | Some (address, portClass) -> $"%s{call.Answer} local=%s{address}:%s{portClass}"

    /// The rows this kernel refuses rather than answers, by the probe line they
    /// came from: a bind of a broadcast or multicast address that would
    /// succeed, which this kernel does not record because nothing downstream
    /// could honour it.
    let private refusedOnPurpose (flavour : string) : string list =
        match flavour with
        | "linux" ->
            [
                for kind in [ "stream" ; "dgram" ] do
                    for group in [ "224.0.0.1" ; "255.255.255.255" ] do
                        $"M %s{kind} AF_INET %s{group} fresh len 16..128"

                        for port in [ "0" ; "P" ] do
                            $"Z %s{kind} AF_INET %s{group} %s{port} fresh"
            ]
        | _ ->
            [
                "M dgram AF_INET 224.0.0.1 fresh len 16..16"
                "M dgram AF_UNSPEC 224.0.0.1 fresh len 16..16"
                for family in [ "AF_INET" ; "AF_UNSPEC" ; "AF_INET6" ] do
                    for port in [ "0" ; "P" ] do
                        $"Z dgram %s{family} 224.0.0.1 %s{port} fresh"
            ]

    let private check (flavour : string) (platform : SimulatedUnixPlatform) : unit =
        let all = calls flavour platform
        all |> List.isEmpty |> shouldEqual false

        let actual =
            all
            |> List.map (fun call -> call, replay platform call)
            |> List.filter (fun (call, answer) -> answer <> expected call)
            |> List.groupBy (fun (call, _) -> call.Source)
            |> List.map (fun (source, rows) ->
                // Everything before the answer, which every probe line
                // introduces with its first ": ", with its padding collapsed.
                Regex.Replace(source.Substring (0, source.IndexOf ": "), " +", " ").TrimEnd (),
                rows |> List.map snd |> List.distinct |> String.concat " | "
            )

        let allowed =
            refusedOnPurpose flavour
            |> List.map (fun line -> line, "refused:UnmodelledMulticast")
            |> Set.ofList

        let actual = Set.ofList actual

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

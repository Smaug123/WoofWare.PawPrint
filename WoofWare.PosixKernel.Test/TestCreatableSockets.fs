namespace WoofWare.PosixKernel.Test

open System
open System.IO
open System.Reflection
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `SimulatedUnixPlatform.creatableSockets` is this kernel's declared protocol
/// table: which sockets it will make, and by omission which it refuses. Its
/// oracle is the measurement in `socketMatrix/`, taken by calling `socket(2)`
/// on each real platform, rather than anything in this repo — a table checked
/// against a copy of itself would catch a later typo and nothing else.
///
/// The measurement is keyed by the symbolic names `socket(2)` itself uses
/// (`INET`, `SOCK_STREAM`, `TCP`), which is why this can read it without
/// meeting any client's numbering of them.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestCreatableSockets =

    let private assy = Assembly.GetExecutingAssembly ()

    /// The triple a measured row names, where this library has words for all
    /// three. `None` where it does not: `AF_PACKET`, `SOCK_RDM` and every
    /// protocol but the three modelled ones are shapes no socket here can be,
    /// which is a different thing from a shape the table omits.
    let private shapeOf
        (family : string)
        (kind : string)
        (protocol : string)
        : (SocketDomain * SocketKind * SocketProtocol) option
        =
        let domain =
            match family with
            | "INET" -> Some SocketDomain.InterNetwork
            | "INET6" -> Some SocketDomain.InterNetworkV6
            | "UNIX" -> Some SocketDomain.Unix
            | _ -> None

        let kind =
            match kind with
            | "STREAM" -> Some SocketKind.Stream
            | "DGRAM" -> Some SocketKind.Datagram
            | "RAW" -> Some SocketKind.Raw
            | "SEQPACKET" -> Some SocketKind.SeqPacket
            | _ -> None

        let protocol =
            match protocol with
            | "UNSPEC" -> Some SocketProtocol.Unspecified
            | "TCP" -> Some SocketProtocol.Tcp
            | "UDP" -> Some SocketProtocol.Udp
            | _ -> None

        match domain, kind, protocol with
        | Some domain, Some kind, Some protocol -> Some (domain, kind, protocol)
        | _ -> None

    /// The triples `socket(2)` really created, and those it created that this
    /// library has no word for, kept apart.
    let private measuredCreated
        (flavourFile : string)
        : Set<SocketDomain * SocketKind * SocketProtocol> * (string * string * string) list
        =
        let resource = $"WoofWare.PosixKernel.Test.socketMatrix.%s{flavourFile}"

        use stream =
            match assy.GetManifestResourceStream resource with
            | null -> failwith $"embedded resource %s{resource} is missing"
            | stream -> stream

        use reader = new StreamReader (stream)

        let created =
            reader.ReadToEnd().Split '\n'
            |> Array.toList
            |> List.filter (fun line ->
                not (String.IsNullOrWhiteSpace line)
                && not (line.StartsWith ("#", StringComparison.Ordinal))
            )
            |> List.choose (fun line ->
                match line.Split '\t' with
                | [| family ; kind ; protocol ; "OK" ; _ |] -> Some (family, kind, protocol)
                | [| _ ; _ ; _ ; _ ; _ |] -> None
                | _ -> failwith $"malformed row in %s{resource}: %s{line}"
            )

        let named, unnamed =
            created
            |> List.partition (fun (family, kind, protocol) -> (shapeOf family kind protocol).IsSome)

        let shapes =
            named
            |> List.map (fun (family, kind, protocol) ->
                match shapeOf family kind protocol with
                | Some shape -> shape
                | None -> failwith "unreachable: partitioned on this"
            )
            |> Set.ofList

        if List.length named <> Set.count shapes then
            failwith $"%s{resource} has two OK rows naming one shape, so this measurement cannot be a set"

        shapes, unnamed

    [<Test>]
    let ``the Linux table is exactly what Linux created`` () : unit =
        let created, unnamed = measuredCreated "linux.tsv"

        unnamed |> shouldEqual []

        SimulatedUnixPlatform.creatableSockets SimulatedUnixPlatform.linuxX64
        |> shouldEqual created

    /// Darwin creates three sockets this library refuses, and the refusal is in
    /// the *representation*: they are ICMP datagram sockets, and
    /// `SocketProtocol` has no `ICMP`. Asserting the whole set rather than
    /// skipping them means a fourth appearing is a failure rather than a silent
    /// divergence.
    ///
    /// Modelling them would need more than a word. Linux gates the same three
    /// behind `net.ipv4.ping_group_range` and refuses here, so a `SocketProtocol
    /// .Icmp` would make the Darwin flavour create a socket the Linux flavour
    /// does not, for a sysctl reason this kernel has nowhere to put.
    [<Test>]
    let ``the Darwin table is what Darwin created, less the ping sockets`` () : unit =
        let created, unnamed = measuredCreated "darwin.tsv"

        unnamed
        |> List.sort
        |> shouldEqual
            [
                "INET", "DGRAM", "ICMP"
                "INET6", "DGRAM", "ICMP"
                "INET6", "DGRAM", "ICMPV6"
            ]

        SimulatedUnixPlatform.creatableSockets SimulatedUnixPlatform.macOsArm64
        |> shouldEqual created

    /// The flavours' relationship, stated where a reader will look for it: the
    /// Darwin kernel creates strictly fewer of the shapes this library names,
    /// and the difference is entirely `AF_UNIX`.
    [<Test>]
    let ``Darwin creates a strict subset of what Linux creates`` () : unit =
        let linux = SimulatedUnixPlatform.creatableSockets SimulatedUnixPlatform.linuxX64
        let darwin = SimulatedUnixPlatform.creatableSockets SimulatedUnixPlatform.macOsArm64

        Set.isProperSubset darwin linux |> shouldEqual true

        Set.difference linux darwin
        |> shouldEqual (
            Set.ofList
                [
                    SocketDomain.Unix, SocketKind.Raw, SocketProtocol.Unspecified
                    SocketDomain.Unix, SocketKind.SeqPacket, SocketProtocol.Unspecified
                ]
        )

    /// A protocol names a kind, and a table that paired them the other way
    /// would be a socket no kernel makes. This holds for a reason the
    /// measurement cannot state — it is a fact about what the words mean — so
    /// it is asserted rather than derived.
    [<Test>]
    let ``no table pairs a protocol with the wrong kind`` () : unit =
        for platform in [ SimulatedUnixPlatform.linuxX64 ; SimulatedUnixPlatform.macOsArm64 ] do
            for domain, kind, protocol in SimulatedUnixPlatform.creatableSockets platform do
                match protocol with
                | SocketProtocol.Tcp ->
                    if kind <> SocketKind.Stream then
                        failwith $"%O{platform} creates %O{domain}/%O{kind} with TCP, which is not a stream protocol"
                | SocketProtocol.Udp ->
                    if kind <> SocketKind.Datagram then
                        failwith $"%O{platform} creates %O{domain}/%O{kind} with UDP, which is not a datagram protocol"
                | SocketProtocol.Unspecified -> ()

                // A local socket carries no internet protocol: `AF_UNIX` with
                // anything but the default is refused by the shim's own
                // conversion long before this table.
                if domain = SocketDomain.Unix && protocol <> SocketProtocol.Unspecified then
                    failwith $"%O{platform} creates a Unix-domain socket with protocol %O{protocol}"

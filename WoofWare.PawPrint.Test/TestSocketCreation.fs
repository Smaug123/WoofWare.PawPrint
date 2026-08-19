namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Reflection
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `SimulatedUnixPlatform.socketCreation` is a transcription of two things at
/// once: the native shim's three argument screens, which are C we can read, and
/// the set of sockets each kernel will actually make, which is not. So the
/// oracle here is a *measurement* rather than a restatement.
///
/// `socketMatrix/linux.tsv` and `socketMatrix/darwin.tsv` were produced by
/// sweeping all 330 PAL triples through a C program that mirrors the shim's
/// conversions and then calls `socket(2)`, run on a real Linux 6.18.5 container
/// and on a real macOS 25.6.0 host. Each row records which of the shim's screens
/// refused it, or what the syscall answered. See the files' own headers for the
/// method and for why the Linux sweep was taken at euid 1000.
///
/// The correspondence being checked is total, which is what makes this more than
/// a spot check: every row must map onto exactly one classifier answer, and a
/// transcription slip anywhere in the per-family protocol tables shows up as a
/// row whose screen fires in PawPrint and not in the measurement, or the other
/// way round.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestSocketCreation =

    let private assy = Assembly.GetExecutingAssembly ()

    /// PAL numbering, from `pal_networking.h`. Spelled out here rather than
    /// reached through the implementation, so that a slip in the implementation's
    /// own constants cannot cancel out against this test.
    let private palAddressFamily : Map<string, int> =
        [
            "UNSPEC", 0
            "UNIX", 1
            "INET", 2
            "INET6", 23
            "PACKET", 65536
            "CAN", 65537
        ]
        |> Map.ofList

    let private palSocketType : Map<string, int> =
        [ "STREAM", 1 ; "DGRAM", 2 ; "RAW", 3 ; "RDM", 4 ; "SEQPACKET", 5 ]
        |> Map.ofList

    let private palProtocolType : Map<string, int> =
        [
            "UNSPEC", 0
            "ICMP", 1
            "IGMP", 2
            "TCP", 6
            "UDP", 17
            "ROUTING", 43
            "FRAGMENT", 44
            "ICMPV6", 58
            "NONE", 59
            "DSTOPTS", 60
            "RAW", 255
        ]
        |> Map.ofList

    /// One measured row: the triple, and what really happened to it.
    type private MeasuredRow =
        {
            Family : string
            Kind : string
            Protocol : string
            /// `SCREEN <name>`, `OK`, or `SYSCALL <errno>`, verbatim.
            Outcome : string
            Detail : string
        }

    let private rows (flavourFile : string) : MeasuredRow list =
        let resource = $"WoofWare.PawPrint.Test.socketMatrix.%s{flavourFile}"

        use stream =
            match assy.GetManifestResourceStream resource with
            | null -> failwith $"embedded resource %s{resource} is missing"
            | stream -> stream

        use reader = new StreamReader (stream)

        reader.ReadToEnd().Split '\n'
        |> Array.toList
        |> List.filter (fun line ->
            not (String.IsNullOrWhiteSpace line)
            && not (line.StartsWith ("#", StringComparison.Ordinal))
        )
        |> List.map (fun line ->
            match line.Split '\t' with
            | [| family ; kind ; protocol ; outcome ; detail |] ->
                {
                    Family = family
                    Kind = kind
                    Protocol = protocol
                    Outcome = outcome
                    Detail = detail
                }
            | _ -> failwith $"malformed row in %s{resource}: %s{line}"
        )

    /// What the classifier must answer for a measured row, as a string, so that a
    /// disagreement reports both sides in the same vocabulary.
    let private expected (row : MeasuredRow) : string =
        match row.Outcome with
        | "SCREEN" ->
            match row.Detail with
            | "EAFNOSUPPORT" -> "AddressFamily"
            | "EPROTOTYPE" -> "SocketType"
            | "EPROTONOSUPPORT" -> "Protocol"
            | other -> failwith $"unknown screen %s{other}"
        | "OK" -> "Ok"
        // Every row that reached `socket(2)` and failed is a socket PawPrint has
        // not modelled. The errno is deliberately not compared: PawPrint reports
        // none of them, and that refusal is the design (see `socketCreation`).
        | "SYSCALL" -> "Unmodelled"
        | other -> failwith $"unknown outcome %s{other}"

    let private actual (platform : SimulatedUnixPlatform) (row : MeasuredRow) : string =
        let result =
            SimulatedUnixPlatform.socketCreation
                platform
                palAddressFamily.[row.Family]
                palSocketType.[row.Kind]
                palProtocolType.[row.Protocol]

        match result with
        | Ok _ -> "Ok"
        | Error SocketCreationRefusal.AddressFamily -> "AddressFamily"
        | Error SocketCreationRefusal.SocketType -> "SocketType"
        | Error SocketCreationRefusal.Protocol -> "Protocol"
        | Error SocketCreationRefusal.Unmodelled -> "Unmodelled"

    let private disagreements (platform : SimulatedUnixPlatform) (flavourFile : string) : string list =
        let measured = rows flavourFile
        measured |> List.length |> shouldEqual 330

        measured
        |> List.choose (fun row ->
            let expected = expected row
            let actual = actual platform row

            if expected = actual then
                None
            else
                Some $"%s{row.Family}/%s{row.Kind}/%s{row.Protocol}: measured %s{expected}, PawPrint %s{actual}"
        )

    [<Test>]
    let ``the Linux flavour agrees with the Linux measurement on every triple`` () : unit =
        disagreements SimulatedUnixPlatform.linuxX64 "linux.tsv" |> shouldEqual []

    /// Darwin has exactly three rows PawPrint refuses although the platform
    /// creates them, and this asserts the *whole* set rather than skipping them,
    /// so that a fourth one appearing is a failure rather than a silent
    /// divergence.
    ///
    /// All three are ICMP datagram sockets, which Darwin hands to any user.
    /// Linux gates the same three behind `net.ipv4.ping_group_range` and answers
    /// EACCES here, so modelling them would mean the *Darwin* flavour creating a
    /// socket the Linux flavour refuses, for a reason (a sysctl) that PawPrint
    /// has nowhere to put. `SocketProtocol` cannot represent `PT_ICMP` at all,
    /// so the refusal is in the representation and not only in the classifier.
    ///
    /// Nothing is lost by refusing: an ICMP socket is unreachable without
    /// send/receive, which no entry point offers yet. Whoever implements those
    /// should revisit this together with the sysctl.
    [<Test>]
    let ``the Darwin flavour agrees with the Darwin measurement but for the ping sockets`` () : unit =
        disagreements SimulatedUnixPlatform.macOsArm64 "darwin.tsv"
        |> shouldEqual
            [
                "INET/DGRAM/ICMP: measured Ok, PawPrint Unmodelled"
                "INET6/DGRAM/ICMP: measured Ok, PawPrint Unmodelled"
                "INET6/DGRAM/ICMPV6: measured Ok, PawPrint Unmodelled"
            ]

    /// And the Linux measurement really does refuse those same three, so the
    /// exception above is about Darwin specifically rather than about ICMP
    /// datagram sockets being unmodelled everywhere.
    [<Test>]
    let ``Linux refuses the ping sockets that Darwin creates`` () : unit =
        let linux =
            rows "linux.tsv"
            |> List.map (fun row -> (row.Family, row.Kind, row.Protocol), (row.Outcome, row.Detail))
            |> Map.ofList

        linux.[("INET", "DGRAM", "ICMP")]
        |> shouldEqual ("SYSCALL", "Permission denied(13)")

        linux.[("INET6", "DGRAM", "ICMP")]
        |> shouldEqual ("SYSCALL", "Permission denied(13)")

        linux.[("INET6", "DGRAM", "ICMPV6")]
        |> shouldEqual ("SYSCALL", "Permission denied(13)")

    /// The two flavours really do disagree, so a `socketCreation` that ignored
    /// its platform argument would fail rather than pass both tests above by
    /// coincidence. These are the rows that separate them, and each is a
    /// different *kind* of divergence.
    [<Test>]
    let ``the flavours disagree where the shim's ifdefs and the kernels do`` () : unit =
        let linux =
            rows "linux.tsv"
            |> List.map (fun row -> (row.Family, row.Kind, row.Protocol), expected row)
            |> Map.ofList

        let darwin =
            rows "darwin.tsv"
            |> List.map (fun row -> (row.Family, row.Kind, row.Protocol), expected row)
            |> Map.ofList

        // The shim's `#ifdef AF_PACKET`: refused by the address-family screen on
        // Darwin, and reaching the kernel on Linux.
        linux.[("PACKET", "DGRAM", "UNSPEC")] |> shouldEqual "Unmodelled"
        darwin.[("PACKET", "DGRAM", "UNSPEC")] |> shouldEqual "AddressFamily"

        // A kernel divergence rather than a shim one: Linux makes a
        // `SOCK_SEQPACKET` Unix-domain socket, Darwin refuses it.
        linux.[("UNIX", "SEQPACKET", "UNSPEC")] |> shouldEqual "Ok"
        darwin.[("UNIX", "SEQPACKET", "UNSPEC")] |> shouldEqual "Unmodelled"

    /// The classifier's *output* is not covered by the tables above, which only
    /// compare which answer fired. A triple that creates a socket must hand back
    /// the domain, kind and protocol it was given, per field: nothing else in
    /// this PR reads them, so a transposition here would otherwise survive until
    /// `SystemNative_GetSocketType` reported it.
    [<TestCase(2, 1, 0, "InterNetwork", "Stream", "Unspecified")>]
    [<TestCase(2, 1, 6, "InterNetwork", "Stream", "Tcp")>]
    [<TestCase(2, 2, 0, "InterNetwork", "Datagram", "Unspecified")>]
    [<TestCase(2, 2, 17, "InterNetwork", "Datagram", "Udp")>]
    [<TestCase(23, 1, 0, "InterNetworkV6", "Stream", "Unspecified")>]
    [<TestCase(23, 1, 6, "InterNetworkV6", "Stream", "Tcp")>]
    [<TestCase(23, 2, 0, "InterNetworkV6", "Datagram", "Unspecified")>]
    [<TestCase(23, 2, 17, "InterNetworkV6", "Datagram", "Udp")>]
    [<TestCase(1, 1, 0, "Unix", "Stream", "Unspecified")>]
    [<TestCase(1, 2, 0, "Unix", "Datagram", "Unspecified")>]
    [<TestCase(1, 3, 0, "Unix", "Raw", "Unspecified")>]
    [<TestCase(1, 5, 0, "Unix", "SeqPacket", "Unspecified")>]
    let ``a created socket carries the triple it was asked for``
        (family : int)
        (kind : int)
        (protocol : int)
        (expectedDomain : string)
        (expectedKind : string)
        (expectedProtocol : string)
        : unit
        =
        match SimulatedUnixPlatform.socketCreation SimulatedUnixPlatform.linuxX64 family kind protocol with
        | Error refusal -> failwith $"expected a socket, got %O{refusal}"
        | Ok (domain, socketKind, socketProtocol) ->
            sprintf "%O" domain |> shouldEqual expectedDomain
            sprintf "%O" socketKind |> shouldEqual expectedKind
            sprintf "%O" socketProtocol |> shouldEqual expectedProtocol

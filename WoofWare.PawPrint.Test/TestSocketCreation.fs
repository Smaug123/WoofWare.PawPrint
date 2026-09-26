namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Reflection
open System.Runtime.InteropServices
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PosixKernel

/// `SystemNative_Socket` is two things composed: the native shim's three
/// argument screens (`SocketArgumentsPal.socketArguments`), which are C we can
/// read, and then `socket(2)` (`UnixSocket.socket`), which is not. So the
/// oracle here is a *measurement* rather than a restatement.
///
/// `socketMatrix/{linux,darwin}.tsv` were produced by sweeping all 330
/// PAL triples through a C program that mirrors the shim's conversions and then
/// calls `socket(2)`, run on a real Linux 6.18.5 container and on a real macOS
/// 25.6.0 host. Each row records which of the shim's screens refused it, or what
/// the syscall answered. See the files' own headers for the method and for why
/// the Linux sweep was taken at euid 1000.
///
/// The kernel's half is held to a far larger sweep of its own in
/// `WoofWare.PosixKernel.Test`'s `TestSocketSyscall`; this fixture is about the
/// composition.
///
/// The correspondence being checked is total, which is what makes this more than
/// a spot check: every row must map onto exactly one answer, and a
/// transcription slip anywhere in the per-family protocol tables shows up as a
/// row whose screen fires in PawPrint and not in the measurement, or the other
/// way round.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestSocketCreation =

    let private assy = Assembly.GetExecutingAssembly ()

    [<DllImport("libSystem.Native", EntryPoint = "SystemNative_Socket")>]
    extern int private hostSocket(int addressFamily, int socketType, int protocolType, nativeint& createdSocket)

    [<DllImport("libSystem.Native", EntryPoint = "SystemNative_Close")>]
    extern int private hostClose(nativeint fd)

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

    /// What a measured row says `SystemNative_Socket` answers: `Ok`, or the
    /// name of the error it returned, whether a screen's or the kernel's.
    let private expected (platform : SimulatedUnixPlatform) (row : MeasuredRow) : string =
        match row.Outcome with
        | "SCREEN" -> row.Detail
        | "OK" -> "Ok"
        | "SYSCALL" ->
            // `<strerror text>(<raw errno>)`.
            let opening = row.Detail.LastIndexOf '('

            let raw =
                Int32.Parse (row.Detail.Substring (opening + 1, row.Detail.Length - opening - 2))

            match UnixError.ofRawErrnoUnder (SimulatedUnixPlatform.rawErrnoNumbering platform) raw with
            | Some error -> string<UnixError> error
            | None -> failwith $"row %A{row}: raw errno %d{raw} names no modelled error"
        | other -> failwith $"unknown outcome %s{other}"

    /// What PawPrint's composition answers for a PAL triple: `Ok`, the name of
    /// the error, or `Refused` where the library declines to answer.
    let private composed (platform : SimulatedUnixPlatform) (family : int) (kind : int) (protocol : int) : string =
        match SocketArgumentsPal.socketArguments platform family kind protocol with
        | Error screen -> string<UnixError> (SocketArgumentScreen.error screen)
        | Ok (domain, socketType, protocol) ->
            let system : UnixSystem<int, string> = UnixSystem.initial platform

            match UnixSocket.socket domain socketType protocol system with
            | Ok (Ok _) -> "Ok"
            | Ok (Error error) -> string<UnixError> error
            | Error _ -> "Refused"

    let private actual (platform : SimulatedUnixPlatform) (row : MeasuredRow) : string =
        composed platform palAddressFamily.[row.Family] palSocketType.[row.Kind] palProtocolType.[row.Protocol]

    /// Every row PawPrint answers differently from the measurement, and every
    /// row it refuses, each as `FAMILY/KIND/PROTOCOL: measured, PawPrint`.
    let private compare (platform : SimulatedUnixPlatform) (flavourFile : string) : string list * string list =
        let measured = rows flavourFile
        measured |> List.length |> shouldEqual 330

        let described =
            measured
            |> List.map (fun row ->
                let expected = expected platform row
                let actual = actual platform row

                actual,
                expected,
                $"%s{row.Family}/%s{row.Kind}/%s{row.Protocol}: measured %s{expected}, PawPrint %s{actual}"
            )

        let disagreements =
            described
            |> List.choose (fun (actual, expected, text) ->
                if actual <> expected && actual <> "Refused" then
                    Some text
                else
                    None
            )

        let refused =
            described
            |> List.choose (fun (actual, _, text) -> if actual = "Refused" then Some text else None)

        disagreements, refused

    /// Under Linux, PawPrint refuses exactly the rows whose answer depends on
    /// what it does not model: privilege (every raw and packet row, EPERM at
    /// the measured euid), `net.ipv4.ping_group_range` (the ICMP datagram rows,
    /// EACCES), which families the kernel was built with (`AF_CAN`, absent from
    /// the measured kernel) and whether it has SCTP (every internet seqpacket
    /// row).
    [<Test>]
    let ``the Linux flavour agrees with the Linux measurement on every triple it answers`` () : unit =
        let disagreements, refused = compare SimulatedUnixPlatform.linuxX64 "linux.tsv"
        disagreements |> shouldEqual []

        let unexpected =
            rows "linux.tsv"
            |> List.filter (fun row -> actual SimulatedUnixPlatform.linuxX64 row = "Refused")
            |> List.filter (fun row ->
                let privileged = row.Detail.EndsWith "(1)" || row.Detail.EndsWith "(13)"
                let family = row.Family = "CAN"
                let sctp = (row.Family = "INET" || row.Family = "INET6") && row.Kind = "SEQPACKET"
                not (privileged || family || sctp)
            )

        unexpected |> shouldEqual []

        // 15 internet raw rows (every protocol but the unspecified one, which
        // the kernel answers), 3 ICMP datagram rows, 55 AF_PACKET and 10 AF_CAN
        // rows, and 17 internet seqpacket rows.
        refused |> List.length |> shouldEqual 100

    /// Darwin refuses its raw rows (EPERM, or EOPNOTSUPP for the IPv6 extension
    /// headers, at the measured euid) and the three ping sockets it creates for
    /// any user, and answers every other row.
    ///
    /// Linux gates the same ping sockets behind `net.ipv4.ping_group_range` and
    /// answers EACCES here, so modelling them would mean the *Darwin* flavour
    /// creating a socket the Linux flavour refuses, for a reason (a sysctl) that
    /// PawPrint has nowhere to put. Nothing is lost by refusing: an ICMP socket
    /// is unreachable without send/receive, which no entry point offers yet.
    [<Test>]
    let ``the Darwin flavour agrees with the Darwin measurement on every triple it answers`` () : unit =
        let disagreements, refused = compare SimulatedUnixPlatform.macOsArm64 "darwin.tsv"
        disagreements |> shouldEqual []

        refused
        |> List.filter (fun text -> not (text.Contains "/RAW/"))
        |> shouldEqual
            [
                "INET/DGRAM/ICMP: measured Ok, PawPrint Refused"
                "INET6/DGRAM/ICMP: measured Ok, PawPrint Refused"
                "INET6/DGRAM/ICMPV6: measured Ok, PawPrint Refused"
            ]

        // 17 internet raw rows, and the three ping sockets.
        refused |> List.length |> shouldEqual 20

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

    /// The two flavours really do disagree, so a composition that ignored its
    /// platform would fail rather than pass both tests above by coincidence.
    /// These are rows that separate them, and each is a different *kind* of
    /// divergence.
    [<Test>]
    let ``the flavours disagree where the shim's ifdefs and the kernels do`` () : unit =
        let answer (platform : SimulatedUnixPlatform) (family : string) (kind : string) (protocol : string) =
            composed platform palAddressFamily.[family] palSocketType.[kind] palProtocolType.[protocol]

        let linux = answer SimulatedUnixPlatform.linuxX64
        let darwin = answer SimulatedUnixPlatform.macOsArm64

        // The shim's `#ifdef AF_PACKET`: refused by the address-family screen on
        // Darwin, and reaching the kernel on Linux.
        linux "PACKET" "DGRAM" "UNSPEC" |> shouldEqual "Refused"
        darwin "PACKET" "DGRAM" "UNSPEC" |> shouldEqual "EAFNOSUPPORT"

        // A kernel divergence rather than a shim one: Linux makes a
        // `SOCK_SEQPACKET` Unix-domain socket, Darwin refuses it.
        linux "UNIX" "SEQPACKET" "UNSPEC" |> shouldEqual "Ok"
        darwin "UNIX" "SEQPACKET" "UNSPEC" |> shouldEqual "EPROTONOSUPPORT"

        // The kernels' errno for one protocol under another type.
        linux "INET" "STREAM" "UDP" |> shouldEqual "EPROTONOSUPPORT"
        darwin "INET" "STREAM" "UDP" |> shouldEqual "EPROTOTYPE"

    /// A triple that creates a socket must hand the kernel the domain, kind and
    /// protocol it was given, per field: the tables above compare only which
    /// answer came back, so a transposition here would otherwise survive until
    /// something read the socket back.
    [<TestCase(2, 1, 0, "Inet", "Stream", "Default")>]
    [<TestCase(2, 1, 6, "Inet", "Stream", "Tcp")>]
    [<TestCase(2, 2, 0, "Inet", "Datagram", "Default")>]
    [<TestCase(2, 2, 17, "Inet", "Datagram", "Udp")>]
    [<TestCase(23, 1, 0, "Inet6", "Stream", "Default")>]
    [<TestCase(23, 1, 6, "Inet6", "Stream", "Tcp")>]
    [<TestCase(23, 2, 0, "Inet6", "Datagram", "Default")>]
    [<TestCase(23, 2, 17, "Inet6", "Datagram", "Udp")>]
    [<TestCase(1, 1, 0, "Unix", "Stream", "Default")>]
    [<TestCase(1, 2, 0, "Unix", "Datagram", "Default")>]
    // Linux makes a Unix-domain `SOCK_RAW` request a datagram socket.
    [<TestCase(1, 3, 0, "Unix", "Datagram", "Default")>]
    [<TestCase(1, 5, 0, "Unix", "SeqPacket", "Default")>]
    let ``a created socket carries the triple it was asked for``
        (family : int)
        (kind : int)
        (protocol : int)
        (expectedDomain : string)
        (expectedKind : string)
        (expectedProtocol : string)
        : unit
        =
        let platform = SimulatedUnixPlatform.linuxX64
        let system : UnixSystem<int, string> = UnixSystem.initial platform

        let domain, socketType, protocol =
            match SocketArgumentsPal.socketArguments platform family kind protocol with
            | Ok arguments -> arguments
            | Error screen -> failwith $"expected the screens to pass, got %O{screen}"

        match UnixSocket.socket domain socketType protocol system with
        | Ok (Ok (fd, system)) ->
            let socket =
                match FileDescriptorRegistry.tryFindTarget fd system.Process.FileDescriptors with
                | Some (OpenFileTarget.Socket socketId) -> UnixMachineState.socket socketId system.Machine
                | other -> failwith $"descriptor %d{fd} names %A{other}"

            sprintf "%O" socket.Domain |> shouldEqual expectedDomain
            sprintf "%O" socket.Kind |> shouldEqual expectedKind
            sprintf "%O" socket.Protocol |> shouldEqual expectedProtocol
            // The shim's `SOCK_CLOEXEC` has nothing to change, and nothing asked
            // for `SOCK_NONBLOCK`.
            UnixSocket.isNonBlocking fd system |> shouldEqual (Some false)
        | other -> failwith $"expected a socket, got %A{other}"

    /// The `socket(2)` arguments the shim passes, stated independently of the
    /// implementation: every family, type and protocol numbered as each
    /// platform's headers number them, and Linux's `SOCK_CLOEXEC` on every type.
    [<Test>]
    let ``the screens pass the platform's own numbers to socket(2)`` () : unit =
        let arguments (platform : SimulatedUnixPlatform) (family : string) (kind : string) (protocol : string) =
            SocketArgumentsPal.socketArguments
                platform
                palAddressFamily.[family]
                palSocketType.[kind]
                palProtocolType.[protocol]

        let linux = arguments SimulatedUnixPlatform.linuxX64
        let darwin = arguments SimulatedUnixPlatform.macOsArm64

        linux "INET6" "STREAM" "TCP" |> shouldEqual (Ok (10, 0x80001, 6))
        darwin "INET6" "STREAM" "TCP" |> shouldEqual (Ok (30, 1, 6))
        linux "INET6" "RAW" "ICMP" |> shouldEqual (Ok (10, 0x80003, 58))
        linux "INET" "RAW" "ICMP" |> shouldEqual (Ok (2, 0x80003, 1))
        linux "INET" "DGRAM" "RAW" |> shouldEqual (Ok (2, 0x80002, 255))
        linux "INET6" "SEQPACKET" "ROUTING" |> shouldEqual (Ok (10, 0x80005, 43))
        linux "INET6" "RDM" "FRAGMENT" |> shouldEqual (Ok (10, 0x80004, 44))
        linux "INET6" "DGRAM" "NONE" |> shouldEqual (Ok (10, 0x80002, 59))
        linux "INET6" "DGRAM" "DSTOPTS" |> shouldEqual (Ok (10, 0x80002, 60))
        linux "INET" "DGRAM" "IGMP" |> shouldEqual (Ok (2, 0x80002, 2))
        linux "UNIX" "STREAM" "UNSPEC" |> shouldEqual (Ok (1, 0x80001, 0))
        linux "UNSPEC" "STREAM" "UNSPEC" |> shouldEqual (Ok (0, 0x80001, 0))
        linux "PACKET" "DGRAM" "TCP" |> shouldEqual (Ok (17, 0x80002, 6))
        linux "CAN" "RAW" "RAW" |> shouldEqual (Ok (29, 0x80003, 1))
        linux "CAN" "DGRAM" "UNSPEC" |> shouldEqual (Ok (29, 0x80002, 0))

    /// The composition as it stood when the library decided no kernel answer and
    /// PawPrint listed the sockets it made: the shim's screens, then a creation
    /// for one of a fixed set of shapes, and otherwise a crash. Kept as the
    /// oracle for the property below.
    let private before (platform : SimulatedUnixPlatform) (family : int) (kind : int) (protocol : int) : string =
        let isLinux = SimulatedUnixPlatform.flavour platform = SimulatedUnixFlavour.Linux

        let familyConverts =
            match family with
            | 0
            | 1
            | 2
            | 23 -> true
            | 65536
            | 65537 -> isLinux
            | _ -> false

        let typeConverts = kind >= 1 && kind <= 5

        let protocolConverts =
            match family with
            | 65536 -> true
            | 65537 -> protocol = 0 || protocol = 255
            | 2 -> List.contains protocol [ 0 ; 1 ; 6 ; 17 ; 2 ; 255 ]
            | 23 -> List.contains protocol [ 0 ; 58 ; 1 ; 6 ; 17 ; 2 ; 255 ; 60 ; 59 ; 43 ; 44 ]
            | _ -> protocol = 0

        if not familyConverts then
            "EAFNOSUPPORT"
        elif not typeConverts then
            "EPROTOTYPE"
        elif not protocolConverts then
            "EPROTONOSUPPORT"
        else

        let created =
            [
                2, 1, 0
                2, 1, 6
                2, 2, 0
                2, 2, 17
                23, 1, 0
                23, 1, 6
                23, 2, 0
                23, 2, 17
                1, 1, 0
                1, 2, 0
            ]
            @ (if isLinux then [ 1, 3, 0 ; 1, 5, 0 ] else [])

        if List.contains (family, kind, protocol) created then
            "Ok"
        else
            "Unmodelled"

    /// Wherever the composition answered before the kernel's half moved into
    /// the library, it answers the same now; where it crashed, it now either
    /// answers or still refuses, and never creates a socket.
    [<Test>]
    let ``the composition answers what it answered before`` () : unit =
        let interesting (values : int list) : Gen<int> =
            Gen.frequency [ 3, Gen.elements values ; 1, ArbMap.defaults |> ArbMap.generate<int> ]

        let triples =
            Gen.zip3
                (interesting [ 0 ; 1 ; 2 ; 23 ; 65536 ; 65537 ; 3 ; -1 ])
                (interesting [ 0 ; 1 ; 2 ; 3 ; 4 ; 5 ; 6 ; -1 ])
                (interesting [ 0 ; 1 ; 2 ; 6 ; 17 ; 43 ; 44 ; 58 ; 59 ; 60 ; 255 ; 256 ; -1 ])

        let property (family : int, kind : int, protocol : int) : unit =
            for platform in [ SimulatedUnixPlatform.linuxX64 ; SimulatedUnixPlatform.macOsArm64 ] do
                let now = composed platform family kind protocol

                match before platform family kind protocol with
                | "Unmodelled" ->
                    if now = "Ok" then
                        failwith
                            $"%O{platform}: (%d{family}, %d{kind}, %d{protocol}) used to be refused and now creates a socket"
                | answer -> now |> shouldEqual answer

        Prop.forAll (Arb.fromGen triples) property |> Check.QuickThrowOnFailure

    /// The checked-in matrices came from a C program that *mirrors* the shim's
    /// three conversion functions rather than calling them, so a transcription
    /// slip could in principle sit in both that mirror and PawPrint and cancel
    /// out. This closes that hole by asking the host's own `libSystem.Native`.
    ///
    /// The claim checked is the one that matters: **wherever PawPrint answers
    /// with an error rather than refusing, the real entry point answers the
    /// same PAL error**, whether it was a screen's or the kernel's. Rows
    /// PawPrint refuses have no answer to compare, and rows it creates depend on
    /// the host's configuration -- whether the kernel was built with IPv6 -- so
    /// asserting those here would make the suite depend on the machine it runs
    /// on. The portable creating rows have a differential guest of their own
    /// (`sourcesPure/SocketCreateScreens.cs`).
    ///
    /// Runs against whichever matrix matches the host, so a macOS dev box pins
    /// the Darwin column and CI pins the Linux one.
    [<Test>]
    let ``the host's own libSystem.Native answers every error row as PawPrint does`` () : unit =
        let platform, flavourFile =
            if OperatingSystem.IsLinux () then
                SimulatedUnixPlatform.linuxX64, "linux.tsv"
            elif OperatingSystem.IsMacOS () then
                SimulatedUnixPlatform.macOsArm64, "darwin.tsv"
            else
                Assert.Ignore "this test needs a Unix host whose libSystem.Native PawPrint models"
                failwith "unreachable: Assert.Ignore did not throw"

        let checkedRows, disagreements =
            rows flavourFile
            |> List.fold
                (fun (count, disagreements) row ->
                    let family = palAddressFamily.[row.Family]
                    let kind = palSocketType.[row.Kind]
                    let protocol = palProtocolType.[row.Protocol]

                    let expected =
                        match SocketArgumentsPal.socketArguments platform family kind protocol with
                        | Error screen -> Some (UnixErrorPal.toPal (SocketArgumentScreen.error screen))
                        | Ok (domain, socketType, protocol) ->
                            match
                                UnixSocket.socket
                                    domain
                                    socketType
                                    protocol
                                    (UnixSystem.initial platform : UnixSystem<int, string>)
                            with
                            | Ok (Error error) -> Some (UnixErrorPal.toPal error)
                            | Ok (Ok _)
                            | Error _ -> None

                    match expected with
                    | None -> count, disagreements
                    | Some expected ->

                    let mutable created = nativeint 0
                    let actual = hostSocket (family, kind, protocol, &created)

                    // Only reachable if PawPrint reported an error for a triple
                    // the host really makes a socket for, which is the failure
                    // this test exists to catch -- but close it rather than leak
                    // the descriptor out of the assertion.
                    if actual = 0 then
                        hostClose created |> ignore<int>

                    if actual = expected then
                        count + 1, disagreements
                    else
                        count + 1,
                        $"%s{row.Family}/%s{row.Kind}/%s{row.Protocol}: PawPrint 0x%X{expected}, host 0x%X{actual}"
                        :: disagreements
                )
                (0, [])

        disagreements |> List.rev |> shouldEqual []

        // Not vacuous, and pinned against the matrix rather than a constant: the
        // rows PawPrint answers with an error are the rows the measurement
        // recorded as screens, and the kernel's errors it answers rather than
        // refuses. The flavours have different counts, so a constant would be
        // right on whichever platform it was written on and wrong in CI.
        let errorRows =
            rows flavourFile
            |> List.filter (fun row ->
                row.Outcome = "SCREEN"
                || (row.Outcome = "SYSCALL" && actual platform row <> "Refused")
            )
            |> List.length

        checkedRows |> shouldEqual errorRows
        errorRows |> shouldBeGreaterThan 100

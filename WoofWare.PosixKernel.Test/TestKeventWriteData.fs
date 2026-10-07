namespace WoofWare.PosixKernel.Test

open System
open System.IO
open System.Reflection
open System.Text.RegularExpressions
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// What an `EVFILT_WRITE` event's `data` holds for a Darwin TCP socket, the free
/// space in its send buffer (`DarwinReadiness.sendBufferSpace`), and the
/// `net.inet.tcp.sendspace` configuration it is computed from
/// (`UnixBootImage.withTcpSendSpace`).
///
/// Held to `docs/plans/2026-08-23-posix-kernel-extraction/kevent-write-data.c`'s
/// output, measured on Darwin 27.0.0 arm64 and embedded: every row of the routes
/// this kernel can connect over is replayed through `kevent`, and the IPv6 rows,
/// which no connect here can reach, through `sendBufferSpace` on a socket built by
/// hand. A property sweeps every admissible configuration against a restatement of
/// the rule.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestKeventWriteData =

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 200

    // ------------------------------------------------------------------
    // The probe's output
    // ------------------------------------------------------------------

    let private probeLines : Lazy<string list> =
        lazy
            (let assembly = Assembly.GetExecutingAssembly ()
             let resource = "WoofWare.PosixKernel.Test.keventWriteData.darwin.txt"

             use stream =
                 match assembly.GetManifestResourceStream resource with
                 | null -> failwith $"embedded resource %s{resource} not found"
                 | stream -> stream

             use reader = new StreamReader (stream)

             reader.ReadToEnd().Split ('\n', StringSplitOptions.RemoveEmptyEntries)
             |> Array.toList)

    /// Each line `pattern` matches, as its named groups.
    let private rows (pattern : string) : Map<string, string> list =
        let regex = Regex pattern

        probeLines.Force ()
        |> List.choose (fun line ->
            let m = regex.Match line

            if m.Success then
                regex.GetGroupNames ()
                |> Array.filter (fun name -> name <> "0")
                |> Array.map (fun name -> name, m.Groups.[name].Value)
                |> Map.ofArray
                |> Some
            else
                None
        )

    /// The established rows (section E) of `route`: for each end, every value the
    /// WRITE event reported (under EV_CLEAR and level registration, on first
    /// delivery and again) and SO_SNDBUF.
    let private establishedRows (route : string) : (string * int64 list) list =
        rows (
            "^E\t(?<route>[^\t]+)\t(?<end>connecting|accepted)\t.* clear=(?<clear>\\d+) clear-again=(?<clearAgain>\\d+) level=(?<level>\\d+) level-again=(?<levelAgain>\\d+) SO_SNDBUF=(?<sndbuf>\\d+) "
        )
        |> List.filter (fun row -> row.["route"] = route)
        |> List.map (fun row ->
            row.["end"],
            [ "clear" ; "clearAgain" ; "level" ; "levelAgain" ; "sndbuf" ]
            |> List.map (fun key -> Int64.Parse row.[key])
        )

    /// The rows of section B over `route` whose size `withTcpSendSpace` admits: the
    /// size set, and the WRITE data under EV_CLEAR and level registration.
    let private presizedRows (route : string) : (int * int64 list) list =
        rows (
            "^B\t(?<route>[^\t]+)\tSO_SNDBUF set (?<size>\\d+), read back \\d+\tclear=(?<clear>\\d+) level=(?<level>\\d+) "
        )
        |> List.filter (fun row -> row.["route"] = route)
        |> List.map (fun row -> Int32.Parse row.["size"], [ Int64.Parse row.["clear"] ; Int64.Parse row.["level"] ])
        |> List.filter (fun (size, _) ->
            size >= UnixMachineState.darwinLoopbackSendPipe
            && size <= UnixMachineState.darwinSocketBufferMax
        )

    /// The refused rows (section R) of `family`, the first trial of the sweep: every
    /// value the WRITE event reported.
    let private refusedRow (family : string) : int64 list =
        match
            rows (
                "^R\t(?<family>IPv4|IPv6)\tclear=(?<clear>\\d+) \\(flags 0x[0-9a-f]+\\) clear-again=(?<clearAgain>\\d+) level=(?<level>\\d+) level-again=(?<levelAgain>\\d+) "
            )
            |> List.filter (fun row -> row.["family"] = family)
        with
        | [ row ] ->
            [ "clear" ; "clearAgain" ; "level" ; "levelAgain" ]
            |> List.map (fun key -> Int64.Parse row.[key])
        | other -> failwith $"the probe printed %d{List.length other} first refusal rows for %s{family}, expected one"

    /// The rows of section R for `family` whose size `withTcpSendSpace` admits.
    let private presizedRefusedRows (family : string) : (int * int64 list) list =
        rows (
            "^R\t(?<family>IPv4|IPv6)\tSO_SNDBUF set (?<size>\\d+), read back \\d+\tclear=(?<clear>\\d+) level=(?<level>\\d+) "
        )
        |> List.filter (fun row -> row.["family"] = family)
        |> List.map (fun row -> Int32.Parse row.["size"], [ Int64.Parse row.["clear"] ; Int64.Parse row.["level"] ])
        |> List.filter (fun (size, _) ->
            size >= UnixMachineState.darwinLoopbackSendPipe
            && size <= UnixMachineState.darwinSocketBufferMax
        )

    // ------------------------------------------------------------------
    // Driving the kernel
    // ------------------------------------------------------------------

    /// `KeventWorld.darwin`, booted with the send space `sendSpace`.
    let private darwinWithSendSpace (sendSpace : int option) : UnixSystem<int, string> =
        KeventWorld.darwinWith (
            UnixBootImage.withTcpSendSpace sendSpace
            >> Configured.expectOk TcpSendSpaceRefusal.describe
        )

    /// The `data` of the WRITE event registering `fd` in a new kqueue reports, under
    /// EV_CLEAR or level registration.
    let private writeData (fd : int) (clear : bool) (system : UnixSystem<int, string>) : int64 =
        let kq, system = KeventWorld.kqueue system

        let flags =
            if clear then
                KeventFlags.Add ||| KeventFlags.Clear
            else
                KeventFlags.Add

        let system = KeventWorld.register kq fd KeventFilter.Write flags 1UL system

        match KeventWorld.apply kq [] 4 system with
        | KeventOutcome.Answered [ event ], _ when event.Filter = KeventFilter.Write -> event.Data
        | other, _ -> failwith $"expected one WRITE event of fd %d{fd}, got %A{other}"

    /// What the WRITE filter of each end of a loopback connection reports, under each
    /// registration: connecting end, then accepted end.
    let private connectedWriteData (system : UnixSystem<int, string>) : int64 list * int64 list =
        let listener, system = KeventWorld.listenerAt 5000us system
        let client, system = KeventWorld.client 5000us system
        let accepted, system = KeventWorld.accept listener system

        let both fd =
            [ writeData fd true system ; writeData fd false system ]

        both client, both accepted

    /// What the WRITE filter of a socket whose non-blocking connect was refused
    /// reports, under each registration.
    let private refusedWriteData (system : UnixSystem<int, string>) : int64 list =
        let refused, system = KeventWorld.client 5001us system

        match FileDescriptorRegistry.tryFindTarget refused system.Process.FileDescriptors with
        | Some (OpenFileTarget.Socket socketId) ->
            match (UnixMachineState.socket socketId system.Machine).Phase with
            | SocketPhase.Refused _ -> ()
            | other -> failwith $"expected the connect to port 5001 to be refused, got %A{other}"
        | other -> failwith $"fd %d{refused} names %A{other}, not a socket"

        [ writeData refused true system ; writeData refused false system ]

    /// A stream socket of `domain` in `phase`, built by hand: no connect here reaches
    /// an IPv6 one.
    let private socketIn (domain : SocketDomain) (phase : SocketPhase) : SocketDescription =
        {
            Domain = domain
            Kind = SocketKind.Stream
            Protocol = SocketProtocol.Tcp
            Binding = None
            ReuseAddress = false
            Phase = phase
        }

    let private darwinMachine (sendSpace : int option) : UnixMachineState = (darwinWithSendSpace sendSpace).Machine

    // ------------------------------------------------------------------
    // The measured rows
    // ------------------------------------------------------------------

    [<Test>]
    let ``an established IPv4 socket's WRITE data is what Darwin reported, at either end`` () : unit =
        let measured = establishedRows "IPv4 127.0.0.1"
        measured |> List.map fst |> shouldEqual [ "connecting" ; "accepted" ]

        let connecting, accepted = connectedWriteData KeventWorld.darwin

        for (endName, values) in measured do
            let modelled =
                match endName with
                | "connecting" -> connecting
                | _ -> accepted

            // Every value the probe printed for the end is one number: the
            // buffer's size, whichever registration and however often asked.
            List.distinct values |> shouldHaveLength 1
            List.distinct modelled |> shouldEqual [ List.head values ]

    [<Test>]
    let ``an established IPv6 socket's send buffer is what Darwin reported`` () : unit =
        let measured = establishedRows "IPv6 ::1"
        measured |> shouldHaveLength 2

        let modelled =
            DarwinReadiness.sendBufferSpace
                (socketIn SocketDomain.Inet6 (SocketPhase.Established (ConnectionId 0L, ConnectionEnd.Client)))
                (darwinMachine None)

        for (_, values) in measured do
            List.distinct values |> shouldEqual [ modelled ]

    [<Test>]
    let ``a configured send space gives the IPv4 buffer Darwin grew from it`` () : unit =
        let measured = presizedRows "IPv4 127.0.0.1"
        // Every swept size from 49152 to the maximum.
        measured |> shouldHaveLength 13

        for (size, values) in measured do
            let connecting, accepted = connectedWriteData (darwinWithSendSpace (Some size))

            List.distinct values |> shouldHaveLength 1

            (size, connecting @ accepted |> List.distinct)
            |> shouldEqual (size, [ List.head values ])

    [<Test>]
    let ``a configured send space gives the IPv6 buffer Darwin grew from it`` () : unit =
        let measured = presizedRows "IPv6 ::1"
        measured |> shouldHaveLength 13

        for (size, values) in measured do
            let modelled =
                DarwinReadiness.sendBufferSpace
                    (socketIn SocketDomain.Inet6 (SocketPhase.Established (ConnectionId 0L, ConnectionEnd.Client)))
                    (darwinMachine (Some size))

            (size, List.distinct values) |> shouldEqual (size, [ modelled ])

    [<Test>]
    let ``a refused socket's WRITE data is what Darwin reported`` () : unit =
        let measured = refusedRow "IPv4"
        List.distinct measured |> shouldHaveLength 1

        refusedWriteData KeventWorld.darwin
        |> List.distinct
        |> shouldEqual [ List.head measured ]

        for error in [ RefusalError.Pending ; RefusalError.Reported ] do
            DarwinReadiness.sendBufferSpace
                (socketIn SocketDomain.Inet6 (SocketPhase.Refused error))
                (darwinMachine None)
            |> List.singleton
            |> shouldEqual (List.distinct (refusedRow "IPv6"))

    [<Test>]
    let ``a refused socket's WRITE data does not follow the configured send space`` () : unit =
        for family in [ "IPv4" ; "IPv6" ] do
            let measured = presizedRefusedRows family
            // 100000, 131072, 262144 and 1000000.
            measured |> shouldHaveLength 4

            for (size, values) in measured do
                let modelled =
                    match family with
                    | "IPv4" -> refusedWriteData (darwinWithSendSpace (Some size))
                    | _ ->
                        [
                            DarwinReadiness.sendBufferSpace
                                (socketIn SocketDomain.Inet6 (SocketPhase.Refused RefusalError.Pending))
                                (darwinMachine (Some size))
                        ]

                (family, size, List.distinct modelled)
                |> shouldEqual (family, size, List.distinct values)

    // ------------------------------------------------------------------
    // Every admissible configuration
    // ------------------------------------------------------------------

    /// The rule, restated: the least whole number of segments that holds the send
    /// space, but no more than the socket buffer maximum.
    let private grown (segment : int64) (sendSpace : int) : int64 =
        let sendSpace = int64 sendSpace

        let segments =
            if sendSpace % segment = 0L then
                sendSpace / segment
            else
                sendSpace / segment + 1L

        Math.Min (segments * segment, int64 UnixMachineState.darwinSocketBufferMax)

    let private admissible : Gen<int> =
        Gen.oneof
            [
                Gen.choose (UnixMachineState.darwinLoopbackSendPipe, UnixMachineState.darwinSocketBufferMax)
                // Around the edges, and around whole segments.
                Gen.elements
                    [
                        UnixMachineState.darwinLoopbackSendPipe
                        UnixMachineState.darwinLoopbackSendPipe + 1
                        UnixMachineState.darwinSocketBufferMax
                        UnixMachineState.darwinSocketBufferMax - 1
                    ]
                Gen.choose (4, 513)
                |> Gen.bind (fun segments ->
                    Gen.elements [ -1 ; 0 ; 1 ] |> Gen.map (fun offset -> segments * 16332 + offset)
                )
                |> Gen.filter (fun size ->
                    size >= UnixMachineState.darwinLoopbackSendPipe
                    && size <= UnixMachineState.darwinSocketBufferMax
                )
            ]

    [<Test>]
    let ``every admissible send space gives both ends of a connection the grown buffer`` () : unit =
        let property (sendSpace : int) : unit =
            let connecting, accepted = connectedWriteData (darwinWithSendSpace (Some sendSpace))

            connecting @ accepted |> List.distinct |> shouldEqual [ grown 16332L sendSpace ]

            DarwinReadiness.sendBufferSpace
                (socketIn SocketDomain.Inet6 (SocketPhase.Established (ConnectionId 0L, ConnectionEnd.Client)))
                (darwinMachine (Some sendSpace))
            |> shouldEqual (grown 16312L sendSpace)

            refusedWriteData (darwinWithSendSpace (Some sendSpace))
            |> List.distinct
            |> shouldEqual [ 2048L ]

        Check.One (config, Prop.forAll (Arb.fromGen admissible) property)

    // ------------------------------------------------------------------
    // The configuration and its refusals
    // ------------------------------------------------------------------

    [<Test>]
    let ``each flavour starts with its measured send space`` () : unit =
        KeventWorld.darwin.Machine.TcpSendSpace |> shouldEqual 131072

        let linux =
            UnixSystem.initial<int, string> SimulatedUnixPlatform.linuxX64 Map.empty 0 (CpuId 0)

        (UnixBootImage.boot linux).Machine.TcpSendSpace |> shouldEqual 16384

        (UnixBootImage.withTcpSendSpace None linux
         |> Configured.expectOk TcpSendSpaceRefusal.describe
         |> UnixBootImage.boot)
            .Machine.TcpSendSpace
        |> shouldEqual 16384

        (darwinMachine None).TcpSendSpace |> shouldEqual 131072

    [<Test>]
    let ``a Darwin send space outside the admissible range is refused, naming the bound it is past`` () : unit =
        let darwin =
            UnixSystem.initial<int, string> SimulatedUnixPlatform.macOsArm64 Map.empty 0 (CpuId 0)

        let anywhere =
            Gen.oneof
                [
                    Gen.choose (Int32.MinValue, UnixMachineState.darwinLoopbackSendPipe - 1)
                    Gen.choose (UnixMachineState.darwinSocketBufferMax + 1, Int32.MaxValue)
                    Gen.choose (UnixMachineState.darwinLoopbackSendPipe, UnixMachineState.darwinSocketBufferMax)
                    Gen.elements
                        [
                            UnixMachineState.darwinLoopbackSendPipe - 1
                            UnixMachineState.darwinLoopbackSendPipe
                            UnixMachineState.darwinSocketBufferMax
                            UnixMachineState.darwinSocketBufferMax + 1
                            0
                        ]
                ]

        let property (size : int) : unit =
            // The bounds as numbers, so that the oracle shares no constant with
            // the setter: 3 x the loopback MTU of 16384, and kern.ipc.maxsockbuf.
            let expected : Result<unit, TcpSendSpaceRefusal> =
                if size > 8388608 then
                    Error (TcpSendSpaceRefusal.AboveSocketBufferMax (size, 8388608))
                elif size < 49152 then
                    Error (TcpSendSpaceRefusal.BelowLoopbackSendPipe (size, 49152))
                else
                    Ok ()

            match UnixBootImage.withTcpSendSpace (Some size) darwin with
            | Ok image ->
                expected |> shouldEqual (Ok ())
                (UnixBootImage.boot image).Machine.TcpSendSpace |> shouldEqual size
            | Error refusal -> Error refusal |> shouldEqual expected

        Check.One (config, Prop.forAll (Arb.fromGen anywhere) property)

        (darwinMachine (Some UnixMachineState.darwinLoopbackSendPipe)).TcpSendSpace
        |> shouldEqual UnixMachineState.darwinLoopbackSendPipe

        (darwinMachine (Some UnixMachineState.darwinSocketBufferMax)).TcpSendSpace
        |> shouldEqual UnixMachineState.darwinSocketBufferMax

    [<Test>]
    let ``a Linux send space is refused, since nothing reads it`` () : unit =
        let linux =
            UnixSystem.initial<int, string> SimulatedUnixPlatform.linuxX64 Map.empty 0 (CpuId 0)

        for size in [ Int32.MinValue ; 0 ; 16384 ; 131072 ; Int32.MaxValue ] do
            UnixBootImage.withTcpSendSpace (Some size) linux
            |> Result.map ignore<UnixBootImage<int, string>>
            |> shouldEqual (Error (TcpSendSpaceRefusal.NotReadOn (SimulatedUnixFlavour.Linux, size)))

    [<Test>]
    let ``the send buffer is refused where Darwin's rule does not reach`` () : unit =
        let established =
            socketIn SocketDomain.Inet (SocketPhase.Established (ConnectionId 0L, ConnectionEnd.Client))

        let linux =
            (UnixSystem.initial<int, string> SimulatedUnixPlatform.linuxX64 Map.empty 0 (CpuId 0)
             |> UnixBootImage.boot)
                .Machine

        let refusals =
            [
                "Linux-flavoured", established, linux
                "never ready", socketIn SocketDomain.Inet SocketPhase.Idle, darwinMachine None
                "never ready",
                socketIn
                    SocketDomain.Inet
                    (SocketPhase.Listening
                        {
                            Backlog = 1
                            Queue = []
                            Drained = false
                        }),
                darwinMachine None
                "does not model",
                { established with
                    Kind = SocketKind.Datagram
                },
                darwinMachine None
                "assembled the machine by hand",
                established,
                { darwinMachine None with
                    TcpSendSpace = UnixMachineState.darwinLoopbackSendPipe - 1
                }
                "assembled the machine by hand",
                established,
                { darwinMachine None with
                    TcpSendSpace = UnixMachineState.darwinSocketBufferMax + 1
                }
            ]

        for (expected, socket, machine) in refusals do
            let e =
                Assert.Throws<Exception> (fun () -> DarwinReadiness.sendBufferSpace socket machine |> ignore)

            e.Message |> shouldContainText expected

namespace WoofWare.PosixKernel.Test

open System
open System.Collections.Immutable
open System.IO
open System.Reflection
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `read(2)` and `write(2)` on a connected TCP socket, through the syscalls: the
/// wakes a transfer raises for an edge-triggered waiter, replayed from the E
/// section of `tcp-transfer.c` (docs/plans/2026-10-07-tcp-byte-transfer), and
/// what the S section cannot show (`TestConnectedTransferAgainstHost` replays
/// that): a sleep that is refused, a buffer that faults, a connect whose
/// completion is unreported, Linux's send-space mark, a reset's wakes, a close
/// a dropped `accept` makes, the invariants on a connection's bytes, and one
/// process's transfer waking another's waiter.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestConnectedTransfer =

    let private payload : byte[] = Array.init 400000 (fun i -> byte (i * 13 + 5))

    let private port : uint16 = 6000us

    /// A system of `platform`, booted from an image `configure` configured,
    /// with tasks 1 to 4 and `SIGPIPE` ignored.
    let private systemOn
        (configure : UnixBootImage<int, string> -> UnixBootImage<int, string>)
        (platform : SimulatedUnixPlatform)
        : UnixSystem<int, string>
        =
        let system =
            UnixSystem.initial<int, string> platform
            |> configure
            |> Launched.boot UnixSystem.pipedStandardStreams 0 (CpuId 0)
            |> fun system -> ([ 1..4 ], system) ||> List.foldBack Tasks.ensure

        { system with
            Process =
                { system.Process with
                    Signals = SignalState.setDisposition Signal.SIGPIPE SignalDisposition.Ignore system.Process.Signals
                }
        }

    /// TCP buffers as small as each flavour admits.
    let private small (image : UnixBootImage<int, string>) : UnixBootImage<int, string> =
        match SimulatedUnixPlatform.flavour (UnixBootImage.platform image) with
        | SimulatedUnixFlavour.Linux ->
            image
            |> UnixBootImage.withTcpSendSpaceMax (Some 30000)
            |> UnixBootImage.withTcpReceiveSpace (Some 10000)
        | SimulatedUnixFlavour.Darwin ->
            image
            |> UnixBootImage.withTcpSendSpace (Some UnixMachineState.darwinLoopbackSendPipe)
            |> UnixBootImage.withTcpReceiveSpace (Some UnixMachineState.darwinLoopbackReceivePipe)

    /// A connected pair on `system`, made by a blocking connect and accept:
    /// the connecting socket, then the accepted one, each non-blocking if
    /// `nonBlocking`.
    let private pair (nonBlocking : bool) (system : UnixSystem<int, string>) : int * int * UnixSystem<int, string> =
        let listener, system = KeventWorld.listenerAt port system
        let client, system = KeventWorld.stream false system

        let system =
            match KeventWorld.connect client port system with
            | ConnectOutcome.Completed, system -> system
            | other, _ -> failwith $"connect: %A{other}"

        let server, system = KeventWorld.accept listener system
        let system = KeventWorld.close listener system
        let _, system = UnixDescriptor.setNonBlocking client nonBlocking system
        let _, system = UnixDescriptor.setNonBlocking server nonBlocking system
        client, server, system

    /// A write of the first `count` bytes of the payload through `fd`.
    let private send
        (fd : int)
        (count : int)
        (system : UnixSystem<int, string>)
        : Result<WriteOutcome<WriteAnswer, int, string>, WriteRefusal>
        =
        WriteOutcomes.admitThenWrite
            system.Leader
            fd
            UserBuffer.Mapped
            (ImmutableArray.Create (payload, 0, count))
            system

    let private sent (fd : int) (count : int) (system : UnixSystem<int, string>) : int64 * UnixSystem<int, string> =
        match send fd count system with
        | Ok (WriteOutcome.Returns (WriteAnswer.Completed written, system)) -> written, system
        | other -> failwith $"a write of %d{count} through fd %d{fd}: %A{other}"

    let private sentAll (fd : int) (count : int) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        match sent fd count system with
        | written, system when written = int64 count -> system
        | written, _ -> failwith $"a write of %d{count} through fd %d{fd} took %d{written}"

    let private received
        (fd : int)
        (count : int)
        (system : UnixSystem<int, string>)
        : ReadAnswer * UnixSystem<int, string>
        =
        match ReadOutcomes.read fd UserBuffer.Mapped (uint64 count) system with
        | Ok answered -> answered
        | Error refusal -> failwith $"a read of %d{count} through fd %d{fd}: %s{ReadRefusal.describe refusal}"

    let private receivedCount
        (fd : int)
        (count : int)
        (system : UnixSystem<int, string>)
        : int * UnixSystem<int, string>
        =
        match received fd count system with
        | ReadAnswer.Completed bytes, system -> bytes.Length, system
        | other, _ -> failwith $"a read of %d{count} through fd %d{fd}: %A{other}"

    /// Write through `fd` until a write answers `EAGAIN`: how much was taken.
    let private fill (fd : int) (system : UnixSystem<int, string>) : int64 * UnixSystem<int, string> =
        let rec go (total : int64) (system : UnixSystem<int, string>) =
            match send fd 65536 system with
            | Ok (WriteOutcome.Returns (WriteAnswer.Completed written, system)) -> go (total + written) system
            | Ok (WriteOutcome.Returns (WriteAnswer.Failed UnixError.EAGAIN, system)) -> total, system
            | other -> failwith $"filling fd %d{fd}: %A{other}"

        go 0L system

    let private socketOf (fd : int) (system : UnixSystem<int, string>) : SocketId =
        match FileDescriptorRegistry.tryFindTarget fd (UnixSystemState.fileDescriptors system) with
        | Some (OpenFileTarget.Socket socketId) -> socketId
        | other -> failwith $"fd %d{fd} names %A{other}"

    let private assertClean (system : UnixSystem<int, string>) : unit =
        UnixSystem.checkInvariants system |> shouldEqual []

    // ------------------------------------------------------------------
    // A connect whose completion is unreported
    // ------------------------------------------------------------------

    /// Linux's non-blocking connect leaves its socket in
    /// `EstablishedPendingReport` until a later connect reports the
    /// completion, which the .NET runtime never makes: it asks `SO_ERROR`
    /// instead. The phase records only what that connect would answer; the
    /// socket is the connection's client end, and transfers as one.
    [<Test>]
    let ``a Linux client whose connect is unreported transfers as the client end`` () : unit =
        let system = systemOn id SimulatedUnixPlatform.linuxX64
        let listener, system = KeventWorld.listenerAt port system
        let client, system = KeventWorld.client port system
        let server, system = KeventWorld.accept listener system

        (UnixMachineState.socket (socketOf client system) system.Machine).Phase
        |> SocketPhase.connectionEnd
        |> Option.map snd
        |> shouldEqual (Some ConnectionEnd.Client)

        match (UnixMachineState.socket (socketOf client system) system.Machine).Phase with
        | SocketPhase.EstablishedPendingReport _ -> ()
        | other -> failwith $"expected the report pending, got %A{other}"

        let system = sentAll server 10 system
        let read, system = receivedCount client 4096 system
        read |> shouldEqual 10

        let system = sentAll client 7 system
        let read, system = receivedCount server 4096 system
        read |> shouldEqual 7

        // The transfers leave the report pending; the next connect makes it.
        match (UnixMachineState.socket (socketOf client system) system.Machine).Phase with
        | SocketPhase.EstablishedPendingReport _ -> ()
        | other -> failwith $"expected the report pending, got %A{other}"

        KeventWorld.connect client port system
        |> fst
        |> shouldEqual ConnectOutcome.Completed

        assertClean system

    // ------------------------------------------------------------------
    // A sleep, and a fault, refused
    // ------------------------------------------------------------------

    [<Test>]
    let ``a blocking transfer that would sleep is refused, and one that need not is answered`` () : unit =
        for platform in Machines.platforms do
            let client, server, system = pair false (systemOn small platform)

            ReadOutcomes.read client UserBuffer.Mapped 10UL system
            |> shouldEqual (Error (ReadRefusal.ConnectionSleep (socketOf client system)))

            // Bytes waiting answer a blocking read at once.
            let system = sentAll server 5 system
            let read, system = receivedCount client 10 system
            read |> shouldEqual 5

            // A write the buffers have room for is taken whole; one they do
            // not, or that finds no room at all, would sleep.
            let system = sentAll client 100 system

            send client payload.Length system
            |> shouldEqual (Error (WriteRefusal.ConnectionSleep (socketOf client system)))

            let _, system = UnixDescriptor.setNonBlocking client true system
            let _, system = fill client system
            let _, system = UnixDescriptor.setNonBlocking client false system

            send client 1 system
            |> shouldEqual (Error (WriteRefusal.ConnectionSleep (socketOf client system)))

            // A FIN answers a blocking read at once, with end of file.
            let _, system = UnixDescriptor.setNonBlocking server true system

            let rec drain (system : UnixSystem<int, string>) =
                match received server 65536 system with
                | ReadAnswer.Completed bytes, system when not bytes.IsEmpty -> drain system
                | _, system -> system

            let system = drain system |> KeventWorld.close server

            let rec readToEnd (system : UnixSystem<int, string>) =
                match received client 65536 system with
                | ReadAnswer.Completed bytes, system when not bytes.IsEmpty -> readToEnd system
                | answer, system -> answer, system

            let answer, system = readToEnd system
            answer |> shouldEqual (ReadAnswer.Completed ImmutableArray.Empty)
            assertClean system

    [<Test>]
    let ``a transfer through an unmapped buffer is refused only where it would copy`` () : unit =
        // Low enough that no flavour's screen faults it before the socket.
        let unmapped = UserBuffer.Unmapped 0x1000UL

        for platform in Machines.platforms do
            let client, server, system = pair true (systemOn id platform)

            // Nothing to copy: the socket's own answer.
            ReadOutcomes.read client unmapped 10UL system
            |> Result.map fst
            |> shouldEqual (Ok (ReadAnswer.Failed UnixError.EAGAIN))

            WriteOutcomes.write client ImmutableArray.Empty system
            |> Result.map fst
            |> shouldEqual (Ok (WriteAnswer.Completed 0L))

            UnixReadWrite.admitWrite system.Leader client unmapped 0UL system
            |> Result.map (fun outcome ->
                match outcome with
                | WriteOutcome.Returns (admission, _) -> admission
                | other -> failwith $"%A{other}"
            )
            |> shouldEqual (Ok (WriteAdmission.Answered (WriteAnswer.Completed 0L)))

            // Something to copy.
            UnixReadWrite.admitWrite system.Leader client unmapped 10UL system
            |> shouldEqual (Error (WriteRefusal.ConnectionFault (socketOf client system)))

            let system = sentAll server 10 system

            ReadOutcomes.read client unmapped 10UL system
            |> shouldEqual (Error (ReadRefusal.ConnectionFault (socketOf client system)))

    // ------------------------------------------------------------------
    // Edges: the E section
    // ------------------------------------------------------------------

    /// The E section's rows of one flavour's run: scenario, step, and what a
    /// zero-timeout wait reported.
    let private edgeRows (flavour : SimulatedUnixFlavour) : (string * string * string) list =
        let resource =
            match flavour with
            | SimulatedUnixFlavour.Linux -> "WoofWare.PosixKernel.Test.tcpTransfer.linux.txt"
            | SimulatedUnixFlavour.Darwin -> "WoofWare.PosixKernel.Test.tcpTransfer.darwin.txt"

        use stream =
            match Assembly.GetExecutingAssembly().GetManifestResourceStream resource with
            | null -> failwith $"embedded resource %s{resource} not found"
            | stream -> stream

        use reader = new StreamReader (stream)

        reader.ReadToEnd().Split '\n'
        |> Array.toList
        |> List.choose (fun line ->
            match line.TrimEnd('\r').Split '\t' |> Array.toList with
            | [ scenario ; step ; seen ] when scenario.StartsWith ("E", StringComparison.Ordinal) ->
                Some (scenario, step, seen)
            | _ -> None
        )

    /// An edge-triggered registration of `fd` for `what` ('r', 'w' or 'b'), on
    /// a fresh epoll instance or kqueue, as the probe's `edge_register` makes
    /// one; and the wait the probe's `edge_wait` makes on it, rendered as it
    /// prints.
    let private edgeRegister
        (fd : int)
        (what : char)
        (system : UnixSystem<int, string>)
        : (UnixSystem<int, string> -> string * UnixSystem<int, string>) * UnixSystem<int, string>
        =
        match SimulatedUnixPlatform.flavour system.Machine.UnixPlatform with
        | SimulatedUnixFlavour.Linux ->
            let epoll, system =
                match UnixPoll.epollCreate1 0 system with
                | Ok (Ok created) -> created
                | other -> failwith $"epoll_create1: %A{other}"

            let events =
                match what with
                | 'r' -> EpollEvents.In
                | 'w' -> EpollEvents.Out
                | _ -> EpollEvents.In ||| EpollEvents.Out

            let system =
                match
                    UnixPoll.epollCtl
                        epoll
                        1
                        fd
                        (EpollEventArgument.Readable (events ||| EpollEvents.EdgeTriggered, 0UL))
                        system
                with
                | Ok (EpollCtlAnswer.Changed, system) -> system
                | other -> failwith $"epoll_ctl: %A{other}"

            let wait (system : UnixSystem<int, string>) =
                match UnixPoll.epollWait 4 epoll 2 UserBuffer.Mapped 0 system with
                | Ok (EpollWaitOutcome.Answered [], system) -> "none", system
                | Ok (EpollWaitOutcome.Answered ((_, events) :: _), system) -> $"0x%x{events}", system
                | other -> failwith $"epoll_wait: %A{other}"

            wait, system
        | SimulatedUnixFlavour.Darwin ->
            let kq, system = KeventWorld.kqueue system
            let clear = KeventFlags.Add ||| KeventFlags.Clear

            let system =
                if what <> 'w' then
                    KeventWorld.register kq fd KeventFilter.Read clear 0UL system
                else
                    system

            let system =
                if what <> 'r' then
                    KeventWorld.register kq fd KeventFilter.Write clear 0UL system
                else
                    system

            let wait (system : UnixSystem<int, string>) =
                match KeventWorld.apply kq [] 2 system with
                | KeventOutcome.Answered [], system -> "none", system
                | KeventOutcome.Answered events, system ->
                    events
                    |> List.map (fun event ->
                        let name = if event.Filter = KeventFilter.Read then "READ" else "WRITE"

                        let eof =
                            if event.Flags &&& KeventFlags.Eof <> 0us then
                                " EOF"
                            else
                                ""

                        $"%s{name}(data=%d{event.Data}%s{eof})"
                    )
                    |> String.concat " ",
                    system
                | other, _ -> failwith $"kevent: %A{other}"

            wait, system

    /// E1, E2 up to the fill, and E3, replayed: the steps the model holds to
    /// the byte. `c` is the observed socket and `s` its peer, as in the probe.
    [<Test>]
    let ``each transfer queues an edge-triggered waiter as each flavour was measured to`` () : unit =
        for platform in Machines.platforms do
            let flavour = SimulatedUnixPlatform.flavour platform
            let rows = edgeRows flavour

            // Each step, the probe's name for it, and what it does to `c` and
            // `s`, against the scenario's first rows in order.
            let replay
                (scenario : string)
                (what : char)
                (steps : (string * (int -> int -> UnixSystem<int, string> -> UnixSystem<int, string>)) list)
                : unit
                =
                let measured =
                    rows
                    |> List.filter (fun (name, _, _) -> name = scenario)
                    |> List.truncate steps.Length

                measured.Length |> shouldEqual steps.Length

                let c, s, system = pair true (systemOn id platform)
                let wait, system = edgeRegister c what system

                (system, List.zip steps measured)
                ||> List.fold (fun system ((label, act), (_, step, expected)) ->
                    step.StartsWith (label, StringComparison.Ordinal) |> shouldEqual true
                    let system = act c s system
                    let seen, system = wait system
                    (scenario, step, seen) |> shouldEqual (scenario, step, expected)
                    assertClean system
                    system
                )
                |> ignore

            let nothing (_ : int) (_ : int) (system : UnixSystem<int, string>) = system

            replay
                "E1"
                'r'
                [
                    "after ADD", nothing
                    "peer wrote 100", (fun _ s system -> sentAll s 100 system)
                    "peer wrote 100 more", (fun _ s system -> sentAll s 100 system)
                    "read 50 of 200", (fun c _ system -> receivedCount c 50 system |> snd)
                    "nothing", nothing
                ]

            replay
                "E2"
                'w'
                [
                    "after ADD", nothing
                    "wrote 1000", (fun c _ system -> sentAll c 1000 system)
                    "peer read 1000", (fun _ s system -> receivedCount s 1000 system |> snd)
                    "filled", (fun c _ system -> fill c system |> snd)
                ]

            if flavour = SimulatedUnixFlavour.Linux then
                replay
                    "E3"
                    'b'
                    [
                        "after ADD", nothing
                        "peer wrote 100", (fun _ s system -> sentAll s 100 system)
                    ]

    /// E2 after the fill: the peer reads 4096 bytes at a time. On Linux exactly
    /// one OUT edge comes, at the first read that leaves the writer's send
    /// buffer at most two thirds full, and none after (measured: one edge,
    /// then none). Where it falls is the model's: it counts bytes, where Linux
    /// counts the segments' memory. Darwin raises WRITE on each read that moves
    /// bytes out of the writer's send buffer and leaves the low-water mark
    /// free; its measured first edge came later, once loopback acknowledged,
    /// which the model does at once.
    [<Test>]
    let ``after a fill, the drain raises the send-space edge by each flavour's rule`` () : unit =
        for platform in Machines.platforms do
            let flavour = SimulatedUnixPlatform.flavour platform
            let c, s, system = pair true (systemOn id platform)
            let wait, system = edgeRegister c 'w' system
            let _, system = wait system
            let filled, system = fill c system
            let none, system = wait system
            none |> shouldEqual "none"

            let writable (system : UnixSystem<int, string>) : bool =
                match
                    UnixPoll.poll
                        0
                        [
                            {
                                Fd = c
                                Events = 0x4s
                            }
                        ]
                        0
                        system
                with
                | Ok (PollOutcome.Answered ([ revents ], _), _) -> revents &&& 0x4s <> 0s
                | other -> failwith $"poll: %A{other}"

            // Each read, what the wait then reported and whether `c` was then
            // writable, until the fill has all but drained.
            let rec drain (reads : int) (seen : (string * bool) list) (system : UnixSystem<int, string>) =
                if reads = 0 then
                    List.rev seen
                else
                    let _, system = receivedCount s 4096 system
                    let edge, system = wait system
                    drain (reads - 1) ((edge, writable system) :: seen) system

            let seen = drain (int (filled / 4096L)) [] system

            match flavour with
            | SimulatedUnixFlavour.Linux ->
                let edges =
                    seen |> List.indexed |> List.filter (fun (_, (edge, _)) -> edge <> "none")

                edges |> List.map (snd >> fst) |> shouldEqual [ "0x4" ]
                let first = edges |> List.head |> fst
                // At the first read that left `c` writable, and not before.
                seen.[first] |> snd |> shouldEqual true
                seen |> List.take first |> List.forall (snd >> not) |> shouldEqual true
            | SimulatedUnixFlavour.Darwin ->
                // The first read moved 4096 bytes out of a full send buffer.
                fst seen.[0] |> shouldEqual "WRITE(data=4096)"
                fst seen.[1] |> shouldEqual "WRITE(data=8192)"

    /// Linux marks a send buffer out of space (`SOCK_NOSPACE`) when a write
    /// meets `EAGAIN` or is short, and when `tcp_poll` finds it unwritable:
    /// a `poll`, or an epoll `ADD`. A whole write that leaves it unwritable
    /// marks nothing, so its drain raises no OUT edge unless something polled
    /// it first.
    [<Test>]
    let ``Linux's send-space edge needs the buffer marked out of space, which a poll marks too`` () : unit =
        let platform = SimulatedUnixPlatform.linuxX64

        // 10000 of a write go to the receive buffer and the rest wait in a
        // send buffer of 30000, unwritable above 20000 queued.
        let unwritable (registerFirst : bool) (pollBetween : bool) : string list =
            let c, s, system = pair true (systemOn small platform)

            let wait, system =
                if registerFirst then
                    let wait, system = edgeRegister c 'w' system
                    let _, system = wait system
                    Some wait, system
                else
                    None, system

            let system = sentAll c 35000 system

            let system =
                if pollBetween then
                    match
                        UnixPoll.poll
                            0
                            [
                                {
                                    Fd = c
                                    Events = 0x1s
                                }
                            ]
                            0
                            system
                    with
                    | Ok (PollOutcome.Answered _, system) -> system
                    | other -> failwith $"poll: %A{other}"
                else
                    system

            let wait, system =
                match wait with
                | Some wait -> wait, system
                | None -> edgeRegister c 'w' system

            ((system, []), [ 1..6 ])
            ||> List.fold (fun (system, seen) _ ->
                let _, system = receivedCount s 4096 system
                let edge, system = wait system
                system, seen @ [ edge ]
            )
            |> snd

        let edgesIn (seen : string list) = seen |> List.filter ((<>) "none")

        // Registered before, nothing polled since: no edge.
        unwritable true false |> edgesIn |> shouldEqual []

        // epoll_wait's re-poll of a pending entry marks it too: here the
        // entry an arrival of bytes queued.
        let c, s, system = pair true (systemOn small platform)
        let wait, system = edgeRegister c 'b' system
        let _, system = wait system
        let system = sentAll c 35000 system
        let system = sentAll s 100 system
        let arrived, system = wait system
        arrived |> shouldEqual "0x1"

        let _, afterDrain =
            ((system, []), [ 1..6 ])
            ||> List.fold (fun (system, seen) _ ->
                let _, system = receivedCount s 4096 system
                let edge, system = wait system
                system, seen @ [ edge ]
            )

        // OUT's edge, reporting IN too: the 100 bytes are still unread.
        edgesIn afterDrain |> shouldEqual [ "0x5" ]
        // A poll in between marked it.
        unwritable true true |> edgesIn |> shouldEqual [ "0x4" ]
        // The ADD itself polled it.
        unwritable false false |> edgesIn |> shouldEqual [ "0x4" ]

    /// A FIN sent behind bytes the peer's receive buffer has no room for yet
    /// arrives with the last of them: until then the peer reads like an open
    /// one, with no RDHUP, and once it has read them all it sees end of file.
    [<Test>]
    let ``a FIN queued behind bytes arrives with the last of them`` () : unit =
        let c, s, system = pair true (systemOn small SimulatedUnixPlatform.linuxX64)
        // 10000 reach `c`'s receive buffer; the rest wait in `s`'s send buffer.
        let system = sentAll s 25000 system |> KeventWorld.close s

        let level (system : UnixSystem<int, string>) : int16 =
            match
                UnixPoll.poll
                    0
                    [
                        {
                            Fd = c
                            Events = 0x2001s
                        }
                    ]
                    0
                    system
            with
            | Ok (PollOutcome.Answered ([ revents ], _), _) -> revents
            | other -> failwith $"poll: %A{other}"

        // IN, and no RDHUP yet.
        level system |> shouldEqual 0x1s

        let rec readAll (total : int) (system : UnixSystem<int, string>) =
            match received c 4096 system with
            | ReadAnswer.Completed bytes, system when not bytes.IsEmpty -> readAll (total + bytes.Length) system
            | answer, system -> total, answer, system

        let total, last, system = readAll 0 system
        total |> shouldEqual 25000
        last |> shouldEqual (ReadAnswer.Completed ImmutableArray.Empty)
        level system |> shouldEqual 0x2001s
        assertClean system

    // ------------------------------------------------------------------
    // Resets
    // ------------------------------------------------------------------

    /// A close over unread bytes resets the peer. Linux queues every epoll
    /// registration of it, whatever its interest, and Darwin activates WRITE
    /// then READ, as for a refusal, which goes through the same
    /// `soisdisconnected`.
    [<Test>]
    let ``a reset wakes every epoll registration, and activates kqueue WRITE then READ`` () : unit =
        for platform in Machines.platforms do
            let c, s, system = pair true (systemOn id platform)
            // Unread by `s`.
            let system = sentAll c 10 system

            let wait, system =
                match SimulatedUnixPlatform.flavour platform with
                | SimulatedUnixFlavour.Linux -> edgeRegister c 'w' system
                | SimulatedUnixFlavour.Darwin -> edgeRegister c 'b' system

            let _, system = wait system
            let system = KeventWorld.close s system
            let seen, system = wait system

            match SimulatedUnixPlatform.flavour platform with
            | SimulatedUnixFlavour.Linux ->
                // OUT|ERR|HUP: the level, masked by OUT and the two every
                // registration stores.
                seen |> shouldEqual "0x1c"
            | SimulatedUnixFlavour.Darwin -> seen |> shouldEqual "WRITE(data=146988 EOF) READ(data=0 EOF)"

            ReadOutcomes.read c UserBuffer.Mapped 10UL system
            |> Result.map fst
            |> shouldEqual (Ok (ReadAnswer.Failed UnixError.ECONNRESET))

            assertClean system

    /// `write` given the bytes without an admission first answers as the
    /// admission and its write do, `SIGPIPE` included: three writes of 100
    /// bytes each way, from each state a close can leave the writer in.
    [<Test>]
    let ``a write that skips its admission answers a connection as one that makes it`` () : unit =
        let summary (outcome : Result<WriteOutcome<WriteAnswer, int, string>, WriteRefusal>) =
            match outcome with
            | Ok (WriteOutcome.Returns (answer, system)) -> (answer, None), system
            | Ok (WriteOutcome.ReturnsRaising (answer, entry, system)) -> (answer, Some entry.Signal), system
            | other -> failwith $"%A{other}"

        for platform in Machines.platforms do
            // The peer closed over unread bytes; closed cleanly; or neither.
            for build in [ "reset" ; "fin" ; "open" ] do
                let c, s, system = pair true (systemOn id platform)

                let system =
                    match build with
                    | "reset" -> sentAll c 10 system |> KeventWorld.close s
                    | "fin" -> KeventWorld.close s system
                    | _ -> system

                let thrice
                    (write : UnixSystem<int, string> -> Result<WriteOutcome<WriteAnswer, int, string>, WriteRefusal>)
                    =
                    ((system, []), [ 1..3 ])
                    ||> List.fold (fun (system, seen) _ ->
                        let answer, system = summary (write system)
                        system, seen @ [ answer ]
                    )
                    |> snd

                let admitted = thrice (send c 100)

                let direct =
                    thrice (fun system ->
                        UnixReadWrite.write system.Leader c (ImmutableArray.Create (payload, 0, 100)) system
                    )

                (build, direct) |> shouldEqual (build, admitted)

    /// What a reset leaves of the survivor's binding and options, against a
    /// FIN, replayed from `reset-binding.c`'s rows for both flavours: a reset
    /// releases the survivor's port (on Linux, unless it was bound
    /// explicitly) though `getsockname` still reports it, and Darwin then
    /// refuses `setsockopt`.
    [<Test>]
    let ``a reset releases its survivor's port and, on Darwin, its options, as each flavour was measured to``
        ()
        : unit
        =
        for platform in Machines.platforms do
            let resource =
                match SimulatedUnixPlatform.flavour platform with
                | SimulatedUnixFlavour.Linux -> "WoofWare.PosixKernel.Test.resetBinding.linux.txt"
                | SimulatedUnixFlavour.Darwin -> "WoofWare.PosixKernel.Test.resetBinding.darwin.txt"

            use stream =
                match Assembly.GetExecutingAssembly().GetManifestResourceStream resource with
                | null -> failwith $"embedded resource %s{resource} not found"
                | stream -> stream

            use reader = new StreamReader (stream)

            let rows =
                reader.ReadToEnd().Split '\n'
                |> Array.filter (fun line -> line.StartsWith ("survivor=", StringComparison.Ordinal))
                |> Array.toList

            rows.Length |> shouldEqual 8

            let answer (failed : UnixError option) : string =
                match failed with
                | None -> "ok"
                | Some UnixError.EINVAL -> "-1 Invalid argument"
                | Some UnixError.EADDRINUSE -> "-1 Address already in use"
                | Some other -> $"-1 %O{other}"

            for row in rows do
                let field (name : string) =
                    row.Split ' '
                    |> Array.find (fun part -> part.StartsWith (name + "=", StringComparison.Ordinal))
                    |> fun part -> part.Substring (name.Length + 1)

                let survivor = field "survivor"
                let reset = field "how" = "r"
                let locked = field "locked" = "1"

                let system = systemOn id platform
                let listener, system = KeventWorld.stream false system
                let listenerPort = if locked then 47100us else 0us

                let system =
                    match
                        CopyIn.bind
                            listener
                            UserBuffer.Mapped
                            16u
                            (CopyIn.inet platform (KeventWorld.loopback listenerPort))
                            system
                    with
                    | Ok (BindAnswer.Bound _, system) -> system
                    | other -> failwith $"bind: %A{other}"

                let system = KeventWorld.listen listener system

                let port =
                    match (UnixMachineState.socket (socketOf listener system) system.Machine).Binding with
                    | Some binding -> binding.Endpoint.Port
                    | None -> failwith "the listener is unbound"

                let client, system = KeventWorld.stream false system

                let system =
                    if locked then
                        KeventWorld.bind client 47101us system
                    else
                        system

                let system =
                    match KeventWorld.connect client port system with
                    | ConnectOutcome.Completed, system -> system
                    | other, _ -> failwith $"connect: %A{other}"

                let accepted, system = KeventWorld.accept listener system
                let system = KeventWorld.close listener system

                let keep, gone =
                    if survivor = "c" then
                        client, accepted
                    else
                        accepted, client

                let system = if reset then sentAll keep 10 system else system
                let system = KeventWorld.close gone system

                let endpoint =
                    match (UnixMachineState.socket (socketOf keep system) system.Machine).Binding with
                    | Some binding -> binding.Endpoint
                    | None -> failwith "the survivor is unbound"

                let soError, system =
                    match KeventWorld.readSocketError keep system with
                    | GetSockOptAnswer.Reported (value, _), system -> value, system
                    | other -> failwith $"SO_ERROR: %A{other}"

                let level = SimulatedUnixPlatform.socketOptionLevel platform
                let reuse = SimulatedUnixPlatform.reuseAddressOption platform

                let setsockopt, system =
                    match UnixSocket.admitSetSockOpt keep level reuse UserBuffer.Mapped 4u system with
                    | Ok (SetSockOptAdmission.Answered error) -> Some error, system
                    | Ok (SetSockOptAdmission.Transfer _) ->
                        match UnixSocket.setsockopt keep level reuse UserBuffer.Mapped 4u (Some 1) system with
                        | Ok (SetSockOptAnswer.Set, system) -> None, system
                        | Ok (SetSockOptAnswer.Failed error, system) -> Some error, system
                        | Error refusal -> failwith $"setsockopt: %A{refusal}"
                    | Error refusal -> failwith $"setsockopt: %A{refusal}"

                let bindFresh (withReuse : bool) (system : UnixSystem<int, string>) =
                    let fresh, system = KeventWorld.stream false system

                    let system =
                        if withReuse then
                            ReuseAddress.set true fresh system
                        else
                            system

                    match CopyIn.bind fresh UserBuffer.Mapped 16u (CopyIn.inet platform endpoint) system with
                    | Ok (BindAnswer.Bound _, system) -> None, system
                    | Ok (BindAnswer.Failed error, system) -> Some error, system
                    | other -> failwith $"bind: %A{other}"

                let bind, system = bindFresh false system
                let bindReuse, system = bindFresh true system

                let portSame =
                    (UnixMachineState.socket (socketOf keep system) system.Machine).Binding
                    |> Option.map (fun binding -> binding.Endpoint)
                    |> (=) (Some endpoint)

                assertClean system

                let how = field "how"
                let lockedField = field "locked"
                let setsockoptAnswer = answer setsockopt
                let bindAnswer = answer bind
                let bindReuseAnswer = answer bindReuse
                let same = if portSame then 1 else 0

                // SO_ERROR in the flavour's own numbering, which is the
                // measured host's.
                let modelled =
                    $"survivor=%s{survivor} how=%s{how} locked=%s{lockedField} so_error=%d{soError} setsockopt=%s{setsockoptAnswer} bind=%s{bindAnswer} bind_reuse=%s{bindReuseAnswer} getsockname_port_same=%d{same}"

                modelled |> shouldEqual row

    /// Linux's accept that reads a negative address length answers EINVAL
    /// having taken the connection, whose server end then closes as a close
    /// does: over bytes the client had sent it, a reset.
    [<Test>]
    let ``a connection a Linux accept drops resets a client that had written to it`` () : unit =
        let system = systemOn id SimulatedUnixPlatform.linuxX64
        let listener, system = KeventWorld.listenerAt port system
        let client, system = KeventWorld.stream true system
        let _, system = KeventWorld.connect client port system
        let system = sentAll client 10 system

        let system =
            match UnixConnection.accept 1 listener UserBuffer.Mapped 0xFFFFFFFFu system with
            | Ok (AcceptOutcome.DroppedConnection UnixError.EINVAL, system) -> system
            | other -> failwith $"accept: %A{other}"

        assertClean system

        ReadOutcomes.read client UserBuffer.Mapped 10UL system
        |> Result.map fst
        |> shouldEqual (Ok (ReadAnswer.Failed UnixError.ECONNRESET))

    // ------------------------------------------------------------------
    // The invariants
    // ------------------------------------------------------------------

    /// `system` with `connection`'s transfer as `change` makes it.
    let private withTransfer
        (change : TcpTransfer -> TcpTransfer)
        (system : UnixSystem<int, string>)
        : ConnectionId * UnixSystem<int, string>
        =
        let connection = system.Machine.Connections |> Map.toList |> List.exactlyOne |> fst
        let tcp = system.Machine.Connections.[connection]

        connection,
        { system with
            Machine =
                { system.Machine with
                    Connections =
                        Map.add
                            connection
                            { tcp with
                                Transfer = change tcp.Transfer
                            }
                            system.Machine.Connections
                }
        }

    [<Test>]
    let ``checkInvariants rejects a connection whose bytes or ends disagree with its sockets`` () : unit =
        for platform in Machines.platforms do
            let flavour = SimulatedUnixPlatform.flavour platform
            let c, s, system = pair true (systemOn id platform)
            assertClean system

            // A receive buffer holding more than its capacity.
            let connection, overfull =
                system
                |> withTransfer (fun transfer ->
                    { transfer with
                        ToClient =
                            { transfer.ToClient with
                                Receiving =
                                    ByteQueue.append
                                        (ImmutableArray.Create<byte> (
                                            Array.zeroCreate<byte> (transfer.ToClient.ReceiveCapacity + 1)
                                        ))
                                        ByteQueue.empty
                            }
                    }
                )

            match UnixSystem.checkInvariants overfull with
            | [ UnixSystemDefect.TcpTransferBroken (broken, [ _ ]) ] -> broken |> shouldEqual connection
            | other -> failwith $"%A{other}"

            // The other flavour's rules.
            let other =
                match flavour with
                | SimulatedUnixFlavour.Linux -> TcpTransferRules.Darwin
                | SimulatedUnixFlavour.Darwin -> TcpTransferRules.Linux Set.empty

            let otherFlavour =
                match flavour with
                | SimulatedUnixFlavour.Linux -> SimulatedUnixFlavour.Darwin
                | SimulatedUnixFlavour.Darwin -> SimulatedUnixFlavour.Linux

            system
            |> withTransfer (fun transfer ->
                { transfer with
                    Rules = other
                }
            )
            |> snd
            |> UnixSystem.checkInvariants
            |> shouldEqual [ UnixSystemDefect.TcpTransferNotOfFlavour (connection, otherFlavour, flavour) ]

            // An end closed under its socket.
            system
            |> withTransfer (TcpTransfer.close ConnectionEnd.Client >> snd)
            |> snd
            |> UnixSystem.checkInvariants
            |> shouldEqual
                [
                    UnixSystemDefect.ConnectionEndClosedUnderSocket (
                        connection,
                        ConnectionEnd.Client,
                        socketOf c system
                    )
                ]

            // An end open with no socket: the server's socket has forgotten it.
            let serverSocket = socketOf s system

            { system with
                Machine =
                    { system.Machine with
                        Sockets =
                            Map.add
                                serverSocket
                                { system.Machine.Sockets.[serverSocket] with
                                    Phase = SocketPhase.Idle
                                }
                                system.Machine.Sockets
                    }
            }
            |> UnixSystem.checkInvariants
            |> shouldEqual
                [
                    UnixSystemDefect.ConnectionEndOpenWithoutHolder (connection, ConnectionEnd.Server)
                ]

    // ------------------------------------------------------------------
    // Across processes
    // ------------------------------------------------------------------

    /// A connection from a socket of `a` to a listener of `b`, accepted, both
    /// ends non-blocking: the client's descriptor in `a`, the accepted one's
    /// in `b`.
    let private across
        (a : ProcessId)
        (b : ProcessId)
        (machine : SimulatedMachine<int, string>)
        : int * int * SimulatedMachine<int, string>
        =
        let listener, machine = Machines.inProcess b (KeventWorld.listenerAt port) machine
        let client, machine = Machines.inProcess a (KeventWorld.client port) machine

        let accepted, machine =
            Machines.inProcess
                b
                (fun view ->
                    let accepted, view = KeventWorld.accept listener view
                    accepted, UnixDescriptor.setNonBlocking accepted true view |> snd
                )
                machine

        client, accepted, machine

    let private woken
        (asleep : (ProcessId * int) list)
        (machine : SimulatedMachine<int, string>)
        : (ProcessId * int) list
        =
        let asleep =
            asleep
            |> List.groupBy fst
            |> List.map (fun (pid, tasks) -> pid, tasks |> List.map snd |> Set.ofList)
            |> Map.ofList

        SimulatedMachine.wakes asleep machine |> List.map fst

    /// `task` of `pid` waits with no timeout on the flavour's event queue,
    /// watching `fd` for `what` ('r' or 'w'), edge-triggered, and sleeps.
    let private sleepOn
        (pid : ProcessId)
        (task : int)
        (fd : int)
        (what : char)
        (machine : SimulatedMachine<int, string>)
        : SimulatedMachine<int, string>
        =
        Machines.doIn
            pid
            (fun view ->
                match SimulatedUnixPlatform.flavour view.Machine.UnixPlatform with
                | SimulatedUnixFlavour.Linux ->
                    let wait, view = edgeRegister fd what view
                    let _, view = wait view

                    let epoll =
                        FileDescriptorRegistry.fds (UnixSystemState.fileDescriptors view)
                        |> Map.keys
                        |> Seq.filter (fun fd ->
                            match FileDescriptorRegistry.tryFindTarget fd (UnixSystemState.fileDescriptors view) with
                            | Some (OpenFileTarget.Epoll _) -> true
                            | _ -> false
                        )
                        |> Seq.exactlyOne

                    match UnixPoll.epollWait task epoll 2 UserBuffer.Mapped -1 view with
                    | Ok (EpollWaitOutcome.WouldBlock _, view) -> view
                    | other -> failwith $"epoll_wait: expected to sleep, got %A{other}"
                | SimulatedUnixFlavour.Darwin ->
                    let wait, view = edgeRegister fd what view
                    let _, view = wait view

                    let kq =
                        FileDescriptorRegistry.fds (UnixSystemState.fileDescriptors view)
                        |> Map.keys
                        |> Seq.filter (fun fd ->
                            match FileDescriptorRegistry.tryFindTarget fd (UnixSystemState.fileDescriptors view) with
                            | Some (OpenFileTarget.Kqueue _) -> true
                            | _ -> false
                        )
                        |> Seq.exactlyOne

                    match UnixKqueue.kevent task kq 0 [] 2 UserBuffer.Mapped KeventTimeout.Null view with
                    | Ok (KeventOutcome.WouldBlock _, view) -> view
                    | other -> failwith $"kevent: expected to sleep, got %A{other}"
            )
            machine

    [<Test>]
    let ``bytes one process writes wake another's event queue and poll asleep on its end`` () : unit =
        for platform in Machines.platforms do
            let pids, machine = Machines.ofCount platform 2
            let a, b = pids.[0], pids.[1]
            let client, accepted, machine = across a b machine

            let machine = sleepOn b 1 accepted 'r' machine

            let machine =
                Machines.doIn
                    b
                    (fun view ->
                        match
                            UnixPoll.poll
                                2
                                [
                                    {
                                        Fd = accepted
                                        Events = 0x1s
                                    }
                                ]
                                -1
                                view
                        with
                        | Ok (PollOutcome.WouldBlock _, view) -> view
                        | other -> failwith $"poll: expected to sleep, got %A{other}"
                    )
                    machine

            Machines.assertClean machine
            woken [ b, 1 ; b, 2 ] machine |> shouldEqual []

            let machine = Machines.doIn a (sentAll client 100) machine
            Machines.assertClean machine
            woken [ b, 1 ; b, 2 ] machine |> List.sort |> shouldEqual [ b, 1 ; b, 2 ]

    [<Test>]
    let ``a read in one process wakes another asleep for send space on its end`` () : unit =
        for platform in Machines.platforms do
            let pids, machine = Machines.ofCount platform 2
            let a, b = pids.[0], pids.[1]
            let client, accepted, machine = across a b machine

            // `b` fills its end, then sleeps for room.
            let machine = Machines.doIn b (fun view -> fill accepted view |> snd) machine
            let machine = sleepOn b 1 accepted 'w' machine
            woken [ b, 1 ] machine |> shouldEqual []

            // `a` drains until `b`'s end has room by its flavour's rule.
            let rec drain (reads : int) (machine : SimulatedMachine<int, string>) =
                if reads = 0 then
                    failwith "the drain never woke the writer"
                else
                    let machine =
                        Machines.doIn a (fun view -> receivedCount client 4096 view |> snd) machine

                    Machines.assertClean machine

                    match woken [ b, 1 ] machine with
                    | [] -> drain (reads - 1) machine
                    | woke -> woke

            drain 2000 machine |> shouldEqual [ b, 1 ]

    [<Test>]
    let ``a process's end resets another's connection over bytes it left unread, and FINs it otherwise`` () : unit =
        for platform in Machines.platforms do
            for unread in [ true ; false ] do
                let pids, machine = Machines.ofCount platform 2
                let a, b = pids.[0], pids.[1]
                let client, accepted, machine = across a b machine

                // `a` sends 50; `b` sends 30, which `a` reads unless `unread`.
                let machine = Machines.doIn a (sentAll client 50) machine
                let machine = Machines.doIn b (sentAll accepted 30) machine

                let machine =
                    if unread then
                        machine
                    else
                        Machines.doIn a (fun view -> receivedCount client 4096 view |> snd) machine

                let ended =
                    Machines.inProcess a (fun view -> UnixTaskLifecycle.exitGroup 0 0 view, view) machine
                    |> fst

                let machine =
                    match SimulatedMachine.endProcess ended machine with
                    | Ok (_, machine) -> machine
                    | Error refusal -> failwith $"%A{refusal}"

                Machines.assertClean machine

                let answers =
                    Machines.inProcess
                        b
                        (fun view ->
                            let first, view = received accepted 4096 view
                            let second, view = received accepted 4096 view
                            [ first ; second ], view
                        )
                        machine
                    |> fst

                let bytes (count : int) =
                    ReadAnswer.Completed (ImmutableArray.Create (payload, 0, count))

                if unread then
                    // What arrived stays readable; then the reset.
                    answers |> shouldEqual [ bytes 50 ; ReadAnswer.Failed UnixError.ECONNRESET ]
                else
                    answers |> shouldEqual [ bytes 50 ; ReadAnswer.Completed ImmutableArray.Empty ]

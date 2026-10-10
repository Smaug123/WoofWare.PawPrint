namespace WoofWare.PosixKernel.Test

open System
open System.Collections.Immutable
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `TcpTransfer`'s half-close rules replayed through sections S, T, R, P and L
/// of `tcp-shutdown.c` (docs/plans/2026-10-08-tcp-shutdown-linger), measured
/// on Linux 6.18.5 aarch64 and Darwin 27.0, on a connection with each
/// flavour's default buffers. `c`, the connecting socket, is the client end;
/// `p`, its accepted peer, the server end.
///
/// Each section is driven as the probe drives it, and each line the probe
/// printed is rebuilt from the model and compared with it. A line marked
/// `~timing` is not compared (the plan's section 2 says why each is excluded),
/// and on a line marked `~counts` the byte counts are not. In L, the linger
/// option's own answers and the binds are not this layer's, so they are not
/// compared either.
///
/// Readiness is rebuilt from the facts `TcpTransfer` keeps by the plan's
/// section 2.3: on Linux `tcp_poll`'s rules over the shut sides, on Darwin
/// `filt_soread` and `filt_sowrite` folded by `KqueuePoll.callback` as a
/// Darwin `poll` folds them.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestTcpShutdownMeasured =

    let private c : ConnectionEnd = ConnectionEnd.Client
    let private p : ConnectionEnd = ConnectionEnd.Server

    let private errorName (error : TcpError) : string =
        match error with
        | TcpError.ConnectionReset -> "ECONNRESET"
        | TcpError.BrokenPipe -> "EPIPE"

    /// A Darwin error's number, as `EVFILT_READ` and `EVFILT_WRITE` report it
    /// in `fflags`.
    let private darwinErrno (error : TcpError) : int =
        match error with
        | TcpError.ConnectionReset -> 54
        | TcpError.BrokenPipe -> 32

    /// One connection, driven call by call as the probe drives its pair.
    /// Binds, edges and raw `how`s are not this layer's.
    type private Session (flavour : SimulatedUnixFlavour) =
        let platform =
            match flavour with
            | SimulatedUnixFlavour.Linux -> SimulatedUnixPlatform.linuxX64
            | SimulatedUnixFlavour.Darwin -> SimulatedUnixPlatform.macOsArm64

        let mutable transfer : TcpTransfer =
            UnixSystem.initial<int, string> platform
            |> Launched.boot UnixSystem.pipedStandardStreams 0 (CpuId 0)
            |> fun system -> TcpBufferSizing.newTransfer SocketDomain.Inet system.Machine

        let mutable refused = false

        let notThisLayer (what : string) : 'a =
            failwith $"TcpTransfer has no %s{what}: the section asking for it is not this layer's to replay"

        member _.WriteAnswer (writer : ConnectionEnd) (count : int) : Result<int, string> =
            match TcpTransfer.admitWrite writer count transfer with
            | TcpWriteAdmission.Answered answer, after ->
                transfer <- after

                match answer with
                | TcpWriteAnswer.Wrote n -> Ok n
                | TcpWriteAnswer.WouldBlock -> Error "-1 EAGAIN"
                // `EPIPE` raises `SIGPIPE`, which the probe counts.
                | TcpWriteAnswer.Failed TcpError.BrokenPipe -> Error "-1 EPIPE+SIGPIPE"
                | TcpWriteAnswer.Failed error -> Error ("-1 " + errorName error)
            | TcpWriteAdmission.Take taken, after ->
                let bytes = ImmutableArray.CreateRange (Array.zeroCreate<byte> taken)
                transfer <- snd (TcpTransfer.write writer bytes after)
                Ok taken

        member _.Read (reader : ConnectionEnd) (count : int) : string =
            let answer, _, after = TcpTransfer.read reader TcpReceiveCall.Read count transfer
            transfer <- after

            match answer with
            | TcpReadAnswer.Bytes bytes -> string bytes.Length
            | TcpReadAnswer.EndOfFile -> "0"
            | TcpReadAnswer.WouldBlock -> "-1 EAGAIN"
            | TcpReadAnswer.Failed error -> "-1 " + errorName error

        interface IShutdownPair with
            member _.Refused = refused
            member this.Read reader count = this.Read reader count
            member this.WriteAnswer writer count = this.WriteAnswer writer count

            member _.SoError e =
                let error, after = TcpTransfer.takeError e transfer
                transfer <- after

                match error with
                | None -> "0"
                | Some error -> errorName error

            member _.Shutdown e how =
                match TcpTransfer.shutdown e how transfer with
                | Ok (answer, _, after) ->
                    transfer <- after

                    match answer with
                    | TcpShutdownAnswer.Shut -> "0"
                    | TcpShutdownAnswer.NotConnected -> "-1 ENOTCONN"
                | Error _ ->
                    refused <- true
                    ""

            member _.ShutdownRaw _ _ = notThisLayer "raw how"

            member _.Close e =
                if TcpTransfer.closeRefused e transfer then
                    refused <- true
                else
                    transfer <- snd (TcpTransfer.close e transfer)

                "0"

            member _.Abort e =
                if TcpTransfer.abortRefused e transfer then
                    refused <- true
                else
                    transfer <- snd (TcpTransfer.abort e transfer)

                "0"

            member _.Fionread e = TcpTransfer.readable e transfer

            member this.Fill writer =
                let rec go (total : int64) =
                    match this.WriteAnswer writer (1 <<< 20) with
                    | Ok n -> go (total + int64 n)
                    | Error _ -> total

                go 0L

            member this.Drain reader =
                let rec go (total : int64) =
                    match this.Read reader (1 <<< 20) with
                    | answer when answer.StartsWith ("-", StringComparison.Ordinal) || answer = "0" -> total, answer
                    | count -> go (total + int64 count)

                go 0L

            member _.Settle () = ()
            member _.RegisterEdges _ = notThisLayer "edge registration"
            member _.Edges () = notThisLayer "edge registration"
            member _.PortOf _ = 0
            member _.BindFree _ = notThisLayer "bind"
            member _.PeerName _ = notThisLayer "getpeername"
            member _.BindSeesClosedEnds = false

            // What the probe's `rdy` prints for `e`: `poll` (and, on Linux, an
            // epoll registration, which agrees with it), then `FIONREAD`.
            member _.Rdy e =
                let readable = TcpTransfer.readable e transfer
                let state = (TcpTransfer.towards e transfer).Receiver
                let receiveShut = TcpTransfer.receiveShut e transfer
                let sendShut = TcpTransfer.sendShut e transfer
                let pending = TcpTransfer.pendingError e transfer

                match flavour with
                | SimulatedUnixFlavour.Linux ->
                    // `poll` and the epoll ADD are each a `tcp_poll`.
                    transfer <- TcpTransfer.polled e transfer

                    let bits =
                        match state with
                        | TcpEndState.Closed -> failwith "rdy of a closed end"
                        | TcpEndState.Reset _ -> 0x1 ||| 0x4 ||| 0x10 ||| 0x2000 ||| (if pending.IsSome then 0x8 else 0)
                        | TcpEndState.Open _ ->
                            (if readable > 0 || receiveShut then 0x1 else 0)
                            ||| (if sendShut || TcpTransfer.linuxSendable e transfer then
                                     0x4
                                 else
                                     0)
                            ||| (if receiveShut && sendShut then 0x10 else 0)
                            ||| (if receiveShut then 0x2000 else 0)

                    $"poll=0x%x{bits} epoll=0x%x{bits} fionread=%d{readable}"
                | SimulatedUnixFlavour.Darwin ->
                    let isReset =
                        match state with
                        | TcpEndState.Reset _ -> true
                        | TcpEndState.Open _ -> false
                        | TcpEndState.Closed -> failwith "rdy of a closed end"

                    let fflags = pending |> Option.map darwinErrno |> Option.defaultValue 0
                    let space = int64 (TcpTransfer.sendSpace e transfer)

                    let readReport =
                        if isReset || receiveShut then
                            Some (KqueueFilterReport.EndOfFile (int64 readable, None))
                        elif readable > 0 then
                            Some (KqueueFilterReport.Ready (int64 readable))
                        else
                            None

                    let writeReport =
                        if isReset || sendShut then
                            Some (KqueueFilterReport.EndOfFile (space, None))
                        elif space >= int64 TcpTransfer.darwinSendLowWater then
                            Some (KqueueFilterReport.Ready space)
                        else
                            None

                    let render (report : KqueueFilterReport option) : string =
                        match report with
                        | None -> "-"
                        | Some (KqueueFilterReport.Ready data) -> $"%d{data}/0"
                        | Some (KqueueFilterReport.EndOfFile (data, _)) -> $"%d{data}/EOF/%d{fflags}"

                    let events = DarwinPollEvents.In ||| DarwinPollEvents.Out ||| DarwinPollEvents.Pri

                    // A poll registers READ before WRITE, and its kqueue
                    // reports them in that order.
                    let revents =
                        [ KqueueFilter.Read, readReport ; KqueueFilter.Write, writeReport ]
                        |> List.fold
                            (fun revents (filter, report) ->
                                match report with
                                | None -> revents
                                | Some report -> KqueuePoll.callback events filter report false revents
                            )
                            0s

                    $"poll=0x%x{revents} kq-read=%s{render readReport} kq-write=%s{render writeReport} fionread=%d{readable}"

    let private pairOf (flavour : SimulatedUnixFlavour) (shape : ShutdownPairShape) : IShutdownPair =
        match shape with
        | ShutdownPairShape.Plain -> Session flavour :> IShutdownPair
        | other -> failwith $"TcpTransfer has no sockets to bind, so no %A{other} pair"

    let private replay (flavour : SimulatedUnixFlavour) (scenarios : ShutdownProbe.Scenario list) : ShutdownLine list =
        scenarios |> List.collect (fun scenario -> scenario (pairOf flavour))

    let private disagreementsBy
        (probe : ShutdownProbe.Probe)
        (flavour : SimulatedUnixFlavour)
        (section : string)
        (scenarios : ShutdownProbe.Scenario list)
        : string list
        =
        replay flavour scenarios |> ShutdownProbe.disagreements probe flavour section id

    let private disagreements = disagreementsBy ShutdownProbe.Probe.Shutdown

    let private flavours : SimulatedUnixFlavour list =
        [ SimulatedUnixFlavour.Linux ; SimulatedUnixFlavour.Darwin ]

    [<Test>]
    let ``shutdown answers, and what each end then sees, as each flavour measured (section S)`` () : unit =
        for flavour in flavours do
            disagreements flavour "S" ShutdownProbe.sectionS |> shouldEqual []

    [<Test>]
    let ``shutdown twice, and after the peer's FIN or reset, as each flavour measured (section T)`` () : unit =
        for flavour in flavours do
            disagreements flavour "T" ShutdownProbe.sectionT |> shouldEqual []

    [<Test>]
    let ``bytes arriving after the receive side is shut, as each flavour measured (section R)`` () : unit =
        for flavour in flavours do
            disagreements flavour "R" ShutdownProbe.sectionR |> shouldEqual []

    [<Test>]
    let ``what the peer sees of a close after shutdown, as each flavour measured (section P)`` () : unit =
        for flavour in flavours do
            disagreements flavour "P" ShutdownProbe.sectionP |> shouldEqual []

    [<Test>]
    let ``a close under linger zero resets by the two FINs, as each flavour measured (section L)`` () : unit =
        for flavour in flavours do
            disagreements flavour "L" ShutdownProbe.sectionL |> shouldEqual []

    [<Test>]
    let ``once both ends have shut writing, as each flavour measured (tcp-shutdown-exchange.c)`` () : unit =
        for flavour in flavours do
            for section in [ "K" ; "U" ; "Q" ; "V" ; "W" ; "T" ; "G" ] do
                disagreementsBy ShutdownProbe.Probe.Exchange flavour section (ShutdownProbe.sectionExchange section)
                |> shouldEqual []

    [<Test>]
    let ``the model refuses only where the probe marked the outcome as waiting on a timer`` () : unit =
        // A row the model refuses must be one the probe excluded, and the
        // refusals must be there at all: Darwin's SHUT_RD before the peer's
        // unsent bytes (R), and its close under linger zero after its own
        // queued FIN met the peer's (L).
        let refusals =
            [
                for flavour in flavours do
                    for section, scenarios in [ "R", ShutdownProbe.sectionR ; "L", ShutdownProbe.sectionL ] do
                        let probe = ShutdownProbe.measured ShutdownProbe.Probe.Shutdown flavour section
                        let model = replay flavour scenarios

                        for line, modelled in List.zip probe model do
                            match modelled with
                            | ShutdownLine.Refused -> line.Mark |> shouldEqual ShutdownMark.Timing
                            | ShutdownLine.Line _
                            | ShutdownLine.Elsewhere -> ()

                        let refused =
                            model |> List.filter (fun line -> line = ShutdownLine.Refused) |> List.length

                        flavour, section, refused
            ]

        refusals
        |> shouldEqual
            [
                SimulatedUnixFlavour.Linux, "R", 0
                SimulatedUnixFlavour.Linux, "L", 0
                SimulatedUnixFlavour.Darwin, "R", 4
                SimulatedUnixFlavour.Darwin, "L", 3
            ]

        // And Darwin's close over unread bytes after its own FIN was queued
        // and the peer's arrived, in either order.
        [
            for flavour in flavours do
                let refused =
                    replay flavour (ShutdownProbe.sectionExchange "G")
                    |> List.filter (fun line -> line = ShutdownLine.Refused)
                    |> List.length

                flavour, refused
        ]
        |> shouldEqual [ SimulatedUnixFlavour.Linux, 0 ; SimulatedUnixFlavour.Darwin, 2 ]

namespace WoofWare.PosixKernel.Test

open System
open System.Collections.Immutable
open System.IO
open System.Reflection
open System.Text.RegularExpressions
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `TcpTransfer` replayed through every state and call of the S section of
/// `tcp-transfer.c` (docs/plans/2026-10-07-tcp-byte-transfer), measured on
/// Linux 6.18.5 aarch64 and Darwin 27.0, on a connection with each flavour's
/// default buffers. `s`, the observed socket, is the client end; `p`, its
/// accepted peer, the server end.
///
/// Each row is held on its answers, on `FIONREAD` before and after, and on
/// whether an error is pending before and after: on Linux, `POLLERR` in
/// `poll`'s `revents`; on Darwin, `EVFILT_READ`'s `fflags`.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestTcpTransferMeasured =

    [<RequireQualifiedAccess>]
    type private State =
        | Idle
        | DataIn
        | SendFull
        | Fin
        | FinData
        | FinDrained
        | Reset
        | ResetData
        | LingerZero
        | FinWritten
        | SendFullReset

    [<RequireQualifiedAccess>]
    type private Operation =
        /// `getsockopt(SO_ERROR)`.
        | Readiness
        | Read
        | ReadZero
        | Recv
        | RecvZero
        /// `recv(MSG_PEEK)` twice, then `read`.
        | Peek
        | Write
        | WriteZero
        | Send
        | SendZero
        | SendNoSignal
        /// `recv(MSG_DONTWAIT)`, then `send(MSG_DONTWAIT)` of 65536 bytes.
        | DontWait

    [<RequireQualifiedAccess>]
    type private Seen =
        | Returned of count : int64
        | Failed of error : UnixError * raisedSigPipe : bool
        | PendingError of error : UnixError option

    type private Readiness =
        {
            ErrorPending : bool
            Readable : int
        }

    type private Row =
        {
            State : State
            Operation : Operation
            Before : Readiness
            Seen : Seen list
            After : Readiness
        }

    let private stateNamed (name : string) : State =
        match name with
        | "IDLE" -> State.Idle
        | "DATA_IN" -> State.DataIn
        | "SNDFULL" -> State.SendFull
        | "FIN" -> State.Fin
        | "FIN_DATA" -> State.FinData
        | "FIN_DRAINED" -> State.FinDrained
        | "RST" -> State.Reset
        | "RST_DATA" -> State.ResetData
        | "LINGER0" -> State.LingerZero
        | "FIN_WRITTEN" -> State.FinWritten
        | "SNDFULL_RST" -> State.SendFullReset
        | other -> failwith $"the probe reported state %s{other}, which this test does not know"

    let private operationNamed (name : string) : Operation =
        match name with
        | "none" -> Operation.Readiness
        | "read" -> Operation.Read
        | "read0" -> Operation.ReadZero
        | "recv" -> Operation.Recv
        | "recv0" -> Operation.RecvZero
        | "peek" -> Operation.Peek
        | "write" -> Operation.Write
        | "write0" -> Operation.WriteZero
        | "send" -> Operation.Send
        | "send0" -> Operation.SendZero
        | "sendnosig" -> Operation.SendNoSignal
        | "dontwait" -> Operation.DontWait
        | other -> failwith $"the probe reported operation %s{other}, which this test does not know"

    let private errorNamed (name : string) : UnixError =
        match name with
        | "EAGAIN" -> UnixError.EAGAIN
        | "EPIPE" -> UnixError.EPIPE
        | "ECONNRESET" -> UnixError.ECONNRESET
        | other -> failwith $"the probe reported %s{other}, which this test does not know"

    let private readablePattern : Regex = Regex @"fionread=(-?\d+)"
    let private reventsPattern : Regex = Regex @"poll=0x([0-9a-f]+)"
    let private darwinReadPattern : Regex = Regex @"READ\((-|[^)]*fflags=(\d+))\)"

    let private readinessOf (flavour : SimulatedUnixFlavour) (text : string) : Readiness =
        let readable = readablePattern.Match text

        if not readable.Success then
            failwith $"no FIONREAD in %s{text}"

        let errorPending =
            match flavour with
            | SimulatedUnixFlavour.Linux ->
                let revents = reventsPattern.Match text

                if not revents.Success then
                    failwith $"no revents in %s{text}"

                // POLLERR
                Convert.ToInt32 (revents.Groups.[1].Value, 16) &&& 0x8 <> 0
            | SimulatedUnixFlavour.Darwin ->
                let filter = darwinReadPattern.Match text

                if not filter.Success then
                    failwith $"no EVFILT_READ in %s{text}"

                filter.Groups.[1].Value <> "-" && filter.Groups.[2].Value <> "0"

        {
            ErrorPending = errorPending
            Readable = int readable.Groups.[1].Value
        }

    let private seenOf (text : string) : Seen =
        if text.StartsWith ("SO_ERROR=", StringComparison.Ordinal) then
            match text.Substring 9 with
            | "0" -> Seen.PendingError None
            | name -> Seen.PendingError (Some (errorNamed name))
        else
            match text.Split ' ' |> Array.toList with
            | [ "-1" ; name ] -> Seen.Failed (errorNamed name, false)
            | [ "-1" ; name ; "SIGPIPE" ] -> Seen.Failed (errorNamed name, true)
            | [ count ] -> Seen.Returned (int64 count)
            | _ -> failwith $"the probe reported %s{text}, which this test cannot read"

    let private rowsOf (flavour : SimulatedUnixFlavour) : Row list =
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
            | [ "S" ; state ; operation ; before ; seen ; after ] ->
                Some
                    {
                        State = stateNamed state
                        Operation = operationNamed operation
                        Before = readinessOf flavour before
                        Seen = seen.Split " ; " |> Array.toList |> List.map seenOf
                        After = readinessOf flavour after
                    }
            | _ -> None
        )

    let private s : ConnectionEnd = ConnectionEnd.Client
    let private p : ConnectionEnd = ConnectionEnd.Server

    /// A fresh connection between two sockets of a default machine of
    /// `flavour`, over IPv4, as the probe's were.
    let private fresh (flavour : SimulatedUnixFlavour) : TcpTransfer =
        let platform =
            match flavour with
            | SimulatedUnixFlavour.Linux -> SimulatedUnixPlatform.linuxX64
            | SimulatedUnixFlavour.Darwin -> SimulatedUnixPlatform.macOsArm64

        let system =
            UnixSystem.initial<int, string> platform UnixSystem.pipedStandardStreams 0 (CpuId 0)
            |> UnixBootImage.boot

        TcpBufferSizing.newTransfer SocketDomain.Inet system.Machine

    /// A write by `writer` of `count` bytes: what it answered, the transfer
    /// after.
    let private write (writer : ConnectionEnd) (count : int) (transfer : TcpTransfer) : TcpWriteAnswer * TcpTransfer =
        match TcpTransfer.admitWrite writer count transfer with
        | TcpWriteAdmission.Answered answer, transfer -> answer, transfer
        | TcpWriteAdmission.Take taken, transfer ->
            let bytes = ImmutableArray.CreateRange (Array.init taken byte)
            TcpWriteAnswer.Wrote taken, snd (TcpTransfer.write writer bytes transfer)

    let private writeExactly (writer : ConnectionEnd) (count : int) (transfer : TcpTransfer) : TcpTransfer =
        match write writer count transfer with
        | TcpWriteAnswer.Wrote written, transfer when written = count -> transfer
        | answer, _ -> failwith $"a write of %d{count} by %A{writer} answered %A{answer}"

    let private close (closer : ConnectionEnd) (transfer : TcpTransfer) : TcpTransfer =
        snd (TcpTransfer.close closer transfer)

    let private read
        (reader : ConnectionEnd)
        (call : TcpReceiveCall)
        (count : int)
        (transfer : TcpTransfer)
        : TcpReadAnswer * TcpTransfer
        =
        let answer, _, transfer = TcpTransfer.read reader call count transfer
        answer, transfer

    /// Bring a fresh connection to `state`, as the probe's `build` does.
    let private build (flavour : SimulatedUnixFlavour) (state : State) : TcpTransfer =
        let rec fill (transfer : TcpTransfer) : TcpTransfer =
            match write s 65536 transfer with
            | TcpWriteAnswer.Wrote _, transfer -> fill transfer
            | TcpWriteAnswer.WouldBlock, transfer -> transfer
            | answer, _ -> failwith $"filling the send buffer answered %A{answer}"

        let transfer = fresh flavour

        match state with
        | State.Idle -> transfer
        | State.DataIn -> transfer |> writeExactly p 1000
        | State.SendFull -> fill transfer
        | State.SendFullReset -> fill transfer |> close p
        | State.Fin -> transfer |> close p
        | State.FinData -> transfer |> writeExactly p 1000 |> close p
        | State.FinDrained ->
            match transfer |> writeExactly p 1000 |> close p |> read s TcpReceiveCall.Read 4096 with
            | TcpReadAnswer.Bytes bytes, transfer when bytes.Length = 1000 -> transfer
            | answer, _ -> failwith $"FIN_DRAINED: the drain answered %A{answer}"
        | State.Reset -> transfer |> writeExactly s 1000 |> close p
        | State.ResetData -> transfer |> writeExactly p 1000 |> writeExactly s 1000 |> close p
        | State.LingerZero -> snd (TcpTransfer.abort p transfer)
        | State.FinWritten -> transfer |> close p |> writeExactly s 100

    let private readSeen (answer : TcpReadAnswer) : Seen =
        match answer with
        | TcpReadAnswer.Bytes bytes -> Seen.Returned (int64 bytes.Length)
        | TcpReadAnswer.EndOfFile -> Seen.Returned 0L
        | TcpReadAnswer.WouldBlock -> Seen.Failed (UnixError.EAGAIN, false)
        | TcpReadAnswer.Failed error -> Seen.Failed (TcpError.toUnixError error, false)

    let private writeSeen (signals : bool) (answer : TcpWriteAnswer) : Seen =
        match answer with
        | TcpWriteAnswer.Wrote count -> Seen.Returned (int64 count)
        | TcpWriteAnswer.WouldBlock -> Seen.Failed (UnixError.EAGAIN, false)
        | TcpWriteAnswer.Failed TcpError.BrokenPipe -> Seen.Failed (UnixError.EPIPE, signals)
        | TcpWriteAnswer.Failed TcpError.ConnectionReset -> Seen.Failed (UnixError.ECONNRESET, false)

    /// One call: a read or a write, and the bytes a write offers.
    [<RequireQualifiedAccess>]
    type private Call =
        | Receive of call : TcpReceiveCall * count : int
        | Send of count : int * signals : bool
        | TakeError

    let private callsOf (operation : Operation) : Call list =
        let thrice (call : Call) = List.replicate 3 call

        match operation with
        | Operation.Readiness -> [ Call.TakeError ]
        | Operation.Read -> thrice (Call.Receive (TcpReceiveCall.Read, 4096))
        | Operation.ReadZero -> thrice (Call.Receive (TcpReceiveCall.Read, 0))
        | Operation.Recv -> thrice (Call.Receive (TcpReceiveCall.Receive, 4096))
        | Operation.RecvZero -> thrice (Call.Receive (TcpReceiveCall.Receive, 0))
        | Operation.Peek ->
            [
                Call.Receive (TcpReceiveCall.Peek, 4096)
                Call.Receive (TcpReceiveCall.Peek, 4096)
                Call.Receive (TcpReceiveCall.Read, 4096)
            ]
        | Operation.Write
        | Operation.Send -> thrice (Call.Send (100, true))
        | Operation.WriteZero
        | Operation.SendZero -> thrice (Call.Send (0, true))
        | Operation.SendNoSignal -> thrice (Call.Send (100, false))
        | Operation.DontWait -> [ Call.Receive (TcpReceiveCall.Receive, 4096) ; Call.Send (65536, true) ]

    let private perform (call : Call) (transfer : TcpTransfer) : Seen * TcpTransfer =
        match call with
        | Call.TakeError ->
            let error, transfer = TcpTransfer.takeError s transfer
            Seen.PendingError (error |> Option.map TcpError.toUnixError), transfer
        | Call.Receive (call, count) ->
            let answer, transfer = read s call count transfer
            readSeen answer, transfer
        | Call.Send (count, signals) ->
            let answer, transfer = write s count transfer
            writeSeen signals answer, transfer

    let private errorPending (transfer : TcpTransfer) : bool =
        match (TcpTransfer.towards s transfer).Receiver with
        | TcpEndState.Reset (_, pending) -> pending
        | TcpEndState.Open
        | TcpEndState.FinQueued
        | TcpEndState.FinReceived
        | TcpEndState.Closed -> false

    let private readinessOfModel (transfer : TcpTransfer) : Readiness =
        {
            ErrorPending = errorPending transfer
            Readable = TcpTransfer.readable s transfer
        }

    /// Whether the row's answer to `call` is one the model is held to. A send
    /// buffer filled to EAGAIN is not a state either kernel holds still:
    /// Linux frees space on a timer, and Darwin's loopback drains on another
    /// thread, so a later write there is taken where the model, which frees
    /// space only when the peer reads, still answers EAGAIN. Linux's first
    /// write in the state is still held, being made before any time passes;
    /// Darwin's is not, having drained before the probe made it.
    let private heldTo (flavour : SimulatedUnixFlavour) (state : State) (call : Call) (writesBefore : int) : bool =
        match state, call with
        | State.SendFull, Call.Send (count, _) when count > 0 ->
            match flavour with
            | SimulatedUnixFlavour.Linux -> writesBefore = 0
            | SimulatedUnixFlavour.Darwin -> false
        | _ -> true

    let private disagreements (flavour : SimulatedUnixFlavour) : string list =
        rowsOf flavour
        |> List.collect (fun row ->
            let name = $"%A{flavour} %A{row.State} %A{row.Operation}"
            let transfer = build flavour row.State
            let before = readinessOfModel transfer
            let calls = callsOf row.Operation

            if calls.Length <> row.Seen.Length then
                failwith $"%s{name}: the probe made %d{row.Seen.Length} calls, and this test %d{calls.Length}"

            let answers, transfer, _ =
                List.zip calls row.Seen
                |> List.fold
                    (fun (answers, transfer, writes) (call, measured) ->
                        let seen, transfer = perform call transfer

                        let isWrite =
                            match call with
                            | Call.Send (count, _) -> count > 0
                            | Call.Receive _
                            | Call.TakeError -> false

                        let answers =
                            if heldTo flavour row.State call writes then
                                answers @ [ seen, measured ]
                            else
                                answers

                        answers, transfer, (if isWrite then writes + 1 else writes)
                    )
                    ([], transfer, 0)

            let after = readinessOfModel transfer

            [
                if before <> row.Before then
                    $"%s{name}: before the calls the model reports %A{before}, measured %A{row.Before}"
                for seen, measured in answers do
                    if seen <> measured then
                        $"%s{name}: the model answered %A{seen}, measured %A{measured}"
                // The probe read a dontwait row's readiness straight after its
                // send, with no pause: Darwin's loopback had not yet delivered
                // the bytes, nor the reset a write after a FIN provokes, which
                // the model delivers at once. Its receive side had settled.
                let after, measuredAfter =
                    match flavour, row.Operation with
                    | SimulatedUnixFlavour.Darwin, Operation.DontWait ->
                        { after with
                            ErrorPending = false
                        },
                        { row.After with
                            ErrorPending = false
                        }
                    | _ -> after, row.After

                if after <> measuredAfter then
                    $"%s{name}: after the calls the model reports %A{after}, measured %A{measuredAfter}"
            ]
        )

    [<Test>]
    let ``each flavour's run holds every state and operation`` () : unit =
        for flavour in [ SimulatedUnixFlavour.Linux ; SimulatedUnixFlavour.Darwin ] do
            let rows = rowsOf flavour
            rows.Length |> shouldEqual (11 * 12)

            rows
            |> List.map (fun row -> row.State, row.Operation)
            |> List.distinct
            |> List.length
            |> shouldEqual (11 * 12)

    [<Test>]
    let ``Linux's transfer rules answer every measured state and call as Linux 6.18 did`` () : unit =
        disagreements SimulatedUnixFlavour.Linux |> shouldEqual []

    [<Test>]
    let ``Darwin's transfer rules answer every measured state and call as Darwin 27 did`` () : unit =
        disagreements SimulatedUnixFlavour.Darwin |> shouldEqual []

namespace WoofWare.PosixKernel.Test

open System
open System.Collections.Immutable
open System.IO
open System.Reflection
open System.Text.RegularExpressions
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

    [<RequireQualifiedAccess>]
    type private Mark =
        | Exact
        | Counts
        | Timing

    type private Measured =
        {
            Text : string
            Mark : Mark
        }

    /// What the model makes of one line of the probe's.
    [<RequireQualifiedAccess>]
    type private Modelled =
        | Line of string
        /// The model refused a call the line depends on.
        | Refused
        /// The line reports something this layer does not model.
        | Elsewhere

    /// The probe whose output a section is replayed from.
    [<RequireQualifiedAccess>]
    type private Probe =
        /// `tcp-shutdown.c`.
        | Shutdown
        /// `tcp-shutdown-exchange.c`: once both ends have shut writing.
        | Exchange

    let private measuredBy (probe : Probe) (flavour : SimulatedUnixFlavour) (section : string) : Measured list =
        let name =
            match probe with
            | Probe.Shutdown -> "tcpShutdown"
            | Probe.Exchange -> "tcpShutdownExchange"

        let resource =
            match flavour with
            | SimulatedUnixFlavour.Linux -> $"WoofWare.PosixKernel.Test.%s{name}.linux.txt"
            | SimulatedUnixFlavour.Darwin -> $"WoofWare.PosixKernel.Test.%s{name}.darwin.txt"

        use stream =
            match Assembly.GetExecutingAssembly().GetManifestResourceStream resource with
            | null -> failwith $"embedded resource %s{resource} not found"
            | stream -> stream

        use reader = new StreamReader (stream)

        reader.ReadToEnd().Split '\n'
        |> Array.toList
        |> List.map (fun line -> line.TrimEnd '\r')
        |> List.filter (fun line -> line.StartsWith (section + "\t", StringComparison.Ordinal))
        |> List.map (fun line ->
            if line.EndsWith ("\t~timing", StringComparison.Ordinal) then
                {
                    Text = line.Substring (0, line.Length - 8)
                    Mark = Mark.Timing
                }
            elif line.EndsWith ("\t~counts", StringComparison.Ordinal) then
                {
                    Text = line.Substring (0, line.Length - 8)
                    Mark = Mark.Counts
                }
            else
                {
                    Text = line
                    Mark = Mark.Exact
                }
        )

    let private measured : SimulatedUnixFlavour -> string -> Measured list =
        measuredBy Probe.Shutdown

    let private countsPattern : Regex =
        Regex @"(fionread=|fionread\(c\)=|kq-read=|kq-write=|p-drained=|p-took=|p-unsent\()-?\d+"

    let private withoutCounts (text : string) : string = countsPattern.Replace (text, "$1N")

    let private lingerPattern : Regex =
        Regex @"\tlinger set early=.*? at close=[^\t]*\t"

    /// The answers to setting `SO_LINGER` belong to `setsockopt`.
    let private withoutLinger (text : string) : string =
        lingerPattern.Replace (text, "\t(linger)\t")

    let private c : ConnectionEnd = ConnectionEnd.Client
    let private p : ConnectionEnd = ConnectionEnd.Server

    let private otherEnd (e : ConnectionEnd) : ConnectionEnd = if e = c then p else c

    let private howName (how : TcpShutdownHow) : string =
        match how with
        | TcpShutdownHow.Read -> "RD"
        | TcpShutdownHow.Write -> "WR"
        | TcpShutdownHow.Both -> "RDWR"

    let private hows : TcpShutdownHow list =
        [ TcpShutdownHow.Read ; TcpShutdownHow.Write ; TcpShutdownHow.Both ]

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

    /// One connection, driven call by call as the probe drives its pair, and
    /// the lines it prints.
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
        let mutable lines : Modelled list = []

        member _.Lines : Modelled list = List.rev lines

        member _.Emit (text : string) : unit =
            lines <- (if refused then Modelled.Refused else Modelled.Line text) :: lines

        member _.EmitElsewhere () : unit = lines <- Modelled.Elsewhere :: lines

        member _.Read (reader : ConnectionEnd) (count : int) : string =
            if refused then
                ""
            else
                let answer, _, after = TcpTransfer.read reader TcpReceiveCall.Read count transfer
                transfer <- after

                match answer with
                | TcpReadAnswer.Bytes bytes -> string bytes.Length
                | TcpReadAnswer.EndOfFile -> "0"
                | TcpReadAnswer.WouldBlock -> "-1 EAGAIN"
                | TcpReadAnswer.Failed error -> "-1 " + errorName error

        /// The count written, or `None` with the answer when nothing was.
        member _.WriteAnswer (writer : ConnectionEnd) (count : int) : Result<int, string> =
            if refused then
                Error ""
            else
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

        member this.Write (writer : ConnectionEnd) (count : int) : string =
            match this.WriteAnswer writer count with
            | Ok n -> string n
            | Error answer -> answer

        member _.SoError (e : ConnectionEnd) : string =
            if refused then
                ""
            else
                let error, after = TcpTransfer.takeError e transfer
                transfer <- after

                match error with
                | None -> "0"
                | Some error -> errorName error

        member _.Shutdown (e : ConnectionEnd) (how : TcpShutdownHow) : string =
            if refused then
                ""
            else
                match TcpTransfer.shutdown e how transfer with
                | Ok (answer, _, after) ->
                    transfer <- after

                    match answer with
                    | TcpShutdownAnswer.Shut -> "0"
                    | TcpShutdownAnswer.NotConnected -> "-1 ENOTCONN"
                | Error _ ->
                    refused <- true
                    ""

        member _.Close (e : ConnectionEnd) : string =
            if not refused then
                if TcpTransfer.closeRefused e transfer then
                    refused <- true
                else
                    transfer <- snd (TcpTransfer.close e transfer)

            "0"

        /// A close under `SO_LINGER` {1, 0}.
        member _.Abort (e : ConnectionEnd) : string =
            if not refused then
                if TcpTransfer.abortRefused e transfer then
                    refused <- true
                else
                    transfer <- snd (TcpTransfer.abort e transfer)

            "0"

        member _.Fionread (e : ConnectionEnd) : int =
            if refused then 0 else TcpTransfer.readable e transfer

        /// Writes of 1 MiB until one is refused for want of room: the total.
        member this.Fill (writer : ConnectionEnd) : int64 =
            let rec go (total : int64) =
                match this.WriteAnswer writer (1 <<< 20) with
                | Ok n -> go (total + int64 n)
                | Error _ -> total

            go 0L

        /// Reads of 1 MiB until one answers anything but bytes: the total, and
        /// that last answer.
        member this.Drain (reader : ConnectionEnd) : int64 * string =
            let rec go (total : int64) =
                match this.Read reader (1 <<< 20) with
                | "" -> total, ""
                | answer when answer.StartsWith ("-", StringComparison.Ordinal) || answer = "0" -> total, answer
                | count -> go (total + int64 count)

            go 0L

        /// What the probe's `rdy` prints for `e`: `poll` (and, on Linux, an
        /// epoll registration, which agrees with it), then `FIONREAD`.
        member _.Rdy (e : ConnectionEnd) : string =
            if refused then
                ""
            else
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

    let private sectionS (flavour : SimulatedUnixFlavour) : Modelled list =
        [
            for start in [ "idle" ; "cunread" ; "cunsent" ] do
                for how in hows do
                    let s = Session flavour
                    let hn = howName how

                    match start with
                    | "cunread" -> s.Write p 1000 |> ignore
                    | "cunsent" -> s.Fill c |> ignore
                    | _ -> ()

                    let sh = s.Shutdown c how
                    s.Emit $"S\t%s{start}\t%s{hn}\tshutdown=%s{sh}"
                    s.Emit $"S\t%s{start}\t%s{hn}\trdy(c)\t%s{s.Rdy c}"
                    s.Emit $"S\t%s{start}\t%s{hn}\trdy(p)\t%s{s.Rdy p}"
                    let r1 = s.Read c 4096
                    let r2 = s.Read c 4096
                    let r3 = s.Read p 4096
                    s.Emit $"S\t%s{start}\t%s{hn}\tc-read=%s{r1},%s{r2} p-read=%s{r3}"
                    let w1 = s.Write c 100
                    let w2 = s.Write c 0
                    let w3 = s.Write p 100
                    s.Emit $"S\t%s{start}\t%s{hn}\tc-write100=%s{w1} c-write0=%s{w2} p-write100=%s{w3}"
                    s.Emit $"S\t%s{start}\t%s{hn}\tafter rdy(c)\t%s{s.Rdy c}"
                    s.Emit $"S\t%s{start}\t%s{hn}\tafter rdy(p)\t%s{s.Rdy p}"
                    let r1 = s.Read c 4096
                    let r2 = s.Read c 4096
                    let r3 = s.Read p 4096
                    let e1 = s.SoError c
                    let e2 = s.SoError p

                    s.Emit
                        $"S\t%s{start}\t%s{hn}\tafter c-read=%s{r1},%s{r2} p-read=%s{r3} soerr(c)=%s{e1} soerr(p)=%s{e2}"

                    if start = "cunsent" then
                        let n, last = s.Drain p
                        s.Emit $"S\t%s{start}\t%s{hn}\tp-drained=%d{n} last=%s{last} rdy(p)\t%s{s.Rdy p}"

                    yield! s.Lines
        ]

    let private sectionT (flavour : SimulatedUnixFlavour) : Modelled list =
        [
            for first in hows do
                for second in hows do
                    let s = Session flavour
                    s.Write p 10 |> ignore
                    let a = s.Shutdown c first
                    let b = s.Shutdown c second

                    s.Emit
                        $"T\tidle\t%s{howName first} then %s{howName second}\t%s{a},%s{b}\trdy(c)\t%s{s.Rdy c}\trdy(p)\t%s{s.Rdy p}"

                    yield! s.Lines
            for peer in [ "peer-fin" ; "peer-reset" ] do
                for how in hows do
                    let s = Session flavour

                    if peer = "peer-reset" then
                        s.Write c 100 |> ignore

                    s.Close p |> ignore
                    let sh = s.Shutdown c how
                    s.Emit $"T\t%s{peer}\t%s{howName how}\t%s{sh}\tsoerr(c) after=%s{s.SoError c}"
                    yield! s.Lines
            for how in hows do
                let s = Session flavour
                s.Shutdown p TcpShutdownHow.Write |> ignore
                let r = s.Shutdown c how
                let rp = s.Rdy p
                let rc = s.Rdy c
                let r1 = s.Read p 4096
                let w1 = s.Write c 100

                s.Emit
                    $"T\tpeer-shut-wr\t%s{howName how}\t%s{r}\trdy(p)\t%s{rp}\trdy(c)\t%s{rc}\tp-read=%s{r1} c-write100=%s{w1}"

                yield! s.Lines
            for how in hows do
                let s = Session flavour
                s.Write c 100 |> ignore
                s.Close p |> ignore
                let e = s.SoError c
                let sh = s.Shutdown c how
                s.Emit $"T\tpeer-reset-taken\t%s{howName how}\t%s{sh} (soerr first %s{e})"
                yield! s.Lines
        ]

    let private sectionP (flavour : SimulatedUnixFlavour) : Modelled list =
        [
            let s = Session flavour
            s.Write p 1000 |> ignore
            let sh = s.Shutdown c TcpShutdownHow.Both
            s.Emit $"P\tunread-rdwr\tshutdown=%s{sh} fionread(c)=%d{s.Fionread c}"
            s.Emit $"P\tunread-rdwr\tbefore close rdy(p)\t%s{s.Rdy p}"
            s.Close c |> ignore
            s.Emit $"P\tunread-rdwr\tafter close rdy(p)\t%s{s.Rdy p}"
            let r1 = s.Read p 4096
            let r2 = s.Read p 4096
            let w1 = s.Write p 100
            let w2 = s.Write p 100
            let e = s.SoError p
            s.Emit $"P\tunread-rdwr\tp-read=%s{r1},%s{r2} p-write100=%s{w1} p-write100=%s{w2} soerr(p)=%s{e}"
            yield! s.Lines

            let s = Session flavour
            s.Write p 1000 |> ignore
            let sh = s.Shutdown c TcpShutdownHow.Read
            s.Emit $"P\tunread-rd\tshutdown=%s{sh} fionread(c)=%d{s.Fionread c}"
            s.Close c |> ignore
            s.Emit $"P\tunread-rd\tafter close rdy(p)\t%s{s.Rdy p}"
            let r1 = s.Read p 4096
            let w1 = s.Write p 100
            let e = s.SoError p
            s.Emit $"P\tunread-rd\tp-read=%s{r1} p-write100=%s{w1} soerr(p)=%s{e}"
            yield! s.Lines

            let s = Session flavour
            let sh = s.Shutdown c TcpShutdownHow.Read
            s.Emit $"P\trd-then-fill\tshutdown=%s{sh}"

            // p writes 65536 at a time until three tries take nothing, or one
            // fails otherwise, capped at 16 MiB.
            let rec fill (total : int64) (dry : int) =
                if total >= (16L <<< 20) || dry >= 3 then
                    total
                else
                    match s.WriteAnswer p 65536 with
                    | Ok n -> fill (total + int64 n) 0
                    | Error "-1 EAGAIN" -> fill total (dry + 1)
                    | Error answer ->
                        // The probe prints this answer without its signals.
                        let answer = answer.Replace ("+SIGPIPE", "")
                        s.Emit $"P\trd-then-fill\twrite %s{answer}"
                        total

            let took = fill 0L 0
            let q = s.Fionread c
            s.Emit $"P\trd-then-fill\tp-took=%d{took} fionread(c)=%d{q} rdy(c)\t%s{s.Rdy c}"
            s.Emit $"P\trd-then-fill\tc-read=%s{s.Read c 4096}"
            yield! s.Lines
        ]

    let private sectionR (flavour : SimulatedUnixFlavour) : Modelled list =
        [
            for how in [ TcpShutdownHow.Read ; TcpShutdownHow.Both ] do
                for sent in [ 0 ; 1 ] do
                    let s = Session flavour
                    let hn = howName how

                    if sent = 1 then
                        s.Write c 100 |> ignore
                        s.Read p 4096 |> ignore

                    let r = s.Shutdown c how
                    s.Emit $"R\t%s{hn}\tc-sent-first=%d{sent}\tshutdown=%s{r}\trdy(c)\t%s{s.Rdy c}"

                    for k in 1..4 do
                        let w = s.Write p 100
                        let rc = s.Rdy c
                        let rp = s.Rdy p
                        s.Emit $"R\t%s{hn}\tc-sent-first=%d{sent}\tp-write#%d{k}=%s{w}\trdy(c)\t%s{rc}\trdy(p)\t%s{rp}"

                    let r1 = s.Read c 4096
                    let r2 = s.Read p 4096
                    let e1 = s.SoError c
                    let e2 = s.SoError p

                    s.Emit
                        $"R\t%s{hn}\tc-sent-first=%d{sent}\tc-read=%s{r1} p-read=%s{r2} soerr(c)=%s{e1} soerr(p)=%s{e2}"

                    yield! s.Lines
            for how in [ TcpShutdownHow.Read ; TcpShutdownHow.Both ] do
                let s = Session flavour
                let hn = howName how
                let n = s.Fill p
                let r = s.Shutdown c how
                let rc = s.Rdy c
                let rp = s.Rdy p
                s.Emit $"R\t%s{hn}\tp-unsent(%d{n})\tshutdown=%s{r}\trdy(c)\t%s{rc}\trdy(p)\t%s{rp}"
                let r1 = s.Read c 4096
                let rc = s.Rdy c
                let rp = s.Rdy p
                s.Emit $"R\t%s{hn}\tp-unsent\tc-read=%s{r1}\trdy(c)\t%s{rc}\trdy(p)\t%s{rp}"
                let r1 = s.Read c 4096
                let w1 = s.Write p 100
                let e1 = s.SoError c
                let e2 = s.SoError p
                s.Emit $"R\t%s{hn}\tp-unsent\tc-read=%s{r1} p-write100=%s{w1} soerr(c)=%s{e1} soerr(p)=%s{e2}"

                if how = TcpShutdownHow.Read then
                    let rc = s.Rdy c
                    let rp = s.Rdy p
                    let e1 = s.SoError c
                    let e2 = s.SoError p

                    s.Emit
                        $"R\t%s{hn}\tp-unsent\t5 s later\trdy(c)\t%s{rc}\trdy(p)\t%s{rp}\tsoerr(c)=%s{e1} soerr(p)=%s{e2}"

                yield! s.Lines
        ]

    let private sectionL (flavour : SimulatedUnixFlavour) : Modelled list =
        let starts =
            [
                "idle"
                "cunread"
                "cunsent"
                "pdata"
                "afterwr"
                "afterfin"
                "cunsent-afterwr"
                "bothfin-cfirst"
                "bothfin-pfirst"
                "cqueued-pfin"
                "pfin-cqueued"
            ]

        [
            for linger in [ true ; false ] do
                for start in starts do
                    let s = Session flavour
                    let lt = if linger then "linger0" else "nolinger"

                    let filled =
                        List.contains start [ "cunsent" ; "cunsent-afterwr" ; "cqueued-pfin" ; "pfin-cqueued" ]

                    if start = "cunread" then
                        s.Write p 1000 |> ignore

                    if start = "pfin-cqueued" || start = "bothfin-pfirst" then
                        s.Shutdown p TcpShutdownHow.Write |> ignore

                    if filled then
                        s.Fill c |> ignore

                    if start = "pdata" then
                        s.Write c 1000 |> ignore

                    if
                        List.contains
                            start
                            [
                                "afterwr"
                                "cunsent-afterwr"
                                "bothfin-cfirst"
                                "bothfin-pfirst"
                                "cqueued-pfin"
                                "pfin-cqueued"
                            ]
                    then
                        s.Shutdown c TcpShutdownHow.Write |> ignore

                    if List.contains start [ "afterfin" ; "bothfin-cfirst" ; "cqueued-pfin" ] then
                        s.Shutdown p TcpShutdownHow.Write |> ignore

                    let closed = if linger then s.Abort c else s.Close c
                    s.Emit $"L\t%s{lt}\t%s{start}\t(linger)\tclose=%s{closed} rdy(p)\t%s{s.Rdy p}"
                    s.EmitElsewhere ()
                    let r1 = s.Read p 4096
                    let r2 = s.Read p 4096
                    let w1 = s.Write p 100
                    let w2 = s.Write p 100
                    let e = s.SoError p
                    let r3 = s.Read p 4096

                    s.Emit
                        $"L\t%s{lt}\t%s{start}\tp-read=%s{r1},%s{r2} p-write100=%s{w1} p-write100=%s{w2} soerr(p)=%s{e} p-read=%s{r3}"

                    if filled || start = "pdata" then
                        let n, last = s.Drain p
                        s.Emit $"L\t%s{lt}\t%s{start}\tp-drained=%d{n} last=%s{last}"

                    s.EmitElsewhere ()
                    yield! s.Lines
        ]

    /// `tcp-shutdown-exchange.c`'s sections, each on fresh pairs: both ends
    /// shut writing (K, U), and closes over unread bytes once one or both
    /// have (Q, V, W, T).
    let private sectionsExchange (section : string) (flavour : SimulatedUnixFlavour) : Modelled list =
        let exchange (s : Session) (cFirst : bool) : unit =
            s.Write p 1 |> ignore

            if cFirst then
                s.Shutdown c TcpShutdownHow.Write |> ignore
                s.Shutdown p TcpShutdownHow.Write |> ignore
            else
                s.Shutdown p TcpShutdownHow.Write |> ignore
                s.Shutdown c TcpShutdownHow.Write |> ignore

        let order (cFirst : bool) = if cFirst then "c-first" else "p-first"

        [
            match section with
            | "K" ->
                for cFirst in [ true ; false ] do
                    for who in [ c ; p ] do
                        for how in hows do
                            let s = Session flavour
                            exchange s cFirst
                            let r = s.Shutdown who how
                            let name = if who = c then "c" else "p"
                            s.Emit $"K\t%s{order cFirst}\t%s{name}\t%s{howName how}\tshutdown=%s{r}"
                            yield! s.Lines
            | "U" ->
                for cFirst in [ true ; false ] do
                    let s = Session flavour
                    exchange s cFirst
                    s.Close c |> ignore
                    let r1 = s.Read p 4096
                    let r2 = s.Read p 4096
                    let e = s.SoError p
                    s.Emit $"U\t%s{order cFirst}\tp-read=%s{r1},%s{r2} soerr(p)=%s{e}"
                    yield! s.Lines
            | "Q" ->
                for soErrorFirst in [ false ; true ] do
                    let s = Session flavour
                    s.Fill c |> ignore
                    s.Shutdown c TcpShutdownHow.Write |> ignore
                    s.Shutdown p TcpShutdownHow.Write |> ignore
                    s.Close p |> ignore

                    if soErrorFirst then
                        let e = s.SoError c
                        let r1 = s.Read c 4096
                        let r2 = s.Read c 4096
                        s.Emit $"Q\tsoerr-first\tsoerr(c)=%s{e} c-read=%s{r1},%s{r2}"
                    else
                        let r1 = s.Read c 4096
                        let r2 = s.Read c 4096
                        let e = s.SoError c
                        s.Emit $"Q\tread-first\tc-read=%s{r1},%s{r2} soerr(c)=%s{e}"

                    yield! s.Lines
            | "V"
            | "W" ->
                let rows =
                    if section = "V" then
                        [
                            // name, p shuts writing first, c fills, c shuts writing
                            "noshut-one", false, false, false
                            "one", true, false, false
                            "one-cfin", true, false, true
                            "full", true, true, false
                            "full-cfin", true, true, true
                        ]
                    else
                        [ "one", false, false, true ; "full", false, true, true ]

                for name, pShuts, full, cShuts in rows do
                    let s = Session flavour

                    if pShuts then
                        s.Shutdown p TcpShutdownHow.Write |> ignore

                    if full then s.Fill c |> ignore else s.Write c 1 |> ignore

                    if cShuts then
                        s.Shutdown c TcpShutdownHow.Write |> ignore

                    s.Close p |> ignore
                    let r1 = s.Read c 4096
                    let r2 = s.Read c 4096
                    let e1 = s.SoError c
                    let w = s.Write c 100
                    let e2 = s.SoError c

                    s.Emit
                        $"%s{section}\t%s{name}\tc-read=%s{r1},%s{r2} soerr(c)=%s{e1} c-write100=%s{w} soerr(c)=%s{e2}"

                    yield! s.Lines
            | "T" ->
                for name, pShuts in [ "V-full-cfin", true ; "W-full", false ] do
                    let s = Session flavour

                    if pShuts then
                        s.Shutdown p TcpShutdownHow.Write |> ignore

                    s.Fill c |> ignore
                    s.Shutdown c TcpShutdownHow.Write |> ignore
                    s.Close p |> ignore
                    let r1 = s.Read c 4096
                    let e1 = s.SoError c
                    // No timer of the kernel's fires in the 5 s: the model
                    // has none to fire.
                    let r2 = s.Read c 4096
                    let e2 = s.SoError c

                    s.Emit
                        $"T\t%s{name}\tat once c-read=%s{r1} soerr(c)=%s{e1}; 5 s later c-read=%s{r2} soerr(c)=%s{e2}"

                    yield! s.Lines
            | "G" ->
                // A FIN's state at the close: none, queued behind a full send
                // buffer, or arrived.
                let fins = [ "none" ; "queued" ; "arrived" ]

                for cFin in fins do
                    for pFin in fins do
                        for cFirst in [ true ; false ] do
                            if cFirst || (cFin <> "none" && pFin <> "none") then
                                let s = Session flavour

                                if pFin = "queued" then
                                    s.Fill p |> ignore
                                else
                                    s.Write p 1 |> ignore

                                if cFin = "queued" then
                                    s.Fill c |> ignore

                                let shutC () =
                                    if cFin <> "none" then
                                        s.Shutdown c TcpShutdownHow.Write |> ignore

                                let shutP () =
                                    if pFin <> "none" then
                                        s.Shutdown p TcpShutdownHow.Write |> ignore

                                if cFirst then
                                    shutC ()
                                    shutP ()
                                else
                                    shutP ()
                                    shutC ()

                                s.Close c |> ignore
                                let e1 = s.SoError p
                                let drained, last = s.Drain p
                                let e2 = s.SoError p
                                let r2 = s.Read p 4096
                                let some = if drained > 0L then "some" else "none"

                                s.Emit
                                    $"G\tc-%s{cFin}\tp-%s{pFin}\t%s{order cFirst}\tsoerr(p)=%s{e1} p-drained=%s{some} last=%s{last}; 2 s later soerr(p)=%s{e2} p-read=%s{r2}"

                                yield! s.Lines
            | other -> failwith $"tcp-shutdown-exchange.c has no section %s{other}"
        ]

    let private disagreementsBy
        (probe : Probe)
        (flavour : SimulatedUnixFlavour)
        (section : string)
        (model : SimulatedUnixFlavour -> Modelled list)
        : string list
        =
        let probe = measuredBy probe flavour section
        let model = model flavour

        if probe.Length <> model.Length then
            [
                $"%A{flavour} %s{section}: the probe printed %d{probe.Length} lines and the model %d{model.Length}"
            ]
        else
            List.zip probe model
            |> List.choose (fun (line, modelled) ->
                match line.Mark, modelled with
                | Mark.Timing, _
                | _, Modelled.Elsewhere -> None
                | _, Modelled.Refused -> Some $"the model refuses where the probe measured %s{line.Text}"
                | Mark.Counts, Modelled.Line text ->
                    let expected = withoutCounts (withoutLinger line.Text)

                    if withoutCounts text = expected then
                        None
                    else
                        Some $"measured %s{expected}\n   model %s{withoutCounts text}"
                | Mark.Exact, Modelled.Line text ->
                    let expected = withoutLinger line.Text

                    if text = expected then
                        None
                    else
                        Some $"measured %s{expected}\n   model %s{text}"
            )

    let private disagreements = disagreementsBy Probe.Shutdown

    let private flavours : SimulatedUnixFlavour list =
        [ SimulatedUnixFlavour.Linux ; SimulatedUnixFlavour.Darwin ]

    [<Test>]
    let ``shutdown answers, and what each end then sees, as each flavour measured (section S)`` () : unit =
        for flavour in flavours do
            disagreements flavour "S" sectionS |> shouldEqual []

    [<Test>]
    let ``shutdown twice, and after the peer's FIN or reset, as each flavour measured (section T)`` () : unit =
        for flavour in flavours do
            disagreements flavour "T" sectionT |> shouldEqual []

    [<Test>]
    let ``bytes arriving after the receive side is shut, as each flavour measured (section R)`` () : unit =
        for flavour in flavours do
            disagreements flavour "R" sectionR |> shouldEqual []

    [<Test>]
    let ``what the peer sees of a close after shutdown, as each flavour measured (section P)`` () : unit =
        for flavour in flavours do
            disagreements flavour "P" sectionP |> shouldEqual []

    [<Test>]
    let ``a close under linger zero resets by the two FINs, as each flavour measured (section L)`` () : unit =
        for flavour in flavours do
            disagreements flavour "L" sectionL |> shouldEqual []

    [<Test>]
    let ``once both ends have shut writing, as each flavour measured (tcp-shutdown-exchange.c)`` () : unit =
        for flavour in flavours do
            for section in [ "K" ; "U" ; "Q" ; "V" ; "W" ; "T" ; "G" ] do
                let probe = measuredBy Probe.Exchange flavour section

                if probe.IsEmpty then
                    failwith $"%A{flavour}: the probe printed no section %s{section}"

                disagreementsBy Probe.Exchange flavour section (sectionsExchange section)
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
                    for section, model in [ "R", sectionR ; "L", sectionL ] do
                        let probe = measured flavour section

                        for line, modelled in List.zip probe (model flavour) do
                            match modelled with
                            | Modelled.Refused -> line.Mark |> shouldEqual Mark.Timing
                            | Modelled.Line _
                            | Modelled.Elsewhere -> ()

                        let refused =
                            model flavour
                            |> List.filter (fun line -> line = Modelled.Refused)
                            |> List.length

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
                    sectionsExchange "G" flavour
                    |> List.filter (fun line -> line = Modelled.Refused)
                    |> List.length

                flavour, refused
        ]
        |> shouldEqual [ SimulatedUnixFlavour.Linux, 0 ; SimulatedUnixFlavour.Darwin, 2 ]

namespace WoofWare.PosixKernel.Test

open System
open System.IO
open System.Reflection
open System.Text.RegularExpressions
open WoofWare.PosixKernel

/// How the probe marked a line it printed.
[<RequireQualifiedAccess>]
type internal ShutdownMark =
    | Exact
    /// Byte counts and durations on the line depend on timing: compare only
    /// its answers and readiness bits.
    | Counts
    /// The line's outcome waits on a timer, or comes from a state the model
    /// refuses: it is not compared at all.
    | Timing

/// One line a probe printed, without its mark.
type internal ShutdownMeasured =
    {
        Text : string
        Mark : ShutdownMark
    }

/// What a replay makes of one line of the probe's.
[<RequireQualifiedAccess>]
type internal ShutdownLine =
    | Line of string
    /// The replay refused a call the line depends on.
    | Refused
    /// The line reports something the replaying layer does not model.
    | Elsewhere

/// How the connected pair a scenario starts from was made: by `connect` and
/// `accept` from sockets bound to port 0, or with the connecting socket, or
/// the listener whose port the accepted socket shares, bound explicitly to a
/// port (`tcp-shutdown.c`'s `pair_locked`).
[<RequireQualifiedAccess>]
type internal ShutdownPairShape =
    | Plain
    | ClientLocked
    | ListenerLocked

/// A connected pair, `c` (`ConnectionEnd.Client`) and `p`
/// (`ConnectionEnd.Server`), both non-blocking, driven call by call as
/// `tcp-shutdown.c` (docs/plans/2026-10-08-tcp-shutdown-linger) drives its
/// pair; each answer rendered as the probe prints it. A layer that cannot
/// make a call fails the test.
type internal IShutdownPair =
    /// Whether a call so far was refused, so that nothing after it is
    /// meaningful.
    abstract Refused : bool
    abstract Read : ConnectionEnd -> int -> string
    /// The count written, or the answer when the write took nothing, with
    /// `+SIGPIPE` where it raised that.
    abstract WriteAnswer : ConnectionEnd -> int -> Result<int, string>
    /// `getsockopt(SO_ERROR)`: `0` or the errno's name.
    abstract SoError : ConnectionEnd -> string
    abstract Shutdown : ConnectionEnd -> TcpShutdownHow -> string
    /// `shutdown(2)` with a raw `how`.
    abstract ShutdownRaw : ConnectionEnd -> int -> string
    abstract Close : ConnectionEnd -> string
    /// Closes the end with `SO_LINGER` set to {1, 0}.
    abstract Abort : ConnectionEnd -> string
    abstract Fionread : ConnectionEnd -> int
    /// What the probe's `rdy` prints.
    abstract Rdy : ConnectionEnd -> string
    /// Writes of 1 MiB until three tries take nothing: the total.
    abstract Fill : ConnectionEnd -> int64
    /// Reads of 1 MiB until one answers anything but bytes: the total, and
    /// that answer.
    abstract Drain : ConnectionEnd -> int64 * string
    /// The probe's pause for what a call set in motion to arrive.
    abstract Settle : unit -> unit
    /// Registers each of the ends edge-triggered, as the probe's
    /// `edge_port` does.
    abstract RegisterEdges : ConnectionEnd list -> unit
    /// What the edge registrations report now, as the probe's `edges`
    /// prints it.
    abstract Edges : unit -> string
    /// The port the end's socket is bound to.
    abstract PortOf : ConnectionEnd -> int
    /// Whether a fresh socket binds the loopback address at `port`:
    /// `bind-ok`, `bind-EADDRINUSE` or the answer.
    abstract BindFree : int -> string
    /// `getpeername(2)`: `ok` or the errno's name.
    abstract PeerName : ConnectionEnd -> string
    /// Whether a fresh socket's bind sees an endpoint a closed socket's end
    /// still holds (`TIME_WAIT`, or a FIN on its way): a real kernel's does.
    abstract BindSeesClosedEnds : bool

/// The lines one scenario of a section prints, against one pair, recording
/// as `IShutdownPair`'s calls answer; once a call has been refused, every line
/// after it is `ShutdownLine.Refused`.
type internal ShutdownRecorder (pair : IShutdownPair) =
    let mutable lines : ShutdownLine list = []

    member _.Lines : ShutdownLine list = List.rev lines

    member _.Emit (text : string) : unit =
        lines <-
            (if pair.Refused then
                 ShutdownLine.Refused
             else
                 ShutdownLine.Line text)
            :: lines

    member _.EmitElsewhere () : unit =
        lines <- ShutdownLine.Elsewhere :: lines

    member _.Read (e : ConnectionEnd) (count : int) : string =
        if pair.Refused then "" else pair.Read e count

    member _.WriteAnswer (e : ConnectionEnd) (count : int) : Result<int, string> =
        if pair.Refused then Error "" else pair.WriteAnswer e count

    member this.Write (e : ConnectionEnd) (count : int) : string =
        match this.WriteAnswer e count with
        | Ok n -> string n
        | Error answer -> answer

    member _.SoError (e : ConnectionEnd) : string =
        if pair.Refused then "" else pair.SoError e

    member _.Shutdown (e : ConnectionEnd) (how : TcpShutdownHow) : string =
        if pair.Refused then "" else pair.Shutdown e how

    member _.ShutdownRaw (e : ConnectionEnd) (how : int) : string =
        if pair.Refused then "" else pair.ShutdownRaw e how

    member _.Close (e : ConnectionEnd) : string =
        if pair.Refused then "" else pair.Close e

    member _.Abort (e : ConnectionEnd) : string =
        if pair.Refused then "" else pair.Abort e

    member _.Fionread (e : ConnectionEnd) : int =
        if pair.Refused then 0 else pair.Fionread e

    member _.Rdy (e : ConnectionEnd) : string = if pair.Refused then "" else pair.Rdy e

    member _.Fill (e : ConnectionEnd) : int64 =
        if pair.Refused then 0L else pair.Fill e

    member _.Drain (e : ConnectionEnd) : int64 * string =
        if pair.Refused then 0L, "" else pair.Drain e

    member _.Settle () : unit = pair.Settle ()

    member _.RegisterEdges (ends : ConnectionEnd list) : unit =
        if not pair.Refused then
            pair.RegisterEdges ends

    member _.Edges () : string =
        if pair.Refused then "" else pair.Edges ()

    member _.PortOf (e : ConnectionEnd) : int = pair.PortOf e

    member _.BindFree (port : int) : string =
        if pair.Refused then "" else pair.BindFree port

    member _.PeerName (e : ConnectionEnd) : string =
        if pair.Refused then "" else pair.PeerName e

    member _.BindSeesClosedEnds : bool = pair.BindSeesClosedEnds

/// The sections of `tcp-shutdown.c` (docs/plans/2026-10-08-tcp-shutdown-linger)
/// and `tcp-shutdown-exchange.c` beside it, as scripts over an
/// `IShutdownPair`, each a list of independent scenarios whose lines, in
/// order, are the lines the probe printed for the section. Read the probe's
/// header for what each section and line reports.
[<RequireQualifiedAccess>]
module internal ShutdownProbe =

    /// The probe whose output a section is replayed from.
    [<RequireQualifiedAccess>]
    type Probe =
        /// `tcp-shutdown.c`.
        | Shutdown
        /// `tcp-shutdown-exchange.c`: once both ends have shut writing.
        | Exchange

    /// One scenario: a fresh pair, driven to completion, giving its lines.
    type Scenario = (ShutdownPairShape -> IShutdownPair) -> ShutdownLine list

    /// Every line of `section` the probe printed under `flavour`.
    let measured (probe : Probe) (flavour : SimulatedUnixFlavour) (section : string) : ShutdownMeasured list =
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
                    Mark = ShutdownMark.Timing
                }
            elif line.EndsWith ("\t~counts", StringComparison.Ordinal) then
                {
                    Text = line.Substring (0, line.Length - 8)
                    Mark = ShutdownMark.Counts
                }
            else
                {
                    Text = line
                    Mark = ShutdownMark.Exact
                }
        )

    let private countsPattern : Regex =
        Regex
            @"(fionread=|fionread\(c\)=|kq-read=|kq-write=|p-drained=|p-took=|p-unsent\(|[cp]-(?:read|write)=(?=-?\d+/))-?\d+"

    /// `text` with the byte counts a `~counts` line may differ in replaced.
    let withoutCounts (text : string) : string = countsPattern.Replace (text, "$1N")

    let private lingerPattern : Regex =
        Regex @"\tlinger set early=.*? at close=[^\t]*\t"

    /// `text` with the answers to setting `SO_LINGER` replaced: they belong
    /// to `setsockopt`.
    let withoutLinger (text : string) : string =
        lingerPattern.Replace (text, "\t(linger)\t")

    let private c : ConnectionEnd = ConnectionEnd.Client
    let private p : ConnectionEnd = ConnectionEnd.Server

    let howName (how : TcpShutdownHow) : string =
        match how with
        | TcpShutdownHow.Read -> "RD"
        | TcpShutdownHow.Write -> "WR"
        | TcpShutdownHow.Both -> "RDWR"

    let hows : TcpShutdownHow list =
        [ TcpShutdownHow.Read ; TcpShutdownHow.Write ; TcpShutdownHow.Both ]

    let private scenario (body : ShutdownRecorder -> unit) : Scenario =
        fun pairOf ->
            let s = ShutdownRecorder (pairOf ShutdownPairShape.Plain)
            body s
            s.Lines

    let sectionS : Scenario list =
        [
            for start in [ "idle" ; "cunread" ; "cunsent" ] do
                for how in hows do
                    scenario (fun s ->
                        let hn = howName how

                        match start with
                        | "cunread" ->
                            s.Write p 1000 |> ignore<string>
                            s.Settle ()
                        | "cunsent" -> s.Fill c |> ignore<int64>
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
                    )
        ]

    let sectionT : Scenario list =
        [
            for first in hows do
                for second in hows do
                    scenario (fun s ->
                        s.Write p 10 |> ignore<string>
                        s.Settle ()
                        let a = s.Shutdown c first
                        let b = s.Shutdown c second

                        s.Emit
                            $"T\tidle\t%s{howName first} then %s{howName second}\t%s{a},%s{b}\trdy(c)\t%s{s.Rdy c}\trdy(p)\t%s{s.Rdy p}"
                    )
            for peer in [ "peer-fin" ; "peer-reset" ] do
                for how in hows do
                    scenario (fun s ->
                        if peer = "peer-reset" then
                            s.Write c 100 |> ignore<string>

                        s.Close p |> ignore<string>
                        let sh = s.Shutdown c how
                        s.Emit $"T\t%s{peer}\t%s{howName how}\t%s{sh}\tsoerr(c) after=%s{s.SoError c}"
                    )
            for how in hows do
                scenario (fun s ->
                    s.Shutdown p TcpShutdownHow.Write |> ignore<string>
                    let r = s.Shutdown c how
                    let rp = s.Rdy p
                    let rc = s.Rdy c
                    let r1 = s.Read p 4096
                    let w1 = s.Write c 100

                    s.Emit
                        $"T\tpeer-shut-wr\t%s{howName how}\t%s{r}\trdy(p)\t%s{rp}\trdy(c)\t%s{rc}\tp-read=%s{r1} c-write100=%s{w1}"
                )
            for how in hows do
                scenario (fun s ->
                    s.Write c 100 |> ignore<string>
                    s.Close p |> ignore<string>
                    let e = s.SoError c
                    let sh = s.Shutdown c how
                    s.Emit $"T\tpeer-reset-taken\t%s{howName how}\t%s{sh} (soerr first %s{e})"
                )
        ]

    let sectionP : Scenario list =
        [
            scenario (fun s ->
                s.Write p 1000 |> ignore<string>
                s.Settle ()
                let sh = s.Shutdown c TcpShutdownHow.Both
                s.Emit $"P\tunread-rdwr\tshutdown=%s{sh} fionread(c)=%d{s.Fionread c}"
                s.Emit $"P\tunread-rdwr\tbefore close rdy(p)\t%s{s.Rdy p}"
                s.Close c |> ignore<string>
                s.Emit $"P\tunread-rdwr\tafter close rdy(p)\t%s{s.Rdy p}"
                let r1 = s.Read p 4096
                let r2 = s.Read p 4096
                let w1 = s.Write p 100
                let w2 = s.Write p 100
                let e = s.SoError p
                s.Emit $"P\tunread-rdwr\tp-read=%s{r1},%s{r2} p-write100=%s{w1} p-write100=%s{w2} soerr(p)=%s{e}"
            )
            scenario (fun s ->
                s.Write p 1000 |> ignore<string>
                s.Settle ()
                let sh = s.Shutdown c TcpShutdownHow.Read
                s.Emit $"P\tunread-rd\tshutdown=%s{sh} fionread(c)=%d{s.Fionread c}"
                s.Close c |> ignore<string>
                s.Emit $"P\tunread-rd\tafter close rdy(p)\t%s{s.Rdy p}"
                let r1 = s.Read p 4096
                let w1 = s.Write p 100
                let e = s.SoError p
                s.Emit $"P\tunread-rd\tp-read=%s{r1} p-write100=%s{w1} soerr(p)=%s{e}"
            )
            scenario (fun s ->
                let sh = s.Shutdown c TcpShutdownHow.Read
                s.Emit $"P\trd-then-fill\tshutdown=%s{sh}"

                // p writes 65536 at a time until three tries take nothing, or
                // one fails otherwise, capped at 16 MiB.
                let rec fill (total : int64) (dry : int) =
                    if total >= (16L <<< 20) || dry >= 3 then
                        total
                    else
                        match s.WriteAnswer p 65536 with
                        | Ok n -> fill (total + int64 n) 0
                        | Error "-1 EAGAIN" ->
                            s.Settle ()
                            fill total (dry + 1)
                        | Error answer ->
                            // The probe prints this answer without its
                            // signals.
                            let answer = answer.Replace ("+SIGPIPE", "")
                            s.Emit $"P\trd-then-fill\twrite %s{answer}"
                            total

                let took = fill 0L 0
                s.Settle ()
                let q = s.Fionread c
                s.Emit $"P\trd-then-fill\tp-took=%d{took} fionread(c)=%d{q} rdy(c)\t%s{s.Rdy c}"
                s.Emit $"P\trd-then-fill\tc-read=%s{s.Read c 4096}"
            )
        ]

    let sectionR : Scenario list =
        [
            for how in [ TcpShutdownHow.Read ; TcpShutdownHow.Both ] do
                for sent in [ 0 ; 1 ] do
                    scenario (fun s ->
                        let hn = howName how

                        if sent = 1 then
                            s.Write c 100 |> ignore<string>
                            s.Read p 4096 |> ignore<string>

                        let r = s.Shutdown c how
                        s.Emit $"R\t%s{hn}\tc-sent-first=%d{sent}\tshutdown=%s{r}\trdy(c)\t%s{s.Rdy c}"

                        for k in 1..4 do
                            let w = s.Write p 100
                            let rc = s.Rdy c
                            let rp = s.Rdy p

                            s.Emit
                                $"R\t%s{hn}\tc-sent-first=%d{sent}\tp-write#%d{k}=%s{w}\trdy(c)\t%s{rc}\trdy(p)\t%s{rp}"

                        let r1 = s.Read c 4096
                        let r2 = s.Read p 4096
                        let e1 = s.SoError c
                        let e2 = s.SoError p

                        s.Emit
                            $"R\t%s{hn}\tc-sent-first=%d{sent}\tc-read=%s{r1} p-read=%s{r2} soerr(c)=%s{e1} soerr(p)=%s{e2}"
                    )
            for how in [ TcpShutdownHow.Read ; TcpShutdownHow.Both ] do
                scenario (fun s ->
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
                        // The probe waits 5 s here; no layer replaying it
                        // keeps a timer that fires in that time.
                        let rc = s.Rdy c
                        let rp = s.Rdy p
                        let e1 = s.SoError c
                        let e2 = s.SoError p

                        s.Emit
                            $"R\t%s{hn}\tp-unsent\t5 s later\trdy(c)\t%s{rc}\trdy(p)\t%s{rp}\tsoerr(c)=%s{e1} soerr(p)=%s{e2}"
                )
        ]

    let sectionL : Scenario list =
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
                    scenario (fun s ->
                        let lt = if linger then "linger0" else "nolinger"

                        let filled =
                            List.contains start [ "cunsent" ; "cunsent-afterwr" ; "cqueued-pfin" ; "pfin-cqueued" ]

                        if start = "cunread" then
                            s.Write p 1000 |> ignore<string>

                        if start = "pfin-cqueued" || start = "bothfin-pfirst" then
                            s.Shutdown p TcpShutdownHow.Write |> ignore<string>

                        if filled then
                            s.Fill c |> ignore<int64>

                        if start = "pdata" then
                            s.Write c 1000 |> ignore<string>

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
                            s.Shutdown c TcpShutdownHow.Write |> ignore<string>

                        if List.contains start [ "afterfin" ; "bothfin-cfirst" ; "cqueued-pfin" ] then
                            s.Shutdown p TcpShutdownHow.Write |> ignore<string>

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
                    )
        ]

    /// Section E: edge-triggered registrations, then `shutdown(c, how)`. The
    /// listener's rows are stage 5's, and so are elsewhere.
    let sectionE : Scenario list =
        [
            for start in [ "idle" ; "cunread" ; "cunsent" ] do
                for how in hows do
                    scenario (fun s ->
                        let hn = howName how

                        match start with
                        | "cunread" ->
                            s.Write p 1000 |> ignore<string>
                            s.Settle ()
                        | "cunsent" -> s.Fill c |> ignore<int64>
                        | _ -> ()

                        s.RegisterEdges [ c ; p ]
                        s.Emit $"E\t%s{start}\t%s{hn}\tdrain\t%s{s.Edges ()}"
                        s.Emit $"E\t%s{start}\t%s{hn}\tagain\t%s{s.Edges ()}"
                        s.Shutdown c how |> ignore<string>
                        s.Emit $"E\t%s{start}\t%s{hn}\tafter\t%s{s.Edges ()}"
                    )
            for start in [ "reset" ; "bothfin" ] do
                for how in hows do
                    scenario (fun s ->
                        let hn = howName how

                        if start = "reset" then
                            s.Write c 100 |> ignore<string>
                            s.Settle ()
                            s.Close p |> ignore<string>
                            s.RegisterEdges [ c ]
                        else
                            s.Shutdown c TcpShutdownHow.Write |> ignore<string>
                            s.Shutdown p TcpShutdownHow.Write |> ignore<string>
                            s.RegisterEdges [ c ; p ]

                        s.Emit $"E\t%s{start}\t%s{hn}\tdrain\t%s{s.Edges ()}"
                        s.Emit $"E\t%s{start}\t%s{hn}\tagain\t%s{s.Edges ()}"
                        let sh = s.Shutdown c how
                        s.Emit $"E\t%s{start}\t%s{hn}\tafter shutdown=%s{sh}\t%s{s.Edges ()}"
                    )
            for _ in hows do
                fun _ -> [ ShutdownLine.Elsewhere ]
        ]

    /// Section F: which FIN went first, and whether each end's endpoint binds,
    /// before and after each socket has closed.
    let sectionF : Scenario list =
        let bindTwice (s : ShutdownRecorder) (port : int) : string * string =
            let first = s.BindFree port
            s.Settle ()
            let second = s.BindFree port
            first, second

        [
            for order in
                [
                    "p-first-close"
                    "p-first-wr"
                    "p-first-queued-close"
                    "p-first-queued-wr"
                    "c-first-close"
                    "c-first-wr"
                    "c-queued"
                ] do
                scenario (fun s ->
                    let cport = s.PortOf c

                    let closersEndpoint () =
                        // The line binds `c`'s endpoint once `c` has gone.
                        let b1, b2 = bindTwice s cport
                        b1, b2

                    let emitClosed (prefix : string) (b1 : string) (b2 : string) (rp : string) =
                        if s.BindSeesClosedEnds then
                            s.Emit
                                $"F\t%s{order}\t%s{prefix}closers-endpoint at once %s{b1}, 30 ms later %s{b2}\trdy(p)\t%s{rp}"
                        else
                            s.EmitElsewhere ()

                    match order with
                    | "p-first-close" ->
                        s.Shutdown p TcpShutdownHow.Write |> ignore<string>
                        s.Close c |> ignore<string>
                    | "p-first-wr" ->
                        s.Shutdown p TcpShutdownHow.Write |> ignore<string>
                        s.Shutdown c TcpShutdownHow.Write |> ignore<string>
                        s.Close c |> ignore<string>
                    | "p-first-queued-close"
                    | "p-first-queued-wr" ->
                        s.Shutdown p TcpShutdownHow.Write |> ignore<string>
                        s.Fill c |> ignore<int64>

                        if order = "p-first-queued-wr" then
                            s.Shutdown c TcpShutdownHow.Write |> ignore<string>

                        s.Close c |> ignore<string>
                        let b1, b2 = closersEndpoint ()
                        emitClosed "before p drains: " b1 b2 (s.Rdy p)
                        let n, last = s.Drain p
                        s.Emit $"F\t%s{order}\tp-drained=%d{n} last=%s{last}"
                    | "c-first-close" ->
                        s.Close c |> ignore<string>
                        s.Shutdown p TcpShutdownHow.Write |> ignore<string>
                    | "c-first-wr" ->
                        s.Shutdown c TcpShutdownHow.Write |> ignore<string>
                        s.Shutdown p TcpShutdownHow.Write |> ignore<string>
                        s.Close c |> ignore<string>
                    | _ ->
                        s.Fill c |> ignore<int64>
                        s.Shutdown c TcpShutdownHow.Write |> ignore<string>
                        s.Shutdown p TcpShutdownHow.Write |> ignore<string>
                        let n, last = s.Drain p
                        s.Emit $"F\t%s{order}\tp-drained=%d{n} last=%s{last}"
                        s.Close c |> ignore<string>

                    let b1, b2 = closersEndpoint ()
                    emitClosed "" b1 b2 (s.Rdy p)
                )
            for name, shape in
                [
                    "open-p-first", ShutdownPairShape.Plain
                    "open-c-first", ShutdownPairShape.Plain
                    "open-p-first-queued", ShutdownPairShape.Plain
                    "open-p-first-c-locked", ShutdownPairShape.ClientLocked
                    "open-c-first-l-locked", ShutdownPairShape.ListenerLocked
                    "open-p-only", ShutdownPairShape.Plain
                    "open-c-rdwr", ShutdownPairShape.Plain
                ] do
                fun pairOf ->
                    let s = ShutdownRecorder (pairOf shape)
                    let cport = s.PortOf c
                    let pport = s.PortOf p

                    match name with
                    | "open-p-first"
                    | "open-p-first-c-locked" ->
                        s.Shutdown p TcpShutdownHow.Write |> ignore<string>
                        s.Shutdown c TcpShutdownHow.Write |> ignore<string>
                    | "open-c-first"
                    | "open-c-first-l-locked" ->
                        s.Shutdown c TcpShutdownHow.Write |> ignore<string>
                        s.Shutdown p TcpShutdownHow.Write |> ignore<string>
                    | "open-p-only" -> s.Shutdown p TcpShutdownHow.Write |> ignore<string>
                    | "open-c-rdwr" -> s.Shutdown c TcpShutdownHow.Both |> ignore<string>
                    | _ ->
                        s.Shutdown p TcpShutdownHow.Write |> ignore<string>
                        s.Fill c |> ignore<int64>
                        s.Shutdown c TcpShutdownHow.Write |> ignore<string>
                        let q1 = s.BindFree cport
                        let qp = s.BindFree pport
                        let g1 = s.PeerName c
                        let g2 = s.PeerName p

                        s.Emit
                            $"F\t%s{name}\tbefore p drains: c-endpoint %s{q1} p-endpoint %s{qp} getpeername(c)=%s{g1} getpeername(p)=%s{g2}"

                        let n, last = s.Drain p
                        s.Emit $"F\t%s{name}\tp-drained=%d{n} last=%s{last}"

                    let b1, b2 = bindTwice s cport
                    let bp = s.BindFree pport
                    let g1 = s.PeerName c
                    let g2 = s.PeerName p

                    s.Emit
                        $"F\t%s{name}\tboth open: c-endpoint at once %s{b1}, 30 ms later %s{b2}; p-endpoint %s{bp}; getpeername(c)=%s{g1} getpeername(p)=%s{g2}"

                    s.Close c |> ignore<string>

                    if s.BindSeesClosedEnds then
                        let b3 = s.BindFree cport
                        let b4 = s.BindFree pport
                        let g3 = s.PeerName p
                        s.Emit $"F\t%s{name}\tc closed: c-endpoint %s{b3} p-endpoint %s{b4} getpeername(p)=%s{g3}"
                    else
                        s.EmitElsewhere ()

                    s.Close p |> ignore<string>

                    if s.BindSeesClosedEnds then
                        let b5 = s.BindFree cport
                        let b6 = s.BindFree pport
                        s.Emit $"F\t%s{name}\tboth closed: c-endpoint %s{b5} p-endpoint %s{b6}"
                    else
                        s.EmitElsewhere ()

                    s.Lines
        ]

    /// The rows of section U on a connected socket: a `how` out of range.
    let sectionUConnected : Scenario list =
        [
            for how in [ 3 ; -1 ] do
                scenario (fun s -> s.Emit $"U\tconnected\thow=%d{how}\t%s{s.ShutdownRaw c how}")
        ]

    /// `tcp-shutdown-exchange.c`'s sections, each on fresh pairs: both ends
    /// shut writing (K, U), and closes over unread bytes once one or both
    /// have (Q, V, W, T, G).
    let sectionExchange (section : string) : Scenario list =
        let exchange (s : ShutdownRecorder) (cFirst : bool) : unit =
            s.Write p 1 |> ignore<string>

            if cFirst then
                s.Shutdown c TcpShutdownHow.Write |> ignore<string>
                s.Shutdown p TcpShutdownHow.Write |> ignore<string>
            else
                s.Shutdown p TcpShutdownHow.Write |> ignore<string>
                s.Shutdown c TcpShutdownHow.Write |> ignore<string>

        let order (cFirst : bool) = if cFirst then "c-first" else "p-first"

        match section with
        | "K" ->
            [
                for cFirst in [ true ; false ] do
                    for who in [ c ; p ] do
                        for how in hows do
                            scenario (fun s ->
                                exchange s cFirst
                                let r = s.Shutdown who how
                                let name = if who = c then "c" else "p"
                                s.Emit $"K\t%s{order cFirst}\t%s{name}\t%s{howName how}\tshutdown=%s{r}"
                            )
            ]
        | "U" ->
            [
                for cFirst in [ true ; false ] do
                    scenario (fun s ->
                        exchange s cFirst
                        s.Close c |> ignore<string>
                        let r1 = s.Read p 4096
                        let r2 = s.Read p 4096
                        let e = s.SoError p
                        s.Emit $"U\t%s{order cFirst}\tp-read=%s{r1},%s{r2} soerr(p)=%s{e}"
                    )
            ]
        | "Q" ->
            [
                for soErrorFirst in [ false ; true ] do
                    scenario (fun s ->
                        s.Fill c |> ignore<int64>
                        s.Shutdown c TcpShutdownHow.Write |> ignore<string>
                        s.Shutdown p TcpShutdownHow.Write |> ignore<string>
                        s.Close p |> ignore<string>

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
                    )
            ]
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

            [
                for name, pShuts, full, cShuts in rows do
                    scenario (fun s ->
                        if pShuts then
                            s.Shutdown p TcpShutdownHow.Write |> ignore<string>

                        if full then
                            s.Fill c |> ignore<int64>
                        else
                            s.Write c 1 |> ignore<string>

                        if cShuts then
                            s.Shutdown c TcpShutdownHow.Write |> ignore<string>

                        s.Close p |> ignore<string>
                        let r1 = s.Read c 4096
                        let r2 = s.Read c 4096
                        let e1 = s.SoError c
                        let w = s.Write c 100
                        let e2 = s.SoError c

                        s.Emit
                            $"%s{section}\t%s{name}\tc-read=%s{r1},%s{r2} soerr(c)=%s{e1} c-write100=%s{w} soerr(c)=%s{e2}"
                    )
            ]
        | "T" ->
            [
                for name, pShuts in [ "V-full-cfin", true ; "W-full", false ] do
                    scenario (fun s ->
                        if pShuts then
                            s.Shutdown p TcpShutdownHow.Write |> ignore<string>

                        s.Fill c |> ignore<int64>
                        s.Shutdown c TcpShutdownHow.Write |> ignore<string>
                        s.Close p |> ignore<string>
                        let r1 = s.Read c 4096
                        let e1 = s.SoError c
                        // No timer of the kernel's fires in the 5 s: the
                        // model has none to fire.
                        let r2 = s.Read c 4096
                        let e2 = s.SoError c

                        s.Emit
                            $"T\t%s{name}\tat once c-read=%s{r1} soerr(c)=%s{e1}; 5 s later c-read=%s{r2} soerr(c)=%s{e2}"
                    )
            ]
        | "G" ->
            // A FIN's state at the close: none, queued behind a full send
            // buffer, or arrived.
            let fins = [ "none" ; "queued" ; "arrived" ]

            [
                for cFin in fins do
                    for pFin in fins do
                        for cFirst in [ true ; false ] do
                            if cFirst || (cFin <> "none" && pFin <> "none") then
                                scenario (fun s ->
                                    if pFin = "queued" then
                                        s.Fill p |> ignore<int64>
                                    else
                                        s.Write p 1 |> ignore<string>

                                    if cFin = "queued" then
                                        s.Fill c |> ignore<int64>

                                    let shutC () =
                                        if cFin <> "none" then
                                            s.Shutdown c TcpShutdownHow.Write |> ignore<string>

                                    let shutP () =
                                        if pFin <> "none" then
                                            s.Shutdown p TcpShutdownHow.Write |> ignore<string>

                                    if cFirst then
                                        shutC ()
                                        shutP ()
                                    else
                                        shutP ()
                                        shutC ()

                                    s.Close c |> ignore<string>
                                    let e1 = s.SoError p
                                    let drained, last = s.Drain p
                                    let e2 = s.SoError p
                                    let r2 = s.Read p 4096
                                    let some = if drained > 0L then "some" else "none"

                                    s.Emit
                                        $"G\tc-%s{cFin}\tp-%s{pFin}\t%s{order cFirst}\tsoerr(p)=%s{e1} p-drained=%s{some} last=%s{last}; 2 s later soerr(p)=%s{e2} p-read=%s{r2}"
                                )
            ]
        | other -> failwith $"tcp-shutdown-exchange.c has no section %s{other}"

    /// Every way `replayed`, the lines a replay made of `section` of `probe`
    /// under `flavour`, disagrees with what the probe printed, each normalised
    /// by `normalise` first: skipping `~timing` lines, comparing a `~counts`
    /// line without its counts, and the answers to setting `SO_LINGER` never.
    let disagreements
        (probe : Probe)
        (flavour : SimulatedUnixFlavour)
        (section : string)
        (normalise : string -> string)
        (replayed : ShutdownLine list)
        : string list
        =
        let measured = measured probe flavour section

        if measured.IsEmpty then
            [ $"%A{flavour}: the probe printed no section %s{section}" ]
        elif measured.Length <> replayed.Length then
            [
                $"%A{flavour} %s{section}: the probe printed %d{measured.Length} lines and the replay %d{replayed.Length}"
            ]
        else
            List.zip measured replayed
            |> List.choose (fun (line, modelled) ->
                match line.Mark, modelled with
                | ShutdownMark.Timing, _
                | _, ShutdownLine.Elsewhere -> None
                | _, ShutdownLine.Refused -> Some $"the replay refuses where the probe measured %s{line.Text}"
                | ShutdownMark.Counts, ShutdownLine.Line text ->
                    let expected = normalise (withoutCounts (withoutLinger line.Text))
                    let got = normalise (withoutCounts text)

                    if got = expected then
                        None
                    else
                        Some $"measured %s{expected}\n  replay %s{got}"
                | ShutdownMark.Exact, ShutdownLine.Line text ->
                    let expected = normalise (withoutLinger line.Text)
                    let got = normalise text

                    if got = expected then
                        None
                    else
                        Some $"measured %s{expected}\n  replay %s{got}"
            )

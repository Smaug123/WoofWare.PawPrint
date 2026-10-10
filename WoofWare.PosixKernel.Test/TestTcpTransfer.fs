namespace WoofWare.PosixKernel.Test

open System.Collections.Immutable
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// One call on a TCP connection's transfer, by one end.
[<RequireQualifiedAccess>]
type internal TcpTransferOp =
    /// A non-blocking write of `count` bytes.
    | Write of writer : ConnectionEnd * count : int
    | Read of reader : ConnectionEnd * call : TcpReceiveCall * count : int
    | TakeError of ConnectionEnd
    | Close of ConnectionEnd
    /// A close under `SO_LINGER` {1, 0}.
    | Abort of ConnectionEnd
    | Shutdown of shutter : ConnectionEnd * how : TcpShutdownHow
    /// A Linux `tcp_poll` of the end's socket, which may mark its send buffer
    /// out of space.
    | Poll of ConnectionEnd

/// What one call answered, in a form both the library and the reference give.
[<RequireQualifiedAccess>]
type internal TcpTransferSeen =
    | Bytes of byte list
    | EndOfFile
    | WouldBlock
    | ReadFailed of TcpError
    | Wrote of int
    | WriteFailed of TcpError
    | Error of TcpError option
    | Closed
    | ShutdownAnswered of TcpShutdownAnswer
    | Polled
    /// The kernel refuses the call: what a real kernel does next waits on a
    /// timer.
    | Refused

/// A naive statement of the transfer rules, held to the library's `TcpTransfer`.
///
/// Written from the measured tables (sections 2.3 and 2.4 of
/// `docs/plans/2026-10-07-tcp-byte-transfer.md`, and sections 2.1, 2.2 and
/// 2.7 and the abort table of 3.2 of
/// `docs/plans/2026-10-08-tcp-shutdown-linger.md`) rather than from the
/// library: each direction is a pair of byte lists, and each end a handful of
/// flags, where the library keeps chunked queues, a FIN per direction and one
/// state per end.
[<RequireQualifiedAccess>]
module internal TcpTransferReference =

    /// The bytes on their way to one end.
    type Flow =
        {
            /// Still in the sender's send buffer.
            InFlight : byte list
            /// In the receiver's receive buffer.
            Arrived : byte list
            /// Bytes a reset left in the sender's send buffer, which Darwin
            /// goes on counting there and never delivers.
            Stranded : int
            SendCap : int
            RecvCap : int
        }

    /// What one end knows.
    type End =
        {
            Closed : bool
            /// The peer has made its FIN, by `SHUT_WR` or by closing cleanly.
            /// It arrives once the bytes in flight here have.
            FinSent : bool
            /// The peer's FIN has arrived.
            FinArrived : bool
            /// The peer made its FIN after this end's had arrived.
            FinPassive : bool
            /// This end's own `SHUT_RD` was applied.
            ShutRead : bool
            GotReset : bool
            /// The peer's FIN had arrived, and this end had made none, when
            /// the reset came.
            ResetAfterFin : bool
            ErrorWaiting : bool
            /// Linux's `SOCK_NOSPACE` on this end's send buffer.
            Armed : bool
        }

    type Model =
        {
            Linux : bool
            Flows : Map<ConnectionEnd, Flow>
            Ends : Map<ConnectionEnd, End>
        }

    let other (e : ConnectionEnd) : ConnectionEnd =
        if e = ConnectionEnd.Client then
            ConnectionEnd.Server
        else
            ConnectionEnd.Client

    let create (linux : bool) (sendCap : int) (recvCap : int) : Model =
        let flow =
            {
                InFlight = []
                Arrived = []
                Stranded = 0
                SendCap = sendCap
                RecvCap = recvCap
            }

        let fresh =
            {
                Closed = false
                FinSent = false
                FinArrived = false
                FinPassive = false
                ShutRead = false
                GotReset = false
                ResetAfterFin = false
                ErrorWaiting = false
                Armed = false
            }

        {
            Linux = linux
            Flows = Map.ofList [ ConnectionEnd.Client, flow ; ConnectionEnd.Server, flow ]
            Ends = Map.ofList [ ConnectionEnd.Client, fresh ; ConnectionEnd.Server, fresh ]
        }

    let private setFlow (e : ConnectionEnd) (f : Flow) (m : Model) : Model =
        { m with
            Flows = Map.add e f m.Flows
        }

    let private setEnd (e : ConnectionEnd) (x : End) (m : Model) : Model =
        { m with
            Ends = Map.add e x m.Ends
        }

    /// `e` has made its FIN.
    let writeShut (m : Model) (e : ConnectionEnd) : bool = m.Ends.[other e].FinSent

    /// `e` can receive no more: its own `SHUT_RD`, or the peer's FIN.
    let readShut (m : Model) (e : ConnectionEnd) : bool =
        m.Ends.[e].ShutRead || m.Ends.[e].FinArrived

    /// The error a waiting one is, at end `e`.
    let private errorAt (m : Model) (e : ConnectionEnd) : TcpError =
        if m.Linux && m.Ends.[e].ResetAfterFin then
            TcpError.BrokenPipe
        else
            TcpError.ConnectionReset

    let private linuxWritable (f : Flow) : bool =
        let q = List.length f.InFlight
        // The queue is at most two thirds of the buffer, in the kernel's
        // integer arithmetic.
        f.SendCap - q >= q / 2

    /// How much of `count` bytes a write takes when `free` bytes are free.
    let private accepts (m : Model) (count : int) (free : int) : int option =
        if m.Linux then
            (if free = 0 then None else Some (min count free))
        elif count <= free then
            Some count
        elif free >= 2048 then
            Some free
        else
            None

    /// Whether bytes reaching `r` now are answered with a reset: on Darwin
    /// once it cannot receive, and on Linux once it has made its FIN too.
    let private resetsOnArrival (m : Model) (r : ConnectionEnd) : bool =
        let e = m.Ends.[r]

        not e.Closed && not e.GotReset && readShut m r && (not m.Linux || writeShut m r)

    /// Bytes reached `r`, which answered with a reset. Both ends are reset,
    /// but for a sender that has closed already; what is in flight either way
    /// is lost, or on Darwin stays counted against a sender still open. On
    /// Darwin the end that reset sees no error.
    let private resetByArrival (r : ConnectionEnd) (m : Model) : TcpWake list * Model =
        let s = other r
        let senderGone = m.Ends.[s].Closed

        let afterFin (e : ConnectionEnd) =
            m.Ends.[e].FinArrived && not (writeShut m e)

        let lose (f : Flow) =
            { f with
                InFlight = []
                Stranded = (if m.Linux then 0 else f.Stranded + f.InFlight.Length)
            }

        (if senderGone then [] else [ TcpWake.PeerReset s ]) @ [ TcpWake.PeerReset r ],
        m
        |> setFlow
            r
            (if senderGone then
                 { m.Flows.[r] with
                     InFlight = []
                 }
             else
                 lose m.Flows.[r])
        |> setFlow s (lose m.Flows.[s])
        |> setEnd
            s
            (if senderGone then
                 m.Ends.[s]
             else
                 { m.Ends.[s] with
                     GotReset = true
                     ResetAfterFin = afterFin s
                     ErrorWaiting = true
                     Armed = false
                 })
        |> setEnd
            r
            { m.Ends.[r] with
                GotReset = true
                ResetAfterFin = afterFin r
                ErrorWaiting = m.Linux
                Armed = false
            }

    /// Top `r`'s receive buffer up from the send buffer towards it, or reset
    /// if bytes would reach an end that resets on them: how many bytes moved,
    /// the wakes a reset raises, whether the FIN arrived, and the model after.
    let private topUp (r : ConnectionEnd) (m : Model) : int * TcpWake list * bool * Model =
        let f = m.Flows.[r]
        let room = f.RecvCap - List.length f.Arrived
        let n = min room (List.length f.InFlight)

        if n > 0 && resetsOnArrival m r then
            let wakes, m = resetByArrival r m
            0, wakes, false, m
        else
            let f =
                { f with
                    Arrived = f.Arrived @ List.take n f.InFlight
                    InFlight = List.skip n f.InFlight
                }

            let me = m.Ends.[r]
            let finArrives = n > 0 && f.InFlight.IsEmpty && me.FinSent && not me.FinArrived

            let m =
                if finArrives then
                    setEnd
                        r
                        { me with
                            FinArrived = true
                        }
                        m
                else
                    m

            n, [], finArrives, setFlow r f m

    /// `s` makes its FIN, which arrives at once if nothing is in flight ahead
    /// of it.
    let private makeFin (s : ConnectionEnd) (m : Model) : TcpWake list * Model =
        let p = other s
        let peer = m.Ends.[p]
        let arrives = m.Flows.[p].InFlight.IsEmpty

        (if arrives && not peer.Closed then
             [ TcpWake.PeerFinished p ]
         else
             []),
        setEnd
            p
            { peer with
                FinSent = true
                FinArrived = arrives
                FinPassive = m.Ends.[s].FinArrived
            }
            m

    /// A write by `w` of `bytes`, of which it takes what the rules allow.
    let write (w : ConnectionEnd) (bytes : byte list) (m : Model) : TcpTransferSeen * TcpWake list * Model =
        let me = m.Ends.[w]
        let p = other w

        if me.GotReset then
            if m.Linux && me.ErrorWaiting then
                TcpTransferSeen.WriteFailed (errorAt m w),
                [],
                setEnd
                    w
                    { me with
                        ErrorWaiting = false
                    }
                    m
            else
                TcpTransferSeen.WriteFailed TcpError.BrokenPipe, [], m
        elif writeShut m w then
            TcpTransferSeen.WriteFailed TcpError.BrokenPipe, [], m
        elif bytes.IsEmpty then
            TcpTransferSeen.Wrote 0, [], m
        else
            let toPeer = m.Flows.[p]
            let peerGone = m.Ends.[p].Closed

            // Nothing drains the send buffer towards a peer that has gone, or
            // one that answers an arrival with a reset.
            let free =
                if peerGone || resetsOnArrival m p then
                    toPeer.SendCap - List.length toPeer.InFlight - toPeer.Stranded
                else
                    toPeer.RecvCap - List.length toPeer.Arrived + toPeer.SendCap
                    - List.length toPeer.InFlight
                    - toPeer.Stranded

            match accepts m bytes.Length free with
            | None ->
                TcpTransferSeen.WouldBlock,
                [],
                (if m.Linux then
                     setEnd
                         w
                         { me with
                             Armed = true
                         }
                         m
                 else
                     m)
            | Some n ->
                let taken = List.take n bytes

                // Linux marks the socket out of space when it takes only part.
                let m =
                    if m.Linux && n < bytes.Length then
                        setEnd
                            w
                            { me with
                                Armed = true
                            }
                            m
                    else
                        m

                if peerGone then
                    let toPeer =
                        if m.Linux then
                            toPeer
                        else
                            { toPeer with
                                Stranded = toPeer.Stranded + n
                            }

                    TcpTransferSeen.Wrote n,
                    [ TcpWake.PeerReset w ],
                    m
                    |> setFlow p toPeer
                    // The closed peer's kernel resets, ending what it still had
                    // on its way here.
                    |> setFlow
                        w
                        { m.Flows.[w] with
                            InFlight = []
                        }
                    |> setEnd
                        w
                        { m.Ends.[w] with
                            GotReset = true
                            ResetAfterFin = me.FinArrived
                            ErrorWaiting = true
                            Armed = false
                        }
                else
                    let m =
                        setFlow
                            p
                            { toPeer with
                                InFlight = toPeer.InFlight @ taken
                            }
                            m

                    let moved, resetWakes, _, m = topUp p m

                    let wakes =
                        if moved = 0 then []
                        elif m.Linux then [ TcpWake.DataArrived p ]
                        else [ TcpWake.DataArrived p ; TcpWake.SendSpace w ]

                    TcpTransferSeen.Wrote n, wakes @ resetWakes, m

    /// A read by `r` of up to `count` bytes, by `call`.
    let read
        (r : ConnectionEnd)
        (call : TcpReceiveCall)
        (count : int)
        (m : Model)
        : TcpTransferSeen * TcpWake list * Model
        =
        let me = m.Ends.[r]
        let f = m.Flows.[r]

        if m.Linux && call = TcpReceiveCall.Read && count = 0 then
            TcpTransferSeen.Bytes [], [], m
        elif not f.Arrived.IsEmpty then
            let n = min count f.Arrived.Length
            let got = List.take n f.Arrived

            if call = TcpReceiveCall.Peek then
                TcpTransferSeen.Bytes got, [], m
            else
                let s = other r

                let moved, resetWakes, finArrived, m =
                    topUp
                        r
                        (setFlow
                            r
                            { f with
                                Arrived = List.skip n f.Arrived
                            }
                            m)

                let sender = m.Ends.[s]
                let canStillWrite = not sender.Closed && not sender.GotReset

                let spaceWakes, m =
                    if not canStillWrite then
                        [], m
                    elif not m.Linux then
                        (if moved > 0 then [ TcpWake.SendSpace s ] else []), m
                    elif sender.Armed && linuxWritable m.Flows.[r] then
                        [ TcpWake.SendSpace s ],
                        setEnd
                            s
                            { sender with
                                Armed = false
                            }
                            m
                    else
                        [], m

                let finWake = if finArrived then [ TcpWake.PeerFinished r ] else []

                TcpTransferSeen.Bytes got,
                (if moved > 0 then [ TcpWake.DataArrived r ] else [])
                @ finWake
                @ spaceWakes
                @ resetWakes,
                m
        elif m.Linux then
            if me.GotReset && me.ResetAfterFin then
                TcpTransferSeen.EndOfFile, [], m
            elif me.GotReset && me.ErrorWaiting then
                TcpTransferSeen.ReadFailed (errorAt m r),
                [],
                setEnd
                    r
                    { me with
                        ErrorWaiting = false
                    }
                    m
            elif me.GotReset || readShut m r then
                TcpTransferSeen.EndOfFile, [], m
            else
                TcpTransferSeen.WouldBlock, [], m
        elif me.GotReset && me.ErrorWaiting then
            let m =
                if call = TcpReceiveCall.Peek then
                    m
                else
                    setEnd
                        r
                        { me with
                            ErrorWaiting = false
                        }
                        m

            TcpTransferSeen.ReadFailed TcpError.ConnectionReset, [], m
        elif me.GotReset || readShut m r then
            TcpTransferSeen.EndOfFile, [], m
        elif count = 0 then
            TcpTransferSeen.Bytes [], [], m
        else
            TcpTransferSeen.WouldBlock, [], m

    let takeError (e : ConnectionEnd) (m : Model) : TcpTransferSeen * Model =
        let me = m.Ends.[e]

        if me.ErrorWaiting then
            TcpTransferSeen.Error (Some (errorAt m e)),
            setEnd
                e
                { me with
                    ErrorWaiting = false
                }
                m
        else
            TcpTransferSeen.Error None, m

    /// `shutdown(s, how)`: the 2.1 table. `None` where the kernel refuses.
    let shutdown
        (s : ConnectionEnd)
        (how : TcpShutdownHow)
        (m : Model)
        : (TcpShutdownAnswer * TcpWake list * Model) option
        =
        let rd = how <> TcpShutdownHow.Write
        let wr = how <> TcpShutdownHow.Read
        let me = m.Ends.[s]

        let shutRead (m : Model) : Model =
            setEnd
                s
                { m.Ends.[s] with
                    ShutRead = true
                }
                m

        if me.GotReset then
            Some (TcpShutdownAnswer.NotConnected, (if m.Linux then [ TcpWake.ShutDown (s, how) ] else []), m)
        elif m.Linux then
            // Linux applies whatever is asked, again or not.
            let m = if rd then shutRead m else m
            let finWakes, m = if wr && not (writeShut m s) then makeFin s m else [], m
            Some (TcpShutdownAnswer.Shut, TcpWake.ShutDown (s, how) :: finWakes, m)
        // Darwin: the receive half first, failing if the side is already
        // shut; then the send half, likewise.
        elif rd && readShut m s then
            Some (TcpShutdownAnswer.NotConnected, [], m)
        else
            let waiting = m.Flows.[s].InFlight.Length

            let finGoesAtOnce = wr && not (writeShut m s) && m.Flows.[other s].InFlight.IsEmpty

            let peerStillSends = not me.FinSent

            if
                rd
                && waiting > 0
                && not (how = TcpShutdownHow.Both && finGoesAtOnce && peerStillSends)
            then
                None
            else
                // SHUT_RD flushes what was received.
                let m =
                    if rd then
                        shutRead m
                        |> setFlow
                            s
                            { m.Flows.[s] with
                                Arrived = []
                            }
                    else
                        m

                let writeFails = wr && writeShut m s
                let writeApplied = wr && not writeFails
                let finWakes, m = if writeApplied then makeFin s m else [], m
                let _, resetWakes, _, m = if rd then topUp s m else 0, [], false, m

                let applied =
                    match rd, writeApplied with
                    | true, true -> [ TcpWake.ShutDown (s, TcpShutdownHow.Both) ]
                    | true, false -> [ TcpWake.ShutDown (s, TcpShutdownHow.Read) ]
                    | false, true -> [ TcpWake.ShutDown (s, TcpShutdownHow.Write) ]
                    | false, false -> []

                let answer =
                    if writeFails then
                        TcpShutdownAnswer.NotConnected
                    else
                        TcpShutdownAnswer.Shut

                Some (answer, applied @ finWakes @ resetWakes, m)

    /// Whether Darwin refuses `c`'s close under `SO_LINGER` {1, 0}: `c` made
    /// its FIN before the peer's arrived, the FIN still waits behind its
    /// bytes, and the peer's has arrived.
    let abortRefused (c : ConnectionEnd) (m : Model) : bool =
        let p = other c
        let me = m.Ends.[c]
        let peer = m.Ends.[p]

        not m.Linux
        && not me.GotReset
        && not peer.Closed
        && peer.FinSent
        && not peer.FinArrived
        && not peer.FinPassive
        && me.FinArrived

    /// A Linux poll of `e`, which marks an open end that can still send, but
    /// whose send buffer is more than two thirds full, out of space.
    let poll (e : ConnectionEnd) (m : Model) : Model =
        let me = m.Ends.[e]

        if
            m.Linux
            && not me.Closed
            && not me.GotReset
            && not (writeShut m e)
            && not (linuxWritable m.Flows.[other e])
        then
            setEnd
                e
                { me with
                    Armed = true
                }
                m
        else
            m

    /// `c` closes; with `abortive`, under `SO_LINGER` {1, 0}.
    let close (abortive : bool) (c : ConnectionEnd) (m : Model) : TcpWake list * Model =
        let p = other c
        let me = m.Ends.[c]

        let closedMe =
            { me with
                Closed = true
                ShutRead = false
                GotReset = false
                ResetAfterFin = false
                ErrorWaiting = false
                Armed = false
            }

        let forget (f : Flow) =
            { f with
                Stranded = 0
            }

        if m.Ends.[p].Closed then
            let empty (f : Flow) =
                { f with
                    InFlight = []
                    Arrived = []
                    Stranded = 0
                }

            [],
            m
            |> setEnd c closedMe
            |> setFlow c (empty m.Flows.[c])
            |> setFlow p (empty m.Flows.[p])
        elif me.GotReset then
            // The peer was reset too: there is nothing to tell it.
            [],
            m
            |> setEnd c closedMe
            |> setFlow
                c
                { m.Flows.[c] with
                    Arrived = []
                }
            |> setFlow p (forget m.Flows.[p])
        else
            let toMe = m.Flows.[c]
            let toPeer = m.Flows.[p]
            let unread = not toMe.Arrived.IsEmpty || not toMe.InFlight.IsEmpty
            // Once both FINs have arrived, a close under linger {1, 0} is the
            // ordinary close.
            let exchangeDone = m.Ends.[p].FinArrived && me.FinArrived

            if unread || (abortive && not exchangeDone) then
                [ TcpWake.PeerReset p ],
                m
                |> setEnd c closedMe
                |> setEnd
                    p
                    { m.Ends.[p] with
                        GotReset = true
                        ResetAfterFin = m.Ends.[p].FinArrived && not (writeShut m p)
                        ErrorWaiting = true
                        Armed = false
                    }
                |> setFlow
                    c
                    { toMe with
                        Arrived = []
                        InFlight = []
                        Stranded = (if m.Linux then 0 else toMe.Stranded + toMe.InFlight.Length)
                    }
                |> setFlow
                    p
                    { toPeer with
                        InFlight = []
                        Stranded = 0
                    }
            else
                let m = m |> setEnd c closedMe |> setFlow p (forget toPeer)

                if m.Ends.[p].FinSent then [], m else makeFin c m

/// `TcpTransfer` held to `TcpTransferReference` over random calls.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestTcpTransfer =

    let private ends : ConnectionEnd list =
        [ ConnectionEnd.Client ; ConnectionEnd.Server ]

    /// The library's state in the reference's terms.
    let private view (transfer : TcpTransfer) : TcpTransferReference.Model =
        let flow (e : ConnectionEnd) : TcpTransferReference.Flow =
            let d = TcpTransfer.towards e transfer

            {
                InFlight = List.ofSeq (ByteQueue.peek (ByteQueue.length d.Sending) d.Sending)
                Arrived = List.ofSeq (ByteQueue.peek (ByteQueue.length d.Receiving) d.Receiving)
                Stranded =
                    match transfer.Rules with
                    | TcpTransferRules.Linux _ -> 0
                    | TcpTransferRules.Darwin stranded ->
                        Map.tryFind (TcpTransferReference.other e) stranded |> Option.defaultValue 0
                SendCap = d.SendCapacity
                RecvCap = d.ReceiveCapacity
            }

        let armed (e : ConnectionEnd) : bool =
            match transfer.Rules with
            | TcpTransferRules.Linux set -> Set.contains e set
            | TcpTransferRules.Darwin _ -> false

        let endOf (e : ConnectionEnd) : TcpTransferReference.End =
            let inbound = TcpTransfer.towards e transfer

            let finSent, finArrived, finPassive =
                match inbound.Fin with
                | TcpFin.NotSent -> false, false, false
                | TcpFin.Queued passive -> true, false, passive
                | TcpFin.Arrived passive -> true, true, passive

            let madeOwnFin = TcpTransfer.sendShut e transfer

            let closed, shutRead, reset, waiting =
                match inbound.Receiver with
                | TcpEndState.Open receiveShut -> false, receiveShut, false, false
                | TcpEndState.Reset pending -> false, false, true, pending
                | TcpEndState.Closed -> true, false, false, false

            {
                Closed = closed
                FinSent = finSent
                FinArrived = finArrived
                FinPassive = finPassive
                ShutRead = shutRead
                GotReset = reset
                ResetAfterFin = reset && finArrived && not madeOwnFin
                ErrorWaiting = waiting
                Armed = armed e
            }

        {
            Linux =
                (match transfer.Rules with
                 | TcpTransferRules.Linux _ -> true
                 | TcpTransferRules.Darwin _ -> false)
            Flows = ends |> List.map (fun e -> e, flow e) |> Map.ofList
            Ends = ends |> List.map (fun e -> e, endOf e) |> Map.ofList
        }

    /// A reset end's own `SHUT_RD` is not state the library keeps.
    let private comparable (m : TcpTransferReference.Model) : TcpTransferReference.Model =
        { m with
            Ends =
                m.Ends
                |> Map.map (fun _ e ->
                    if e.GotReset then
                        { e with
                            ShutRead = false
                        }
                    else
                        e
                )
        }

    let private payload (start : int) (count : int) : byte list =
        List.init count (fun i -> byte ((start + i) % 251))

    /// One call on the library: what it answered, the wakes, and the transfer
    /// after.
    let private apply
        (op : TcpTransferOp)
        (start : int)
        (transfer : TcpTransfer)
        : TcpTransferSeen * TcpWake list * TcpTransfer
        =
        match op with
        | TcpTransferOp.Write (writer, count) ->
            match TcpTransfer.admitWrite writer count transfer with
            | TcpWriteAdmission.Answered answer, transfer ->
                let seen =
                    match answer with
                    | TcpWriteAnswer.Wrote n -> TcpTransferSeen.Wrote n
                    | TcpWriteAnswer.WouldBlock -> TcpTransferSeen.WouldBlock
                    | TcpWriteAnswer.Failed error -> TcpTransferSeen.WriteFailed error

                seen, [], transfer
            | TcpWriteAdmission.Take n, transfer ->
                let wakes, transfer =
                    TcpTransfer.write writer (ImmutableArray.CreateRange (payload start n)) transfer

                TcpTransferSeen.Wrote n, wakes, transfer
        | TcpTransferOp.Read (reader, call, count) ->
            let answer, wakes, transfer = TcpTransfer.read reader call count transfer

            let seen =
                match answer with
                | TcpReadAnswer.Bytes bytes -> TcpTransferSeen.Bytes (List.ofSeq bytes)
                | TcpReadAnswer.EndOfFile -> TcpTransferSeen.EndOfFile
                | TcpReadAnswer.WouldBlock -> TcpTransferSeen.WouldBlock
                | TcpReadAnswer.Failed error -> TcpTransferSeen.ReadFailed error

            seen, wakes, transfer
        | TcpTransferOp.TakeError e ->
            let error, transfer = TcpTransfer.takeError e transfer
            TcpTransferSeen.Error error, [], transfer
        | TcpTransferOp.Close e ->
            let wakes, transfer = TcpTransfer.close e transfer
            TcpTransferSeen.Closed, wakes, transfer
        | TcpTransferOp.Abort e ->
            if TcpTransfer.abortRefused e transfer then
                TcpTransferSeen.Refused, [], transfer
            else
                let wakes, transfer = TcpTransfer.abort e transfer
                TcpTransferSeen.Closed, wakes, transfer
        | TcpTransferOp.Shutdown (e, how) ->
            match TcpTransfer.shutdown e how transfer with
            | Ok (answer, wakes, transfer) -> TcpTransferSeen.ShutdownAnswered answer, wakes, transfer
            | Error _ -> TcpTransferSeen.Refused, [], transfer
        | TcpTransferOp.Poll e -> TcpTransferSeen.Polled, [], TcpTransfer.polled e transfer

    /// The same call on the reference.
    let private applyReference
        (op : TcpTransferOp)
        (start : int)
        (model : TcpTransferReference.Model)
        : TcpTransferSeen * TcpWake list * TcpTransferReference.Model
        =
        match op with
        | TcpTransferOp.Write (writer, count) -> TcpTransferReference.write writer (payload start count) model
        | TcpTransferOp.Read (reader, call, count) -> TcpTransferReference.read reader call count model
        | TcpTransferOp.TakeError e ->
            let seen, model = TcpTransferReference.takeError e model
            seen, [], model
        | TcpTransferOp.Close e ->
            let wakes, model = TcpTransferReference.close false e model
            TcpTransferSeen.Closed, wakes, model
        | TcpTransferOp.Abort e ->
            if TcpTransferReference.abortRefused e model then
                TcpTransferSeen.Refused, [], model
            else
                let wakes, model = TcpTransferReference.close true e model
                TcpTransferSeen.Closed, wakes, model
        | TcpTransferOp.Shutdown (e, how) ->
            match TcpTransferReference.shutdown e how model with
            | Some (answer, wakes, model) -> TcpTransferSeen.ShutdownAnswered answer, wakes, model
            | None -> TcpTransferSeen.Refused, [], model
        | TcpTransferOp.Poll e -> TcpTransferSeen.Polled, [], TcpTransferReference.poll e model

    let private actor (op : TcpTransferOp) : ConnectionEnd =
        match op with
        | TcpTransferOp.Write (e, _)
        | TcpTransferOp.Read (e, _, _)
        | TcpTransferOp.TakeError e
        | TcpTransferOp.Close e
        | TcpTransferOp.Abort e
        | TcpTransferOp.Shutdown (e, _)
        | TcpTransferOp.Poll e -> e

    /// A scale for one run: the capacities, and how big a call is.
    type private Scale =
        {
            SendCap : int
            RecvCap : int
            MaxCall : int
        }

    let private scaleGen : Gen<Scale> =
        Gen.oneof
            [
                // Small enough that Linux's two-thirds mark and full buffers come
                // up constantly; every short Darwin write is then refused.
                Gen.map3
                    (fun s r m ->
                        {
                            SendCap = s
                            RecvCap = r
                            MaxCall = m
                        }
                    )
                    (Gen.choose (1, 24))
                    (Gen.choose (1, 24))
                    (Gen.choose (1, 40))
                // Around Darwin's 2048-byte low-water mark.
                Gen.map3
                    (fun s r m ->
                        {
                            SendCap = s
                            RecvCap = r
                            MaxCall = m
                        }
                    )
                    (Gen.choose (1, 6000))
                    (Gen.choose (1, 6000))
                    (Gen.choose (1, 9000))
            ]

    let private howGen : Gen<TcpShutdownHow> =
        Gen.elements [ TcpShutdownHow.Read ; TcpShutdownHow.Write ; TcpShutdownHow.Both ]

    let private opGen (maxCall : int) : Gen<TcpTransferOp> =
        let endGen = Gen.elements ends
        let count = Gen.frequency [ 1, Gen.constant 0 ; 6, Gen.choose (1, maxCall) ]

        Gen.frequency
            [
                6, Gen.map2 (fun e n -> TcpTransferOp.Write (e, n)) endGen count
                6,
                Gen.map3
                    (fun e c n -> TcpTransferOp.Read (e, c, n))
                    endGen
                    (Gen.elements [ TcpReceiveCall.Read ; TcpReceiveCall.Receive ; TcpReceiveCall.Peek ])
                    count
                1, Gen.map TcpTransferOp.TakeError endGen
                1, Gen.map TcpTransferOp.Close endGen
                1, Gen.map TcpTransferOp.Abort endGen
                3, Gen.map2 (fun e how -> TcpTransferOp.Shutdown (e, how)) endGen howGen
                1, Gen.map TcpTransferOp.Poll endGen
            ]

    /// An end fills both buffers towards its peer and shuts writing, so its
    /// FIN waits behind its bytes, and then the peer shuts writing, so the
    /// peer's FIN arrives first: the state in which Darwin refuses a close
    /// under linger zero, which random calls rarely reach.
    let private openingGen (scale : Scale) : Gen<TcpTransferOp list> =
        gen {
            let! e = Gen.elements ends
            let! abortNow = Gen.elements [ true ; false ]

            return
                [
                    TcpTransferOp.Write (e, scale.SendCap + scale.RecvCap)
                    TcpTransferOp.Shutdown (e, TcpShutdownHow.Write)
                    TcpTransferOp.Shutdown (TcpTransferReference.other e, TcpShutdownHow.Write)
                    if abortNow then
                        TcpTransferOp.Abort e
                ]
        }

    let private runGen : Gen<SimulatedUnixFlavour * Scale * TcpTransferOp list> =
        gen {
            let! flavour = Gen.elements [ SimulatedUnixFlavour.Linux ; SimulatedUnixFlavour.Darwin ]
            let! scale = scaleGen
            let! opening = Gen.frequency [ 7, Gen.constant [] ; 1, openingGen scale ]
            let! ops = Gen.listOf (opGen scale.MaxCall) |> Gen.map (List.truncate 60)
            return flavour, scale, opening @ ops
        }

    /// What a call reached, for the coverage floors.
    [<RequireQualifiedAccess>]
    type private Reached =
        | Shutdown of SimulatedUnixFlavour * TcpShutdownHow * TcpShutdownAnswer
        /// Darwin's `SHUT_RDWR` failed in its send half after applying its
        /// receive half.
        | DarwinHalfApplied
        | ShutdownRefused
        /// Bytes reached an end that cannot take them, which reset both ends.
        | ArrivalReset of SimulatedUnixFlavour * by : string
        | AbortReset of SimulatedUnixFlavour
        /// An abort after both FINs had arrived, which is the ordinary close.
        | AbortAfterExchange of SimulatedUnixFlavour
        | AbortRefused
        | WriteAfterShutdown of SimulatedUnixFlavour
        /// A FIN made after the opposite one had arrived.
        | PassiveFin of SimulatedUnixFlavour
        /// A reset that leaves Linux's `EPIPE` pending.
        | ResetInCloseWait
        /// A Linux poll of an end whose send side is shut, while its send
        /// buffer is more than two thirds full.
        | PollOfFullShutSender

    let private reached
        (flavour : SimulatedUnixFlavour)
        (op : TcpTransferOp)
        (seen : TcpTransferSeen)
        (before : TcpTransferReference.Model)
        (after : TcpTransferReference.Model)
        : Reached list
        =
        let resetNow (e : ConnectionEnd) =
            after.Ends.[e].GotReset && not before.Ends.[e].GotReset

        [
            match op, seen with
            | TcpTransferOp.Shutdown (_, how), TcpTransferSeen.ShutdownAnswered answer ->
                Reached.Shutdown (flavour, how, answer)
            | TcpTransferOp.Shutdown _, TcpTransferSeen.Refused -> Reached.ShutdownRefused
            | TcpTransferOp.Abort _, TcpTransferSeen.Refused -> Reached.AbortRefused
            | TcpTransferOp.Abort e, TcpTransferSeen.Closed when
                not before.Ends.[TcpTransferReference.other e].Closed
                && not before.Ends.[e].GotReset
                ->
                if resetNow (TcpTransferReference.other e) then
                    Reached.AbortReset flavour
                elif
                    before.Ends.[e].FinArrived
                    && before.Ends.[TcpTransferReference.other e].FinArrived
                then
                    Reached.AbortAfterExchange flavour
            | TcpTransferOp.Write (w, _), TcpTransferSeen.WriteFailed TcpError.BrokenPipe when
                not before.Ends.[w].GotReset
                ->
                Reached.WriteAfterShutdown flavour
            | TcpTransferOp.Poll e, _ when
                before.Linux
                && not before.Ends.[e].GotReset
                && TcpTransferReference.writeShut before e
                ->
                let f = before.Flows.[TcpTransferReference.other e]
                let queued = f.InFlight.Length

                if f.SendCap - queued < queued / 2 then
                    Reached.PollOfFullShutSender
            | _ -> ()

            match op, seen with
            | TcpTransferOp.Shutdown (_, TcpShutdownHow.Both),
              TcpTransferSeen.ShutdownAnswered TcpShutdownAnswer.NotConnected when
                flavour = SimulatedUnixFlavour.Darwin
                && after.Ends.[actor op].ShutRead
                && not before.Ends.[actor op].ShutRead
                ->
                Reached.DarwinHalfApplied
            | _ -> ()

            if ends |> List.forall resetNow then
                let by =
                    match op with
                    | TcpTransferOp.Write _ -> "write"
                    | TcpTransferOp.Read _ -> "read"
                    | TcpTransferOp.Shutdown _ -> "shutdown"
                    | other -> $"%A{other}"

                Reached.ArrivalReset (flavour, by)

            for e in ends do
                if
                    after.Ends.[e].FinSent
                    && not before.Ends.[e].FinSent
                    && after.Ends.[e].FinPassive
                then
                    Reached.PassiveFin flavour

                if resetNow e && after.Ends.[e].ResetAfterFin && after.Linux then
                    Reached.ResetInCloseWait
        ]

    [<Test>]
    let ``every call answers, wakes and leaves the transfer as the reference does`` () : unit =
        let property
            (cover : Reached -> unit)
            (flavour : SimulatedUnixFlavour, scale : Scale, ops : TcpTransferOp list)
            : unit
            =
            let mutable transfer = TcpTransfer.create flavour scale.SendCap scale.RecvCap

            let mutable model =
                TcpTransferReference.create (flavour = SimulatedUnixFlavour.Linux) scale.SendCap scale.RecvCap

            let mutable start = 0

            for op in ops do
                // A closed end has no socket to make a call on.
                if not model.Ends.[actor op].Closed then
                    let seen, wakes, after = apply op start transfer
                    let expectedSeen, expectedWakes, expected = applyReference op start model

                    (op, seen) |> shouldEqual (op, expectedSeen)
                    (op, List.sort wakes) |> shouldEqual (op, List.sort expectedWakes)
                    (op, comparable (view after)) |> shouldEqual (op, comparable expected)
                    TcpTransfer.violations after |> shouldEqual []

                    for e in ends do
                        if not expected.Ends.[e].Closed then
                            TcpTransfer.readable e after |> shouldEqual expected.Flows.[e].Arrived.Length

                            TcpTransfer.receiveShut e after
                            |> shouldEqual (not expected.Ends.[e].GotReset && TcpTransferReference.readShut expected e)

                        TcpTransfer.sendShut e after
                        |> shouldEqual (TcpTransferReference.writeShut expected e)

                        if not expected.Ends.[e].Closed then
                            // A read asleep has an answer exactly when a read
                            // made now would not answer EAGAIN.
                            let readNow, _, _ = TcpTransferReference.read e TcpReceiveCall.Receive 1 expected

                            TcpTransfer.readAnswers e after
                            |> shouldEqual (readNow <> TcpTransferSeen.WouldBlock)

                            // A write asleep is ended by a reset, or by its own
                            // end's send side being shut.
                            if expected.Ends.[e].GotReset || TcpTransferReference.writeShut expected e then
                                TcpTransfer.writeResumes e 1 after |> shouldEqual true

                    for label in reached flavour op seen model expected do
                        cover label

                    transfer <- after
                    model <- expected
                    start <- start + 1000

        let coverage =
            CoverageSample.check (Config.QuickThrowOnFailure.WithMaxTest 3000) (Arb.fromGen runGen) property

        let flavours = [ SimulatedUnixFlavour.Linux ; SimulatedUnixFlavour.Darwin ]

        let hows = [ TcpShutdownHow.Read ; TcpShutdownHow.Write ; TcpShutdownHow.Both ]

        let floors : (Reached * int) list =
            [
                for flavour in flavours do
                    for how in hows do
                        Reached.Shutdown (flavour, how, TcpShutdownAnswer.Shut), 20
                        Reached.Shutdown (flavour, how, TcpShutdownAnswer.NotConnected), 20

                    Reached.ArrivalReset (flavour, "write"), 20
                    Reached.AbortReset flavour, 20
                    Reached.AbortAfterExchange flavour, 5
                    Reached.WriteAfterShutdown flavour, 20
                    Reached.PassiveFin flavour, 20
                Reached.ArrivalReset (SimulatedUnixFlavour.Linux, "read"), 10
                Reached.ArrivalReset (SimulatedUnixFlavour.Darwin, "shutdown"), 5
                Reached.DarwinHalfApplied, 10
                Reached.ShutdownRefused, 20
                Reached.AbortRefused, 5
                Reached.ResetInCloseWait, 20
                Reached.PollOfFullShutSender, 10
            ]

        let short =
            floors
            |> List.filter (fun (label, floor) -> coverage.Count label < floor)
            |> List.map (fun (label, floor) -> label, coverage.Count label, floor)

        short |> shouldEqual []

    [<Test>]
    let ``a Linux writer that met EAGAIN gets one SendSpace when its send buffer drains to two thirds`` () : unit =
        // Plan section 2.4: no OUT edge to a writer that never met EAGAIN, and
        // exactly one after it did, at two thirds of SO_SNDBUF.
        let transfer = TcpTransfer.create SimulatedUnixFlavour.Linux 30 10

        let bytes (n : int) =
            ImmutableArray.CreateRange (Array.zeroCreate<byte> n)

        let writeAll (n : int) (transfer : TcpTransfer) : TcpWake list * TcpTransfer =
            match TcpTransfer.admitWrite ConnectionEnd.Client n transfer with
            | TcpWriteAdmission.Take taken, transfer when taken = n ->
                TcpTransfer.write ConnectionEnd.Client (bytes n) transfer
            | other, _ -> failwith $"the write of %d{n} was %A{other}"

        let wakes, transfer = writeAll 40 transfer
        wakes |> shouldEqual [ TcpWake.DataArrived ConnectionEnd.Server ]

        let drain (n : int) (transfer : TcpTransfer) : TcpWake list * TcpTransfer =
            let _, wakes, transfer =
                TcpTransfer.read ConnectionEnd.Server TcpReceiveCall.Read n transfer

            wakes, transfer

        // 30 in flight of 30 would be unwritable, but nothing armed the wake.
        let wakes, transfer = drain 10 transfer
        wakes |> shouldEqual [ TcpWake.DataArrived ConnectionEnd.Server ]

        let wakes, transfer = writeAll 10 transfer
        wakes |> shouldEqual []

        let admission, transfer = TcpTransfer.admitWrite ConnectionEnd.Client 1 transfer
        admission |> shouldEqual (TcpWriteAdmission.Answered TcpWriteAnswer.WouldBlock)

        // 30 queued: 30 - 30 < 15. After reading 10, 20 queued: 10 >= 10.
        let wakes, transfer = drain 9 transfer
        wakes |> shouldEqual [ TcpWake.DataArrived ConnectionEnd.Server ]
        let wakes, transfer = drain 1 transfer

        wakes
        |> shouldEqual
            [
                TcpWake.DataArrived ConnectionEnd.Server
                TcpWake.SendSpace ConnectionEnd.Client
            ]

        // Once only.
        let wakes, _ = drain 10 transfer
        wakes |> shouldEqual [ TcpWake.DataArrived ConnectionEnd.Server ]

    [<Test>]
    let ``a Linux write that takes only part of its bytes arms the send-space wake`` () : unit =
        // `tcp_sendmsg` sets SOCK_NOSPACE before it returns a short count.
        let transfer = TcpTransfer.create SimulatedUnixFlavour.Linux 30 10

        let transfer =
            match TcpTransfer.admitWrite ConnectionEnd.Client 41 transfer with
            | TcpWriteAdmission.Take 40, transfer ->
                snd (
                    TcpTransfer.write
                        ConnectionEnd.Client
                        (ImmutableArray.CreateRange (Array.zeroCreate<byte> 40))
                        transfer
                )
            | other, _ -> failwith $"the write of 41 was %A{other}"

        let _, wakes, _ =
            TcpTransfer.read ConnectionEnd.Server TcpReceiveCall.Read 10 transfer

        wakes
        |> shouldEqual
            [
                TcpWake.DataArrived ConnectionEnd.Server
                TcpWake.SendSpace ConnectionEnd.Client
            ]

    [<Test>]
    let ``a FIN still behind bytes in flight has not arrived when the peer resets`` () : unit =
        // The client closes cleanly with ten bytes still in its send buffer,
        // so its FIN waits behind them. The server writes back before reading:
        // the reset reaches an ESTABLISHED end, so Linux pends ECONNRESET and
        // reports it, rather than the EPIPE of a reset in CLOSE_WAIT.
        let transfer = TcpTransfer.create SimulatedUnixFlavour.Linux 10 10

        let transfer =
            match TcpTransfer.admitWrite ConnectionEnd.Client 20 transfer with
            | TcpWriteAdmission.Take 20, transfer ->
                snd (
                    TcpTransfer.write
                        ConnectionEnd.Client
                        (ImmutableArray.CreateRange (Array.zeroCreate<byte> 20))
                        transfer
                )
            | other, _ -> failwith $"the write of 20 was %A{other}"

        let wakes, transfer = TcpTransfer.close ConnectionEnd.Client transfer
        wakes |> shouldEqual []

        (TcpTransfer.towards ConnectionEnd.Server transfer).Fin
        |> shouldEqual (TcpFin.Queued false)

        let transfer =
            match TcpTransfer.admitWrite ConnectionEnd.Server 5 transfer with
            | TcpWriteAdmission.Take 5, transfer ->
                snd (
                    TcpTransfer.write
                        ConnectionEnd.Server
                        (ImmutableArray.CreateRange (Array.zeroCreate<byte> 5))
                        transfer
                )
            | other, _ -> failwith $"the write of 5 was %A{other}"

        (TcpTransfer.towards ConnectionEnd.Server transfer).Receiver
        |> shouldEqual (TcpEndState.Reset true)

        let answer, _, transfer =
            TcpTransfer.read ConnectionEnd.Server TcpReceiveCall.Read 100 transfer

        match answer with
        | TcpReadAnswer.Bytes bytes -> bytes.Length |> shouldEqual 10
        | other -> failwith $"the first read answered %A{other}"

        let answer, _, _ =
            TcpTransfer.read ConnectionEnd.Server TcpReceiveCall.Read 100 transfer

        answer |> shouldEqual (TcpReadAnswer.Failed TcpError.ConnectionReset)

    [<Test>]
    let ``a Linux send buffer near Int32.MaxValue does not overflow the space a write sees`` () : unit =
        let transfer =
            TcpTransfer.create SimulatedUnixFlavour.Linux System.Int32.MaxValue 131072

        TcpTransfer.admitWrite ConnectionEnd.Client 1 transfer
        |> fst
        |> shouldEqual (TcpWriteAdmission.Take 1)

    [<Test>]
    let ``ByteQueue.peek answers what take would and leaves the queue as it was`` () : unit =
        let property (chunks : byte list list, count : int) : unit =
            let queue =
                chunks
                |> List.fold (fun q chunk -> ByteQueue.append (ImmutableArray.CreateRange chunk) q) ByteQueue.empty

            let all = List.concat chunks
            let count = if all.IsEmpty then 0 else abs count % (all.Length + 1)
            let peeked = ByteQueue.peek count queue
            let taken, _ = ByteQueue.take count queue
            List.ofSeq peeked |> shouldEqual (List.take count all)
            List.ofSeq taken |> shouldEqual (List.ofSeq peeked)
            ByteQueue.length queue |> shouldEqual all.Length
            List.ofSeq (ByteQueue.peek all.Length queue) |> shouldEqual all

        Check.One (
            Config.QuickThrowOnFailure.WithMaxTest 500,
            Prop.forAll (ArbMap.defaults |> ArbMap.arbitrary<byte list list * int>) property
        )

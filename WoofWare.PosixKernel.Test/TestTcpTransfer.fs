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
    /// A close that resets whatever is unread.
    | Abort of ConnectionEnd

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

/// A naive statement of the transfer rules, held to the library's `TcpTransfer`.
///
/// Written from the measured tables (sections 2.3 and 2.4 of
/// `docs/plans/2026-10-07-tcp-byte-transfer.md`) rather than from the library:
/// each direction is a pair of byte lists, and each end a handful of flags,
/// where the library keeps chunked queues and one state per end.
[<RequireQualifiedAccess>]
module internal TcpTransferReference =

    /// The bytes on their way to one end.
    type Flow =
        {
            /// Still in the sender's send buffer.
            InFlight : byte list
            /// In the receiver's receive buffer.
            Arrived : byte list
            SendCap : int
            RecvCap : int
        }

    /// What one end knows. `GotFin` and `GotReset` are both set when a reset
    /// followed a FIN.
    type End =
        {
            Closed : bool
            GotFin : bool
            GotReset : bool
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
                SendCap = sendCap
                RecvCap = recvCap
            }

        let fresh =
            {
                Closed = false
                GotFin = false
                GotReset = false
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

    /// The error a waiting one is, at end `e`.
    let private errorAt (m : Model) (e : ConnectionEnd) : TcpError =
        if m.Linux && m.Ends.[e].GotFin then
            TcpError.BrokenPipe
        else
            TcpError.ConnectionReset

    /// Top the receive buffer up from the send buffer: how many bytes moved.
    let private topUp (f : Flow) : int * Flow =
        let room = f.RecvCap - List.length f.Arrived
        let n = min room (List.length f.InFlight)

        n,
        { f with
            Arrived = f.Arrived @ List.take n f.InFlight
            InFlight = List.skip n f.InFlight
        }

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
        elif bytes.IsEmpty then
            TcpTransferSeen.Wrote 0, [], m
        else
            let toPeer = m.Flows.[p]
            let peerGone = m.Ends.[p].Closed

            let free =
                if peerGone then
                    toPeer.SendCap - List.length toPeer.InFlight
                else
                    toPeer.RecvCap - List.length toPeer.Arrived + toPeer.SendCap
                    - List.length toPeer.InFlight

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

                if peerGone then
                    let toPeer =
                        if m.Linux then
                            toPeer
                        else
                            { toPeer with
                                InFlight = toPeer.InFlight @ taken
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
                        { me with
                            GotReset = true
                            ErrorWaiting = true
                            Armed = false
                        }
                else
                    let moved, toPeer =
                        topUp
                            { toPeer with
                                InFlight = toPeer.InFlight @ taken
                            }

                    let wakes =
                        if moved = 0 then []
                        elif m.Linux then [ TcpWake.DataArrived p ]
                        else [ TcpWake.DataArrived p ; TcpWake.SendSpace w ]

                    TcpTransferSeen.Wrote n, wakes, setFlow p toPeer m

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
                let moved, f =
                    topUp
                        { f with
                            Arrived = List.skip n f.Arrived
                        }

                let s = other r
                let sender = m.Ends.[s]
                let m = setFlow r f m
                let canStillWrite = not sender.Closed && not sender.GotReset

                let spaceWakes, m =
                    if not canStillWrite then
                        [], m
                    elif not m.Linux then
                        (if moved > 0 then [ TcpWake.SendSpace s ] else []), m
                    elif sender.Armed && linuxWritable f then
                        [ TcpWake.SendSpace s ],
                        setEnd
                            s
                            { sender with
                                Armed = false
                            }
                            m
                    else
                        [], m

                TcpTransferSeen.Bytes got, (if moved > 0 then [ TcpWake.DataArrived r ] else []) @ spaceWakes, m
        elif m.Linux then
            if me.GotFin then
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
            elif me.GotReset then
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
        elif me.GotReset || me.GotFin then
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

    /// `c` closes; with `abortive`, it resets whatever is unread.
    let close (abortive : bool) (c : ConnectionEnd) (m : Model) : TcpWake list * Model =
        let p = other c
        let me = m.Ends.[c]

        let closedMe =
            { me with
                Closed = true
                Armed = false
            }

        if m.Ends.[p].Closed then
            let empty (f : Flow) =
                { f with
                    InFlight = []
                    Arrived = []
                }

            [],
            m
            |> setEnd c closedMe
            |> setFlow c (empty m.Flows.[c])
            |> setFlow p (empty m.Flows.[p])
        else
            let toMe = m.Flows.[c]
            let toPeer = m.Flows.[p]

            if abortive || not toMe.Arrived.IsEmpty || not toMe.InFlight.IsEmpty then
                [ TcpWake.PeerReset p ],
                m
                |> setEnd c closedMe
                |> setEnd
                    p
                    { m.Ends.[p] with
                        GotReset = true
                        ErrorWaiting = true
                        Armed = false
                    }
                |> setFlow
                    c
                    { toMe with
                        Arrived = []
                        InFlight = (if m.Linux then [] else toMe.InFlight)
                    }
                |> setFlow
                    p
                    { toPeer with
                        InFlight = []
                    }
            else
                [ TcpWake.PeerFinished p ],
                m
                |> setEnd c closedMe
                |> setEnd
                    p
                    { m.Ends.[p] with
                        GotFin = true
                    }

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
                SendCap = d.SendCapacity
                RecvCap = d.ReceiveCapacity
            }

        let armed (e : ConnectionEnd) : bool =
            match transfer.Rules with
            | TcpTransferRules.Linux set -> Set.contains e set
            | TcpTransferRules.Darwin -> false

        let endOf (e : ConnectionEnd) : TcpTransferReference.End =
            let closed, fin, reset, waiting =
                match (TcpTransfer.towards e transfer).Receiver with
                | TcpEndState.Open -> false, false, false, false
                | TcpEndState.FinReceived -> false, true, false, false
                | TcpEndState.Reset (afterFin, pending) -> false, afterFin, true, pending
                | TcpEndState.Closed -> true, false, false, false

            {
                Closed = closed
                GotFin = fin
                GotReset = reset
                ErrorWaiting = waiting
                Armed = armed e
            }

        {
            Linux =
                (match transfer.Rules with
                 | TcpTransferRules.Linux _ -> true
                 | TcpTransferRules.Darwin -> false)
            Flows = ends |> List.map (fun e -> e, flow e) |> Map.ofList
            Ends = ends |> List.map (fun e -> e, endOf e) |> Map.ofList
        }

    /// A closed end's flags past `Closed` are not state the library keeps.
    let private comparable (m : TcpTransferReference.Model) : TcpTransferReference.Model =
        { m with
            Ends =
                m.Ends
                |> Map.map (fun _ e ->
                    if e.Closed then
                        {
                            Closed = true
                            GotFin = false
                            GotReset = false
                            ErrorWaiting = false
                            Armed = false
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
            let wakes, transfer = TcpTransfer.abort e transfer
            TcpTransferSeen.Closed, wakes, transfer

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
            let wakes, model = TcpTransferReference.close true e model
            TcpTransferSeen.Closed, wakes, model

    let private actor (op : TcpTransferOp) : ConnectionEnd =
        match op with
        | TcpTransferOp.Write (e, _)
        | TcpTransferOp.Read (e, _, _)
        | TcpTransferOp.TakeError e
        | TcpTransferOp.Close e
        | TcpTransferOp.Abort e -> e

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
            ]

    let private runGen : Gen<SimulatedUnixFlavour * Scale * TcpTransferOp list> =
        gen {
            let! flavour = Gen.elements [ SimulatedUnixFlavour.Linux ; SimulatedUnixFlavour.Darwin ]
            let! scale = scaleGen
            let! ops = Gen.listOf (opGen scale.MaxCall) |> Gen.map (List.truncate 60)
            return flavour, scale, ops
        }

    [<Test>]
    let ``every call answers, wakes and leaves the transfer as the reference does`` () : unit =
        let property (flavour : SimulatedUnixFlavour, scale : Scale, ops : TcpTransferOp list) : unit =
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

                    transfer <- after
                    model <- expected
                    start <- start + 1000

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 2000, Prop.forAll (Arb.fromGen runGen) property)

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

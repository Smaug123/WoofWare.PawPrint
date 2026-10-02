namespace WoofWare.PosixKernel.Test

open System
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `kevent`'s changelist, replayed against what Darwin answered.
///
/// Each scenario below re-enacts a section of
/// `docs/plans/2026-08-23-posix-kernel-extraction/kevent-register.c` through the
/// syscalls, and every `kevent` it makes is compared with the line the probe printed
/// for the same call on Darwin 27.0.0 arm64 (2026-10-02), embedded: its result, its
/// errno, and every entry it wrote, field by field. The one field compared loosely is
/// an `EVFILT_WRITE` event's `data`, the send buffer's free space, which this kernel
/// does not model; the probe's must be positive.
///
/// The sections the kernel cannot re-enact are left out: anything that writes data
/// onto a connection (P4, P7, P8), `shutdown` (P5), a datagram or IPv6 socket (P9,
/// P12), and `dup2` (G6). Their refusals are tested in `TestKeventRegistration`.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestKeventMeasured =

    /// One scenario's state and the rows it has recorded so far.
    type private Script () =
        let rows = ResizeArray<string list * KeventWorld.Observed> ()
        member val System : UnixSystem<int, string> = KeventWorld.darwin with get, set
        member val Names : Map<int, string> = Map.empty with get, set
        member this.Rows : (string list * KeventWorld.Observed) list = List.ofSeq rows

        member this.Name (fd : int) (name : string) : unit =
            this.Names <- Map.add fd name this.Names

        member this.Fresh (f : UnixSystem<int, string> -> int * UnixSystem<int, string>) : int =
            let fd, system = f this.System
            this.System <- system
            fd

        member this.Step (f : UnixSystem<int, string> -> UnixSystem<int, string>) : unit = this.System <- f this.System

        /// `kevent(kq, changes, nchanges, out, nevents, {0,0})`, recorded under `prefix`.
        member this.KeventN
            (prefix : string list)
            (kq : int)
            (nchanges : int)
            (changes : Kevent list)
            (nevents : int)
            : unit
            =
            match
                UnixKqueue.kevent
                    1
                    kq
                    nchanges
                    changes
                    nevents
                    UserBuffer.Mapped
                    (KeventTimeout.Readable (0L, 0L))
                    this.System
            with
            | Ok (outcome, system) ->
                rows.Add (prefix, KeventWorld.render this.Names outcome)
                this.System <- system
            | Error refusal -> failwith $"%A{prefix}: refused, %s{KeventRefusal.describe refusal}"

        member this.Kevent (prefix : string list) (kq : int) (changes : Kevent list) (nevents : int) : unit =
            this.KeventN prefix kq (List.length changes) changes nevents

        member this.Show (prefix : string list) (kq : int) : unit = this.Kevent prefix kq [] 16

        member this.Register (kq : int) (fd : int) (filter : int16) (flags : uint16) (userData : uint64) : unit =
            this.System <- KeventWorld.register kq fd filter flags userData this.System

    let private read = KeventFilter.Read
    let private write = KeventFilter.Write
    let private addClear = KeventFlags.Add ||| KeventFlags.Clear
    let private receipt = KeventFlags.Receipt
    let private delete = KeventFlags.Delete
    let private change = KeventWorld.change

    let private modes : (string * uint16) list =
        [ "clear", KeventFlags.Add ||| KeventFlags.Clear ; "level", KeventFlags.Add ]

    // ------------------------------------------------------------------
    // P: what each producer activates
    // ------------------------------------------------------------------

    let private sectionP (mode : string) (add : uint16) : (string list * KeventWorld.Observed) list =
        let p (label : string) = [ "P" ; mode ; label ]
        let rows = ResizeArray ()

        // P1: a listener.
        do
            let s = Script ()
            let l = s.Fresh (KeventWorld.listenerAt 5000us)
            s.Name l "L"
            let kq = s.Fresh KeventWorld.kqueue
            s.Register kq l read add 0x11UL
            s.Register kq l write add 0x12UL
            s.Show (p "P1.0 fresh listener") kq
            s.Fresh (KeventWorld.client 5000us) |> ignore<int>
            s.Show (p "P1.1 one connection queued") kq
            s.Show (p "P1.2 again") kq
            s.Fresh (KeventWorld.client 5000us) |> ignore<int>
            s.Show (p "P1.3 a second queued") kq
            s.Show (p "P1.4 again") kq
            s.Fresh (KeventWorld.accept l) |> ignore<int>
            s.Show (p "P1.5 one accepted, one queued") kq
            s.Show (p "P1.6 again") kq
            s.Fresh (KeventWorld.accept l) |> ignore<int>
            s.Show (p "P1.7 both accepted") kq
            s.Fresh (KeventWorld.client 5000us) |> ignore<int>
            s.Fresh (KeventWorld.accept l) |> ignore<int>
            s.Show (p "P1.8 connected and accepted before the poll") kq
            rows.AddRange s.Rows

        // P2: a connect completing.
        do
            let s = Script ()
            let l = s.Fresh (KeventWorld.listenerAt 5000us)
            let socket = s.Fresh (KeventWorld.stream true)
            s.Name socket "S"
            let kq = s.Fresh KeventWorld.kqueue
            s.Register kq socket read add 0x21UL
            s.Register kq socket write add 0x22UL
            s.Show (p "P2.0 idle socket") kq

            s.Step (fun system ->
                match KeventWorld.connect socket 5000us system with
                | ConnectOutcome.Failed UnixError.EINPROGRESS, system -> system
                | other, _ -> failwith $"expected EINPROGRESS, got %A{other}"
            )

            s.Show (p "P2.2 connected") kq
            s.Show (p "P2.3 again") kq
            let a = s.Fresh (KeventWorld.accept l)
            s.Show (p "P2.5 after the peer accepted") kq
            let kq2 = s.Fresh KeventWorld.kqueue
            s.Register kq2 socket read add 0x23UL
            s.Register kq2 socket write add 0x24UL
            s.Show (p "P2.6 ADD on a connected socket, new kqueue") kq2
            s.Name a "A"
            s.Register kq2 a write add 0x25UL
            s.Show (p "P2.7 ADD WRITE on the accepted end") kq2
            rows.AddRange s.Rows

        // P3: a connect refused, and its error read.
        do
            let s = Script ()
            let socket = s.Fresh (KeventWorld.stream true)
            s.Name socket "S"
            let kq = s.Fresh KeventWorld.kqueue
            s.Register kq socket read add 0x31UL
            s.Register kq socket write add 0x32UL
            s.Step (KeventWorld.connect socket 6000us >> snd)
            s.Show (p "P3.1 refused") kq
            s.Show (p "P3.2 again") kq

            s.Step (fun system ->
                match KeventWorld.readSocketError socket system with
                | GetSockOptAnswer.Reported (61, 4u), system -> system
                | other, _ -> failwith $"expected ECONNREFUSED from SO_ERROR, got %A{other}"
            )

            s.Show (p "P3.4 after the SO_ERROR read") kq
            s.Register kq socket read add 0x33UL
            s.Register kq socket write add 0x34UL
            s.Show (p "P3.5 re-ADD after the SO_ERROR read") kq
            rows.AddRange s.Rows

        // P3b: a blocking refusal.
        do
            let s = Script ()
            let socket = s.Fresh (KeventWorld.stream false)
            s.Name socket "S"

            s.Step (fun system ->
                match KeventWorld.connect socket 6000us system with
                | ConnectOutcome.Failed UnixError.ECONNREFUSED, system -> system
                | other, _ -> failwith $"expected ECONNREFUSED, got %A{other}"
            )

            s.Step (fun system ->
                match KeventWorld.readSocketError socket system with
                | GetSockOptAnswer.Reported (0, 4u), system -> system
                | other, _ -> failwith $"expected no pending error, got %A{other}"
            )

            let kq = s.Fresh KeventWorld.kqueue
            s.Register kq socket read add 0x35UL
            s.Register kq socket write add 0x36UL
            s.Show (p "P3b.2 ADD after a blocking refusal") kq
            rows.AddRange s.Rows

        // P6: the peer closing.
        do
            let s = Script ()
            let l = s.Fresh (KeventWorld.listenerAt 5000us)
            let c = s.Fresh (KeventWorld.client 5000us)
            let a = s.Fresh (KeventWorld.accept l)
            s.Step (KeventWorld.close l)
            s.Name c "C"
            s.Name a "A"
            let kq = s.Fresh KeventWorld.kqueue
            s.Register kq a read add 0x51UL
            s.Register kq a write add 0x52UL
            s.Show (p "P6.0 established") kq
            s.Step (KeventWorld.close c)
            s.Show (p "P6.1 peer closed") kq
            s.Show (p "P6.2 again") kq
            s.Register kq a read add 0x53UL
            s.Register kq a write add 0x54UL
            s.Show (p "P6.3 re-ADD both") kq
            rows.AddRange s.Rows

        // P10: READ registered on an idle socket that then listens.
        do
            let s = Script ()
            let socket = s.Fresh (KeventWorld.stream false)
            s.Name socket "S"
            s.Step (KeventWorld.bind socket 5000us)
            let kq = s.Fresh KeventWorld.kqueue
            s.Register kq socket read add 0xa1UL
            s.Show (p "P10.0 bound, idle") kq
            s.Step (KeventWorld.listen socket)
            s.Show (p "P10.1 listening") kq
            s.Fresh (KeventWorld.client 5000us) |> ignore<int>
            s.Show (p "P10.2 connected to") kq
            rows.AddRange s.Rows

        // P11: an accepted connection's peer closing does not touch the listener. (The
        // probe's peer also wrote a byte first, which this kernel cannot.)
        do
            let s = Script ()
            let l = s.Fresh (KeventWorld.listenerAt 5000us)
            s.Name l "L"
            let kq = s.Fresh KeventWorld.kqueue
            s.Register kq l read add 0xb1UL
            let c = s.Fresh (KeventWorld.client 5000us)
            s.Show (p "P11.0 one queued") kq
            let a = s.Fresh (KeventWorld.accept l)
            s.Name a "A"
            s.Name c "C"
            s.Step (KeventWorld.close c)
            s.Show (p "P11.1 the accepted connection's peer wrote and closed") kq
            rows.AddRange s.Rows

        List.ofSeq rows

    // ------------------------------------------------------------------
    // O: delivery order
    // ------------------------------------------------------------------

    let private sectionO (mode : string) (add : uint16) : (string list * KeventWorld.Observed) list =
        let o (label : string) = [ "O" ; mode ; label ]
        let rows = ResizeArray ()

        let listeners (s : Script) (count : int) : int list =
            [
                for i in 1..count do
                    let l = s.Fresh (KeventWorld.listenerAt (uint16 (5000 + i)))
                    s.Name l $"L%d{i}"
                    l
            ]

        for order in
            [
                [ 1 ; 2 ; 3 ]
                [ 1 ; 3 ; 2 ]
                [ 2 ; 1 ; 3 ]
                [ 2 ; 3 ; 1 ]
                [ 3 ; 1 ; 2 ]
                [ 3 ; 2 ; 1 ]
            ] do
            let s = Script ()
            let kq = s.Fresh KeventWorld.kqueue
            let ls = listeners s 3

            ls |> List.iteri (fun i l -> s.Register kq l read add (uint64 (0x101 + i)))

            for i in order do
                s.Fresh (KeventWorld.client (uint16 (5000 + i))) |> ignore<int>

            let connected = order |> List.map (sprintf "L%d") |> String.concat ","
            s.Show (o $"O1 registered L1,L2,L3; connected %s{connected}") kq
            rows.AddRange s.Rows

        for readFirst in [ true ; false ] do
            do
                let s = Script ()
                let socket = s.Fresh (KeventWorld.stream true)
                s.Name socket "S"
                let kq = s.Fresh KeventWorld.kqueue

                if readFirst then
                    s.Register kq socket read add 1UL
                    s.Register kq socket write add 2UL
                else
                    s.Register kq socket write add 2UL
                    s.Register kq socket read add 1UL

                s.Step (KeventWorld.connect socket 6000us >> snd)

                let order = if readFirst then "READ,WRITE" else "WRITE,READ"
                s.Show (o $"O2 refused, registered %s{order}") kq
                rows.AddRange s.Rows

            do
                let s = Script ()
                let l = s.Fresh (KeventWorld.listenerAt 5000us)
                let socket = s.Fresh (KeventWorld.stream true)
                s.Name socket "S"
                s.Name l "L"
                let kq = s.Fresh KeventWorld.kqueue

                if readFirst then
                    s.Register kq socket write add 2UL
                    s.Register kq l read add 1UL
                else
                    s.Register kq l read add 1UL
                    s.Register kq socket write add 2UL

                s.Step (KeventWorld.connect socket 5000us >> snd)
                let order = if readFirst then "S:WRITE,L:READ" else "L:READ,S:WRITE"
                s.Show (o $"O2b one connect, registered %s{order}") kq
                rows.AddRange s.Rows

        do
            let s = Script ()
            let kq = s.Fresh KeventWorld.kqueue

            match listeners s 2 with
            | [ l1 ; l2 ] ->
                s.Register kq l1 read add 0x101UL
                s.Register kq l2 read add 0x102UL
                s.Fresh (KeventWorld.client 5001us) |> ignore<int>
                s.Fresh (KeventWorld.client 5002us) |> ignore<int>
                s.Fresh (KeventWorld.client 5001us) |> ignore<int>
                s.Show (o "O3 connected L1, L2, L1 again") kq
                s.Kevent (o "O4 room for 1") kq [] 1
                s.Fresh (KeventWorld.client 5001us) |> ignore<int>
                s.Kevent (o "O4 room for 1") kq [] 1
                s.Kevent (o "O4 room for 1") kq [] 1
                s.Kevent (o "O4 room for 1") kq [] 1
            | other -> failwith $"expected two listeners, got %A{other}"

            rows.AddRange s.Rows

        do
            let s = Script ()
            let kq = s.Fresh KeventWorld.kqueue

            match listeners s 3 with
            | [ l1 ; l2 ; l3 ] ->
                s.Register kq l1 read add 0x101UL
                s.Register kq l2 read add 0x102UL
                s.Fresh (KeventWorld.client 5002us) |> ignore<int>
                s.Fresh (KeventWorld.client 5003us) |> ignore<int>
                s.Fresh (KeventWorld.client 5001us) |> ignore<int>
                s.Register kq l3 read add 0x103UL
                s.Kevent (o "O5 L1,L2 registered; connected L2, L3, L1; then L3 ADDed") kq [] 2
                s.Show (o "O5 the rest") kq
            | other -> failwith $"expected three listeners, got %A{other}"

            rows.AddRange s.Rows

        for throughLFirst in [ true ; false ] do
            let s = Script ()
            let l = s.Fresh (KeventWorld.listenerAt 5000us)
            let d = s.Fresh (KeventWorld.dup l)
            s.Name l "L"
            s.Name d "D"
            let kq = s.Fresh KeventWorld.kqueue

            if throughLFirst then
                s.Register kq l read add 1UL
                s.Register kq d read add 2UL
            else
                s.Register kq d read add 2UL
                s.Register kq l read add 1UL

            s.Fresh (KeventWorld.client 5000us) |> ignore<int>
            let order = if throughLFirst then "L,D" else "D,L"
            s.Show (o $"O6 one listener through L and its dup D, registered %s{order}; a connection") kq
            rows.AddRange s.Rows

        List.ofSeq rows

    // ------------------------------------------------------------------
    // D, R, G, X, Y
    // ------------------------------------------------------------------

    /// The probe's `scene_new`: a listener with a connection queued, so READ-ready, and
    /// the connected client, so WRITE-ready, and a kqueue.
    let private scene () : Script * int * int * int =
        let s = Script ()
        let l = s.Fresh (KeventWorld.listenerAt 5000us)
        let c = s.Fresh (KeventWorld.client 5000us)
        let kq = s.Fresh KeventWorld.kqueue
        s.Name l "L"
        s.Name c "C"
        s, l, c, kq

    let private sectionD () : (string list * KeventWorld.Observed) list =
        let d (label : string) = [ "D" ; label ]
        let rows = ResizeArray ()

        do
            let s, l, _, kq = scene ()
            s.Register kq l read addClear 1UL
            s.Show (d "D1 ADD|CLEAR udata 1") kq
            s.Register kq l read addClear 2UL
            s.Show (d "D1 re-ADD|CLEAR udata 2 after delivery") kq
            s.Show (d "D1 again") kq
            rows.AddRange s.Rows

        do
            let s, l, _, kq = scene ()
            s.Register kq l read addClear 1UL
            s.Register kq l read addClear 2UL
            s.Show (d "D2 ADD udata 1, re-ADD udata 2 while queued") kq
            s.Show (d "D2 again") kq
            rows.AddRange s.Rows

        do
            let s, l, _, kq = scene ()
            s.Register kq l read addClear 1UL
            s.Show (d "D3 ADD|CLEAR") kq
            s.Register kq l read KeventFlags.Add 2UL
            s.Show (d "D3 re-ADD without CLEAR") kq
            s.Show (d "D3 again") kq
            s.Show (d "D3 again") kq
            rows.AddRange s.Rows

        do
            let s, l, _, kq = scene ()
            s.Register kq l read KeventFlags.Add 1UL
            s.Show (d "D4 ADD without CLEAR") kq
            s.Show (d "D4 again") kq
            s.Register kq l read addClear 2UL
            s.Show (d "D4 re-ADD with CLEAR") kq
            s.Show (d "D4 again") kq
            rows.AddRange s.Rows

        do
            let s = Script ()
            let l = s.Fresh (KeventWorld.listenerAt 5000us)
            s.Name l "L"
            let kq = s.Fresh KeventWorld.kqueue
            s.Register kq l read addClear 1UL
            s.Register kq l read addClear 2UL
            s.Show (d "D5 ADD, re-ADD on nothing ready") kq
            s.Fresh (KeventWorld.client 5000us) |> ignore<int>
            s.Show (d "D5 then a connection") kq
            rows.AddRange s.Rows

        do
            let s, l, c, kq = scene ()
            s.Register kq l read addClear 1UL
            s.Register kq c write addClear 2UL
            s.Register kq l read delete 0UL
            s.Show (d "D6 two queued, the first deleted") kq
            s.Register kq l read addClear 3UL
            s.Show (d "D6 the first ADDed again") kq
            rows.AddRange s.Rows

        do
            let s, l, c, kq = scene ()
            s.Register kq l read addClear 1UL
            s.Register kq c write addClear 2UL
            s.Register kq l read addClear 3UL
            s.Show (d "D7 L, C queued, then L re-ADDed") kq
            rows.AddRange s.Rows

        do
            let s, l, _, kq = scene ()

            s.Kevent
                (d "D8 the same ADD twice in one changelist, room for 8")
                kq
                [
                    change l read (addClear ||| receipt) 1UL
                    change l read (addClear ||| receipt) 2UL
                ]
                8

            s.Show (d "D8 poll") kq

            s.Kevent
                (d "D8 the same DELETE twice in one changelist, room for 8")
                kq
                [
                    change l read (delete ||| receipt) 0UL
                    change l read (delete ||| receipt) 0UL
                ]
                8

            rows.AddRange s.Rows

        List.ofSeq rows

    let private sectionR () : (string list * KeventWorld.Observed) list =
        let r (label : string) = [ "R" ; label ]
        let rows = ResizeArray ()

        for room, label in
            [
                8, "R1 two receipts, room for 8"
                1, "R2 two receipts, room for 1"
                0, "R3 two receipts, room for 0"
            ] do
            let s, l, c, kq = scene ()

            s.Kevent
                (r label)
                kq
                [
                    change l read (addClear ||| receipt) 1UL
                    change c write (addClear ||| receipt) 2UL
                ]
                room

            s.Show (r $"%s{label.Substring (0, 2)} poll") kq
            rows.AddRange s.Rows

        do
            let s, _, c, kq = scene ()
            s.Kevent (r "R4 DELETE of nothing with receipt, room for 8") kq [ change c read (delete ||| receipt) 3UL ] 8
            s.Kevent (r "R4 DELETE of nothing with receipt, room for 0") kq [ change c read (delete ||| receipt) 3UL ] 0
            s.Kevent (r "R4 DELETE of nothing, room for 8") kq [ change c read delete 3UL ] 8
            s.Kevent (r "R4 DELETE of nothing, room for 0") kq [ change c read delete 3UL ] 0
            rows.AddRange s.Rows

        do
            let s, l, c, kq = scene ()

            s.Kevent
                (r "R5 [DELETE of nothing, ADD] with receipts, room for 0")
                kq
                [
                    change c read (delete ||| receipt) 3UL
                    change l read (addClear ||| receipt) 1UL
                ]
                0

            s.Show (r "R5 poll") kq
            rows.AddRange s.Rows

        for room, label in [ 8, "R6" ; 0, "R7" ] do
            let s, l, c, kq = scene ()

            s.Kevent
                (r $"%s{label} [DELETE of nothing, ADD], room for %d{room}")
                kq
                [ change c read delete 3UL ; change l read addClear 1UL ]
                room

            s.Show (r $"%s{label} poll") kq
            rows.AddRange s.Rows

        do
            let s, l, _, kq = scene ()
            s.Kevent (r "R8 ADD of a ready listener, room for 8") kq [ change l read addClear 1UL ] 8
            s.Show (r "R8 poll") kq
            rows.AddRange s.Rows

        do
            let s, l, c, kq = scene ()
            s.Register kq c write addClear 2UL

            s.Kevent
                (r "R9 ADD with receipt beside a queued registration, room for 8")
                kq
                [ change l read (addClear ||| receipt) 1UL ]
                8

            s.Show (r "R9 poll") kq
            rows.AddRange s.Rows

        do
            let s, l, c, kq = scene ()
            s.Register kq c write addClear 2UL

            s.Kevent
                (r "R9b ADD without receipt beside a queued registration, room for 8")
                kq
                [ change l read addClear 1UL ]
                8

            s.Show (r "R9b poll") kq
            rows.AddRange s.Rows

        do
            let s, _, c, kq = scene ()
            let closed = s.Fresh (KeventWorld.dup c)
            s.Step (KeventWorld.close closed)
            s.Name closed "closed"

            s.Kevent
                (r "R10 ADD of a closed descriptor with receipt, room for 8")
                kq
                [ change closed read (addClear ||| receipt) 4UL ]
                8

            s.Kevent
                (r "R10 ADD of a closed descriptor with receipt, room for 0")
                kq
                [ change closed read (addClear ||| receipt) 4UL ]
                0

            s.Kevent (r "R10 ADD of a closed descriptor, room for 8") kq [ change closed read addClear 4UL ] 8
            s.Kevent (r "R10 ADD of a closed descriptor, room for 0") kq [ change closed read addClear 4UL ] 0

            s.Kevent
                (r "R10 DELETE of a closed descriptor with receipt, room for 8")
                kq
                [ change closed read (delete ||| receipt) 4UL ]
                8

            rows.AddRange s.Rows

        do
            let s, l, _, kq = scene ()

            let idents =
                [
                    UInt64.MaxValue, "UINT64_MAX"
                    (1UL <<< 32) + uint64 l, "2^32+L"
                    uint64 Int32.MaxValue, "INT_MAX"
                    uint64 Int32.MaxValue + 1UL, "INT_MAX+1"
                    uint64 (int64 -1), "(uint64)-1"
                    1000000UL, "1000000"
                ]

            for ident, name in idents do
                s.Kevent
                    (r $"R11 ADD of ident %s{name} with receipt, room for 8")
                    kq
                    [
                        { change 0 read (addClear ||| receipt) 5UL with
                            Ident = ident
                        }
                    ]
                    8

            s.Show (r "R11 poll") kq
            rows.AddRange s.Rows

        do
            let s, l, c, kq = scene ()

            s.Kevent
                (r "R12 [ADD, DELETE of nothing, ADD] with receipts, room for 2")
                kq
                [
                    change l read (addClear ||| receipt) 1UL
                    change c read (delete ||| receipt) 3UL
                    change c write (addClear ||| receipt) 2UL
                ]
                2

            s.Show (r "R12 poll") kq
            rows.AddRange s.Rows

        do
            let s, l, c, kq = scene ()
            let closed = s.Fresh (KeventWorld.dup c)
            s.Step (KeventWorld.close closed)
            s.Name closed "closed"

            s.Kevent
                (r "R13 [ADD closed, ADD], room for 1")
                kq
                [ change closed read addClear 4UL ; change l read addClear 1UL ]
                1

            s.Show (r "R13 poll") kq
            rows.AddRange s.Rows

        do
            let s, l, c, kq = scene ()

            s.Kevent
                (r "R14 two ADDs of ready registrations without receipts, room for 1")
                kq
                [ change l read addClear 1UL ; change c write addClear 2UL ]
                1

            s.Show (r "R14 poll") kq
            rows.AddRange s.Rows

        List.ofSeq rows

    let private sectionG () : (string list * KeventWorld.Observed) list =
        let g (label : string) = [ "G" ; label ]
        let rows = ResizeArray ()

        do
            let s = Script ()
            let l = s.Fresh (KeventWorld.listenerAt 5000us)
            let d = s.Fresh (KeventWorld.dup l)
            s.Name l "L"
            s.Name d "D"
            let kq = s.Fresh KeventWorld.kqueue
            s.Register kq l read addClear 1UL
            s.Step (KeventWorld.close l)
            s.Fresh (KeventWorld.client 5000us) |> ignore<int>
            s.Show (g "G1 registered through L, L closed, D lives, then a connection") kq
            s.Register kq d read addClear 2UL
            s.Show (g "G1 then ADD through D") kq
            s.Kevent (g "G1 DELETE through the closed L") kq [ change l read (delete ||| receipt) 0UL ] 8
            rows.AddRange s.Rows

        do
            let s = Script ()
            let l = s.Fresh (KeventWorld.listenerAt 5000us)
            let d = s.Fresh (KeventWorld.dup l)
            s.Name l "L"
            s.Name d "D"
            let kq = s.Fresh KeventWorld.kqueue
            s.Register kq d read addClear 1UL
            s.Step (KeventWorld.close l)
            s.Fresh (KeventWorld.client 5000us) |> ignore<int>
            s.Show (g "G2 registered through D, L closed, then a connection") kq
            rows.AddRange s.Rows

        do
            let s = Script ()
            let l = s.Fresh (KeventWorld.listenerAt 5000us)
            s.Name l "L"
            let kq = s.Fresh KeventWorld.kqueue
            s.Register kq l read addClear 1UL
            s.Step (KeventWorld.close l)
            let l2 = s.Fresh (KeventWorld.listenerAt 5001us)
            l2 |> shouldEqual l
            s.Fresh (KeventWorld.client 5001us) |> ignore<int>
            s.Show (g "G3 registered on L, L closed, a new listener on the same number connected to") kq
            rows.AddRange s.Rows

        do
            let s = Script ()
            let l = s.Fresh (KeventWorld.listenerAt 5000us)
            let d = s.Fresh (KeventWorld.dup l)
            s.Name l "L"
            s.Name d "D"
            s.Fresh (KeventWorld.client 5000us) |> ignore<int>
            let kq = s.Fresh KeventWorld.kqueue
            s.Register kq l read addClear 1UL
            s.Step (KeventWorld.close l)
            s.Show (g "G4 queued through L, then L closed while D lives") kq
            rows.AddRange s.Rows

        do
            let s = Script ()
            let l = s.Fresh (KeventWorld.listenerAt 5000us)
            let d = s.Fresh (KeventWorld.dup l)
            s.Name l "L"
            s.Name d "D"
            let kq1 = s.Fresh KeventWorld.kqueue
            let kq2 = s.Fresh KeventWorld.kqueue
            s.Register kq1 l read addClear 1UL
            s.Register kq2 l read addClear 2UL
            s.Register kq2 d read addClear 3UL
            s.Step (KeventWorld.close l)
            s.Fresh (KeventWorld.client 5000us) |> ignore<int>
            s.Show (g "G5 L in two kqueues and D in the second; L closed; first kqueue") kq1
            s.Show (g "G5 second kqueue") kq2
            rows.AddRange s.Rows

        List.ofSeq rows

    let private sectionX () : (string list * KeventWorld.Observed) list =
        let x (label : string) = [ "X" ; label ]
        let rows = ResizeArray ()

        do
            let s = Script ()
            let kq = s.Fresh KeventWorld.kqueue
            let kqueueTarget = s.Fresh KeventWorld.kqueue

            let file =
                s.Fresh (fun system ->
                    let fd, registry =
                        FileDescriptorRegistry.openFile
                            (InodeNumber 1L)
                            FileAccessMode.ReadWrite
                            system.Process.FileDescriptors

                    fd, KeventWorld.withRegistry registry system
                )

            let directory =
                s.Fresh (fun system ->
                    let fd, registry =
                        FileDescriptorRegistry.openDirectory (InodeNumber 1L) system.Process.FileDescriptors

                    fd, KeventWorld.withRegistry registry system
                )

            let udp =
                s.Fresh (NewSocket.create SocketDomain.Inet SocketKind.Datagram SocketProtocol.Udp)

            // Standard input and output are the read and write ends of the launch pipes.
            let targets =
                [
                    "file", file
                    "pipe-read", 0
                    "pipe-write", 1
                    "kqueue", kqueueTarget
                    "directory", directory
                    "udp", udp
                ]

            for name, fd in targets do
                s.Name fd name

                for filter, filterName in [ read, "READ" ; write, "WRITE" ] do
                    s.Kevent
                        (x $"X3 DELETE of nothing on %s{name} %s{filterName}")
                        kq
                        [ change fd filter (delete ||| receipt) 0UL ]
                        8

            rows.AddRange s.Rows

        do
            let s, l, c, kq = scene ()

            s.Kevent
                (x "X4 [ADD, DELETE of nothing] with receipts, room for 1")
                kq
                [
                    change l read (addClear ||| receipt) 1UL
                    change c read (delete ||| receipt) 3UL
                ]
                1

            s.Show (x "X4 poll") kq
            rows.AddRange s.Rows

        do
            let s, l, c, kq = scene ()

            s.Kevent
                (x "X4 [ADD rcpt, DELETE of nothing, ADD] room for 1")
                kq
                [
                    change l read (addClear ||| receipt) 1UL
                    change c read delete 3UL
                    change c write addClear 2UL
                ]
                1

            s.Show (x "X4 poll") kq
            rows.AddRange s.Rows

        do
            let s, l, c, kq = scene ()

            s.Kevent
                (x "X4 [ADD of ready, DELETE of nothing] no receipts, room for 1")
                kq
                [ change l read addClear 1UL ; change c read delete 3UL ]
                1

            s.Show (x "X4 poll") kq
            rows.AddRange s.Rows

        do
            let s, l, c, kq = scene ()
            s.Register kq l read addClear 1UL

            s.Kevent
                (x "X5 DELETE of nothing beside a queued registration, room for 8")
                kq
                [ change c read delete 3UL ]
                8

            s.Show (x "X5 poll") kq
            rows.AddRange s.Rows

        do
            let s, l, _, kq = scene ()
            s.Register kq l read addClear 1UL

            s.Kevent
                (x "X9 DELETE of 2^32+L with L registered, room for 8")
                kq
                [
                    { change l read (delete ||| receipt) 0UL with
                        Ident = (1UL <<< 32) ||| uint64 l
                    }
                ]
                8

            s.Show (x "X9 poll") kq
            rows.AddRange s.Rows

        do
            let s, l, _, kq = scene ()
            s.Register kq l read addClear 1UL
            s.Show (x "X7 ADD|CLEAR") kq
            s.Kevent (x "X7 re-ADD|CLEAR|RECEIPT") kq [ change l read (addClear ||| receipt) 2UL ] 8
            s.Show (x "X7 poll") kq
            rows.AddRange s.Rows

        List.ofSeq rows

    let private sectionY () : (string list * KeventWorld.Observed) list =
        let y (label : string) = [ "Y" ; label ]
        let rows = ResizeArray ()

        for _ in 1..3 do
            let s = Script ()
            let l = s.Fresh (KeventWorld.listenerAt 5000us)
            let l2 = s.Fresh (KeventWorld.listenerAt 5001us)
            s.Name l "L"
            s.Name l2 "L2"
            let k = s.Fresh KeventWorld.kqueue
            let d = s.Fresh (KeventWorld.dup k)
            s.Register k l read addClear 1UL

            s.Step (fun system ->
                match UnixKqueue.kevent 2 k 0 [] 4 UserBuffer.Mapped KeventTimeout.Null system with
                | Ok (KeventOutcome.WouldBlock _, parked) -> parked
                | other -> failwith $"expected the sleeper to park, got %A{other}"
            )

            s.Step (KeventWorld.close k)

            s.Step (fun system ->
                match UnixKqueue.finishKevent 2 system with
                | Ok (KeventOutcome.Failed UnixError.EBADF, finished) -> finished
                | other -> failwith $"expected the sleeper to fail with EBADF, got %A{other}"
            )

            s.Fresh (KeventWorld.client 5000us) |> ignore<int>
            s.Show (y "Y1 then L ready; a {0,0} wait through D") d

            s.Kevent
                (y "Y1 an ADD with receipt through D, room for 8")
                d
                [ change l2 read (addClear ||| receipt) 2UL ]
                8

            s.Kevent
                (y "Y1 an ADD with receipt through D, room for 0")
                d
                [ change l2 read (addClear ||| receipt) 2UL ]
                0

            s.Kevent (y "Y1 an ADD without receipt through D, room for 0") d [ change l2 read addClear 3UL ] 0
            rows.AddRange s.Rows

        do
            let s, l, _, kq = scene ()

            s.KeventN
                (y "Y3 [ADD with receipt, unreadable], room for 8")
                kq
                2
                [ change l read (addClear ||| receipt) 1UL ]
                8

            s.Show (y "Y3 poll") kq
            rows.AddRange s.Rows

        do
            let s, l, _, kq = scene ()
            s.KeventN (y "Y3 [ADD of a ready listener, unreadable], room for 8") kq 2 [ change l read addClear 1UL ] 8
            s.Show (y "Y3 poll") kq
            rows.AddRange s.Rows

        do
            let s, _, c, kq = scene ()
            s.KeventN (y "Y3 [DELETE of nothing, unreadable], room for 8") kq 2 [ change c read delete 1UL ] 8
            s.KeventN (y "Y3 [DELETE of nothing, unreadable], room for 0") kq 2 [ change c read delete 1UL ] 0
            rows.AddRange s.Rows

        do
            let s, l, _, kq = scene ()
            s.KeventN (y "Y3 [ADD, unreadable], room for 0") kq 2 [ change l read addClear 1UL ] 0
            s.Show (y "Y3 poll") kq
            rows.AddRange s.Rows

        List.ofSeq rows

    /// Compare `rows` with the probe: each label's calls, in order, against the
    /// probe's lines carrying that label, in order. Returns how many were compared.
    let private compare (rows : (string list * KeventWorld.Observed) list) : int =
        let failures =
            rows
            |> List.groupBy fst
            |> List.collect (fun (prefix, rows) ->
                let measured = KeventWorld.observedAll prefix

                if List.length measured <> List.length rows then
                    [
                        $"%A{prefix}: the probe printed %d{List.length measured} lines and the model made %d{List.length rows} calls"
                    ]
                else
                    List.zip measured (List.map snd rows)
                    |> List.indexed
                    |> List.choose (fun (index, (measured, modelled)) ->
                        if KeventWorld.agrees measured modelled then
                            None
                        else
                            Some $"%A{prefix} (call %d{index}): Darwin answered %A{measured}, the model %A{modelled}"
                    )
            )

        match failures with
        | [] -> List.length rows
        | failures -> failwith (String.concat "\n" failures)

    [<Test>]
    let ``each producer activates what it activated on Darwin`` () : unit =
        // P1, P2, P3, P3b, P6, P10 and P11, with EV_CLEAR and without.
        modes
        |> List.collect (fun (mode, add) -> sectionP mode add)
        |> compare
        |> shouldEqual (2 * 29)

    [<Test>]
    let ``events are reported in the order Darwin reported them`` () : unit =
        modes
        |> List.collect (fun (mode, add) -> sectionO mode add)
        |> compare
        |> shouldEqual (2 * 19)

    [<Test>]
    let ``a re-ADD and an EV_DELETE do what they did on Darwin`` () : unit =
        sectionD () |> compare |> shouldEqual 21

    [<Test>]
    let ``receipts and errors are echoed as Darwin echoed them`` () : unit =
        sectionR () |> compare |> shouldEqual 40

    [<Test>]
    let ``closing a descriptor removes its registrations as on Darwin`` () : unit =
        sectionG () |> compare |> shouldEqual 8

    [<Test>]
    let ``deletes, full eventlists and re-ADDs answer as on Darwin`` () : unit =
        sectionX () |> compare |> shouldEqual 25

    [<Test>]
    let ``a drained kqueue and an unreadable change answer as on Darwin`` () : unit =
        sectionY () |> compare |> shouldEqual 20

    /// The probe's ident sweep (X2): an ADD reads the low 32 bits as a descriptor, EBADF
    /// when it is not open, EINVAL when it is and a high bit is set; a DELETE of nothing
    /// is ENOENT whatever the ident.
    [<Test>]
    let ``every ident the probe swept is answered as Darwin answered it`` () : unit =
        let system = KeventWorld.darwin
        let l, system = KeventWorld.listenerAt 5000us system
        let kq, system = KeventWorld.kqueue system
        let closed, system = KeventWorld.dup l system
        let system = KeventWorld.close closed system

        let lows =
            [
                "open", uint64 l
                "closed", uint64 closed
                "0x7fffffff", 0x7fffffffUL
                "0x80000000", 0x80000000UL
                "0xffffffff", 0xffffffffUL
            ]

        let mutable compared = 0

        for high in [ 0UL ; 1UL ; 2UL ; 0x7fffffffUL ; 0x80000000UL ; 0xffffffffUL ] do
            for lowName, low in lows do
                for isDelete in [ false ; true ] do
                    let flags =
                        if isDelete then
                            KeventFlags.Delete ||| KeventFlags.Receipt
                        else
                            KeventFlags.Add ||| KeventFlags.Clear ||| KeventFlags.Receipt

                    let ident = (high <<< 32) ||| low

                    let probed =
                        KeventWorld.probeColumns
                            [
                                "X"
                                $"""X2 %s{if isDelete then "DELETE" else "ADD"} high=0x%x{high} low=%s{lowName}"""
                            ]

                    let modelled =
                        match
                            KeventWorld.apply
                                kq
                                [
                                    { KeventWorld.change 0 KeventFilter.Read flags 5UL with
                                        Ident = ident
                                    }
                                ]
                                2
                                system
                        with
                        | KeventOutcome.Echoed [ entry ], _ -> [ "rv=1" ; "-" ; $"data=%d{entry.Data}" ]
                        | other, _ -> failwith $"ident 0x%x{ident}: expected one echoed entry, got %A{other}"

                    if probed <> modelled then
                        failwith $"ident 0x%x{ident}, %s{lowName}: Darwin answered %A{probed}, the model %A{modelled}"

                    compared <- compared + 1

        compared |> shouldEqual 60

    /// The probe's Y4 and Y5: what a failure that ends the call leaves written in the
    /// eventlist after receipts were copied into it, and an echo into an eventlist that
    /// cannot be written.
    [<Test>]
    let ``a failure after receipts leaves them written, as on Darwin`` () : unit =
        for room in [ 1 ; 2 ] do
            let system = KeventWorld.darwin
            let l, system = KeventWorld.listenerAt 5000us system
            let c, system = KeventWorld.client 5000us system
            let kq, system = KeventWorld.kqueue system
            let closed, system = KeventWorld.dup c system
            let system = KeventWorld.close closed system

            let changes =
                [
                    change l read (addClear ||| receipt) 1UL
                    change c write (addClear ||| receipt) 2UL
                    change closed read addClear 4UL
                ]

            let written, after =
                match KeventWorld.apply kq changes room system with
                | KeventOutcome.FailedAfterEchoing (UnixError.EBADF, written), after -> written, after
                | other, _ -> failwith $"room %d{room}: expected EBADF after echoing, got %A{other}"

            let names = Map.ofList [ l, "L" ; c, "C" ]

            let rendered =
                written
                |> List.mapi (fun i entry ->
                    let filter = if entry.Filter = read then "READ" else "WRITE"

                    $"[%d{i}] ident=%s{names.[int entry.Ident]} filter=%s{filter} flags=0x%x{entry.Flags} data=%d{entry.Data} udata=0x%x{entry.UserData}"
                )

            let untouched = [ List.length written .. 2 ] |> List.map (sprintf "[%d] untouched")

            KeventWorld.probeColumns [ "Y" ; $"Y4 [ADD rcpt, ADD rcpt, ADD closed], room for %d{room}" ]
            |> shouldEqual [ "rv=-1" ; "EBADF" ; String.concat "; " (rendered @ untouched) ]

            // Both ADDs applied, the dropped receipt's too.
            let polled, _ = KeventWorld.apply kq [] 16 after

            KeventWorld.agrees
                (List.item (room - 1) (KeventWorld.observedAll [ "Y" ; "Y4 poll" ]))
                (KeventWorld.render names polled)
            |> shouldEqual true

        // Y5: the receipt's change applied, and the change after it not.
        let system = KeventWorld.darwin
        let l, system = KeventWorld.listenerAt 5000us system
        let c, system = KeventWorld.client 5000us system
        let kq, system = KeventWorld.kqueue system

        match
            UnixKqueue.kevent
                4
                kq
                2
                [ change l read (addClear ||| receipt) 1UL ; change c write addClear 2UL ]
                4
                (UserBuffer.Unmapped 0x1000UL)
                (KeventTimeout.Readable (0L, 0L))
                system
        with
        | Ok (KeventOutcome.Failed UnixError.EFAULT, after) ->
            KeventWorld.probeColumns [ "Y" ; "Y5 [ADD rcpt, ADD] into an unwritable eventlist, room for 4" ]
            |> shouldEqual [ "rv=-1" ; "EFAULT" ]

            let polled, _ = KeventWorld.apply kq [] 16 after

            KeventWorld.agrees
                (List.head (KeventWorld.observedAll [ "Y" ; "Y5 poll" ]))
                (KeventWorld.render (Map.ofList [ l, "L" ; c, "C" ]) polled)
            |> shouldEqual true
        | other -> failwith $"expected EFAULT, got %A{other}"

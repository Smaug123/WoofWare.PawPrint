namespace WoofWare.PosixKernel.Test

open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `KqueueFilter.Timer` is reserved: `kevent` refuses `EVFILT_TIMER`, and a Darwin `poll`
/// registers only read and write filters, so no kqueue and no poll's own kqueue ever holds a
/// timer's registration, or queues one. And the queries a client reads a sleeping poll's
/// kqueue through answer what that kqueue holds.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestKqueueTimerReserved =

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 300

    /// Every `EVFILT_*` number in Darwin 27's `<sys/event.h>`, `EVFILT_TIMER` (-7) among them,
    /// and one past the last.
    let private filterNumbers : int16 list =
        [ -1s ; -2s ; -3s ; -4s ; -5s ; -6s ; -7s ; -8s ; -9s ; -10s ; -12s ; -15s ; -19s ; -20s ]

    /// `EV_ADD`, alone and with `EV_CLEAR` and `EV_RECEIPT`; `EV_DELETE`, alone and with
    /// `EV_RECEIPT`; and `EV_ADD | EV_ONESHOT`, which is refused.
    let private flagChoices : uint16 list =
        [ 0x01us ; 0x21us ; 0x41us ; 0x61us ; 0x02us ; 0x42us ; 0x11us ]

    /// A Darwin system holding a listener, a connected pair, an idle socket, a pipe holding
    /// bytes, and two kqueues; with the descriptors other than the kqueues.
    let private world : Lazy<int list * int * int * UnixSystem<int, string>> =
        lazy
            (let listener, system = KeventWorld.listenerAt 5300us KeventWorld.darwin
             let client, system = KeventWorld.client 5300us system
             let accepted, system = KeventWorld.accept listener system
             let idle, system = KeventWorld.stream true system

             let (readEnd, writeEnd), system =
                 match UnixPipe.tryPipe2 0 UserBuffer.Mapped system with
                 | Ok (Pipe2Answer.Created (readFd, writeFd), system) -> (readFd, writeFd), system
                 | other -> failwith $"pipe did not make a pipe: %A{other}"

             let kq1, system = KeventWorld.kqueue system
             let kq2, system = KeventWorld.kqueue system
             [ listener ; client ; accepted ; idle ; readEnd ; writeEnd ], kq1, kq2, system)

    let private holdsTimer (keys : (int * KqueueFilter) seq) : bool =
        keys |> Seq.exists (fun (_, filter) -> filter = KqueueFilter.Timer)

    /// Every kqueue a descriptor of `system` names.
    let private kqueues (system : UnixSystem<int, string>) : KqueueState list =
        FileDescriptorRegistry.fds (UnixSystemState.fileDescriptors system)
        |> Map.toList
        |> List.choose (fun (fd, _) ->
            match FileDescriptorRegistry.tryFindTarget fd (UnixSystemState.fileDescriptors system) with
            | Some (OpenFileTarget.Kqueue state) -> Some state
            | _ -> None
        )

    let private noTimerAnywhere (system : UnixSystem<int, string>) : unit =
        for state in kqueues system do
            holdsTimer (Map.keys state.Registrations) |> shouldEqual false
            holdsTimer state.Active |> shouldEqual false

        for KeyValue (_, queue) in system.Machine.PollQueues do
            holdsTimer (Map.keys queue.Registrations) |> shouldEqual false
            holdsTimer queue.Active |> shouldEqual false

    [<Test>]
    let ``kevent refuses EVFILT_TIMER, whatever its ident`` () : unit =
        let fds, kq, _, system = world.Force ()

        for ident in [ yield! List.map uint64 fds ; 900UL ; 0x1_0000_0003UL ; System.UInt64.MaxValue ] do
            let timer =
                {
                    Ident = ident
                    Filter = -7s
                    Flags = KeventFlags.Add
                    FilterFlags = 0u
                    Data = 1000L
                    UserData = 0UL
                }

            for change in [ timer ; { timer with Data = 0L } ] do
                match
                    UnixKqueue.tryKevent 4 kq 1 [ change ] 1 UserBuffer.Mapped (KeventTimeout.Readable (0L, 0L)) system
                with
                | Error (KeventRefusal.UnmodelledFilter refused) -> refused |> shouldEqual change
                | other -> failwith $"%A{change}: expected UnmodelledFilter, got %A{other}"

    [<Test>]
    let ``no changelist registers or queues a timer`` () : unit =
        let fds, kq1, kq2, system = world.Force ()

        let idents =
            [
                yield! List.map uint64 fds
                uint64 kq1
                900UL
                0x1_0000_0000UL ||| uint64 (List.head fds)
                System.UInt64.MaxValue
            ]

        let changeGen : Gen<Kevent> =
            gen {
                let! ident = Gen.elements idents
                let! filter = Gen.elements filterNumbers
                let! flags = Gen.elements flagChoices
                let! userData = Gen.choose (0, 3)

                return
                    {
                        Ident = ident
                        Filter = filter
                        Flags = flags
                        FilterFlags = 0u
                        Data = 0L
                        UserData = uint64 userData
                    }
            }

        let callGen : Gen<bool * Kevent list * int> =
            Gen.zip3
                (Gen.elements [ false ; true ])
                (Gen.choose (0, 4) |> Gen.bind (fun n -> Gen.listOfLength n changeGen))
                (Gen.choose (-1, 4))

        let property (calls : (bool * Kevent list * int) list) : unit =
            (system, calls)
            ||> List.fold (fun system (second, changes, room) ->
                let kq = if second then kq2 else kq1

                let after =
                    match
                        UnixKqueue.tryKevent
                            4
                            kq
                            (List.length changes)
                            changes
                            room
                            UserBuffer.Mapped
                            (KeventTimeout.Readable (0L, 0L))
                            system
                    with
                    | Ok (KeventOutcome.Answered events, after) ->
                        events |> List.exists (fun event -> event.Filter = -7s) |> shouldEqual false
                        after
                    | Ok (_, after) -> after
                    | Error _ -> system

                noTimerAnywhere after
                after
            )
            |> ignore

        Check.One (config, Prop.forAll (Arb.fromGen (Gen.listOf callGen)) property)

    [<Test>]
    let ``no sleeping poll registers or queues a timer, and its queries read what it holds`` () : unit =
        let fds, kq, _, system = world.Force ()
        let listener = List.head fds

        let entryGen : Gen<PollEntry> =
            Gen.map2
                (fun fd events ->
                    {
                        Fd = fd
                        Events = events
                    }
                )
                (Gen.elements (-1 :: 900 :: kq :: fds))
                (Gen.choose (0, 0xFFFF) |> Gen.map (uint16 >> int16))

        let queries (queue : PollQueue) : unit =
            PollQueue.registered queue |> shouldEqual (List.ofSeq (Map.keys queue.Registrations))
            PollQueue.activated queue |> shouldEqual queue.Active

            for KeyValue ((fd, filter), registration) in queue.Registrations do
                PollQueue.entry fd filter queue |> shouldEqual (Some registration.Entry)

            for filter in [ KqueueFilter.Read ; KqueueFilter.Write ; KqueueFilter.Timer ] do
                PollQueue.entry 900 filter queue |> shouldEqual None

        let property (entries : PollEntry list) (connect : bool) : unit =
            match UnixPoll.tryPoll 2 entries 1000 system with
            | Ok (PollOutcome.WouldBlock _, parked) ->
                noTimerAnywhere parked
                Map.iter (fun _ queue -> queries queue) parked.Machine.PollQueues

                if connect then
                    // A connection activates the listener's read filter, if the poll
                    // registered one.
                    let _, connected = KeventWorld.client 5300us parked
                    noTimerAnywhere connected
                    Map.iter (fun _ queue -> queries queue) connected.Machine.PollQueues
            | Ok (_, after) -> noTimerAnywhere after
            | Error _ -> ()

        let entriesGen =
            Gen.choose (0, 5) |> Gen.bind (fun n -> Gen.listOfLength n entryGen)

        Check.One (
            config,
            Prop.forAll (Arb.fromGen (Gen.zip entriesGen (Gen.elements [ false ; true ]))) (fun (entries, connect) ->
                property entries connect
            )
        )

        // A control, so that the queries above cannot pass by never seeing a queue: a poll
        // of the listener alone sleeps, registers its read filter, and a connection queues it.
        match UnixPoll.tryPoll 2 [ { Fd = listener ; Events = 0x0001s } ] 1000 system with
        | Ok (PollOutcome.WouldBlock _, parked) ->
            let queue = Seq.exactlyOne parked.Machine.PollQueues.Values
            PollQueue.registered queue |> shouldEqual [ listener, KqueueFilter.Read ]
            PollQueue.entry listener KqueueFilter.Read queue |> shouldEqual (Some 0)
            PollQueue.activated queue |> shouldEqual []
            let _, connected = KeventWorld.client 5300us parked
            let queue = Seq.exactlyOne connected.Machine.PollQueues.Values
            PollQueue.activated queue |> shouldEqual [ listener, KqueueFilter.Read ]
        | other -> failwith $"a poll of the idle listener did not sleep: %A{other}"

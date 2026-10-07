namespace WoofWare.PosixKernel.Test

open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// What one process on a `SimulatedMachine` does to another through the
/// machine they share: a connection or a close in one waking a call asleep in
/// another, a kqueue or a Darwin `poll` in one seeing a socket event another
/// caused, and one process's end closing everything it held.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestCrossProcess =

    // ------------------------------------------------------- one view's checks

    [<Test>]
    let ``one process's descriptor table checks only what one table can tell, on a machine holding others`` () : unit =
        for platform in Machines.platforms do
            let pids, machine = Machines.ofCount platform 2
            let a, b = pids.[0], pids.[1]

            // The other process holds descriptors of its own, which this table
            // does not name; and on Darwin a kqueue registering a socket
            // through a descriptor number this table has not opened.
            let machine =
                match SimulatedUnixPlatform.flavour platform with
                | SimulatedUnixFlavour.Linux ->
                    machine |> Machines.doIn b (fun view -> KeventWorld.stream true view |> snd)
                | SimulatedUnixFlavour.Darwin ->
                    machine
                    |> Machines.doIn
                        b
                        (fun view ->
                            let socket, view = KeventWorld.stream true view
                            let kq, view = KeventWorld.kqueue view
                            KeventWorld.register kq socket -1s 0x1us 0UL view
                        )

            Machines.assertClean machine

            let registry = UnixSystem.fileDescriptors (Machines.viewOf a machine)
            FileDescriptorRegistry.checkInvariants registry |> shouldEqual []

            // A description this table alone names more often than its count
            // records is caught from the one view...
            let stdout = FileDescriptorRegistry.tryFindId 1 registry |> Option.get

            FileDescriptorRegistry.Unchecked.setDescriptorCount stdout 0 registry
            |> FileDescriptorRegistry.checkInvariants
            |> shouldEqual [ FileDescriptorRegistryDefect.DescriptorCountMismatch (stdout, 0, 1) ]

            // ...while one counted more often than this table names it may be
            // named in another process's table, so only the machine can tell.
            FileDescriptorRegistry.Unchecked.setDescriptorCount stdout 2 registry
            |> FileDescriptorRegistry.checkInvariants
            |> shouldEqual []

    [<Test>]
    let ``a descriptor table on a machine holding its process alone checks every count exactly`` () : unit =
        for platform in Machines.platforms do
            let pids, machine = Machines.ofCount platform 1
            let registry = UnixSystem.fileDescriptors (Machines.viewOf pids.[0] machine)
            let stdout = FileDescriptorRegistry.tryFindId 1 registry |> Option.get

            FileDescriptorRegistry.Unchecked.setDescriptorCount stdout 2 registry
            |> FileDescriptorRegistry.checkInvariants
            |> shouldEqual [ FileDescriptorRegistryDefect.DescriptorCountMismatch (stdout, 2, 1) ]

    // ------------------------------------------------------- Darwin kqueues

    let private darwin : SimulatedUnixPlatform = SimulatedUnixPlatform.macOsArm64

    let private addClear : uint16 = KeventFlags.Add ||| KeventFlags.Clear

    /// The events one `kevent` wait on `kq`, with room for `room`, reports at
    /// once in the view of `pid`.
    let private reported
        (pid : ProcessId)
        (kq : int)
        (room : int)
        (machine : SimulatedMachine<int, string>)
        : Kevent list * SimulatedMachine<int, string>
        =
        Machines.inProcess
            pid
            (fun view ->
                match KeventWorld.apply kq [] room view with
                | KeventOutcome.Answered events, view -> events, view
                | other, _ -> failwith $"kevent on fd %d{kq}: %A{other}"
            )
            machine

    let private summary (events : Kevent list) : (uint64 * int16 * int64 * uint64) list =
        events
        |> List.map (fun event -> event.Ident, event.Filter, event.Data, event.UserData)

    [<Test>]
    let ``a connection from another process activates the listener's registration in its kqueue`` () : unit =
        let pids, machine = Machines.ofCount darwin 2
        let a, b = pids.[0], pids.[1]

        let (listener, kq), machine =
            Machines.inProcess
                b
                (fun view ->
                    let listener, view = KeventWorld.listenerAt 8080us view
                    let kq, view = KeventWorld.kqueue view
                    (listener, kq), KeventWorld.register kq listener KeventFilter.Read addClear 7UL view
                )
                machine

        // The connecting socket takes the same number in its own process as
        // the listener has in the other, so an activation that read the
        // registration's descriptor in the connecting process's table would
        // find the connecting socket instead.
        let client, machine = Machines.inProcess a (KeventWorld.client 8080us) machine
        client |> shouldEqual listener
        Machines.assertClean machine

        let events, machine = reported b kq 4 machine
        summary events |> shouldEqual [ uint64 listener, KeventFilter.Read, 1L, 7UL ]
        Machines.assertClean machine

        // EV_CLEAR: reported once.
        reported b kq 4 machine |> fst |> shouldEqual []

    [<Test>]
    let ``another process's close of its end activates the peer's READ registration with EOF`` () : unit =
        let pids, machine = Machines.ofCount darwin 2
        let a, b = pids.[0], pids.[1]

        let listener, machine = Machines.inProcess b (KeventWorld.listenerAt 8080us) machine

        let client, machine = Machines.inProcess a (KeventWorld.client 8080us) machine

        let (accepted, kq), machine =
            Machines.inProcess
                b
                (fun view ->
                    let accepted, view = KeventWorld.accept listener view
                    let kq, view = KeventWorld.kqueue view
                    (accepted, kq), KeventWorld.register kq accepted KeventFilter.Read addClear 9UL view
                )
                machine

        reported b kq 4 machine |> fst |> shouldEqual []

        let machine = Machines.doIn a (KeventWorld.close client) machine
        Machines.assertClean machine

        let events, machine = reported b kq 4 machine

        events
        |> List.map (fun event -> event.Ident, event.Filter, event.Flags &&& KeventFlags.Eof <> 0us, event.UserData)
        |> shouldEqual [ uint64 accepted, KeventFilter.Read, true, 9UL ]

        Machines.assertClean machine

    [<Test>]
    let ``a close in one process leaves another process's registrations through the same number`` () : unit =
        let pids, machine = Machines.ofCount darwin 2
        let a, b = pids.[0], pids.[1]

        let (listener, kq), machine =
            Machines.inProcess
                b
                (fun view ->
                    let listener, view = KeventWorld.listenerAt 8080us view
                    let kq, view = KeventWorld.kqueue view
                    (listener, kq), KeventWorld.register kq listener KeventFilter.Read addClear 7UL view
                )
                machine

        // The other process opens and closes the same number.
        let own, machine = Machines.inProcess a (KeventWorld.stream true) machine
        own |> shouldEqual listener
        let machine = Machines.doIn a (KeventWorld.close own) machine
        Machines.assertClean machine

        let _, machine = Machines.inProcess a (KeventWorld.client 8080us) machine

        reported b kq 4 machine
        |> fst
        |> summary
        |> shouldEqual [ uint64 listener, KeventFilter.Read, 1L, 7UL ]

    // -------------------------------------------------------- Darwin poll

    let private pollIn : int16 = 0x0001s
    let private pollHup : int16 = 0x0010s

    let private entry (fd : int) (events : int16) : PollEntry =
        {
            Fd = fd
            Events = events
        }

    /// `task` of `pid` polls `entries` with no timeout, and sleeps.
    let private sleepInPoll
        (pid : ProcessId)
        (task : int)
        (entries : PollEntry list)
        (machine : SimulatedMachine<int, string>)
        : SimulatedMachine<int, string>
        =
        Machines.doIn
            pid
            (fun view ->
                match UnixPoll.poll task entries -1 view with
                | Ok (PollOutcome.WouldBlock _, view) -> view
                | other -> failwith $"poll: expected to sleep, got %A{other}"
            )
            machine

    /// The `poll` `task` of `pid` sleeps in, finished.
    let private finishPoll
        (pid : ProcessId)
        (task : int)
        (machine : SimulatedMachine<int, string>)
        : PollOutcome * SimulatedMachine<int, string>
        =
        Machines.inProcess
            pid
            (fun view ->
                match UnixPoll.finishPoll task view with
                | Ok finished -> finished
                | Error refusal -> failwith $"finishPoll: %A{refusal}"
            )
            machine

    [<Test>]
    let ``a connection from another process wakes a sleeping poll on the listener`` () : unit =
        let pids, machine = Machines.ofCount darwin 2
        let a, b = pids.[0], pids.[1]

        let listener, machine = Machines.inProcess b (KeventWorld.listenerAt 8080us) machine

        let machine = sleepInPoll b 1 [ entry listener pollIn ] machine
        Machines.assertClean machine

        UnixWait.wakes (Set.singleton 1) (Machines.viewOf b machine) |> shouldEqual []

        let client, machine = Machines.inProcess a (KeventWorld.client 8080us) machine
        client |> shouldEqual listener
        Machines.assertClean machine

        UnixWait.wakes (Set.singleton 1) (Machines.viewOf b machine)
        |> shouldEqual [ 1, Set.singleton WakePrimitive.KqueuePollReportable ]

        let outcome, machine = finishPoll b 1 machine
        outcome |> shouldEqual (PollOutcome.Answered ([ pollIn ], 1))
        Machines.assertClean machine

    [<Test>]
    let ``another process's close wakes a sleeping poll on the peer with a hang-up`` () : unit =
        let pids, machine = Machines.ofCount darwin 2
        let a, b = pids.[0], pids.[1]

        let listener, machine = Machines.inProcess b (KeventWorld.listenerAt 8080us) machine

        let client, machine = Machines.inProcess a (KeventWorld.client 8080us) machine
        let accepted, machine = Machines.inProcess b (KeventWorld.accept listener) machine
        let machine = sleepInPoll b 1 [ entry accepted pollIn ] machine

        let machine = Machines.doIn a (KeventWorld.close client) machine
        Machines.assertClean machine

        UnixWait.wakes (Set.singleton 1) (Machines.viewOf b machine)
        |> shouldEqual [ 1, Set.singleton WakePrimitive.KqueuePollReportable ]

        let outcome, machine = finishPoll b 1 machine
        outcome |> shouldEqual (PollOutcome.Answered ([ pollIn ||| pollHup ], 1))
        Machines.assertClean machine

    [<Test>]
    let ``a close in one process leaves another process's sleeping poll's filters through the same number`` () : unit =
        let pids, machine = Machines.ofCount darwin 2
        let a, b = pids.[0], pids.[1]

        let listener, machine = Machines.inProcess b (KeventWorld.listenerAt 8080us) machine

        let machine = sleepInPoll b 1 [ entry listener pollIn ] machine
        let own, machine = Machines.inProcess a (KeventWorld.stream true) machine
        own |> shouldEqual listener
        let machine = Machines.doIn a (KeventWorld.close own) machine
        let _, machine = Machines.inProcess a (KeventWorld.client 8080us) machine
        Machines.assertClean machine

        finishPoll b 1 machine
        |> fst
        |> shouldEqual (PollOutcome.Answered ([ pollIn ], 1))

    // ------------------------------------------------------ the machine's wakes

    let private woken
        (asleep : (ProcessId * int) list)
        (machine : SimulatedMachine<int, string>)
        : ((ProcessId * int) * Set<WakePrimitive>) list
        =
        let asleep =
            asleep
            |> List.groupBy fst
            |> List.map (fun (pid, tasks) -> pid, tasks |> List.map snd |> Set.ofList)
            |> Map.ofList

        SimulatedMachine.wakes asleep machine

    /// `task` of `pid` accepts on the blocking listener `listener`, which holds
    /// nothing, and sleeps.
    let private sleepInAccept
        (pid : ProcessId)
        (task : int)
        (listener : int)
        (machine : SimulatedMachine<int, string>)
        : SimulatedMachine<int, string>
        =
        Machines.doIn
            pid
            (fun view ->
                match UnixConnection.accept task listener UserBuffer.Mapped 16u view with
                | Ok (AcceptOutcome.WouldBlock _, view) -> view
                | other -> failwith $"accept: expected to sleep, got %A{other}"
            )
            machine

    let private finishAccept
        (pid : ProcessId)
        (task : int)
        (machine : SimulatedMachine<int, string>)
        : AcceptOutcome * SimulatedMachine<int, string>
        =
        Machines.inProcess
            pid
            (fun view ->
                match UnixConnection.finishAccept task view with
                | Ok finished -> finished
                | Error refusal -> failwith $"finishAccept: %A{refusal}"
            )
            machine

    [<Test>]
    let ``a connection from another process wakes an accept asleep on the listener`` () : unit =
        for platform in Machines.platforms do
            let pids, machine = Machines.ofCount platform 2
            let a, b = pids.[0], pids.[1]

            let listener, machine = Machines.inProcess b (KeventWorld.listenerAt 8080us) machine

            let machine = sleepInAccept b 1 listener machine
            Machines.assertClean machine
            woken [ b, 1 ] machine |> shouldEqual []

            let _, machine = Machines.inProcess a (KeventWorld.client 8080us) machine
            Machines.assertClean machine

            let listenerId = KeventWorld.idOf listener (Machines.viewOf b machine)

            woken [ b, 1 ] machine
            |> shouldEqual [ (b, 1), Set.singleton (WakePrimitive.AcceptQueueNonEmpty listenerId) ]

            match finishAccept b 1 machine with
            | AcceptOutcome.Accepted _, machine -> Machines.assertClean machine
            | other, _ -> failwith $"finishAccept: %A{other}"

    [<Test>]
    let ``a connection from another process wakes an epoll_wait on an instance watching the listener`` () : unit =
        let pids, machine = Machines.ofCount SimulatedUnixPlatform.linuxX64 2
        let a, b = pids.[0], pids.[1]

        let (listener, epoll), machine =
            Machines.inProcess
                b
                (fun view ->
                    let listener, view = KeventWorld.listenerAt 8080us view

                    let epoll, view =
                        match UnixPoll.epollCreate1 0 view with
                        | Ok (Ok created) -> created
                        | other -> failwith $"epoll_create1: %A{other}"

                    let view =
                        match
                            UnixPoll.epollCtl
                                epoll
                                1
                                listener
                                (EpollEventArgument.Readable (EpollEvents.In ||| EpollEvents.EdgeTriggered, 5UL))
                                view
                        with
                        | Ok (EpollCtlAnswer.Changed, view) -> view
                        | other -> failwith $"epoll_ctl: %A{other}"

                    (listener, epoll), view
                )
                machine

        let machine =
            Machines.doIn
                b
                (fun view ->
                    match UnixPoll.epollWait 1 epoll 4 UserBuffer.Mapped -1 view with
                    | Ok (EpollWaitOutcome.WouldBlock _, view) -> view
                    | other -> failwith $"epoll_wait: expected to sleep, got %A{other}"
                )
                machine

        woken [ b, 1 ] machine |> shouldEqual []

        let _, machine = Machines.inProcess a (KeventWorld.client 8080us) machine
        Machines.assertClean machine

        let epollId = KeventWorld.idOf epoll (Machines.viewOf b machine)

        woken [ b, 1 ] machine
        |> shouldEqual [ (b, 1), Set.singleton (WakePrimitive.EpollEventDeliverable epollId) ]

        let outcome, machine =
            Machines.inProcess
                b
                (fun view ->
                    match UnixPoll.finishEpollWait 1 view with
                    | Ok finished -> finished
                    | Error refusal -> failwith $"finishEpollWait: %A{refusal}"
                )
                machine

        outcome |> shouldEqual (EpollWaitOutcome.Answered [ 5UL, EpollEvents.In ])
        ignore listener
        Machines.assertClean machine

    [<Test>]
    let ``another process's close wakes an epoll_wait watching the peer, with the half-close`` () : unit =
        let pids, machine = Machines.ofCount SimulatedUnixPlatform.linuxX64 2
        let a, b = pids.[0], pids.[1]

        let listener, machine = Machines.inProcess b (KeventWorld.listenerAt 8080us) machine

        let client, machine = Machines.inProcess a (KeventWorld.client 8080us) machine

        let epoll, machine =
            Machines.inProcess
                b
                (fun view ->
                    let accepted, view = KeventWorld.accept listener view

                    let epoll, view =
                        match UnixPoll.epollCreate1 0 view with
                        | Ok (Ok created) -> created
                        | other -> failwith $"epoll_create1: %A{other}"

                    let view =
                        match
                            UnixPoll.epollCtl
                                epoll
                                1
                                accepted
                                (EpollEventArgument.Readable (EpollEvents.RdHup ||| EpollEvents.EdgeTriggered, 6UL))
                                view
                        with
                        | Ok (EpollCtlAnswer.Changed, view) -> view
                        | other -> failwith $"epoll_ctl: %A{other}"

                    epoll, view
                )
                machine

        let machine =
            Machines.doIn
                b
                (fun view ->
                    match UnixPoll.epollWait 1 epoll 4 UserBuffer.Mapped -1 view with
                    | Ok (EpollWaitOutcome.WouldBlock _, view) -> view
                    | other -> failwith $"epoll_wait: expected to sleep, got %A{other}"
                )
                machine

        woken [ b, 1 ] machine |> shouldEqual []

        let machine = Machines.doIn a (KeventWorld.close client) machine
        Machines.assertClean machine

        woken [ b, 1 ] machine |> List.map fst |> shouldEqual [ b, 1 ]

        let outcome, machine =
            Machines.inProcess
                b
                (fun view ->
                    match UnixPoll.finishEpollWait 1 view with
                    | Ok finished -> finished
                    | Error refusal -> failwith $"finishEpollWait: %A{refusal}"
                )
                machine

        outcome |> shouldEqual (EpollWaitOutcome.Answered [ 6UL, EpollEvents.RdHup ])
        Machines.assertClean machine

    [<Test>]
    let ``a connection from another process wakes a kevent wait on a kqueue watching the listener`` () : unit =
        let pids, machine = Machines.ofCount darwin 2
        let a, b = pids.[0], pids.[1]

        let (listener, kq), machine =
            Machines.inProcess
                b
                (fun view ->
                    let listener, view = KeventWorld.listenerAt 8080us view
                    let kq, view = KeventWorld.kqueue view
                    (listener, kq), KeventWorld.register kq listener KeventFilter.Read addClear 7UL view
                )
                machine

        let machine =
            Machines.doIn
                b
                (fun view ->
                    match UnixKqueue.kevent 1 kq 0 [] 4 UserBuffer.Mapped KeventTimeout.Null view with
                    | Ok (KeventOutcome.WouldBlock _, view) -> view
                    | other -> failwith $"kevent: expected to sleep, got %A{other}"
                )
                machine

        woken [ b, 1 ] machine |> shouldEqual []

        let _, machine = Machines.inProcess a (KeventWorld.client 8080us) machine
        Machines.assertClean machine

        let kqueueId = KeventWorld.idOf kq (Machines.viewOf b machine)

        woken [ b, 1 ] machine
        |> shouldEqual [ (b, 1), Set.singleton (WakePrimitive.KqueueEventDeliverable kqueueId) ]

        let outcome, machine =
            Machines.inProcess
                b
                (fun view ->
                    match UnixKqueue.finishKevent 1 view with
                    | Ok finished -> finished
                    | Error refusal -> failwith $"finishKevent: %A{refusal}"
                )
                machine

        match outcome with
        | KeventOutcome.Answered events -> summary events |> shouldEqual [ uint64 listener, KeventFilter.Read, 1L, 7UL ]
        | other -> failwith $"finishKevent: %A{other}"

        Machines.assertClean machine

    [<Test>]
    let ``a lock released in one process wakes an flock asleep on the same file in another`` () : unit =
        let lockEx = 2
        let lockUn = 8

        for platform in Machines.platforms do
            let pids, machine = Machines.ofCount platform 2
            let a, b = pids.[0], pids.[1]

            let openShared (view : UnixSystem<int, string>) : int * UnixSystem<int, string> =
                match
                    OpenFlagWords.openPath
                        {
                            Access = FileAccessMode.ReadWrite
                            Create = true
                            Exclusive = false
                            Truncate = false
                            NoFollow = false
                            CloseOnExec = false
                            Synchronous = false
                            DataSynchronous = false
                            Directory = false
                        }
                        (PathArg.ofText "/locked")
                        0o644
                        view
                with
                | Ok (SyscallAnswer.Completed fd, view) -> int fd, view
                | other -> failwith $"open: %A{other}"

            let flock (task : int) (fd : int) (operation : int) (view : UnixSystem<int, string>) =
                match UnixDescriptor.flock task fd operation view with
                | Ok answered -> answered
                | Error refusal -> failwith $"flock: %A{refusal}"

            let held, machine = Machines.inProcess a openShared machine
            let waiting, machine = Machines.inProcess b openShared machine

            let outcome, machine = Machines.inProcess a (flock 1 held lockEx) machine
            outcome |> shouldEqual (SyscallOutcome.Answered (SyscallAnswer.Completed 0L))

            let outcome, machine = Machines.inProcess b (flock 1 waiting lockEx) machine

            match outcome with
            | SyscallOutcome.WouldBlock _ -> ()
            | other -> failwith $"the second flock: expected to sleep, got %A{other}"

            Machines.assertClean machine
            woken [ b, 1 ] machine |> shouldEqual []

            let outcome, machine = Machines.inProcess a (flock 1 held lockUn) machine
            outcome |> shouldEqual (SyscallOutcome.Answered (SyscallAnswer.Completed 0L))

            let waitingId = KeventWorld.idOf waiting (Machines.viewOf b machine)

            woken [ b, 1 ] machine
            |> shouldEqual
                [
                    (b, 1), Set.singleton (WakePrimitive.FlockGrantable (waitingId, FlockMode.Exclusive))
                ]

            let outcome, machine =
                Machines.inProcess
                    b
                    (fun view ->
                        match UnixDescriptor.flockAcquire 1 view with
                        | Ok finished -> finished
                        | Error refusal -> failwith $"flockAcquire: %A{refusal}"
                    )
                    machine

            outcome |> shouldEqual (SyscallOutcome.Answered (SyscallAnswer.Completed 0L))
            Machines.assertClean machine

    [<Test>]
    let ``the machine wakes in park order across processes, and one waiter of an exclusive queue`` () : unit =
        for platform in Machines.platforms do
            let pids, machine = Machines.ofCount platform 2
            let a, b = pids.[0], pids.[1]

            let listenerA, machine =
                Machines.inProcess a (KeventWorld.listenerAt 8080us) machine

            let listenerB, machine =
                Machines.inProcess b (KeventWorld.listenerAt 8081us) machine

            // Parked in this order: b's 1, a's 1, b's 2, the last two on the
            // listeners the first two are not.
            let machine =
                machine
                |> sleepInAccept b 1 listenerB
                |> sleepInAccept a 1 listenerA
                |> sleepInAccept b 2 listenerB

            let asleep = [ a, 1 ; b, 1 ; b, 2 ]
            woken asleep machine |> shouldEqual []

            // One connection to each, made by the other process's third task.
            let _, machine = Machines.inProcess a (KeventWorld.client 8081us) machine
            let _, machine = Machines.inProcess b (KeventWorld.client 8080us) machine
            Machines.assertClean machine

            // Park order, not process order; and of b's two accepters on one
            // listener holding one connection, the one that parked first.
            woken asleep machine |> List.map fst |> shouldEqual [ b, 1 ; a, 1 ]

            // A woken accepter that has not yet finished stands for the
            // connection, so its queue wakes nobody else meanwhile.
            woken [ a, 1 ; b, 2 ] machine |> List.map fst |> shouldEqual [ a, 1 ]

    [<Test>]
    let ``the machine's wakes refuse a process it does not hold`` () : unit =
        let _, machine = Machines.ofCount SimulatedUnixPlatform.linuxX64 1
        let ghost = ProcessId.parseOrFail "test" 77

        let error = Assert.Throws<exn> (fun () -> woken [ ghost, 1 ] machine |> ignore)

        error.Message |> shouldContainText "77"

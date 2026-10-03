namespace WoofWare.PosixKernel.Test

open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// A `poll(2)` that finds nothing ready and sleeps: the park, its wake condition,
/// and `UnixPoll.finishPoll`.
///
/// The facts these rows hold the library to were measured by
/// `docs/plans/2026-08-23-posix-kernel-extraction/poll-timeout.c` on Linux 6.18.5
/// aarch64 and Darwin 27.0.0 (2026-09-26): an expired wait returns 0 and never
/// before `ms` milliseconds; every negative timeout is infinite; readiness ends
/// the wait at once; and a woken poll looks each descriptor up again by number.
///
/// The waiter polls a listening socket, which presents nothing while its accept
/// queue is empty and `IN|RDNORM` once it is not. The queue is set directly, so
/// that a row can make the descriptor ready, and unready again, at an instant it
/// chooses.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestPollTimeout =

    let private pollIn : int16 = 0x0001s
    let private pollOut : int16 = 0x0004s
    let private pollRdNorm : int16 = 0x0040s

    let private task : int = 1
    let private nanosecondsPerMillisecond : int64 = 1_000_000L

    let private withRegistry
        (registry : FileDescriptorRegistry)
        (system : UnixSystem<int, string>)
        : UnixSystem<int, string>
        =
        { system with
            Process =
                { system.Process with
                    FileDescriptors = registry
                }
        }

    let private idOf (fd : int) (system : UnixSystem<int, string>) : OpenFileDescriptionId =
        match FileDescriptorRegistry.tryFindWithId fd system.Process.FileDescriptors with
        | Some (id, _) -> id
        | None -> failwith $"fd %d{fd} names no description"

    /// A Linux-flavoured system with task 1 registered, a listening socket with an
    /// empty accept queue, and the descriptor onto it.
    let private world : int * UnixSystem<int, string> =
        let system =
            UnixSystem.initial<int, string> SimulatedUnixPlatform.linuxX64 UnixSystem.pipedStandardStreams 0 (CpuId 0)
            |> UnixBootImage.boot

        let system = Tasks.spawn task system

        let socketId = system.Machine.NextSocketId
        let (SocketId raw) = socketId

        let socket =
            {
                Domain = SocketDomain.Inet
                Kind = SocketKind.Stream
                Protocol = SocketProtocol.Tcp
                Binding =
                    Some
                        {
                            Endpoint =
                                {
                                    Address = 0x7F000001u
                                    Port = 40000us
                                }
                            LockedAddress = Some 0x7F000001u
                            LockedPort = true
                        }
                ReuseAddress = false
                Phase =
                    SocketPhase.Listening
                        {
                            Backlog = 8
                            Queue = []
                            Drained = false
                        }
            }

        let fd, registry =
            FileDescriptorRegistry.createSocket socketId system.Process.FileDescriptors

        fd,
        { withRegistry registry system with
            Machine =
                { system.Machine with
                    Sockets = Map.add socketId socket system.Machine.Sockets
                    NextSocketId = SocketId (raw + 1L)
                }
        }

    let private listener : int = fst world
    let private idle : UnixSystem<int, string> = snd world

    /// `system` with the listener's accept queue holding `queue`.
    let private withQueue (queue : ConnectionId list) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        let socketId =
            match FileDescriptorRegistry.tryFindTarget listener system.Process.FileDescriptors with
            | Some (OpenFileTarget.Socket socketId) -> socketId
            | other -> failwith $"expected the listener, got %O{other}"

        let socket = UnixMachineState.socket socketId system.Machine

        { system with
            Machine =
                { system.Machine with
                    Sockets =
                        Map.add
                            socketId
                            { socket with
                                Phase =
                                    SocketPhase.Listening
                                        {
                                            Backlog = 8
                                            Queue = queue
                                            Drained = false
                                        }
                            }
                            system.Machine.Sockets
                }
        }

    let private ready : UnixSystem<int, string> -> UnixSystem<int, string> =
        withQueue [ ConnectionId 99L ]

    let private after (nanoseconds : int64) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        { system with
            Machine = UnixMachineState.advanceClock nanoseconds system.Machine
        }

    let private entry (fd : int) (events : int16) : PollEntry =
        {
            Fd = fd
            Events = events
        }

    let private parks
        (entries : PollEntry list)
        (milliseconds : int)
        (system : UnixSystem<int, string>)
        : WakeCondition * UnixSystem<int, string>
        =
        match UnixPoll.poll task entries milliseconds system with
        | Ok (PollOutcome.WouldBlock condition, parked) -> condition, parked
        | other -> failwith $"expected a park, got %A{other}"

    let private finishes (system : UnixSystem<int, string>) : int16 list * int * UnixSystem<int, string> =
        match UnixPoll.finishPoll task system with
        | Ok (PollOutcome.Answered (reported, count), finished) -> reported, count, finished
        | other -> failwith $"expected the poll to finish, got %A{other}"

    let private reparks (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        match UnixPoll.finishPoll task system with
        | Ok (PollOutcome.WouldBlock _, parked) -> parked
        | other -> failwith $"expected the poll to park again, got %A{other}"

    let private woken (system : UnixSystem<int, string>) : Set<WakePrimitive> option =
        match UnixWait.wakes (Set.singleton task) system with
        | [] -> None
        | [ woken, fired ] when woken = task -> Some fired
        | other -> failwith $"unexpected wake %A{other}"

    /// Requests the listener answers once its queue is non-empty, with the
    /// `revents` it answers: `IN` or `RDNORM` asked, alongside anything else.
    let private readableRequest : Gen<int16> =
        Gen.zip (Gen.elements [ pollIn ; pollRdNorm ; pollIn ||| pollRdNorm ]) (Gen.elements [ 0s ; pollOut ])
        |> Gen.map (fun (read, other) -> read ||| other)

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 300

    /// Positive timeouts, often small, where an off-by-one in the conversion to
    /// a deadline would show most.
    let private timeoutGen : Gen<int> =
        Gen.oneof
            [
                Gen.choose (1, 10)
                Gen.choose (1, 100_000)
                Gen.constant System.Int32.MaxValue
            ]

    [<Test>]
    let ``a poll that times out fires exactly at its deadline, and answers 0`` () : unit =
        let property =
            Prop.forAll (Arb.fromGen (Gen.zip3 timeoutGen (Gen.choose64 (0L, 1_000_000_000_000L)) readableRequest))
            <| fun (milliseconds, start, events) ->
                let started = after start idle
                let _, parked = parks [ entry listener events ] milliseconds started
                let deadline = start + int64 milliseconds * nanosecondsPerMillisecond

                UnixWait.deadlines (Set.singleton task) parked |> shouldEqual [ deadline ]

                // One nanosecond short: nothing wakes it, and a finish attempted
                // anyway sleeps again on the same deadline.
                let justBefore = after (deadline - start - 1L) parked
                woken justBefore |> shouldEqual None
                let parkedAgain = reparks justBefore
                UnixWait.deadlines (Set.singleton task) parkedAgain |> shouldEqual [ deadline ]

                let atDeadline = after (deadline - start) parked

                woken atDeadline
                |> shouldEqual (Some (Set.singleton (WakePrimitive.DeadlinePassed deadline)))

                let reported, count, finished = finishes atDeadline
                reported |> shouldEqual [ 0s ]
                count |> shouldEqual 0
                UnixTaskTable.parkedFor task finished.Tasks |> shouldEqual None
                UnixSystem.checkInvariants finished |> shouldEqual []

        Check.One (config, property)

    [<Test>]
    let ``readiness before the deadline wins, and answers what is ready`` () : unit =
        let property =
            Prop.forAll (Arb.fromGen (Gen.zip3 timeoutGen readableRequest (Gen.choose64 (0L, 99_999_999_999L))))
            <| fun (milliseconds, events, lateness) ->
                let _, parked = parks [ entry 1 pollIn ; entry listener events ] milliseconds idle
                let deadline = int64 milliseconds * nanosecondsPerMillisecond
                let readyAt = lateness % deadline
                let becameReady = parked |> after readyAt |> ready

                woken becameReady
                |> shouldEqual (
                    Some (
                        Set.singleton (
                            WakePrimitive.DescriptorReady (
                                idOf listener idle,
                                uint32 (uint16 events) ||| 0x0008u ||| 0x0010u
                            )
                        )
                    )
                )

                let reported, count, finished = finishes becameReady
                reported |> shouldEqual [ 0s ; events &&& (pollIn ||| pollRdNorm) ]
                count |> shouldEqual 1
                UnixTaskTable.parkedFor task finished.Tasks |> shouldEqual None

        Check.One (config, property)

    [<Test>]
    let ``a descriptor ready at the deadline is reported, not the timeout`` () : unit =
        // A real poll scans once more when its time is up, so what is ready then is
        // what it returns.
        let _, parked = parks [ entry listener pollIn ] 5 idle

        let reported, count, _ =
            parked |> after (5L * nanosecondsPerMillisecond) |> ready |> finishes

        reported |> shouldEqual [ pollIn ]
        count |> shouldEqual 1

    [<Test>]
    let ``an unrequested ERR or HUP ends the wait`` () : unit =
        // Asked for nothing at all, the entry still waits for the two conditions
        // poll reports unasked: here, a connection refusal arriving.
        let _, parked = parks [ entry listener 0s ] 10 idle

        let refused =
            let socketId =
                match FileDescriptorRegistry.tryFindTarget listener parked.Process.FileDescriptors with
                | Some (OpenFileTarget.Socket socketId) -> socketId
                | other -> failwith $"expected the listener, got %O{other}"

            let socket = UnixMachineState.socket socketId parked.Machine

            { parked with
                Machine =
                    { parked.Machine with
                        Sockets =
                            Map.add
                                socketId
                                { socket with
                                    Phase = SocketPhase.Refused RefusalError.Pending
                                }
                                parked.Machine.Sockets
                    }
            }

        woken refused |> Option.isSome |> shouldEqual true
        let reported, count, _ = finishes refused
        reported |> shouldEqual [ 0x0008s ||| 0x0010s ]
        count |> shouldEqual 1

    [<Test>]
    let ``every negative timeout waits with no deadline`` () : unit =
        for milliseconds in [ -1 ; -2 ; -1000 ; System.Int32.MinValue ] do
            let condition, parked = parks [ entry listener pollIn ] milliseconds idle
            WakeCondition.deadlines condition |> shouldEqual []
            UnixWait.deadlines (Set.singleton task) parked |> shouldEqual []

            let muchLater = after 1_000_000_000_000_000L parked
            woken muchLater |> shouldEqual None
            let reported, count, _ = muchLater |> ready |> finishes
            reported |> shouldEqual [ pollIn ]
            count |> shouldEqual 1

    [<Test>]
    let ``a timeout of zero never parks`` () : unit =
        match UnixPoll.poll task [ entry listener pollIn ] 0 idle with
        | Ok (PollOutcome.Answered (reported, count), after) ->
            reported |> shouldEqual [ 0s ]
            count |> shouldEqual 0
            after |> shouldEqual idle
        | other -> failwith $"expected an answer, got %A{other}"

    [<Test>]
    let ``a wait with nothing to watch and no deadline sleeps until a signal`` () : unit =
        // `poll(NULL, 0, -1)` sleeps until a signal interrupts it.
        for entries in [ [] ; [ entry -1 pollIn ] ; [ entry -5 pollIn ; entry -1 0s ] ] do
            for milliseconds in [ -1 ; System.Int32.MinValue ] do
                match UnixPoll.poll task entries milliseconds idle with
                | Ok (PollOutcome.WouldBlock condition, parked) ->
                    condition
                    |> shouldEqual (WakeCondition.Primitive WakePrimitive.SignalDeliverable)

                    UnixWait.wakes (Set.singleton task) parked |> shouldEqual []
                    UnixWait.deadlines (Set.singleton task) parked |> shouldEqual []
                | other -> failwith $"expected the poll to sleep, got %A{other}"

    [<Test>]
    let ``a deadline past the clock's range is refused, and one just inside it parks`` () : unit =
        let timeout = 1_000_000L

        let nearTheEnd (uptime : int64) : UnixSystem<int, string> =
            { idle with
                Machine = UnixMachineState.advanceClock uptime idle.Machine
            }

        UnixPoll.poll task [ entry listener pollIn ] 1 (nearTheEnd (System.Int64.MaxValue - timeout + 1L))
        |> shouldEqual (Error (PollRefusal.DeadlineBeyondClock (System.Int64.MaxValue - timeout + 1L, 1)))

        let _, parked =
            parks [ entry listener pollIn ] 1 (nearTheEnd (System.Int64.MaxValue - timeout))

        UnixWait.deadlines (Set.singleton task) parked
        |> shouldEqual [ System.Int64.MaxValue ]

    [<Test>]
    let ``a wait with nothing to watch and a deadline sleeps until it`` () : unit =
        let condition, parked = parks [ entry -1 pollIn ] 3 idle

        condition
        |> shouldEqual (Interruptible.condition (WakeCondition.Primitive (WakePrimitive.DeadlinePassed 3_000_000L)))

        let reported, count, _ = parked |> after 3_000_000L |> finishes
        reported |> shouldEqual [ 0s ]
        count |> shouldEqual 0

    [<Test>]
    let ``a woken poll whose readiness has gone again sleeps on its original deadline`` () : unit =
        let _, parked = parks [ entry listener pollIn ] 10 idle
        let becameReady = parked |> after 1_000_000L |> ready
        woken becameReady |> Option.isSome |> shouldEqual true

        // Taken by someone else before the woken poll ran.
        let gone = becameReady |> withQueue []
        let parkedAgain = reparks gone

        UnixWait.deadlines (Set.singleton task) parkedAgain
        |> shouldEqual [ 10_000_000L ]

        (UnixTaskTable.parkOf task parkedAgain.Tasks |> Option.get).Ordinal
        |> shouldBeGreaterThan (UnixTaskTable.parkOf task parked.Tasks |> Option.get).Ordinal

        let reported, count, _ = parkedAgain |> after 9_000_000L |> finishes
        reported |> shouldEqual [ 0s ]
        count |> shouldEqual 0

    [<Test>]
    let ``the park records each entry with the description it named`` () : unit =
        let _, parked = parks [ entry listener pollIn ; entry -3 pollOut ] 7 idle

        UnixTaskTable.parkedFor task parked.Tasks
        |> shouldEqual (
            Some (
                ParkedSyscall.Poll
                    {
                        Entries =
                            [
                                ParkedPollEntry.Watched (listener, idOf listener idle, pollIn)
                                ParkedPollEntry.Ignored -3
                            ]
                        Deadline = Some 7_000_000L
                    }
            )
        )

        UnixSystem.checkInvariants parked |> shouldEqual []

    [<Test>]
    let ``closing a descriptor a parked poll watches is refused, and closing a dup of it is not`` () : unit =
        let dupFd, registry =
            match FileDescriptorRegistry.dup listener idle.Process.FileDescriptors with
            | Ok (fd, registry) -> fd, registry
            | Error error -> failwith $"dup failed: %O{error}"

        let withDup = withRegistry registry idle
        let _, parked = parks [ entry listener pollIn ] 10 withDup

        match UnixDescriptor.close listener parked with
        | Error (CloseRefusal.PolledDescriptor (fd, waiter)) ->
            fd |> shouldEqual listener
            waiter |> shouldEqual task
        | other -> failwith $"expected the close to be refused, got %A{other}"

        match UnixDescriptor.close dupFd parked with
        | Ok (SyscallAnswer.Completed 0L, closed) ->
            UnixSystem.checkInvariants closed |> shouldEqual []
            let reported, count, _ = closed |> ready |> finishes
            reported |> shouldEqual [ pollIn ]
            count |> shouldEqual 1
        | other -> failwith $"expected the dup's close to succeed, got %A{other}"

    [<Test>]
    let ``closing a polled directory descriptor is refused too`` () : unit =
        // A directory presents IN|OUT|RDNORM|WRNORM, so only a request for none of
        // them, such as PRI alone, leaves it waiting.
        let directory, registry =
            FileDescriptorRegistry.openDirectory (InodeNumber 1L) idle.Process.FileDescriptors

        let _, parked = parks [ entry directory 0x0002s ] 10 (withRegistry registry idle)
        UnixSystem.checkInvariants parked |> shouldEqual []

        match UnixDescriptor.close directory parked with
        | Error (CloseRefusal.PolledDescriptor (fd, waiter)) ->
            fd |> shouldEqual directory
            waiter |> shouldEqual task
        | other -> failwith $"expected the close to be refused, got %A{other}"

        let reported, count, _ = parked |> after 10_000_000L |> finishes
        reported |> shouldEqual [ 0s ]
        count |> shouldEqual 0

    /// `system` with `fd` closed and a file opened in its place, as only a caller
    /// going around `UnixDescriptor.close` could.
    let private forgeRebind (fd : int) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        let registry =
            match FileDescriptorRegistry.dropDescriptor fd Set.empty system.Process.FileDescriptors with
            | Ok (registry, _) -> registry
            | Error error -> failwith $"drop failed: %O{error}"

        let reopened, registry =
            FileDescriptorRegistry.openFile (InodeNumber 1L) FileAccessMode.ReadOnly registry

        reopened |> shouldEqual fd
        withRegistry registry system

    let private pollDefects (system : UnixSystem<int, string>) : UnixSystemDefect<int> list =
        // `Is*` tests rather than a match: `UnixSystemDefect` has 64 cases, so a
        // match with two outcomes compiles to a 64-entry switch with two targets,
        // whose branch the x64 JIT of runtimes 10.0.0 to 10.0.11 inverts
        // (dotnet/runtime#131716).
        UnixSystem.checkInvariants system
        |> List.filter (fun defect ->
            defect.IsParkedPollDescriptorRebound
            || defect.IsParkedOnAbsentDescription
            || defect.IsParkedPollOnEventQueue
        )

    [<Test>]
    let ``a parked poll whose descriptor names something else is a defect`` () : unit =
        let watched = idOf listener idle

        // With a dup keeping the description alive, the number is rebound.
        let _, registry =
            match FileDescriptorRegistry.dup listener idle.Process.FileDescriptors with
            | Ok dup -> dup
            | Error error -> failwith $"dup failed: %O{error}"

        let _, parked = parks [ entry listener pollIn ] 10 (withRegistry registry idle)
        let rebound = forgeRebind listener parked

        pollDefects rebound
        |> shouldEqual
            [
                UnixSystemDefect.ParkedPollDescriptorRebound (task, listener, watched, Some (idOf listener rebound))
            ]

        let exn = Assert.Throws<exn> (fun () -> UnixPoll.finishPoll task rebound |> ignore)

        exn.Message |> shouldContainText "now names"

        // Without one, the description is gone altogether.
        let _, parked = parks [ entry listener pollIn ] 10 idle
        let destroyed = forgeRebind listener parked

        pollDefects destroyed
        |> shouldEqual [ UnixSystemDefect.ParkedOnAbsentDescription (task, watched) ]

        let exn =
            Assert.Throws<exn> (fun () -> UnixPoll.finishPoll task destroyed |> ignore)

        exn.Message |> shouldContainText "a park holds its descriptions"

    [<Test>]
    let ``a parked task cannot poll again, and only a parked poll can be finished`` () : unit =
        let _, parked = parks [ entry listener pollIn ] 10 idle

        let exn =
            Assert.Throws<exn> (fun () -> UnixPoll.poll task [ entry listener pollIn ] 0 parked |> ignore)

        exn.Message |> shouldContainText "is parked in"

        let exn = Assert.Throws<exn> (fun () -> UnixPoll.finishPoll task idle |> ignore)
        exn.Message |> shouldContainText "is not parked"

    [<Test>]
    let ``a task parked in a poll cannot be parked in another syscall`` () : unit =
        let _, parked = parks [ entry listener pollIn ] 10 idle

        for other in
            [
                ParkedSyscall.Flock
                    {
                        Requester = idOf listener idle
                        Mode = FlockMode.Shared
                    }
                ParkedSyscall.EpollWait
                    {
                        Epoll = idOf listener idle
                        MaxEvents = 1
                        Buffer = UserBuffer.Mapped
                        Deadline = None
                    }
            ] do
            let exn = Assert.Throws<exn> (fun () -> UnixWait.park task other parked |> ignore)

            exn.Message |> shouldContainText "without clearing the first"

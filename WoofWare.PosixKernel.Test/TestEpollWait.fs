namespace WoofWare.PosixKernel.Test

open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `UnixPoll.epollWait` and `UnixPoll.finishSocketWait`: the argument screens,
/// the timeout, the park and its finish, and which of several waiters on one
/// port an event wakes.
///
/// The facts these rows hold the library to were measured by
/// `docs/plans/2026-08-23-posix-kernel-extraction/epoll-wait.c` on Linux 6.18.5
/// aarch64 (2026-09-27): a wait that finds nothing returns 0 at its timeout and
/// never before it, and at once for a timeout of 0; every negative timeout is
/// infinite; an event ends the wait at once, and wins over an expired deadline
/// when both hold as the waiter runs; a negative `maxevents` is screened as 0
/// is; and each event wakes one waiter, the one that parked last.
///
/// The port holds one edge-triggered registration of a listening socket, which
/// presents nothing while its accept queue is empty and `IN|RDNORM` once it is
/// not. The queue and the port's ready list are set directly, so that a row can
/// make the registration deliverable, or stale, at an instant it chooses.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestEpollWait =

    let private task : int = 1
    let private nanosecondsPerMillisecond : int64 = 1_000_000L
    let private data : uint64 = 0x1234UL

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

    let private withTask (name : int) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        Tasks.ensure name system

    let private createPort (system : UnixSystem<int, string>) : int * UnixSystem<int, string> =
        match UnixPoll.epollCreate1 0 system with
        | Ok (Ok (fd, system)) -> fd, system
        | other -> failwith $"expected a port, got %A{other}"

    /// A Linux-flavoured system with tasks 1 to 6 registered, a listening socket
    /// with an empty accept queue, and a port holding one edge-triggered
    /// `EPOLLIN` registration of it: the listener's descriptor, and the port's.
    let private world : int * int * UnixSystem<int, string> =
        let system =
            (UnixSystem.initial<int, string> SimulatedUnixPlatform.linuxX64 UnixSystem.pipedStandardStreams 0 (CpuId 0)
             |> UnixBootImage.boot,
             [ 1..6 ])
            ||> List.fold (fun system name -> withTask name system)

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

        let listenerFd, registry =
            FileDescriptorRegistry.createSocket socketId system.Process.FileDescriptors

        let system =
            { withRegistry registry system with
                Machine =
                    { system.Machine with
                        Sockets = Map.add socketId socket system.Machine.Sockets
                        NextSocketId = SocketId (raw + 1L)
                    }
            }

        let portFd, system = createPort system

        let system =
            match
                UnixPoll.epollCtl
                    portFd
                    1
                    listenerFd
                    (EpollEventArgument.Readable (EpollEvents.In ||| EpollEvents.EdgeTriggered, data))
                    system
            with
            | Ok (EpollCtlAnswer.Changed, system) -> system
            | other -> failwith $"expected the registration to succeed, got %A{other}"

        listenerFd, portFd, system

    let private listener : int =
        let fd, _, _ = world
        fd

    let private port : int =
        let _, fd, _ = world
        fd

    let private idle : UnixSystem<int, string> =
        let _, _, system = world
        system

    let private portId : OpenFileDescriptionId = idOf port idle

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

    /// A connection arrives: the listener becomes ready, and its registration is
    /// signalled onto the port's ready list.
    let private signal (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        let key = listener, idOf listener system

        let system = withQueue [ ConnectionId 99L ] system

        let alreadyReady =
            match FileDescriptorRegistry.tryFindTarget port system.Process.FileDescriptors with
            | Some (OpenFileTarget.Epoll state) -> List.contains key state.Ready
            | other -> failwith $"expected the port, got %O{other}"

        if alreadyReady then
            system
        else
            withRegistry
                (FileDescriptorRegistry.appendSocketEventReady portId key system.Process.FileDescriptors)
                system

    /// The connection is taken by someone else: the listener's level drops, and
    /// the pending entry goes stale.
    let private unready : UnixSystem<int, string> -> UnixSystem<int, string> =
        withQueue []

    let private pendingEntries (system : UnixSystem<int, string>) : int =
        match FileDescriptorRegistry.tryFindTarget port system.Process.FileDescriptors with
        | Some (OpenFileTarget.Epoll state) -> List.length state.Ready
        | other -> failwith $"expected the port, got %O{other}"

    let private after (nanoseconds : int64) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        { system with
            Machine = UnixMachineState.advanceClock nanoseconds system.Machine
        }

    /// What the one registration reports when the listener is ready: `IN`, since
    /// `RDNORM` was not asked for.
    let private delivered : (uint64 * uint32) list = [ data, EpollEvents.In ]

    let private waits
        (milliseconds : int)
        (system : UnixSystem<int, string>)
        : Result<EpollWaitOutcome * UnixSystem<int, string>, EpollWaitRefusal>
        =
        UnixPoll.epollWait task port 8 UserBuffer.Mapped milliseconds system

    let private parks
        (milliseconds : int)
        (system : UnixSystem<int, string>)
        : WakeCondition * UnixSystem<int, string>
        =
        match waits milliseconds system with
        | Ok (EpollWaitOutcome.WouldBlock condition, parked) -> condition, parked
        | other -> failwith $"expected a park, got %A{other}"

    let private finishes (system : UnixSystem<int, string>) : (uint64 * uint32) list * UnixSystem<int, string> =
        match UnixPoll.finishSocketWait task system with
        | Ok (EpollWaitOutcome.Answered events, finished) -> events, finished
        | other -> failwith $"expected the wait to finish, got %A{other}"

    let private reparks (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        match UnixPoll.finishSocketWait task system with
        | Ok (EpollWaitOutcome.WouldBlock _, parked) -> parked
        | other -> failwith $"expected the wait to park again, got %A{other}"

    let private woken (system : UnixSystem<int, string>) : Set<WakePrimitive> option =
        match UnixWait.wakes (Set.singleton task) system with
        | [] -> None
        | [ woken, fired ] when woken = task -> Some fired
        | other -> failwith $"unexpected wake %A{other}"

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 300

    /// Positive timeouts, often small, where an off-by-one in the conversion to
    /// a deadline would show most.
    let private positiveTimeout : Gen<int> =
        Gen.oneof
            [
                Gen.choose (1, 10)
                Gen.choose (1, 100_000)
                Gen.constant System.Int32.MaxValue
            ]

    let private negativeTimeout : Gen<int> =
        Gen.oneof
            [
                Gen.elements [ -1 ; -2 ; -3 ; -1000 ; System.Int32.MinValue ]
                Gen.choose (System.Int32.MinValue, -1)
            ]

    let private anyTimeout : Gen<int> =
        Gen.oneof [ positiveTimeout ; negativeTimeout ; Gen.constant 0 ]

    let private start : Gen<int64> = Gen.choose64 (0L, 1_000_000_000_000L)

    // ------------------------------------------------------------------
    // The screens
    // ------------------------------------------------------------------

    /// An address every modelled architecture's screen rejects.
    let private wild : UserBuffer = UserBuffer.Unmapped System.UInt64.MaxValue

    [<Test>]
    let ``every count is screened by Linux's ladder: descriptor, count, buffer, kind`` () : unit =
        // The descriptors: stdin (not a port), the listener (a socket), the port, and one
        // that is not open.
        let descriptors = Gen.elements [ 0 ; listener ; port ; 99 ]

        let counts =
            Gen.oneof
                [
                    Gen.choose (-3, 3)
                    Gen.elements
                        [
                            System.Int32.MinValue
                            134217727
                            134217728
                            178956970
                            178956971
                            System.Int32.MaxValue
                        ]
                ]

        let buffers = Gen.elements [ UserBuffer.Mapped ; wild ]

        // Measured (`epoll-wait.c`, section G, and TestSocketWait's rows): EBADF for a
        // descriptor that is not open, then EINVAL for a count outside 1..EP_MAX_EVENTS
        // (178956970 on x86-64), then EFAULT for a range reaching the kernel half, then
        // EINVAL for anything but an epoll instance.
        let oracle (fd : int) (maxEvents : int) (buffer : UserBuffer) : UnixError option =
            if fd = 99 then
                Some UnixError.EBADF
            elif maxEvents <= 0 || maxEvents > 178956970 then
                Some UnixError.EINVAL
            elif buffer = wild then
                Some UnixError.EFAULT
            elif fd <> port then
                Some UnixError.EINVAL
            else
                None

        let property ((fd : int, maxEvents : int), (buffer : UserBuffer, milliseconds : int)) : unit =
            match oracle fd maxEvents buffer, UnixPoll.epollWait task fd maxEvents buffer milliseconds idle with
            | Some error, actual -> actual |> shouldEqual (Ok (EpollWaitOutcome.Failed error, idle))
            | None, Ok (EpollWaitOutcome.Failed error, _) ->
                failwith $"the ladder let the call through, and epollWait failed it with %O{error}"
            | None, Ok _ -> ()
            | other -> failwith $"unexpected pair %A{other}"

        Check.One (
            config,
            Prop.forAll (Arb.fromGen (Gen.zip (Gen.zip descriptors counts) (Gen.zip buffers anyTimeout))) property
        )

    [<Test>]
    let ``a negative count is screened as zero is: after the descriptor, before the buffer and the kind`` () : unit =
        // Measured (section G): EBADF for a bad fd whatever the buffer, and otherwise EINVAL,
        // even with a kernel-range buffer and on a descriptor that is not a port.
        let property (maxEvents : int, milliseconds : int) : unit =
            for fd, buffer, expected in
                [
                    99, UserBuffer.Mapped, UnixError.EBADF
                    99, wild, UnixError.EBADF
                    port, wild, UnixError.EINVAL
                    port, UserBuffer.Mapped, UnixError.EINVAL
                    listener, wild, UnixError.EINVAL
                    0, UserBuffer.Mapped, UnixError.EINVAL
                ] do
                UnixPoll.epollWait task fd maxEvents buffer milliseconds idle
                |> shouldEqual (Ok (EpollWaitOutcome.Failed expected, idle))

        let negativeCount =
            Gen.oneof
                [
                    Gen.elements [ -1 ; System.Int32.MinValue ]
                    Gen.choose (System.Int32.MinValue, -1)
                ]

        Check.One (config, Prop.forAll (Arb.fromGen (Gen.zip negativeCount anyTimeout)) property)

    [<Test>]
    let ``Darwin has no epoll, so every wait is refused`` () : unit =
        let darwin =
            UnixSystem.initial<int, string> SimulatedUnixPlatform.macOsArm64 UnixSystem.pipedStandardStreams 0 (CpuId 0)
            |> UnixBootImage.boot
            |> withTask task

        UnixPoll.epollWait task 3 8 UserBuffer.Mapped -1 darwin
        |> shouldEqual (Error (EpollWaitRefusal.UnmodelledFlavour SimulatedUnixFlavour.Darwin))

    // ------------------------------------------------------------------
    // Answering at once
    // ------------------------------------------------------------------

    [<Test>]
    let ``a deliverable event is answered at once, whatever the timeout`` () : unit =
        let property (milliseconds : int, now : int64) : unit =
            let ready = idle |> after now |> signal

            match waits milliseconds ready with
            | Ok (EpollWaitOutcome.Answered events, answered) ->
                events |> shouldEqual delivered
                UnixTaskTable.parkedFor task answered.Tasks |> shouldEqual None
                pendingEntries answered |> shouldEqual 0
            | other -> failwith $"expected the event, got %A{other}"

        Check.One (config, Prop.forAll (Arb.fromGen (Gen.zip anyTimeout start)) property)

    [<Test>]
    let ``a timeout of 0 with nothing deliverable answers no events, consuming the stale entry`` () : unit =
        let property (now : int64) : unit =
            let stale = idle |> after now |> signal |> unready
            pendingEntries stale |> shouldEqual 1

            match waits 0 stale with
            | Ok (EpollWaitOutcome.Answered [], answered) ->
                UnixTaskTable.parkedFor task answered.Tasks |> shouldEqual None
                // The walk consumed the stale entry even though it delivered nothing.
                pendingEntries answered |> shouldEqual 0
                answered |> shouldEqual (snd (SocketEventPort.drain portId 8 stale))
            | other -> failwith $"expected no events, got %A{other}"

        Check.One (config, Prop.forAll (Arb.fromGen start) property)

    // ------------------------------------------------------------------
    // Parking, and the deadline
    // ------------------------------------------------------------------

    [<Test>]
    let ``a positive timeout fires exactly at its deadline, and answers no events`` () : unit =
        let property (milliseconds : int, now : int64) : unit =
            let started = after now idle
            let condition, parked = parks milliseconds started
            let deadline = now + int64 milliseconds * nanosecondsPerMillisecond

            condition
            |> shouldEqual (
                Interruptible.condition (
                    WakeCondition.AnyOf (
                        WakeCondition.Primitive (WakePrimitive.SocketEventDeliverable portId),
                        [ WakeCondition.Primitive (WakePrimitive.DeadlinePassed deadline) ]
                    )
                )
            )

            UnixWait.deadlines (Set.singleton task) parked |> shouldEqual [ deadline ]

            // One nanosecond short: nothing wakes it, and a finish attempted anyway sleeps
            // again on the same deadline.
            let short = after (deadline - 1L - now) parked
            woken short |> shouldEqual None
            let reparked = reparks short
            UnixWait.deadlines (Set.singleton task) reparked |> shouldEqual [ deadline ]

            // At the deadline: woken by it, and finished with no events.
            let expired = after (deadline - now) parked

            woken expired
            |> shouldEqual (Some (Set.singleton (WakePrimitive.DeadlinePassed deadline)))

            let events, finished = finishes expired
            events |> shouldEqual []
            UnixTaskTable.parkedFor task finished.Tasks |> shouldEqual None

        let bounded =
            Gen.zip positiveTimeout start
            |> Gen.filter (fun (ms, now) -> now + int64 ms * nanosecondsPerMillisecond < System.Int64.MaxValue)

        Check.One (config, Prop.forAll (Arb.fromGen bounded) property)

    [<Test>]
    let ``a negative timeout waits for an event or a signal, and no deadline`` () : unit =
        let property (milliseconds : int, now : int64) : unit =
            let condition, parked = parks milliseconds (after now idle)

            condition
            |> shouldEqual (
                Interruptible.condition (WakeCondition.Primitive (WakePrimitive.SocketEventDeliverable portId))
            )

            UnixWait.deadlines (Set.singleton task) parked |> shouldEqual []

            match UnixTaskTable.parkedFor task parked.Tasks with
            | Some (ParkedSyscall.SocketWait wait) ->
                wait
                |> shouldEqual
                    {
                        Port = portId
                        MaxEvents = 8
                        Buffer = UserBuffer.Mapped
                        Deadline = None
                    }
            | other -> failwith $"expected a socket wait, got %A{other}"

            // However long it has slept.
            woken (after 1_000_000_000_000L parked) |> shouldEqual None

        Check.One (config, Prop.forAll (Arb.fromGen (Gen.zip negativeTimeout start)) property)

    [<Test>]
    let ``an event before the deadline ends the wait with it`` () : unit =
        let property (milliseconds : int, elapsed : int64) : unit =
            let _, parked = parks milliseconds idle
            let deadline = int64 milliseconds * nanosecondsPerMillisecond
            let ready = parked |> after (min elapsed (deadline - 1L)) |> signal

            woken ready
            |> shouldEqual (Some (Set.singleton (WakePrimitive.SocketEventDeliverable portId)))

            let events, finished = finishes ready
            events |> shouldEqual delivered
            UnixTaskTable.parkedFor task finished.Tasks |> shouldEqual None

        Check.One (config, Prop.forAll (Arb.fromGen (Gen.zip positiveTimeout start)) property)

    [<Test>]
    let ``an event and an expired deadline together answer the event`` () : unit =
        // Measured (section E): 40 trials of 40 reported the event rather than 0.
        let property (milliseconds : int, overshoot : int64) : unit =
            let _, parked = parks milliseconds idle
            let deadline = int64 milliseconds * nanosecondsPerMillisecond
            let both = parked |> after (deadline + overshoot) |> signal

            woken both
            |> shouldEqual (
                Some (
                    Set.ofList
                        [
                            WakePrimitive.SocketEventDeliverable portId
                            WakePrimitive.DeadlinePassed deadline
                        ]
                )
            )

            let events, _ = finishes both
            events |> shouldEqual delivered

        Check.One (
            config,
            Prop.forAll (Arb.fromGen (Gen.zip (Gen.choose (1, 100_000)) (Gen.choose64 (0L, 1_000_000L)))) property
        )

    [<Test>]
    let ``a wake whose event goes stale parks again on the same deadline, behind every other park`` () : unit =
        let _, parked = parks 5 idle
        let ordinalBefore = (UnixTaskTable.parkOf task parked.Tasks).Value.Ordinal
        let ready = parked |> after 1L |> signal

        woken ready
        |> shouldEqual (Some (Set.singleton (WakePrimitive.SocketEventDeliverable portId)))

        // Someone else takes the connection before the woken waiter runs.
        let stale = unready ready
        let reparked = reparks stale

        pendingEntries reparked |> shouldEqual 0

        UnixWait.deadlines (Set.singleton task) reparked
        |> shouldEqual [ 5L * nanosecondsPerMillisecond ]

        let ordinalAfter = (UnixTaskTable.parkOf task reparked.Tasks).Value.Ordinal
        ordinalAfter |> shouldBeGreaterThan ordinalBefore

    [<Test>]
    let ``a deadline past the clock's range is refused, unless an event is deliverable`` () : unit =
        let late = after (System.Int64.MaxValue - 5L) idle

        waits 1 late
        |> shouldEqual (Error (EpollWaitRefusal.DeadlineBeyondClock (System.Int64.MaxValue - 5L, 1)))

        match waits 1 (signal late) with
        | Ok (EpollWaitOutcome.Answered events, _) -> events |> shouldEqual delivered
        | other -> failwith $"expected the event, got %A{other}"

    [<Test>]
    let ``a parked task cannot wait again, and an unparked one cannot finish`` () : unit =
        let _, parked = parks -1 idle

        let exn = Assert.Throws<exn> (fun () -> waits -1 parked |> ignore)
        exn.Message |> shouldContainText "is parked"

        let exn =
            Assert.Throws<exn> (fun () -> UnixPoll.finishSocketWait task idle |> ignore)

        exn.Message |> shouldContainText "is not parked"

    // ------------------------------------------------------------------
    // The copy-out
    // ------------------------------------------------------------------

    /// Buffers every screen passes, but that this library cannot copy events out to, with
    /// the refusal each earns: an address in user space that names no storage, and real
    /// memory whose bytes the caller cannot produce.
    let private uncopyable : (UserBuffer * EpollWaitRefusal) list =
        [
            UserBuffer.Unmapped 0x1000UL, EpollWaitRefusal.UnmeasuredCopyOutFault portId
            UserBuffer.Opaque, EpollWaitRefusal.Buffer BufferRefusal.OpaqueAtTransfer
        ]

    [<Test>]
    let ``a delivery to a buffer the library cannot copy to is refused, and a wait that delivers nothing is not``
        ()
        : unit
        =
        let property (milliseconds : int, now : int64) : unit =
            let started = after now idle

            for buffer, refusal in uncopyable do
                UnixPoll.epollWait task port 8 buffer milliseconds (signal started)
                |> shouldEqual (Error refusal)

                // Nothing to copy: the buffer is never looked at past the screen.
                match UnixPoll.epollWait task port 8 buffer milliseconds started with
                | Ok (EpollWaitOutcome.Answered [], _) -> milliseconds |> shouldEqual 0
                | Ok (EpollWaitOutcome.WouldBlock _, _) -> milliseconds |> shouldNotEqual 0
                | other -> failwith $"expected no delivery, got %A{other}"

        let bounded = Gen.zip anyTimeout (Gen.choose64 (0L, 1_000_000L))
        Check.One (config, Prop.forAll (Arb.fromGen bounded) property)

    [<Test>]
    let ``a parked wait finishes by copying to the buffer it was entered with`` () : unit =
        for buffer, refusal in uncopyable do
            let _, parked =
                match UnixPoll.epollWait task port 8 buffer 5 idle with
                | Ok (EpollWaitOutcome.WouldBlock condition, parked) -> condition, parked
                | other -> failwith $"expected a park, got %A{other}"

            match UnixTaskTable.parkedFor task parked.Tasks with
            | Some (ParkedSyscall.SocketWait wait) -> wait.Buffer |> shouldEqual buffer
            | other -> failwith $"expected a socket wait, got %A{other}"

            UnixPoll.finishSocketWait task (signal parked) |> shouldEqual (Error refusal)

            // Timing out copies nothing, so it answers.
            match UnixPoll.finishSocketWait task (after (5L * nanosecondsPerMillisecond) parked) with
            | Ok (EpollWaitOutcome.Answered [], finished) ->
                UnixTaskTable.parkedFor task finished.Tasks |> shouldEqual None
            | other -> failwith $"expected the wait to time out, got %A{other}"

    // ------------------------------------------------------------------
    // Several waiters on one port
    // ------------------------------------------------------------------

    /// One park the property below makes: `waiter` waits on port number `port`
    /// of the generated set, until `deadline` if it has one.
    type private GeneratedPark =
        {
            Waiter : int
            Port : int
            Deadline : int64 option
        }

    [<Test>]
    let ``each event wakes the waiter that parked last, unless a woken waiter of its port has yet to finish``
        ()
        : unit
        =
        // Up to three ports, each with or without a deliverable event; tasks parking on them
        // in a generated order, some more than once; a clock; and which parked tasks the
        // client holds asleep. The oracle states the rule per task, where `UnixWait.wakes`
        // groups by port.
        let gen =
            gen {
                let! portCount = Gen.choose (1, 3)
                let! pending = Gen.listOfLength portCount (ArbMap.defaults |> ArbMap.generate<bool>)

                let park =
                    gen {
                        let! waiter = Gen.choose (1, 6)
                        let! port = Gen.choose (0, portCount - 1)
                        let! deadline = Gen.optionOf (Gen.choose64 (0L, 100L))

                        return
                            {
                                Waiter = waiter
                                Port = port
                                Deadline = deadline
                            }
                    }

                let! parks = Gen.listOf park |> Gen.map (List.truncate 12)
                let! clock = Gen.choose64 (0L, 100L)
                let! asleepMask = Gen.listOfLength 6 (ArbMap.defaults |> ArbMap.generate<bool>)
                return pending, parks, clock, asleepMask
            }

        let property (pending : bool list, parks : GeneratedPark list, clock : int64, asleepMask : bool list) : unit =
            // The ports, every one of them registering stdin, which presents HUP whatever it is
            // asked, so that a pending entry for it is always deliverable.
            let stdinKey = 0, idOf 0 idle

            let ports, system =
                ((idle, []), pending)
                ||> List.fold (fun (system, ports) isPending ->
                    let fd, system = createPort system
                    let id = idOf fd system

                    let registry =
                        FileDescriptorRegistry.addEpollRegistration
                            id
                            stdinKey
                            {
                                Events = EpollEvents.EdgeTriggered ||| EpollEvents.Err ||| EpollEvents.Hup
                                Data = uint64 fd
                                RegisteredAt = 0L
                            }
                            system.Process.FileDescriptors

                    let registry =
                        if isPending then
                            FileDescriptorRegistry.appendSocketEventReady id stdinKey registry
                        else
                            registry

                    withRegistry registry system, ports @ [ id ]
                )
                |> fun (system, ports) -> ports, system

            let system =
                (system, parks)
                ||> List.fold (fun system park ->
                    UnixWait.park
                        park.Waiter
                        (ParkedSyscall.SocketWait
                            {
                                Port = ports.[park.Port]
                                MaxEvents = 1
                                Buffer = UserBuffer.Mapped
                                Deadline = park.Deadline
                            })
                        system
                )
                |> after clock

            let parkedTasks =
                system.Tasks
                |> Map.toList
                |> List.choose (fun (name, state) ->
                    match state.Parked with
                    | Some {
                               Syscall = ParkedSyscall.SocketWait wait
                               Ordinal = ordinal
                           } -> Some (name, wait, ordinal)
                    | _ -> None
                )

            let asleep =
                parkedTasks
                |> List.map (fun (name, _, _) -> name)
                |> List.filter (fun name -> asleepMask.[name - 1])
                |> Set.ofList

            let isPending (portId : OpenFileDescriptionId) : bool =
                pending.[List.findIndex ((=) portId) ports]

            let finishingOn (portId : OpenFileDescriptionId) : bool =
                parkedTasks
                |> List.exists (fun (name, wait, _) -> not (Set.contains name asleep) && wait.Port = portId)

            let lastAsleepOn (portId : OpenFileDescriptionId) : int =
                parkedTasks
                |> List.filter (fun (name, wait, _) -> Set.contains name asleep && wait.Port = portId)
                |> List.maxBy (fun (_, _, ordinal) -> ordinal)
                |> fun (name, _, _) -> name

            let expected =
                parkedTasks
                |> List.filter (fun (name, _, _) -> Set.contains name asleep)
                |> List.sortBy (fun (_, _, ordinal) -> ordinal)
                |> List.choose (fun (name, wait, _) ->
                    let expired =
                        match wait.Deadline with
                        | Some deadline when deadline <= clock -> Some (WakePrimitive.DeadlinePassed deadline)
                        | _ -> None

                    let event =
                        if isPending wait.Port then
                            Some (WakePrimitive.SocketEventDeliverable wait.Port)
                        else
                            None

                    let chosen =
                        event.IsSome && not (finishingOn wait.Port) && lastAsleepOn wait.Port = name

                    if expired.IsSome || chosen then
                        Some (name, Set.ofList (Option.toList expired @ Option.toList event))
                    else
                        None
                )

            UnixWait.wakes asleep system |> shouldEqual expected

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 1000, Prop.forAll (Arb.fromGen gen) property)

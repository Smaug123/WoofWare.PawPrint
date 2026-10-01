namespace WoofWare.PosixKernel.Test

open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// A blocking `accept(2)` on a listener with nothing queued: the park, its
/// finish, which of several accepters a connection wakes, and what `close`
/// does to a listener an accept sleeps on.
///
/// The facts these rows hold the library to were measured by
/// `docs/plans/2026-08-23-posix-kernel-extraction/blocking-accept.c` on Linux
/// 6.18.5 aarch64 and Darwin 27.0.0 arm64 (2026-09-27): a connection wakes
/// exactly one accepter, the one that parked first, and one that accepts again
/// waits behind the others; making the listener non-blocking does not wake a
/// sleeping accept, and Darwin's accepted socket then inherits the flag; and a
/// close of a descriptor onto the listener leaves the wait alone on Linux
/// unless it is the last, while on Darwin closing the one the wait was entered
/// through ends it.
///
/// Every system here is built through the syscalls themselves (`socket`,
/// `bind`, `listen`, `connect`), so a queued connection is one `connect` put
/// there.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestBlockingAccept =

    let private platforms : SimulatedUnixPlatform list =
        [ SimulatedUnixPlatform.linuxX64 ; SimulatedUnixPlatform.macOsArm64 ]

    let private inetFamily : int option =
        Some SimulatedUnixPlatform.internetAddressFamily

    let private loopback (port : uint16) : InternetEndpoint =
        InternetEndpoint.ofParts InternetEndpoint.LoopbackAddress port

    let private idOf (fd : int) (system : UnixSystem<int, string>) : OpenFileDescriptionId =
        match FileDescriptorRegistry.tryFindWithId fd system.Process.FileDescriptors with
        | Some (id, _) -> id
        | None -> failwith $"fd %d{fd} names no description"

    /// A new blocking stream socket bound to loopback at `port` and listening,
    /// and its descriptor.
    let private listenerAt (port : uint16) (system : UnixSystem<int, string>) : int * UnixSystem<int, string> =
        let fd, system =
            NewSocket.create SocketDomain.Inet SocketKind.Stream SocketProtocol.Tcp system

        let system =
            match UnixSocket.bind fd UserBuffer.Mapped 16 inetFamily (Some (loopback port)) system with
            | Ok (BindAnswer.Bound _, system) -> system
            | other -> failwith $"binding the listener at port %d{port}: %A{other}"

        match UnixSocket.listen fd 8 system with
        | Ok (ListenAnswer.Listening _, system) -> fd, system
        | other -> failwith $"listening at port %d{port}: %A{other}"

    /// A system on `platform` with tasks 1 to 6, and a blocking listener at
    /// port 5000: its descriptor.
    let private world (platform : SimulatedUnixPlatform) : int * UnixSystem<int, string> =
        let system =
            (UnixSystem.initial<int, string> platform UnixSystem.pipedStandardStreams 0 (CpuId 0), [ 1..6 ])
            ||> List.fold (fun system name -> Tasks.ensure name system)

        listenerAt 5000us system

    /// A new client socket, connected to loopback at `port`.
    let private connectTo (port : uint16) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        let fd, system =
            NewSocket.create SocketDomain.Inet SocketKind.Stream SocketProtocol.Tcp system

        match UnixConnection.connect fd UserBuffer.Mapped 16 inetFamily (Some (loopback port)) system with
        | Ok (ConnectOutcome.Completed, system) -> system
        | other -> failwith $"connecting to port %d{port}: %A{other}"

    let private queueOf (fd : int) (system : UnixSystem<int, string>) : ConnectionId list =
        match FileDescriptorRegistry.tryFindTarget fd system.Process.FileDescriptors with
        | Some (OpenFileTarget.Socket socketId) ->
            match (UnixMachineState.socket socketId system.Machine).Phase with
            | SocketPhase.Listening listenState -> listenState.Queue
            | phase -> failwith $"fd %d{fd} is %A{phase}, not listening"
        | other -> failwith $"fd %d{fd} names %A{other}, not a socket"

    let private dupOf (fd : int) (system : UnixSystem<int, string>) : int * UnixSystem<int, string> =
        match UnixDescriptor.dup fd system with
        | SyscallAnswer.Completed newFd, system -> int newFd, system
        | other, _ -> failwith $"dup of %d{fd} answered %A{other}"

    let private setNonBlocking (fd : int) (value : bool) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        UnixSocket.setNonBlocking fd value system |> snd

    /// `task` parks in an accept through `fd`, whose queue must be empty.
    let private parkIn (task : int) (fd : int) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        match UnixConnection.accept task fd UserBuffer.Mapped 16 system with
        | Ok (AcceptOutcome.WouldBlock _, system) -> system
        | other -> failwith $"expected task %d{task} to park, got %A{other}"

    /// `task` finishes its accept, which must hand a connection over.
    let private finishWithConnection (task : int) (system : UnixSystem<int, string>) : int * UnixSystem<int, string> =
        match UnixConnection.finishAccept task system with
        | Ok (AcceptOutcome.Accepted (fd, _, _), system) -> fd, system
        | other -> failwith $"expected task %d{task}'s accept to finish with a connection, got %A{other}"

    let private awake (tasks : int list) (system : UnixSystem<int, string>) : int list =
        UnixWait.wakes (Set.ofList tasks) system |> List.map fst

    let private userBuffers : Gen<UserBuffer> =
        Gen.oneof
            [
                Gen.constant UserBuffer.Mapped
                Gen.constant UserBuffer.Opaque
                Gen.constant UserBuffer.Addressless
                Gen.choose (0, 0x10000) |> Gen.map (uint64 >> UserBuffer.Unmapped)
            ]

    let private platformGen : Gen<SimulatedUnixPlatform> = Gen.elements platforms

    // ------------------------------------------------------------------
    // The park
    // ------------------------------------------------------------------

    /// The park records the listener's description, whichever descriptor the
    /// call came through, and the destination and length it was entered with;
    /// nothing else about the system changes but the park counter. The
    /// destination is not screened before the call sleeps, so every kind parks.
    [<Test>]
    let ``a blocking listener with nothing queued parks the caller, and changes nothing else`` () : unit =
        let gen =
            gen {
                let! platform = platformGen
                let! destination = userBuffers
                let! declaredLength = Gen.choose (0, 64)
                let! throughDup = ArbMap.defaults |> ArbMap.generate<bool>
                let! task = Gen.choose (1, 6)
                return platform, destination, declaredLength, throughDup, task
            }

        let property
            (
                platform : SimulatedUnixPlatform,
                destination : UserBuffer,
                declaredLength : int,
                throughDup : bool,
                task : int
            )
            : unit
            =
            let fd, system = world platform

            let fd, system = if throughDup then dupOf fd system else fd, system

            let listener = idOf fd system

            match UnixConnection.accept task fd destination declaredLength system with
            | Ok (AcceptOutcome.WouldBlock condition, parked) ->
                condition
                |> shouldEqual (WakeCondition.Primitive (WakePrimitive.AcceptQueueNonEmpty listener))

                UnixTaskTable.parkOf task parked.Tasks
                |> shouldEqual (
                    Some
                        {
                            Syscall =
                                ParkedSyscall.Accept
                                    {
                                        Listener = listener
                                        Destination = destination
                                        DeclaredLength = declaredLength
                                    }
                            Ordinal = system.Machine.NextParkOrdinal
                        }
                )

                { parked with
                    Tasks = system.Tasks
                    Machine =
                        { parked.Machine with
                            NextParkOrdinal = system.Machine.NextParkOrdinal
                        }
                }
                |> shouldEqual system

                UnixSystem.checkInvariants parked |> shouldEqual []
            | other -> failwith $"expected the accept to park, got %A{other}"

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 200, Prop.forAll (Arb.fromGen gen) property)

    /// The wait holds exactly while the listener has something queued, and a
    /// real `connect` is what puts it there.
    [<TestCaseSource(nameof platforms)>]
    let ``the wait is satisfied exactly while a connection is queued`` (platform : SimulatedUnixPlatform) : unit =
        let fd, system = world platform
        let listener = idOf fd system
        let condition = WakeCondition.Primitive (WakePrimitive.AcceptQueueNonEmpty listener)
        let system = parkIn 1 fd system

        WakeCondition.satisfied condition system |> shouldEqual Set.empty
        UnixWait.wakes (Set.singleton 1) system |> shouldEqual []

        let system = connectTo 5000us system

        WakeCondition.satisfied condition system
        |> shouldEqual (Set.singleton (WakePrimitive.AcceptQueueNonEmpty listener))

        UnixWait.wakes (Set.singleton 1) system
        |> shouldEqual [ 1, Set.singleton (WakePrimitive.AcceptQueueNonEmpty listener) ]

        UnixWait.deadlines (Set.singleton 1) system |> shouldEqual []

    [<TestCaseSource(nameof platforms)>]
    let ``a parked task cannot accept again, and an unparked one cannot finish``
        (platform : SimulatedUnixPlatform)
        : unit
        =
        let fd, system = world platform
        let parked = parkIn 1 fd system

        Assert.Throws<exn> (fun () -> UnixConnection.accept 1 fd UserBuffer.Mapped 16 parked |> ignore<_>)
        |> ignore<exn>

        Assert.Throws<exn> (fun () -> UnixConnection.finishAccept 2 parked |> ignore<_>)
        |> ignore<exn>

    // ------------------------------------------------------------------
    // The finish
    // ------------------------------------------------------------------

    /// What happens to the listener between the park and the finish.
    [<RequireQualifiedAccess>]
    type private MidWait =
        /// A new client connects.
        | Connect
        /// The listener's description is made blocking or non-blocking.
        | SetNonBlocking of bool
        /// Another task takes the oldest queued connection, if there is one.
        | Steal

    /// The reference: a parked accept that finds a connection answers exactly
    /// what an accept made at that moment through the same description, with
    /// the parked call's destination and length, would -- the listening
    /// description's `O_NONBLOCK` included, as it stands then. One that finds
    /// the queue empty parks again on the same record, behind every other park,
    /// whatever the description's flag says.
    [<Test>]
    let ``finishing a parked accept answers what an accept made at that moment would`` () : unit =
        let midWait : Gen<MidWait> =
            Gen.oneof
                [
                    Gen.constant MidWait.Connect
                    ArbMap.defaults |> ArbMap.generate<bool> |> Gen.map MidWait.SetNonBlocking
                    Gen.constant MidWait.Steal
                ]

        let gen =
            gen {
                let! platform = platformGen
                let! destination = userBuffers
                let! declaredLength = Gen.choose (0, 64)
                let! actions = Gen.listOf midWait |> Gen.map (List.truncate 6)
                return platform, destination, declaredLength, actions
            }

        let property
            (platform : SimulatedUnixPlatform, destination : UserBuffer, declaredLength : int, actions : MidWait list)
            : unit
            =
            let fd, system = world platform
            let listener = idOf fd system

            let system =
                match UnixConnection.accept 1 fd destination declaredLength system with
                | Ok (AcceptOutcome.WouldBlock _, system) -> system
                | other -> failwith $"expected the accept to park, got %A{other}"

            let parkedRecord = UnixTaskTable.parkedFor 1 system.Tasks

            let system =
                (system, actions)
                ||> List.fold (fun system action ->
                    match action with
                    | MidWait.Connect -> connectTo 5000us system
                    | MidWait.SetNonBlocking value -> setNonBlocking fd value system
                    | MidWait.Steal ->
                        if List.isEmpty (queueOf fd system) then
                            system
                        else
                            match UnixConnection.accept 2 fd UserBuffer.Mapped 16 system with
                            | Ok (AcceptOutcome.Accepted _, system) -> system
                            | other -> failwith $"expected the steal to succeed, got %A{other}"
                )

            let finished = UnixConnection.finishAccept 1 system

            if List.isEmpty (queueOf fd system) then
                match finished with
                | Ok (AcceptOutcome.WouldBlock condition, parkedAgain) ->
                    condition
                    |> shouldEqual (WakeCondition.Primitive (WakePrimitive.AcceptQueueNonEmpty listener))

                    UnixTaskTable.parkOf 1 parkedAgain.Tasks
                    |> shouldEqual (
                        parkedRecord
                        |> Option.map (fun syscall ->
                            {
                                Syscall = syscall
                                Ordinal = system.Machine.NextParkOrdinal
                            }
                        )
                    )

                    UnixSystem.checkInvariants parkedAgain |> shouldEqual []
                | other -> failwith $"expected the accept to park again, got %A{other}"
            else
                let oracle =
                    UnixConnection.accept
                        1
                        fd
                        destination
                        declaredLength
                        { system with
                            Tasks = UnixTaskTable.unpark 1 system.Tasks
                        }

                finished |> shouldEqual oracle

                match finished with
                | Ok (_, finished) -> UnixSystem.checkInvariants finished |> shouldEqual []
                | Error _ -> ()

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 500, Prop.forAll (Arb.fromGen gen) property)

    /// Measured (`blocking-accept.c`, section F): setting `O_NONBLOCK` on the
    /// listener while an accept sleeps does not wake it, and when a connection
    /// then arrives, Darwin's accepted socket carries the flag and Linux's does
    /// not.
    [<TestCaseSource(nameof platforms)>]
    let ``a listener made non-blocking under a sleeping accept does not wake it, and Darwin's accepted socket inherits the flag``
        (platform : SimulatedUnixPlatform)
        : unit
        =
        let fd, system = world platform
        let system = parkIn 1 fd system |> setNonBlocking fd true

        UnixWait.wakes (Set.singleton 1) system |> shouldEqual []

        let accepted, system = connectTo 5000us system |> finishWithConnection 1

        let expected =
            match SimulatedUnixPlatform.flavour platform with
            | SimulatedUnixFlavour.Linux -> false
            | SimulatedUnixFlavour.Darwin -> true

        UnixSocket.isNonBlocking accepted system |> shouldEqual (Some expected)

    // ------------------------------------------------------------------
    // Several accepters
    // ------------------------------------------------------------------

    /// Measured (`blocking-accept.c`, section B1): three accepters parked in
    /// any order return one per connection, in the order they parked.
    [<TestCaseSource(nameof platforms)>]
    let ``accepters wake in the order they parked, one per connection`` (platform : SimulatedUnixPlatform) : unit =
        let fd, system = world platform
        let system = system |> parkIn 3 fd |> parkIn 1 fd |> parkIn 2 fd
        let system = connectTo 5000us system

        awake [ 1 ; 2 ; 3 ] system |> shouldEqual [ 3 ]
        // Woken and not yet finished, 3 stands for the connection: nobody else wakes.
        awake [ 1 ; 2 ] system |> shouldEqual []

        let _, system = finishWithConnection 3 system
        awake [ 1 ; 2 ] system |> shouldEqual []

        let system = connectTo 5000us system
        awake [ 1 ; 2 ] system |> shouldEqual [ 1 ]
        let _, system = finishWithConnection 1 system

        let system = connectTo 5000us system
        awake [ 2 ] system |> shouldEqual [ 2 ]

    /// Measured (`blocking-accept.c`, section B2): of two threads that each
    /// accept again as soon as they return, the returns alternate, so an
    /// accepter that accepts again waits behind the other.
    [<TestCaseSource(nameof platforms)>]
    let ``an accepter that accepts again waits behind the others`` (platform : SimulatedUnixPlatform) : unit =
        let fd, system = world platform
        let system = system |> parkIn 1 fd |> parkIn 2 fd |> connectTo 5000us

        awake [ 1 ; 2 ] system |> shouldEqual [ 1 ]

        let _, system = finishWithConnection 1 system
        let system = system |> parkIn 1 fd |> connectTo 5000us

        awake [ 1 ; 2 ] system |> shouldEqual [ 2 ]

    /// A woken accepter that finds the connection gone -- another caller took
    /// it through a non-blocking accept -- parks again, and so goes behind the
    /// accepter that was waiting with it.
    [<TestCaseSource(nameof platforms)>]
    let ``a woken accepter that finds the queue empty parks again behind the others``
        (platform : SimulatedUnixPlatform)
        : unit
        =
        let fd, system = world platform
        let system = system |> parkIn 1 fd |> parkIn 2 fd |> connectTo 5000us

        awake [ 1 ; 2 ] system |> shouldEqual [ 1 ]

        let system = setNonBlocking fd true system

        let system =
            match UnixConnection.accept 3 fd UserBuffer.Mapped 16 system with
            | Ok (AcceptOutcome.Accepted _, system) -> system
            | other -> failwith $"expected task 3 to take the connection, got %A{other}"

        let system =
            match UnixConnection.finishAccept 1 system with
            | Ok (AcceptOutcome.WouldBlock _, system) -> system
            | other -> failwith $"expected task 1 to park again, got %A{other}"

        let system = connectTo 5000us system
        awake [ 1 ; 2 ] system |> shouldEqual [ 2 ]

    /// Measured (`blocking-accept.c`, section B4): a connection to a listener
    /// one thread accepts on and another polls wakes both, since a poll waits
    /// non-exclusively. `poll(2)` is modelled for Linux only.
    [<Test>]
    let ``a connection wakes an accepter and a poller of the same listener`` () : unit =
        let fd, system = world SimulatedUnixPlatform.linuxX64
        let system = parkIn 1 fd system

        let system =
            match
                UnixPoll.poll
                    2
                    [
                        {
                            Fd = fd
                            Events = 0x0001s
                        }
                    ]
                    -1
                    system
            with
            | Ok (PollOutcome.WouldBlock _, system) -> system
            | other -> failwith $"expected the poll to park, got %A{other}"

        let system = connectTo 5000us system
        awake [ 1 ; 2 ] system |> shouldEqual [ 1 ; 2 ]

    /// One park the property below makes: `waiter` accepts on listener number
    /// `listener` of the generated set.
    type private GeneratedPark =
        {
            Waiter : int
            Listener : int
        }

    [<Test>]
    let ``a connection wakes the accepter that parked first, unless a woken accepter of its listener has yet to finish``
        ()
        : unit
        =
        // Up to three listeners, each with or without a queued connection; tasks parking on
        // them in a generated order, some more than once; and which parked tasks the client
        // holds asleep. The oracle states the rule per task, where `UnixWait.wakes` groups by
        // listener.
        let gen =
            gen {
                let! platform = platformGen
                let! listenerCount = Gen.choose (1, 3)
                let! queued = Gen.listOfLength listenerCount (ArbMap.defaults |> ArbMap.generate<bool>)

                let park =
                    gen {
                        let! waiter = Gen.choose (1, 6)
                        let! listener = Gen.choose (0, listenerCount - 1)

                        return
                            {
                                Waiter = waiter
                                Listener = listener
                            }
                    }

                let! parks = Gen.listOf park |> Gen.map (List.truncate 12)
                let! asleepMask = Gen.listOfLength 6 (ArbMap.defaults |> ArbMap.generate<bool>)
                return platform, queued, parks, asleepMask
            }

        let property
            (platform : SimulatedUnixPlatform, queued : bool list, parks : GeneratedPark list, asleepMask : bool list)
            : unit
            =
            let _, system = world platform

            let listeners, system =
                ((system, []), List.indexed queued)
                ||> List.fold (fun (system, listeners) (index, isQueued) ->
                    let port = 6000us + uint16 index
                    let fd, system = listenerAt port system

                    let system = if isQueued then connectTo port system else system

                    system, listeners @ [ idOf fd system ]
                )
                |> fun (system, listeners) -> listeners, system

            let system =
                (system, parks)
                ||> List.fold (fun system park ->
                    UnixWait.park
                        park.Waiter
                        (ParkedSyscall.Accept
                            {
                                Listener = listeners.[park.Listener]
                                Destination = UserBuffer.Mapped
                                DeclaredLength = 16
                            })
                        system
                )

            let parkedTasks =
                system.Tasks
                |> Map.toList
                |> List.choose (fun (name, state) ->
                    match state.Parked with
                    | Some {
                               Syscall = ParkedSyscall.Accept accept
                               Ordinal = ordinal
                           } -> Some (name, accept.Listener, ordinal)
                    | _ -> None
                )

            let asleep =
                parkedTasks
                |> List.map (fun (name, _, _) -> name)
                |> List.filter (fun name -> asleepMask.[name - 1])
                |> Set.ofList

            let isQueued (listener : OpenFileDescriptionId) : bool =
                queued.[List.findIndex ((=) listener) listeners]

            let finishingOn (listener : OpenFileDescriptionId) : bool =
                parkedTasks
                |> List.exists (fun (name, on, _) -> not (Set.contains name asleep) && on = listener)

            let firstAsleepOn (listener : OpenFileDescriptionId) : int =
                parkedTasks
                |> List.filter (fun (name, on, _) -> Set.contains name asleep && on = listener)
                |> List.minBy (fun (_, _, ordinal) -> ordinal)
                |> fun (name, _, _) -> name

            let expected =
                parkedTasks
                |> List.filter (fun (name, _, _) -> Set.contains name asleep)
                |> List.sortBy (fun (_, _, ordinal) -> ordinal)
                |> List.choose (fun (name, listener, _) ->
                    if isQueued listener && not (finishingOn listener) && firstAsleepOn listener = name then
                        Some (name, Set.singleton (WakePrimitive.AcceptQueueNonEmpty listener))
                    else
                        None
                )

            UnixWait.wakes asleep system |> shouldEqual expected
            UnixSystem.checkInvariants system |> shouldEqual []

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 1000, Prop.forAll (Arb.fromGen gen) property)

    // ------------------------------------------------------------------
    // Closing the listener
    // ------------------------------------------------------------------

    /// Measured (`blocking-accept.c`, section C1): on Linux the sleeping accept
    /// holds the file, so the last close leaves the socket listening under it.
    /// The table cannot keep a description with no descriptor, so the close is
    /// refused.
    [<Test>]
    let ``Linux: closing the last descriptor onto a listener an accept sleeps on is refused`` () : unit =
        let fd, system = world SimulatedUnixPlatform.linuxX64
        let listener = idOf fd system
        let system = parkIn 1 fd system

        match UnixDescriptor.close fd system with
        | Error refusal ->
            refusal
            |> shouldEqual (CloseRefusal.LinuxLastListenerDescriptorWithAccepter (listener, 1))
        | Ok (answer, _) -> failwith $"expected the close to be refused, got %A{answer}"

    /// Measured (`blocking-accept.c`, sections C2 and C3): on Linux, closing
    /// either of two descriptors onto the listener -- the one the accept came
    /// through, or another -- leaves the accept waiting, and a connection then
    /// completes it.
    [<Test>]
    let ``Linux: closing one of two descriptors onto the listener leaves the accept waiting`` () : unit =
        for closeEntered in [ true ; false ] do
            let fd, system = world SimulatedUnixPlatform.linuxX64
            let other, system = dupOf fd system
            let system = parkIn 1 fd system

            let system =
                match UnixDescriptor.close (if closeEntered then fd else other) system with
                | Ok (SyscallAnswer.Completed 0L, system) -> system
                | other -> failwith $"expected the close to succeed, got %A{other}"

            UnixWait.wakes (Set.singleton 1) system |> shouldEqual []
            UnixSystem.checkInvariants system |> shouldEqual []

            let system = connectTo 5000us system
            awake [ 1 ] system |> shouldEqual [ 1 ]
            finishWithConnection 1 system |> ignore<_>

    /// Measured (`blocking-accept.c`, sections C1 and C2): on Darwin, closing
    /// the descriptor the accept came through ends it at once with
    /// ECONNABORTED, even while a `dup` keeps the listener open. Closing another
    /// descriptor did not (C3), but the park does not record which descriptor
    /// the call came through, so every close onto the listener is refused.
    [<Test>]
    let ``Darwin: closing any descriptor onto a listener an accept sleeps on is refused`` () : unit =
        for closeEntered in [ true ; false ] do
            let fd, system = world SimulatedUnixPlatform.macOsArm64
            let listener = idOf fd system
            let other, system = dupOf fd system
            let system = parkIn 1 fd system

            match UnixDescriptor.close (if closeEntered then fd else other) system with
            | Error refusal ->
                refusal
                |> shouldEqual (CloseRefusal.DarwinListenerDescriptorWithAccepter (listener, 1))
            | Ok (answer, _) -> failwith $"expected the close to be refused, got %A{answer}"

    // ------------------------------------------------------------------
    // What the park may name
    // ------------------------------------------------------------------

    /// A park names a listening socket; `accept` parks on nothing else, so a
    /// park on anything else was recorded by hand, and `WakeCondition.satisfied`
    /// would crash on it.
    [<TestCaseSource(nameof platforms)>]
    let ``a park on anything but a listening socket is a defect`` (platform : SimulatedUnixPlatform) : unit =
        let _, system = world platform

        let idle, system =
            NewSocket.create SocketDomain.Inet SocketKind.Stream SocketProtocol.Tcp system

        let parkedOn (listener : OpenFileDescriptionId) : UnixSystem<int, string> =
            UnixWait.park
                1
                (ParkedSyscall.Accept
                    {
                        Listener = listener
                        Destination = UserBuffer.Mapped
                        DeclaredLength = 16
                    })
                system

        for description in [ idOf idle system ; idOf 0 system ] do
            UnixSystem.checkInvariants (parkedOn description)
            |> shouldEqual [ UnixSystemDefect.ParkedAcceptOnNonListener (1, description) ]

        let absent = OpenFileDescriptionId 1_000_000L

        UnixSystem.checkInvariants (parkedOn absent)
        |> shouldEqual [ UnixSystemDefect.ParkedOnAbsentDescription (1, absent) ]

    /// `SO_RCVTIMEO` bounds a blocking accept on Linux, answering EAGAIN at the
    /// timeout (`blocking-accept.c`, section E1; Darwin's accept ignores it).
    /// The park has no deadline because this option cannot be set. The numbers
    /// are each flavour's `<sys/socket.h>`, as the probe printed them.
    [<Test>]
    let ``SO_RCVTIMEO, which would bound the wait on Linux, cannot be set`` () : unit =
        for platform, level, option in
            [
                SimulatedUnixPlatform.linuxX64, 1, 20
                SimulatedUnixPlatform.macOsArm64, 0xffff, 0x1006
            ] do
            SimulatedUnixPlatform.socketOptionLevel platform |> shouldEqual level
            let fd, system = world platform

            let socketId =
                match FileDescriptorRegistry.tryFindTarget fd system.Process.FileDescriptors with
                | Some (OpenFileTarget.Socket socketId) -> socketId
                | other -> failwith $"expected a socket, got %A{other}"

            UnixSocket.admitSetSockOpt fd level option UserBuffer.Mapped 16u system
            |> shouldEqual (Error (SocketOptionRefusal.UnmodelledOption (socketId, level, option)))

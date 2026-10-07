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
/// close of a descriptor onto the listener leaves the wait alone on Linux, the
/// last one included, the listener going as the accept returns
/// (`open-file-references.c` section A), while on Darwin closing the one the
/// wait was entered through ends it.
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
        match FileDescriptorRegistry.tryFindWithId fd (UnixSystemState.fileDescriptors system) with
        | Some (id, _) -> id
        | None -> failwith $"fd %d{fd} names no description"

    /// A new blocking stream socket bound to loopback at `port` and listening,
    /// and its descriptor.
    let private listenerAt (port : uint16) (system : UnixSystem<int, string>) : int * UnixSystem<int, string> =
        let fd, system =
            NewSocket.create SocketDomain.Inet SocketKind.Stream SocketProtocol.Tcp system

        let system =
            match
                CopyIn.bind fd UserBuffer.Mapped 16u (CopyIn.inet (UnixSystem.platform system) (loopback port)) system
            with
            | Ok (BindAnswer.Bound _, system) -> system
            | other -> failwith $"binding the listener at port %d{port}: %A{other}"

        match UnixSocket.listen fd 8 system with
        | Ok (ListenAnswer.Listening _, system) -> fd, system
        | other -> failwith $"listening at port %d{port}: %A{other}"

    /// A system on `platform` with tasks 1 to 6, and a blocking listener at
    /// port 5000: its descriptor.
    let private world (platform : SimulatedUnixPlatform) : int * UnixSystem<int, string> =
        let system =
            (UnixSystem.initial<int, string> platform UnixSystem.pipedStandardStreams 0 (CpuId 0)
             |> UnixBootImage.boot,
             [ 1..6 ])
            ||> List.fold (fun system name -> Tasks.ensure name system)

        listenerAt 5000us system

    /// A new client socket, connected to loopback at `port`.
    let private connectTo (port : uint16) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        let fd, system =
            NewSocket.create SocketDomain.Inet SocketKind.Stream SocketProtocol.Tcp system

        match
            CopyIn.connect fd UserBuffer.Mapped 16u (CopyIn.inet (UnixSystem.platform system) (loopback port)) system
        with
        | Ok (ConnectOutcome.Completed, system) -> system
        | other -> failwith $"connecting to port %d{port}: %A{other}"

    let private queueOf (fd : int) (system : UnixSystem<int, string>) : ConnectionId list =
        match FileDescriptorRegistry.tryFindTarget fd (UnixSystemState.fileDescriptors system) with
        | Some (OpenFileTarget.Socket socketId) ->
            match (UnixMachineState.socket socketId system.Machine).Phase with
            | SocketPhase.Listening listenState -> listenState.Queue
            | phase -> failwith $"fd %d{fd} is %A{phase}, not listening"
        | other -> failwith $"fd %d{fd} names %A{other}, not a socket"

    let private dupOf (fd : int) (system : UnixSystem<int, string>) : int * UnixSystem<int, string> =
        match Answered.dup fd system with
        | SyscallAnswer.Completed newFd, system -> int newFd, system
        | other, _ -> failwith $"dup of %d{fd} answered %A{other}"

    let private setNonBlocking (fd : int) (value : bool) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        UnixDescriptor.setNonBlocking fd value system |> snd

    /// `task` parks in an accept through `fd`, whose queue must be empty.
    let private parkIn (task : int) (fd : int) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        match UnixConnection.accept task fd UserBuffer.Mapped 16u system with
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
                let! declaredLength = Gen.choose (0, 64) |> Gen.map uint32
                let! throughDup = ArbMap.defaults |> ArbMap.generate<bool>
                let! task = Gen.choose (1, 6)
                return platform, destination, declaredLength, throughDup, task
            }

        let property
            (
                platform : SimulatedUnixPlatform,
                destination : UserBuffer,
                declaredLength : uint32,
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
                |> shouldEqual (
                    Interruptible.closable [ WakeCondition.Primitive (WakePrimitive.AcceptQueueNonEmpty listener) ]
                )

                UnixTaskTable.parkOf task parked.Tasks
                |> shouldEqual (
                    Some
                        {
                            Syscall =
                                ParkedSyscall.Accept
                                    {
                                        Listener = SleepTarget.Waiting (listener, fd)
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

        WakeCondition.satisfied 1 condition system |> shouldEqual Set.empty
        UnixWait.wakes (Set.singleton 1) system |> shouldEqual []

        let system = connectTo 5000us system

        WakeCondition.satisfied 1 condition system
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

        Assert.Throws<exn> (fun () -> UnixConnection.accept 1 fd UserBuffer.Mapped 16u parked |> ignore<_>)
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
                let! declaredLength = Gen.choose (0, 64) |> Gen.map uint32
                let! actions = Gen.listOf midWait |> Gen.map (List.truncate 6)
                return platform, destination, declaredLength, actions
            }

        let property
            (platform : SimulatedUnixPlatform, destination : UserBuffer, declaredLength : uint32, actions : MidWait list)
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
                            match UnixConnection.accept 2 fd UserBuffer.Mapped 16u system with
                            | Ok (AcceptOutcome.Accepted _, system) -> system
                            | other -> failwith $"expected the steal to succeed, got %A{other}"
                )

            let finished = UnixConnection.finishAccept 1 system

            if List.isEmpty (queueOf fd system) then
                match finished with
                | Ok (AcceptOutcome.WouldBlock condition, parkedAgain) ->
                    condition
                    |> shouldEqual (
                        Interruptible.closable [ WakeCondition.Primitive (WakePrimitive.AcceptQueueNonEmpty listener) ]
                    )

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

        UnixDescriptor.isNonBlocking accepted system |> shouldEqual (Some expected)

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
            match UnixConnection.accept 3 fd UserBuffer.Mapped 16u system with
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

                    system, listeners @ [ idOf fd system, fd ]
                )
                |> fun (system, listeners) -> listeners, system

            let system =
                (system, parks)
                ||> List.fold (fun system park ->
                    UnixWait.park
                        park.Waiter
                        (ParkedSyscall.Accept
                            {
                                Listener = SleepTarget.Waiting listeners.[park.Listener]
                                Destination = UserBuffer.Mapped
                                DeclaredLength = 16u
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
                           } -> Some (name, SleepTarget.description accept.Listener |> Option.get, ordinal)
                    | _ -> None
                )

            let asleep =
                parkedTasks
                |> List.map (fun (name, _, _) -> name)
                |> List.filter (fun name -> asleepMask.[name - 1])
                |> Set.ofList

            let isQueued (listener : OpenFileDescriptionId) : bool =
                queued.[List.findIndex (fst >> (=) listener) listeners]

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

    /// A new client socket's connect to loopback at `port`: what it answered.
    let private connectAnswer (port : uint16) (system : UnixSystem<int, string>) : ConnectOutcome =
        let fd, system =
            NewSocket.create SocketDomain.Inet SocketKind.Stream SocketProtocol.Tcp system

        match
            CopyIn.connect fd UserBuffer.Mapped 16u (CopyIn.inet (UnixSystem.platform system) (loopback port)) system
        with
        | Ok (outcome, _) -> outcome
        | Error refusal -> failwith $"connecting to port %d{port}: %A{refusal}"

    /// Measured (`open-file-references.c` section A1, and `blocking-accept.c`
    /// section C1): on Linux the sleeping accept holds the file, so the last
    /// close wakes nothing and the socket goes on listening under it; a connect
    /// then completes the accept, and the listener closes as the accept
    /// returns, so the next connect is refused.
    [<Test>]
    let ``Linux: the last close under a sleeping accept leaves the socket listening until the accept returns``
        ()
        : unit
        =
        let fd, system = world SimulatedUnixPlatform.linuxX64
        let listener = idOf fd system
        let system = parkIn 1 fd system

        let system =
            match UnixDescriptor.close fd system with
            | Ok (SyscallAnswer.Completed 0L, system) -> system
            | other -> failwith $"expected the close to succeed, got %A{other}"

        awake [ 1 ] system |> shouldEqual []
        UnixSystem.checkInvariants system |> shouldEqual []

        OpenFileTable.descriptions system.Machine.OpenFiles
        |> Map.containsKey listener
        |> shouldEqual true

        let system = connectTo 5000us system
        awake [ 1 ] system |> shouldEqual [ 1 ]
        let accepted, system = finishWithConnection 1 system

        OpenFileTable.descriptions system.Machine.OpenFiles
        |> Map.containsKey listener
        |> shouldEqual false

        FileDescriptorRegistry.tryFind accepted (UnixSystemState.fileDescriptors system)
        |> Option.isSome
        |> shouldEqual true

        UnixSystem.checkInvariants system |> shouldEqual []

        connectAnswer 5000us system
        |> shouldEqual (ConnectOutcome.Failed UnixError.ECONNREFUSED)

    /// The control for the row above (`open-file-references.c` section A2):
    /// with a `dup` kept, the listener outlives the accept, and the next
    /// connect is queued.
    [<Test>]
    let ``Linux: with a dup kept, the listener outlives the accept that slept on it`` () : unit =
        let fd, system = world SimulatedUnixPlatform.linuxX64
        let listener = idOf fd system
        let _, system = dupOf fd system
        let system = parkIn 1 fd system

        let system =
            match UnixDescriptor.close fd system with
            | Ok (SyscallAnswer.Completed 0L, system) -> system
            | other -> failwith $"expected the close to succeed, got %A{other}"

        let system = connectTo 5000us system
        let _, system = finishWithConnection 1 system

        OpenFileTable.descriptions system.Machine.OpenFiles
        |> Map.containsKey listener
        |> shouldEqual true

        connectAnswer 5000us system |> shouldEqual ConnectOutcome.Completed

    /// What the accept's return would do to a second queued connection, once
    /// nothing else holds the listener: a real kernel resets the client, in a
    /// state this kernel has not measured, so the finish refuses, as `close`
    /// refuses to destroy such a listener.
    [<Test>]
    let ``Linux: an accept whose return would reset a second queued connection is refused`` () : unit =
        let fd, system = world SimulatedUnixPlatform.linuxX64
        let system = parkIn 1 fd system

        let system =
            match UnixDescriptor.close fd system with
            | Ok (SyscallAnswer.Completed 0L, system) -> system
            | other -> failwith $"expected the close to succeed, got %A{other}"

        let system = connectTo 5000us system |> connectTo 5000us

        match UnixConnection.finishAccept 1 system with
        | Error (AcceptRefusal.Release (DescriptionReleaseRefusal.ListenerWouldResetUnacceptedClient _)) -> ()
        | other -> failwith $"expected the finish to be refused, got %A{other}"

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

    /// `system` with a caught `SIGUSR1` pending for `task`.
    let private signalled (task : int) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        { system with
            Process =
                { system.Process with
                    Signals =
                        system.Process.Signals
                        |> SignalState.setDisposition
                            Signal.SIGUSR1
                            (SignalDisposition.Catch (SignalCatch.ofHandler "h"))
                        |> SignalState.enqueue
                            {
                                Signal = Signal.SIGUSR1
                                Target = ValueSome task
                            }
                }
        }

    let private closed (fd : int) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        match UnixDescriptor.close fd system with
        | Ok (SyscallAnswer.Completed 0L, system) -> system
        | other -> failwith $"expected the close of fd %d{fd} to succeed, got %A{other}"

    let private finishAborted (task : int) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        match UnixConnection.finishAccept task system with
        | Ok (AcceptOutcome.Failed UnixError.ECONNABORTED, system) -> system
        | other -> failwith $"expected task %d{task}'s accept to answer ECONNABORTED, got %A{other}"

    let private acceptAnswer (fd : int) (system : UnixSystem<int, string>) =
        UnixConnection.accept 6 fd UserBuffer.Mapped 16u system

    /// Measured (`close-ends-call.c`, sections A1-A4, and `blocking-accept.c`
    /// section C): on Darwin, closing the descriptor an accept was made
    /// through ends at once, with ECONNABORTED, every accept asleep on the
    /// listener, through that descriptor or a `dup` of it. The calls hold
    /// nothing once the close returns: with no `dup` the listener is gone, and a
    /// connect is refused; with one, the port still listens, and a later accept
    /// takes the connection.
    [<Test>]
    let ``Darwin: closing the descriptor an accept was made through ends every accept on the listener`` () : unit =
        for keepDup in [ false ; true ] do
            let fd, system = world SimulatedUnixPlatform.macOsArm64
            let listener = idOf fd system
            let other, system = if keepDup then dupOf fd system else -1, system
            let system = parkIn 1 fd system |> parkIn 2 fd
            let system = if keepDup then parkIn 3 other system else system
            let parked = if keepDup then [ 1 ; 2 ; 3 ] else [ 1 ; 2 ]

            let system = closed fd system

            UnixSystem.checkInvariants system |> shouldEqual []
            awake parked system |> shouldEqual parked

            OpenFileTable.descriptions system.Machine.OpenFiles
            |> Map.containsKey listener
            |> shouldEqual keepDup

            let system =
                (system, parked) ||> List.fold (fun system task -> finishAborted task system)

            UnixSystem.checkInvariants system |> shouldEqual []

            if keepDup then
                let system = connectTo 5000us (setNonBlocking other true system)

                match acceptAnswer other system with
                | Ok (AcceptOutcome.Accepted _, _) -> ()
                | answer -> failwith $"expected the drained listener to hand over a queued connection, got %A{answer}"
            else
                connectAnswer 5000us system
                |> shouldEqual (ConnectOutcome.Failed UnixError.ECONNREFUSED)

    /// Measured (`blocking-accept.c`, section C3): closing a descriptor onto
    /// the listener that no accept was made through ends nothing, and a
    /// connection then completes the accept.
    [<Test>]
    let ``Darwin: closing another descriptor onto the listener leaves the accept waiting`` () : unit =
        let fd, system = world SimulatedUnixPlatform.macOsArm64
        let other, system = dupOf fd system
        let system = parkIn 1 fd system |> closed other

        awake [ 1 ] system |> shouldEqual []
        UnixSystem.checkInvariants system |> shouldEqual []

        let system = connectTo 5000us system
        awake [ 1 ] system |> shouldEqual [ 1 ]
        finishWithConnection 1 system |> ignore<_>

    /// Measured (`close-ends-call.c`, section A7): a listener a close has
    /// drained still hands a queued connection to an accept (A7c), and answers
    /// a non-blocking one EAGAIN when none is queued (A7a), but a blocking one
    /// that would sleep answers ECONNABORTED once anything wakes it, and of two
    /// such sleepers one connection wakes one (A7g), which this kernel's wake
    /// does not follow, so the sleep is refused.
    [<Test>]
    let ``Darwin: a drained listener hands over a queued connection and refuses a sleep`` () : unit =
        let fd, system = world SimulatedUnixPlatform.macOsArm64
        let other, system = dupOf fd system

        let socket =
            match FileDescriptorRegistry.tryFindTarget other (UnixSystemState.fileDescriptors system) with
            | Some (OpenFileTarget.Socket socketId) -> socketId
            | target -> failwith $"fd %d{other} names %A{target}, not a socket"

        let system = parkIn 1 fd system |> closed fd |> finishAborted 1

        match acceptAnswer other system with
        | Error (AcceptRefusal.DarwinDrainedListener refused) -> refused |> shouldEqual socket
        | answer -> failwith $"expected a sleep on the drained listener to be refused, got %A{answer}"

        match acceptAnswer other (setNonBlocking other true system) with
        | Ok (AcceptOutcome.Failed UnixError.EAGAIN, _) -> ()
        | answer -> failwith $"expected EAGAIN, got %A{answer}"

        match acceptAnswer other (connectTo 5000us system) with
        | Ok (AcceptOutcome.Accepted _, after) -> UnixSystem.checkInvariants after |> shouldEqual []
        | answer -> failwith $"expected the queued connection, got %A{answer}"

        // A second `listen` leaves it drained (section A7h).
        let relistened =
            match UnixSocket.listen other 16 system with
            | Ok (ListenAnswer.Listening _, system) -> system
            | other -> failwith $"listening again: %A{other}"

        match acceptAnswer other relistened with
        | Error (AcceptRefusal.DarwinDrainedListener _) -> ()
        | answer -> failwith $"expected a sleep on the drained listener to be refused, got %A{answer}"

    /// Measured (`close-ends-call.c`, sections A7b and A7d): an accept woken on
    /// a drained listener answers ECONNABORTED whatever woke it, leaving a
    /// connection queued. So an accept the close ended answers it beside a
    /// connection that had already woken it, and beside a signal pending before
    /// the close or after.
    [<Test>]
    let ``Darwin: an accept a close has ended answers ECONNABORTED beside a connection or a signal`` () : unit =
        // A connection queued, which woke the accept before the close.
        let fd, system = world SimulatedUnixPlatform.macOsArm64
        let other, system = dupOf fd system
        let system = parkIn 1 fd system |> connectTo 5000us
        awake [ 1 ] system |> shouldEqual [ 1 ]
        let system = closed fd system |> finishAborted 1
        queueOf other system |> List.length |> shouldEqual 1
        UnixSystem.checkInvariants system |> shouldEqual []

        // A signal pending before the close, and one after.
        for before in [ true ; false ] do
            let fd, system = world SimulatedUnixPlatform.macOsArm64
            let system = parkIn 1 fd system
            let system = if before then signalled 1 system else system
            let system = closed fd system
            let system = if before then system else signalled 1 system
            awake [ 1 ] system |> shouldEqual [ 1 ]
            finishAborted 1 system |> UnixSystem.checkInvariants |> shouldEqual []

    /// The close records its answer in the park, which forgets the descriptor
    /// number: a later descriptor under the same number, onto another
    /// listener, belongs to the calls made through it alone.
    [<Test>]
    let ``Darwin: a descriptor number reused after the close ends only what was made through it`` () : unit =
        let fd, system = world SimulatedUnixPlatform.macOsArm64
        let _, system = dupOf fd system
        let system = parkIn 1 fd system |> closed fd

        let second, system = listenerAt 5001us system
        second |> shouldEqual fd
        let system = parkIn 2 second system
        awake [ 2 ] system |> shouldEqual []

        let system = closed second system
        awake [ 1 ; 2 ] system |> shouldEqual [ 1 ; 2 ]
        let system = finishAborted 1 system |> finishAborted 2
        UnixSystem.checkInvariants system |> shouldEqual []

    // ------------------------------------------------------------------
    // Accepts, closes and dups, against a reference
    // ------------------------------------------------------------------

    /// One step of the closing property. A task is named by its index, modulo
    /// their number, among those that can take the step: those making no call,
    /// for an accept; those in one, for a signal; those woken, for a finish.
    [<RequireQualifiedAccess>]
    type private AcceptOp =
        | Accept of task : int * fd : int
        | Connect
        | Close of fd : int
        | Dup of fd : int
        | Signal of task : int
        | Wake
        | Finish of task : int

    type private AcceptPark =
        {
            /// The descriptor the accept was made through; `None` once a close
            /// has ended it, under Darwin.
            Through : int option
            Ordinal : int
            /// The client has woken it, and not yet finished it.
            Woken : bool
        }

    type private AcceptReference =
        {
            Linux : bool
            Restart : bool
            /// Open descriptor to whether it names the listener.
            Fds : Map<int, bool>
            Parks : Map<int, AcceptPark>
            NextOrdinal : int
            Signalled : Set<int>
            Queued : int
            Drained : bool
            /// Whether the listening socket still exists.
            Alive : bool
        }

    /// Whether something still holds the listener: a descriptor, or an accept
    /// that has not been ended (every accept, under Linux).
    let private listenerHeld (r : AcceptReference) : bool =
        (r.Fds |> Map.exists (fun _ listener -> listener))
        || (r.Parks |> Map.exists (fun _ park -> park.Through.IsSome))

    let private lowestFree (r : AcceptReference) : int =
        Seq.initInfinite id |> Seq.find (fun n -> not (Map.containsKey n r.Fds))

    let private acceptTasks : int list = [ 1 ; 2 ; 3 ; 4 ]

    let rec private returnToUser (task : int) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        match UnixSignal.onReturnToUser task system with
        | Ok (None, system) -> system
        | Ok (Some (SignalDelivery.RunHandlers frames), system) ->
            (system, frames)
            ||> List.fold (fun system frame -> UnixSignal.sigreturn task frame.Id system)
            |> returnToUser task
        | other -> failwith $"returning task %d{task} to user mode: %A{other}"

    /// `weights` are the frequencies of an accept, a connect, a close, a dup, a
    /// signal, a wake and a finish; descriptors are drawn from 3 to `highestFd`.
    let private acceptOpGenWeighted (highestFd : int) (weights : int * int * int * int * int * int * int) =
        let accepts, connects, closes, dups, signals, wakes, finishes = weights
        let task = Gen.elements acceptTasks
        let fd = Gen.choose (3, highestFd)

        Gen.frequency
            [
                accepts, Gen.map2 (fun t fd -> AcceptOp.Accept (t, fd)) task fd
                connects, Gen.constant AcceptOp.Connect
                closes, Gen.map AcceptOp.Close fd
                dups, Gen.map AcceptOp.Dup fd
                signals, Gen.map AcceptOp.Signal task
                wakes, Gen.constant AcceptOp.Wake
                finishes, Gen.map AcceptOp.Finish task
            ]

    let private acceptOpGen : Gen<AcceptOp> =
        acceptOpGenWeighted 8 (6, 4, 3, 3, 2, 6, 8)

    /// Weighted towards accepts asleep through several descriptors, and the
    /// closes that end them, which `acceptOpGen` reaches only now and then.
    let private closingAcceptOpGen : Gen<AcceptOp> =
        acceptOpGenWeighted 6 (8, 1, 4, 5, 1, 4, 5)

    /// Measured on Darwin (`close-ends-call.c`) and Linux (`blocking-accept.c`,
    /// `open-file-references.c`): several tasks accepting on one listener
    /// through several descriptors onto it, with connections, closes, `dup`s
    /// and signals between, against a reference that states each rule again.
    /// Under Linux no close ends an accept; under Darwin a close of the
    /// descriptor one was made through ends every accept on the listener and
    /// drains it, whatever descriptor number is reused afterwards.
    [<Test>]
    let ``accepts, closes and dups on one listener keep to the reference`` () : unit =
        let covered = System.Collections.Concurrent.ConcurrentDictionary<string, int> ()

        let cover (label : string) =
            covered.AddOrUpdate (label, 1, (fun _ n -> n + 1)) |> ignore

        let property (platform : SimulatedUnixPlatform, restart : bool, ops : AcceptOp list) : unit =
            let linux = SimulatedUnixPlatform.flavour platform = SimulatedUnixFlavour.Linux
            let flavourName = if linux then "Linux" else "Darwin"
            let listenerFd, system = world platform

            let mutable system =
                { system with
                    Process =
                        { system.Process with
                            Signals =
                                system.Process.Signals
                                |> SignalState.setDisposition
                                    Signal.SIGUSR1
                                    (SignalDisposition.Catch
                                        { SignalCatch.ofHandler "h" with
                                            Restart = restart
                                        })
                        }
                }

            let listenerId = idOf listenerFd system

            let mutable reference =
                {
                    Linux = linux
                    Restart = restart
                    Fds =
                        FileDescriptorRegistry.fds (UnixSystemState.fileDescriptors system)
                        |> Map.map (fun _ id -> id = listenerId)
                    Parks = Map.empty
                    NextOrdinal = 0
                    Signalled = Set.empty
                    Queued = 0
                    Drained = false
                    Alive = true
                }

            let answered (task : int) (r : AcceptReference) =
                { r with
                    Parks = Map.remove task r.Parks
                    Signalled = Set.remove task r.Signalled
                }

            // The accepted socket is closed at once: the reference follows the
            // listener's descriptors and the clients', and nothing else.
            let dropAccepted (fd : int) (system : UnixSystem<int, string>) =
                match UnixDescriptor.close fd system with
                | Ok (SyscallAnswer.Completed 0L, system) -> system
                | other -> failwith $"closing the accepted fd %d{fd}: %A{other}"

            for i, op in List.indexed ops do
                let pick (eligible : int -> bool) (index : int) : int option =
                    match List.filter eligible acceptTasks with
                    | [] -> None
                    | candidates -> Some candidates.[index % List.length candidates]

                let onListenerOrClosed (fd : int) =
                    match Map.tryFind fd reference.Fds with
                    | Some false -> false
                    | Some true
                    | None -> true

                let resolved =
                    match op with
                    | AcceptOp.Accept (index, fd) when onListenerOrClosed fd ->
                        pick (fun t -> not (Map.containsKey t reference.Parks)) index
                        |> Option.map (fun t -> AcceptOp.Accept (t, fd))
                    | AcceptOp.Close fd
                    | AcceptOp.Dup fd when onListenerOrClosed fd -> Some op
                    | AcceptOp.Accept _
                    | AcceptOp.Close _
                    | AcceptOp.Dup _ -> None
                    | AcceptOp.Connect -> if reference.Queued < 8 then Some op else None
                    | AcceptOp.Signal index ->
                        pick (fun t -> Map.containsKey t reference.Parks) index
                        |> Option.map AcceptOp.Signal
                    | AcceptOp.Finish index ->
                        pick (fun t -> Map.tryFind t reference.Parks |> Option.exists (fun p -> p.Woken)) index
                        |> Option.map AcceptOp.Finish
                    | AcceptOp.Wake -> Some op

                match resolved with
                | None -> ()
                | Some op ->

                let where = $"%O{platform}, restart %b{restart}, op %d{i} (%A{op})"

                match op with
                | AcceptOp.Accept (task, fd) ->
                    let answer = UnixConnection.accept task fd UserBuffer.Mapped 16u system

                    match Map.tryFind fd reference.Fds, answer with
                    | None, Ok (AcceptOutcome.Failed UnixError.EBADF, _) -> cover $"%s{flavourName} accept: EBADF"
                    | Some _, Ok (AcceptOutcome.Accepted (accepted, _, _), after) when reference.Queued > 0 ->
                        cover $"%s{flavourName} accept: a queued connection"
                        system <- dropAccepted accepted after

                        reference <-
                            { reference with
                                Queued = reference.Queued - 1
                            }
                    | Some _, Error (AcceptRefusal.DarwinDrainedListener _) when
                        reference.Drained && reference.Queued = 0
                        ->
                        cover "Darwin accept: refused, the listener drained"
                    | Some _, Ok (AcceptOutcome.WouldBlock _, after) when not reference.Drained && reference.Queued = 0 ->
                        cover $"%s{flavourName} accept: sleeps"
                        system <- after

                        reference <-
                            { reference with
                                Parks =
                                    Map.add
                                        task
                                        {
                                            Through = Some fd
                                            Ordinal = reference.NextOrdinal
                                            Woken = false
                                        }
                                        reference.Parks
                                NextOrdinal = reference.NextOrdinal + 1
                            }
                    | _, other -> failwith $"%s{where}: %A{other}"
                | AcceptOp.Connect ->
                    let client, created =
                        NewSocket.create SocketDomain.Inet SocketKind.Stream SocketProtocol.Tcp system

                    client |> shouldEqual (lowestFree reference)

                    match
                        CopyIn.connect
                            client
                            UserBuffer.Mapped
                            16u
                            (CopyIn.inet (UnixSystem.platform created) (loopback 5000us))
                            created
                    with
                    | Ok (ConnectOutcome.Completed, after) when reference.Alive ->
                        system <- after

                        reference <-
                            { reference with
                                Fds = Map.add client false reference.Fds
                                Queued = reference.Queued + 1
                            }
                    | Ok (ConnectOutcome.Failed UnixError.ECONNREFUSED, after) when not reference.Alive ->
                        cover $"%s{flavourName} connect: refused, the listener gone"
                        system <- after

                        reference <-
                            { reference with
                                Fds = Map.add client false reference.Fds
                            }
                    | other -> failwith $"%s{where}: %A{other}"
                | AcceptOp.Close fd ->
                    match Map.tryFind fd reference.Fds with
                    | None ->
                        match UnixDescriptor.close fd system with
                        | Ok (SyscallAnswer.Failed UnixError.EBADF, _) -> ()
                        | other -> failwith $"%s{where}: %A{other}"
                    | Some _ ->

                    let drains =
                        not linux
                        && reference.Parks |> Map.exists (fun _ park -> park.Through = Some fd)

                    let after =
                        { reference with
                            Fds = Map.remove fd reference.Fds
                            Drained = reference.Drained || drains
                            Parks =
                                if drains then
                                    reference.Parks
                                    |> Map.map (fun _ park ->
                                        { park with
                                            Through = None
                                        }
                                    )
                                else
                                    reference.Parks
                        }

                    let dies = reference.Alive && not (listenerHeld after)

                    match UnixDescriptor.close fd system with
                    | Error (CloseRefusal.Release (DescriptionReleaseRefusal.ListenerWouldResetUnacceptedClient _)) when
                        dies && reference.Queued > 0
                        ->
                        cover $"%s{flavourName} close: refused, a connection left queued"
                    | Ok (SyscallAnswer.Completed 0L, closed) when not (dies && reference.Queued > 0) ->
                        if drains then
                            cover "Darwin close: ends every accept on the listener"

                            if
                                reference.Parks
                                |> Map.exists (fun _ park -> park.Through.IsSome && park.Through <> Some fd)
                            then
                                cover "Darwin close: ends an accept made through another descriptor"

                        if
                            not linux
                            && reference.Parks |> Map.exists (fun _ park -> park.Through.IsNone)
                            && Map.containsKey fd reference.Fds
                            && reference.Fds.[fd]
                        then
                            cover "Darwin close: a descriptor onto the listener while an ended accept is unfinished"

                        system <- closed

                        reference <-
                            { after with
                                Alive = reference.Alive && not dies
                            }
                    | other -> failwith $"%s{where}: dies %b{dies}, %A{other}"
                | AcceptOp.Dup fd ->
                    let answer, after = Answered.dup fd system

                    match Map.tryFind fd reference.Fds with
                    | None -> answer |> shouldEqual (SyscallAnswer.Failed UnixError.EBADF)
                    | Some named ->
                        let lowest = lowestFree reference
                        answer |> shouldEqual (SyscallAnswer.Completed (int64 lowest))

                        if reference.Parks |> Map.exists (fun _ park -> park.Through.IsNone) then
                            cover $"%s{flavourName} dup: a number an ended accept was made through"

                        reference <-
                            { reference with
                                Fds = Map.add lowest named reference.Fds
                            }

                    system <- after
                | AcceptOp.Signal task ->
                    system <-
                        { system with
                            Process =
                                { system.Process with
                                    Signals =
                                        SignalState.enqueue
                                            {
                                                Signal = Signal.SIGUSR1
                                                Target = ValueSome task
                                            }
                                            system.Process.Signals
                                }
                        }

                    reference <-
                        { reference with
                            Signalled = Set.add task reference.Signalled
                        }
                | AcceptOp.Wake ->
                    let asleep =
                        reference.Parks |> Map.filter (fun _ park -> not park.Woken) |> Map.toList

                    let finishing =
                        reference.Parks |> Map.exists (fun _ park -> park.Woken && park.Through.IsSome)

                    let first =
                        if reference.Queued = 0 || finishing then
                            None
                        else
                            asleep
                            |> List.filter (fun (_, park) -> park.Through.IsSome)
                            |> List.sortBy (fun (_, park) -> park.Ordinal)
                            |> List.tryHead
                            |> Option.map fst

                    let expected =
                        asleep
                        |> List.filter (fun (task, park) ->
                            park.Through.IsNone
                            || Set.contains task reference.Signalled
                            || first = Some task
                        )
                        |> List.sortBy (fun (_, park) -> park.Ordinal)
                        |> List.map fst

                    let woken =
                        UnixWait.wakes (asleep |> List.map fst |> Set.ofList) system |> List.map fst

                    if woken <> expected then
                        failwith $"%s{where}: woke %A{woken}, expected %A{expected}"

                    reference <-
                        { reference with
                            Parks =
                                (reference.Parks, woken)
                                ||> List.fold (fun parks task ->
                                    Map.add
                                        task
                                        { parks.[task] with
                                            Woken = true
                                        }
                                        parks
                                )
                        }
                | AcceptOp.Finish task ->
                    let park = reference.Parks.[task]
                    let signalled = Set.contains task reference.Signalled
                    let answer = UnixConnection.finishAccept task system

                    let settle (r : AcceptReference) (after : UnixSystem<int, string>) =
                        let r = answered task r
                        let dies = r.Alive && not (listenerHeld r)

                        system <- returnToUser task after

                        reference <-
                            { r with
                                Alive = r.Alive && not dies
                            }

                    match park.Through, answer with
                    | None, Ok (AcceptOutcome.Failed UnixError.ECONNABORTED, after) ->
                        cover "Darwin finish: ECONNABORTED"

                        if reference.Queued > 0 then
                            cover "Darwin finish: ECONNABORTED with a connection queued"

                        if signalled then
                            cover "Darwin finish: ECONNABORTED with a signal pending"

                        settle reference after
                    | Some _, _ when reference.Queued > 0 ->
                        let leftHeld = listenerHeld (answered task reference)

                        match answer with
                        | Error (AcceptRefusal.Interruption _) when signalled && not linux ->
                            cover "Darwin finish: refused, a connection and a signal"
                        | Error (AcceptRefusal.Release (DescriptionReleaseRefusal.ListenerWouldResetUnacceptedClient _)) when
                            not leftHeld && reference.Queued > 1
                            ->
                            cover $"%s{flavourName} finish: refused, a connection left queued"
                        | Ok (AcceptOutcome.Accepted (accepted, _, _), after) when leftHeld || reference.Queued = 1 ->
                            cover $"%s{flavourName} finish: a connection"

                            settle
                                { reference with
                                    Queued = reference.Queued - 1
                                }
                                (dropAccepted accepted after)
                        | other -> failwith $"%s{where}: %A{other}"
                    | Some _, Ok (AcceptOutcome.Failed UnixError.EINTR, after) when signalled && not restart ->
                        settle reference after
                    | Some _, Ok (AcceptOutcome.Restarts, after) when signalled && restart -> settle reference after
                    | Some _, Ok (AcceptOutcome.WouldBlock _, after) when not signalled ->
                        system <- after

                        reference <-
                            { reference with
                                Parks =
                                    Map.add
                                        task
                                        { park with
                                            Woken = false
                                            Ordinal = reference.NextOrdinal
                                        }
                                        reference.Parks
                                NextOrdinal = reference.NextOrdinal + 1
                            }
                    | _, other -> failwith $"%s{where}: %A{other}"

                match UnixSystem.checkInvariants system with
                | [] -> ()
                | defects -> failwith $"%s{where}: %A{defects}"

                // Every park agrees with the reference's.
                for task in acceptTasks do
                    let expected =
                        Map.tryFind task reference.Parks |> Option.map (fun park -> park.Through)

                    let actual =
                        UnixTaskTable.parkedFor task system.Tasks
                        |> Option.map (fun parked ->
                            match parked with
                            | ParkedSyscall.Accept {
                                                       Listener = SleepTarget.Waiting (_, fd)
                                                   } -> Some fd
                            | ParkedSyscall.Accept {
                                                       Listener = SleepTarget.EndedByClose _
                                                   } -> None
                            | other -> failwith $"%s{where}: task %d{task} parked in %A{other}"
                        )

                    if expected <> actual then
                        failwith $"%s{where}: task %d{task} parked as %A{actual}, expected %A{expected}"

                let socket =
                    system.Machine.Sockets
                    |> Map.tryPick (fun _ socket ->
                        match socket.Phase with
                        | SocketPhase.Listening listenState -> Some listenState
                        | _ -> None
                    )

                match socket with
                | Some listenState when reference.Alive ->
                    (List.length listenState.Queue, listenState.Drained)
                    |> shouldEqual (reference.Queued, reference.Drained)
                | None when not reference.Alive -> ()
                | other -> failwith $"%s{where}: the listener is %A{other}, alive %b{reference.Alive}"

        let gen =
            gen {
                let! platform = platformGen
                let! restart = ArbMap.defaults |> ArbMap.generate<bool>
                let! length = Gen.choose (0, 60)
                let! ops = Gen.listOfLength length acceptOpGen
                return platform, restart, ops
            }

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 1000, Prop.forAll (Arb.fromGen gen) property)

        let closingGen =
            Gen.zip
                (ArbMap.defaults |> ArbMap.generate<bool>)
                (Gen.choose (0, 60)
                 |> Gen.bind (fun length -> Gen.listOfLength length closingAcceptOpGen))
            |> Gen.map (fun (restart, ops) -> SimulatedUnixPlatform.macOsArm64, restart, ops)

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 1000, Prop.forAll (Arb.fromGen closingGen) property)

        let required =
            [
                "Linux accept: sleeps"
                "Linux finish: a connection"
                "Linux close: refused, a connection left queued"
                "Linux finish: refused, a connection left queued"
                "Linux connect: refused, the listener gone"
                "Darwin accept: sleeps"
                "Darwin accept: a queued connection"
                "Darwin accept: refused, the listener drained"
                "Darwin close: ends every accept on the listener"
                "Darwin close: ends an accept made through another descriptor"
                "Darwin close: a descriptor onto the listener while an ended accept is unfinished"
                "Darwin close: refused, a connection left queued"
                "Darwin connect: refused, the listener gone"
                "Darwin dup: a number an ended accept was made through"
                "Darwin finish: ECONNABORTED"
                "Darwin finish: ECONNABORTED with a connection queued"
                "Darwin finish: ECONNABORTED with a signal pending"
                "Darwin finish: refused, a connection and a signal"
                "Darwin finish: a connection"
            ]

        let missing = required |> List.filter (fun label -> not (covered.ContainsKey label))

        if not (List.isEmpty missing) then
            failwith $"the property never reached %A{missing}; it reached %A{List.ofSeq covered.Keys |> List.sort}"

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

        let parkedOn (listener : OpenFileDescriptionId) (fd : int) : UnixSystem<int, string> =
            UnixWait.park
                1
                (ParkedSyscall.Accept
                    {
                        Listener = SleepTarget.Waiting (listener, fd)
                        Destination = UserBuffer.Mapped
                        DeclaredLength = 16u
                    })
                system

        for fd in [ idle ; 0 ] do
            let description = idOf fd system

            UnixSystem.checkInvariants (parkedOn description fd)
            |> shouldEqual [ UnixSystemDefect.ParkedAcceptOnNonListener (1, description) ]

        let absent = OpenFileDescriptionId 1_000_000L

        UnixSystem.checkInvariants (parkedOn absent 1_000)
        |> shouldEqual [ UnixSystemDefect.ParkedOnAbsentDescription (1, absent) ]

    /// `system` with the park of `task` passed through `rewrite`.
    let private reparked
        (task : int)
        (rewrite : ParkedAccept -> ParkedAccept)
        (system : UnixSystem<int, string>)
        : UnixSystem<int, string>
        =
        let state = UnixTaskTable.get task system.Tasks

        match state.Parked with
        | Some ({
                    Syscall = ParkedSyscall.Accept accept
                } as park) ->
            { system with
                Tasks =
                    Map.add
                        task
                        { state with
                            Parked =
                                Some
                                    { park with
                                        Syscall = ParkedSyscall.Accept (rewrite accept)
                                    }
                        }
                        system.Tasks
            }
        | other -> failwith $"task %d{task} is parked in %A{other}"

    /// `system` with the listener `fd` names marked drained, as only a Darwin
    /// close marks one.
    let private drainedByHand (fd : int) (system : UnixSystem<int, string>) : SocketId * UnixSystem<int, string> =
        match FileDescriptorRegistry.tryFindTarget fd (UnixSystemState.fileDescriptors system) with
        | Some (OpenFileTarget.Socket socketId) ->
            let socket = UnixMachineState.socket socketId system.Machine

            match socket.Phase with
            | SocketPhase.Listening listenState ->
                socketId,
                { system with
                    Machine =
                        { system.Machine with
                            Sockets =
                                Map.add
                                    socketId
                                    { socket with
                                        Phase =
                                            SocketPhase.Listening
                                                { listenState with
                                                    Drained = true
                                                }
                                    }
                                    system.Machine.Sockets
                        }
                }
            | phase -> failwith $"fd %d{fd} is %A{phase}"
        | other -> failwith $"fd %d{fd} names %A{other}"

    /// Under Darwin the descriptor a sleeping accept was made through names its
    /// listener while it sleeps, since a close of it ends the accept; under
    /// Linux the number is not consulted.
    [<TestCaseSource(nameof platforms)>]
    let ``an accept asleep through a descriptor that names something else is a Darwin defect``
        (platform : SimulatedUnixPlatform)
        : unit
        =
        let fd, system = world platform
        let listener = idOf fd system

        let system =
            parkIn 1 fd system
            |> reparked
                1
                (fun accept ->
                    { accept with
                        Listener = SleepTarget.Waiting (listener, 0)
                    }
                )

        UnixSystem.checkInvariants system
        |> shouldEqual (
            match SimulatedUnixPlatform.flavour platform with
            | SimulatedUnixFlavour.Linux -> []
            | SimulatedUnixFlavour.Darwin ->
                [
                    UnixSystemDefect.ParkedCallDescriptorRebound (1, 0, listener, Some (idOf 0 system))
                ]
        )

    /// Only Darwin's close ends an accept, or drains a listener; and no accept
    /// sleeps on a drained one.
    [<Test>]
    let ``an ended accept or a drained listener under Linux, and a sleep on a drained listener, are defects``
        ()
        : unit
        =
        let fd, system = world SimulatedUnixPlatform.linuxX64
        let socketId, drained = drainedByHand fd system

        UnixSystem.checkInvariants drained
        |> shouldEqual [ UnixSystemDefect.ListenerDrainedUnderLinux socketId ]

        let ended =
            parkIn 1 fd system
            |> reparked
                1
                (fun accept ->
                    { accept with
                        Listener = SleepTarget.EndedByClose socketId
                    }
                )

        UnixSystem.checkInvariants ended
        |> shouldEqual [ UnixSystemDefect.ParkedCallEndedByCloseUnderLinux 1 ]

        let fd, system = world SimulatedUnixPlatform.macOsArm64
        let listener = idOf fd system
        let _, asleepOnDrained = parkIn 1 fd system |> drainedByHand fd

        UnixSystem.checkInvariants asleepOnDrained
        |> shouldEqual [ UnixSystemDefect.ParkedAcceptOnDrainedListener (1, listener) ]

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
                match FileDescriptorRegistry.tryFindTarget fd (UnixSystemState.fileDescriptors system) with
                | Some (OpenFileTarget.Socket socketId) -> socketId
                | other -> failwith $"expected a socket, got %A{other}"

            UnixSocket.admitSetSockOpt fd level option UserBuffer.Mapped 16u system
            |> shouldEqual (Error (SocketOptionRefusal.UnmodelledOption (socketId, level, option)))

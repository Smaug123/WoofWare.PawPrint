namespace WoofWare.PosixKernel.Test

open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// The laws of the wake-condition algebra, checked against a reference oracle.
///
/// The oracle flattens a condition into the list of primitives it mentions with an
/// explicit work stack, and decides each primitive from a truth table this file states
/// by construction, where `WakeCondition.satisfied` recurses over the tree and asks the
/// kernel. The world is built so that every primitive's answer is known: two socket
/// event ports, which share one anonymous inode and so contend under `flock`, with an
/// exclusive lock held through the first; neither port has anything to deliver;
/// two kqueues, the first with a ready listener queued and the second drained;
/// the standard streams, whose readiness is the launch shape's; and two tasks,
/// of which only `signalled` has a caught signal pending.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestWakeCondition =

    let private propertyConfig : Config = Config.QuickThrowOnFailure.WithMaxTest 1000

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

    /// The world, and the two ports' descriptions: `locker` holds an exclusive lock and
    /// `blocked` contends with it.
    let private world : UnixSystem<int, string> * OpenFileDescriptionId * OpenFileDescriptionId =
        let system =
            UnixSystem.initial<int, string> SimulatedUnixPlatform.linuxX64 UnixSystem.pipedStandardStreams 0 (CpuId 0)
            |> UnixBootImage.boot

        let lockerFd, registry =
            FileDescriptorRegistry.createEpoll system.Process.FileDescriptors

        let blockedFd, registry = FileDescriptorRegistry.createEpoll registry

        // Two kqueues, for `KqueueDrained`'s two live answers: fds 5 and 6. The world is
        // Linux's, which has no kqueue; the primitive asks only of the description.
        let _, registry = FileDescriptorRegistry.createKqueue registry
        let drainedFd, registry = FileDescriptorRegistry.createKqueue registry

        let registry =
            match FileDescriptorRegistry.tryFindId drainedFd registry with
            | Some id -> FileDescriptorRegistry.drainKqueue id registry
            | None -> failwith "expected the kqueue's description"

        let registry =
            match FileDescriptorRegistry.flock lockerFd (FlockRequest.Acquire FlockMode.Exclusive) registry with
            | registry, None -> registry
            | _, Some error -> failwith $"expected the lock to be granted, got %O{error}"

        let system = withRegistry registry system |> Tasks.spawn 1

        // A listener holding a connection, registered for EVFILT_READ with the first
        // kqueue and queued there, for `KqueueEventDeliverable`'s true answer. The
        // filter reads only the socket's phase.
        let listenerFd, system =
            NewSocket.create SocketDomain.Inet SocketKind.Stream SocketProtocol.Tcp system

        let inet = Some SimulatedUnixPlatform.internetAddressFamily
        let loopback = InternetEndpoint.ofParts InternetEndpoint.LoopbackAddress 5000us

        let system =
            match UnixSocket.bind listenerFd UserBuffer.Mapped 16u inet (Some loopback) system with
            | Ok (BindAnswer.Bound _, system) -> system
            | other -> failwith $"binding the listener: %A{other}"

        let system =
            match UnixSocket.listen listenerFd 8 system with
            | Ok (ListenAnswer.Listening _, system) -> system
            | other -> failwith $"listening: %A{other}"

        let clientFd, system =
            NewSocket.create SocketDomain.Inet SocketKind.Stream SocketProtocol.Tcp system

        let system =
            match UnixConnection.connect clientFd UserBuffer.Mapped 16u inet (Some loopback) system with
            | Ok (ConnectOutcome.Completed, system) -> system
            | other -> failwith $"connecting: %A{other}"

        let system =
            let key = listenerFd, KqueueFilter.Read

            withRegistry
                (FileDescriptorRegistry.setKqueueState
                    (idOf 5 system)
                    {
                        Drained = false
                        Registrations =
                            Map.ofList
                                [
                                    key,
                                    {
                                        Clear = true
                                        Receipt = false
                                        UserData = 0UL
                                        RegisteredAt = 0L
                                    }
                                ]
                        Active = [ key ]
                    }
                    system.Process.FileDescriptors)
                system

        let system =
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
                                    Target = ValueSome 1
                                }
                    }
            }

        system, idOf lockerFd system, idOf blockedFd system

    let private system : UnixSystem<int, string> =
        let system, _, _ = world
        system

    let private locker : OpenFileDescriptionId =
        let _, locker, _ = world
        locker

    let private blocked : OpenFileDescriptionId =
        let _, _, blocked = world
        blocked

    /// The primitives whose answer does not depend on the clock, each with that answer.
    let private fixedTruths : (WakePrimitive * bool) list =
        [
            WakePrimitive.FlockGrantable (locker, FlockMode.Exclusive), true
            WakePrimitive.FlockGrantable (locker, FlockMode.Shared), true
            WakePrimitive.FlockGrantable (blocked, FlockMode.Exclusive), false
            WakePrimitive.FlockGrantable (blocked, FlockMode.Shared), false
            WakePrimitive.SocketEventDeliverable locker, false
            WakePrimitive.SocketEventDeliverable blocked, false
            // The launch shape's standard streams: stdin presents HUP alone,
            // stdout OUT and WRNORM.
            WakePrimitive.DescriptorReady (idOf 1 system, 0x0004u), true
            WakePrimitive.DescriptorReady (idOf 1 system, 0x0001u ||| 0x0008u ||| 0x0010u), false
            WakePrimitive.DescriptorReady (idOf 0 system, 0x0010u), true
            WakePrimitive.DescriptorReady (idOf 0 system, 0x0001u), false
            // Standard input is a pipe holding nothing whose writer has
            // closed; standard output one the client drains, so empty, with a
            // reader.
            WakePrimitive.PipeHasBytes (idOf 0 system), false
            WakePrimitive.PipeWriteEndClosed (idOf 0 system), true
            WakePrimitive.PipeHasRoom (idOf 1 system, 1, 0), true
            WakePrimitive.PipeHasRoom (idOf 1 system, 70000, 65536), true
            WakePrimitive.PipeReadEndClosed (idOf 1 system), false
            // Linux: never.
            WakePrimitive.PipeReadWhileNonBlocking (idOf 1 system, -1L), false
            WakePrimitive.KqueueDrained (idOf 5 system), false
            WakePrimitive.KqueueDrained (idOf 6 system), true
            WakePrimitive.KqueueEventDeliverable (idOf 5 system), true
            WakePrimitive.KqueueEventDeliverable (idOf 6 system), false
        ]

    let private at (clock : int64) : UnixSystem<int, string> =
        { system with
            Machine = UnixMachineState.advanceClock clock system.Machine
        }

    /// The task with a caught signal pending; the other, 0, has none.
    let private signalled : int = 1

    let private oracleHolds (waiter : int) (clock : int64) (primitive : WakePrimitive) : bool =
        match primitive with
        | WakePrimitive.DeadlinePassed deadline -> clock >= deadline
        | WakePrimitive.SignalDeliverable -> waiter = signalled
        | WakePrimitive.EndedByClose
        | WakePrimitive.FlockGrantable _
        | WakePrimitive.SocketEventDeliverable _
        | WakePrimitive.KqueueDrained _
        | WakePrimitive.KqueueEventDeliverable _
        | WakePrimitive.KqueuePollReportable
        | WakePrimitive.DescriptorReady _
        | WakePrimitive.AcceptQueueNonEmpty _
        | WakePrimitive.PipeHasBytes _
        | WakePrimitive.PipeWriteEndClosed _
        | WakePrimitive.PipeHasRoom _
        | WakePrimitive.PipeReadEndClosed _
        | WakePrimitive.PipeReadWhileNonBlocking _ ->
            match List.tryFind (fun (p, _) -> p = primitive) fixedTruths with
            | Some (_, truth) -> truth
            | None -> failwith $"the oracle's truth table has no row for %O{primitive}"

    /// Every primitive `condition` mentions, in order and with repeats, found with an
    /// explicit work stack rather than by recursion.
    let private flatten (condition : WakeCondition) : WakePrimitive list =
        let mutable stack = [ condition ]
        let found = ResizeArray<WakePrimitive> ()

        while not (List.isEmpty stack) do
            match stack with
            | [] -> ()
            | WakeCondition.Primitive primitive :: rest ->
                found.Add primitive
                stack <- rest
            | WakeCondition.AnyOf (first, others) :: rest -> stack <- first :: others @ rest

        List.ofSeq found

    let private oracleSatisfied (waiter : int) (clock : int64) (condition : WakeCondition) : Set<WakePrimitive> =
        flatten condition |> List.filter (oracleHolds waiter clock) |> Set.ofList

    let private waiterGen : Gen<int> = Gen.elements [ 0 ; signalled ]

    /// Clocks from a small range, so that deadlines drawn from the same range land on
    /// both sides of the clock and on it.
    let private clockGen : Gen<int64> = Gen.choose (0, 40) |> Gen.map int64

    let private primitiveGen : Gen<WakePrimitive> =
        Gen.oneof
            [
                Gen.elements (List.map fst fixedTruths)
                clockGen |> Gen.map WakePrimitive.DeadlinePassed
                Gen.constant WakePrimitive.SignalDeliverable
            ]

    let rec private conditionGen (size : int) : Gen<WakeCondition> =
        if size <= 0 then
            primitiveGen |> Gen.map WakeCondition.Primitive
        else
            Gen.oneof
                [
                    primitiveGen |> Gen.map WakeCondition.Primitive
                    gen {
                        let! first = conditionGen (size / 2)
                        let! count = Gen.choose (0, 3)
                        let! rest = Gen.listOfLength count (conditionGen (size / 3))
                        return WakeCondition.AnyOf (first, rest)
                    }
                ]

    let private sizedCondition : Gen<WakeCondition> =
        Gen.sized (fun size -> conditionGen (min size 12))

    [<Test>]
    let ``the oracle's truth table is what the kernel answers of each primitive alone`` () : unit =
        // The oracle's fixed rows are hand-stated, so they are checked once here; every law
        // below leans on them.
        for primitive, truth in fixedTruths do
            for waiter in [ 0 ; signalled ] do
                WakeCondition.satisfied waiter (WakeCondition.Primitive primitive) system
                |> shouldEqual (if truth then Set.singleton primitive else Set.empty)

        let signal = WakePrimitive.SignalDeliverable

        WakeCondition.satisfied 0 (WakeCondition.Primitive signal) system
        |> shouldEqual Set.empty

        WakeCondition.satisfied signalled (WakeCondition.Primitive signal) system
        |> shouldEqual (Set.singleton signal)

    [<Test>]
    let ``a kevent wait on a kqueue that has gone is a broken park, not an answer`` () : unit =
        // A park holds the kqueue it waits on until the call returns, so the kqueue
        // cannot have gone under a waiter.
        for primitive in
            [
                WakePrimitive.KqueueDrained (OpenFileDescriptionId 999L)
                WakePrimitive.KqueueEventDeliverable (OpenFileDescriptionId 999L)
            ] do
            let exn =
                Assert.Throws<exn> (fun () ->
                    WakeCondition.satisfied 0 (WakeCondition.Primitive primitive) system
                    |> ignore<Set<WakePrimitive>>
                )

            exn.Message |> shouldContainText "a park holds what it waits on"

    [<Test>]
    let ``satisfied agrees with the flattening oracle`` () : unit =
        let mutable empty = 0
        let mutable nonEmpty = 0

        let property =
            Prop.forAll (Arb.fromGen (Gen.zip3 waiterGen clockGen sizedCondition))
            <| fun (waiter, clock, condition) ->
                let actual = WakeCondition.satisfied waiter condition (at clock)

                if Set.isEmpty actual then
                    empty <- empty + 1
                else
                    nonEmpty <- nonEmpty + 1

                actual |> shouldEqual (oracleSatisfied waiter clock condition)

        Check.One (propertyConfig, property)
        empty |> shouldBeGreaterThan 50
        nonEmpty |> shouldBeGreaterThan 50

    [<Test>]
    let ``AnyOf is the union of its members`` () : unit =
        let property =
            Prop.forAll (
                Arb.fromGen (
                    Gen.zip3
                        clockGen
                        sizedCondition
                        (Gen.choose (0, 4) |> Gen.bind (fun n -> Gen.listOfLength n sizedCondition))
                )
            )
            <| fun (clock, first, rest) ->
                let system = at clock

                WakeCondition.satisfied signalled (WakeCondition.AnyOf (first, rest)) system
                |> shouldEqual (
                    first :: rest
                    |> List.map (fun c -> WakeCondition.satisfied signalled c system)
                    |> Set.unionMany
                )

        Check.One (propertyConfig, property)

    [<Test>]
    let ``AnyOf is associative`` () : unit =
        let property =
            Prop.forAll (Arb.fromGen (Gen.zip clockGen (Gen.zip3 sizedCondition sizedCondition sizedCondition)))
            <| fun (clock, (a, b, c)) ->
                let system = at clock
                let left = WakeCondition.AnyOf (WakeCondition.AnyOf (a, [ b ]), [ c ])
                let right = WakeCondition.AnyOf (a, [ WakeCondition.AnyOf (b, [ c ]) ])
                let flat = WakeCondition.AnyOf (a, [ b ; c ])

                WakeCondition.satisfied signalled left system
                |> shouldEqual (WakeCondition.satisfied signalled right system)

                WakeCondition.satisfied signalled left system
                |> shouldEqual (WakeCondition.satisfied signalled flat system)

        Check.One (propertyConfig, property)

    [<Test>]
    let ``AnyOf is idempotent, and one member alone is that member`` () : unit =
        let property =
            Prop.forAll (Arb.fromGen (Gen.zip clockGen sizedCondition))
            <| fun (clock, a) ->
                let system = at clock
                let alone = WakeCondition.satisfied signalled a system

                WakeCondition.satisfied signalled (WakeCondition.AnyOf (a, [ a ])) system
                |> shouldEqual alone

                WakeCondition.satisfied signalled (WakeCondition.AnyOf (a, [])) system
                |> shouldEqual alone

        Check.One (propertyConfig, property)

    [<Test>]
    let ``a deadline holds exactly when the clock is at or past it`` () : unit =
        let mutable onTheDeadline = 0

        let property =
            Prop.forAll (Arb.fromGen (Gen.zip clockGen clockGen))
            <| fun (clock, deadline) ->
                if clock = deadline then
                    onTheDeadline <- onTheDeadline + 1

                let primitive = WakePrimitive.DeadlinePassed deadline

                WakeCondition.satisfied signalled (WakeCondition.Primitive primitive) (at clock)
                |> shouldEqual (
                    if clock >= deadline then
                        Set.singleton primitive
                    else
                        Set.empty
                )

        Check.One (propertyConfig, property)
        onTheDeadline |> shouldBeGreaterThan 5

    [<Test>]
    let ``deadlines are exactly the DeadlinePassed leaves`` () : unit =
        let property =
            Prop.forAll (Arb.fromGen sizedCondition)
            <| fun condition ->
                WakeCondition.deadlines condition
                |> shouldEqual (
                    flatten condition
                    |> List.choose (fun primitive ->
                        match primitive with
                        | WakePrimitive.DeadlinePassed deadline -> Some deadline
                        | WakePrimitive.EndedByClose
                        | WakePrimitive.FlockGrantable _
                        | WakePrimitive.SocketEventDeliverable _
                        | WakePrimitive.KqueueDrained _
                        | WakePrimitive.KqueueEventDeliverable _
                        | WakePrimitive.KqueuePollReportable
                        | WakePrimitive.DescriptorReady _
                        | WakePrimitive.AcceptQueueNonEmpty _
                        | WakePrimitive.PipeHasBytes _
                        | WakePrimitive.PipeWriteEndClosed _
                        | WakePrimitive.PipeHasRoom _
                        | WakePrimitive.PipeReadEndClosed _
                        | WakePrimitive.PipeReadWhileNonBlocking _
                        | WakePrimitive.SignalDeliverable -> None
                    )
                )

        Check.One (propertyConfig, property)

    [<Test>]
    let ``the existing parks carry no deadline`` () : unit =
        // What keeps a client's idle clock jump unchanged until a parking syscall takes a
        // timeout.
        [
            ParkedSyscall.Flock
                {
                    Requester = blocked
                    Mode = FlockMode.Exclusive
                }
            ParkedSyscall.SocketWait
                {
                    Port = locker
                    MaxEvents = 1
                    Buffer = UserBuffer.Mapped
                    Deadline = None
                }
        ]
        |> List.collect (WakeCondition.ofPark >> WakeCondition.deadlines)
        |> shouldEqual []

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
/// exclusive lock held through the first; neither port has anything to deliver.
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
        let system = UnixSystem.initial<int, string> SimulatedUnixPlatform.linuxX64

        let lockerFd, registry =
            FileDescriptorRegistry.createSocketEventPort system.Process.FileDescriptors

        let blockedFd, registry = FileDescriptorRegistry.createSocketEventPort registry

        let registry =
            match FileDescriptorRegistry.flock lockerFd (FlockRequest.Acquire FlockMode.Exclusive) registry with
            | registry, None -> registry
            | _, Some error -> failwith $"expected the lock to be granted, got %O{error}"

        let system = withRegistry registry system
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
        ]

    let private at (clock : int64) : UnixSystem<int, string> =
        { system with
            Machine = UnixMachineState.advanceClock clock system.Machine
        }

    let private oracleHolds (clock : int64) (primitive : WakePrimitive) : bool =
        match primitive with
        | WakePrimitive.DeadlinePassed deadline -> clock >= deadline
        | WakePrimitive.FlockGrantable _
        | WakePrimitive.SocketEventDeliverable _ ->
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

    let private oracleSatisfied (clock : int64) (condition : WakeCondition) : Set<WakePrimitive> =
        flatten condition |> List.filter (oracleHolds clock) |> Set.ofList

    /// Clocks from a small range, so that deadlines drawn from the same range land on
    /// both sides of the clock and on it.
    let private clockGen : Gen<int64> = Gen.choose (0, 40) |> Gen.map int64

    let private primitiveGen : Gen<WakePrimitive> =
        Gen.oneof
            [
                Gen.elements (List.map fst fixedTruths)
                clockGen |> Gen.map WakePrimitive.DeadlinePassed
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
            WakeCondition.satisfied (WakeCondition.Primitive primitive) system
            |> shouldEqual (if truth then Set.singleton primitive else Set.empty)

    [<Test>]
    let ``satisfied agrees with the flattening oracle`` () : unit =
        let mutable empty = 0
        let mutable nonEmpty = 0

        let property =
            Prop.forAll (Arb.fromGen (Gen.zip clockGen sizedCondition))
            <| fun (clock, condition) ->
                let actual = WakeCondition.satisfied condition (at clock)

                if Set.isEmpty actual then
                    empty <- empty + 1
                else
                    nonEmpty <- nonEmpty + 1

                actual |> shouldEqual (oracleSatisfied clock condition)

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

                WakeCondition.satisfied (WakeCondition.AnyOf (first, rest)) system
                |> shouldEqual (
                    first :: rest
                    |> List.map (fun c -> WakeCondition.satisfied c system)
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

                WakeCondition.satisfied left system
                |> shouldEqual (WakeCondition.satisfied right system)

                WakeCondition.satisfied left system
                |> shouldEqual (WakeCondition.satisfied flat system)

        Check.One (propertyConfig, property)

    [<Test>]
    let ``AnyOf is idempotent, and one member alone is that member`` () : unit =
        let property =
            Prop.forAll (Arb.fromGen (Gen.zip clockGen sizedCondition))
            <| fun (clock, a) ->
                let system = at clock
                let alone = WakeCondition.satisfied a system

                WakeCondition.satisfied (WakeCondition.AnyOf (a, [ a ])) system
                |> shouldEqual alone

                WakeCondition.satisfied (WakeCondition.AnyOf (a, [])) system
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

                WakeCondition.satisfied (WakeCondition.Primitive primitive) (at clock)
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
                        | WakePrimitive.FlockGrantable _
                        | WakePrimitive.SocketEventDeliverable _ -> None
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
                }
        ]
        |> List.collect (WakeCondition.ofPark >> WakeCondition.deadlines)
        |> shouldEqual []

namespace WoofWare.PosixKernel.Test

open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// Park order, and which parked tasks a system wakes.
///
/// The socket-event half of `UnixWait.wakes` lives in `TestUnixSystemStep`, beside the
/// port fixtures that can make a port deliverable.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestUnixWait =

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
        { system with
            Tasks = UnixTaskTable.register name (CpuId 0) (OsThreadId (uint32 name + 1u)) system.Tasks
        }

    /// Two socket event ports, which contend under `flock` because they share one
    /// anonymous inode, with an exclusive lock held through `locker`; and tasks 1 to 4.
    let private world : UnixSystem<int, string> * int * OpenFileDescriptionId * OpenFileDescriptionId =
        let system = UnixSystem.initial<int, string> SimulatedUnixPlatform.linuxX64

        let lockerFd, registry =
            FileDescriptorRegistry.createSocketEventPort system.Process.FileDescriptors

        let blockedFd, registry = FileDescriptorRegistry.createSocketEventPort registry

        let registry =
            match FileDescriptorRegistry.flock lockerFd (FlockRequest.Acquire FlockMode.Exclusive) registry with
            | registry, None -> registry
            | _, Some error -> failwith $"expected the lock to be granted, got %O{error}"

        let system =
            withRegistry registry system
            |> withTask 1
            |> withTask 2
            |> withTask 3
            |> withTask 4

        system, lockerFd, idOf lockerFd system, idOf blockedFd system

    let private system : UnixSystem<int, string> =
        let system, _, _, _ = world
        system

    let private lockerFd : int =
        let _, fd, _, _ = world
        fd

    let private locker : OpenFileDescriptionId =
        let _, _, locker, _ = world
        locker

    let private blocked : OpenFileDescriptionId =
        let _, _, _, blocked = world
        blocked

    /// The park each task makes: 1 and 2 wait on a port, 3 and 4 for a lock, so that
    /// both kinds of park are in the table at once.
    let private parkOfTask (task : int) : ParkedSyscall =
        if task <= 2 then
            ParkedSyscall.SocketWait
                {
                    Port = blocked
                    MaxEvents = 1
                }
        else
            ParkedSyscall.Flock
                {
                    Requester = blocked
                    Mode = FlockMode.Exclusive
                }

    let private releaseLock (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        match FileDescriptorRegistry.flock lockerFd FlockRequest.Release system.Process.FileDescriptors with
        | registry, None -> withRegistry registry system
        | _, Some error -> failwith $"expected the release to succeed, got %O{error}"

    [<RequireQualifiedAccess>]
    type private Op =
        | Park of task : int
        | Unpark of task : int
        | AdvanceClock of nanoseconds : int64
        | CreatePort
        | Register of task : int

    let private opGen : Gen<Op> =
        let task = Gen.choose (1, 4)

        Gen.frequency
            [
                4, task |> Gen.map Op.Park
                2, task |> Gen.map Op.Unpark
                1, Gen.choose (0, 1000) |> Gen.map (int64 >> Op.AdvanceClock)
                1, Gen.constant Op.CreatePort
                1, Gen.choose (5, 1000) |> Gen.map Op.Register
            ]

    let private apply (op : Op) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        match op with
        | Op.Park task -> UnixWait.park task (parkOfTask task) system
        | Op.Unpark task ->
            { system with
                Tasks = UnixTaskTable.unpark task system.Tasks
            }
        | Op.AdvanceClock nanoseconds ->
            { system with
                Machine = UnixMachineState.advanceClock nanoseconds system.Machine
            }
        | Op.CreatePort ->
            let _, registry =
                FileDescriptorRegistry.createSocketEventPort system.Process.FileDescriptors

            withRegistry registry system
        | Op.Register task ->
            if Map.containsKey task system.Tasks then
                system
            else
                withTask task system

    [<Test>]
    let ``park order is strictly increasing, and survives unrelated changes`` () : unit =
        let mutable reparks = 0

        let property =
            Prop.forAll (Arb.fromGen (Gen.listOf opGen))
            <| fun ops ->
                ((system, None), ops)
                ||> List.fold (fun (before, lastMinted : ParkOrdinal option) op ->
                    let after = apply op before

                    let subject =
                        match op with
                        | Op.Park task
                        | Op.Unpark task -> Some task
                        | Op.AdvanceClock _
                        | Op.CreatePort
                        | Op.Register _ -> None

                    // Every other task keeps its park, ordinal and all.
                    for task in Map.keys before.Tasks do
                        if Some task <> subject then
                            UnixTaskTable.parkOf task after.Tasks
                            |> shouldEqual (UnixTaskTable.parkOf task before.Tasks)

                    UnixSystem.checkInvariants after |> shouldEqual []

                    match op with
                    | Op.Park task ->
                        if (UnixTaskTable.parkOf task before.Tasks).IsSome then
                            reparks <- reparks + 1

                        match UnixTaskTable.parkOf task after.Tasks with
                        | None -> failwith "expected the park to be recorded"
                        | Some park ->
                            park.Syscall |> shouldEqual (parkOfTask task)

                            match lastMinted with
                            | Some last -> park.Ordinal |> shouldBeGreaterThan last
                            | None -> ()

                            after, Some park.Ordinal
                    | Op.Unpark task ->
                        UnixTaskTable.parkOf task after.Tasks |> shouldEqual None
                        after, lastMinted
                    | Op.AdvanceClock _
                    | Op.CreatePort
                    | Op.Register _ -> after, lastMinted
                )
                |> ignore

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 300, property)
        reparks |> shouldBeGreaterThan 50

    [<Test>]
    let ``a released lock wakes every waiter, in the order they parked`` () : unit =
        // Parked against the table's own key order, so that park order is told apart from it.
        let parked =
            system |> UnixWait.park 4 (parkOfTask 4) |> UnixWait.park 3 (parkOfTask 3)

        let asleep = Set.ofList [ 3 ; 4 ]

        let fired =
            Set.singleton (WakePrimitive.FlockGrantable (blocked, FlockMode.Exclusive))

        UnixWait.wakes asleep parked |> shouldEqual (Ok [])

        UnixWait.wakes asleep (releaseLock parked)
        |> shouldEqual (Ok [ 4, fired ; 3, fired ])

        // A re-park moves the waiter behind every park already made.
        let reparked = parked |> UnixWait.park 4 (parkOfTask 4)

        UnixWait.wakes asleep (releaseLock reparked)
        |> shouldEqual (Ok [ 3, fired ; 4, fired ])

    [<Test>]
    let ``only the tasks the client holds asleep are woken`` () : unit =
        // A woken task keeps its park until its call finishes, so the park alone would wake
        // it a second time.
        let released =
            system
            |> UnixWait.park 3 (parkOfTask 3)
            |> UnixWait.park 4 (parkOfTask 4)
            |> releaseLock

        UnixWait.wakes (Set.singleton 4) released
        |> shouldEqual (
            Ok
                [
                    4, Set.singleton (WakePrimitive.FlockGrantable (blocked, FlockMode.Exclusive))
                ]
        )

    [<Test>]
    let ``a task held asleep with no park is a client bug`` () : unit =
        let exn =
            Assert.Throws<exn> (fun () -> UnixWait.wakes (Set.singleton 1) system |> ignore)

        exn.Message |> shouldContainText "records no park"

        let exn =
            Assert.Throws<exn> (fun () -> UnixWait.deadlines (Set.singleton 1) system |> ignore)

        exn.Message |> shouldContainText "records no park"

    [<Test>]
    let ``a quiet port wakes none of its waiters, however many there are`` () : unit =
        // The exclusive-wake refusal is about a deliverable event, not about sharing a port.
        let parked =
            system |> UnixWait.park 1 (parkOfTask 1) |> UnixWait.park 2 (parkOfTask 2)

        UnixWait.wakes (Set.ofList [ 1 ; 2 ]) parked |> shouldEqual (Ok [])

    [<Test>]
    let ``a park of a different syscall over an existing one is refused`` () : unit =
        let parked = system |> UnixWait.park 1 (parkOfTask 1)

        let exn =
            Assert.Throws<exn> (fun () -> UnixWait.park 1 (parkOfTask 3) parked |> ignore)

        exn.Message |> shouldContainText "without clearing the first"

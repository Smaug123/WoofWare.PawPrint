namespace WoofWare.PosixKernel.Test

open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// The holds a syscall in flight has on open file descriptions, which the
/// machine's open file table records (`OpenFileTable.holdCount`), and the thread
/// IDs live tasks hold, which the machine's thread ID allocator records: each a
/// fact a process's view cannot derive, since another process's tasks hold some.
///
/// The property drives parks of every shape, re-parks, the ends of calls,
/// closes, dups, spawns and exits through a process, and holds the recorded
/// facts to a reference the test keeps itself: for each task, the descriptions
/// the call it parked holds, and for each task, the thread ID it was given.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestCallHolds =

    let private platforms : SimulatedUnixPlatform list =
        [ SimulatedUnixPlatform.linuxX64 ; SimulatedUnixPlatform.macOsArm64 ]

    let private taskNames : int list = [ 0 ; 1 ; 2 ; 3 ]

    /// One step: each index is read modulo whatever it picks from.
    [<RequireQualifiedAccess>]
    type private Op =
        | Park of task : int * shape : int * a : int * b : int
        | Unpark of task : int
        | Close of fd : int
        | Dup of fd : int
        | Spawn of child : int
        | Exit of task : int
        | ExitGroup of task : int

    let private opGen : Gen<Op> =
        let small = Gen.choose (0, 63)

        Gen.frequency
            [
                8, Gen.map4 (fun t s a b -> Op.Park (t, s, a, b)) small small small small
                4, Gen.map Op.Unpark small
                4, Gen.map Op.Close small
                3, Gen.map Op.Dup small
                3, Gen.map Op.Spawn small
                2, Gen.map Op.Exit small
                1, Gen.map Op.ExitGroup small
            ]

    /// What the test knows independently of the library: the descriptions each
    /// parked task's call holds, as the test built its park, and the thread ID
    /// each live task was given.
    type private Model =
        {
            Holds : Map<int, OpenFileDescriptionId list>
            Tids : Map<int, OsThreadId>
        }

    /// How often each path the property exists for was reached.
    type private Coverage =
        {
            mutable Parks : int
            mutable Reparks : int
            mutable RepeatedHolds : int
            mutable Unparks : int
            mutable DestroyedAtUnpark : int
            mutable KeptByHoldAtClose : int
            mutable EndedByClose : int
            mutable Spawns : int
            mutable Exits : int
            mutable EndedWithHolds : int
        }

    let private world (platform : SimulatedUnixPlatform) : UnixSystem<int, string> =
        let system =
            UnixSystem.initial<int, string> platform
            |> Launched.boot UnixSystem.pipedStandardStreams 0 (CpuId 0)

        // A process whose pipe writes end at once: no `SIGPIPE` to refuse a
        // close that ends one.
        let system =
            { system with
                Process =
                    { system.Process with
                        Signals =
                            system.Process.Signals
                            |> SignalState.setDisposition Signal.SIGPIPE SignalDisposition.Ignore
                    }
            }

        let system =
            match UnixPipe.pipe2 0 UserBuffer.Mapped system with
            | Ok (Pipe2Answer.Created _, system) -> system
            | other -> failwith $"pipe2: %A{other}"

        match SimulatedUnixPlatform.flavour platform with
        | SimulatedUnixFlavour.Linux ->
            let _, registry =
                FileDescriptorRegistry.createEpoll (UnixSystemState.fileDescriptors system)

            UnixSystemState.withFileDescriptors registry system
        | SimulatedUnixFlavour.Darwin ->
            match UnixKqueue.kqueue system with
            | Ok (_, system) -> system
            | Error refusal -> failwith $"kqueue: %A{refusal}"

    let private fds (system : UnixSystem<int, string>) : (int * OpenFileDescriptionId * OpenFileTarget) list =
        let registry = UnixSystemState.fileDescriptors system

        FileDescriptorRegistry.fds registry
        |> Map.toList
        |> List.map (fun (fd, id) -> fd, id, (OpenFileTable.get "test" id system.Machine.OpenFiles).Target)

    /// The park of shape `shape` for the descriptors `system` has, with the
    /// descriptions it holds by the test's own reckoning; or `None` when no
    /// descriptor the shape needs is open.
    let private shapeOf
        (shape : int)
        (a : int)
        (b : int)
        (system : UnixSystem<int, string>)
        : (ParkedSyscall * OpenFileDescriptionId list) option
        =
        let all = fds system

        let pick (xs : 'a list) (i : int) : 'a option =
            if xs.IsEmpty then None else Some xs.[i % xs.Length]

        let ofTarget (predicate : OpenFileTarget -> bool) =
            all |> List.filter (fun (_, _, target) -> predicate target)

        let readEnds =
            ofTarget (
                function
                | OpenFileTarget.Pipe (_, PipeEnd.Read) -> true
                | _ -> false
            )

        let writeEnds =
            ofTarget (
                function
                | OpenFileTarget.Pipe (_, PipeEnd.Write) -> true
                | _ -> false
            )

        let pipes = readEnds @ writeEnds

        match shape % 7 with
        | 0 ->
            pick all a
            |> Option.map (fun (_, id, _) ->
                ParkedSyscall.Flock
                    {
                        Requester = id
                        Mode = FlockMode.Exclusive
                    },
                [ id ]
            )
        | 1 ->
            pick readEnds a
            |> Option.map (fun (fd, id, _) ->
                ParkedSyscall.PipeRead
                    {
                        Reader = SleepTarget.Waiting (id, fd)
                        Buffer = UserBuffer.Mapped
                        Count = 4
                    },
                [ id ]
            )
        // A Darwin write sleeps only once its pipe's buffer has grown as far as
        // it goes, which a park made here would not reflect.
        | 2 when SimulatedUnixPlatform.flavour system.Machine.UnixPlatform = SimulatedUnixFlavour.Darwin -> None
        | 2 ->
            pick writeEnds a
            |> Option.map (fun (fd, id, _) ->
                ParkedSyscall.PipeWrite
                    {
                        Writer = SleepTarget.Waiting (id, fd)
                        Buffer = UserBuffer.Mapped
                        Count = 10
                        Written = 0
                        ReadsSeen = 0L
                    },
                [ id ]
            )
        | 3 ->
            // A Linux poll holds every description it watches, once per entry, so
            // a descriptor watched twice is held twice.
            match SimulatedUnixPlatform.flavour system.Machine.UnixPlatform with
            | SimulatedUnixFlavour.Darwin ->
                Some (
                    ParkedSyscall.KqueuePoll
                        {
                            Entries = []
                            // `park` makes the kqueue this names.
                            Queue = PollQueueId -1L
                            Deadline = None
                        },
                    []
                )
            | SimulatedUnixFlavour.Linux ->
                if pipes.IsEmpty then
                    None
                else
                    let watched = [ a ; b ; a + b ] |> List.take (1 + b % 3) |> List.choose (pick pipes)

                    Some (
                        ParkedSyscall.Poll
                            {
                                Entries = watched |> List.map (fun (fd, id, _) -> ParkedPollEntry.Watched (fd, id, 1s))
                                Deadline = None
                            },
                        watched |> List.map (fun (_, id, _) -> id)
                    )
        | 4 ->
            ofTarget (
                function
                | OpenFileTarget.Epoll _ -> true
                | _ -> false
            )
            |> fun epolls -> pick epolls a
            |> Option.map (fun (_, id, _) ->
                ParkedSyscall.EpollWait
                    {
                        Epoll = id
                        MaxEvents = 1
                        Buffer = UserBuffer.Mapped
                        Deadline = None
                    },
                [ id ]
            )
        | 5 ->
            ofTarget (
                function
                | OpenFileTarget.Kqueue _ -> true
                | _ -> false
            )
            |> fun kqueues -> pick kqueues a
            |> Option.map (fun (fd, id, _) ->
                ParkedSyscall.Kevent
                    {
                        Kqueue = id
                        Fd = fd
                        MaxEvents = 1
                        Buffer = UserBuffer.Mapped
                        Deadline = None
                    },
                [ id ]
            )
        | _ ->
            // The same description through two entries of one poll, or a flock
            // on whatever `b` picks: a park that names one description twice.
            match SimulatedUnixPlatform.flavour system.Machine.UnixPlatform with
            | SimulatedUnixFlavour.Linux ->
                pick pipes b
                |> Option.map (fun (fd, id, _) ->
                    ParkedSyscall.Poll
                        {
                            Entries = [ ParkedPollEntry.Watched (fd, id, 1s) ; ParkedPollEntry.Watched (fd, id, 4s) ]
                            Deadline = None
                        },
                    [ id ; id ]
                )
            | SimulatedUnixFlavour.Darwin ->
                pick all b
                |> Option.map (fun (_, id, _) ->
                    ParkedSyscall.Flock
                        {
                            Requester = id
                            Mode = FlockMode.Shared
                        },
                    [ id ]
                )

    /// `task` parked in `parked`, as `UnixWait.park` parks it; a Darwin poll
    /// first makes the kqueue it holds, empty, as the call does before it
    /// sleeps, unless the task is already asleep in one, whose it keeps.
    let private park
        (task : int)
        (parked : ParkedSyscall)
        (system : UnixSystem<int, string>)
        : UnixSystem<int, string>
        =
        match parked with
        | ParkedSyscall.KqueuePoll poll ->
            match UnixTaskTable.parkedFor task system.Tasks with
            | Some (ParkedSyscall.KqueuePoll existing) ->
                UnixWait.park
                    task
                    (ParkedSyscall.KqueuePoll
                        { poll with
                            Queue = existing.Queue
                        })
                    system
            | _ ->
                let queue, machine =
                    UnixMachineState.addPollQueue
                        {
                            Owner = system.Process.ProcessId
                            Registrations = Map.empty
                            Active = []
                        }
                        system.Machine

                UnixWait.park
                    task
                    (ParkedSyscall.KqueuePoll
                        { poll with
                            Queue = queue
                        })
                    { system with
                        Machine = machine
                    }
        | _ -> UnixWait.park task parked system

    let private kindOf (parked : ParkedSyscall) : int =
        match parked with
        | ParkedSyscall.EpollWait _ -> 0
        | ParkedSyscall.Flock _ -> 1
        | ParkedSyscall.Poll _ -> 2
        | ParkedSyscall.Accept _ -> 3
        | ParkedSyscall.PipeRead _ -> 4
        | ParkedSyscall.PipeWrite _ -> 5
        | ParkedSyscall.Kevent _ -> 6
        | ParkedSyscall.KqueuePoll _ -> 7
        | ParkedSyscall.ConnectionRead _ -> 8
        | ParkedSyscall.ConnectionWrite _ -> 9

    /// The recorded facts against the model: each description's holds, which
    /// descriptions exist, and which thread IDs are live.
    let private agree (where : string) (model : Model) (system : UnixSystem<int, string>) : unit =
        let implied =
            model.Holds |> Map.toList |> List.collect snd |> List.countBy id |> Map.ofList

        let named = fds system |> List.map (fun (_, id, _) -> id) |> Set.ofList

        let present =
            OpenFileTable.descriptions system.Machine.OpenFiles |> Map.keys |> Set.ofSeq

        let expected = Set.union named (implied |> Map.keys |> Set.ofSeq)

        if present <> expected then
            failwith $"%s{where}: descriptions %A{present} exist, where %A{expected} are referenced"

        for id in present do
            let recorded = OpenFileTable.holdCount id system.Machine.OpenFiles
            let wanted = Some (Map.tryFind id implied |> Option.defaultValue 0)

            if recorded <> wanted then
                failwith $"%s{where}: %O{id} records %A{recorded} holds, where the parks hold it %A{wanted} times"

        ThreadIdAllocator.live system.Machine.ThreadIds
        |> shouldEqual (model.Tids |> Map.values |> Set.ofSeq)

        match UnixSystem.checkInvariants system with
        | [] -> ()
        | defects -> failwith $"%s{where}: %A{defects}"

    let private run (coverage : Coverage) (platform : SimulatedUnixPlatform) (ops : Op list) : unit =
        let darwin = SimulatedUnixPlatform.flavour platform = SimulatedUnixFlavour.Darwin

        let initial = world platform

        let model =
            {
                Holds = Map.empty
                Tids = Map.ofList [ 0, UnixTaskTable.osThreadIdOf 0 initial.Tasks ]
            }

        agree "initial" model initial

        let step ((system, model) : UnixSystem<int, string> * Model) (op : Op) =
            let live = system.Tasks |> Map.keys |> List.ofSeq
            let liveTask (i : int) = live.[i % live.Length]

            let system, model =
                match op with
                | Op.Park (t, shape, a, b) ->
                    let task = liveTask t

                    match shapeOf shape a b system with
                    | None -> system, model
                    | Some (parked, held) ->
                        match UnixTaskTable.parkedFor task system.Tasks with
                        | Some existing when kindOf existing <> kindOf parked -> system, model
                        | existing ->
                            if existing.IsSome then
                                coverage.Reparks <- coverage.Reparks + 1

                            if List.length (List.distinct held) < List.length held then
                                coverage.RepeatedHolds <- coverage.RepeatedHolds + 1

                            coverage.Parks <- coverage.Parks + 1

                            // A re-park lets go of what the earlier park held, which
                            // goes if nothing else references it, as it would at the
                            // earlier call's return.
                            let released =
                                existing |> Option.map ParkedSyscall.descriptions |> Option.defaultValue []

                            park task parked system
                            |> ObjectLifetime.releaseUnreferencedUnrefusable "test" released,
                            { model with
                                Holds = Map.add task held model.Holds
                            }
                | Op.Unpark t ->
                    let task = liveTask t

                    match UnixTaskTable.parkedFor task system.Tasks with
                    | None -> system, model
                    | Some parked ->
                        // As a finishing call does: the park goes, then whatever it
                        // was the last reference to.
                        let held = ParkedSyscall.descriptions parked
                        let before = OpenFileTable.descriptions system.Machine.OpenFiles |> Map.count

                        let after =
                            UnixParkState.unpark task system
                            |> ObjectLifetime.releaseUnreferencedUnrefusable "test" held

                        if OpenFileTable.descriptions after.Machine.OpenFiles |> Map.count < before then
                            coverage.DestroyedAtUnpark <- coverage.DestroyedAtUnpark + 1

                        coverage.Unparks <- coverage.Unparks + 1

                        after,
                        { model with
                            Holds = Map.remove task model.Holds
                        }
                | Op.Close i ->
                    match fds system with
                    | [] -> system, model
                    | all ->
                        let fd, id, _ = all.[i % all.Length]

                        match UnixDescriptor.close fd system with
                        | Error _ -> system, model
                        | Ok (_, after) ->
                            // Under Darwin a close of the descriptor a pipe transfer
                            // sleeps through ends the call, which then holds nothing.
                            let holds =
                                if not darwin then
                                    model.Holds
                                else
                                    model.Holds
                                    |> Map.map (fun task held ->
                                        match UnixTaskTable.parkedFor task system.Tasks with
                                        | Some (ParkedSyscall.PipeRead {
                                                                           Reader = SleepTarget.Waiting (_, entered)
                                                                       })
                                        | Some (ParkedSyscall.PipeWrite {
                                                                            Writer = SleepTarget.Waiting (_, entered)
                                                                        }) when entered = fd ->
                                            coverage.EndedByClose <- coverage.EndedByClose + 1
                                            []
                                        | _ -> held
                                    )

                            let model =
                                { model with
                                    Holds = holds
                                }

                            if
                                OpenFileTable.tryFind id after.Machine.OpenFiles |> Option.isSome
                                && not (fds after |> List.exists (fun (_, other, _) -> other = id))
                            then
                                coverage.KeptByHoldAtClose <- coverage.KeptByHoldAtClose + 1

                            after, model
                | Op.Dup i ->
                    match fds system with
                    | [] -> system, model
                    | all ->
                        let fd, _, _ = all.[i % all.Length]

                        match UnixDescriptor.dup fd system with
                        | Ok (_, after) -> after, model
                        | Error refusal -> failwith $"dup: %A{refusal}"
                | Op.Spawn c ->
                    let child = taskNames.[c % taskNames.Length]

                    if Map.containsKey child system.Tasks then
                        system, model
                    else

                    let parent = system.Leader

                    if (UnixTaskTable.parkedFor parent system.Tasks).IsSome then
                        system, model
                    else

                    let before = ThreadIdAllocator.live system.Machine.ThreadIds

                    match UnixTaskLifecycle.spawn parent child (CpuId 0) system with
                    | Error refusal -> failwith $"spawn: %s{SpawnRefusal.describe refusal}"
                    | Ok (SpawnAnswer.Failed error, _) -> failwith $"spawn: %O{error}"
                    | Ok (SpawnAnswer.Spawned id, after) ->
                        Set.contains id before |> shouldEqual false
                        coverage.Spawns <- coverage.Spawns + 1

                        after,
                        { model with
                            Tids = Map.add child id model.Tids
                        }
                | Op.Exit t ->
                    let task = liveTask t

                    if task = system.Leader then
                        system, model
                    else

                    match UnixTaskLifecycle.exitThread task 0 system with
                    | Ok (TaskOutcome.Continues after) ->
                        coverage.Exits <- coverage.Exits + 1

                        after,
                        { model with
                            Tids = Map.remove task model.Tids
                        }
                    | Error (ThreadExitRefusal.Parked _) -> system, model
                    | other -> failwith $"exit of %d{task}: %A{other}"
                | Op.ExitGroup t ->
                    let task = liveTask t

                    if (UnixTaskTable.parkedFor task system.Tasks).IsSome then
                        system, model
                    else

                    // Every task goes, with the holds its call had and its thread
                    // ID; descriptors are not closed, so what they name stays.
                    let ended = UnixTaskLifecycle.exitGroup task 0 system

                    if not model.Holds.IsEmpty then
                        coverage.EndedWithHolds <- coverage.EndedWithHolds + 1

                    ThreadIdAllocator.live ended.Machine.ThreadIds |> shouldEqual Set.empty

                    for id in OpenFileTable.descriptions ended.Machine.OpenFiles |> Map.keys do
                        OpenFileTable.holdCount id ended.Machine.OpenFiles |> shouldEqual (Some 0)

                        OpenFileTable.descriptorCount id ended.Machine.OpenFiles
                        |> shouldEqual (OpenFileTable.descriptorCount id system.Machine.OpenFiles)

                    // The run goes on from the state before, as if the call had
                    // not been made.
                    system, model

            agree $"%A{op}" model system
            system, model

        List.fold step (initial, model) ops |> ignore

    [<TestCaseSource(nameof platforms)>]
    let ``every description records the holds the parks name, and the allocator the IDs the tasks hold``
        (platform : SimulatedUnixPlatform)
        : unit
        =
        let coverage =
            {
                Parks = 0
                Reparks = 0
                RepeatedHolds = 0
                Unparks = 0
                DestroyedAtUnpark = 0
                KeptByHoldAtClose = 0
                EndedByClose = 0
                Spawns = 0
                Exits = 0
                EndedWithHolds = 0
            }

        let gen =
            gen {
                let! length = Gen.choose (0, 60)
                return! Gen.listOfLength length opGen
            }

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 500, Prop.forAll (Arb.fromGen gen) (run coverage platform))

        coverage.Parks |> shouldBeGreaterThan 100
        coverage.Reparks |> shouldBeGreaterThan 10
        coverage.Unparks |> shouldBeGreaterThan 100
        coverage.DestroyedAtUnpark |> shouldBeGreaterThan 0
        coverage.KeptByHoldAtClose |> shouldBeGreaterThan 0
        coverage.Spawns |> shouldBeGreaterThan 100
        coverage.Exits |> shouldBeGreaterThan 10
        coverage.EndedWithHolds |> shouldBeGreaterThan 10

        match SimulatedUnixPlatform.flavour platform with
        | SimulatedUnixFlavour.Darwin -> coverage.EndedByClose |> shouldBeGreaterThan 0
        | SimulatedUnixFlavour.Linux -> coverage.RepeatedHolds |> shouldBeGreaterThan 10

    [<Test>]
    let ``checkInvariants reports exactly a description whose holds disagree with the parks`` () : unit =
        let property (platform : SimulatedUnixPlatform) (shape : int) (a : int) (b : int) (wrong : int) : unit =
            let system = world platform

            match shapeOf shape a b system with
            | None -> ()
            | Some (parked, held) ->
                let system = park 0 parked system

                let ids =
                    OpenFileTable.descriptions system.Machine.OpenFiles |> Map.keys |> List.ofSeq

                let id = ids.[abs wrong % ids.Length]
                let actual = held |> List.filter ((=) id) |> List.length
                let candidates = [ 0 .. actual + 3 ] |> List.filter (fun count -> count <> actual)
                let forged = candidates.[abs wrong % candidates.Length]

                let rewritten =
                    let rec toward (system : UnixSystem<int, string>) =
                        match OpenFileTable.holdCount id system.Machine.OpenFiles with
                        | Some n when n < forged -> toward (UnixSystemState.mapOpenFiles (OpenFileTable.hold id) system)
                        | Some n when n > forged ->
                            toward (UnixSystemState.mapOpenFiles (OpenFileTable.releaseHold id) system)
                        | _ -> system

                    toward system

                let defects = UnixSystem.checkInvariants rewritten

                let expected =
                    [ UnixSystemDefect.HoldCountMismatch (id, forged, actual) ]
                    // A description no descriptor names, which no call holds by its
                    // record, is a leak besides.
                    @ (if
                           forged = 0
                           && OpenFileTable.descriptorCount id rewritten.Machine.OpenFiles = Some 0
                       then
                           [ UnixSystemDefect.UnreferencedDescription id ]
                       else
                           [])

                List.sortBy (sprintf "%A") defects
                |> shouldEqual (List.sortBy (sprintf "%A") expected)

        Check.One (
            Config.QuickThrowOnFailure.WithMaxTest 300,
            Prop.forAll
                (Arb.fromGen (
                    Gen.zip
                        (Gen.elements platforms)
                        (Gen.zip
                            (Gen.zip (Gen.choose (0, 63)) (Gen.choose (0, 63)))
                            (Gen.zip (Gen.choose (0, 63)) (Gen.choose (0, 63))))
                ))
                (fun (platform, ((shape, a), (b, wrong))) -> property platform shape a b wrong)
        )

    [<Test>]
    let ``checkInvariants reports exactly the thread IDs the allocator records live and no task holds, and the reverse``
        ()
        : unit
        =
        for platform in platforms do
            let system = world platform |> Tasks.spawn 1 |> Tasks.spawn 2
            let one = UnixTaskTable.osThreadIdOf 1 system.Tasks

            // Task 1's ID freed behind its back: the allocator could hand it out again.
            let freed =
                { system with
                    Machine =
                        { system.Machine with
                            ThreadIds = ThreadIdAllocator.release one system.Machine.ThreadIds
                        }
                }

            UnixSystem.checkInvariants freed
            |> shouldEqual [ UnixSystemDefect.LiveThreadIdsMismatch (Set.empty, Set.singleton one) ]

            // Task 1 gone without its ID: the allocator never hands it out again.
            let leaked =
                { system with
                    Tasks = Map.remove 1 system.Tasks
                }

            UnixSystem.checkInvariants leaked
            |> shouldEqual [ UnixSystemDefect.LiveThreadIdsMismatch (Set.singleton one, Set.empty) ]

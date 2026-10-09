namespace WoofWare.PosixKernel.Test

open System
open System.Collections.Immutable
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// One step of the scheduling property, on a machine of several processes.
///
/// Every index is reduced modulo the number of candidates the step can be
/// taken by, and a step with none is skipped. Each process's leader, task 0,
/// never parks, so it can always write the byte that wakes a reader, and end
/// the process.
[<RequireQualifiedAccess>]
type SchedulingOp =
    /// The process's leader creates a task on the processor `cpu`, modulo the
    /// machine's count.
    | Spawn of proc : int * cpu : int
    /// The client dispatches a live task, parked or not: to `cpu` modulo the
    /// count, or with `beyond`, to a processor the machine does not have.
    | Dispatch of task : int * cpu : int * beyond : int option
    /// The client dispatches a task it has woken and not yet finished, to
    /// `cpu` modulo the count: how a woken task gets back onto a processor.
    | DispatchWoken of task : int * cpu : int
    /// A task other than a leader reads a byte from its process's pipe, which
    /// parks it if the pipe is empty.
    | Park of proc : int
    /// The process's leader writes a byte into its pipe, and the client asks
    /// which of the readers it holds asleep that wakes.
    | Wake of proc : int
    /// The client finishes a woken read, which returns or parks again.
    | Finish of task : int
    /// A task that may exit does: a leader only when it is its process's last.
    | ExitThread of task : int
    /// The process's leader calls `exit_group`.
    | EndProcess of proc : int
    /// A process is launched, its leader on `cpu`, modulo the count.
    | Launch of cpu : int

/// `UnixScheduling` against a reference that derives what runs where from the
/// history of events alone, rather than from a record it updates: a task runs
/// on a processor exactly when the most recent event naming the task or that
/// processor is the task's dispatch there; a task's processor is that of its
/// latest dispatch, or else the one it was created on. The events are creation,
/// dispatch, park and exit.
///
/// Run on a machine of 1 to 4 processors and 1 to 3 processes, with at most 6
/// tasks, on Linux and Darwin.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestUnixScheduling =

    let private propertyConfig : Config = Config.QuickThrowOnFailure.WithMaxTest 300

    let private maxTasks : int = 6
    let private maxProcesses : int = 3

    [<RequireQualifiedAccess>]
    type private Event =
        | Created of serial : int * cpu : CpuId
        | Dispatched of serial : int * cpu : CpuId
        | Parked of serial : int
        | Exited of serial : int

    type private Live =
        {
            Pid : ProcessId
            Name : int
            /// Unique across the run, so that the reference never confuses two
            /// tasks however names and IDs are reused.
            Serial : int
            /// Parked in its read, and held asleep by the client.
            Asleep : bool
            /// Parked in its read, and woken, but not yet finished.
            Woken : bool
        }

    type private World =
        {
            Platform : SimulatedUnixPlatform
            Count : int
            Machine : SimulatedMachine<int, string>
            /// In creation order.
            Live : Live list
            NextName : Map<ProcessId, int>
            NextSerial : int
            /// Newest first.
            Events : Event list
        }

    // --- the reference ---

    /// The processor `serial` runs on: its latest event is its dispatch to a
    /// processor no later event dispatches another task to.
    let private runningOracle (serial : int) (events : Event list) : CpuId option =
        let rec go (dispatchedSince : Set<CpuId>) (events : Event list) : CpuId option =
            match events with
            | [] -> None
            | Event.Dispatched (s, cpu) :: _ when s = serial ->
                if Set.contains cpu dispatchedSince then None else Some cpu
            | Event.Dispatched (_, cpu) :: rest -> go (Set.add cpu dispatchedSince) rest
            | Event.Created (s, _) :: _
            | Event.Parked s :: _
            | Event.Exited s :: _ when s = serial -> None
            | _ :: rest -> go dispatchedSince rest

        go Set.empty events

    let private cpuOracle (serial : int) (events : Event list) : CpuId =
        events
        |> List.pick (fun event ->
            match event with
            | Event.Dispatched (s, cpu)
            | Event.Created (s, cpu) when s = serial -> Some cpu
            | _ -> None
        )

    /// Whether `serial`'s latest event is a dispatch: so that, if it is not
    /// running, another task was dispatched to its processor since.
    let private lastDispatched (serial : int) (events : Event list) : bool =
        events
        |> List.tryPick (fun event ->
            match event with
            | Event.Dispatched (s, _) when s = serial -> Some true
            | Event.Created (s, _)
            | Event.Parked s
            | Event.Exited s when s = serial -> Some false
            | _ -> None
        )
        |> Option.defaultValue false

    // --- the run ---

    /// What the generated runs reached, summed over every run of a property.
    type private Reached =
        {
            mutable DisplacedGetCpu : int
            mutable WokenDispatched : int
            mutable CrossProcessDisplacement : int
            mutable BeyondDispatch : int
        }

    let private throws (f : unit -> 'a) : bool =
        try
            f () |> ignore<'a>
            false
        with _ ->
            true

    let private viewOf (pid : ProcessId) (world : World) : UnixSystem<int, string> = Machines.viewOf pid world.Machine

    let private withPipe (pid : ProcessId) (machine : SimulatedMachine<int, string>) : SimulatedMachine<int, string> =
        machine
        |> Machines.doIn
            pid
            (fun view ->
                match UnixPipe.pipe2 0 UserBuffer.Mapped view with
                | Ok (Pipe2Answer.Created (3, 4), view) -> view
                | other -> failwith $"pipe2: %A{other}"
            )

    let private launch (cpu : int) (world : World) : World =
        if Map.count world.NextName >= maxProcesses || List.length world.Live >= maxTasks then
            world
        else

        let cpu = CpuId (cpu % world.Count)

        match
            SimulatedMachine.launch (Launched.launch world.Platform UnixSystem.pipedStandardStreams 0 cpu) world.Machine
        with
        | Error refusal -> failwith $"launch: %s{ProcessCreationRefusal.describe refusal}"
        | Ok (pid, machine) ->
            { world with
                Machine = withPipe pid machine
                Live =
                    world.Live
                    @ [
                        {
                            Pid = pid
                            Name = 0
                            Serial = world.NextSerial
                            Asleep = false
                            Woken = false
                        }
                    ]
                NextName = Map.add pid 1 world.NextName
                NextSerial = world.NextSerial + 1
                Events = Event.Created (world.NextSerial, cpu) :: world.Events
            }

    let private initialWorld (platform : SimulatedUnixPlatform) (count : int) (leaderCpu : int) : World =
        let leaderCpu = CpuId (leaderCpu % count)

        let system =
            UnixSystem.initial platform
            |> UnixBootImage.withProcessorCount count
            |> Configured.expectOk ProcessorCountRefusal.describe
            |> Launched.boot UnixSystem.pipedStandardStreams 0 leaderCpu

        let pid = UnixSystem.processId system

        {
            Platform = platform
            Count = count
            Machine = SimulatedMachine.ofSystem system |> withPipe pid
            Live =
                [
                    {
                        Pid = pid
                        Name = 0
                        Serial = 0
                        Asleep = false
                        Woken = false
                    }
                ]
            NextName = Map.ofList [ pid, 1 ]
            NextSerial = 1
            Events = [ Event.Created (0, leaderCpu) ]
        }

    let private pick (index : int) (candidates : 'a list) : 'a option =
        match candidates with
        | [] -> None
        | _ -> Some (List.item (index % List.length candidates) candidates)

    let private processes (world : World) : ProcessId list =
        world.NextName |> Map.keys |> List.ofSeq

    let private replace (task : Live) (world : World) : World =
        { world with
            Live =
                world.Live
                |> List.map (fun live -> if live.Serial = task.Serial then task else live)
        }

    /// The process `pid` ended: every task of it exits.
    let private ended (pid : ProcessId) (world : World) : World =
        let dying = world.Live |> List.filter (fun live -> live.Pid = pid)

        { world with
            Live = world.Live |> List.filter (fun live -> live.Pid <> pid)
            NextName = Map.remove pid world.NextName
            Events =
                (dying |> List.rev |> List.map (fun live -> Event.Exited live.Serial))
                @ world.Events
        }

    let private endOn (ended : EndedProcess<int, string>) (machine : SimulatedMachine<int, string>) =
        match SimulatedMachine.endProcess ended machine with
        | Ok (_, machine) -> machine
        | Error refusal -> failwith $"endProcess: %s{ProcessEndRefusal.describe refusal}"

    let private beyondCpus (count : int) : int list =
        [ -1 ; count ; count + 1 ; Int32.MinValue ; Int32.MaxValue ]

    let rec private apply (reached : Reached) (op : SchedulingOp) (world : World) : World =
        match op with
        | SchedulingOp.Launch cpu -> launch cpu world
        | SchedulingOp.Spawn (proc, cpu) ->
            match pick proc (processes world) with
            | Some pid when List.length world.Live < maxTasks ->
                let name = world.NextName.[pid]
                let cpu = CpuId (cpu % world.Count)

                let machine =
                    world.Machine
                    |> Machines.doIn
                        pid
                        (fun view ->
                            match UnixTaskLifecycle.spawn 0 name cpu view with
                            | Ok (SpawnAnswer.Spawned _, view) -> view
                            | other -> failwith $"spawn: %A{other}"
                        )

                { world with
                    Machine = machine
                    Live =
                        world.Live
                        @ [
                            {
                                Pid = pid
                                Name = name
                                Serial = world.NextSerial
                                Asleep = false
                                Woken = false
                            }
                        ]
                    NextName = Map.add pid (name + 1) world.NextName
                    NextSerial = world.NextSerial + 1
                    Events = Event.Created (world.NextSerial, cpu) :: world.Events
                }
            | Some _
            | None -> world
        | SchedulingOp.Dispatch (index, cpu, beyond) ->
            match pick index world.Live with
            | None -> world
            | Some task ->
                match beyond with
                | Some k ->
                    let cpus = beyondCpus world.Count
                    let cpu = CpuId (List.item (k % List.length cpus) cpus)
                    reached.BeyondDispatch <- reached.BeyondDispatch + 1

                    throws (fun () -> UnixScheduling.dispatch task.Name cpu (viewOf task.Pid world))
                    |> shouldEqual true

                    world
                | None ->
                    let cpu = CpuId (cpu % world.Count)

                    if task.Woken then
                        reached.WokenDispatched <- reached.WokenDispatched + 1

                    let displacesAnotherProcess =
                        world.Live
                        |> List.exists (fun other ->
                            other.Pid <> task.Pid && runningOracle other.Serial world.Events = Some cpu
                        )

                    if displacesAnotherProcess then
                        reached.CrossProcessDisplacement <- reached.CrossProcessDisplacement + 1

                    { world with
                        Machine = world.Machine |> Machines.doIn task.Pid (UnixScheduling.dispatch task.Name cpu)
                        Events = Event.Dispatched (task.Serial, cpu) :: world.Events
                    }
        | SchedulingOp.DispatchWoken (index, cpu) ->
            match pick index (world.Live |> List.filter (fun live -> live.Woken)) with
            | None -> world
            | Some task -> apply reached (SchedulingOp.Dispatch (List.findIndex ((=) task) world.Live, cpu, None)) world
        | SchedulingOp.Park proc ->
            let candidates =
                world.Live
                |> List.filter (fun live -> live.Name <> 0 && not live.Asleep && not live.Woken)

            match pick proc candidates with
            | None -> world
            | Some task ->
                let outcome, machine =
                    world.Machine
                    |> Machines.inProcess
                        task.Pid
                        (fun view ->
                            match UnixReadWrite.read task.Name 3 UserBuffer.Mapped 1UL view with
                            | Ok (ReadOutcome.WouldBlock _, view) -> true, view
                            | Ok (ReadOutcome.Answered (ReadAnswer.Completed bytes), view) when bytes.Length = 1 ->
                                false, view
                            | other -> failwith $"read: %A{other}"
                        )

                let world =
                    { world with
                        Machine = machine
                    }

                if outcome then
                    { replace
                          { task with
                              Asleep = true
                          }
                          world with
                        Events = Event.Parked task.Serial :: world.Events
                    }
                else
                    world
        | SchedulingOp.Wake proc ->
            match pick proc (processes world) with
            | None -> world
            | Some pid ->
                let machine =
                    world.Machine
                    |> Machines.doIn
                        pid
                        (fun view ->
                            match
                                WriteOutcomes.admitThenWrite 0 4 UserBuffer.Mapped (ImmutableArray.Create 1uy) view
                            with
                            | Ok (WriteOutcome.Returns (WriteAnswer.Completed 1L, view)) -> view
                            | other -> failwith $"write: %A{other}"
                        )

                let asleep =
                    world.Live
                    |> List.filter (fun live -> live.Asleep)
                    |> List.groupBy (fun live -> live.Pid)
                    |> List.map (fun (pid, tasks) -> pid, tasks |> List.map (fun live -> live.Name) |> Set.ofList)
                    |> Map.ofList

                let woken = SimulatedMachine.wakes asleep machine |> List.map fst |> Set.ofList

                { world with
                    Machine = machine
                    Live =
                        world.Live
                        |> List.map (fun live ->
                            if Set.contains (live.Pid, live.Name) woken then
                                { live with
                                    Asleep = false
                                    Woken = true
                                }
                            else
                                live
                        )
                }
        | SchedulingOp.Finish index ->
            match pick index (world.Live |> List.filter (fun live -> live.Woken)) with
            | None -> world
            | Some task ->
                let parksAgain, machine =
                    world.Machine
                    |> Machines.inProcess
                        task.Pid
                        (fun view ->
                            match UnixReadWrite.finishRead task.Name view with
                            | Ok (ReadOutcome.WouldBlock _, view) -> true, view
                            | Ok (ReadOutcome.Answered (ReadAnswer.Completed bytes), view) when bytes.Length = 1 ->
                                false, view
                            | other -> failwith $"finishRead: %A{other}"
                        )

                let world =
                    replace
                        { task with
                            Asleep = parksAgain
                            Woken = false
                        }
                        { world with
                            Machine = machine
                        }

                if parksAgain then
                    { world with
                        Events = Event.Parked task.Serial :: world.Events
                    }
                else
                    world
        | SchedulingOp.ExitThread index ->
            let darwin =
                SimulatedUnixPlatform.flavour world.Platform = SimulatedUnixFlavour.Darwin

            let candidates =
                world.Live
                |> List.filter (fun live ->
                    let alone =
                        world.Live |> List.forall (fun other -> other.Pid <> live.Pid || other = live)

                    not live.Asleep
                    && not live.Woken
                    && (live.Name <> 0 || alone)
                    && not (darwin && alone)
                )

            match pick index candidates with
            | None -> world
            | Some task ->
                match UnixTaskLifecycle.exitThread task.Name 0 (viewOf task.Pid world) with
                | Ok (TaskOutcome.Continues view) ->
                    { world with
                        Machine = SimulatedMachine.unfocus view world.Machine
                        Live = world.Live |> List.filter (fun live -> live.Serial <> task.Serial)
                        Events = Event.Exited task.Serial :: world.Events
                    }
                | Ok (TaskOutcome.ProcessEnded e) ->
                    { world with
                        Machine = endOn e world.Machine
                    }
                    |> ended task.Pid
                | Error refusal -> failwith $"exitThread: %s{ThreadExitRefusal.describe refusal}"
        | SchedulingOp.EndProcess proc ->
            match pick proc (processes world) with
            | None -> world
            | Some pid ->
                let e = UnixTaskLifecycle.exitGroup 0 0 (viewOf pid world)

                { world with
                    Machine = endOn e world.Machine
                }
                |> ended pid

    /// Everything the reference says, asked of the library.
    let private check (reached : Reached) (world : World) : unit =
        Machines.assertClean world.Machine

        let linux =
            SimulatedUnixPlatform.flavour world.Platform = SimulatedUnixFlavour.Linux

        for task in world.Live do
            let view = viewOf task.Pid world
            let running = runningOracle task.Serial world.Events
            let context = $"%A{task}, after %A{List.rev world.Events}"

            UnixScheduling.runningOn task.Name view
            |> fun answered ->
                if answered <> running then
                    failwith $"runningOn %A{answered}: %s{context}"

            (UnixTaskTable.get task.Name view.Tasks).Cpu
            |> shouldEqual (cpuOracle task.Serial world.Events)

            let parked = task.Asleep || task.Woken

            match running with
            | Some cpu ->
                if not (obj.ReferenceEquals (UnixScheduling.dispatch task.Name cpu view, view)) then
                    failwith $"re-dispatching to %O{cpu} changed the system: %s{context}"
            | None -> ()

            if linux then
                match running with
                | Some cpu when not parked ->
                    UnixScheduling.getcpu task.Name view
                    |> shouldEqual (
                        Ok
                            {
                                Cpu = cpu
                                Node = NumaNode 0
                            }
                    )
                | Some _
                | None ->
                    if not parked && lastDispatched task.Serial world.Events then
                        reached.DisplacedGetCpu <- reached.DisplacedGetCpu + 1

                    if not (throws (fun () -> UnixScheduling.getcpu task.Name view)) then
                        failwith $"getcpu answered for a task not running, or parked: %s{context}"
            else
                UnixScheduling.getcpu task.Name view
                |> shouldEqual (Error (GetCpuRefusal.UnmodelledFlavour SimulatedUnixFlavour.Darwin))

    let private op : Gen<SchedulingOp> =
        let index = Gen.choose (0, 1000)

        Gen.frequency
            [
                10, Gen.map2 (fun p c -> SchedulingOp.Spawn (p, c)) index index
                36, Gen.map3 (fun t c b -> SchedulingOp.Dispatch (t, c, None)) index index index
                4, Gen.map3 (fun t c b -> SchedulingOp.Dispatch (t, c, Some b)) index index index
                14, Gen.map2 (fun t c -> SchedulingOp.DispatchWoken (t, c)) index index
                12, Gen.map SchedulingOp.Park index
                10, Gen.map SchedulingOp.Wake index
                8, Gen.map SchedulingOp.Finish index
                6, Gen.map SchedulingOp.ExitThread index
                3, Gen.map SchedulingOp.EndProcess index
                7, Gen.map SchedulingOp.Launch index
            ]

    let private run : Gen<SimulatedUnixPlatform * int * int * int * SchedulingOp list> =
        gen {
            let! platform =
                Gen.frequency
                    [
                        3, Gen.constant SimulatedUnixPlatform.linuxX64
                        1, Gen.constant SimulatedUnixPlatform.macOsArm64
                    ]

            let! count = Gen.choose (1, 4)
            let! leaderCpu = Gen.choose (0, 1000)
            let! processes = Gen.choose (1, maxProcesses)
            let! length = Gen.choose (1, 60)
            let! ops = Gen.listOfLength length op
            return platform, count, leaderCpu, processes, ops
        }

    [<Test>]
    let ``what runs where agrees with the history of dispatches, parks and exits`` () : unit =
        let reached =
            {
                DisplacedGetCpu = 0
                WokenDispatched = 0
                CrossProcessDisplacement = 0
                BeyondDispatch = 0
            }

        let property (platform : SimulatedUnixPlatform, count : int, leaderCpu : int, processes : int, ops) : unit =
            let world =
                (initialWorld platform count leaderCpu, [ 2..processes ])
                ||> List.fold (fun world p -> launch (leaderCpu + p) world)

            check reached world

            (world, ops)
            ||> List.fold (fun world op ->
                let world = apply reached op world
                check reached world
                world
            )
            |> ignore<World>

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen run) property)

        printfn $"%A{reached}"

        // Eight runs of 300 reached at least 3247 displaced getcpus, 61 woken
        // dispatches (122 once `DispatchWoken` had its present weight), 554
        // displacements across processes and 260 dispatches beyond the
        // machine. Each floor is a third of that or less, so a generator change
        // that starves one fails here rather than passing on the others.
        let floors =
            [
                "a displaced task's getcpu", reached.DisplacedGetCpu, 1000
                "a woken task dispatched while parked", reached.WokenDispatched, 40
                "a displacement across processes", reached.CrossProcessDisplacement, 180
                "a dispatch beyond the machine", reached.BeyondDispatch, 85
            ]

        for name, count, floor in floors do
            if count < floor then
                failwith $"%s{name}: reached %d{count} times over the run, below the floor of %d{floor}"

    [<Test>]
    let ``two processes dispatched onto one processor in turn displace each other`` () : unit =
        // The shape a client with one global interleaving produces: one task
        // per step, from whichever process, onto the same processor indices.
        let pids, machine = Machines.withTasks SimulatedUnixPlatform.linuxX64 2 1

        let first, second =
            match pids with
            | [ first ; second ] -> first, second
            | other -> failwith $"expected two processes, got %A{other}"

        let runningOn (pid : ProcessId) (machine : SimulatedMachine<int, string>) : CpuId option =
            UnixScheduling.runningOn 0 (Machines.viewOf pid machine)

        let getcpu (pid : ProcessId) (machine : SimulatedMachine<int, string>) : bool =
            not (throws (fun () -> UnixScheduling.getcpu 0 (Machines.viewOf pid machine)))

        let mutable machine = machine

        for round in 1..3 do
            for pid, other in [ first, second ; second, first ] do
                machine <- Machines.doIn pid (UnixScheduling.dispatch 0 (CpuId 0)) machine
                runningOn pid machine |> shouldEqual (Some (CpuId 0))
                runningOn other machine |> shouldEqual None
                getcpu pid machine |> shouldEqual true
                getcpu other machine |> shouldEqual false
                // The displaced task keeps the processor as its own.
                (UnixTaskTable.get 0 (Machines.viewOf other machine).Tasks).Cpu
                |> shouldEqual (CpuId 0)

                Machines.assertClean machine

    [<Test>]
    let ``a dispatch elsewhere leaves the task's previous processor idle`` () : unit =
        let system : UnixSystem<int, string> =
            UnixSystem.initial SimulatedUnixPlatform.linuxX64
            |> UnixBootImage.withProcessorCount 2
            |> Configured.expectOk ProcessorCountRefusal.describe
            |> Launched.boot UnixSystem.pipedStandardStreams 0 (CpuId 0)
            |> Tasks.spawn 1

        let system =
            system
            |> UnixScheduling.dispatch 0 (CpuId 0)
            |> UnixScheduling.dispatch 0 (CpuId 1)

        system.Machine.Occupants
        |> shouldEqual (Map.ofList [ CpuId 1, (UnixTaskTable.get 0 system.Tasks).OsThreadId ])

        UnixScheduling.runningOn 0 system |> shouldEqual (Some (CpuId 1))
        UnixScheduling.runningOn 1 system |> shouldEqual None
        UnixSystem.checkInvariants system |> shouldEqual []

    [<Test>]
    let ``getcpu is refused under Darwin, which has none`` () : unit =
        let system : UnixSystem<int, string> =
            UnixSystem.initial SimulatedUnixPlatform.macOsArm64
            |> Launched.boot UnixSystem.pipedStandardStreams 0 (CpuId 0)
            |> UnixScheduling.dispatch 0 (CpuId 0)

        UnixScheduling.getcpu 0 system
        |> shouldEqual (Error (GetCpuRefusal.UnmodelledFlavour SimulatedUnixFlavour.Darwin))

        UnixScheduling.runningOn 0 system |> shouldEqual (Some (CpuId 0))

    [<Test>]
    let ``runningOn answers None for a name that is no task`` () : unit =
        let system : UnixSystem<int, string> =
            UnixSystem.initial SimulatedUnixPlatform.linuxX64
            |> Launched.boot UnixSystem.pipedStandardStreams 0 (CpuId 0)
            |> UnixScheduling.dispatch 0 (CpuId 0)

        UnixScheduling.runningOn 7 system |> shouldEqual None

    // --- the machine's record, forged ---

    /// `machine` with the processors' occupants replaced by `occupants`, as no
    /// public route can do.
    let private withOccupants
        (occupants : (CpuId * OsThreadId) list)
        (machine : SimulatedMachine<int, string>)
        : SimulatedMachine<int, string>
        =
        { machine with
            Machine =
                { machine.Machine with
                    Occupants = Map.ofList occupants
                }
        }

    let private threadIdOf (pid : ProcessId) (task : int) (machine : SimulatedMachine<int, string>) : OsThreadId =
        (UnixTaskTable.get task (Machines.viewOf pid machine).Tasks).OsThreadId

    [<Test>]
    let ``checkInvariants reports an occupied processor the machine does not have`` () : unit =
        let pids, machine =
            Machines.withTasksOn
                (UnixBootImage.withProcessorCount 2
                 >> Configured.expectOk ProcessorCountRefusal.describe)
                SimulatedUnixPlatform.linuxX64
                1
                1

        let pid = List.exactlyOne pids
        let leader = threadIdOf pid 0 machine
        let forged = withOccupants [ CpuId 2, leader ] machine

        let defects = SimulatedMachine.checkInvariants forged

        defects
        |> List.contains (
            SimulatedMachineDefect.Machine (UnixSystemDefect.OccupiedCpuBeyondMachine (CpuId 2, leader, 2))
        )
        |> shouldEqual true

    [<Test>]
    let ``checkInvariants reports an occupant no live task is, and only where it can tell`` () : unit =
        let pids, machine = Machines.withTasks SimulatedUnixPlatform.linuxX64 2 1

        let first, second =
            match pids with
            | [ first ; second ] -> first, second
            | other -> failwith $"expected two processes, got %A{other}"

        let secondLeader = threadIdOf second 0 machine

        // The thread ID of a task that has exited.
        let nobody, machine =
            let machine = Machines.doIn first (Tasks.spawn 1) machine
            let nobody = threadIdOf first 1 machine

            let exited =
                Machines.doIn
                    first
                    (fun view ->
                        match UnixTaskLifecycle.exitThread 1 0 view with
                        | Ok (TaskOutcome.Continues view) -> view
                        | other -> failwith $"exitThread: %A{other}"
                    )
                    machine

            nobody, exited

        // A thread ID no task holds is reported by the machine and by each
        // view.
        let forged = withOccupants [ CpuId 0, nobody ] machine
        let defect = UnixSystemDefect.OccupantWithoutTask (CpuId 0, nobody)

        SimulatedMachine.checkInvariants forged
        |> shouldEqual [ SimulatedMachineDefect.Machine defect ]

        UnixSystem.checkInvariants (Machines.viewOf first forged)
        |> shouldEqual [ defect ]

        // Another process's running task is no defect in a view that cannot
        // see that process's tasks.
        let shared = withOccupants [ CpuId 0, secondLeader ] machine
        SimulatedMachine.checkInvariants shared |> shouldEqual []
        UnixSystem.checkInvariants (Machines.viewOf first shared) |> shouldEqual []
        UnixSystem.checkInvariants (Machines.viewOf second shared) |> shouldEqual []

    [<Test>]
    let ``checkInvariants reports a running task whose processor is another`` () : unit =
        let pids, machine =
            Machines.withTasksOn
                (UnixBootImage.withProcessorCount 2
                 >> Configured.expectOk ProcessorCountRefusal.describe)
                SimulatedUnixPlatform.linuxX64
                1
                1

        let pid = List.exactlyOne pids
        // The leader was created on processor 0.
        let forged = withOccupants [ CpuId 1, threadIdOf pid 0 machine ] machine

        SimulatedMachine.checkInvariants forged
        |> shouldEqual
            [
                SimulatedMachineDefect.Machine (UnixSystemDefect.OccupantOnAnotherCpu (0, CpuId 1, CpuId 0))
            ]

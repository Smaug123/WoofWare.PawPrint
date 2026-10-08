namespace WoofWare.PosixKernel.Test

open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// Tasks joining and leaving a process, and the process ending:
/// `UnixTaskLifecycle.spawn`, `UnixTaskLifecycle.exitThread` and
/// `UnixTaskLifecycle.exitGroup`. Every system here has task 0 as its leader.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestUnixTaskLifecycle =

    let private platforms : SimulatedUnixPlatform list =
        [ SimulatedUnixPlatform.linuxX64 ; SimulatedUnixPlatform.macOsArm64 ]

    let private withTask (name : int) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        Tasks.ensure name system

    let private mapSignals
        (f : SignalState<int, string> -> SignalState<int, string>)
        (system : UnixSystem<int, string>)
        : UnixSystem<int, string>
        =
        { system with
            Process =
                { system.Process with
                    Signals = f system.Process.Signals
                }
        }

    /// A system with one epoll instance (a kqueue on Darwin), which a task can park on
    /// in `flock`, and that instance's description.
    let private world (platform : SimulatedUnixPlatform) : UnixSystem<int, string> * OpenFileDescriptionId =
        let system =
            UnixSystem.initial<int, string> platform
            |> Launched.boot UnixSystem.pipedStandardStreams 0 (CpuId 0)

        let create =
            match SimulatedUnixPlatform.flavour system.Machine.UnixPlatform with
            | SimulatedUnixFlavour.Linux -> FileDescriptorRegistry.createEpoll
            | SimulatedUnixFlavour.Darwin -> FileDescriptorRegistry.createKqueue system.Process.ProcessId

        let fd, registry = create (UnixSystemState.fileDescriptors system)

        let id =
            match FileDescriptorRegistry.tryFindWithId fd registry with
            | Some (id, _) -> id
            | None -> failwith $"fd %d{fd} names no description"

        UnixSystemState.withFileDescriptors registry system, id

    let private flockOn (queue : OpenFileDescriptionId) : ParkedSyscall =
        ParkedSyscall.Flock
            {
                Requester = queue
                Mode = FlockMode.Exclusive
            }

    let private exitOrFail (task : int) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        match UnixTaskLifecycle.exitThread task 0 system with
        | Ok (TaskOutcome.Continues system) -> system
        | Ok (TaskOutcome.ProcessEnded ended) ->
            failwith $"expected the process to carry on, but it ended: %O{ended.Termination}"
        | Error refusal -> failwith $"expected the exit to be answered, got: %s{ThreadExitRefusal.describe refusal}"

    /// The per-task entries the process holds, each of which must name a live task.
    let private perTaskEntries (proc : UnixProcessState<int, string>) : string list =
        let masks =
            SignalState.tasksWithFrames proc.Signals
            |> Set.toList
            |> List.map (fun t -> $"frames of %d{t}")

        let pending =
            SignalState.pending proc.Signals
            |> List.choose (fun entry ->
                match entry.Target with
                | ValueSome t -> Some $"%O{entry.Signal} pending on %d{t}"
                | ValueNone -> None
            )

        masks @ pending

    [<Test>]
    let ``a thread's own pending signals are discarded at its exit, and the process's stay`` () : unit =
        // Measured on Linux 6.18.5 and Darwin 27.0.0 by
        // `docs/plans/2026-08-23-posix-kernel-extraction/thread-exit-pending.c`: a signal
        // pending on an exiting thread alone is never delivered afterwards, nor pending on
        // any other thread, whether the others block it or not.
        for platform in platforms do
            let system, _ = world platform

            let system =
                system
                |> withTask 1
                |> withTask 2
                |> HandlerFrames.enterIn "h" 1 (Set.ofList [ Signal.SIGUSR1 ; Signal.SIGUSR2 ])
                |> HandlerFrames.enterIn "h" 2 (Set.singleton Signal.SIGUSR2)
                |> mapSignals (
                    SignalState.enqueue
                        {
                            Signal = Signal.SIGUSR1
                            Target = ValueSome 1
                        }
                )
                |> mapSignals (
                    SignalState.enqueue
                        {
                            Signal = Signal.SIGUSR2
                            Target = ValueNone
                        }
                )
                |> mapSignals (
                    SignalState.enqueue
                        {
                            Signal = Signal.SIGUSR2
                            Target = ValueSome 2
                        }
                )

            let after = exitOrFail 1 system

            after.Tasks |> Map.keys |> List.ofSeq |> shouldEqual [ 0 ; 2 ]

            SignalState.pending after.Process.Signals
            |> shouldEqual
                [
                    {
                        Signal = Signal.SIGUSR2
                        Target = ValueNone
                    }
                    {
                        Signal = Signal.SIGUSR2
                        Target = ValueSome 2
                    }
                ]

            SignalState.tasksWithFrames after.Process.Signals
            |> shouldEqual (Set.singleton 2)

            SignalState.maskOf 2 after.Process.Signals
            |> shouldEqual (Set.singleton Signal.SIGUSR2)

            UnixSystem.checkInvariants after |> shouldEqual []

    [<Test>]
    let ``a parked task's exit is refused, and names the park`` () : unit =
        for platform in platforms do
            let system, queue = world platform

            let system = system |> withTask 1 |> withTask 2 |> UnixWait.park 2 (flockOn queue)

            let park =
                match UnixTaskTable.parkOf 2 system.Tasks with
                | Some park -> park
                | None -> failwith "expected task 2 to be parked"

            UnixTaskLifecycle.exitThread 2 0 system
            |> shouldEqual (Error (ThreadExitRefusal.Parked (2, park)))

    /// The leader and one other task, which leaves, so that what follows is about
    /// being *last* rather than about being alone from the start. The leader, which
    /// stays, blocks a signal and has one pending on it alone.
    let private lastTaskStanding (platform : SimulatedUnixPlatform) : UnixSystem<int, string> =
        let system, _ = world platform

        system
        |> withTask 1
        |> exitOrFail 1
        |> HandlerFrames.enterIn "h" 0 (Set.singleton Signal.SIGUSR1)
        |> mapSignals (
            SignalState.enqueue
                {
                    Signal = Signal.SIGUSR1
                    Target = ValueSome 0
                }
        )

    [<Test>]
    let ``on Linux the last task's exit ends the process, with that task's status`` () : unit =
        // Measured on Linux 6.18.5 (aarch64 and x86-64) by
        // `docs/plans/2026-08-23-posix-kernel-extraction/last-thread-exit-status.c`: the
        // raw thread-exit syscall of the last thread ends the process, with its own
        // argument's low 8 bits. The last thread here is the leader, since a leader
        // cannot exit before the others.
        for platform in [ SimulatedUnixPlatform.linuxX64 ; SimulatedUnixPlatform.linuxArm64 ] do
            let system = lastTaskStanding platform

            for status, kept in [ 0, 0 ; 5, 5 ; 256, 0 ; 263, 7 ; -1, 255 ; System.Int32.MinValue, 0 ] do
                match UnixTaskLifecycle.exitThread 0 status system with
                | Ok (TaskOutcome.ProcessEnded ended) ->
                    match ended.Termination with
                    | ProcessTermination.Exited exitStatus ->
                        (status, ExitStatus.waitidStatus exitStatus) |> shouldEqual (status, kept)
                    | ProcessTermination.Signaled _ -> failwith $"expected an exit, got %O{ended.Termination}"

                    EndedMachine.assertTasksGone system ended
                    perTaskEntries ended.FinalProcess |> shouldEqual []

                    { ended.FinalProcess with
                        Signals = system.Process.Signals
                    }
                    |> shouldEqual system.Process
                | other -> failwith $"expected the process to end, got %A{other}"

    [<Test>]
    let ``on Darwin the last task's exit is refused`` () : unit =
        UnixTaskLifecycle.exitThread 0 0 (lastTaskStanding SimulatedUnixPlatform.macOsArm64)
        |> shouldEqual (Error (ThreadExitRefusal.LastTaskOnDarwin 0))

    [<Test>]
    let ``the leader's exit is refused while another task lives, and answered once it is last`` () : unit =
        // Measured on Linux 6.18.5 and Darwin 27.0.0 by
        // `docs/plans/2026-08-23-posix-kernel-extraction/leader-exits-first.c`: the
        // process carries on without its leader, which this library does not model.
        for platform in platforms do
            let system, _ = world platform
            let system = system |> withTask 1 |> withTask 2

            UnixTaskLifecycle.exitThread 0 0 system
            |> shouldEqual (Error (ThreadExitRefusal.LeaderBeforeOthers 0))

            let system = system |> exitOrFail 1

            UnixTaskLifecycle.exitThread 0 0 system
            |> shouldEqual (Error (ThreadExitRefusal.LeaderBeforeOthers 0))

            let system = system |> exitOrFail 2

            match SimulatedUnixPlatform.flavour platform, UnixTaskLifecycle.exitThread 0 0 system with
            | SimulatedUnixFlavour.Linux, Ok (TaskOutcome.ProcessEnded _) -> ()
            | SimulatedUnixFlavour.Darwin, Error (ThreadExitRefusal.LastTaskOnDarwin 0) -> ()
            | flavour, other ->
                failwith $"%O{flavour}: expected the lone leader's exit to be the last task's, got %A{other}"

    [<Test>]
    let ``a parked leader's exit is refused as parked`` () : unit =
        // The park is the first thing a task in a syscall is refused for, whoever it is.
        for platform in platforms do
            let system, queue = world platform
            let system = system |> withTask 1 |> UnixWait.park 0 (flockOn queue)
            let park = UnixTaskTable.parkOf 0 system.Tasks |> Option.get

            UnixTaskLifecycle.exitThread 0 0 system
            |> shouldEqual (Error (ThreadExitRefusal.Parked (0, park)))

    [<Test>]
    let ``exit_group ends the process with the flavour's status, parked tasks included`` () : unit =
        for platform in platforms do
            let system, queue = world platform

            let system =
                system
                |> withTask 1
                |> withTask 2
                |> withTask 3
                |> UnixWait.park 3 (flockOn queue)
                |> HandlerFrames.enterIn "h" 2 (Set.singleton Signal.SIGUSR2)
                |> mapSignals (
                    SignalState.enqueue
                        {
                            Signal = Signal.SIGUSR2
                            Target = ValueSome 2
                        }
                )
                |> mapSignals (
                    SignalState.enqueue
                        {
                            Signal = Signal.SIGUSR1
                            Target = ValueNone
                        }
                )

            let ended = UnixTaskLifecycle.exitGroup 1 257 system

            let expected =
                match SimulatedUnixPlatform.flavour platform with
                | SimulatedUnixFlavour.Linux -> 1
                | SimulatedUnixFlavour.Darwin -> 257

            match ended.Termination with
            | ProcessTermination.Exited status -> ExitStatus.waitidStatus status |> shouldEqual expected
            | ProcessTermination.Signaled _ -> failwith $"expected an exit, got %O{ended.Termination}"

            EndedMachine.assertTasksGone system ended
            perTaskEntries ended.FinalProcess |> shouldEqual []

            // The process-directed signal is the process's, not a task's.
            SignalState.pending ended.FinalProcess.Signals
            |> shouldEqual
                [
                    {
                        Signal = Signal.SIGUSR1
                        Target = ValueNone
                    }
                ]

    [<Test>]
    let ``exit_group from a parked task, or one that was never registered, fails loudly`` () : unit =
        let system, queue = world SimulatedUnixPlatform.linuxX64
        let system = system |> withTask 1 |> withTask 2 |> UnixWait.park 2 (flockOn queue)

        let parked =
            Assert.Throws<exn> (fun () -> UnixTaskLifecycle.exitGroup 2 0 system |> ignore<EndedProcess<int, string>>)

        parked.Message |> shouldContainText "parked"

        let unknown =
            Assert.Throws<exn> (fun () -> UnixTaskLifecycle.exitGroup 3 0 system |> ignore<EndedProcess<int, string>>)

        unknown.Message |> shouldContainText "names no task"

    [<Test>]
    let ``exiting a task that was never registered fails loudly`` () : unit =
        let system, _ = world SimulatedUnixPlatform.linuxX64
        let system = system |> withTask 1 |> withTask 2

        let exn =
            Assert.Throws<exn> (fun () ->
                UnixTaskLifecycle.exitThread 3 0 system
                |> ignore<Result<TaskOutcome<int, string>, ThreadExitRefusal<int>>>
            )

        exn.Message |> shouldContainText "names no task"

    [<RequireQualifiedAccess>]
    type private Op =
        | Spawn of parent : int * child : int
        | Block of task : int * Signal
        | Unblock of task : int * Signal
        | EnqueueOnTask of task : int * Signal
        | EnqueueOnProcess of Signal
        | Park of task : int
        | Unpark of task : int
        | Exit of task : int * status : int32
        | ExitGroup of task : int * status : int32

    let private opGen : Gen<Op> =
        let task = Gen.choose (0, 4)

        // SIGCHLD's default is to ignore it, so it exercises both flavours' rules for an
        // ignored signal that is blocked; the rest terminate by default.
        let signal =
            Gen.elements
                [
                    Signal.SIGUSR1
                    Signal.SIGUSR2
                    Signal.SIGTERM
                    Signal.SIGHUP
                    Signal.SIGCHLD
                ]

        // Weighted towards the edges of the bits either flavour keeps.
        let status =
            Gen.oneof
                [
                    Gen.choose (-3, 260)
                    Gen.elements
                        [
                            0xffffff
                            0x1000000
                            0x1000007
                            System.Int32.MaxValue
                            System.Int32.MinValue
                        ]
                    ArbMap.defaults |> ArbMap.generate<int32>
                ]

        Gen.frequency
            [
                4, Gen.zip task task |> Gen.map Op.Spawn
                3, Gen.zip task signal |> Gen.map Op.Block
                1, Gen.zip task signal |> Gen.map Op.Unblock
                3, Gen.zip task signal |> Gen.map Op.EnqueueOnTask
                1, signal |> Gen.map Op.EnqueueOnProcess
                1, task |> Gen.map Op.Park
                1, task |> Gen.map Op.Unpark
                3, Gen.zip task status |> Gen.map Op.Exit
                1, Gen.zip task status |> Gen.map Op.ExitGroup
            ]

    /// What the table must say about which tasks exist, kept by a model that knows
    /// nothing of signals: the tasks spawned and not since exited, and the ones of
    /// those that are parked.
    type private Model =
        {
            Live : Set<int>
            Parked : Set<int>
        }

    /// Coverage of the paths the property exists for, so that a generator change which
    /// stops reaching one is noticed.
    type private Coverage =
        {
            mutable ExitsDroppingAMask : int
            mutable ExitsDroppingOwnPending : int
            mutable ExitsKeepingProcessPending : int
            mutable RefusedParked : int
            mutable RefusedLeaderFirst : int
            mutable SpawnsRefusedForAMask : int
            mutable RefusedLast : int
            mutable EndedByLastExit : int
            mutable EndedByExitGroup : int
            mutable EndedWithParkedTask : int
        }

    /// The per-task entries the system holds, each of which must name a task.
    let private orphans (system : UnixSystem<int, string>) : string list =
        let tasks = system.Tasks |> Map.keys |> Set.ofSeq

        let masks =
            Set.difference (SignalState.tasksWithFrames system.Process.Signals) tasks
            |> Set.toList
            |> List.map (fun t -> $"frames of %d{t}")

        let pending =
            SignalState.pending system.Process.Signals
            |> List.choose (fun entry ->
                match entry.Target with
                | ValueSome t when not (Set.contains t tasks) -> Some $"%O{entry.Signal} pending on %d{t}"
                | ValueSome _
                | ValueNone -> None
            )

        masks @ pending

    /// What the flavour keeps of an exit status, written out independently of
    /// `ExitStatus.ofExitArgument`.
    let private kept (platform : SimulatedUnixPlatform) (status : int32) : int32 =
        match SimulatedUnixPlatform.flavour platform with
        | SimulatedUnixFlavour.Linux -> status &&& 0xff
        | SimulatedUnixFlavour.Darwin -> status &&& 0xffffff

    /// Check a process that ended: it exited with what the flavour keeps of `status`,
    /// on the machine it ran on, and holds nothing for any task.
    let private assertExited
        (platform : SimulatedUnixPlatform)
        (status : int32)
        (before : UnixSystem<int, string>)
        (ended : EndedProcess<int, string>)
        : unit
        =
        match ended.Termination with
        | ProcessTermination.Exited exitStatus ->
            ExitStatus.waitidStatus exitStatus |> shouldEqual (kept platform status)
        | ProcessTermination.Signaled _ -> failwith $"expected an exit, got %O{ended.Termination}"

        EndedMachine.assertTasksGone before ended
        perTaskEntries ended.FinalProcess |> shouldEqual []

        SignalState.pending ended.FinalProcess.Signals
        |> shouldEqual (
            SignalState.pending before.Process.Signals
            |> List.filter (fun entry -> entry.Target = ValueNone)
        )

        SignalState.dispositions ended.FinalProcess.Signals
        |> shouldEqual (SignalState.dispositions before.Process.Signals)

        { ended.FinalProcess with
            Signals = before.Process.Signals
        }
        |> shouldEqual before.Process

    let private runProperty (platform : SimulatedUnixPlatform) (coverage : Coverage) : unit =
        let initial, queue = world platform

        let step ((system, model) : UnixSystem<int, string> * Model) (op : Op) : UnixSystem<int, string> * Model =
            let live (task : int) = Set.contains task model.Live

            let after, model' =
                match op with
                | Op.Spawn (parent, child) when
                    live parent
                    && not (Set.contains parent model.Parked)
                    && not (live child)
                    && not (Set.isEmpty (SignalState.maskOf parent system.Process.Signals))
                    ->
                    // A mask is held only as handler frames, which a new task
                    // cannot inherit, so a spawn from a task that blocks
                    // anything is refused. The refusal carries no system, so
                    // the run goes on from this one: no task was added, and no
                    // thread ID handed out.
                    UnixTaskLifecycle.spawn parent child (CpuId 0) system
                    |> shouldEqual (
                        Error (
                            SpawnRefusal.InheritedHandlerMask (parent, SignalState.maskOf parent system.Process.Signals)
                        )
                    )

                    coverage.SpawnsRefusedForAMask <- coverage.SpawnsRefusedForAMask + 1
                    system, model
                | Op.Spawn (parent, child) when
                    live parent && not (Set.contains parent model.Parked) && not (live child)
                    ->
                    let after =
                        match UnixTaskLifecycle.spawn parent child (CpuId 0) system with
                        | Ok (SpawnAnswer.Spawned _, after) -> after
                        | Ok (SpawnAnswer.Failed error, _) ->
                            failwith $"spawning %d{child} from %d{parent} failed with %O{error}"
                        | Error refusal ->
                            failwith
                                $"spawning %d{child} from %d{parent} was refused: %s{SpawnRefusal.describe refusal}"

                    // The child starts with its parent's mask, which is empty,
                    // and nothing pending on it.
                    SignalState.maskOf child after.Process.Signals |> shouldEqual Set.empty
                    SignalState.framesOf child after.Process.Signals |> shouldEqual []

                    SignalState.pending after.Process.Signals
                    |> shouldEqual (SignalState.pending system.Process.Signals)

                    after,
                    { model with
                        Live = Set.add child model.Live
                    }
                | Op.Block (task, signal) when live task ->
                    let tasks = system.Tasks |> Map.keys |> Set.ofSeq

                    match
                        HandlerFrames.tryEnter
                            "h"
                            system.Leader
                            tasks
                            task
                            (Set.singleton signal)
                            system.Process.Signals
                    with
                    | Some signals -> mapSignals (fun _ -> signals) system, model
                    | None -> system, model
                | Op.Unblock (task, _) when
                    live task
                    && not (List.isEmpty (SignalState.framesOf task system.Process.Signals))
                    ->
                    mapSignals (HandlerFrames.leave task) system, model
                | Op.EnqueueOnTask (task, signal) when live task ->
                    mapSignals
                        (SignalState.enqueue
                            {
                                Signal = signal
                                Target = ValueSome task
                            })
                        system,
                    model
                | Op.EnqueueOnProcess signal ->
                    mapSignals
                        (SignalState.enqueue
                            {
                                Signal = signal
                                Target = ValueNone
                            })
                        system,
                    model
                | Op.Park task when live task ->
                    UnixWait.park task (flockOn queue) system,
                    { model with
                        Parked = Set.add task model.Parked
                    }
                | Op.Unpark task when live task ->
                    UnixParkState.unpark task system,
                    { model with
                        Parked = Set.remove task model.Parked
                    }
                | Op.ExitGroup (task, status) when live task && not (Set.contains task model.Parked) ->
                    // An ended process has no state to carry on with, so the run goes on
                    // from the one it ended in, as if the call had not been made.
                    let ended = UnixTaskLifecycle.exitGroup task status system
                    assertExited platform status system ended
                    coverage.EndedByExitGroup <- coverage.EndedByExitGroup + 1

                    if not model.Parked.IsEmpty then
                        coverage.EndedWithParkedTask <- coverage.EndedWithParkedTask + 1

                    system, model
                | Op.Exit (task, status) when live task ->
                    let result = UnixTaskLifecycle.exitThread task status system

                    if Set.contains task model.Parked then
                        let park = UnixTaskTable.parkOf task system.Tasks |> Option.get
                        result |> shouldEqual (Error (ThreadExitRefusal.Parked (task, park)))
                        coverage.RefusedParked <- coverage.RefusedParked + 1
                        system, model
                    elif task = 0 && model.Live.Count > 1 then
                        result |> shouldEqual (Error (ThreadExitRefusal.LeaderBeforeOthers task))
                        coverage.RefusedLeaderFirst <- coverage.RefusedLeaderFirst + 1
                        system, model
                    elif model.Live.Count = 1 then
                        match SimulatedUnixPlatform.flavour platform with
                        | SimulatedUnixFlavour.Linux ->
                            match result with
                            | Ok (TaskOutcome.ProcessEnded ended) -> assertExited platform status system ended
                            | other -> failwith $"expected the last task's exit to end the process, got %A{other}"

                            coverage.EndedByLastExit <- coverage.EndedByLastExit + 1
                        | SimulatedUnixFlavour.Darwin ->
                            result |> shouldEqual (Error (ThreadExitRefusal.LastTaskOnDarwin task))
                            coverage.RefusedLast <- coverage.RefusedLast + 1

                        // As for `ExitGroup`: the run goes on from the state before.
                        system, model
                    else
                        let after =
                            match result with
                            | Ok (TaskOutcome.Continues after) -> after
                            | Ok (TaskOutcome.ProcessEnded ended) ->
                                failwith
                                    $"expected task %d{task}'s exit to leave the process running, but it ended: %O{ended.Termination}"
                            | Error refusal ->
                                failwith
                                    $"expected task %d{task}'s exit to be answered, got: %s{ThreadExitRefusal.describe refusal}"

                        let signalsBefore = system.Process.Signals
                        let signalsAfter = after.Process.Signals

                        // Everything held for `task` alone goes, and nothing else changes.
                        SignalState.pending signalsAfter
                        |> shouldEqual (
                            SignalState.pending signalsBefore
                            |> List.filter (fun entry -> entry.Target <> ValueSome task)
                        )

                        SignalState.tasksWithFrames signalsAfter
                        |> shouldEqual (Set.remove task (SignalState.tasksWithFrames signalsBefore))

                        for other in Set.remove task model.Live do
                            SignalState.framesOf other signalsAfter
                            |> shouldEqual (SignalState.framesOf other signalsBefore)

                        SignalState.dispositions signalsAfter
                        |> shouldEqual (SignalState.dispositions signalsBefore)

                        after.Tasks |> shouldEqual (Map.remove task system.Tasks)

                        // The task's thread ID is freed, and nothing else on the
                        // machine moves.
                        ThreadIdAllocator.live after.Machine.ThreadIds
                        |> shouldEqual (
                            Set.remove
                                (UnixTaskTable.osThreadIdOf task system.Tasks)
                                (ThreadIdAllocator.live system.Machine.ThreadIds)
                        )

                        { after.Machine with
                            ThreadIds = system.Machine.ThreadIds
                        }
                        |> shouldEqual system.Machine

                        { after.Process with
                            Signals = signalsBefore
                        }
                        |> shouldEqual system.Process

                        if not (SignalState.maskOf task signalsBefore).IsEmpty then
                            coverage.ExitsDroppingAMask <- coverage.ExitsDroppingAMask + 1

                        if
                            SignalState.pending signalsBefore
                            |> List.exists (fun entry -> entry.Target = ValueSome task)
                        then
                            coverage.ExitsDroppingOwnPending <- coverage.ExitsDroppingOwnPending + 1

                        if
                            SignalState.pending signalsBefore
                            |> List.exists (fun entry -> entry.Target = ValueNone)
                        then
                            coverage.ExitsKeepingProcessPending <- coverage.ExitsKeepingProcessPending + 1

                        after,
                        { model with
                            Live = Set.remove task model.Live
                        }
                // Every other op names a task that does not exist, or spawns from a
                // parked task or onto a live one, which no client does.
                | Op.Spawn _
                | Op.Block _
                | Op.Unblock _
                | Op.EnqueueOnTask _
                | Op.Park _
                | Op.Unpark _
                | Op.Exit _
                | Op.ExitGroup _ -> system, model

            // The task set is exactly the model's, and no per-task entry outlives its task.
            after.Tasks |> Map.keys |> Set.ofSeq |> shouldEqual model'.Live
            orphans after |> shouldEqual []
            UnixSystem.checkInvariants after |> shouldEqual []

            after, model'

        let property =
            Prop.forAll (Arb.fromGen (Gen.listOf opGen))
            <| fun ops ->
                ((initial,
                  {
                      Live = Set.singleton 0
                      Parked = Set.empty
                  }),
                 ops)
                ||> List.fold step
                |> ignore<UnixSystem<int, string> * Model>

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 1000, property)

    [<TestCaseSource(nameof platforms)>]
    let ``the task set follows spawns and exits, no per-task entry outlives its task, and the process ends as its flavour says``
        (platform : SimulatedUnixPlatform)
        : unit
        =
        let coverage =
            {
                ExitsDroppingAMask = 0
                ExitsDroppingOwnPending = 0
                ExitsKeepingProcessPending = 0
                RefusedParked = 0
                RefusedLeaderFirst = 0
                SpawnsRefusedForAMask = 0
                RefusedLast = 0
                EndedByLastExit = 0
                EndedByExitGroup = 0
                EndedWithParkedTask = 0
            }

        runProperty platform coverage

        // Each floor sits at least four standard deviations below the count that
        // 1000 cases reach, so it fails when a generator change stops reaching the
        // path, not by chance. Measured over 30 runs per flavour, the rarest are
        // `EndedWithParkedTask`, at a mean of 30 and a standard deviation of 7,
        // and `ExitsDroppingAMask`, at 55 and 9: a task has a mask only while
        // it is inside a handler, which takes a delivery to put it there.
        coverage.ExitsDroppingAMask |> shouldBeGreaterThan 20
        coverage.ExitsDroppingOwnPending |> shouldBeGreaterThan 20
        coverage.ExitsKeepingProcessPending |> shouldBeGreaterThan 20
        coverage.RefusedParked |> shouldBeGreaterThan 20
        coverage.RefusedLeaderFirst |> shouldBeGreaterThan 20
        coverage.SpawnsRefusedForAMask |> shouldBeGreaterThan 20
        coverage.EndedByExitGroup |> shouldBeGreaterThan 50
        coverage.EndedWithParkedTask |> shouldBeGreaterThan 3

        match SimulatedUnixPlatform.flavour platform with
        | SimulatedUnixFlavour.Linux ->
            coverage.EndedByLastExit |> shouldBeGreaterThan 20
            coverage.RefusedLast |> shouldEqual 0
        | SimulatedUnixFlavour.Darwin ->
            coverage.RefusedLast |> shouldBeGreaterThan 20
            coverage.EndedByLastExit |> shouldEqual 0

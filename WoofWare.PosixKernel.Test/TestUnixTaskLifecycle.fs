namespace WoofWare.PosixKernel.Test

open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// Tasks leaving a process: `UnixTaskLifecycle.exitThread`.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestUnixTaskLifecycle =

    let private platforms : SimulatedUnixPlatform list =
        [ SimulatedUnixPlatform.linuxX64 ; SimulatedUnixPlatform.macOsArm64 ]

    let private withTask (name : int) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        { system with
            Tasks = UnixTaskTable.register name (CpuId 0) (OsThreadId (uint32 name + 1u)) system.Tasks
        }

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

    /// A system with one socket event port, which a task can park on in `flock`, and
    /// that port's description.
    let private world (platform : SimulatedUnixPlatform) : UnixSystem<int, string> * OpenFileDescriptionId =
        let system = UnixSystem.initial<int, string> platform

        let fd, registry =
            FileDescriptorRegistry.createSocketEventPort system.Process.FileDescriptors

        let id =
            match FileDescriptorRegistry.tryFindWithId fd registry with
            | Some (id, _) -> id
            | None -> failwith $"fd %d{fd} names no description"

        { system with
            Process =
                { system.Process with
                    FileDescriptors = registry
                }
        },
        id

    let private flockOn (port : OpenFileDescriptionId) : ParkedSyscall =
        ParkedSyscall.Flock
            {
                Requester = port
                Mode = FlockMode.Exclusive
            }

    let private exitOrFail (task : int) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        match UnixTaskLifecycle.exitThread task system with
        | Ok system -> system
        | Error refusal -> failwith $"expected the exit to be answered, got: %s{ThreadExitRefusal.describe refusal}"

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
                |> mapSignals (SignalState.block 1 Signal.SIGUSR1)
                |> mapSignals (SignalState.block 1 Signal.SIGUSR2)
                |> mapSignals (SignalState.block 2 Signal.SIGUSR2)
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

            after.Tasks |> Map.keys |> List.ofSeq |> shouldEqual [ 2 ]

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

            SignalState.blockedTasks after.Process.Signals |> shouldEqual (Set.singleton 2)

            SignalState.blockedFor 2 after.Process.Signals
            |> shouldEqual (Set.singleton Signal.SIGUSR2)

            UnixSystem.checkInvariants after |> shouldEqual []

    [<Test>]
    let ``a parked task's exit is refused, and names the park`` () : unit =
        for platform in platforms do
            let system, port = world platform

            let system = system |> withTask 1 |> withTask 2 |> UnixWait.park 2 (flockOn port)

            let park =
                match UnixTaskTable.parkOf 2 system.Tasks with
                | Some park -> park
                | None -> failwith "expected task 2 to be parked"

            UnixTaskLifecycle.exitThread 2 system
            |> shouldEqual (Error (ThreadExitRefusal.Parked (2, park)))

    [<Test>]
    let ``the last task's exit is refused, as its flavour's own case`` () : unit =
        let refusalOn (platform : SimulatedUnixPlatform) =
            let system, _ = world platform
            // Two tasks, one of which leaves, so the refusal is about being *last* rather
            // than about being the first registered.
            let system = system |> withTask 1 |> withTask 2 |> exitOrFail 1
            UnixTaskLifecycle.exitThread 2 system

        refusalOn SimulatedUnixPlatform.linuxX64
        |> shouldEqual (Error (ThreadExitRefusal.LastTaskOnLinux 2))

        refusalOn SimulatedUnixPlatform.linuxArm64
        |> shouldEqual (Error (ThreadExitRefusal.LastTaskOnLinux 2))

        refusalOn SimulatedUnixPlatform.macOsArm64
        |> shouldEqual (Error (ThreadExitRefusal.LastTaskOnDarwin 2))

    [<Test>]
    let ``exiting a task that was never registered fails loudly`` () : unit =
        let system, _ = world SimulatedUnixPlatform.linuxX64
        let system = system |> withTask 1 |> withTask 2

        let exn =
            Assert.Throws<exn> (fun () ->
                UnixTaskLifecycle.exitThread 3 system
                |> ignore<Result<UnixSystem<int, string>, ThreadExitRefusal<int>>>
            )

        exn.Message |> shouldContainText "names no task"

    [<RequireQualifiedAccess>]
    type private Op =
        | Register of task : int
        | Block of task : int * Signal
        | Unblock of task : int * Signal
        | EnqueueOnTask of task : int * Signal
        | EnqueueOnProcess of Signal
        | Park of task : int
        | Unpark of task : int
        | Exit of task : int

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

        Gen.frequency
            [
                4, task |> Gen.map Op.Register
                3, Gen.zip task signal |> Gen.map Op.Block
                1, Gen.zip task signal |> Gen.map Op.Unblock
                3, Gen.zip task signal |> Gen.map Op.EnqueueOnTask
                1, signal |> Gen.map Op.EnqueueOnProcess
                1, task |> Gen.map Op.Park
                1, task |> Gen.map Op.Unpark
                3, task |> Gen.map Op.Exit
            ]

    /// What the table must say about which tasks exist, kept by a model that knows
    /// nothing of signals: the tasks registered and not since exited, and the ones of
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
            mutable RefusedLast : int
        }

    /// The per-task entries the system holds, each of which must name a task.
    let private orphans (system : UnixSystem<int, string>) : string list =
        let tasks = system.Tasks |> Map.keys |> Set.ofSeq

        let masks =
            Set.difference (SignalState.blockedTasks system.Process.Signals) tasks
            |> Set.toList
            |> List.map (fun t -> $"mask of %d{t}")

        let pending =
            SignalState.pending system.Process.Signals
            |> List.choose (fun entry ->
                match entry.Target with
                | ValueSome t when not (Set.contains t tasks) -> Some $"%O{entry.Signal} pending on %d{t}"
                | ValueSome _
                | ValueNone -> None
            )

        masks @ pending

    let private runProperty (platform : SimulatedUnixPlatform) (coverage : Coverage) : unit =
        let initial, port = world platform

        let step ((system, model) : UnixSystem<int, string> * Model) (op : Op) : UnixSystem<int, string> * Model =
            let live (task : int) = Set.contains task model.Live

            let after, model' =
                match op with
                | Op.Register task ->
                    if live task then
                        system, model
                    else
                        withTask task system,
                        { model with
                            Live = Set.add task model.Live
                        }
                | Op.Block (task, signal) when live task -> mapSignals (SignalState.block task signal) system, model
                | Op.Unblock (task, signal) when live task -> mapSignals (SignalState.unblock task signal) system, model
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
                    UnixWait.park task (flockOn port) system,
                    { model with
                        Parked = Set.add task model.Parked
                    }
                | Op.Unpark task when live task ->
                    { system with
                        Tasks = UnixTaskTable.unpark task system.Tasks
                    },
                    { model with
                        Parked = Set.remove task model.Parked
                    }
                | Op.Exit task when live task ->
                    let result = UnixTaskLifecycle.exitThread task system

                    if Set.contains task model.Parked then
                        let park = UnixTaskTable.parkOf task system.Tasks |> Option.get
                        result |> shouldEqual (Error (ThreadExitRefusal.Parked (task, park)))
                        coverage.RefusedParked <- coverage.RefusedParked + 1
                        system, model
                    elif model.Live.Count = 1 then
                        let expected =
                            match SimulatedUnixPlatform.flavour platform with
                            | SimulatedUnixFlavour.Linux -> ThreadExitRefusal.LastTaskOnLinux task
                            | SimulatedUnixFlavour.Darwin -> ThreadExitRefusal.LastTaskOnDarwin task

                        result |> shouldEqual (Error expected)
                        coverage.RefusedLast <- coverage.RefusedLast + 1
                        system, model
                    else
                        let after =
                            match result with
                            | Ok after -> after
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

                        SignalState.blockedTasks signalsAfter
                        |> shouldEqual (Set.remove task (SignalState.blockedTasks signalsBefore))

                        for other in Set.remove task model.Live do
                            SignalState.blockedFor other signalsAfter
                            |> shouldEqual (SignalState.blockedFor other signalsBefore)

                        SignalState.enabled signalsAfter
                        |> shouldEqual (SignalState.enabled signalsBefore)

                        after.Tasks |> shouldEqual (Map.remove task system.Tasks)
                        after.Machine |> shouldEqual system.Machine

                        { after.Process with
                            Signals = signalsBefore
                        }
                        |> shouldEqual system.Process

                        if not (SignalState.blockedFor task signalsBefore).IsEmpty then
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
                // Every other op names a task that does not exist, which no client does.
                | Op.Block _
                | Op.Unblock _
                | Op.EnqueueOnTask _
                | Op.Park _
                | Op.Unpark _
                | Op.Exit _ -> system, model

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
                      Live = Set.empty
                      Parked = Set.empty
                  }),
                 ops)
                ||> List.fold step
                |> ignore<UnixSystem<int, string> * Model>

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 500, property)

    [<TestCaseSource(nameof platforms)>]
    let ``the task set follows registrations and exits, and no per-task entry outlives its task``
        (platform : SimulatedUnixPlatform)
        : unit
        =
        let coverage =
            {
                ExitsDroppingAMask = 0
                ExitsDroppingOwnPending = 0
                ExitsKeepingProcessPending = 0
                RefusedParked = 0
                RefusedLast = 0
            }

        runProperty platform coverage

        coverage.ExitsDroppingAMask |> shouldBeGreaterThan 50
        coverage.ExitsDroppingOwnPending |> shouldBeGreaterThan 50
        coverage.ExitsKeepingProcessPending |> shouldBeGreaterThan 20
        coverage.RefusedParked |> shouldBeGreaterThan 20
        coverage.RefusedLast |> shouldBeGreaterThan 20

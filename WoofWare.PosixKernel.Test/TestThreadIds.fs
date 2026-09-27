namespace WoofWare.PosixKernel.Test

open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// The thread IDs a process's tasks report: what the leader starts with, how each
/// flavour's counter hands out the rest, and the settings that move it.
///
/// The literal rows are the measurements of
/// `docs/plans/2026-08-23-posix-kernel-extraction/thread-ids.c` (Linux 6.18.5 and
/// Darwin 27.0.0) and `pid-allocation.c` (Linux 6.18.5), replayed through the model.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestThreadIds =

    let private linux : UnixSystem<int, string> =
        UnixSystem.initial SimulatedUnixPlatform.linuxX64 0 (CpuId 0)

    let private darwin : UnixSystem<int, string> =
        UnixSystem.initial SimulatedUnixPlatform.macOsArm64 0 (CpuId 0)

    let private pid (value : int32) : ProcessId = ProcessId.parseOrFail "test" value

    let private idOf (task : int) (system : UnixSystem<int, string>) : uint64 =
        OsThreadId.toUInt64 (UnixTaskTable.osThreadIdOf task system.Tasks)

    let private spawnOrFail (child : int) (system : UnixSystem<int, string>) : uint64 * UnixSystem<int, string> =
        match UnixTaskLifecycle.spawn system.Leader child (CpuId 0) system with
        | Ok (id, system) -> OsThreadId.toUInt64 id, system
        | Error error -> failwith $"spawning %d{child} failed with %O{error}"

    let private exitOrFail (task : int) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        match UnixTaskLifecycle.exitThread task 0 system with
        | Ok (TaskOutcome.Continues system) -> system
        | other -> failwith $"expected task %d{task}'s exit to leave the process running, got %A{other}"

    /// Spawn `child` and exit it at once, as `pthread_create` then `pthread_join`
    /// would: the id it had.
    let private spawnAndExit (child : int) (system : UnixSystem<int, string>) : uint64 * UnixSystem<int, string> =
        let id, system = spawnOrFail child system
        id, exitOrFail child system

    [<Test>]
    let ``a fresh process's leader has the process ID as its thread ID, on both flavours`` () : unit =
        for system in [ linux ; darwin ] do
            system.Leader |> shouldEqual 0
            system.Tasks |> Map.keys |> List.ofSeq |> shouldEqual [ 0 ]

            idOf 0 system
            |> shouldEqual (uint64 (ProcessId.toInt32 UnixSystem.defaultProcessId))

            UnixTaskTable.cpuOf 0 system.Tasks |> shouldEqual (CpuId 0)
            UnixSystem.checkInvariants system |> shouldEqual []

    [<Test>]
    let ``the leader is on the processor it was given`` () : unit =
        let system : UnixSystem<string, string> =
            UnixSystem.initial SimulatedUnixPlatform.linuxArm64 "main" (CpuId 3)

        system.Leader |> shouldEqual "main"
        UnixTaskTable.cpuOf "main" system.Tasks |> shouldEqual (CpuId 3)

    [<Test>]
    let ``Linux: the leader's tid is the pid, and threads count up from it, never reusing an exited one's`` () : unit =
        // Row 1 and 2 of `thread-ids.c` on Linux: pid 8, leader tid 8; eight
        // concurrent threads 9..16; eight created and joined one at a time, 17..24.
        let system = UnixSystem.withProcessId "test" (pid 8) linux
        idOf 0 system |> shouldEqual 8UL

        let concurrent, system =
            (system, [ 1..8 ])
            ||> List.mapFold (fun system child -> spawnOrFail child system)

        concurrent |> shouldEqual [ 9UL .. 16UL ]

        let system =
            (system, [ 1..8 ]) ||> List.fold (fun system task -> exitOrFail task system)

        let sequential, system =
            (system, [ 11..18 ])
            ||> List.mapFold (fun system child -> spawnAndExit child system)

        sequential |> shouldEqual [ 17UL .. 24UL ]
        UnixSystem.checkInvariants system |> shouldEqual []

    [<Test>]
    let ``Darwin: the leader's id is whatever the counter says, and threads take the ids after it`` () : unit =
        // Row 1 of `thread-ids.c` on Darwin: pid 58948, leader id 2897490. The
        // measured machine was not quiet, and gave the first thread 2897497; this
        // library models a quiet one, where the counter's next id is the next
        // thread's. Ids were consecutive between threads created one at a time.
        let system =
            darwin
            |> UnixSystem.withProcessId "test" (pid 58948)
            |> UnixSystem.withLeaderThreadId "test" 2897490UL

        UnixSystem.processId system |> shouldEqual (pid 58948)
        idOf 0 system |> shouldEqual 2897490UL

        let concurrent, system =
            (system, [ 1..8 ])
            ||> List.mapFold (fun system child -> spawnOrFail child system)

        concurrent |> shouldEqual [ 2897491UL .. 2897498UL ]

        let system =
            (system, [ 1..8 ]) ||> List.fold (fun system task -> exitOrFail task system)

        let sequential, system =
            (system, [ 11..18 ])
            ||> List.mapFold (fun system child -> spawnAndExit child system)

        sequential |> shouldEqual [ 2897499UL .. 2897506UL ]
        UnixSystem.checkInvariants system |> shouldEqual []

    [<Test>]
    let ``Darwin: the process ID does not move the thread IDs, and the counter crosses 32 bits`` () : unit =
        let system = darwin |> UnixSystem.withProcessId "test" (pid 100)

        idOf 0 system
        |> shouldEqual (uint64 (ProcessId.toInt32 UnixSystem.defaultProcessId))

        let system = system |> UnixSystem.withLeaderThreadId "test" 0xFFFF_FFFFUL
        let id, system = spawnOrFail 1 system
        id |> shouldEqual 0x1_0000_0000UL
        UnixSystem.checkInvariants system |> shouldEqual []

    [<Test>]
    let ``Darwin: no id comes back, and ids never go backwards`` () : unit =
        // Row 4 of `thread-ids.c` on Darwin: 20000 threads created and joined one at
        // a time, and none reused an id or went backwards.
        let ids, system =
            (darwin, [ 1..20000 ])
            ||> List.mapFold (fun system child -> spawnAndExit child system)

        ids
        |> List.pairwise
        |> List.forall (fun (a, b) -> b = a + 1UL)
        |> shouldEqual true

        UnixSystem.checkInvariants system |> shouldEqual []

    [<Test>]
    let ``Linux: at a small pid_max the ids wrap to 300, skip live ones, and run out with EAGAIN`` () : unit =
        // `pid-allocation.c` on Linux 6.18.5 aarch64, with pid 9 and pid_max 1000.
        let system =
            linux
            |> UnixSystem.withProcessId "test" (pid 9)
            |> UnixSystem.withPidMax "test" 1000

        // "skip first_wrap from=999 to=300"
        let rec runToWrap (child : int) (last : uint64) (system : UnixSystem<int, string>) =
            let id, system = spawnAndExit child system

            if id < last then
                (last, id), child + 1, system
            else
                runToWrap (child + 1) id system

        let firstWrap, child, system = runToWrap 1 9UL system
        firstWrap |> shouldEqual (999UL, 300UL)

        // "skip held 301 303 joined_between=302"
        let held1, system = spawnOrFail child system
        let between, system = spawnAndExit (child + 1) system
        let held2, system = spawnOrFail (child + 2) system
        (held1, between, held2) |> shouldEqual (301UL, 302UL, 303UL)

        // "skip second_wrap from=999 to=300", then 302, 304, 305, 306.
        let secondWrap, child, system = runToWrap (child + 3) held2 system
        secondWrap |> shouldEqual (999UL, 300UL)

        let afterWrap, system =
            (system, [ child .. child + 3 ])
            ||> List.mapFold (fun system child -> spawnAndExit child system)

        afterWrap |> shouldEqual [ 302UL ; 304UL ; 305UL ; 306UL ]

        // "exhaust": pid_max lowered to 400 with the cursor at 307, and 301 and 303
        // still held; 98 threads start, 307..399 then 300, 302, 304, 305, 306, and
        // the next fails with EAGAIN.
        let system = system |> UnixSystem.withPidMax "test" 400

        let rec fill (child : int) (started : uint64 list) (system : UnixSystem<int, string>) =
            match UnixTaskLifecycle.spawn system.Leader child (CpuId 0) system with
            | Ok (id, system) -> fill (child + 1) (OsThreadId.toUInt64 id :: started) system
            | Error error -> List.rev started, error, system

        let started, error, system = fill (child + 4) [] system

        error |> shouldEqual UnixError.EAGAIN

        started
        |> shouldEqual ([ 307UL .. 399UL ] @ [ 300UL ; 302UL ; 304UL ; 305UL ; 306UL ])

        UnixSystem.checkInvariants system |> shouldEqual []

    [<Test>]
    let ``Linux: pid_max takes exactly 301 to 4194304, and 4194304 is the default`` () : unit =
        // `pid-allocation.c`'s bounds sweep, on Linux 6.18.5 aarch64 and x86-64.
        UnixSystem.defaultPidMax |> shouldEqual 4194304

        let accepts (value : int32) : bool =
            try
                UnixSystem.withPidMax "test" value (UnixSystem.withProcessId "test" (pid 8) linux)
                |> ignore<UnixSystem<int, string>>

                true
            with _ ->
                false

        for value in
            [
                -1
                0
                1
                2
                299
                300
                4194305
                4194306
                8388608
                System.Int32.MaxValue
            ] do
            (value, accepts value) |> shouldEqual (value, false)

        for value in [ 301 ; 302 ; 303 ; 1000 ; 32768 ; 4194302 ; 4194303 ; 4194304 ] do
            (value, accepts value) |> shouldEqual (value, true)

        let property (value : int32) : bool =
            accepts value = (value >= 301 && value <= 4194304)

        Check.One (
            Config.QuickThrowOnFailure.WithMaxTest 500,
            Prop.forAll
                (Arb.fromGen (
                    Gen.oneof
                        [
                            Gen.choose (250, 350)
                            Gen.choose (4194250, 4194350)
                            ArbMap.defaults |> ArbMap.generate<int32>
                        ]
                ))
                property
        )

    [<Test>]
    let ``the identity setters refuse what no process could be`` () : unit =
        let refuses (f : unit -> UnixSystem<int, string>) (text : string) : unit =
            let exn = Assert.Throws<exn> (fun () -> f () |> ignore<UnixSystem<int, string>>)
            exn.Message |> shouldContainText text

        let spawned = linux |> spawnOrFail 1 |> snd
        let darwinSpawned = darwin |> spawnOrFail 1 |> snd

        // Every one is a boot-time setting.
        refuses (fun () -> UnixSystem.withProcessId "ctx" (pid 8) spawned) "before any thread"
        refuses (fun () -> UnixSystem.withProcessId "ctx" (pid 8) darwinSpawned) "before any thread"
        refuses (fun () -> UnixSystem.withLeaderThreadId "ctx" 7UL darwinSpawned) "before any thread"

        // A Linux pid is a thread ID, so it is below pid_max.
        let small =
            linux
            |> UnixSystem.withProcessId "ctx" (pid 8)
            |> UnixSystem.withPidMax "ctx" 1000

        refuses (fun () -> UnixSystem.withProcessId "ctx" (pid 1000) small) "not below pid_max"
        refuses (fun () -> UnixSystem.withPidMax "ctx" 4242 linux) "not below pid_max"
        UnixSystem.withProcessId "ctx" (pid 999) small |> idOf 0 |> shouldEqual 999UL

        // Each flavour's own setting.
        refuses (fun () -> UnixSystem.withLeaderThreadId "ctx" 7UL linux) "the process ID"
        refuses (fun () -> UnixSystem.withPidMax "ctx" 1000 darwin) "no pid_max"

        refuses (fun () -> UnixSystem.withLeaderThreadId "ctx" 0UL darwin) "Darwin counter"
        refuses (fun () -> UnixSystem.withLeaderThreadId "ctx" System.UInt64.MaxValue darwin) "Darwin counter"

    [<Test>]
    let ``Linux: moving the pid moves the leader's tid and the counter with it`` () : unit =
        let system = linux |> UnixSystem.withProcessId "test" (pid 500)
        idOf 0 system |> shouldEqual 500UL
        spawnOrFail 1 system |> fst |> shouldEqual 501UL
        UnixSystem.checkInvariants system |> shouldEqual []

    [<Test>]
    let ``spawning from a parked task, from no task, or onto a live one fails loudly`` () : unit =
        let system = linux |> spawnOrFail 1 |> snd

        let fails (f : unit -> unit) (text : string) : unit =
            let exn = Assert.Throws<exn> (fun () -> f ())
            exn.Message |> shouldContainText text

        let spawn (parent : int) (child : int) (system : UnixSystem<int, string>) () : unit =
            UnixTaskLifecycle.spawn parent child (CpuId 0) system
            |> ignore<Result<OsThreadId * UnixSystem<int, string>, UnixError>>

        fails (spawn 7 2 system) "names no task"
        fails (spawn 0 1 system) "already names a task"
        fails (spawn 1 0 system) "already names a task"

        let parked =
            UnixWait.park
                1
                (ParkedSyscall.Poll
                    {
                        Entries = []
                        Deadline = None
                    })
                system

        fails (spawn 1 2 parked) "parked"

    [<Test>]
    let ``a new thread starts with its creator's mask and nothing pending on it`` () : unit =
        // Measured on Linux 6.18.5 (aarch64 and x86-64) and Darwin 27.0.0 by
        // `docs/plans/2026-08-23-posix-kernel-extraction/thread-spawn-mask.c`.
        for system in [ linux ; darwin ] do
            let signals (f : SignalState<int, string> -> SignalState<int, string>) (system : UnixSystem<int, string>) =
                { system with
                    Process =
                        { system.Process with
                            Signals = f system.Process.Signals
                        }
                }

            let system =
                system
                |> signals (SignalState.block 0 Signal.SIGUSR1)
                |> signals (SignalState.block 0 Signal.SIGTERM)
                |> signals (SignalState.block 0 Signal.SIGUSR2)
                |> signals (
                    SignalState.enqueue
                        {
                            Signal = Signal.SIGUSR2
                            Target = ValueSome 0
                        }
                )

            let _, system = spawnOrFail 1 system

            SignalState.blockedFor 1 system.Process.Signals
            |> shouldEqual (Set.ofList [ Signal.SIGUSR1 ; Signal.SIGTERM ; Signal.SIGUSR2 ])

            SignalState.pending system.Process.Signals
            |> shouldEqual
                [
                    {
                        Signal = Signal.SIGUSR2
                        Target = ValueSome 0
                    }
                ]

            // And from an unmasked creator, an unmasked thread.
            let _, system =
                spawnOrFail 2 (exitOrFail 1 system |> signals (SignalState.forgetTask 0))

            SignalState.blockedFor 2 system.Process.Signals |> shouldEqual Set.empty
            SignalState.blockedTasks system.Process.Signals |> shouldEqual Set.empty

    // ------------------------------------------------------------------
    // The reference model.

    [<RequireQualifiedAccess>]
    type private Op =
        /// Spawn a new task from the `parent`th live task (modulo how many there are).
        | Spawn of parent : int
        /// Exit the `task`th live task other than the leader (modulo how many).
        | Exit of task : int
        /// `exit_group` from the `task`th live task (modulo how many), which ends the
        /// process; the run goes on from the state before, as if it had not been made.
        | ExitGroup of task : int

    /// How a machine was set up: Linux's pid and pid_max, or Darwin's first id.
    [<RequireQualifiedAccess>]
    type private Setup =
        | Linux of pid : int32 * pidMax : int32
        | Darwin of first : uint64

    /// The ids a counter hands out, written as a step at a time rather than as the
    /// library's search: from the cursor upwards; at pid_max, back to 300, once;
    /// EAGAIN if that finds every id live. Darwin's is a counter.
    [<RequireQualifiedAccess>]
    type private Model =
        | Linux of cursor : int32 * pidMax : int32
        | Darwin of next : uint64

    let private modelNext (live : Set<uint64>) (model : Model) : Result<uint64 * Model, unit> =
        match model with
        | Model.Linux (cursor, pidMax) ->
            let rec go (candidate : int32) (wrapped : bool) =
                if candidate >= pidMax then
                    if wrapped then Error () else go 300 true
                elif Set.contains (uint64 candidate) live then
                    go (candidate + 1) wrapped
                else
                    Ok (uint64 candidate, Model.Linux (candidate + 1, pidMax))

            go cursor false
        | Model.Darwin next -> Ok (next, Model.Darwin (next + 1UL))

    /// Coverage of the paths the property exists for, so that a generator change
    /// which stops reaching one is noticed.
    type private Coverage =
        {
            mutable Spawned : int
            mutable Wraps : int
            mutable SkippedLive : int
            mutable Exhausted : int
            mutable Exits : int
            mutable Groups : int
        }

    let private setupGen (flavour : SimulatedUnixFlavour) : Gen<Setup> =
        match flavour with
        | SimulatedUnixFlavour.Linux ->
            gen {
                let! pidMax = Gen.choose (301, 340)
                // Mostly near the top, so that the counter wraps in a short run;
                // sometimes anywhere below it, including under 300.
                let! pidValue =
                    Gen.frequency [ 3, Gen.choose (pidMax - 30, pidMax - 1) ; 1, Gen.choose (1, pidMax - 1) ]

                return Setup.Linux (pidValue, pidMax)
            }
        | SimulatedUnixFlavour.Darwin ->
            Gen.oneof
                [
                    Gen.choose (1, 100000) |> Gen.map uint64
                    Gen.constant 0xFFFF_FFF0UL
                    ArbMap.defaults |> ArbMap.generate<uint32> |> Gen.map (fun i -> uint64 i + 1UL)
                ]
            |> Gen.map Setup.Darwin

    let private opGen : Gen<Op> =
        Gen.frequency
            [
                6, Gen.choose (0, 60) |> Gen.map Op.Spawn
                4, Gen.choose (0, 60) |> Gen.map Op.Exit
                1, Gen.choose (0, 60) |> Gen.map Op.ExitGroup
            ]

    let private runModel (coverage : Coverage) (setup : Setup) (ops : Op list) : unit =
        let system, model =
            match setup with
            | Setup.Linux (pidValue, pidMax) ->
                linux
                |> UnixSystem.withProcessId "test" (pid pidValue)
                |> UnixSystem.withPidMax "test" pidMax,
                Model.Linux (pidValue + 1, pidMax)
            | Setup.Darwin first -> darwin |> UnixSystem.withLeaderThreadId "test" first, Model.Darwin (first + 1UL)

        // The leader's id: the pid on Linux, the configured start on Darwin.
        match setup with
        | Setup.Linux (pidValue, _) -> idOf 0 system |> shouldEqual (uint64 pidValue)
        | Setup.Darwin first -> idOf 0 system |> shouldEqual first

        let liveIds (system : UnixSystem<int, string>) : Set<uint64> =
            system.Tasks
            |> Map.toSeq
            |> Seq.map (fun (task, _) -> idOf task system)
            |> Set.ofSeq

        let check (system : UnixSystem<int, string>) : unit =
            UnixSystem.checkInvariants system |> shouldEqual []
            // Unique among live tasks.
            (liveIds system).Count |> shouldEqual system.Tasks.Count

            match setup with
            | Setup.Linux (pidValue, _) -> idOf 0 system |> shouldEqual (uint64 pidValue)
            | Setup.Darwin _ -> ()

        let step
            ((system, model, nextName, minted, last) : UnixSystem<int, string> * Model * int * Set<uint64> * uint64)
            (op : Op)
            =
            let live = system.Tasks |> Map.keys |> List.ofSeq

            match op with
            | Op.Spawn index ->
                let parent = live.[index % live.Length]
                let held = liveIds system
                let actual = UnixTaskLifecycle.spawn parent nextName (CpuId 0) system

                match modelNext held model, actual with
                | Error (), Error error ->
                    error |> shouldEqual UnixError.EAGAIN
                    coverage.Exhausted <- coverage.Exhausted + 1
                    system, model, nextName + 1, minted, last
                | Ok (expected, model), Ok (id, after) ->
                    let id = OsThreadId.toUInt64 id
                    id |> shouldEqual expected
                    idOf nextName after |> shouldEqual id

                    // Stated apart from the model: without a wrap each id is above
                    // the last, so an id at or below it is a wrap, which starts at
                    // 300 and passes over only live ids; between wraps, no id is
                    // handed out twice.
                    if id <= last then
                        coverage.Wraps <- coverage.Wraps + 1
                        (id >= 300UL) |> shouldEqual true

                        for skipped in 300UL .. id - 1UL do
                            Set.contains skipped held |> shouldEqual true
                    elif Set.contains id minted then
                        failwith $"id %d{id} was handed out again before the counter wrapped"

                    // Every id between the last one and this was passed over for
                    // being live: from the last one up, or from 300 after a wrap.
                    if id > last + 1UL || (id <= last && id > 300UL) then
                        coverage.SkippedLive <- coverage.SkippedLive + 1

                    coverage.Spawned <- coverage.Spawned + 1
                    check after
                    let minted = if id <= last then Set.singleton id else Set.add id minted
                    after, model, nextName + 1, minted, id
                | expected, actual -> failwith $"the model says %A{expected}, and the library %A{actual}"
            | Op.Exit index ->
                match live |> List.filter (fun task -> task <> system.Leader) with
                | [] -> system, model, nextName, minted, last
                | others ->
                    let task = others.[index % others.Length]
                    let after = exitOrFail task system
                    coverage.Exits <- coverage.Exits + 1
                    check after
                    after, model, nextName, minted, last
            | Op.ExitGroup index ->
                let task = live.[index % live.Length]
                let ended = UnixTaskLifecycle.exitGroup task 0 system
                ended.Machine |> shouldEqual system.Machine
                coverage.Groups <- coverage.Groups + 1
                system, model, nextName, minted, last

        check system

        ((system, model, 1, Set.singleton (idOf 0 system), idOf 0 system), ops)
        ||> List.fold step
        |> ignore

    let private flavours : SimulatedUnixFlavour list =
        [ SimulatedUnixFlavour.Linux ; SimulatedUnixFlavour.Darwin ]

    [<TestCaseSource(nameof flavours)>]
    let ``thread IDs follow the reference model, are unique among live tasks, and come back only after a wrap``
        (flavour : SimulatedUnixFlavour)
        : unit
        =
        let coverage =
            {
                Spawned = 0
                Wraps = 0
                SkippedLive = 0
                Exhausted = 0
                Exits = 0
                Groups = 0
            }

        let ops =
            gen {
                let! length = Gen.choose (0, 400)
                return! Gen.listOfLength length opGen
            }

        let property =
            Prop.forAll (Arb.fromGen (Gen.zip (setupGen flavour) ops))
            <| fun (setup, ops) -> runModel coverage setup ops

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 200, property)

        coverage.Spawned |> shouldBeGreaterThan 1000
        coverage.Exits |> shouldBeGreaterThan 1000
        coverage.Groups |> shouldBeGreaterThan 100

        match flavour with
        | SimulatedUnixFlavour.Linux ->
            coverage.Wraps |> shouldBeGreaterThan 50
            coverage.SkippedLive |> shouldBeGreaterThan 50
            coverage.Exhausted |> shouldBeGreaterThan 20
        | SimulatedUnixFlavour.Darwin ->
            coverage.Wraps |> shouldEqual 0
            coverage.SkippedLive |> shouldEqual 0
            coverage.Exhausted |> shouldEqual 0

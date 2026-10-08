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

    let private linuxImage : UnixBootImage<int, string> =
        UnixSystem.initial SimulatedUnixPlatform.linuxX64 UnixSystem.pipedStandardStreams 0 (CpuId 0)

    let private darwinImage : UnixBootImage<int, string> =
        UnixSystem.initial SimulatedUnixPlatform.macOsArm64 UnixSystem.pipedStandardStreams 0 (CpuId 0)

    let private linux : UnixSystem<int, string> = UnixBootImage.boot linuxImage

    let private darwin : UnixSystem<int, string> = UnixBootImage.boot darwinImage

    let private pid (value : int32) : ProcessId = ProcessId.parseOrFail "test" value

    let private idOf (task : int) (system : UnixSystem<int, string>) : uint64 =
        OsThreadId.toUInt64 (UnixTaskTable.osThreadIdOf task system.Tasks)

    let private spawnOrFail (child : int) (system : UnixSystem<int, string>) : uint64 * UnixSystem<int, string> =
        match UnixTaskLifecycle.spawn system.Leader child (CpuId 0) system with
        | Ok (SpawnAnswer.Spawned id, system) -> OsThreadId.toUInt64 id, system
        | Ok (SpawnAnswer.Failed error, _) -> failwith $"spawning %d{child} failed with %O{error}"
        | Error refusal -> failwith $"spawning %d{child} was refused: %s{SpawnRefusal.describe refusal}"

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
            UnixSystem.initial SimulatedUnixPlatform.linuxArm64 UnixSystem.pipedStandardStreams "main" (CpuId 3)
            |> UnixBootImage.boot

        system.Leader |> shouldEqual "main"
        UnixTaskTable.cpuOf "main" system.Tasks |> shouldEqual (CpuId 3)

    [<Test>]
    let ``Linux: the leader's tid is the pid, and threads count up from it, never reusing an exited one's`` () : unit =
        // Row 1 and 2 of `thread-ids.c` on Linux: pid 8, leader tid 8; eight
        // concurrent threads 9..16; eight created and joined one at a time, 17..24.
        let system =
            UnixBootImage.withProcessId "test" (pid 8) linuxImage |> UnixBootImage.boot

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
            darwinImage
            |> UnixBootImage.withProcessId "test" (pid 58948)
            |> UnixBootImage.withLeaderThreadId "test" 2897490UL
            |> UnixBootImage.boot

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
        let image = darwinImage |> UnixBootImage.withProcessId "test" (pid 100)

        idOf 0 (UnixBootImage.boot image)
        |> shouldEqual (uint64 (ProcessId.toInt32 UnixSystem.defaultProcessId))

        let system =
            image
            |> UnixBootImage.withLeaderThreadId "test" 0xFFFF_FFFFUL
            |> UnixBootImage.boot

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
            linuxImage
            |> UnixBootImage.withProcessId "test" (pid 9)
            |> UnixBootImage.boot
            |> UnixSystem.writePidMaxSysctl "test" 1000

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
        let system = system |> UnixSystem.writePidMaxSysctl "test" 400

        let rec fill (child : int) (started : uint64 list) (system : UnixSystem<int, string>) =
            match UnixTaskLifecycle.spawn system.Leader child (CpuId 0) system with
            | Ok (SpawnAnswer.Spawned id, system) -> fill (child + 1) (OsThreadId.toUInt64 id :: started) system
            | Ok (SpawnAnswer.Failed error, after) ->
                // A failed creation hands out no thread ID and adds no task.
                after |> shouldEqual system
                List.rev started, error, system
            | Error refusal -> failwith $"spawning %d{child} was refused: %s{SpawnRefusal.describe refusal}"

        let started, error, system = fill (child + 4) [] system

        error |> shouldEqual UnixError.EAGAIN

        started
        |> shouldEqual ([ 307UL .. 399UL ] @ [ 300UL ; 302UL ; 304UL ; 305UL ; 306UL ])

        UnixSystem.checkInvariants system |> shouldEqual []

    [<Test>]
    let ``Linux: pid_max may be written at or below a live thread's ID, which keeps it`` () : unit =
        // `pid-max-below-live.c` on Linux 6.18.5 aarch64: process 5000, with a
        // thread 5001 alive throughout, and two threads started and joined after
        // each write. Every write takes; the process and the live thread keep
        // their ids, and `kill(pid, 0)` still finds the process.
        let system =
            linuxImage
            |> UnixBootImage.withProcessId "test" (pid 5000)
            |> UnixBootImage.boot

        // "child held_tid 5001"
        let held, system = spawnOrFail 1 system
        held |> shouldEqual 5001UL

        // "before new_thread 5002 5003"
        let joined, system =
            (system, [ 2 ; 3 ])
            ||> List.mapFold (fun system child -> spawnAndExit child system)

        joined |> shouldEqual [ 5002UL ; 5003UL ]

        let rows =
            [
                1000, [ 300UL ; 301UL ]
                5000, [ 302UL ; 303UL ]
                5001, [ 304UL ; 305UL ]
                5002, [ 306UL ; 307UL ]
                400, [ 308UL ; 309UL ]
                UnixSystem.defaultPidMax, [ 310UL ; 311UL ]
            ]

        ((system, 4), rows)
        ||> List.fold (fun (system, child) (pidMax, expected) ->
            let written = UnixSystem.writePidMaxSysctl "test" pidMax system

            // The write changes the counter's bound and nothing else.
            { written with
                Machine =
                    { written.Machine with
                        ThreadIds = system.Machine.ThreadIds
                    }
            }
            |> shouldEqual system

            (pidMax, idOf 0 written, idOf 1 written) |> shouldEqual (pidMax, 5000UL, 5001UL)
            UnixSystem.checkInvariants written |> shouldEqual []

            match UnixSignal.kill 5000 0 written with
            | Ok (Ok (KillOutcome.ProcessContinues after)) -> after |> shouldEqual written
            | other -> failwith $"kill(5000, 0) at pid_max %d{pidMax}: %A{other}"

            let joined, after =
                (written, [ child ; child + 1 ])
                ||> List.mapFold (fun system child -> spawnAndExit child system)

            (pidMax, joined) |> shouldEqual (pidMax, expected)
            UnixSystem.checkInvariants after |> shouldEqual []
            after, child + 2
        )
        |> ignore<UnixSystem<int, string> * int>

    [<Test>]
    let ``Linux: pid_max takes exactly 301 to 4194304, and 4194304 is the default`` () : unit =
        // `pid-allocation.c`'s bounds sweep, on Linux 6.18.5 aarch64 and x86-64.
        UnixSystem.defaultPidMax |> shouldEqual 4194304

        let accepts (value : int32) : bool =
            try
                linuxImage
                |> UnixBootImage.withProcessId "test" (pid 8)
                |> UnixBootImage.boot
                |> UnixSystem.writePidMaxSysctl "test" value
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

        // Every one of the setters is a boot-time setting, which a process that
        // has created a thread cannot be given: they take a `UnixBootImage`,
        // and nothing a thread can be created in is one.

        // A Linux pid is a thread ID, so it is below the pid_max the machine
        // boots with, which is the largest Linux has.
        refuses
            (fun () -> UnixBootImage.withProcessId "ctx" (pid 4194304) linuxImage |> UnixBootImage.boot)
            "not below pid_max"

        UnixBootImage.withProcessId "ctx" (pid 4194303) linuxImage
        |> UnixBootImage.boot
        |> idOf 0
        |> shouldEqual 4194303UL

        // Each flavour's own setting.
        refuses (fun () -> UnixBootImage.withLeaderThreadId "ctx" 7UL linuxImage |> UnixBootImage.boot) "the process ID"
        refuses (fun () -> UnixSystem.writePidMaxSysctl "ctx" 1000 darwin) "no pid_max"

        refuses
            (fun () -> UnixBootImage.withLeaderThreadId "ctx" 0UL darwinImage |> UnixBootImage.boot)
            "Darwin counter"

        refuses
            (fun () ->
                UnixBootImage.withLeaderThreadId "ctx" System.UInt64.MaxValue darwinImage
                |> UnixBootImage.boot
            )
            "Darwin counter"

    [<Test>]
    let ``Linux: moving the pid moves the leader's tid and the counter with it`` () : unit =
        let system =
            linuxImage |> UnixBootImage.withProcessId "test" (pid 500) |> UnixBootImage.boot

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
            |> ignore<Result<SpawnAnswer * UnixSystem<int, string>, SpawnRefusal<int>>>

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

        // Each is a bug in the client even from inside a handler that blocks
        // something, which would otherwise be refused.
        let masked = system |> HandlerFrames.enterIn "h" 0 (Set.singleton Signal.SIGUSR1)
        fails (spawn 0 1 masked) "already names a task"
        fails (spawn 7 2 masked) "names no task"

    [<Test>]
    let ``a new thread starts with nothing pending on it, and is refused from inside a handler that blocks`` () : unit =
        // Measured on Linux 6.18.5 (aarch64 and x86-64) and Darwin 27.0.0 by
        // `docs/plans/2026-08-23-posix-kernel-extraction/thread-spawn-mask.c`: a
        // new thread's mask is its creator's, and nothing pending on the creator
        // alone is pending on it. A mask is held only as handler frames, which a
        // new thread cannot inherit, so a creator that blocks anything is refused.
        for system in [ linux ; darwin ] do
            let signals (f : SignalState<int, string> -> SignalState<int, string>) (system : UnixSystem<int, string>) =
                { system with
                    Process =
                        { system.Process with
                            Signals = f system.Process.Signals
                        }
                }

            let pendingOnLeader =
                system
                |> signals (
                    SignalState.enqueue
                        {
                            Signal = Signal.SIGUSR2
                            Target = ValueSome 0
                        }
                )

            let _, spawned = spawnOrFail 1 pendingOnLeader

            SignalState.maskOf 1 spawned.Process.Signals |> shouldEqual Set.empty

            SignalState.pending spawned.Process.Signals
            |> shouldEqual
                [
                    {
                        Signal = Signal.SIGUSR2
                        Target = ValueSome 0
                    }
                ]

            let masked =
                system
                |> HandlerFrames.enterIn "h" 0 (Set.ofList [ Signal.SIGUSR1 ; Signal.SIGTERM ])

            UnixTaskLifecycle.spawn 0 1 (CpuId 0) masked
            |> shouldEqual (
                Error (SpawnRefusal.InheritedHandlerMask (0, Set.ofList [ Signal.SIGUSR1 ; Signal.SIGTERM ]))
            )

            // A handler that blocks nothing is no reason to refuse.
            let unmasked = system |> HandlerFrames.enterIn "h" 0 Set.empty
            let _, spawned = spawnOrFail 1 unmasked
            SignalState.maskOf 1 spawned.Process.Signals |> shouldEqual Set.empty

    [<Test>]
    let ``a thread created from inside a caught signal's handler is refused, and created once the handler returns``
        ()
        : unit
        =
        // The handler blocks its own signal while it runs (no SA_NODEFER), so
        // the new thread would inherit a mask of SIGUSR1, which this library
        // cannot give it.
        for system in [ linux ; darwin ] do
            let usr1 =
                Signal.toRawSignoUnder (SignalState.numbering system.Process.Signals) Signal.SIGUSR1

            let system =
                match UnixSignal.sigaction usr1 (Some (SignalDisposition.Catch (SignalCatch.ofHandler "h"))) system with
                | Ok (_, system) -> system
                | Error error -> failwith $"sigaction failed with %O{error}"

            let system =
                match UnixSignal.pthreadKill 0 usr1 system with
                | Ok (Ok (KillOutcome.ProcessContinues system)) -> system
                | other -> failwith $"pthread_kill answered %A{other}"

            let frame, inHandler =
                match UnixSignal.onReturnToUser 0 system with
                | Ok (Some (SignalDelivery.RunHandlers [ frame ]), system) -> frame, system
                | other -> failwith $"onReturnToUser answered %A{other}"

            UnixTaskLifecycle.spawn 0 1 (CpuId 0) inHandler
            |> shouldEqual (Error (SpawnRefusal.InheritedHandlerMask (0, Set.singleton Signal.SIGUSR1)))

            // Once the handler has returned, the leader blocks nothing again.
            let returned = UnixSignal.sigreturn 0 frame.Id inHandler
            let id, spawned = spawnOrFail 1 returned
            idOf 1 spawned |> shouldEqual id
            UnixSystem.checkInvariants spawned |> shouldEqual []

    [<Test>]
    let ``Darwin: a thread created once the counter has reached the top is refused, and no ID is handed out``
        ()
        : unit
        =
        // The leader takes UInt64.MaxValue - 1, so the counter is left at
        // UInt64.MaxValue, beyond which what Darwin does is unmeasured.
        let system =
            darwinImage
            |> UnixBootImage.withLeaderThreadId "test" (System.UInt64.MaxValue - 1UL)
            |> UnixBootImage.boot

        UnixTaskLifecycle.spawn 0 1 (CpuId 0) system
        |> shouldEqual (Error SpawnRefusal.ThreadIdCounterExhausted)

        // The refusal carries no system, and the one before it is as it was:
        // the leader's is still the only live ID, so the next try is refused too.
        ThreadIdAllocator.live system.Machine.ThreadIds
        |> Set.map OsThreadId.toUInt64
        |> shouldEqual (Set.singleton (System.UInt64.MaxValue - 1UL))

        UnixTaskLifecycle.spawn 0 2 (CpuId 0) system
        |> shouldEqual (Error SpawnRefusal.ThreadIdCounterExhausted)

        // One below, the counter hands out its last ID.
        let system =
            darwinImage
            |> UnixBootImage.withLeaderThreadId "test" (System.UInt64.MaxValue - 2UL)
            |> UnixBootImage.boot

        let id, system = spawnOrFail 1 system
        id |> shouldEqual (System.UInt64.MaxValue - 1UL)

        UnixTaskLifecycle.spawn 0 2 (CpuId 0) system
        |> shouldEqual (Error SpawnRefusal.ThreadIdCounterExhausted)

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
        /// The administrator writes Linux's `pid_max`, whatever the live ids are.
        | WritePidMax of pidMax : int32

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

    /// What the model says a spawn does.
    [<RequireQualifiedAccess>]
    type private Expected =
        | Issued of id : uint64 * after : Model
        /// Linux's EAGAIN: every id it would hand out is live.
        | Exhausted
        /// Darwin's counter is at the top of its range, beyond which nothing
        /// has been measured: refused.
        | CounterAtTop

    let private modelNext (live : Set<uint64>) (model : Model) : Expected =
        match model with
        | Model.Linux (cursor, pidMax) ->
            let rec go (candidate : int32) (wrapped : bool) =
                if candidate >= pidMax then
                    if wrapped then Expected.Exhausted else go 300 true
                elif Set.contains (uint64 candidate) live then
                    go (candidate + 1) wrapped
                else
                    Expected.Issued (uint64 candidate, Model.Linux (candidate + 1, pidMax))

            go cursor false
        | Model.Darwin next ->
            if next = System.UInt64.MaxValue then
                Expected.CounterAtTop
            else
                Expected.Issued (next, Model.Darwin (next + 1UL))

    /// Coverage of the paths the property exists for, so that a generator change
    /// which stops reaching one is noticed.
    type private Coverage =
        {
            mutable Spawned : int
            mutable Wraps : int
            mutable SkippedLive : int
            mutable Exhausted : int
            /// Spawns refused because Darwin's counter is at the top.
            mutable CounterAtTop : int
            mutable Exits : int
            mutable Groups : int
            /// `pid_max` writes at or below a live id.
            mutable LoweredBeneathLive : int
            /// Linux machines that boot with a process ID at or above the pid_max
            /// then written.
            mutable BootedAbove : int
        }

    let private setupGen (flavour : SimulatedUnixFlavour) : Gen<Setup> =
        match flavour with
        | SimulatedUnixFlavour.Linux ->
            gen {
                let! pidMax = Gen.choose (301, 340)
                // Mostly near the top, so that the counter wraps in a short run;
                // sometimes anywhere below it, including under 300.
                // Sometimes at or above it: the administrator may lower pid_max
                // beneath a live id.
                let! pidValue =
                    Gen.frequency
                        [
                            3, Gen.choose (pidMax - 30, pidMax - 1)
                            1, Gen.choose (1, pidMax - 1)
                            1, Gen.choose (pidMax, pidMax + 40)
                        ]

                return Setup.Linux (pidValue, pidMax)
            }
        | SimulatedUnixFlavour.Darwin ->
            Gen.oneof
                [
                    Gen.choose (1, 100000) |> Gen.map uint64
                    Gen.constant 0xFFFF_FFF0UL
                    // Near the top of the range, so that a run reaches it.
                    Gen.choose (1, 40)
                    |> Gen.map (fun below -> System.UInt64.MaxValue - uint64 below)
                    ArbMap.defaults |> ArbMap.generate<uint32> |> Gen.map (fun i -> uint64 i + 1UL)
                ]
            |> Gen.map Setup.Darwin

    let private opGen (flavour : SimulatedUnixFlavour) : Gen<Op> =
        let common =
            [
                6, Gen.choose (0, 60) |> Gen.map Op.Spawn
                4, Gen.choose (0, 60) |> Gen.map Op.Exit
                1, Gen.choose (0, 60) |> Gen.map Op.ExitGroup
            ]

        match flavour with
        | SimulatedUnixFlavour.Linux ->
            // Mostly within the setup's range, so that a write often lands at or
            // below a live id; sometimes above it, so that ids above the first
            // pid_max are handed out and a later write can fall beneath them.
            let pidMax =
                Gen.frequency [ 3, Gen.choose (301, 340) ; 1, Gen.choose (341, 420) ]
                |> Gen.map Op.WritePidMax

            Gen.frequency ((1, pidMax) :: common)
        | SimulatedUnixFlavour.Darwin -> Gen.frequency common

    let private runModel (coverage : Coverage) (setup : Setup) (ops : Op list) : unit =
        let system, model =
            match setup with
            | Setup.Linux (pidValue, pidMax) ->
                if pidValue >= pidMax then
                    coverage.BootedAbove <- coverage.BootedAbove + 1

                linuxImage
                |> UnixBootImage.withProcessId "test" (pid pidValue)
                |> UnixBootImage.boot
                |> UnixSystem.writePidMaxSysctl "test" pidMax,
                Model.Linux (pidValue + 1, pidMax)
            | Setup.Darwin first ->
                darwinImage
                |> UnixBootImage.withLeaderThreadId "test" first
                |> UnixBootImage.boot,
                Model.Darwin (first + 1UL)

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

            // The machine's allocator records exactly the live tasks' IDs, which
            // is what it skips.
            ThreadIdAllocator.live system.Machine.ThreadIds
            |> Set.map OsThreadId.toUInt64
            |> shouldEqual (liveIds system)

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
                | Expected.Exhausted, Ok (SpawnAnswer.Failed error, after) ->
                    error |> shouldEqual UnixError.EAGAIN
                    // A failed creation hands out no thread ID and adds no task.
                    after |> shouldEqual system
                    coverage.Exhausted <- coverage.Exhausted + 1
                    system, model, nextName + 1, minted, last
                | Expected.CounterAtTop, Error refusal ->
                    refusal |> shouldEqual SpawnRefusal.ThreadIdCounterExhausted
                    // A refusal carries no system, so the run goes on from this one.
                    coverage.CounterAtTop <- coverage.CounterAtTop + 1
                    system, model, nextName + 1, minted, last
                | Expected.Issued (expected, model), Ok (SpawnAnswer.Spawned id, after) ->
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

                // Every task's ID is freed with it, and nothing else on the
                // machine moves.
                ThreadIdAllocator.live ended.Machine.ThreadIds |> shouldEqual Set.empty

                { ended.Machine with
                    ThreadIds = system.Machine.ThreadIds
                }
                |> shouldEqual system.Machine

                coverage.Groups <- coverage.Groups + 1
                system, model, nextName, minted, last
            | Op.WritePidMax pidMax ->
                let model =
                    match model with
                    | Model.Linux (cursor, _) -> Model.Linux (cursor, pidMax)
                    | Model.Darwin _ -> failwith "the generator writes pid_max only on Linux"

                if liveIds system |> Set.exists (fun id -> id >= uint64 pidMax) then
                    coverage.LoweredBeneathLive <- coverage.LoweredBeneathLive + 1

                let after = UnixSystem.writePidMaxSysctl "test" pidMax system

                // The write changes the counter's bound and nothing else: every
                // live task keeps its id.
                { after with
                    Machine =
                        { after.Machine with
                            ThreadIds = system.Machine.ThreadIds
                        }
                }
                |> shouldEqual system

                check after
                after, model, nextName, minted, last

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
                CounterAtTop = 0
                Exits = 0
                Groups = 0
                LoweredBeneathLive = 0
                BootedAbove = 0
            }

        let ops =
            gen {
                let! length = Gen.choose (0, 400)
                return! Gen.listOfLength length (opGen flavour)
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
            coverage.CounterAtTop |> shouldEqual 0
            coverage.LoweredBeneathLive |> shouldBeGreaterThan 100
            coverage.BootedAbove |> shouldBeGreaterThan 10
        | SimulatedUnixFlavour.Darwin ->
            coverage.Wraps |> shouldEqual 0
            coverage.SkippedLive |> shouldEqual 0
            coverage.Exhausted |> shouldEqual 0
            coverage.CounterAtTop |> shouldBeGreaterThan 20
            coverage.LoweredBeneathLive |> shouldEqual 0
            coverage.BootedAbove |> shouldEqual 0

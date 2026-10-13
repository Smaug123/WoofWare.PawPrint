namespace WoofWare.PosixKernel.Test

open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `SimulatedMachine.wakesRemembering` sweeps again when something a wake
/// condition reads has changed, even where nothing but that one input has.
/// `TestMultiProcessFuzz` holds it to `SimulatedMachine.wakes` over random
/// interleavings; each test here changes one input of the memo's alone, as no
/// random interleaving does, and checks that the memo does not answer.
///
/// In a Debug build, the memo also counts the answers it has given since its sweep, which decide
/// the ones it checks against a sweep (`WakeSweepMemo.crossChecks`).
///
/// Some of those states are forged: every syscall that changes a process's
/// task table or the set of processes changes the machine state too, so only
/// a hand-made state changes one without the other. The memo must not rely on
/// that.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestWakeSweepMemo =

    let private linux : SimulatedUnixPlatform = SimulatedUnixPlatform.linuxX64

    /// `wakesRemembering` of `asleep` on `machine`, which must answer what
    /// `wakes` answers.
    let private remembering
        (memo : WakeSweepMemo<int, string>)
        (asleep : Map<ProcessId, Set<int>>)
        (machine : SimulatedMachine<int, string>)
        : ((ProcessId * int) * Set<WakePrimitive>) list * WakeSweepMemo<int, string>
        =
        let woken, memo = SimulatedMachine.wakesRemembering memo asleep machine
        woken |> shouldEqual (SimulatedMachine.wakes asleep machine)
        woken, memo

    /// `task` of `pid` in a blocking `accept` on `listener`, which must sleep.
    let private parkInAccept
        (pid : ProcessId)
        (task : int)
        (listener : int)
        (machine : SimulatedMachine<int, string>)
        : SimulatedMachine<int, string>
        =
        Machines.doIn
            pid
            (fun view ->
                match UnixConnection.tryAccept task listener UserBuffer.Mapped 16u view with
                | Ok (AcceptOutcome.WouldBlock _, view) -> view
                | other -> failwith $"accept by task %d{task}: %A{other}"
            )
            machine

    [<Test>]
    let ``a signal for a sleeping task wakes it, though only the process's own state changed`` () : unit =
        let pids, machine = Machines.withTasks linux 1 2
        let pid = pids.[0]

        let listener, machine =
            Machines.inProcess
                pid
                (fun view ->
                    let view =
                        match
                            UnixSignal.sigaction 10 (Some (SignalDisposition.Catch (SignalCatch.ofHandler "h"))) view
                        with
                        | Ok (_, view) -> view
                        | Error error -> failwith $"sigaction: %O{error}"

                    KeventWorld.listenerAt 8080us view
                )
                machine

        let machine = parkInAccept pid 1 listener machine
        let asleep = Map.ofList [ pid, Set.singleton 1 ]

        let woken, memo = remembering WakeSweepMemo.empty asleep machine
        woken |> shouldEqual []

        // Task 0 sends SIGUSR1, which task 1 catches, to task 1.
        let signalled =
            Machines.doIn
                pid
                (fun view ->
                    match UnixSignal.tryPthreadKill 1 10 view with
                    | Ok (Ok (ProcessFate.Continues (KillOutcome.ProcessRuns, view))) -> view
                    | other -> failwith $"pthread_kill: %A{other}"
                )
                machine

        LanguagePrimitives.PhysicalEquality signalled.Machine machine.Machine
        |> shouldEqual true

        LanguagePrimitives.PhysicalEquality signalled.Processes.[pid].Tasks machine.Processes.[pid].Tasks
        |> shouldEqual true

        remembering memo asleep signalled
        |> fst
        |> shouldEqual [ (pid, 1), Set.singleton WakePrimitive.SignalDeliverable ]

    /// One process whose tasks 0 and 1 sleep in `accept` on its listener, which
    /// holds a connection; the sweep has woken task 0, whose call has not yet
    /// finished, so task 1 waits behind it. The process's ID, and the machine.
    let private behindAWokenAccepter () : ProcessId * SimulatedMachine<int, string> =
        let pids, machine = Machines.withTasks linux 1 3
        let pid = pids.[0]

        let listener, machine =
            Machines.inProcess pid (KeventWorld.listenerAt 8080us) machine

        let machine =
            machine
            |> parkInAccept pid 0 listener
            |> parkInAccept pid 1 listener
            // Task 2 connects: tasks 0 and 1 are asleep, and a task asleep in
            // one call makes no other.
            |> Machines.doIn pid (fun view -> KeventWorld.clientAs 2 8080us view |> snd)

        SimulatedMachine.wakes (Map.ofList [ pid, Set.ofList [ 0 ; 1 ] ]) machine
        |> List.map fst
        |> shouldEqual [ pid, 0 ]

        pid, machine

    /// `machine` with task 0 of `pid` unparked in the task table alone, as its
    /// finished `accept` leaves it: a forged state, whose open file table still
    /// counts the park's hold.
    let private finishedInTasksAlone
        (pid : ProcessId)
        (machine : SimulatedMachine<int, string>)
        : SimulatedMachine<int, string>
        =
        let slot = machine.Processes.[pid]

        { machine with
            Processes =
                Map.add
                    pid
                    { slot with
                        Tasks = UnixTaskTable.unpark 0 slot.Tasks
                    }
                    machine.Processes
        }

    [<Test>]
    let ``a woken accepter's finish lets the next one wake, though only the task table changed`` () : unit =
        let pid, machine = behindAWokenAccepter ()
        let asleep = Map.ofList [ pid, Set.singleton 1 ]

        let woken, memo = remembering WakeSweepMemo.empty asleep machine
        woken |> shouldEqual []

        remembering memo asleep (finishedInTasksAlone pid machine)
        |> fst
        |> List.map fst
        |> shouldEqual [ pid, 1 ]

    [<Test>]
    let ``a process gone, or another in its place, is swept again, though nothing else changed`` () : unit =
        let pid, machine = behindAWokenAccepter ()
        let asleep = Map.ofList [ pid, Set.singleton 1 ]
        let finished = finishedInTasksAlone pid machine
        let other = ProcessId.parseOrFail "TestWakeSweepMemo" 9999

        // A second process holding a copy of the first's tasks, task 0 among
        // them asleep in its accept and not woken: task 1 of the first
        // waits behind it.
        let withCopy =
            { finished with
                Processes = Map.add other machine.Processes.[pid] finished.Processes
            }

        let woken, memo = remembering WakeSweepMemo.empty asleep withCopy
        woken |> shouldEqual []

        // Without it, task 1 wakes.
        remembering memo asleep finished
        |> fst
        |> List.map fst
        |> shouldEqual [ pid, 1 ]

        // With a process of no tasks under another ID in its place, task 1
        // wakes too.
        let replaced =
            { finished with
                Processes =
                    Map.add
                        (ProcessId.parseOrFail "TestWakeSweepMemo" 9998)
                        { machine.Processes.[pid] with
                            Tasks = Map.empty
                        }
                        finished.Processes
            }

        remembering memo asleep replaced
        |> fst
        |> List.map fst
        |> shouldEqual [ pid, 1 ]

    [<Test>]
    let ``a woken task the client holds asleep again is swept again, though the machine did not change`` () : unit =
        let pid, machine = behindAWokenAccepter ()

        let woken, memo =
            remembering WakeSweepMemo.empty (Map.ofList [ pid, Set.singleton 1 ]) machine

        woken |> shouldEqual []

        // Task 0 is asleep again in the client's eyes, and nothing waits behind
        // it now: the one waiter the listener wakes is the first to park.
        remembering memo (Map.ofList [ pid, Set.ofList [ 0 ; 1 ] ]) machine
        |> fst
        |> List.map fst
        |> shouldEqual [ pid, 0 ]

    [<Test>]
    let ``the earliest of several deadlines ends the memo's answer, not the latest`` () : unit =
        let pids, machine = Machines.withTasks linux 1 2
        let pid = pids.[0]

        let listener, machine =
            Machines.inProcess pid (KeventWorld.listenerAt 8080us) machine

        // Tasks 0 and 1 poll the idle listener, for 5 ms and for 10 ms.
        let pollFor (task : int) (milliseconds : int) (machine : SimulatedMachine<int, string>) =
            Machines.doIn
                pid
                (fun view ->
                    let entry =
                        {
                            Fd = listener
                            Events = 0x0001s
                        }

                    match UnixPoll.tryPoll task [ entry ] milliseconds view with
                    | Ok (PollOutcome.WouldBlock _, view) -> view
                    | other -> failwith $"poll by task %d{task}: %A{other}"
                )
                machine

        let machine = machine |> pollFor 0 5 |> pollFor 1 10
        let asleep = Map.ofList [ pid, Set.ofList [ 0 ; 1 ] ]

        let woken, memo = remembering WakeSweepMemo.empty asleep machine
        woken |> shouldEqual []

        let later = Machines.doIn pid (UnixSystem.advanceClock 7_000_000L) machine

        LanguagePrimitives.PhysicalEquality later.Machine machine.Machine
        |> shouldEqual true

        match remembering memo asleep later |> fst with
        | [ (woke, fired) ] ->
            woke |> shouldEqual (pid, 0)

            fired
            |> Set.forall (fun primitive ->
                match primitive with
                | WakePrimitive.DeadlinePassed _ -> true
                | _ -> false
            )
            |> shouldEqual true
        | other -> failwith $"expected task 0 alone to wake for its deadline, but %A{other} woke"

    [<Test>]
    let ``the cross-check takes every one of the first answers after a sweep, and at least one in every so many after``
        ()
        : unit
        =
        let hits = 100_000
        let checkedHits = [ 0 .. hits - 1 ] |> List.filter WakeSweepMemo.crossChecks

        checkedHits
        |> List.take WakeSweepMemo.CrossCheckedFirst
        |> shouldEqual [ 0 .. WakeSweepMemo.CrossCheckedFirst - 1 ]

        // No run of `CrossCheckedEvery` answers in a row goes unchecked.
        (checkedHits @ [ hits ])
        |> List.pairwise
        |> List.forall (fun (before, after) -> after - before <= WakeSweepMemo.CrossCheckedEvery)
        |> shouldEqual true

        // And no more are checked than that needs.
        checkedHits.Length
        |> shouldBeSmallerThan (WakeSweepMemo.CrossCheckedFirst + hits / WakeSweepMemo.CrossCheckedEvery + 2)

    [<Test>]
    let ``a memo counts the answers it has given since its sweep, from none after each sweep`` () : unit =
#if DEBUG
        let pid, machine = behindAWokenAccepter ()
        let asleep = Map.ofList [ pid, Set.singleton 1 ]

        let woken, swept = remembering WakeSweepMemo.empty asleep machine
        woken |> shouldEqual []
        swept.HitsSinceSweep |> shouldEqual 0

        let answered =
            (swept, [ 1..5 ])
            ||> List.fold (fun memo expected ->
                let woken, memo = remembering memo asleep machine
                woken |> shouldEqual []
                memo.HitsSinceSweep |> shouldEqual expected
                memo
            )

        // The process's tasks, the same tasks in another map: the memo does not answer, and the
        // sweep wakes nobody, so the memo remembers it afresh.
        let slot = machine.Processes.[pid]

        let copied =
            { machine with
                Processes =
                    Map.add
                        pid
                        { slot with
                            Tasks = Map.ofList (Map.toList slot.Tasks)
                        }
                        machine.Processes
            }

        let woken, reswept = remembering answered asleep copied
        woken |> shouldEqual []
        reswept.HitsSinceSweep |> shouldEqual 0

        let woken, memo = remembering reswept asleep copied
        woken |> shouldEqual []
        memo.HitsSinceSweep |> shouldEqual 1
#else
        Assert.Ignore "A memo counts its answers only in a Debug build, which cross-checks some of them."
#endif

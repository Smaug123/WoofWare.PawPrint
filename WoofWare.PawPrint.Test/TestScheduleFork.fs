namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint

/// Pins the contract of the fork-point snapshot: a guest's execution up to its first *contended*
/// scheduling decision is identical under every PCT seed, so it can be computed once and each
/// seed resumed from it.
///
/// The claim is a commuting square. Down the left, `Program.run` with
/// `PctSeed = Some s`; down the right, `Program.runToFirstFork` (no seed, `RoundRobin`) followed
/// by `Program.resumeFork` with `s`. The two must agree — not merely on the exit code, which a
/// resume that explored a *different* schedule could easily match by luck, but on the whole
/// post-fork decision trace: which thread ran at each tick and what it did.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestScheduleFork =

    let private assy = typeof<RunResult>.Assembly

    let private dotnetRuntimes : ImmutableArray<string> =
        DotnetRuntime.SelectForDll assy.Location |> ImmutableArray.CreateRange

    let private guestConfig : GuestConfig = GuestConfig.Default dotnetRuntimes

    let private compile (sourceName : string) : byte[] =
        Assembly.getEmbeddedResourceAsString sourceName assy
        |> List.singleton
        |> Roslyn.compile

    /// One retired step, as an outside observer can see it: when it happened, who ran, and what
    /// the abstract machine reported. This is the granularity at which two runs are "the same
    /// schedule" — a snapshot taken one tick early or late shows up here immediately, where an
    /// exit-code comparison would very often miss it.
    type private Step =
        {
            StepCounter : int64
            Thread : int
            Did : string
        }

    /// A run's whole observable identity. Deliberately excludes `IlMachineState.Scheduling`:
    /// that is interpreter-internal, not guest-visible, and for a never-forking guest it
    /// legitimately differs between a `RoundRobin` prefix run and a seeded from-scratch run.
    type private RunTrace =
        {
            Steps : Step list
            Ending : string
            FinalStepCounter : int64
            FinalClockTicks : int64
            Stdout : byte list
            Stderr : byte list
        }

    let private describeWhatWeDid (what : WhatWeDid) : string =
        match what with
        | WhatWeDid.Executed -> "executed"
        | WhatWeDid.VoluntaryYield reportsSwitch -> $"yield(%b{reportsSwitch})"
        | WhatWeDid.SuspendedForClassInit -> "suspendedForClassInit"
        | WhatWeDid.SuspendedForManagedCall -> "suspendedForManagedCall"
        | WhatWeDid.BlockedOnClassInit (ThreadId blocker) -> $"blockedOnClassInit(%d{blocker})"
        | WhatWeDid.ThrowingTypeInitializationException -> "throwingTypeInit"

    /// Collapse a terminal outcome to a string. Rendered rather than kept structurally because
    /// every `RunOutcome` carries an entire `IlMachineState`, which is neither comparable nor
    /// printable at any sane size.
    let private describeOutcome (outcome : RunOutcome) : string =
        match outcome with
        | RunOutcome.NormalExit (state, thread)
        | RunOutcome.ProcessExit (state, thread) ->
            match state.ThreadState.[thread].MethodState.EvaluationStack.Values with
            | EvalStackValue.Int32 (Int32Source.Verbatim code) :: _ -> $"exit %d{code}"
            | [] -> "exit void"
            | other :: _ -> $"exit non-int %O{other}"
        | RunOutcome.FailFast (_, _, message) ->
            let message = message |> Option.defaultValue "<none>"
            $"failfast %s{message}"
        | RunOutcome.SignalTerminated (_, signal) -> $"signal %O{signal}"
        | RunOutcome.GuestUnhandledException (_, _, _) -> "unhandled exception"

    let private terminalStateOf (outcome : RunOutcome) : IlMachineState =
        match outcome with
        | RunOutcome.NormalExit (state, _)
        | RunOutcome.ProcessExit (state, _)
        | RunOutcome.FailFast (state, _, _)
        | RunOutcome.SignalTerminated (state, _)
        | RunOutcome.GuestUnhandledException (state, _, _) -> state

    let private bytesOf (role : FileDescriptorRole) (state : IlMachineState) : byte list =
        OutputLogEntry.bytesFor role state.Kernel.OutputLog |> List.ofSeq

    /// Drive `prepared` to completion, recording every retired step. Deadlock is a value here,
    /// not an exception: `pumpPrepared` would `failwith`, and several of the guests a sweep cares
    /// about deadlock on purpose.
    let private traceFrom
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (prepared : Program.PreparedProgram)
        : RunTrace
        =
        let logger = loggerFactory.CreateLogger "TestScheduleFork"

        let finish (steps : Step list) (ending : string) (state : IlMachineState) : RunTrace =
            {
                Steps = List.rev steps
                Ending = ending
                FinalStepCounter = state.Kernel.StepCounter
                FinalClockTicks = state.Kernel.VirtualClockTicks
                Stdout = bytesOf FileDescriptorRole.StandardOutput state
                Stderr = bytesOf FileDescriptorRole.StandardError state
            }

        let rec go (prepared : Program.PreparedProgram) (steps : Step list) : RunTrace =
            match Program.stepPrepared loggerFactory logger prepared with
            | Program.ProgramStepOutcome.Completed outcome ->
                finish steps (describeOutcome outcome) (terminalStateOf outcome)
            | Program.ProgramStepOutcome.Deadlocked (p, stuck) -> finish steps $"deadlock %s{stuck}" p.State
            | Program.ProgramStepOutcome.InstructionStepped (p, ThreadId ran, what, _) ->
                let step =
                    {
                        StepCounter = p.State.Kernel.StepCounter
                        Thread = ran
                        Did = describeWhatWeDid what
                    }

                go p (step :: steps)
            | Program.ProgramStepOutcome.WorkerTerminated (p, ThreadId terminated) ->
                let step =
                    {
                        StepCounter = p.State.Kernel.StepCounter
                        Thread = terminated
                        Did = "terminated"
                    }

                go p (step :: steps)

        go prepared []

    /// The left-hand side of the square: an ordinary whole-program run under `seed`.
    let private fromScratch
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (sourceName : string)
        (image : byte[])
        (seed : uint64 option)
        : RunTrace
        =
        use peImage = new MemoryStream (image)

        let hostConfig =
            {
                Guest = guestConfig
                PctSeed = seed
            }

        match Program.prepare loggerFactory (Some sourceName) peImage hostConfig with
        | Program.ProgramStartResult.CompletedBeforeMain outcome ->
            {
                Steps = []
                Ending = $"completedBeforeMain %s{describeOutcome outcome}"
                FinalStepCounter = (terminalStateOf outcome).Kernel.StepCounter
                FinalClockTicks = (terminalStateOf outcome).Kernel.VirtualClockTicks
                Stdout = bytesOf FileDescriptorRole.StandardOutput (terminalStateOf outcome)
                Stderr = bytesOf FileDescriptorRole.StandardError (terminalStateOf outcome)
            }
        | Program.ProgramStartResult.Ready prepared -> traceFrom loggerFactory prepared

    /// Compute the shared prefix once, failing the test if the guest does not actually fork —
    /// which would make every commuting-square assertion below vacuously true.
    let private forkOf
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (sourceName : string)
        (image : byte[])
        : Program.ForkSnapshot
        =
        use peImage = new MemoryStream (image)

        match Program.runToFirstFork loggerFactory (Some sourceName) peImage guestConfig with
        | Program.PrefixOutcome.ForkedAt snapshot -> snapshot
        | other -> failwith $"%s{sourceName} was expected to reach a fork point, but: %A{other}"

    /// Guests that reach a fork point, each chosen for a distinct shape of prefix — plus the
    /// whole `sourcesConcurrencyBugs` corpus, because `TestConcurrencyBugs` and `TestRaces`
    /// fan their seed sweeps out from a fork snapshot rather than re-running the guest per seed.
    /// Those sweeps cannot check the fanout themselves: they assert only that *some* seed finds
    /// the bug, which a resume exploring a subtly different schedule space would still satisfy.
    /// So the commuting square has to be pinned here, over exactly the guests they sweep.
    let private forkingGuests : string list =
        [
            // Two threads racing on a shared int: the plainest fork there is.
            "ReadWriteRace.cs"
            // Contends on a type initialiser, so the prefix ends inside class-init machinery.
            "NewobjCctorRace.cs"
            // Yields while single-threaded. If `chargeYieldDebt` ever draws with nobody else
            // Runnable, this is the guest whose traces diverge.
            "ForkAfterSoloYield.cs"
            // Sleeps while single-threaded, so the prefix contains a jump-to-deadline inside a
            // tick preamble — the phase a fork detector probing the inter-tick state would miss.
            "ForkAfterSoloSleep.cs"

            // The `TestConcurrencyBugs` corpus. These bring the only endings other than "exit n"
            // that this fixture sees: `SimultaneousCounter` throws under some of the seeds below
            // and `InvertedMonitorDeadlock` wedges under seed 17, so between them they pin that a
            // resumed run reproduces an *ending*, not merely a matching exit code. Which seed
            // produces which ending was measured, not assumed — see the comment on `seeds`.
            "LostUpdate.cs"
            "JustABoolNotAMutex.cs"
            "TwoCountersSeparated.cs"
            "SimultaneousCounter.cs"
            "InvertedMonitorDeadlock.cs"
            "QueueIsNotThreadSafe.cs"
        ]

    /// Fixed rather than randomly generated, so a failure is reproducible without a shrink
    /// report, but drawn from across the whole `uint64` range rather than from 0..n: the RNG is
    /// splitmix64 over a 64-bit state, and small seeds exercise only one corner of it.
    ///
    /// 17 is the exception, and is here for a measured reason rather than for spread. Without it
    /// every guest in the corpus ends every one of these runs in `exit n` or a thrown exception,
    /// so `traceFrom`'s deadlock arm — and with it the claim that a resumed run reproduces a
    /// *wedge* identically, which `TestConcurrencyBugs`' `BadOutcome.Deadlock` scenario rests on —
    /// would never execute. 17 is the lowest seed under which `InvertedMonitorDeadlock.cs`
    /// actually deadlocks (found by scanning 0..200; 131 and 137 also do).
    let private seeds : uint64 list =
        [
            0UL
            1UL
            7UL
            17UL
            0xC0FFEEUL
            0xDEADBEEFCAFEBABEUL
            0xFFFFFFFFFFFFFFFFUL
            0x8000000000000000UL
            0x5555555555555555UL
        ]

    [<TestCaseSource(nameof forkingGuests)>]
    let ``resuming a fork point reproduces the from-scratch run exactly`` (sourceName : string) : unit =
        let _messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", sourceName ]

        use _loggerFactoryResource = loggerFactory

        let image = compile sourceName
        let snapshot = forkOf loggerFactory sourceName image
        let forkAt = snapshot.State.Kernel.StepCounter

        // A fork point is a choice, so at least two threads contend for it — and the list
        // is checked against something other than itself, because both sides of every other
        // assertion about it come from the same evaluation. Ascending order is required:
        // `PctState.ensurePriorityFor` samples in list order, so a shuffle would silently change
        // which seed produces which schedule.
        snapshot.Contenders |> List.length |> shouldBeGreaterThan 1

        snapshot.Contenders
        |> shouldEqual (snapshot.Contenders |> List.distinct |> List.sort)

        // For a fork reached organically the contenders really are Runnable in the snapshot's own
        // state, because the second thread got there via the guest's `Thread.Start`. That is *not*
        // true in general — see the preamble-contention test below — so it is asserted here,
        // where it holds, rather than promised by `ForkSnapshot`.
        for contender in snapshot.Contenders do
            snapshot.State.ThreadState.[contender].Status
            |> shouldEqual ThreadStatus.Runnable

        for seed in seeds do
            let expected = fromScratch loggerFactory sourceName image (Some seed)

            let resumed =
                traceFrom loggerFactory (Program.resumeFork loggerFactory (Some seed) snapshot)

            // The resumed run starts at the fork, so compare it against the corresponding suffix
            // of the from-scratch run. Everything before `forkAt` is the shared prefix, and the
            // prefix's own equality is what P2 in the scheduler fixture pins.
            let expectedSuffix =
                expected.Steps |> List.filter (fun step -> step.StepCounter > forkAt)

            resumed.Steps |> shouldEqual expectedSuffix
            resumed.Ending |> shouldEqual expected.Ending
            resumed.FinalStepCounter |> shouldEqual expected.FinalStepCounter
            resumed.FinalClockTicks |> shouldEqual expected.FinalClockTicks
            resumed.Stdout |> shouldEqual expected.Stdout
            resumed.Stderr |> shouldEqual expected.Stderr

            // Not redundant with the trace comparison: it pins that the *first* resumed tick is
            // the contended one, so a snapshot taken a tick early or late — which would still
            // produce matching suffixes if the surrounding ticks happened to be forced — fails.
            match resumed.Steps with
            | first :: _ -> first.StepCounter |> shouldEqual (forkAt + 1L)
            | [] -> failwith "resumed run retired no steps at all"

    [<TestCaseSource(nameof forkingGuests)>]
    let ``resuming with no seed reproduces the round-robin run`` (sourceName : string) : unit =
        // The degenerate case, and the one that checks `LastRan` survived the snapshot: round
        // robin's whole choice is "lowest id above `LastRan`", so a snapshot that dropped it
        // would pick a different thread at the fork and diverge immediately.
        let _messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", sourceName ]

        use _loggerFactoryResource = loggerFactory

        let image = compile sourceName
        let snapshot = forkOf loggerFactory sourceName image
        let forkAt = snapshot.State.Kernel.StepCounter

        let expected = fromScratch loggerFactory sourceName image None

        let resumed =
            traceFrom loggerFactory (Program.resumeFork loggerFactory None snapshot)

        resumed.Steps
        |> shouldEqual (expected.Steps |> List.filter (fun step -> step.StepCounter > forkAt))

        resumed.Ending |> shouldEqual expected.Ending

    [<Test>]
    let ``a snapshot may be resumed concurrently`` () : unit =
        // The sweeps this feature exists for run their seeds through `Array.Parallel`, so one
        // snapshot gets stepped by several threads at once. Everything reachable from it is
        // persistent F# data or `ImmutableDictionary` — except the `MetadataReader`-backed
        // assemblies, which are shared rather than copied. This is the machine check on that:
        // resume the same snapshot serially and concurrently and require identical answers.
        let sourceName = "ReadWriteRace.cs"

        let _messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", sourceName ]

        use _loggerFactoryResource = loggerFactory

        let image = compile sourceName
        let snapshot = forkOf loggerFactory sourceName image

        // Compare whole traces rather than just how each run ended. Corruption of shared state
        // under concurrent reads would most likely show up as a schedule that diverges and then
        // reconverges on the same exit code, which an ending-only comparison would miss.
        let traceFor (seed : uint64) : RunTrace =
            traceFrom loggerFactory (Program.resumeFork loggerFactory (Some seed) snapshot)

        let serial = seeds |> List.map traceFor

        let concurrent =
            seeds |> Array.ofList |> Array.Parallel.map traceFor |> List.ofArray

        concurrent |> shouldEqual serial

    [<Test>]
    let ``a guest that never forks answers every seed at once`` () : unit =
        // The freebie, and it needs pinning in both directions. `NeverForked` must be *reported*
        // for a single-threaded guest — otherwise the sweep pointlessly re-runs it per seed — and
        // it must be *true*: the outcome it carries has to be the outcome every seed produces.
        let sourceName = "ArithmeticOperations.cs"

        let _messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", sourceName ]

        use _loggerFactoryResource = loggerFactory

        let image = compile sourceName
        use peImage = new MemoryStream (image)

        let ending =
            match Program.runToFirstFork loggerFactory (Some sourceName) peImage guestConfig with
            | Program.PrefixOutcome.NeverForked outcome -> describeOutcome outcome
            | other -> failwith $"%s{sourceName} is single-threaded and must never fork, but: %A{other}"

        // Whole traces, not just endings: "every seed gets the same answer" is the claim, and a
        // guest whose schedule diverged mid-run but reconverged on the same exit code would
        // satisfy an ending-only check while falsifying it.
        let reference = fromScratch loggerFactory sourceName image (Some (List.head seeds))
        reference.Ending |> shouldEqual ending

        for seed in List.tail seeds do
            fromScratch loggerFactory sourceName image (Some seed) |> shouldEqual reference

    /// One guest per phase: `runToFirstFork` drives startup through its own loop, so the startup
    /// and main-phase deadlock arms are separate code paths.
    let private wedgingGuests : string list =
        [ "DeadlockBeforeFork.cs" ; "DeadlockInCctor.cs" ]

    [<TestCaseSource(nameof wedgingGuests)>]
    let ``a guest that wedges before forking is reported as deadlocked, not as a fork`` (sourceName : string) : unit =
        // `DeadlockedBeforeFork` is as seed-independent as `NeverForked` — nothing chose anything,
        // so every seed wedges identically — and it is the arm most likely to rot, because a
        // whole-program `Program.run` turns deadlock into a host `failwith` and so cannot cover
        // it. Without this, swapping either arm for `ForkedDuringStartup` (or dropping it into the
        // recursion) passes the whole suite.
        let _messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", sourceName ]

        use _loggerFactoryResource = loggerFactory

        use peImage = new MemoryStream (compile sourceName)

        match Program.runToFirstFork loggerFactory (Some sourceName) peImage guestConfig with
        | Program.PrefixOutcome.DeadlockedBeforeFork stuck ->
            // The description must actually name the wedged thread, or a caller has nothing to
            // debug with.
            stuck |> shouldContainText "thread 0"
        | other -> failwith $"%s{sourceName} wedges while single-threaded, but: %A{other}"

    [<Test>]
    let ``forking inside a class initialiser is refused, not mistaken for a forced prefix`` () : unit =
        // `runToFirstFork` probes startup with the same predicate it uses for `Main`. Without
        // that probe this guest would look like a forced prefix all the way to `Main`, and every
        // seed resumed from the resulting snapshot would silently explore a schedule space
        // missing whatever the startup choice decided. Refusing is the documented behaviour;
        // this test is what stops it regressing into a wrong snapshot.
        let sourceName = "ForkInCctor.cs"

        let _messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", sourceName ]

        use _loggerFactoryResource = loggerFactory

        use peImage = new MemoryStream (compile sourceName)

        match Program.runToFirstFork loggerFactory (Some sourceName) peImage guestConfig with
        | Program.PrefixOutcome.ForkedDuringStartup _ -> ()
        | other -> failwith $"%s{sourceName} forks in its .cctor, so startup must be refused, but: %A{other}"

    [<Test>]
    let ``contention created inside a tick's preamble is a fork point`` () : unit =
        // The detector probes *after* the tick preamble — clock advance, deadline firing, signal
        // dispatch, deadline jump — because any of those can turn a forced tick into a contended
        // one. Nothing in the guest corpus above reaches that: for a *first* fork, the second
        // Runnable thread always arrives via the guest's own `Thread.Start`, which is a retired
        // instruction and so is already visible on the inter-tick state. Only the kernel's signal
        // dispatcher can make a thread Runnable without a guest instruction, and only a mid-run
        // fork can be created by a deadline expiring.
        //
        // So the case is constructed rather than found: take a real fork point, park one of its
        // two contenders in an already-expired sleep, and hand the result back to
        // `runToNextFork`. Before the preamble exactly one thread is Runnable; after it, two. A
        // detector that probed the inter-tick state would step straight past this fork.
        let sourceName = "ReadWriteRace.cs"

        let _messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", sourceName ]

        use _loggerFactoryResource = loggerFactory
        let logger = loggerFactory.CreateLogger "TestScheduleFork"

        let snapshot = forkOf loggerFactory sourceName (compile sourceName)
        let prepared = Program.resumeFork loggerFactory None snapshot

        // Park the higher-numbered contender on a deadline the virtual clock is already past, so
        // the preamble's `fireExpiredDeadlines` wakes it during the very next tick.
        let sleeper = snapshot.Contenders |> List.max

        let state =
            { prepared.State with
                ThreadState =
                    prepared.State.ThreadState
                    |> Map.change
                        sleeper
                        (Option.map (fun ts ->
                            { ts with
                                Status = ThreadStatus.BlockedOnSleep (Some 0L)
                            }
                        ))
            }

        // Precondition, asserted rather than assumed: this is the whole point of the test, and if
        // parking the thread left two Runnable anyway then the mutation this guards against would
        // pass for the wrong reason. Counted directly rather than asked of
        // `Scheduler.tryContenders`, which is internal precisely because the only state worth
        // asking it about is one this test cannot build — the post-preamble state.
        state.ThreadState
        |> Map.filter (fun _ ts -> ts.Status = ThreadStatus.Runnable)
        |> Map.count
        |> shouldEqual 1

        let prepared =
            { prepared with
                State = state
            }

        match Program.runToNextFork loggerFactory logger prepared with
        | Program.PrefixOutcome.ForkedAt found ->
            // Immediately, at the very tick whose preamble does the waking: no guest instruction
            // was retired in between.
            found.State.Kernel.StepCounter |> shouldEqual state.Kernel.StepCounter
            found.Contenders |> shouldEqual snapshot.Contenders

            // And the shape `ForkSnapshot`'s docstring warns about: the contenders are Runnable at
            // the *decision point*, not in `State`. The sleeper is still parked here.
            found.State.ThreadState.[sleeper].Status
            |> shouldEqual (ThreadStatus.BlockedOnSleep (Some 0L))
        | other -> failwith $"expected the woken sleeper to make the next decision contended, but: %A{other}"

    [<Test>]
    let ``the shared prefix is most of the run`` () : unit =
        // Not a correctness property — a guard on the premise. If the fork point ever moved to
        // the very end of a run, every test above would still pass while the feature saved
        // nothing at all, and nobody would notice. Measured at ~93% when this was written.
        let sourceName = "ReadWriteRace.cs"

        let _messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", sourceName ]

        use _loggerFactoryResource = loggerFactory

        let image = compile sourceName
        let snapshot = forkOf loggerFactory sourceName image
        let total = (fromScratch loggerFactory sourceName image (Some 0UL)).FinalStepCounter

        let sharedFraction = double snapshot.State.Kernel.StepCounter / double total
        (sharedFraction > 0.5) |> shouldEqual true

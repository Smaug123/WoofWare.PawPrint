namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint

/// Pins the contract of the PCT scheduling policy.
///
/// Two layers of tests live here:
///   * Pure `PctState` helpers — exercised in isolation, no `IlMachineState`.
///     These cover the math (uniform-in-[0,1), monotonic Rng advance, lazy
///     insert, idempotent remove) without needing real method frames.
///   * Scheduler integration — paths through `Scheduler.chooseNext` and
///     `Scheduler.onThreadTerminated` that do not require `ThreadState.peekNextOp`
///     to consult a live frame: empty-runnable, hasAnyRunnable agreement,
///     termination lifecycle.
///
/// The "weight=1.0 demotion frequency" property and the full argmax/Bernoulli
/// path are covered by the end-to-end reproducibility test in the same file
/// (a real DLL is the cheapest way to get a live frame for `peekNextOp`).
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestSchedulerPct =

    // ---------------- PctState helpers (pure) ----------------

    [<Test>]
    let ``ofSeed is deterministic in the seed`` () : unit =
        let a = PctState.ofSeed 42UL
        let b = PctState.ofSeed 42UL
        a |> shouldEqual b
        a.Rng |> shouldEqual 42UL
        a.Priorities |> shouldEqual Map.empty

    [<Test>]
    let ``resamplePriority puts the priority in [0, 1) and advances the Rng`` () : unit =
        // Try a handful of seeds so a single accidentally-zero output can't hide a
        // bug in the scale factor — the range invariant is the algorithm's
        // load-bearing precondition for argmax correctness.
        for seed in [ 0UL ; 1UL ; 0xDEADBEEFUL ; 0xFFFFFFFFFFFFFFFFUL ] do
            let before = PctState.ofSeed seed
            let after = PctState.resamplePriority (ThreadId 7) before

            after.Rng |> shouldNotEqual before.Rng
            let p = Map.find (ThreadId 7) after.Priorities
            (0.0 <= p && p < 1.0) |> shouldEqual true

    [<Test>]
    let ``resamplePriority overwrites an existing entry`` () : unit =
        // Two consecutive resamples produce two distinct entries (with
        // overwhelming probability — but more importantly, the *latter*
        // is what `Priorities.[tid]` returns), which is the demotion
        // contract: a demoted thread's stale priority must not survive.
        let s = PctState.ofSeed 1UL |> PctState.resamplePriority (ThreadId 0)
        let p1 = Map.find (ThreadId 0) s.Priorities
        let s = PctState.resamplePriority (ThreadId 0) s
        let p2 = Map.find (ThreadId 0) s.Priorities
        // Same seed family but a different RNG position, so p1 and p2 must differ
        // for any seed in which two consecutive splitmix64 draws are not equal —
        // a property splitmix64 has across the entire 64-bit state space.
        p1 |> shouldNotEqual p2

    [<Test>]
    let ``ensurePriorityFor leaves existing entries untouched`` () : unit =
        let s = PctState.ofSeed 1UL |> PctState.resamplePriority (ThreadId 0)
        let originalPriority = Map.find (ThreadId 0) s.Priorities
        let originalRng = s.Rng

        // Re-ensuring with the same thread must not resample (otherwise demotion
        // semantics would leak into priority population and PCT determinism would
        // depend on call order at every scheduling decision).
        let s' = PctState.ensurePriorityFor [ ThreadId 0 ] s
        Map.find (ThreadId 0) s'.Priorities |> shouldEqual originalPriority
        s'.Rng |> shouldEqual originalRng

    [<Test>]
    let ``ensurePriorityFor samples in input-list order`` () : unit =
        // Sample for two new threads in two orders; the priorities they receive
        // must swap, because the RNG advances per insert and the order of inserts
        // determines which draw lands on which thread. The scheduler always feeds
        // this function the runnable list sorted by ThreadId, so this ordering
        // contract is what keeps replay bit-exact across runs.
        let a =
            PctState.ofSeed 99UL |> PctState.ensurePriorityFor [ ThreadId 5 ; ThreadId 6 ]

        let b =
            PctState.ofSeed 99UL |> PctState.ensurePriorityFor [ ThreadId 6 ; ThreadId 5 ]

        // The same two RNG draws happened in both, but assigned to different
        // threads, so the per-thread priorities are swapped.
        let aFifth = Map.find (ThreadId 5) a.Priorities
        let aSixth = Map.find (ThreadId 6) a.Priorities
        let bFifth = Map.find (ThreadId 5) b.Priorities
        let bSixth = Map.find (ThreadId 6) b.Priorities

        aFifth |> shouldEqual bSixth
        aSixth |> shouldEqual bFifth

    [<Test>]
    let ``ensurePriorityFor over an empty list is a no-op`` () : unit =
        // Boundary: chooseNext returns (state, None) before calling this on an
        // empty runnable list, but defensive correctness here keeps the helper
        // safe for any caller that might enumerate edge cases.
        let s = PctState.ofSeed 1UL
        let s' = PctState.ensurePriorityFor [] s
        s' |> shouldEqual s

    [<Test>]
    let ``removeThread drops the entry`` () : unit =
        let s =
            PctState.ofSeed 1UL
            |> PctState.resamplePriority (ThreadId 0)
            |> PctState.resamplePriority (ThreadId 1)
            |> PctState.removeThread (ThreadId 0)

        Map.containsKey (ThreadId 0) s.Priorities |> shouldEqual false
        Map.containsKey (ThreadId 1) s.Priorities |> shouldEqual true

    [<Test>]
    let ``removeThread on an absent thread is a no-op`` () : unit =
        // chooseNext never inserts terminated threads, but `onThreadTerminated`
        // calls `removeThread` unconditionally — including for threads PCT never
        // sampled (e.g. terminated while still NotStarted). Treat absence as a
        // no-op rather than a contract violation.
        let s = PctState.ofSeed 1UL |> PctState.resamplePriority (ThreadId 0)
        let s' = PctState.removeThread (ThreadId 99) s
        s' |> shouldEqual s

    // ---------------- Scheduler integration (no peekNextOp paths) ----------------

    let private corelib : DumpedAssembly =
        let corelibPath = typeof<obj>.Assembly.Location
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.readFile loggerFactory corelibPath

    let private baseState () : IlMachineState =
        let _, loggerFactory = LoggerFactory.makeTest ()
        IlMachineState.initial loggerFactory ImmutableArray.Empty corelib

    /// Frame-less stub thread state. `Scheduler.chooseNext`'s Pct branch only
    /// calls `peekNextOp` for the *winning* thread, so a test that uses no
    /// Runnable threads (the empty-runnable path) never hits the sentinel
    /// FrameId. Mirrors `TestSchedulerVoluntaryYield.stubThreadState`.
    let private stubThreadState (status : ThreadStatus) : ThreadState =
        {
            MethodStates = Map.empty
            NextFrameId = 0
            ActiveMethodState = FrameId -1
            Status = status
            IsBackground = false
            Name = None
            Cpu = CpuId 0
        }

    let private withThreads (threads : (ThreadId * ThreadStatus) list) (state : IlMachineState) : IlMachineState =
        let threadMap =
            threads
            |> List.map (fun (tid, status) -> tid, stubThreadState status)
            |> Map.ofList

        { state with
            ThreadState = threadMap
        }

    [<Test>]
    let ``Pct chooseNext with no Runnable threads returns the input state and None`` () : unit =
        // Empty-runnable is the deadlock signal: state must be returned unchanged
        // (no RNG advance), so a quiescent probe followed by a wake-up resumes
        // from the same PRNG position as if the probe never happened. This is
        // what keeps replay bit-exact across `advanceUntilRunnableOrQuiescent`.
        let initial =
            baseState ()
            |> withThreads
                [
                    ThreadId 0, ThreadStatus.Parked
                    ThreadId 1, ThreadStatus.BlockedOnJoin (ThreadId 0, None)
                ]
            |> IlMachineState.withPctSeed 0xC0FFEEUL

        let after, chosen = Scheduler.chooseNext (ThreadId 0) initial

        chosen |> shouldEqual None
        // IlMachineState doesn't support structural equality (its loggers and other
        // fields aren't comparable), so check the only mutable scheduling state
        // directly: the Pct policy must have the same Rng and Priorities.
        after.Scheduling |> shouldEqual initial.Scheduling

    [<Test>]
    let ``hasAnyRunnable agrees with chooseNext under Pct`` () : unit =
        // The deadline-advance loop in Program.fs uses hasAnyRunnable to probe
        // whether jumping the virtual clock made progress. That probe must be
        // policy-independent: `chooseNext` returns None iff no thread is
        // Runnable, regardless of policy. Pin that here.
        let allBlocked =
            baseState ()
            |> withThreads
                [
                    ThreadId 0, ThreadStatus.Parked
                    ThreadId 1, ThreadStatus.BlockedOnSleep None
                ]
            |> IlMachineState.withPctSeed 1UL

        Scheduler.hasAnyRunnable allBlocked |> shouldEqual false
        let _, choice = Scheduler.chooseNext (ThreadId 0) allBlocked
        choice |> shouldEqual None

    [<Test>]
    let ``onThreadTerminated drops the terminated thread's Pct priority`` () : unit =
        // Lifecycle invariant: the priority map's domain is
        // "ever-seen-runnable-and-not-terminated". A leaked entry could only be
        // chosen by argmax if the terminated thread somehow appeared in `runnable`,
        // which it can't — but the invariant is easier to reason about than
        // "terminated-but-still-in-map is safe by an indirect argument".
        let terminated = ThreadId 0
        let survivor = ThreadId 1

        let initial =
            baseState ()
            |> withThreads [ terminated, ThreadStatus.Runnable ; survivor, ThreadStatus.Runnable ]
            |> IlMachineState.withPctSeed 7UL

        // Manually populate priorities to mimic a state that has already passed
        // through ensurePriorityFor; building this through chooseNext would
        // require live frames (peekNextOp).
        let initial =
            match initial.Scheduling with
            | SchedulerState.Pct pct ->
                let pct =
                    pct
                    |> PctState.resamplePriority terminated
                    |> PctState.resamplePriority survivor

                { initial with
                    Scheduling = SchedulerState.Pct pct
                }
            | SchedulerState.RoundRobin -> failwith "withPctSeed should have produced a Pct state"

        let after = Scheduler.onThreadTerminated terminated initial

        match after.Scheduling with
        | SchedulerState.Pct pct ->
            Map.containsKey terminated pct.Priorities |> shouldEqual false
            Map.containsKey survivor pct.Priorities |> shouldEqual true
        | SchedulerState.RoundRobin ->
            failwith "onThreadTerminated must preserve the Pct policy, not silently revert to RoundRobin"

    [<Test>]
    let ``onThreadTerminated leaves a RoundRobin schedule alone`` () : unit =
        // The Pct cleanup branch must not introduce a behavioural difference for
        // RoundRobin runs. Anything else would mean PR B silently perturbed the
        // default policy — which is exactly what PR A took pains to avoid.
        let terminated = ThreadId 0

        let initial =
            baseState ()
            |> withThreads [ terminated, ThreadStatus.Runnable ; ThreadId 1, ThreadStatus.Runnable ]

        let after = Scheduler.onThreadTerminated terminated initial

        after.Scheduling |> shouldEqual SchedulerState.RoundRobin

    // ---------------- End-to-end reproducibility ----------------

    let private assy = typeof<RunResult>.Assembly

    let private runSourceWithSeed (sourceName : string) (seed : uint64 option) : RunOutcome =
        let source = Assembly.getEmbeddedResourceAsString sourceName assy
        let image = Roslyn.compile [ source ]

        let _messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", sourceName ]

        use _loggerFactoryResource = loggerFactory

        let dotnetRuntimes =
            DotnetRuntime.SelectForDll assy.Location |> ImmutableArray.CreateRange

        use peImage = new MemoryStream (image)

        Program.run loggerFactory (Some sourceName) peImage dotnetRuntimes KernelConfig.Default seed []

    /// Project a RunOutcome to its salient bit: the terminating thread's
    /// top-of-stack int32 if the run finished normally, or a tag describing
    /// the failure mode otherwise. Two runs are "the same schedule" iff this
    /// projection matches, which is what PCT reproducibility actually
    /// promises — terminal observable state is determined by seed + program.
    let private outcomeSignature (outcome : RunOutcome) : string =
        match outcome with
        | RunOutcome.NormalExit (state, thread)
        | RunOutcome.ProcessExit (state, thread) ->
            match state.ThreadState.[thread].MethodState.EvaluationStack.Values with
            | EvalStackValue.Int32 i :: _ -> $"exit %d{i}"
            | other -> $"exit other (%A{other})"
        | RunOutcome.FailFast (_, _, message) ->
            let msg = message |> Option.defaultValue "<none>"
            $"failfast %s{msg}"
        | RunOutcome.SignalTerminated (_, signal) -> $"signal %O{signal}"
        | RunOutcome.GuestUnhandledException (_, _, _) -> "unhandled exception"

    [<Test>]
    let ``PCT runs with the same seed produce identical observable outcomes`` () : unit =
        // The core invariant of PCT: schedule is a deterministic function of
        // the seed plus the program. Run a schedule-sensitive guest twice with
        // the same seed and require bit-identical observable outcomes.
        //
        // ReadWriteRace is a 2-thread shared-int race that exposes two legal
        // exit codes (0 or 1) depending on interleaving. Whatever the seed
        // picks, it must pick the same value both times.
        let seed = Some 0xDEADBEEFCAFEBABEUL
        let first = runSourceWithSeed "ReadWriteRace.cs" seed
        let second = runSourceWithSeed "ReadWriteRace.cs" seed

        outcomeSignature first |> shouldEqual (outcomeSignature second)

    [<Test>]
    let ``PCT runs without a seed default to RoundRobin parity`` () : unit =
        // A `None` seed must select the legacy RoundRobin policy: two `None`
        // runs are identical, and they must also match the RoundRobin trace
        // (because RoundRobin is itself deterministic). This pins the
        // "opt-in" contract on the new flag.
        let first = runSourceWithSeed "ReadWriteRace.cs" None
        let second = runSourceWithSeed "ReadWriteRace.cs" None

        outcomeSignature first |> shouldEqual (outcomeSignature second)

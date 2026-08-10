namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open Microsoft.FSharp.Reflection
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// Pins the yield-debt fairness filter: an honoured `Thread.Yield()` / `Thread.Sleep(0)`
/// sends the caller to the back of the run queue, and the scheduler holds it out of the
/// candidate set until the threads that were Runnable alongside it have each taken a step.
///
/// The two defects this fixture exists to prevent are both liveness bugs that a
/// weaker-but-plausible design walks straight into, so they are pinned as properties rather
/// than as examples:
///
///   * *Exclusion bounded by other threads **yielding** rather than **running**.* A rule that
///     re-admits a yielder only once every runnable thread has also yielded lets one
///     non-yielding busy-waiter exclude it forever: `Thread.Yield(); f = true;` racing
///     `while (!f) {}` livelocks, where today it terminates. `bounded exclusion under
///     RoundRobin` is the property that catches this — note its generators deliberately
///     include threads that never yield.
///   * *Debt surviving a park/wake cycle.* A yielder that blocks and later wakes must not
///     still be held out by peers that have since run or blocked themselves. Rather than
///     clear debt in every wake path, membership is filtered against the live Runnable set at
///     read time, so a member that stops being Runnable stops counting.
///
/// `chooseNext`'s `Pct` branch calls `ThreadState.peekNextOp`, which needs a live frame, so
/// the properties here drive `RoundRobin` (whose choice rule is total over frameless stubs)
/// and cover `Pct` through `onStepOutcome`, which touches no frames. That split mirrors
/// `TestSchedulerPct`.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestSchedulerYieldDebt =

    let private corelib : DumpedAssembly =
        let corelibPath = typeof<obj>.Assembly.Location
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.readFile loggerFactory corelibPath

    let private baseState () : IlMachineState =
        let _, loggerFactory = LoggerFactory.makeTest ()
        IlMachineState.initial loggerFactory ImmutableArray.Empty corelib

    /// Frame-less stub thread state. See `TestSchedulerVoluntaryYield.stubThreadState`: the
    /// paths under test here read `Status` and `YieldDebt` only, so a sentinel FrameId that
    /// would crash loudly on dereference is the right stub.
    let private stubThreadState (status : ThreadStatus) : ThreadState =
        {
            MethodStates = Map.empty
            YieldDebt = Set.empty
            NextFrameId = 0
            ActiveMethodState = FrameId -1
            Status = status
            IsBackground = false
            Name = None
            Cpu = CpuId 0
            OsThreadId = OsThreadId 1u
        }

    let private withThreads (threads : (ThreadId * ThreadStatus) list) (state : IlMachineState) : IlMachineState =
        { state with
            ThreadState =
                threads
                |> List.map (fun (tid, status) -> tid, stubThreadState status)
                |> Map.ofList
        }

    let private debtOf (tid : ThreadId) (state : IlMachineState) : Set<ThreadId> = state.ThreadState.[tid].YieldDebt

    let private runnable (n : int) : (ThreadId * ThreadStatus) list =
        List.init n (fun i -> ThreadId i, ThreadStatus.Runnable)

    /// Mirror of what the driver does when a thread retires a step: discharge every yield debt
    /// naming the runner, then apply the consequences specific to the outcome. The two halves
    /// live in different places on purpose — the discharge is applied once at
    /// `Program.stepPrepared`'s single `executeOneStep` seam so that it cannot be forgotten for
    /// an outcome, while `onStepOutcome` sees only the `Stepped` family — so a test that wants
    /// to model "a step happened" has to compose them.
    ///
    /// Note what this helper cannot catch: it re-implements the driver, so it stays green if
    /// the driver itself stops discharging. `TestSchedulerYieldFairness` closes that gap by
    /// asserting the invariant against a real run.
    let private retireStep (ran : ThreadId) (outcome : WhatWeDid) (state : IlMachineState) : IlMachineState =
        state
        |> Scheduler.dischargeYieldDebts ran
        |> Scheduler.onStepOutcome ran outcome

    // ---------------- Charging ----------------

    [<Test>]
    let ``an honoured yield charges a debt naming every other Runnable thread`` () : unit =
        // Blocked threads are not in the run queue, so the yielder owes them nothing — it is
        // not waiting for them, and waiting would be a liveness bug if they never wake.
        let state =
            baseState ()
            |> withThreads
                [
                    ThreadId 0, ThreadStatus.Runnable
                    ThreadId 1, ThreadStatus.Runnable
                    ThreadId 2, ThreadStatus.BlockedOnJoin (ThreadId 0, None)
                ]

        let after = retireStep (ThreadId 0) (WhatWeDid.VoluntaryYield false) state

        debtOf (ThreadId 0) after |> shouldEqual (Set.ofList [ ThreadId 1 ])
        debtOf (ThreadId 1) after |> shouldEqual Set.empty
        debtOf (ThreadId 2) after |> shouldEqual Set.empty

    [<Test>]
    let ``yielding with no other Runnable thread charges nothing`` () : unit =
        // There is nobody to go to the back of the queue behind. This is the case that makes
        // `Thread.Yield()` honestly return FALSE, and it must not produce an empty-but-present
        // debt that `candidates` would then have to reason about.
        let state =
            baseState ()
            |> withThreads
                [
                    ThreadId 0, ThreadStatus.Runnable
                    ThreadId 1, ThreadStatus.BlockedOnSleep (Some 100L)
                ]

        let after = retireStep (ThreadId 0) (WhatWeDid.VoluntaryYield false) state

        debtOf (ThreadId 0) after |> shouldEqual Set.empty

    // ---------------- Filtering and discharge ----------------

    [<Test>]
    let ``a thread with outstanding debt is not chosen`` () : unit =
        let state = baseState () |> withThreads (runnable 3)

        let state = retireStep (ThreadId 0) (WhatWeDid.VoluntaryYield false) state

        // Round-robin from `lastRan = 2` would wrap to thread 0; the debt must override that.
        Scheduler.chooseNext (ThreadId 2) state
        |> snd
        |> shouldEqual (Some (ThreadId 1))

    [<Test>]
    let ``debt is discharged by its members running, re-admitting the yielder`` () : unit =
        let state = baseState () |> withThreads (runnable 3)

        let state = retireStep (ThreadId 0) (WhatWeDid.VoluntaryYield false) state

        debtOf (ThreadId 0) state
        |> shouldEqual (Set.ofList [ ThreadId 1 ; ThreadId 2 ])

        let state = retireStep (ThreadId 1) WhatWeDid.Executed state
        debtOf (ThreadId 0) state |> shouldEqual (Set.ofList [ ThreadId 2 ])
        // Still excluded: thread 2 has not had its turn.
        Scheduler.chooseNext (ThreadId 2) state
        |> snd
        |> shouldEqual (Some (ThreadId 1))

        let state = retireStep (ThreadId 2) WhatWeDid.Executed state
        debtOf (ThreadId 0) state |> shouldEqual Set.empty

        Scheduler.chooseNext (ThreadId 2) state
        |> snd
        |> shouldEqual (Some (ThreadId 0))

    [<Test>]
    let ``a debt member that stops being Runnable stops counting`` () : unit =
        // The park/wake defect, pinned directly: thread 1 blocks without ever taking another
        // step, so it can never discharge the debt by running. It must stop holding thread 0
        // out anyway, or thread 0 waits on a thread that will never satisfy it.
        let state = baseState () |> withThreads (runnable 2)

        let state = retireStep (ThreadId 0) (WhatWeDid.VoluntaryYield false) state

        debtOf (ThreadId 0) state |> shouldEqual (Set.ofList [ ThreadId 1 ])

        let state = Scheduler.blockOnJoin (ThreadId 1) (ThreadId 0) None state

        // The debt still names thread 1 — we do not rewrite it — but the filter ignores it.
        debtOf (ThreadId 0) state |> shouldEqual (Set.ofList [ ThreadId 1 ])

        Scheduler.chooseNext (ThreadId 0) state
        |> snd
        |> shouldEqual (Some (ThreadId 0))

    [<Test>]
    let ``a yielder that is the only Runnable thread is still chosen`` () : unit =
        // Degenerate case of the never-empty invariant: `candidates` must not starve the
        // machine when the sole runnable thread has just yielded.
        let state = baseState () |> withThreads (runnable 2)

        let state = retireStep (ThreadId 0) (WhatWeDid.VoluntaryYield false) state

        let state = Scheduler.blockOnJoin (ThreadId 1) (ThreadId 0) None state

        Scheduler.chooseNext (ThreadId 0) state
        |> snd
        |> shouldEqual (Some (ThreadId 0))

    [<Test>]
    let ``lockstep yielders alternate rather than deadlocking`` () : unit =
        // Two threads that do nothing but yield at each other. Under one-core `sched_yield`
        // this is strict alternation; under a rule that waited for mutual yields it would be
        // fine too, which is why this case alone is not enough — see the property below.
        let mutable state = baseState () |> withThreads (runnable 2)
        let mutable lastRan = ThreadId 1
        let chosen = ResizeArray ()

        for _ in 1..6 do
            let s, choice = Scheduler.chooseNext lastRan state
            let choice = Option.get choice
            chosen.Add choice
            lastRan <- choice
            state <- retireStep choice (WhatWeDid.VoluntaryYield false) s

        chosen
        |> List.ofSeq
        |> shouldEqual [ ThreadId 0 ; ThreadId 1 ; ThreadId 0 ; ThreadId 1 ; ThreadId 0 ; ThreadId 1 ]

    // ---------------- Properties ----------------

    /// One thread's behaviour in a generated schedule. `NeverYields` is the load-bearing case:
    /// the livelock the epoch design suffered needs a peer that only ever executes.
    type private Behaviour =
        | AlwaysYields
        | NeverYields
        | YieldsThenExecutes

    let private behaviourGen : Gen<Behaviour> =
        Gen.elements
            [
                Behaviour.AlwaysYields
                Behaviour.NeverYields
                Behaviour.YieldsThenExecutes
            ]

    let private scheduleGen : Gen<Behaviour list> =
        gen {
            let! n = Gen.choose (1, 5)
            return! Gen.listOfLength n behaviourGen
        }

    [<Test>]
    let ``bounded exclusion under RoundRobin`` () : unit =
        // The property the epoch design fails. Every thread stays Runnable throughout, so any
        // thread that goes unchosen for more than `n` consecutive decisions is being starved
        // by the filter itself rather than by the policy.
        //
        // `n` (not `n + 1`) is the right bound: with every thread Runnable, round-robin's
        // sweep visits each candidate once, and a debt charged during that sweep names only
        // threads that are themselves in it.
        let config = Config.QuickThrowOnFailure

        let property (behaviours : Behaviour list) : unit =
            let n = behaviours.Length
            let mutable state = baseState () |> withThreads (runnable n)
            let mutable lastRan = ThreadId (n - 1)
            let lastChosenAt = System.Collections.Generic.Dictionary<ThreadId, int> ()

            for i in 0 .. n - 1 do
                lastChosenAt.[ThreadId i] <- -1

            for step in 0 .. (6 * n) - 1 do
                let s, choice = Scheduler.chooseNext lastRan state

                match choice with
                | None -> failwith "every thread is Runnable, so a choice must exist"
                | Some choice ->

                lastChosenAt.[choice] <- step
                lastRan <- choice

                let outcome =
                    match behaviours.[let (ThreadId i) = choice in i] with
                    | Behaviour.AlwaysYields -> WhatWeDid.VoluntaryYield false
                    | Behaviour.NeverYields -> WhatWeDid.Executed
                    | Behaviour.YieldsThenExecutes ->
                        if step % 2 = 0 then
                            WhatWeDid.VoluntaryYield false
                        else
                            WhatWeDid.Executed

                state <- retireStep choice outcome s

                // Everyone that has already had a turn must have had one within the last `n`
                // decisions. Threads not yet chosen at all are covered by the end-state check.
                for KeyValue (tid, at) in lastChosenAt do
                    if at >= 0 && step - at > n then
                        failwith
                            $"thread %O{tid} was continuously Runnable but went unchosen for %d{step - at} decisions (bound %d{n}); behaviours %A{behaviours}"

            // And nobody was excluded for the whole run.
            for KeyValue (tid, at) in lastChosenAt do
                if at < 0 then
                    failwith $"thread %O{tid} was continuously Runnable but never chosen; behaviours %A{behaviours}"

        Check.One (config, Prop.forAll (Arb.fromGen scheduleGen) property)

    [<Test>]
    let ``chooseNext returns a choice whenever any thread is Runnable`` () : unit =
        // `candidates` promises never to empty a non-empty Runnable set; if it could, the
        // driver would report a spurious deadlock. Threads block and wake during the run so
        // that debts routinely name threads that are no longer Runnable.
        let config = Config.QuickThrowOnFailure

        let property (behaviours : Behaviour list) : unit =
            let n = behaviours.Length
            let mutable state = baseState () |> withThreads (runnable n)
            let mutable lastRan = ThreadId (n - 1)

            for step in 0 .. (6 * n) - 1 do
                // Park and wake a thread on alternating rounds to churn the Runnable set.
                if n > 1 && step % 3 = 0 then
                    let victim = ThreadId (step % n)

                    state <-
                        if
                            state.ThreadState.[victim].Status = ThreadStatus.Runnable
                            && Scheduler.hasAnyRunnable (Scheduler.blockOnSleep victim (Some 1L) state)
                        then
                            Scheduler.blockOnSleep victim (Some 1L) state
                        else
                            state

                if step % 5 = 0 then
                    let sleeper = ThreadId (step % n)

                    state <-
                        match state.ThreadState.[sleeper].Status with
                        | ThreadStatus.BlockedOnSleep (Some _) -> Scheduler.fireSleepTimeout sleeper state
                        | _ -> state

                let s, choice = Scheduler.chooseNext lastRan state

                Scheduler.hasAnyRunnable s |> shouldEqual choice.IsSome

                match choice with
                | None -> ()
                | Some choice ->
                    lastRan <- choice

                    let outcome =
                        match behaviours.[let (ThreadId i) = choice in i] with
                        | Behaviour.NeverYields -> WhatWeDid.Executed
                        | _ -> WhatWeDid.VoluntaryYield false

                    state <- retireStep choice outcome s

        Check.One (config, Prop.forAll (Arb.fromGen scheduleGen) property)

    // ---------------- Pct: the honour coin ----------------

    /// Seeds chosen by enumeration so that the first `nextDouble` lands either side of
    /// `P_HONOUR_YIELD`. Pinning concrete seeds rather than sampling keeps this a
    /// deterministic test of a deterministic function — the coin is a pure function of the
    /// splitmix64 state — and gives a two-sided assertion: a regression to *always* honouring
    /// fails the decline case, and one to *never* honouring fails the honour case.
    let private seedWhere (wantHonour : bool) : uint64 =
        let rec go (seed : uint64) : uint64 =
            if seed > 10000UL then
                failwith $"no seed below 10000 produces honour=%b{wantHonour}"
            else

            let sample, _ = NonCryptoRandom.nextDouble seed

            if (sample < 0.9) = wantHonour then
                seed
            else
                go (seed + 1UL)

        go 0UL

    let private pctYield (seed : uint64) : Set<ThreadId> =
        let state =
            baseState () |> withThreads (runnable 2) |> IlMachineState.withPctSeed seed

        retireStep (ThreadId 0) (WhatWeDid.VoluntaryYield false) state
        |> debtOf (ThreadId 0)

    [<Test>]
    let ``Pct honours a yield on one seed and declines on another`` () : unit =
        pctYield (seedWhere true) |> shouldEqual (Set.ofList [ ThreadId 1 ])

        // The decline is the point: schedules in which a yield is *not* respected are real
        // executions — it is why `Thread.Yield()` returns a bool — so the exploration policy
        // must be able to produce them. Without this half, a regression to honouring
        // unconditionally would pass every other test in this fixture.
        pctYield (seedWhere false) |> shouldEqual Set.empty

    [<Test>]
    let ``RoundRobin honours every yield and consumes no randomness`` () : unit =
        // RoundRobin is the reproducible baseline and is documented as drawing no random
        // numbers at all, so it takes no coin: it always honours. The asymmetry with Pct is
        // deliberate, and this pins it against a well-meaning refactor that unified the two
        // through a shared sampling path with p = 1.0.
        let state = baseState () |> withThreads (runnable 2)

        let after = retireStep (ThreadId 0) (WhatWeDid.VoluntaryYield false) state

        debtOf (ThreadId 0) after |> shouldEqual (Set.ofList [ ThreadId 1 ])
        after.Scheduling |> shouldEqual SchedulerState.RoundRobin

    [<Test>]
    let ``Pct draws on a yield iff a peer is Runnable`` () : unit =
        // This test used to assert the opposite: that a yield burns exactly one draw
        // *regardless* of the Runnable set, matching the always-burn Bernoulli in
        // `chooseNext`. The stated benefit was that the seed is consumed "at a rate that
        // depends only on the sequence of yields", so a replay could not diverge because a
        // thread happened to be blocked at one of them.
        //
        // That invariant was never actually true — consumption is also one draw per
        // newly-seen Runnable thread (`PctState.ensurePriorityFor`) plus one per demotion,
        // and the demotion count depends on the `ContextSwitchPrior` weights of the ops
        // encountered. So the old assertion pinned a rate that nothing observable depended on
        // and that the rest of the policy did not honour anyway.
        //
        // What is pinned instead is the invariant the schedule-sharing work needs: the policy
        // state changes only where a draw could change something. A yield with no other
        // Runnable thread is forced to "no switch" by the empty-`others` branch of
        // `chargeYieldDebt` whatever the coin says, so the coin is not tossed.
        //
        // Both halves are asserted. Dropping the peer half would let a regression that never
        // draws at all pass.
        let rngAfter (threads : (ThreadId * ThreadStatus) list) : uint64 =
            let state =
                baseState () |> withThreads threads |> IlMachineState.withPctSeed 12345UL

            let after = retireStep (ThreadId 0) (WhatWeDid.VoluntaryYield false) state

            match after.Scheduling with
            | SchedulerState.Pct pct -> pct.Rng
            | other -> failwith $"expected Pct scheduling, got %O{other}"

        let withPeer = rngAfter (runnable 2)

        let alone =
            rngAfter
                [
                    ThreadId 0, ThreadStatus.Runnable
                    ThreadId 1, ThreadStatus.BlockedOnSleep (Some 5L)
                ]

        // Alone: nothing drawn, so the Rng is exactly where `ofSeed` left it.
        alone |> shouldEqual 12345UL

        // With a peer: exactly one draw, the honour coin.
        let _, expected = NonCryptoRandom.nextDouble 12345UL
        withPeer |> shouldEqual expected
        withPeer |> shouldNotEqual alone

    [<Test>]
    let ``a Pct run with no yields consumes no RNG`` () : unit =
        // The fairness filter draws nothing of its own: only an honoured yield consults the
        // policy's coin. So a guest that never yields consumes the seed at exactly the rate
        // `chooseNext` alone would, and its schedule is a function of the seed and the guest,
        // not of whether the filter exists.
        let state =
            baseState () |> withThreads (runnable 3) |> IlMachineState.withPctSeed 99UL

        let after =
            (state, [ ThreadId 0 ; ThreadId 1 ; ThreadId 2 ; ThreadId 0 ])
            ||> List.fold (fun s tid -> Scheduler.onStepOutcome tid WhatWeDid.Executed s)

        after.Scheduling |> shouldEqual (SchedulerState.Pct (PctState.ofSeed 99UL))

    [<Test>]
    let ``a terminating thread is discharged from every outstanding debt`` () : unit =
        // A thread's final step is its bottom-frame `Ret`, which reaches the driver as
        // `ExecutionResult.Terminated` rather than `Stepped` — so the one step that most
        // conclusively satisfies "I am waiting to see you run" is the one that is easiest to
        // forget. It is discharged at the seam, like every other retired step, so the sequence
        // modelled here is "the step was retired, *and* it happened to be a termination".
        //
        // Correctness does not depend on it (a Terminated thread is never in the Runnable set,
        // so `candidates` ignores it either way, and the two `chooseNext` assertions below pass
        // regardless). Cost does: a debt keeping a permanently-unrunnable member never goes
        // empty, so its owner misses the `IsEmpty` fast path in `debtDischarged` for the rest of
        // the run and scans the runnable list on every scheduling decision. Assert on the debt
        // itself, not just on the choice, or the regression is invisible.
        let state = baseState () |> withThreads (runnable 3)

        let state = retireStep (ThreadId 0) (WhatWeDid.VoluntaryYield false) state

        debtOf (ThreadId 0) state
        |> shouldEqual (Set.ofList [ ThreadId 1 ; ThreadId 2 ])

        let state =
            state
            |> Scheduler.dischargeYieldDebts (ThreadId 1)
            |> Scheduler.onThreadTerminated (ThreadId 1)

        debtOf (ThreadId 0) state |> shouldEqual (Set.ofList [ ThreadId 2 ])

        // Thread 2 still owes a step, so thread 0 stays excluded; once it runs, the debt is
        // empty rather than merely ignorable.
        Scheduler.chooseNext (ThreadId 2) state
        |> snd
        |> shouldEqual (Some (ThreadId 2))

        let state = retireStep (ThreadId 2) WhatWeDid.Executed state
        debtOf (ThreadId 0) state |> shouldEqual Set.empty

    [<Test>]
    let ``mapState reaches the state of every ExecutionResult variant`` () : unit =
        // The seam's totality is the enforcement mechanism: `Program.stepPrepared` discharges by
        // mapping over whatever `executeOneStep` returned, so an outcome whose state `mapState`
        // failed to touch would silently skip the per-step bookkeeping. The compiler catches a
        // *missing* variant; this catches one that is present but wired to the wrong field, and
        // pins that no variant is deliberately exempted later.
        let marked = baseState () |> withThreads (runnable 1)

        let mark (_ : IlMachineState) : IlMachineState = marked

        let sentinel = baseState ()
        let thread = ThreadId 0

        let guestException : CliException<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> =
            {
                ExceptionObject = ManagedHeapAddress 1
                StackTrace = []
            }

        let variants : ExecutionResult list =
            [
                ExecutionResult.Terminated (sentinel, thread)
                ExecutionResult.ProcessExit (sentinel, thread)
                ExecutionResult.FailFast (sentinel, thread, Some "m")
                ExecutionResult.SignalTerminated (sentinel, Signal.SIGINT)
                ExecutionResult.Stepped (sentinel, WhatWeDid.Executed, StepEffect.NoEffect)
                ExecutionResult.UnhandledException (sentinel, thread, guestException)
            ]

        // The table above is hand-written, so it can fall behind the type — which is the exact
        // failure this test would then hide, since a variant that is never constructed is never
        // checked no matter how thoroughly the assertions below are written. Tie the two
        // together: adding a variant to `ExecutionResult` fails here until it is listed.
        FSharpType.GetUnionCases typeof<ExecutionResult>
        |> Array.length
        |> shouldEqual variants.Length

        for variant in variants do
            let mapped = ExecutionResult.mapState mark variant

            let state =
                match mapped with
                | ExecutionResult.Terminated (s, _)
                | ExecutionResult.ProcessExit (s, _)
                | ExecutionResult.FailFast (s, _, _)
                | ExecutionResult.SignalTerminated (s, _)
                | ExecutionResult.Stepped (s, _, _)
                | ExecutionResult.UnhandledException (s, _, _) -> s

            // Reference equality would be ideal but `IlMachineState` is a large record; the
            // thread map is enough to tell the marked state from the sentinel.
            state.ThreadState.Count |> shouldEqual marked.ThreadState.Count
            state.ThreadState.Count |> shouldNotEqual sentinel.ThreadState.Count

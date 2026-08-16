namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint

/// End-to-end acceptance for issue #844 itself. The guest's spinners use the BCL's `SpinWait`,
/// which after twenty iterations calls `Thread.Sleep(1)` forever — so this is the sleep half of
/// the backoff, where `TestSchedulerYieldFairness` covers the yield half.
///
/// Two things are asserted, and the first matters more than the numbers: that a thread which
/// has called `Thread.Sleep(1)` is *observably* parked. That fails whenever the sleep deadline
/// falls inside the same tick the sleep was requested in, because `fireExpiredDeadlines` then
/// wakes the thread before the scheduler can ever see it — a state in which `Sleep` costs its
/// caller no scheduling decisions at all and the BCL's backoff does nothing. The fixture
/// watches for a parked thread directly rather than inferring it from a step count, which
/// could improve for unrelated reasons.
///
/// Runs under `Pct`, as `TestSchedulerYieldFairness` does and for the same reason: `RoundRobin`
/// is already maximally yield-respecting, so it is not where scheduling fixes show up.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestSchedulerSleepFairness =

    /// Anchor for locating this test assembly, whose embedded resources carry the guest source.
    type private Marker = class end

    let private testAssy = typeof<Marker>.Assembly

    let private dotnetRuntimes : ImmutableArray<string> =
        DotnetRuntime.SelectForDll testAssy.Location |> ImmutableArray.CreateRange

    /// Compiled once and reused across seeds: recompiling per seed would dominate the run
    /// without changing semantics.
    let private image : byte[] =
        Assembly.getEmbeddedResourceAsString "SpinWaitSpinnersDoNotStarveWorker.cs" testAssy
        |> List.singleton
        |> Roslyn.compile

    /// What one run of the guest tells us. The assertions rest on `ParkedTicks`: the count of
    /// driver steps at which *some* thread was observably in `BlockedOnSleep`. It is zero for
    /// any rate at which a `Sleep(1)` deadline expires before the next scheduling decision,
    /// however many times the guest sleeps.
    type private RunMeasurement =
        {
            ExitCode : int
            Steps : int64
            ParkedTicks : int64
            TotalTicks : int64
        }

    /// Run the guest to completion under one PCT seed, returning (exit code, total steps).
    /// Drives `Program.stepPrepared` directly so the step counter is observable; `Program.run`
    /// would hide it.
    let private runOne (seed : uint64 option) : RunMeasurement =
        let _messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "pct_seed", sprintf "%A" seed ]

        use _loggerFactoryResource = loggerFactory
        use peImage = new MemoryStream (image)
        let logger = loggerFactory.CreateLogger "TestSchedulerSleepFairness"

        match
            Program.prepare
                loggerFactory
                (Some "SpinWaitSpinnersDoNotStarveWorker.cs")
                peImage
                { HostConfig.Default dotnetRuntimes with
                    PctSeed = seed
                }
        with
        | Program.ProgramStartResult.CompletedBeforeMain _ ->
            failwith "guest terminated before Main; it has no static initialisers that could do that"
        | Program.ProgramStartResult.Ready prepared ->
            let mutable parkedTicks = 0L
            let mutable totalTicks = 0L

            let rec loop (prepared : Program.PreparedProgram) : RunMeasurement =
                match Program.stepPrepared loggerFactory logger prepared with
                | Program.ProgramStepOutcome.Completed (RunOutcome.NormalExit (state, thread)) ->
                    let code =
                        match state.ThreadState.[thread].MethodState.EvaluationStack.Values with
                        | EvalStackValue.Int32 (Int32Source.Verbatim code) :: _ -> code
                        | other -> failwith $"guest Main did not return an int32: %A{other}"

                    {
                        ExitCode = code
                        Steps = state.Kernel.StepCounter
                        ParkedTicks = parkedTicks
                        TotalTicks = totalTicks
                    }
                | Program.ProgramStepOutcome.Completed other -> failwith $"unexpected guest outcome: %A{other}"
                | Program.ProgramStepOutcome.Deadlocked (_, stuck) -> failwith $"guest deadlocked: %s{stuck}"
                | Program.ProgramStepOutcome.InstructionStepped (p, _, _, _)
                | Program.ProgramStepOutcome.WorkerTerminated (p, _) ->
                    totalTicks <- totalTicks + 1L

                    let anyParked =
                        p.State.ThreadState
                        |> Map.exists (fun _ ts ->
                            match ts.Status with
                            | ThreadStatus.BlockedOnSleep _ -> true
                            | _ -> false
                        )

                    if anyParked then
                        parkedTicks <- parkedTicks + 1L

                    loop p

            loop prepared

    [<Test>]
    let ``a SpinWait spinner is observably asleep`` () : unit =
        // Under `RoundRobin`, so that reaching the steady state is guaranteed rather than lucky:
        // `SpinWait` yields for twenty iterations before it ever calls `Thread.Sleep(1)`, and a
        // fair rotation gets every spinner there. Under `Pct` it is a coin toss — measured, seed
        // 1 reaches the sleep rung and seed 2 does not, because PCT can starve a spinner for the
        // whole run. That is PCT working as intended, not a bug, so the *qualitative* claim
        // belongs here and the statistical one belongs in the PCT test below.
        let r = runOne None

        r.ExitCode |> shouldEqual 0

        if r.ParkedTicks = 0L then
            failwith
                $"the guest ran %d{r.TotalTicks} ticks with six SpinWait spinners in their Sleep(1) loops, and at no tick was any thread observably parked. Thread.Sleep is not costing the sleeper any scheduling decisions."

        // And not merely once: a sleeping thread should dominate the run.
        if r.ParkedTicks * 2L < r.TotalTicks then
            failwith
                $"only %d{r.ParkedTicks} of %d{r.TotalTicks} ticks had a thread parked in BlockedOnSleep; the spinners are awake for most of the run despite spending every iteration in Sleep(1)."

    [<Test>]
    let ``SpinWait spinners sleep across a PCT seed sweep`` () : unit =
        // Aggregated, because whether a given seed reaches the sleep rung at all is a property
        // of that schedule (see above). Summing over a fixed seed set keeps the test fully
        // deterministic while making a claim about the schedule space rather than one schedule.
        let seeds = [ 1UL .. 10UL ]
        let results = seeds |> List.map (fun seed -> seed, runOne (Some seed))

        for seed, r in results do
            if r.ExitCode <> 0 then
                failwith
                    $"guest returned %d{r.ExitCode} under PCT seed %d{seed}; it should compute 0+1+...+3999 = 7998000"

        let parked = results |> List.sumBy (fun (_, r) -> r.ParkedTicks)
        let total = results |> List.sumBy (fun (_, r) -> r.TotalTicks)

        if parked * 4L < total then
            let perSeed =
                results
                |> List.map (fun (seed, r) -> $"%d{seed}:%d{r.ParkedTicks}/%d{r.TotalTicks}")
                |> String.concat " "

            failwith
                $"only %d{parked} of %d{total} ticks across the sweep had a thread parked in BlockedOnSleep. Per seed: %s{perSeed}"

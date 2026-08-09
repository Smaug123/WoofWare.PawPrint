namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint

/// End-to-end acceptance for the yield-debt fairness filter: a guest in which several threads
/// spin on a flag, yielding every iteration, while one worker does bounded work and sets it.
///
/// The measurement is the total scheduler step count for the run. The worker's own work is
/// fixed, so the step count is a direct measure of how much of the machine the spinners took
/// while contributing nothing.
///
/// **This fixture runs under `Pct`, not `RoundRobin`, and that is the honest scope of the
/// change.** `RoundRobin` picks the lowest id strictly greater than `lastRan` and wraps, so it
/// already never re-picks the thread that just ran while another is Runnable — it is already
/// maximally yield-respecting, and the filter provably cannot improve it. (Measured: this guest
/// costs 22,900 steps under `RoundRobin` both with and without the filter, to the step.) `Pct`
/// is the policy that sticks to a high-priority thread until a demotion draw succeeds, so it is
/// the policy a yield has something to say to.
///
/// Related, and deliberately out of scope here: the `SpinWait`-based guest from issue #844
/// escalates to `Thread.Sleep(1)` after twenty iterations and spends the rest of its life
/// there. A `Sleep(1)` currently parks a thread for less than one scheduler decision, so no
/// amount of yield fairness helps it; that needs virtual time to cost something.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestSchedulerYieldFairness =

    /// Anchor for locating this test assembly, whose embedded resources carry the guest source.
    type private Marker = class end

    let private testAssy = typeof<Marker>.Assembly

    let private dotnetRuntimes : ImmutableArray<string> =
        DotnetRuntime.SelectForDll testAssy.Location |> ImmutableArray.CreateRange

    /// Compiled once and reused across seeds: recompiling per seed would dominate the run
    /// without changing semantics.
    let private image : byte[] =
        Assembly.getEmbeddedResourceAsString "YieldingSpinnersDoNotStarveWorker.cs" testAssy
        |> List.singleton
        |> Roslyn.compile

    /// Run the guest to completion under one PCT seed, returning (exit code, total steps).
    /// Drives `Program.stepPrepared` directly so the step counter is observable; `Program.run`
    /// would hide it.
    let private runOne (seed : uint64) : int * int64 =
        let _messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "pct_seed", string seed ]

        use _loggerFactoryResource = loggerFactory
        use peImage = new MemoryStream (image)
        let logger = loggerFactory.CreateLogger "TestSchedulerYieldFairness"

        match
            Program.prepare
                loggerFactory
                (Some "YieldingSpinnersDoNotStarveWorker.cs")
                peImage
                { HostConfig.Default dotnetRuntimes with
                    PctSeed = Some seed
                }
        with
        | Program.ProgramStartResult.CompletedBeforeMain _ ->
            failwith "guest terminated before Main; it has no static initialisers that could do that"
        | Program.ProgramStartResult.Ready prepared ->
            let rec loop (prepared : Program.PreparedProgram) : int * int64 =
                match Program.stepPrepared loggerFactory logger prepared with
                | Program.ProgramStepOutcome.Completed (RunOutcome.NormalExit (state, thread)) ->
                    let code =
                        match state.ThreadState.[thread].MethodState.EvaluationStack.Values with
                        | EvalStackValue.Int32 (Int32Source.Verbatim code) :: _ -> code
                        | other -> failwith $"guest Main did not return an int32: %A{other}"

                    code, state.Kernel.StepCounter
                | Program.ProgramStepOutcome.Completed other -> failwith $"unexpected guest outcome: %A{other}"
                | Program.ProgramStepOutcome.Deadlocked (_, stuck) -> failwith $"guest deadlocked: %s{stuck}"
                | Program.ProgramStepOutcome.InstructionStepped (p, _, _) -> loop p
                | Program.ProgramStepOutcome.WorkerTerminated (p, _) -> loop p

            loop prepared

    /// Six spinners plus the worker and the entry thread.
    ///
    /// Aggregated over a fixed seed set rather than asserted per seed, because the effect is
    /// statistical and the per-seed numbers are not monotone. `Pct`'s residency is a random
    /// walk over priorities, and charging a yield debt perturbs the RNG stream, so an
    /// individual seed can land on a *worse* interleaving than it did before — measured, seed 7
    /// goes 5,464 → 11,349 while seed 42 goes 16,593 → 4,421. Pinning any single seed would be
    /// pinning luck, and pinning the sum measures the thing the change is actually for.
    ///
    /// The seeds are fixed, so this is fully deterministic despite being a statistical claim:
    /// no flakiness, and a failure reproduces exactly.
    ///
    /// Measured totals over these twenty seeds: 601,789 steps without the yield-debt filter,
    /// 151,253 with it — a 4x reduction. The bound below sits between the two, so it is not a
    /// change-detector on exact interleavings: a scheduler tweak that shuffles individual seeds
    /// around is free to pass, and only losing the fairness effect itself fails.
    [<Test>]
    let ``yielding spinners do not starve the worker under Pct`` () : unit =
        let seeds = [ 1UL .. 20UL ]

        let results = seeds |> List.map (fun seed -> seed, runOne seed)

        for seed, (exitCode, _) in results do
            if exitCode <> 0 then
                failwith $"guest returned %d{exitCode} under PCT seed %d{seed}; it should compute 0+1+...+149 = 11175"

        let total = results |> List.sumBy (fun (_, (_, steps)) -> steps)

        if total > 300_000L then
            let perSeed =
                results
                |> List.map (fun (seed, (_, steps)) -> $"%d{seed}:%d{steps}")
                |> String.concat " "

            failwith
                $"guest took %d{total} scheduler steps in total across %d{seeds.Length} PCT seeds (budget 300000); the yielding spinners are consuming the machine despite having declared they have nothing to do. Per seed: %s{perSeed}"

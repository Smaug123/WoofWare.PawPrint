namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint

/// Liveness acceptance for `Pct`: a thread that never yields, never blocks, and executes
/// nothing but `br` must still be preemptable.
///
/// This is the failure the per-opcode context-switch weights could produce and that no other
/// fixture covers. The yield-fairness and sleep-fairness fixtures both measure how *much* of the
/// machine a spinner takes, which presupposes the run finishes; this one asserts that it
/// finishes at all. The distinction matters because the old failure was not a slow run, it was
/// a scheduler that had no draw capable of moving the machine off the hog.
///
/// Asserted against a step budget rather than by waiting: an unpreemptable hog makes the run
/// non-terminating, and a test that hangs reports nothing useful.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestSchedulerNonYieldingSpinner =

    /// Anchor for locating this test assembly, whose embedded resources carry the guest source.
    type private Marker = class end

    let private testAssy = typeof<Marker>.Assembly

    let private dotnetRuntimes : ImmutableArray<string> =
        DotnetRuntime.SelectForDll testAssy.Location |> ImmutableArray.CreateRange

    let private sourceName = "NonYieldingSpinnerDoesNotHangTheMachine.cs"

    /// Compiled once and reused across seeds.
    let private image : byte[] =
        Assembly.getEmbeddedResourceAsString sourceName testAssy
        |> List.singleton
        |> Roslyn.compile

    /// Run the guest under one PCT seed, abandoning it after `budget` scheduler steps.
    /// Returns `None` if the budget was exhausted, else the guest's exit code and step count.
    let private runOne (seed : uint64) (budget : int64) : (int * int64) option =
        let _messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "pct_seed", string seed ]

        use _loggerFactoryResource = loggerFactory
        use peImage = new MemoryStream (image)
        let logger = loggerFactory.CreateLogger "TestSchedulerNonYieldingSpinner"

        match
            Program.prepare
                loggerFactory
                (Some sourceName)
                peImage
                { HostConfig.Default dotnetRuntimes with
                    PctSeed = Some seed
                }
        with
        | Program.ProgramStartResult.CompletedBeforeMain _ ->
            failwith "guest terminated before Main; it has no static initialisers that could do that"
        | Program.ProgramStartResult.Ready prepared ->
            let rec loop (prepared : Program.PreparedProgram) (steps : int64) : (int * int64) option =
                if steps >= budget then
                    None
                else

                match Program.stepPrepared loggerFactory logger prepared with
                | Program.ProgramStepOutcome.Completed (RunOutcome.NormalExit (state, thread)) ->
                    let code =
                        match state.ThreadState.[thread].MethodState.EvaluationStack.Values with
                        | EvalStackValue.Int32 (Int32Source.Verbatim code) :: _ -> code
                        | other -> failwith $"guest Main did not return an int32: %A{other}"

                    Some (code, state.Kernel.StepCounter)
                | Program.ProgramStepOutcome.Completed other -> failwith $"unexpected guest outcome: %A{other}"
                | Program.ProgramStepOutcome.Deadlocked (_, stuck) -> failwith $"guest deadlocked: %s{stuck}"
                | Program.ProgramStepOutcome.InstructionStepped (p, _, _) -> loop p (steps + 1L)
                | Program.ProgramStepOutcome.WorkerTerminated (p, _) -> loop p (steps + 1L)

            loop prepared 0L

    /// The budget is deliberately far above the measured cost. It is not a performance
    /// assertion -- the fairness fixtures make those -- but a liveness one, and the failure it
    /// guards against is unbounded, not merely large. Setting it near the observed cost would
    /// turn every unrelated scheduling tweak into a failure here.
    ///
    /// Measured over these ten seeds: 5,654 to 8,528 steps, so the budget has about 117x
    /// headroom over the worst. That the gap is load-bearing rather than decorative was checked
    /// by mutation: with `P_BASE` set to 0.0, so that no draw can ever demote the hog, nine of
    /// the ten seeds fail to terminate within the budget. (The tenth, seed 7, is the case where
    /// the worker outranks the spinner on the initial priority draw and never has to be handed
    /// the machine back.)
    let private budget : int64 = 1_000_000L

    [<Test>]
    let ``a spinner executing only branches is still preemptable`` () : unit =
        let seeds = [ 1UL .. 10UL ]

        let results = seeds |> List.map (fun seed -> seed, runOne seed budget)

        let hung = results |> List.filter (fun (_, r) -> Option.isNone r) |> List.map fst

        if not hung.IsEmpty then
            failwith
                $"under PCT seed(s) %A{hung} the guest had not terminated after %d{budget} scheduler steps. A `while (true) {{ }}` thread executes nothing but `br`; if the policy cannot demote it, the worker never runs again and the run never ends."

        for seed, result in results do
            match result with
            | None -> ()
            | Some (exitCode, _) ->
                if exitCode <> 0 then
                    failwith
                        $"guest returned %d{exitCode} under PCT seed %d{seed}; it should compute 0+1+...+199 = 19900"

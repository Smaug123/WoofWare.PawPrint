namespace WoofWare.Pawprint.Test

open System.Collections.Immutable
open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint
open WoofWare.PawPrint.Test

[<TestFixture>]
module TestRaces =
    let private assy = typeof<RunResult>.Assembly

    /// Compile a guest source once. PCT sweeps reuse the resulting image
    /// across many seeds; recompiling per seed would dominate the test
    /// duration without changing semantics.
    let private compileImage (sourceName : string) : byte[] =
        let source = Assembly.getEmbeddedResourceAsString sourceName assy
        Roslyn.compile [ source ]

    let private dotnetRuntimes : ImmutableArray<string> =
        DotnetRuntime.SelectForDll assy.Location |> ImmutableArray.CreateRange

    /// Run `image` through PawPrint with the given scheduler seed and return
    /// the guest's exit code. Fails the test loudly on any non-normal exit:
    /// FailFast / signal / unhandled exception under a race test would
    /// indicate either a corrupted interpreter state or a guest assumption
    /// the test was not designed to handle, and silently folding them into
    /// the observed set would muddle the legality claim.
    let private runPawPrint (sourceName : string) (image : byte[]) (seed : uint64 option) : int =
        let _messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", sourceName ]

        use _loggerFactoryResource = loggerFactory
        use peImage = new MemoryStream (image)

        match Program.run loggerFactory (Some sourceName) peImage dotnetRuntimes Map.empty seed [] with
        | RunOutcome.NormalExit (terminalState, terminatingThread)
        | RunOutcome.ProcessExit (terminalState, terminatingThread) ->
            match terminalState.ThreadState.[terminatingThread].MethodState.EvaluationStack.Values with
            | EvalStackValue.Int32 exitCode :: _ -> exitCode
            | [] -> failwith $"%s{sourceName} (seed=%A{seed}) returned void; race tests expect Int32 exit codes"
            | ret :: _ ->
                failwith $"%s{sourceName} (seed=%A{seed}) returned %O{ret}; race tests expect an Int32 on the stack"
        | RunOutcome.FailFast (_, _, message) ->
            let m = message |> Option.defaultValue "<no message>"
            failwith $"%s{sourceName} (seed=%A{seed}) called Environment.FailFast: %s{m}"
        | RunOutcome.SignalTerminated (_, signal) ->
            failwith $"%s{sourceName} (seed=%A{seed}) was terminated by POSIX signal %O{signal}"
        | RunOutcome.GuestUnhandledException (_, _, exn) ->
            failwith $"%s{sourceName} (seed=%A{seed}) threw unhandled exception: %O{exn.ExceptionObject}"

    /// Run `image` through the real .NET runtime once. Fails the test on
    /// unhandled exception. The host's threading and memory ordering pin
    /// the schedule, so this is not a coverage probe — it's a sanity that
    /// the spec we hand-derived for the race is at least non-empty under
    /// a real CLR.
    let private runRealRuntime (sourceName : string) (image : byte[]) : int =
        match RealRuntime.executeWithRealRuntime [||] image with
        | RealRuntimeResult.NormalExit exitCode -> exitCode
        | RealRuntimeResult.UnhandledException exn ->
            failwith $"%s{sourceName} threw unhandled exception under the real runtime: %O{exn}"

    // Seed sweep used to characterise PCT coverage. The first 30 splitmix64
    // outputs already hit both interleavings of ReadWriteRace; running 64
    // gives a generous margin for future races whose rarer interleavings
    // might land deeper in the seed stream, while still completing in
    // about a second on the existing test machine.
    let private pctSeedSweep : uint64 list = [ 0UL .. 63UL ]

    /// The complete set of legal exit codes for ReadWriteRace.cs:
    ///   * 0 — Main reads `x` before the worker's `x = 1` runs (the
    ///     interleaving round-robin never reaches, since the worker thread
    ///     starts running immediately upon `Thread.Start` and the read in
    ///     Main is several IL ops downstream of `Start`).
    ///   * 1 — Main reads `x` after the worker has written. The PawPrint
    ///     default round-robin schedule produces this outcome.
    /// Any other value (negative, > 1, etc.) is an interpreter bug: the
    /// shared int has no other reachable value across the program's
    /// happens-before graph.
    let private readWriteRaceLegalOutcomes : Set<int> = Set.ofList [ 0 ; 1 ]

    [<Test>]
    let ``ReadWriteRace under the real runtime produces a legal outcome`` () : unit =
        // Sanity oracle: the spec for legal outcomes is a hand-derived
        // ECMA-335 claim and could be wrong. A real CLR run won't enumerate
        // all interleavings (host memory model and JIT bias the schedule),
        // but if a real run produces a value *outside* the spec, the spec
        // is wrong, not the runtime — and we'd want a loud failure here
        // before trusting it as the oracle for the PCT-coverage test.
        let image = compileImage "ReadWriteRace.cs"
        let exitCode = runRealRuntime "ReadWriteRace.cs" image
        readWriteRaceLegalOutcomes |> Set.contains exitCode |> shouldEqual true

    [<Test>]
    let ``ReadWriteRace under PCT covers every legal outcome`` () : unit =
        // The headline schedule-fuzzing claim: PCT, swept across a fixed
        // seed range, must enumerate *exactly* the legal outcome set —
        // neither missing one (we'd lose a class of real-world bugs) nor
        // inventing one (we'd flag false positives as legal). Equality
        // instead of `⊇` so a future PCT regression that produces a value
        // outside `readWriteRaceLegalOutcomes` (e.g. an uninitialised int
        // due to a memory-model bug in the interpreter) is caught here
        // rather than passing silently as "well, it covered 0 and 1, so
        // who cares about the extras."
        let image = compileImage "ReadWriteRace.cs"

        let observed =
            pctSeedSweep
            |> List.map (fun seed -> runPawPrint "ReadWriteRace.cs" image (Some seed))
            |> Set.ofList

        if observed <> readWriteRaceLegalOutcomes then
            let missing = Set.difference readWriteRaceLegalOutcomes observed
            let extra = Set.difference observed readWriteRaceLegalOutcomes

            failwith
                $"ReadWriteRace.cs PCT sweep over %d{List.length pctSeedSweep} seeds observed %A{Set.toList observed}; expected %A{Set.toList readWriteRaceLegalOutcomes}. Missing: %A{Set.toList missing}; Unexpected: %A{Set.toList extra}."

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

    /// Everything about the simulated process except which of its schedules we
    /// are exploring. Naming it separately from `HostConfig` is what lets the
    /// seed-independent prefix of a sweep be computed without a seed in scope.
    let private guestConfig : GuestConfig = GuestConfig.Default dotnetRuntimes

    /// Extract the guest's exit code from a completed run. Fails the test
    /// loudly on any non-normal exit: FailFast / signal / unhandled exception
    /// under a race test would indicate either a corrupted interpreter state
    /// or a guest assumption the test was not designed to handle, and
    /// silently folding them into the observed set would muddle the legality
    /// claim.
    let private exitCodeOfOutcome (sourceName : string) (seed : uint64 option) (outcome : RunOutcome) : int =
        match outcome with
        | RunOutcome.NormalExit (terminalState, terminatingThread)
        | RunOutcome.ProcessExit (terminalState, terminatingThread) ->
            match terminalState.ThreadState.[terminatingThread].MethodState.EvaluationStack.Values with
            | EvalStackValue.Int32 (Int32Source.Verbatim exitCode) :: _ -> exitCode
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
        | RealRuntimeResult.UnhandledException report ->
            failwith $"%s{sourceName} terminated with an unhandled exception under the real runtime:\n%s{report}"
        | RealRuntimeResult.FailFast report ->
            failwith $"%s{sourceName} called Environment.FailFast under the real runtime:\n%s{report}"

    // Seed sweep used to characterise PCT coverage. The first 30 splitmix64
    // outputs already hit both interleavings of ReadWriteRace; running 64
    // gives a generous margin for future races whose rarer interleavings
    // might land deeper in the seed stream. Widening it is cheap: the seeds
    // share their prefix (see `sweepPctExitCodes`), so an extra seed costs
    // only the post-fork suffix of a run.
    let private pctSeedSweep : uint64 list = [ 0UL .. 63UL ]

    /// Sweep `pctSeedSweep` over one guest and return the set of exit codes
    /// observed.
    ///
    /// Everything up to the guest's first *contended* scheduling decision is
    /// forced — no policy has a choice there, so every seed executes it
    /// identically — so it is computed once under the randomness-free
    /// round-robin policy and each seed resumes from the snapshot.
    /// `Program.resumeFork` makes such a run bit-identical to the from-scratch
    /// `PctSeed = Some s` run it replaces, which `TestScheduleFork` pins
    /// directly for both guests swept here; the exact-coverage assertions below
    /// are a second, independent check, since they are sensitive to the whole
    /// seed-to-schedule mapping.
    ///
    /// A guest that never forks fails rather than answering the sweep in one
    /// run: these tests characterise the outcomes a *race* can produce, and a
    /// guest with no schedule space has no race left to characterise.
    let private sweepPctExitCodes (sourceName : string) (image : byte[]) : Set<int> =
        // Scoped to the whole sweep: `Program.resumeFork` documents that the
        // prefix's factory must outlive every resume, since the loaded
        // assemblies and `BaseClassTypes` were built against it. See
        // `TestConcurrencyBugs.demonstrate` for why that is a precondition kept
        // rather than a crash avoided.
        let _prefixMessages, prefixLoggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", sourceName ; "run_phase", "shared-prefix" ]

        use _prefixLoggerFactoryResource = prefixLoggerFactory

        let snapshot =
            use peImage = new MemoryStream (image)

            match Program.runToFirstFork prefixLoggerFactory (Some sourceName) peImage guestConfig with
            | Program.PrefixOutcome.ForkedAt snapshot -> snapshot
            | Program.PrefixOutcome.NeverForked _ ->
                failwith
                    $"%s{sourceName} ran to completion without ever having two threads Runnable at the same tick, so every seed produces the same execution and the sweep characterises nothing."
            | Program.PrefixOutcome.DeadlockedBeforeFork stuck ->
                failwith $"%s{sourceName} wedged before any scheduling choice arose; stuck threads: %s{stuck}"
            | Program.PrefixOutcome.ForkedDuringStartup contenders ->
                failwith
                    $"%s{sourceName} first contends during startup (contenders: %A{contenders}), which `Program.runToFirstFork` refuses to snapshot; the race is supposed to be between two threads inside Main."

        pctSeedSweep
        |> List.map (fun seed ->
            let _messages, loggerFactory =
                LoggerFactory.makeTestWithProperties [ "source_file", sourceName ; "pct_seed", string seed ]

            use _loggerFactoryResource = loggerFactory
            let logger = loggerFactory.CreateLogger "TestRaces"

            Program.resumeFork loggerFactory (Some seed) snapshot
            |> Program.pumpPrepared loggerFactory logger
            |> exitCodeOfOutcome sourceName (Some seed)
        )
        |> Set.ofList

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
        let observed = sweepPctExitCodes "ReadWriteRace.cs" image

        if observed <> readWriteRaceLegalOutcomes then
            let missing = Set.difference readWriteRaceLegalOutcomes observed
            let extra = Set.difference observed readWriteRaceLegalOutcomes

            failwith
                $"ReadWriteRace.cs PCT sweep over %d{List.length pctSeedSweep} seeds observed %A{Set.toList observed}; expected %A{Set.toList readWriteRaceLegalOutcomes}. Missing: %A{Set.toList missing}; Unexpected: %A{Set.toList extra}."

    /// Step `image` instruction by instruction under the given scheduler seed,
    /// returning the guest's exit code and how many steps reported
    /// `WhatWeDid.BlockedOnClassInit` — i.e. how many times a thread found a
    /// type's initialiser already in progress on a *different* thread.
    ///
    /// Drives `Program.stepPrepared` rather than `Program.run` purely for that
    /// count: the outcome of each step is otherwise invisible from the terminal
    /// state, and a contention test that cannot see whether it contended is not
    /// a test.
    let private runCountingClassInitBlocks (sourceName : string) (image : byte[]) (seed : uint64 option) : int * int =
        let _messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", sourceName ]

        use _loggerFactoryResource = loggerFactory
        use peImage = new MemoryStream (image)
        let logger = loggerFactory.CreateLogger "TestRaces"

        match
            Program.prepare
                loggerFactory
                (Some sourceName)
                peImage
                { HostConfig.Default dotnetRuntimes with
                    PctSeed = seed
                }
        with
        | Program.ProgramStartResult.CompletedBeforeMain outcome ->
            failwith
                $"%s{sourceName} (seed=%A{seed}) completed before Main ran (%A{outcome}); the race is between two threads inside Main"
        | Program.ProgramStartResult.Ready prepared ->
            let rec loop (prepared : Program.PreparedProgram) (blocked : int) : int * int =
                match Program.stepPrepared loggerFactory logger prepared with
                | Program.ProgramStepOutcome.Completed outcome -> exitCodeOfOutcome sourceName seed outcome, blocked
                | Program.ProgramStepOutcome.Deadlocked (_, stuck) ->
                    failwith $"%s{sourceName} (seed=%A{seed}) deadlocked with threads stuck: %s{stuck}"
                | Program.ProgramStepOutcome.InstructionStepped (p, _, whatWeDid, _) ->
                    match whatWeDid with
                    | WhatWeDid.BlockedOnClassInit _ -> loop p (blocked + 1)
                    | _ -> loop p blocked
                | Program.ProgramStepOutcome.WorkerTerminated (p, _) -> loop p blocked

            loop prepared 0

    /// ECMA-335 II.10.5.3.2 leaves no latitude here, unlike ReadWriteRace: a
    /// thread that finds a type initialiser running on another thread blocks
    /// until it completes, so both threads necessarily observe the fully
    /// initialised static. The one legal exit code is 0; 1 or 2 would mean a
    /// thread read `SlowCctor.Value` while the `.cctor` was still counting.
    let private newobjCctorRaceLegalOutcomes : Set<int> = Set.ofList [ 0 ]

    [<Test>]
    let ``NewobjCctorRace under the real runtime produces a legal outcome`` () : unit =
        let image = compileImage "NewobjCctorRace.cs"
        let exitCode = runRealRuntime "NewobjCctorRace.cs" image
        newobjCctorRaceLegalOutcomes |> Set.contains exitCode |> shouldEqual true

    [<Test>]
    let ``NewobjCctorRace under PCT covers every legal outcome`` () : unit =
        // Unlike the ReadWriteRace sweep this is a single-element set, so the
        // equality is really "no seed ever sees a partially-initialised type".
        // The sweep is what makes that a claim about the blocking *and* the
        // resumption: a thread parked on the initialiser has to be woken and
        // has to re-execute its `newobj` correctly, under every interleaving
        // PCT reaches, not merely under the default round-robin.
        let image = compileImage "NewobjCctorRace.cs"
        let observed = sweepPctExitCodes "NewobjCctorRace.cs" image

        if observed <> newobjCctorRaceLegalOutcomes then
            let missing = Set.difference newobjCctorRaceLegalOutcomes observed
            let extra = Set.difference observed newobjCctorRaceLegalOutcomes

            failwith
                $"NewobjCctorRace.cs PCT sweep over %d{List.length pctSeedSweep} seeds observed %A{Set.toList observed}; expected %A{Set.toList newobjCctorRaceLegalOutcomes}. Missing: %A{Set.toList missing}; Unexpected: %A{Set.toList extra}."

    [<Test>]
    let ``NewobjCctorRace really does contend on the type initialiser`` () : unit =
        // Guards the tests above against silently ceasing to test anything.
        // Every assertion they make is also satisfied by a schedule in which
        // the two `newobj`s never overlap — the losing thread would simply
        // find the type already initialised and sail through. If a future
        // scheduler change, or a change to the cctor's cost, stops the threads
        // overlapping, this fails and says so, rather than leaving a
        // contention test that contends with nothing.
        let image = compileImage "NewobjCctorRace.cs"
        let exitCode, blocked = runCountingClassInitBlocks "NewobjCctorRace.cs" image None

        exitCode |> shouldEqual 0

        if blocked = 0 then
            failwith
                "NewobjCctorRace.cs ran to completion under the default schedule without either thread ever blocking on the other's type initialiser, so it no longer exercises the contended `newobj` path. Lengthen SlowCctor's initialiser, or pick a seed that interleaves the two `newobj`s."

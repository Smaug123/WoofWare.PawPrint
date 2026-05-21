namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint

/// Harness for "find the bad interleaving" concurrency-bug demonstrations.
/// Each scenario is a one-file C# guest plus a classifier describing what
/// it means for PCT to have *found* the bug; the test sweeps a fixed
/// range of PCT seeds and asserts at least one of them produces the
/// targeted bad outcome.
///
/// Each guest must encode the invariant violation as a deterministic,
/// host-visible event: a sentinel exit code from Main, an unhandled
/// guest exception, a call to Environment.FailFast, or a natural
/// deadlock (no Runnable threads while the entry thread is still
/// alive). Guests SHOULD NOT rely on `Debug.Assert(false)` -- under
/// Release-equivalent CSC settings the call is compiled out, and even
/// under Debug it routes through DebugProvider, not FailFast. Prefer
/// `if (badCondition) Environment.FailFast(...)` or `if (badCondition)
/// return SENTINEL;` so the bad path is unconditionally observable.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestConcurrencyBugs =

    /// Classifier for "PCT found the bug" on a single seeded run. Pick the
    /// one that matches how the guest encodes the bad interleaving; the
    /// scenario fails only if no seed in its sweep produces a matching
    /// outcome.
    [<RequireQualifiedAccess>]
    type BadOutcome =
        /// Main returned this exit code (NormalExit or ProcessExit both
        /// route here). Use when the guest checks the invariant in
        /// managed code and reports the violation via Main's return value.
        | ExitCode of int
        /// Every non-terminated thread is blocked; no Runnable thread is
        /// left to make progress. Use for AB-BA / cyclic-wait scenarios
        /// where the bad interleaving is itself the bug.
        | Deadlock
        /// A guest thread raised an unhandled CLI exception (e.g. a
        /// NullReferenceException from a torn read of a freshly-published
        /// object reference).
        | UnhandledException
        /// A guest thread called `Environment.FailFast`. Useful when the
        /// guest detects the bad state in code that can't easily return
        /// a sentinel value -- e.g. from inside a worker.
        | FailFast

    type Scenario =
        {
            /// Bare filename of the embedded .cs resource; the fsproj
            /// globs sourcesConcurrencyBugs/*.cs as embedded resources, so
            /// the name is the file's basename only.
            SourceName : string
            /// Human-readable description of the scenario and the bad
            /// interleaving it teaches. Surfaced in the failure message
            /// if PCT misses the bug, so make it specific.
            Description : string
            /// What outcome of a single seeded run means "PCT found it".
            Bad : BadOutcome
            /// PCT seeds to sweep. The first match wins; the test fails
            /// only if every seed in the list produced a non-matching
            /// outcome. The default `defaultSeeds` (4096 seeds) is sized
            /// to catch one-shot races at the current `P_BASE = 0.01`
            /// preemption density -- shrink for scenarios whose bad
            /// interleaving is common, widen for ones rarer than that.
            Seeds : uint64 list
        }

        override this.ToString () = this.SourceName

    /// Host-level summary of one seeded run. Distinct from `RunOutcome`
    /// because `Deadlocked` is a step-level observation that
    /// `Program.pumpPrepared` collapses into a host `failwith`; we drive
    /// the pump manually to preserve it.
    [<RequireQualifiedAccess>]
    type private RunSummary =
        | ExitCode of int
        | NonIntReturn of string
        | Deadlock of stuck : string
        | UnhandledException of string
        | FailFast of message : string
        | Signal of string
        /// A worker spawned during a static cctor terminated the
        /// process before Main got to run. The wrapped string is the
        /// pre-Main `RunOutcome` collapsed to its textual classifier.
        | CompletedBeforeMain of summary : string

    let private assy = typeof<RunResult>.Assembly

    let private dotnetRuntimes : ImmutableArray<string> =
        DotnetRuntime.SelectForDll assy.Location |> ImmutableArray.CreateRange

    /// Compile the guest once per scenario. PCT sweeps reuse the resulting
    /// image across all seeds; recompiling per seed would dominate the
    /// test duration without changing semantics.
    let private compileImage (sourceName : string) : byte[] =
        let source = Assembly.getEmbeddedResourceAsString sourceName assy
        Roslyn.compile [ source ]

    let private extractExitCode (state : IlMachineState) (thread : ThreadId) : RunSummary =
        match state.ThreadState.[thread].MethodState.EvaluationStack.Values with
        | EvalStackValue.Int32 code :: _ -> RunSummary.ExitCode code
        | [] -> RunSummary.NonIntReturn "void"
        | head :: _ -> RunSummary.NonIntReturn (sprintf "%O" head)

    let private classifyRunOutcome (outcome : RunOutcome) : RunSummary =
        match outcome with
        | RunOutcome.NormalExit (state, thread)
        | RunOutcome.ProcessExit (state, thread) -> extractExitCode state thread
        | RunOutcome.GuestUnhandledException (_, _, exn) ->
            RunSummary.UnhandledException (sprintf "%O" exn.ExceptionObject)
        | RunOutcome.FailFast (_, _, message) -> RunSummary.FailFast (Option.defaultValue "<no message>" message)
        | RunOutcome.SignalTerminated (_, signal) -> RunSummary.Signal (sprintf "%O" signal)

    /// Run the guest under one PCT seed, returning a host-level summary of
    /// where it ended up. Drives `Program.stepPrepared` directly instead of
    /// `Program.run` so that `ProgramStepOutcome.Deadlocked` surfaces as a
    /// classifier-friendly `RunSummary.Deadlock` rather than the
    /// `failwith "Deadlock: ..."` that `pumpPrepared` would raise.
    let private runOne (sourceName : string) (image : byte[]) (seed : uint64) : RunSummary =
        let _messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", sourceName ; "pct_seed", string seed ]

        use _loggerFactoryResource = loggerFactory
        use peImage = new MemoryStream (image)
        let logger = loggerFactory.CreateLogger "TestConcurrencyBugs"
        let impls = MockEnv.make ()

        match Program.prepare loggerFactory (Some sourceName) peImage dotnetRuntimes impls Map.empty (Some seed) [] with
        | Program.ProgramStartResult.CompletedBeforeMain outcome ->
            // A worker spawned during a static cctor terminated the process
            // before Main got to run. For these scenarios that almost
            // always means the guest accidentally put effectful code into
            // a type initializer; surface it as a labelled summary rather
            // than silently treating it as the Main run's outcome. We
            // collapse the inner `RunOutcome` to its string classifier so
            // `RunSummary` itself remains equality-safe (the live
            // `IlMachineState` in `RunOutcome` carries non-equatable
            // fields like loggers).
            let inner = classifyRunOutcome outcome
            RunSummary.CompletedBeforeMain (sprintf "%A" inner)
        | Program.ProgramStartResult.Ready prepared ->
            let rec loop (prepared : Program.PreparedProgram) : RunSummary =
                match Program.stepPrepared loggerFactory logger impls prepared with
                | Program.ProgramStepOutcome.Completed outcome -> classifyRunOutcome outcome
                | Program.ProgramStepOutcome.Deadlocked (_, stuck) -> RunSummary.Deadlock stuck
                | Program.ProgramStepOutcome.InstructionStepped (p, _, _) -> loop p
                | Program.ProgramStepOutcome.WorkerTerminated (p, _) -> loop p

            loop prepared

    let private matches (bad : BadOutcome) (summary : RunSummary) : bool =
        match bad, summary with
        | BadOutcome.ExitCode want, RunSummary.ExitCode got -> want = got
        | BadOutcome.Deadlock, RunSummary.Deadlock _ -> true
        | BadOutcome.UnhandledException, RunSummary.UnhandledException _ -> true
        | BadOutcome.FailFast, RunSummary.FailFast _ -> true
        | BadOutcome.ExitCode _, _
        | BadOutcome.Deadlock, _
        | BadOutcome.UnhandledException, _
        | BadOutcome.FailFast, _ -> false

    /// Reasonable default sweep, sized to catch one-shot two-thread
    /// races (e.g. a single-iteration shared-counter `++`) at the
    /// current `P_BASE = 0.01` preemption density. Sweeps short-circuit
    /// on the first matching seed via `List.tryFind`, so widening this
    /// is cheap for scenarios whose bad interleaving is common; the
    /// 4096-seed cap matters only when the scenario never reaches the
    /// bad outcome.
    let defaultSeeds : uint64 list = [ 0UL .. 4095UL ]

    // -------------------------------------------------------------------
    // To add a new scenario:
    //
    //   1. Drop the C# source under
    //      WoofWare.PawPrint.Test/sourcesConcurrencyBugs/ -- the fsproj
    //      globs that directory's *.cs as embedded resources.
    //
    //   2. Make the bad interleaving deterministically observable:
    //        * return a sentinel int from Main, OR
    //        * throw an unhandled exception in a worker, OR
    //        * call Environment.FailFast, OR
    //        * naturally deadlock all threads.
    //      Do not rely on Debug.Assert -- see the module docstring.
    //
    //   3. Append a record to `scenarios` below referencing the file and
    //      picking the matching `BadOutcome`. Use `defaultSeeds` unless
    //      a wider sweep is necessary.
    // -------------------------------------------------------------------

    let scenarios : Scenario list =
        [
            {
                SourceName = "LostUpdate.cs"
                Description =
                    "Two threads do an unsynchronized read-modify-write on a shared int counter. "
                    + "Both reading 0 before either writes produces a lost update (counter == 1 instead of 2); "
                    + "Main returns `2 - counter`, so exit code 1 is the bad outcome."
                Bad = BadOutcome.ExitCode 1
                Seeds = defaultSeeds
            }

            {
                SourceName = "JustABoolNotAMutex.cs"
                Description = "Two threads try to guard a critical section using just a boolean flag, not a mutex"
                Bad = BadOutcome.UnhandledException
                Seeds = defaultSeeds
            }

            {
                SourceName = "TwoCountersSeparated.cs"
                Description = "Incrementing a counter after another counter, we can see the first counter be bigger"
                Bad = BadOutcome.UnhandledException
                Seeds = defaultSeeds
            }

            {
                SourceName = "SimultaneousCounter.cs"
                Description = "Two threads can simultaneously see the same counter as having different values"
                Bad = BadOutcome.UnhandledException
                Seeds = defaultSeeds
            }

            {
                SourceName = "InvertedMonitorDeadlock.cs"
                Description = "Deadlock when mutices are taken in the wrong order"
                Bad = BadOutcome.Deadlock
                Seeds = defaultSeeds
            }

            {
                SourceName = "QueueIsNotThreadSafe.cs"
                Description = "Queue can expose state where Count > 0 but Dequeue fails"
                Bad = BadOutcome.UnhandledException
                Seeds = defaultSeeds
            }
        ]

    let private demonstrate (scenario : Scenario) : unit =
        let image = compileImage scenario.SourceName

        // Sweep seeds lazily and stop at the first match. The wider the
        // default sweep grows (currently 4096), the more important this
        // is: walking the whole list would dominate the test budget when
        // the bad interleaving is common. We record every summary the
        // search visited so the failure path can still print a useful
        // diagnostic when no seed matches.
        let visited = ResizeArray<uint64 * RunSummary> ()

        let visit (seed : uint64) : bool =
            let summary = runOne scenario.SourceName image seed
            visited.Add (seed, summary)
            matches scenario.Bad summary

        let hit = scenario.Seeds |> Array.ofList |> Array.Parallel.tryFind visit

        match hit with
        | Some _ -> ()
        | None ->
            // Surface the distinct observed outcomes (with one example
            // seed each) so a regression that silently loses the bad
            // interleaving is easy to diagnose without rerunning by hand.
            let observed =
                visited
                |> Seq.toList
                |> List.groupBy snd
                |> List.map (fun (summary, group) ->
                    let exampleSeed = group |> List.head |> fst
                    summary, exampleSeed, List.length group
                )

            failwith
                $"%s{scenario.SourceName}: %s{scenario.Description}\nPCT swept %d{visited.Count} seeds (of %d{List.length scenario.Seeds}) and never matched the bad outcome %A{scenario.Bad}. Observed (summary, example seed, count): %A{observed}"

    [<TestCaseSource(nameof scenarios)>]
    let ``PCT exhibits the bad interleaving`` (scenario : Scenario) : unit = demonstrate scenario

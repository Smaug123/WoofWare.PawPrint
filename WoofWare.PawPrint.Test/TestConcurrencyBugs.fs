namespace WoofWare.PawPrint.Test

open System.Collections.Concurrent
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

    /// Specification of which guest exception "found the bug" looks like.
    /// `TypeFullName` is matched exactly against the guest exception's
    /// fully-qualified type name (e.g. `"System.InvalidOperationException"`).
    /// When `MessageContains` is non-empty, the guest exception's `_message`
    /// field must additionally contain one of the listed substrings; an
    /// empty list disables the message check. Tightening the type/message
    /// here is what prevents an *unrelated* guest exception (a mistake in
    /// the fixture, a NullReferenceException from a different bug) from
    /// passing the scenario.
    type ExceptionMatch =
        {
            TypeFullName : string
            MessageContains : string list
        }

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
        /// A guest thread raised an unhandled CLI exception matching the
        /// supplied type (and optionally message substring). The exception
        /// type and message are extracted by the host from the guest's
        /// heap-allocated exception object so the scenario fails fast if a
        /// *different* exception is observed (which would indicate a
        /// different bug, not the targeted one).
        | UnhandledException of ExceptionMatch
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
            /// outcome. See `defaultSeeds` for how the default 4096 is
            /// sized against the measured per-scenario hit densities.
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
        /// A guest thread raised an unhandled CLI exception.
        /// `TypeFullName` is the guest type's fully-qualified name (extracted
        /// by the host from the exception object on the managed heap); `Message`
        /// is the guest exception's `_message` field, or `None` if the field
        /// was null / could not be read. We carry the structured fields rather
        /// than just a rendered string so the matcher can do precise type/message
        /// comparisons without scraping `sprintf` output.
        | UnhandledException of typeFullName : string * message : string option
        | FailFast of message : string
        | Signal of string
        /// A worker spawned during a static cctor terminated the
        /// process before Main got to run. The wrapped string is the
        /// pre-Main `RunOutcome` collapsed to its textual classifier.
        | CompletedBeforeMain of summary : string
        /// The run was abandoned after `stepBudget` scheduler steps without
        /// terminating. This is a real outcome, not an error: a guest can have
        /// a genuinely non-terminating interleaving, and a scheduler whose job
        /// is to explore interleavings will eventually pick one. It never
        /// satisfies a `BadOutcome`, so a scenario that only ever exhausts the
        /// budget fails rather than passing vacuously.
        | StepBudgetExhausted

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
        | EvalStackValue.Int32 (Int32Source.Verbatim code) :: _ -> RunSummary.ExitCode code
        | [] -> RunSummary.NonIntReturn "void"
        | head :: _ -> RunSummary.NonIntReturn (sprintf "%O" head)

    /// Read the fully-qualified type name of the guest exception object from the
    /// host's `AllConcreteTypes` registry. The exception object's runtime type is
    /// what we want to match against, not its statically-declared `catch` type,
    /// so we go through the managed heap rather than the `CliException`'s frames.
    let private exceptionTypeFullName (state : IlMachineState) (addr : ManagedHeapAddress) : string =
        let handle = ManagedHeap.getObjectConcreteType addr state.ManagedHeap

        match AllConcreteTypes.lookup handle state.ConcreteTypes with
        | Some ct ->
            if System.String.IsNullOrEmpty ct.Namespace then
                ct.Name
            else
                $"%s{ct.Namespace}.%s{ct.Name}"
        | None ->
            // Structural handles (byref, pointer, array, function pointer) can't
            // be the runtime type of a thrown exception object, so we don't expect
            // to land here under correct guest behaviour. Render the handle so the
            // diagnostic is still actionable if something pathological occurs.
            sprintf "<unresolved type %O>" handle

    /// Read the guest exception's `Exception._message` field via the host's view of
    /// the managed heap. Returns `None` if the field is null. We extract this on
    /// the host side (rather than calling guest code) so we don't perturb the
    /// state we're about to discard, and so the scenario classifier never depends
    /// on the guest's `ToString()` being available.
    let private exceptionMessage (state : IlMachineState) (addr : ManagedHeapAddress) : string option =
        let obj = ManagedHeap.get addr state.ManagedHeap

        match AllocatedNonArrayObject.DereferenceField "_message" obj with
        | CliType.ObjectRef None -> None
        | CliType.ObjectRef (Some msgAddr) -> ManagedHeap.getStringContents msgAddr state.ManagedHeap
        | _ -> None

    let private classifyRunOutcome (outcome : RunOutcome) : RunSummary =
        match outcome with
        | RunOutcome.NormalExit (state, thread)
        | RunOutcome.ProcessExit (state, thread) -> extractExitCode state thread
        | RunOutcome.GuestUnhandledException (state, _, exn) ->
            let typeName = exceptionTypeFullName state exn.ExceptionObject
            let message = exceptionMessage state exn.ExceptionObject
            RunSummary.UnhandledException (typeName, message)
        | RunOutcome.FailFast (_, _, message) -> RunSummary.FailFast (Option.defaultValue "<no message>" message)
        | RunOutcome.SignalTerminated (_, signal) -> RunSummary.Signal (sprintf "%O" signal)

    /// Scheduler steps after which a run is abandoned as non-terminating.
    ///
    /// This bound is not a performance knob; without it the fixture does not
    /// terminate. These guests are deliberately unsynchronized, and some admit
    /// interleavings under which a thread never makes its exit condition true
    /// again. `SimultaneousCounter.cs` is the worked example: `counter` only
    /// ever increases, `Worker1` retires as soon as it observes `counter >= 2`,
    /// and `Worker2` returns only on observing `counter == 3` immediately after
    /// its own increment -- so any schedule that carries the counter past 3
    /// without `Worker2` catching it leaves `Worker2` spinning forever. That is
    /// a legitimate execution which a real machine can also produce; the bug
    /// would be for the harness to run it unboundedly.
    ///
    /// The budget is picked from measurement, not taste: over a 2048-seed sweep
    /// of every scenario, the slowest *terminating* run took 16,990 steps
    /// (`JustABoolNotAMutex.cs`), and four of the six finish inside 1,200. Two
    /// million leaves two orders of magnitude of headroom over the worst
    /// terminating run, while capping a divergent seed at about two seconds --
    /// the seven divergent seeds of `SimultaneousCounter.cs` are what makes the
    /// difference between a 49-second sweep and one that never finishes.
    /// Widening a scenario's sweep because it started reporting
    /// `StepBudgetExhausted` would be assuming the conclusion -- re-measure the
    /// terminating cost first.
    let private stepBudget : int = 2_000_000

    /// Run the guest under one PCT seed, returning a host-level summary of
    /// where it ended up. Drives `Program.stepPrepared` directly instead of
    /// `Program.run` so that `ProgramStepOutcome.Deadlocked` surfaces as a
    /// classifier-friendly `RunSummary.Deadlock` rather than the
    /// `failwith "Deadlock: ..."` that `pumpPrepared` would raise.
    ///
    /// Bounded by `budget` scheduler steps: a run that has not terminated by
    /// then is reported as `RunSummary.StepBudgetExhausted`. Every scenario
    /// sweep passes `stepBudget`; the parameter exists so the bound itself can
    /// be tested without a two-million-step run.
    let private runOneWithBudget (sourceName : string) (image : byte[]) (seed : uint64) (budget : int) : RunSummary =
        let _messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", sourceName ; "pct_seed", string seed ]

        use _loggerFactoryResource = loggerFactory
        use peImage = new MemoryStream (image)
        let logger = loggerFactory.CreateLogger "TestConcurrencyBugs"

        match
            Program.prepare
                loggerFactory
                (Some sourceName)
                peImage
                { HostConfig.Default dotnetRuntimes with
                    PctSeed = (Some seed)
                }
        with
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
            let rec loop (prepared : Program.PreparedProgram) (steps : int) : RunSummary =
                if steps >= budget then
                    RunSummary.StepBudgetExhausted
                else

                match Program.stepPrepared loggerFactory logger prepared with
                | Program.ProgramStepOutcome.Completed outcome -> classifyRunOutcome outcome
                | Program.ProgramStepOutcome.Deadlocked (_, stuck) -> RunSummary.Deadlock stuck
                | Program.ProgramStepOutcome.InstructionStepped (p, _, _) -> loop p (steps + 1)
                | Program.ProgramStepOutcome.WorkerTerminated (p, _) -> loop p (steps + 1)

            loop prepared 0

    /// Run the guest under one PCT seed and the standard `stepBudget`.
    let private runOne (sourceName : string) (image : byte[]) (seed : uint64) : RunSummary =
        runOneWithBudget sourceName image seed stepBudget

    let private matches (bad : BadOutcome) (summary : RunSummary) : bool =
        match bad, summary with
        | BadOutcome.ExitCode want, RunSummary.ExitCode got -> want = got
        | BadOutcome.Deadlock, RunSummary.Deadlock _ -> true
        | BadOutcome.UnhandledException spec, RunSummary.UnhandledException (typeName, message) ->
            // Require the runtime type to match exactly; otherwise a *different* bug
            // raising a different exception would silently count as "found it".
            if typeName <> spec.TypeFullName then
                false
            else
                // Empty list disables the message check (caller opted out). Otherwise
                // at least one substring must appear in the actual message. A null
                // guest message can never satisfy a non-empty `MessageContains`.
                match spec.MessageContains with
                | [] -> true
                | substrings ->
                    match message with
                    | None -> false
                    | Some actual -> substrings |> List.exists actual.Contains
        | BadOutcome.FailFast, RunSummary.FailFast _ -> true
        | BadOutcome.ExitCode _, _
        | BadOutcome.Deadlock, _
        | BadOutcome.UnhandledException _, _
        | BadOutcome.FailFast, _ -> false

    /// Reasonable default sweep, sized to catch one-shot two-thread races
    /// (e.g. a single-iteration shared-counter `++`).
    ///
    /// It is sized against the *rarest* scenario, not the typical one. Measured
    /// over a 2048-seed window, hit density ranges from about 1% -- 21 seeds for
    /// `TwoCountersSeparated.cs`, 27 for `QueueIsNotThreadSafe.cs`, 32 for
    /// `LostUpdate.cs` -- up to 92% for `SimultaneousCounter.cs`. At 1% a
    /// 4096-seed sweep misses only with probability around `0.99^4096`, which is
    /// negligible, and the sweeps short-circuit on the first match, so the cap
    /// costs nothing except when a scenario genuinely never reaches its bad
    /// outcome.
    ///
    /// Widening this because a scenario started failing would be assuming the
    /// conclusion: re-measure its density first, since a real loss of hit rate
    /// is exactly what such a failure reports.
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
                Bad =
                    BadOutcome.UnhandledException
                        {
                            TypeFullName = "System.Exception"
                            MessageContains = [ "failed!" ]
                        }
                Seeds = defaultSeeds
            }

            {
                SourceName = "TwoCountersSeparated.cs"
                Description = "Incrementing a counter after another counter, we can see the first counter be bigger"
                Bad =
                    BadOutcome.UnhandledException
                        {
                            TypeFullName = "System.Exception"
                            MessageContains = [ "counter2 was bigger than counter1!" ]
                        }
                Seeds = defaultSeeds
            }

            {
                SourceName = "SimultaneousCounter.cs"
                // The guest raises one of two messages depending on which worker
                // observes the racing peer first; both are evidence of the same
                // critical-section race and either should satisfy the scenario.
                Description = "Two threads can simultaneously see the same counter as having different values"
                Bad =
                    BadOutcome.UnhandledException
                        {
                            TypeFullName = "System.Exception"
                            MessageContains =
                                [
                                    "we were not the first"
                                    "Worker1 is in the critical section at the same time as us"
                                ]
                        }
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
                Bad =
                    BadOutcome.UnhandledException
                        {
                            TypeFullName = "System.InvalidOperationException"
                            MessageContains = [ "torn enqueue: Count > 0 but slot was empty" ]
                        }
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
        // diagnostic when no seed matches. `Array.Parallel.tryFind` runs
        // the predicate on multiple threads concurrently, so the
        // diagnostic store must be thread-safe — a `ResizeArray.Add`
        // here races and can drop or duplicate entries (or, worse, throw
        // out of `EnsureCapacity`), corrupting the failure diagnostic.
        let visited = ConcurrentBag<uint64 * RunSummary> ()

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

    /// The budget has to bind on a run that would not terminate, and has to stay
    /// clear of runs that would. Only the first property keeps the sweep finite,
    /// but a budget tight enough to cut healthy runs short would silently turn
    /// real outcomes into non-results, so both directions are asserted here
    /// rather than trusting the constant.
    [<Test>]
    let ``step budget binds, and only on a run that overruns it`` () : unit =
        let image = compileImage "LostUpdate.cs"

        // Measured over a 2048-seed sweep, every LostUpdate.cs run terminates within
        // 486 steps -- so 50 steps must bind, and the real budget must not.
        let bounded = runOneWithBudget "LostUpdate.cs" image 0UL 50
        Assert.That (bounded, Is.EqualTo RunSummary.StepBudgetExhausted)

        let unbounded = runOneWithBudget "LostUpdate.cs" image 0UL stepBudget
        Assert.That (unbounded, Is.Not.EqualTo RunSummary.StepBudgetExhausted)

    /// Exhausting the budget must never satisfy a scenario's `BadOutcome`. If it
    /// did, a scenario every one of whose seeds diverged would report success
    /// while having demonstrated nothing at all.
    [<Test>]
    let ``an exhausted budget never counts as finding the bug`` () : unit =
        for scenario in scenarios do
            Assert.That (
                matches scenario.Bad RunSummary.StepBudgetExhausted,
                Is.False,
                sprintf "%s treated a budget exhaustion as its bad outcome" scenario.SourceName
            )

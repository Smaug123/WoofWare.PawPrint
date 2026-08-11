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
/// The sweep does not re-run the whole guest per seed. Everything up to
/// the first tick at which two threads are Runnable is forced — no policy
/// has a choice there, so every seed executes it identically — so it is
/// computed once, under the randomness-free round-robin policy, and each
/// seed resumes from that snapshot. `Program.resumeFork` makes a resumed
/// run bit-identical to the from-scratch `PctSeed = Some s` run it
/// replaces, which `TestScheduleFork` pins over this exact guest corpus.
/// The prefix is 74-94% of a run's instructions on these guests.
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

    let private assy = typeof<RunResult>.Assembly

    let private dotnetRuntimes : ImmutableArray<string> =
        DotnetRuntime.SelectForDll assy.Location |> ImmutableArray.CreateRange

    /// Everything about the simulated process except which of its schedules
    /// we are exploring. `Program.runToFirstFork` takes this rather than a
    /// `HostConfig` precisely so that no seed can reach the shared prefix.
    let private guestConfig : GuestConfig = GuestConfig.Default dotnetRuntimes

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

    /// Compute the part of a run that every seed in the sweep shares: everything
    /// up to the guest's first *contended* scheduling decision.
    ///
    /// Every non-forking outcome is a test failure rather than a sweep of size
    /// one, and deliberately so. A guest with no fork point has no schedule
    /// space at all, so "PCT exhibits the bad interleaving" would be vacuous
    /// even if that single forced run happened to match the scenario's
    /// `BadOutcome` — nothing chose anything, so PCT cannot have found
    /// anything. Failing here says which of the three ways it went wrong.
    let private forkOf
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (scenario : Scenario)
        (image : byte[])
        : Program.ForkSnapshot
        =
        use peImage = new MemoryStream (image)

        match Program.runToFirstFork loggerFactory (Some scenario.SourceName) peImage guestConfig with
        | Program.PrefixOutcome.ForkedAt snapshot -> snapshot
        | Program.PrefixOutcome.NeverForked outcome ->
            // Collapsed to a `RunSummary` rather than rendered directly: a
            // `RunOutcome` carries an entire `IlMachineState`, so `%A` on it
            // would render a large chunk of the guest heap into the message.
            failwith
                $"%s{scenario.SourceName}: the guest ran to completion (%A{classifyRunOutcome outcome}) without ever having two threads Runnable at the same tick, so no seed can explore anything and the scenario tests nothing. A concurrency-bug guest must start a thread whose lifetime overlaps another's."
        | Program.PrefixOutcome.DeadlockedBeforeFork stuck ->
            failwith
                $"%s{scenario.SourceName}: every thread blocked before any scheduling choice arose (stuck: %s{stuck}), so the guest wedges under every seed rather than under a bad interleaving."
        | Program.PrefixOutcome.ForkedDuringStartup contenders ->
            failwith
                $"%s{scenario.SourceName}: a static initialiser started a thread, so the first contended decision happens during startup (contenders: %A{contenders}) and `Program.runToFirstFork` refuses to snapshot it. Start the guest's threads from Main."

    /// Resume the shared prefix under one PCT seed, returning a host-level
    /// summary of where the run ended up. Drives `Program.stepPrepared`
    /// directly instead of `Program.pumpPrepared` so that
    /// `ProgramStepOutcome.Deadlocked` surfaces as a classifier-friendly
    /// `RunSummary.Deadlock` rather than the `failwith "Deadlock: ..."` the
    /// pump would raise.
    ///
    /// The factory here is this seed's own: `resumeFork` rebinds the machine's
    /// logging sink to it, so each seed's trace still carries its own
    /// `pct_seed` property even though the prefix was logged through the
    /// sweep's factory.
    let private runOne (sourceName : string) (snapshot : Program.ForkSnapshot) (seed : uint64) : RunSummary =
        let _messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", sourceName ; "pct_seed", string seed ]

        use _loggerFactoryResource = loggerFactory
        let logger = loggerFactory.CreateLogger "TestConcurrencyBugs"

        let rec loop (prepared : Program.PreparedProgram) : RunSummary =
            match Program.stepPrepared loggerFactory logger prepared with
            | Program.ProgramStepOutcome.Completed outcome -> classifyRunOutcome outcome
            | Program.ProgramStepOutcome.Deadlocked (_, stuck) -> RunSummary.Deadlock stuck
            | Program.ProgramStepOutcome.InstructionStepped (p, _, _, _) -> loop p
            | Program.ProgramStepOutcome.WorkerTerminated (p, _) -> loop p

        loop (Program.resumeFork loggerFactory (Some seed) snapshot)

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

        // Scoped to the whole sweep, not to the prefix computation:
        // `Program.resumeFork` documents that the prefix's factory must outlive
        // every resume, because the loaded assemblies and `BaseClassTypes` were
        // built against it. Honouring that is cheap and the alternative is
        // silent: disposing it right after `forkOf` was measured (with
        // `PAWPRINT_LOG_DIR` set, so the file sinks really are closed) not to
        // fault any seed, because `resumeFork` rebinds the machine's own sink
        // and nothing the prefix captured logs again afterwards. So this is a
        // documented precondition being kept, not a crash being avoided.
        let _prefixMessages, prefixLoggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", scenario.SourceName ; "run_phase", "shared-prefix" ]

        use _prefixLoggerFactoryResource = prefixLoggerFactory

        let snapshot = forkOf prefixLoggerFactory scenario image

        // Sweep seeds lazily and stop at the first match. The wider the
        // default sweep grows (currently 4096), the more important this
        // is: walking the whole list would dominate the test budget when
        // the bad interleaving is common. We record every summary the
        // search visited so the failure path can still print a useful
        // diagnostic when no seed matches. `Array.Parallel.tryFind` runs
        // the predicate on multiple threads concurrently, so the
        // diagnostic store must be thread-safe — a `ResizeArray.Add`
        // here races and can drop or duplicate entries (or, worse, throw
        // out of `EnsureCapacity`), corrupting the failure diagnostic. The
        // one `snapshot` is read by all of those threads at once; that is
        // sound because everything reachable from it is persistent F# data
        // or shared-by-design `MetadataReader` assemblies, and
        // `TestScheduleFork` machine-checks it rather than assuming it.
        let visited = ConcurrentBag<uint64 * RunSummary> ()

        let visit (seed : uint64) : bool =
            let summary = runOne scenario.SourceName snapshot seed
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

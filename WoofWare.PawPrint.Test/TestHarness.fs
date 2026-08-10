namespace WoofWare.PawPrint.Test

open WoofWare.PawPrint

/// Result of executing (some steps of) the program under PawPrint.
type RunResult =
    {
        /// Value that was left on the evaluation stack when execution stopped, **if**
        /// the program executed a `ret` that produced a value and PawPrint
        /// subsequently pushed it onto the stack.  This is only an early-stage
        /// approximation: once PawPrint supports a proper process-exit story we
        /// can promote this to a real exit–code.
        ExitCode : int option

        /// Final interpreter state after we stopped executing.
        FinalState : IlMachineState
    }

type EndToEndTestCase =
    {
        FileName : string
        ExpectedReturnCode : int
        /// Kernel configuration passed to `Program.run`: guest environment
        /// overlay (layered on top of `EmulatedKernel.defaultEnvironment`, so
        /// the invariant-globalization default is always present even when the
        /// overlay is empty) plus the reported processor count.
        KernelConfig : KernelConfig
        /// AppContext properties to seed before the guest starts, as a real host
        /// does from `runtimeconfig.json`. Must be empty for any case run through
        /// the *pure* (differential) harness: the oracle there loads the guest
        /// in-process on the host runtime, whose `AppContext` is already set up
        /// and cannot be reseeded, so a seeded property is a PawPrint-only fact
        /// and belongs in `sourcesImpure`. `TestPureCases.runTest` enforces this.
        AppContext : AppContextProperties
        ExpectsUnhandledException : bool
        /// Optional assertion run against the final PawPrint state once the
        /// guest has exited. Used by impure tests that want to verify
        /// interpreter-internal state (e.g. `state.Kernel.OutputLog`) that
        /// isn't observable as an exit code. Pure tests, which run the
        /// same source on the real CLR for cross-comparison, leave this
        /// `None` — the real runtime has no analogous state to assert
        /// against.
        AssertTerminalState : (IlMachineState -> unit) option
    }

/// `Program.run`, bounded so a guest that never terminates fails its test instead of wedging
/// the whole suite.
///
/// The bound covers the *whole* run, startup included. That matters because guest code runs
/// before `Main` — the AppContext seed, then class initialisers, the entry type's among them —
/// so a static initialiser that never returns is just as capable of hanging the suite as a
/// `Main` that never returns, and is harder to diagnose when it does.
///
/// The bound is a **step count, not a wall-clock timeout**, and that is deliberate. A timeout
/// would make the suite's verdict depend on how loaded the machine is: the same guest could
/// pass on a quiet laptop and fail on a busy CI runner, which is precisely the flakiness this
/// interpreter exists to eliminate. A step budget is a property of the guest, so it gives the
/// same answer everywhere and a failure reproduces exactly.
///
/// This is a test-harness concern only. `Program.run` stays unbounded, because a real host
/// running a real program has no business deciding the guest has gone on too long.
[<RequireQualifiedAccess>]
module BoundedRun =

    open System.IO
    open Microsoft.Extensions.Logging

    /// How many interpreted IL steps a guest may retire, startup and `Main` together, before
    /// the harness calls it stuck.
    ///
    /// Chosen from measurement, not taste. Across the whole end-to-end corpus (596 guests at
    /// the time of writing) the mean guest retires ~22k steps and the heaviest —
    /// `RegexConstructionRepeatedNonBacktracking.cs` — retires 1,817,355. This bound is ~11x
    /// that worst case.
    ///
    /// Startup is inside the budget and does not meaningfully eat into it: class initialisation
    /// is lazy, so very little runs before `Main` and a trivial guest reaches it in 3,299 steps.
    /// That is 0.02% of this bound, and ~550x smaller than the heaviest guest's `Main` above —
    /// so sharing one budget across both phases costs the heaviest guest nothing.
    ///
    /// The other half of the trade is how long a genuinely stuck guest takes to be caught.
    /// Measured on an idle machine, the interpreter retires roughly 90k steps/second, so this
    /// bound trips after about three or four minutes; under the suite's own parallelism it
    /// will be slower, but it is bounded, which is the entire point.
    ///
    /// If a legitimate case ever approaches this, raise it — the cost of a bound that is too
    /// low is a confusing failure on real work. Do not lower it to make a stuck test fail
    /// faster; that trades a rare, clear failure for a common, confusing one.
    let defaultMaxSteps : int64 = 20_000_000L

    /// What each thread that has not terminated is doing, for a diagnostic.
    ///
    /// The IL offset is included, not just the method name, because the name alone does not say
    /// *where* a guest is stuck: a spin loop never leaves its method, so every stop inside it
    /// looks identical. The offset and the kernel's step counter are also the only parts of
    /// this message that two differently-timed runs could disagree about, which is what makes
    /// the stopping point observable to a test at all.
    let private threadSummary (state : IlMachineState) : string =
        state.ThreadState
        |> Map.toSeq
        |> Seq.filter (fun (_, ts) -> ts.Status <> ThreadStatus.Terminated)
        |> Seq.map (fun (ThreadId i, ts) ->
            // A non-terminated thread need not have a frame: `ThreadStatus.hasNoActiveFrame`
            // names `NotStarted` and `Parked`, and `ts.MethodState` throws on both because it
            // resolves the active frame and there isn't one. A guest holding a constructed but
            // unstarted `Thread` is entirely ordinary, so reaching for the method
            // unconditionally would replace this diagnostic with "Frame ... is not live" — a
            // failure about the harness, precisely when the guest is what needs explaining.
            if ThreadStatus.hasNoActiveFrame ts.Status then
                $"thread %d{i} (%O{ts.Status})"
            else
                $"thread %d{i} (%O{ts.Status}) in %s{ts.MethodState.ExecutingMethod.Name} at IL offset %d{ts.MethodState.IlOpIndex}"
        )
        |> String.concat "; "

    let runWith
        (loggerFactory : ILoggerFactory)
        (maxSteps : int64)
        (description : string)
        (originalPath : string option)
        (fileStream : Stream)
        (hostConfig : HostConfig)
        : RunOutcome
        =
        let logger = loggerFactory.CreateLogger "BoundedRun"

        /// Pumping `Main`, once startup has installed it.
        let rec goMain (steps : int64) (prepared : Program.PreparedProgram) : RunOutcome =
            if steps >= maxSteps then
                failwith
                    $"%s{description} did not terminate within %d{maxSteps} interpreted steps, so the harness gave up (kernel step counter %d{prepared.State.Kernel.StepCounter}). This is what a livelocked guest looks like: every thread is runnable but nothing progresses. Threads: %s{threadSummary prepared.State}"
            else

            match Program.stepPrepared loggerFactory logger prepared with
            | Program.ProgramStepOutcome.Completed outcome -> outcome
            | Program.ProgramStepOutcome.Deadlocked (prepared, stuck) ->
                // `Program.run` would raise from inside `pumpPrepared`, discarding the state
                // along with any diagnostic it carries. Reported here instead, with the same
                // detail as a step-budget failure so the two read alike.
                failwith
                    $"%s{description} deadlocked: no runnable threads and the entry thread has not terminated. Stuck: %s{stuck}. Threads: %s{threadSummary prepared.State}"
            | Program.ProgramStepOutcome.InstructionStepped (prepared, _, _, _)
            | Program.ProgramStepOutcome.WorkerTerminated (prepared, _) -> goMain (steps + 1L) prepared

        /// Pumping startup, which runs guest code — the AppContext seed and then class
        /// initialisers, the entry type's own among them.
        ///
        /// Driven step by step rather than through `Program.prepare`, which pumps both phases
        /// internally and returns only once they are done. A static initialiser that never
        /// returns wedges startup exactly as a `Main` that never returns wedges the run, and
        /// behind `prepare` that would hang the suite with no diagnostic; here it is bounded and
        /// reported like any other stuck guest.
        let rec goStartup (steps : int64) (startup : Program.Startup) : RunOutcome =
            if steps >= maxSteps then
                failwith
                    $"%s{description} did not finish starting up within %d{maxSteps} interpreted steps, so the harness gave up (kernel step counter %d{startup.State.Kernel.StepCounter}). Guest code runs before Main — the entry type's static initialiser among it — so this is a guest that wedged before Main was ever installed. Threads: %s{threadSummary startup.State}"
            else

            match Program.stepStartup loggerFactory logger startup with
            | Program.StartupStepOutcome.Completed (Program.ProgramStartResult.CompletedBeforeMain outcome) -> outcome
            // The budget is shared, not per-phase: `Main` resumes the count startup left off at,
            // so the bound is a statement about the whole run. Startup is cheap enough for that
            // to cost nothing — lazy class initialisation means a trivial guest reaches `Main`
            // in a little over 3,000 steps — and a single budget is one number for a caller to
            // reason about instead of two.
            //
            // `steps + 1L`, not `steps`: reaching this outcome means `stepStartup` called
            // `stepPrepared`, which retired the startup frame's final `ret` and bumped the
            // kernel's counter like any other step. Handing `steps` on would drop that tick, so
            // the harness would allow `maxSteps + 1` steps and report a kernel counter one above
            // the budget it claims to have enforced.
            | Program.StartupStepOutcome.Completed (Program.ProgramStartResult.Ready prepared) ->
                goMain (steps + 1L) prepared
            | Program.StartupStepOutcome.Deadlocked (startup, stuck) ->
                // `Program.prepare` raises on this itself, but with no guest identification and
                // no thread summary. Reported here in the same shape as the other three
                // failures so all four read alike.
                failwith
                    $"%s{description} deadlocked during startup: no runnable threads and startup has not finished. Stuck: %s{stuck}. Threads: %s{threadSummary startup.State}"
            // Every remaining outcome yields a new `Startup`, and each costs a step. Counting
            // `PhaseAdvanced` — which retires no guest instruction — is deliberate: it makes the
            // loop bounded by `maxSteps` iterations whatever sequence of outcomes occurs, rather
            // than only when the outcomes are the ones we expect.
            | Program.StartupStepOutcome.Stepped (startup, _, _, _)
            | Program.StartupStepOutcome.WorkerTerminated (startup, _)
            | Program.StartupStepOutcome.PhaseAdvanced startup -> goStartup (steps + 1L) startup

        goStartup 0L (Program.beginStartup loggerFactory originalPath fileStream hostConfig)

    /// `runWith` at `defaultMaxSteps`.
    let run
        (loggerFactory : ILoggerFactory)
        (description : string)
        (originalPath : string option)
        (fileStream : Stream)
        (hostConfig : HostConfig)
        : RunOutcome
        =
        runWith loggerFactory defaultMaxSteps description originalPath fileStream hostConfig

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

/// Whether a case's answer is checked against the same guest run on the real .NET
/// runtime, and on which hosts.
///
/// The oracle runs the guest on the *host's* kernel and the host's shared framework.
/// PawPrint, meanwhile, runs it against whatever kernel `KernelConfig.UnixPlatform`
/// says to impersonate — Linux by default, whatever the host. So the two runtimes are
/// only comparing like with like when the guest's claims are ones that hold on both
/// kernels at once, and a case has to say which of those situations it is in.
type OraclePolicy =
    /// Compare on every host. Sound only for a guest whose claims hold under every
    /// flavour PawPrint models: this is the standing rule for `sourcesPure`, whose
    /// cases run against a Linux-impersonating PawPrint and a macOS oracle on a dev
    /// box and against two Linuxes on CI, and must pass both ways.
    | Always
    /// Compare only on a host whose own kernel is the flavour this case's
    /// `KernelConfig` impersonates; assert PawPrint's exit code alone elsewhere.
    ///
    /// This is for a guest that describes one kernel exactly — the `*Linux.cs` and
    /// `*Darwin.cs` pairs in `sourcesImpure`. Such a guest cannot be `Always`, because
    /// on the other host the real runtime answers for a kernel the guest is not
    /// talking about; but on a matching host it is as good a differential case as
    /// anything in `sourcesPure`, and CI runs Linux.
    ///
    /// Matching flavours makes the comparison *possible*, not automatically valid: the
    /// emulated kernel and the host kernel still disagree about the release string, the
    /// processor count, the clock, the filesystem type under any path, directory
    /// enumeration order, and uid/gid. A guest that can observe any of those must stay
    /// `Never`, whatever host it is run on.
    | WhenHostMatchesEmulatedFlavour
    /// Never compared: the case asserts something only PawPrint can be asked, or
    /// something the real runtime would answer differently for a reason that is not a
    /// PawPrint bug.
    | Never

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
        /// When this case's answer is checked against the same guest run on the
        /// real .NET runtime, rather than only against `ExpectedReturnCode`.
        Oracle : OraclePolicy
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

[<RequireQualifiedAccess>]
module OraclePolicy =

    /// Whether a case with this policy, impersonating this kernel, is compared against
    /// the real runtime on a host of this flavour. `None` is a host whose kernel
    /// PawPrint does not model at all, which never matches.
    let comparesOnHost
        (hostFlavour : SimulatedUnixFlavour option)
        (impersonated : SimulatedUnixFlavour)
        (policy : OraclePolicy)
        : bool
        =
        match policy with
        | OraclePolicy.Always -> true
        | OraclePolicy.Never -> false
        | OraclePolicy.WhenHostMatchesEmulatedFlavour ->
            match hostFlavour with
            | None -> false
            | Some host -> host = impersonated

    /// Whether a host of this width and byte order can stand in for a kernel PawPrint
    /// impersonates at all.
    ///
    /// Both presets describe a 64-bit little-endian kernel, and `SimulatedUnixPlatform`
    /// carries no architecture to check against the host's. The guests that opt into a
    /// comparison read native-width layouts back as bytes -- a `sockaddr_in`'s fields,
    /// the 16-byte `SocketEvent` -- so on a 32-bit or big-endian Linux the two runtimes
    /// would disagree for a reason that is not PawPrint's, and the failure would read
    /// as an interpreter bug. Declining to compare there costs only the oracle.
    let hostShapeCanCompare (isLittleEndian : bool) (pointerSizeBytes : int) : bool =
        isLittleEndian && pointerSizeBytes = 8

    /// `comparesOnHost` asked of the host this test process is running on, for the
    /// kernel the case impersonates.
    ///
    /// A host whose shape the presets do not describe counts as no host at all, which
    /// leaves `Always` alone -- `sourcesPure`'s rule that its claims hold everywhere is
    /// not this policy's to narrow.
    let comparesHere (case : EndToEndTestCase) : bool =
        let host =
            if hostShapeCanCompare System.BitConverter.IsLittleEndian System.IntPtr.Size then
                HostPlatform.flavour ()
            else
                None

        comparesOnHost host (SimulatedUnixPlatform.flavour case.KernelConfig.UnixPlatform) case.Oracle

/// `Program.run`, bounded so a guest that never terminates fails its test instead of wedging
/// the whole suite.
///
/// The bound covers the *whole* run, startup included. That matters because guest code runs
/// before `Main` — the AppContext seed, then class initialisers, the entry type's among them —
/// so a static initialiser that never returns is just as capable of hanging the suite as a
/// `Main` that never returns, and is harder to diagnose when it does.
///
/// The bound is a **step count, not a wall-clock timeout**. A timeout
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
    /// Chosen from measurement. Across the whole end-to-end corpus (596 guests at
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
    /// `GuestLocation.describe` is the same renderer the interpreter uses for its own deadlock
    /// reports, so the four failures below and a deadlock reported by the App read alike. It
    /// includes the IL offset and not just the method name, because the name alone does not say
    /// *where* a guest is stuck: a spin loop never leaves its method, so every stop inside it
    /// looks identical. The offset and the kernel's step counter are also the only parts of
    /// this message that two differently-timed runs could disagree about, which is what makes
    /// the stopping point observable to a test at all.
    let private threadSummary : IlMachineState -> string = GuestLocation.describe

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
            | Program.ProgramStepOutcome.Deadlocked (_, stuck) ->
                // `Program.run` would raise from inside `pumpPrepared`, discarding the state
                // along with any diagnostic it carries. Reported here instead, with the same
                // detail as a step-budget failure so the two read alike.
                //
                // `stuck` is already `threadSummary` of the state at the moment of detection —
                // the interpreter builds it with the same renderer — so re-summarising the
                // returned state here would print the same thing twice in two formats.
                failwith
                    $"%s{description} deadlocked: no runnable threads and the entry thread has not terminated. Threads: %s{stuck}"
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
            | Program.StartupStepOutcome.Deadlocked (_, stuck) ->
                // `Program.prepare` raises on this itself, but with no guest identification.
                // Reported here in the same shape as the other three failures so all four read
                // alike. As above, `stuck` is already the thread summary.
                failwith
                    $"%s{description} deadlocked during startup: no runnable threads and startup has not finished. Threads: %s{stuck}"
            // Every remaining outcome yields a new `Startup`, and each costs a step. Counting
            // `PhaseAdvanced` — which retires no guest instruction — makes the
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

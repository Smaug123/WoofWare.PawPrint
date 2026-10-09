namespace WoofWare.PawPrint

open System
open System.Collections.Immutable
open System.IO
open System.Reflection.Metadata
open Microsoft.Extensions.Logging
open WoofWare.PosixKernel

[<RequireQualifiedAccess>]
module Program =
    /// A program ready to run `Main`, or running it: an opaque handle on the driver
    /// (`MultiProgram`) of the one program, which owns the machine the program's process runs
    /// on. `stepPrepared` steps it.
    [<Struct>]
    type PreparedProgram =
        internal
            {
                Driver : MultiProgram
            }

        /// The program's interpreter state as it currently stands.
        member this.State : IlMachineState = this.Driver.Current.State

        /// The base class types of the CoreLib the program runs against.
        member this.BaseClassTypes : BaseClassTypes<DumpedAssembly> =
            this.Driver.Current.BaseClassTypes

        /// The thread `Main` runs on.
        member this.EntryThread : ThreadId = this.Driver.Current.EntryThread

        /// The thread that retired the program's most recent step, which the scheduler's
        /// round-robin choice starts from.
        member this.LastRan : ThreadId = this.Driver.Current.LastRan

        /// The program with `state` in place of its interpreter state. `state`'s view of the
        /// machine must descend from `this.State`'s, as any state a step or a syscall made from
        /// it does: the driver writes it back into the machine the program runs on, and fails
        /// loudly at the program's end if it cannot.
        member this.WithState (state : IlMachineState) : PreparedProgram =
            {
                Driver =
                    MultiProgram.withCurrent
                        { this.Driver.Current with
                            State = state
                        }
                        this.Driver
            }

    type ProgramStartResult =
        | Ready of PreparedProgram
        | CompletedBeforeMain of RunEnd

    type ProgramStepOutcome =
        /// `effect` is the step's `StepEffect`, forwarded verbatim from
        /// `ExecutionResult.Stepped`. It is what makes a *streaming* driver
        /// possible: `StepEffect.WroteToFd` carries exactly the bytes this step
        /// appended to `EmulatedKernel.OutputLog`, so a driver can write them to
        /// a real stream as they are produced instead of waiting for a
        /// `RunOutcome` and draining the log. A run that never produces a
        /// `RunOutcome` (a livelocked guest, a guest killed from outside,
        /// `Deadlocked`) has no end-of-run drain to reach, so without streaming
        /// its output is lost entirely.
        ///
        /// Steps that terminate the run do not carry an effect: those outcomes
        /// are `Completed`, and their `RunOutcome` carries the final state whose
        /// `OutputLog` is authoritative. A driver that streams should still drain
        /// any log entries beyond what it has written when the run ends, because
        /// writes performed *before* the driver's own loop starts (a `.cctor`
        /// that prints, pumped inside `prepare`) never pass through here.
        | InstructionStepped of PreparedProgram * ranThread : ThreadId * whatWeDid : WhatWeDid * effect : StepEffect
        | WorkerTerminated of PreparedProgram * terminatingThread : ThreadId
        | Completed of RunOutcome
        | Deadlocked of PreparedProgram * stuckThreads : string

    /// Startup in progress: an opaque handle on the driver (`MultiProgram`) of the one program,
    /// as a `PreparedProgram` is, whose entry thread is running one of the calls startup pumps
    /// before `Main` (`StartupPhase`). The same scheduler tick drives startup as drives `Main`.
    ///
    /// This exists so a driver can *step* startup rather than having it run to completion
    /// behind a single call. Guest code runs here — a static initialiser may print, block, or
    /// wedge — and a driver that cannot see those steps cannot stream their output or report
    /// where startup got stuck.
    type Startup =
        private
            {
                Driver : MultiProgram
            }

        /// The machine state as it currently stands. A driver streaming guest output reads
        /// `Kernel.OutputLog` from here when startup ends without a `ProgramStartResult`.
        member this.State : IlMachineState = this.Driver.Current.State

    /// The result of stepping startup once. Mirrors `ProgramStepOutcome`, and for the same
    /// reason carries the step's `StepEffect`: a driver consumes it to stream guest writes as
    /// they happen, which is the whole point of startup being steppable.
    [<RequireQualifiedAccess>]
    type StartupStepOutcome =
        | Stepped of Startup * ranThread : ThreadId * whatWeDid : WhatWeDid * effect : StepEffect
        | WorkerTerminated of Startup * terminatingThread : ThreadId
        /// The entry thread's frame returned and startup moved to its next phase. No guest
        /// instruction retired, so there is no effect to report.
        | PhaseAdvanced of Startup
        | Completed of ProgramStartResult
        | Deadlocked of Startup * stuckThreads : string

    /// How the one program of a driver of one program ended, of the ends the driver finished
    /// with.
    let private onlyEnd (ends : (ProcessId * RunEnd) list) : RunEnd =
        match ends with
        | [ _, runEnd ] -> runEnd
        | _ ->
            failwith
                $"Program: a driver of one program finished with %d{List.length ends} programs' ends (this is a bug in PawPrint)."

    /// `onlyEnd`'s outcome.
    let private onlyOutcome (ends : (ProcessId * RunEnd) list) : RunOutcome =
        match onlyEnd ends with
        | RunEnd.Ended outcome -> outcome

    /// Where the one program of a driver of one program is stuck, of the deadlock's report.
    let private onlyStuck (stuck : (ProcessId * string) list) : string =
        match stuck with
        | [ _, threads ] -> threads
        | _ ->
            failwith
                $"Program: a driver of one program deadlocked naming %d{List.length stuck} programs (this is a bug in PawPrint)."

    /// Advance the machine by one scheduler tick (`MultiProgram.step`).
    ///
    /// What the entry thread's bottom frame returning means depends on what it is running
    /// (`EntryFrameKind`):
    ///   * `StartupCall`: the pumped call is done, which ends a phase of startup rather than the
    ///     run, whatever the other threads are doing, and the program moves to its next phase
    ///     (`RunningProgram.stepDecided`). `stepStartup` steps a program still starting up; this
    ///     fails loudly.
    ///   * `Main`: an `int Main`'s return value is latched as the exit code, the entry thread
    ///     goes to `WaitingForForegroundThreads` and becomes a background thread, as
    ///     `WaitForOtherThreads` makes it, and the run goes on until shutdown has been
    ///     signalled — the first tick, `Main`'s own included, at which no foreground thread was
    ///     alive — at which point it reports `NormalExit`; the exit code is whatever
    ///     `IlMachineState.LatchedExitCode` holds by then, which a worker may have rewritten
    ///     through `Environment.ExitCode`. A worker's `Environment.Exit` in the meantime is a
    ///     `ProcessExit` like any other. If the foreground threads that remain can make no
    ///     progress, the tick reports `Deadlocked`: real .NET would hang there.
    let stepPrepared
        (loggerFactory : ILoggerFactory)
        (logger : ILogger)
        (prepared : PreparedProgram)
        : ProgramStepOutcome
        =
        match MultiProgram.step loggerFactory logger prepared.Driver with
        | MultiStepOutcome.Stepped (driver, _, ProgramEvent.InstructionStepped (ranThread, whatWeDid, effect)) ->
            ProgramStepOutcome.InstructionStepped (
                {
                    Driver = driver
                },
                ranThread,
                whatWeDid,
                effect
            )
        | MultiStepOutcome.Stepped (driver, _, ProgramEvent.WorkerTerminated terminatingThread) ->
            ProgramStepOutcome.WorkerTerminated (
                {
                    Driver = driver
                },
                terminatingThread
            )
        // The machine the process's end leaves holds no process, and a run of one program has
        // nothing more to do with it.
        | MultiStepOutcome.Finished (ends, _) -> ProgramStepOutcome.Completed (onlyOutcome ends)
        | MultiStepOutcome.Deadlocked (driver, stuck) ->
            ProgramStepOutcome.Deadlocked (
                {
                    Driver = driver
                },
                onlyStuck stuck
            )
        | MultiStepOutcome.Stepped (_, _, ProgramEvent.PhaseAdvanced) ->
            failwith
                "Program.stepPrepared: the entry thread's startup call returned, which ends a phase of startup; a program still starting up is stepped by stepStartup"
        | MultiStepOutcome.Stepped (_, pid, ProgramEvent.Ended _) ->
            failwith
                $"Program.stepPrepared: process %O{pid} ended and the driver ran on, but a driver of one program finishes when its program ends (this is a bug in PawPrint)."

    let rec pumpPrepared (loggerFactory : ILoggerFactory) (logger : ILogger) (prepared : PreparedProgram) : RunEnd =
        match stepPrepared loggerFactory logger prepared with
        | ProgramStepOutcome.Completed outcome -> RunEnd.Ended outcome
        | ProgramStepOutcome.Deadlocked (_, stuck) ->
            failwith $"Deadlock: no runnable threads and the process has not exited. Stuck: {stuck}"
        | ProgramStepOutcome.InstructionStepped (prepared, _, _, _)
        | ProgramStepOutcome.WorkerTerminated (prepared, _) -> pumpPrepared loggerFactory logger prepared

    /// Reads the guest assembly and performs the one-time setup needed before Main is ready to schedule.
    ///
    /// `hostConfig.Guest.Kernel` carries the host's choices for the simulated process's kernel and
    /// is applied here rather than by the caller afterwards, because this function pumps the entry
    /// type's `.cctor` and CoreLib latches some of these values during static initialisation
    /// (notably `Environment.ProcessorCount`). `KernelConfig.Default` is the no-preference
    /// choice. Its `Environment` follows whichever `EmulatedKernel.defaultEnvironment` entries it
    /// does not name (see `EmulatedKernel.withEnvironment`), so callers that supply no environment
    /// still get the seeded `DOTNET_SYSTEM_GLOBALIZATION_INVARIANT=1` default, and a caller that
    /// names that variable replaces it — that's how the CLI lets the host process override the
    /// seed if it really needs to.
    ///
    /// `hostConfig.PctSeed = Some s` selects the PCT scheduling policy seeded with `s`; `None` keeps the
    /// default round-robin policy. Applied before any cctor frame is pushed so the very first
    /// `chooseNext` decision is policy-correct — `IlMachineState.initial` defaults the field
    /// to `RoundRobin`, and `withPctSeed` simply overwrites it.
    ///
    /// Raises `UnsupportedRuntimeException`, before any guest code runs, if the CoreLib the guest
    /// resolves along `hostConfig.Guest.DotnetRuntimeDirs` has an `AssemblyVersion` major that is
    /// not in `EmulatedRuntime.supported`. The run then serves the runtime `EmulatedRuntime.ofCoreLib`
    /// reads from that CoreLib.
    let beginStartup
        (loggerFactory : ILoggerFactory)
        (originalPath : string option)
        (fileStream : Stream)
        (hostConfig : HostConfig)
        : Startup
        =
        let entry = ProgramStartup.read loggerFactory originalPath fileStream
        let machineConfig, processConfig = KernelConfig.split hostConfig.Guest.Kernel

        let launch : ProgramLaunch =
            {
                Image = fileStream
                OriginalPath = originalPath
                DotnetRuntimeDirs = hostConfig.Guest.DotnetRuntimeDirs
                Process = processConfig
                Argv = hostConfig.Guest.Argv
                AssemblyPath = hostConfig.Guest.AssemblyPath
                AppContext = hostConfig.Guest.AppContext
                PctSeed = hostConfig.PctSeed
            }

        {
            Driver =
                MultiProgram.launchAll
                    loggerFactory
                    "KernelConfig"
                    (fun _ -> "KernelConfig")
                    machineConfig
                    [ launch, entry ]
        }

    /// Advance startup by one guest instruction, crossing a phase boundary when the entry
    /// thread's current frame returns.
    let stepStartup (loggerFactory : ILoggerFactory) (logger : ILogger) (startup : Startup) : StartupStepOutcome =
        match MultiProgram.step loggerFactory logger startup.Driver with
        | MultiStepOutcome.Stepped (driver, _, ProgramEvent.PhaseAdvanced) ->
            match driver.Current.EntryFrame with
            | EntryFrameKind.Main _ ->
                StartupStepOutcome.Completed (
                    ProgramStartResult.Ready
                        {
                            Driver = driver
                        }
                )
            | EntryFrameKind.StartupCall _ ->
                StartupStepOutcome.PhaseAdvanced
                    {
                        Driver = driver
                    }
        | MultiStepOutcome.Stepped (driver, _, ProgramEvent.InstructionStepped (ran, whatWeDid, effect)) ->
            StartupStepOutcome.Stepped (
                {
                    Driver = driver
                },
                ran,
                whatWeDid,
                effect
            )
        | MultiStepOutcome.Stepped (driver, _, ProgramEvent.WorkerTerminated terminated) ->
            StartupStepOutcome.WorkerTerminated (
                {
                    Driver = driver
                },
                terminated
            )
        | MultiStepOutcome.Deadlocked (driver, stuck) ->
            StartupStepOutcome.Deadlocked (
                {
                    Driver = driver
                },
                onlyStuck stuck
            )
        // The process ended before `Main` was installed, in the class-initialisation phase: the
        // driver fails loudly on an end in an earlier phase (`RunningProgram.runEnd`). The CLR
        // tears the process down, and the guest-level diagnostic is the run's end, rather than a
        // host `failwith` that would mask it.
        | MultiStepOutcome.Finished (ends, _) ->
            StartupStepOutcome.Completed (ProgramStartResult.CompletedBeforeMain (onlyEnd ends))
        | MultiStepOutcome.Stepped (_, pid, ProgramEvent.Ended _) ->
            failwith
                $"Program.stepStartup: process %O{pid} ended and the driver ran on, but a driver of one program finishes when its program ends (this is a bug in PawPrint)."

    /// Reads the guest assembly and performs the one-time setup needed before Main is ready to
    /// schedule, running startup to completion.
    ///
    /// This is `beginStartup` driven by `stepStartup` in a loop. A driver that wants to observe
    /// startup — to stream a static initialiser's output, or to report where startup wedged
    /// rather than throwing out of it — should drive those two directly instead; guest code
    /// runs during startup, and this function gives back nothing until all of it has finished.
    ///
    /// See `beginStartup` for the kernel-config and PCT-seed timing contracts.
    let prepare
        (loggerFactory : ILoggerFactory)
        (originalPath : string option)
        (fileStream : Stream)
        (hostConfig : HostConfig)
        : ProgramStartResult
        =
        let logger = loggerFactory.CreateLogger "Program"

        let rec go (startup : Startup) : ProgramStartResult =
            match stepStartup loggerFactory logger startup with
            | StartupStepOutcome.Completed result -> result
            | StartupStepOutcome.Stepped (startup, _, _, _)
            | StartupStepOutcome.WorkerTerminated (startup, _)
            | StartupStepOutcome.PhaseAdvanced startup -> go startup
            | StartupStepOutcome.Deadlocked (_, stuck) ->
                failwith $"Deadlock during startup: no runnable threads and startup has not finished. Stuck: {stuck}"

        go (beginStartup loggerFactory originalPath fileStream hostConfig)

    /// Returns the outcome of the program run: normal exit or unhandled guest exception.
    ///
    /// `hostConfig.PctSeed` flows through to `prepare`: `Some s` selects PCT with seed `s`,
    /// `None` keeps the default round-robin scheduler. See `prepare` for the
    /// timing contract (applied before the first cctor frame is pushed).
    let run
        (loggerFactory : ILoggerFactory)
        (originalPath : string option)
        (fileStream : Stream)
        (hostConfig : HostConfig)
        : RunEnd
        =
        let logger = loggerFactory.CreateLogger "Program"

        match prepare loggerFactory originalPath fileStream hostConfig with
        | ProgramStartResult.CompletedBeforeMain runEnd -> runEnd
        | ProgramStartResult.Ready prepared -> pumpPrepared loggerFactory logger prepared

    /// A machine state sitting at a scheduler tick *boundary* whose next decision is contended:
    /// once this tick's preamble has run, more than one thread is Runnable, so which of them runs
    /// is a genuine choice — and it is the first such choice since this snapshot's run began.
    ///
    /// Contention is a property of the state the *policy*
    /// sees, which is not the state held here: a deadline expiring or the signal dispatcher waking
    /// can make a second thread Runnable inside the tick. So `State` may well show only one
    /// Runnable thread, and `Contenders` may name a thread that is blocked in it. Guests reaching
    /// their first fork organically do not show this — there the second thread arrives via the
    /// guest's own `Thread.Start`, which is a retired instruction — but `runToNextFork` from
    /// mid-run does, and a caller inspecting `State` should not expect otherwise.
    ///
    /// Why this is worth having: everything before a fork point is forced, so every scheduling
    /// policy makes the same choices there and — since `Scheduler` only ever mutates policy state
    /// at a contended decision — the policy state is still exactly what it was seeded with. A
    /// harness sweeping many PCT seeds over one guest can therefore compute this prefix *once*,
    /// under `RoundRobin`, and hand each seed a run bit-identical to what it would have produced
    /// from scratch. Measured on the `sourcesConcurrencyBugs` guests, that prefix is 74-94% of a
    /// run's instructions and ~90% of its wall clock.
    ///
    /// The state held is the one from *before* the tick's preamble, not from between the preamble
    /// and the choice: a mid-tick value would be a new kind of resumable
    /// thing, and handing it to the ordinary driver would run the preamble twice — advancing
    /// `StepCounter` twice and shifting the spurious-wakeup schedule. Resuming therefore re-runs
    /// the contended tick's preamble, which is policy-independent (see `MultiProgram.advance`) and
    /// so reproduces it exactly.
    ///
    /// Construct one only through `runToFirstFork` / `runToNextFork`: the representation is
    /// private because the type's whole value is the claim that the prefix behind it was forced,
    /// and a hand-built one would carry that claim without having earned it.
    type ForkSnapshot =
        private
            {
                Prepared : PreparedProgram
                Contending : ThreadId list
            }

        /// The machine as it stands at the fork point.
        member this.State : IlMachineState = this.Prepared.State

        /// The threads whose contention makes this a fork point: at least two, ascending by
        /// `ThreadId`. Runnable *at the decision point* — i.e. after this tick's preamble — which
        /// is not necessarily the same as Runnable in `State`. Ascending order is the order
        /// `PctState.ensurePriorityFor` samples in, so it is part of what makes a seeded
        /// schedule reproducible.
        member this.Contenders : ThreadId list = this.Contending

    /// How far a run got before it first had a scheduling choice to make.
    [<RequireQualifiedAccess>]
    type PrefixOutcome =
        /// Reached a contended decision. Resume with `resumeFork`, once per seed.
        | ForkedAt of ForkSnapshot
        /// The program ran to completion without ever reaching a contended decision. No policy
        /// had a choice anywhere, so this is the outcome under *every* seed, and a sweep is
        /// answered by this one run. (Its state's `Scheduling` is the `RoundRobin` the prefix ran
        /// under, where a from-scratch `Pct s` run would carry `Pct (ofSeed s)`; nothing
        /// guest-visible depends on the difference, but do not compare that field.)
        | NeverForked of RunEnd
        /// Every thread blocked before any choice arose. Like `NeverForked`, seed-independent.
        | DeadlockedBeforeFork of stuckThreads : string
        /// A class initialiser started a thread, so the first contended decision happens during
        /// startup rather than in `Main`.
        ///
        /// Detected and refused rather than snapshotted. Snapshotting it is possible — the
        /// detector finds the exact point — but resuming it means handing the caller a
        /// half-finished `Startup` rather than a `PreparedProgram`, so `resumeFork` would have to
        /// return a two-shape value and every caller would have to drive both phases. No guest in
        /// this repository does it, so refuse loudly rather than build the surface. To lift the
        /// restriction, give `ForkSnapshot` a startup arm — nothing else here has to change.
        ///
        /// Carries the contenders rather than a rendered message, so a caller can decide what to
        /// do about the refusal (report it, fall back to per-seed runs).
        | ForkedDuringStartup of contenders : ThreadId list

    /// Guard against a yield retiring at a tick we classified as forced whose *post*-step state is
    /// contended.
    ///
    /// This is the one way a prefix could be seed-dependent despite every decision being forced.
    /// `Scheduler.onStepOutcome` wakes class-init waiters *before* charging the yield debt, so
    /// `chargeYieldDebt` reads contention against a Runnable set that may have grown since the
    /// choice was made. At such a tick a `Pct` policy would toss its honour coin — and could
    /// decline the yield where `RoundRobin` always honours it, which the guest sees directly in
    /// `Thread.Yield()`'s return value. A prefix containing one is not shareable.
    ///
    /// Unreachable today: a thread parked `BlockedOnClassInit` must have executed a step to get
    /// there, and a `.cctor` can only be `InProgress` on another thread, so two threads have
    /// already run and contention has already occurred. But that is a chain of facts about wake
    /// paths rather than a structural property, so check the conclusion and crash rather than
    /// silently emit a snapshot that does not commute.
    let private checkYieldDidNotStraddle (ran : ThreadId) (whatWeDid : WhatWeDid) (after : IlMachineState) : unit =
        match whatWeDid with
        | WhatWeDid.VoluntaryYield _ ->
            match Scheduler.tryContenders after with
            | None -> ()
            | Some contenders ->
                failwith
                    $"Program: thread %O{ran} yielded at a tick whose scheduling decision was forced, but the state after the step is contended (Runnable: %A{contenders}). Scheduler.chargeYieldDebt reads contention after class-init waiters are woken, so a Pct policy would have drawn here — and could have declined the yield where RoundRobin honours it — which means the prefix up to this point is not seed-independent and must not be shared. See Scheduler.onStepOutcome."
        | WhatWeDid.Executed
        | WhatWeDid.Aborted _
        | WhatWeDid.UnhandledException _
        | WhatWeDid.SuspendedForClassInit
        | WhatWeDid.SuspendedForManagedCall
        | WhatWeDid.BlockedOnClassInit _
        | WhatWeDid.ThrowingTypeInitializationException -> ()

    /// Advance `prepared` until the next contended scheduling decision, returning the machine as
    /// it stood at the start of that tick.
    ///
    /// This is the general primitive: from a fresh `Main` it finds the *first* fork point, and
    /// from a mid-run state it finds the next one, which is what a future schedule-space tree
    /// search descends with. What "resume" means differs between those two — see
    /// `IlMachineState.withPctSeed` — but finding the point does not.
    ///
    /// Each *retired* tick's preamble runs exactly once: the probe consumes it and hands the
    /// advanced state straight to the decision half. The fork tick itself is the exception:
    /// its preamble runs here to answer the probe, and again on every resume.
    let rec runToNextFork
        (loggerFactory : ILoggerFactory)
        (logger : ILogger)
        (prepared : PreparedProgram)
        : PrefixOutcome
        =
        match MultiProgram.annotating prepared.State (fun () -> MultiProgram.advance prepared.Driver) with
        | Advanced.Settled (MultiStepOutcome.Finished (ends, _)) -> PrefixOutcome.NeverForked (onlyEnd ends)
        | Advanced.Settled (MultiStepOutcome.Deadlocked (_, stuck)) ->
            PrefixOutcome.DeadlockedBeforeFork (onlyStuck stuck)
        | Advanced.Settled (MultiStepOutcome.Stepped (_, pid, _)) ->
            failwith
                $"Program.runToNextFork: the preamble of a tick of one program settled the tick with process %O{pid} running on, which only the end of another program does (this is a bug in PawPrint)."
        | Advanced.Decide (advanced, tick) ->

        match Scheduler.tryContenders advanced.Current.State with
        | Some contenders ->
            PrefixOutcome.ForkedAt
                {
                    Prepared = prepared
                    Contending = contenders
                }
        | None ->

        // `decide` annotates the step itself.
        match MultiProgram.decide loggerFactory logger advanced tick with
        | MultiStepOutcome.Stepped (_, _, ProgramEvent.PhaseAdvanced) ->
            failwith
                "Program.runToNextFork: the entry thread's startup call returned, but a fork snapshot is taken only once Main is installed"
        | MultiStepOutcome.Deadlocked _ ->
            failwith
                "Program.runToNextFork: the program deadlocked in the decision half of a tick, which only the preamble reports (this is an interpreter bug)."
        | MultiStepOutcome.Stepped (_, pid, ProgramEvent.Ended _) ->
            failwith
                $"Program.runToNextFork: process %O{pid} ended and the driver ran on, but a driver of one program finishes when its program ends (this is a bug in PawPrint)."
        | MultiStepOutcome.Finished (ends, _) -> PrefixOutcome.NeverForked (onlyEnd ends)
        | MultiStepOutcome.Stepped (next, _, ProgramEvent.WorkerTerminated _) ->
            runToNextFork
                loggerFactory
                logger
                {
                    Driver = next
                }
        | MultiStepOutcome.Stepped (next, _, ProgramEvent.InstructionStepped (ran, whatWeDid, _)) ->
            checkYieldDidNotStraddle ran whatWeDid next.Current.State

            runToNextFork
                loggerFactory
                logger
                {
                    Driver = next
                }

    /// Read the guest assembly and run it — startup and all — up to its first contended
    /// scheduling decision.
    ///
    /// Takes a `GuestConfig` rather than a `HostConfig` precisely so that no seed can be passed:
    /// the prefix is the part of the run every seed shares, and it is computed under the
    /// randomness-free `RoundRobin` policy. `resumeFork` supplies the seed afterwards.
    let runToFirstFork
        (loggerFactory : ILoggerFactory)
        (originalPath : string option)
        (fileStream : Stream)
        (guestConfig : GuestConfig)
        : PrefixOutcome
        =
        let logger = loggerFactory.CreateLogger "Program"

        let hostConfig =
            {
                Guest = guestConfig
                PctSeed = None
            }

        let rec goStartup (startup : Startup) : PrefixOutcome =
            // Probe startup with the same predicate `runToNextFork` uses, so a `.cctor` that
            // starts a thread is reported rather than silently mistaken for a forced prefix. The
            // preamble runs twice per startup tick here, once for the probe and once inside
            // `stepStartup`; that is a handful of map operations against `executeOneStep`, and it
            // is paid once for a whole sweep rather than once per seed.
            let contenders =
                match MultiProgram.annotating startup.State (fun () -> MultiProgram.advance startup.Driver) with
                | Advanced.Decide (probed, _) -> Scheduler.tryContenders probed.Current.State
                // `stepStartup` runs the same preamble, and ends or deadlocks the same way.
                | Advanced.Settled _ -> None

            match contenders with
            | Some contenders -> PrefixOutcome.ForkedDuringStartup contenders
            | None ->

            match stepStartup loggerFactory logger startup with
            | StartupStepOutcome.Completed (ProgramStartResult.Ready prepared) ->
                runToNextFork loggerFactory logger prepared
            | StartupStepOutcome.Completed (ProgramStartResult.CompletedBeforeMain outcome) ->
                PrefixOutcome.NeverForked outcome
            | StartupStepOutcome.Deadlocked (_, stuck) -> PrefixOutcome.DeadlockedBeforeFork stuck
            | StartupStepOutcome.Stepped (startup, ran, whatWeDid, _) ->
                checkYieldDidNotStraddle ran whatWeDid startup.State
                goStartup startup
            | StartupStepOutcome.WorkerTerminated (startup, _)
            | StartupStepOutcome.PhaseAdvanced startup -> goStartup startup

        goStartup (beginStartup loggerFactory originalPath fileStream hostConfig)

    /// Install a scheduling policy on a fork snapshot and hand back an ordinary `PreparedProgram`,
    /// to be driven with `stepPrepared` / `pumpPrepared` like any other.
    ///
    /// For a snapshot from `runToFirstFork`, `pctSeed = Some s` gives a run bit-identical to
    /// `Program.run` with `PctSeed = Some s` over the same image and `GuestConfig`: the prefix was
    /// forced, so the policy state a from-scratch run would hold here is exactly
    /// `PctState.ofSeed s`. See `IlMachineState.withPctSeed`, which spells out why that stops
    /// being true for a mid-run snapshot from `runToNextFork`.
    ///
    /// `None` installs no policy at all — it keeps whatever the snapshot carries. For a
    /// `runToFirstFork` snapshot that is the `RoundRobin` the prefix ran under, so it reproduces
    /// the default run; for a mid-run snapshot it is whatever policy got you there, mid-flight.
    ///
    /// `loggerFactory` rebinds the state's logging sink, which would otherwise still be the
    /// prefix's: every seed resumed from one snapshot would log through the factory the *prefix*
    /// was built with, losing whatever per-run properties the caller attaches. The prefix's own
    /// factory must outlive every resume regardless, because `BaseClassTypes` and the loaded
    /// assemblies were built against it.
    ///
    /// One thing a resumed run does *not* reproduce: `StepEffect`s retired during the prefix. A
    /// driver streaming guest output per step sees only post-fork effects. The final state's
    /// `Kernel.OutputLog` is still complete, because it came through the snapshot.
    let resumeFork
        (loggerFactory : ILoggerFactory)
        (pctSeed : uint64 option)
        (snapshot : ForkSnapshot)
        : PreparedProgram
        =
        let state =
            snapshot.Prepared.State |> IlMachineState.withLoggerFactory loggerFactory

        let state =
            match pctSeed with
            | None -> state
            | Some seed -> IlMachineState.withPctSeed seed state

        snapshot.Prepared.WithState state

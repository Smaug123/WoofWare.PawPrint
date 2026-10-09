namespace WoofWare.PawPrint

open Microsoft.Extensions.Logging
open WoofWare.PosixKernel

/// The driver that runs PawPrint programs, each a process, on one simulated machine: it owns
/// the machine, and runs the phases of a scheduler tick that are the machine's rather than any
/// one program's (`MultiProgram.advance`).
///
/// One program is checked out: `Current`. Its kernel's view of the machine
/// (`EmulatedKernel.System`) is the authoritative one, and `Machine` is the machine that view
/// was focused from, so `SimulatedMachine.unfocus` and `SimulatedMachine.endProcess` accept the
/// view, and whatever descends from it, back into `Machine`. `Machine` itself is therefore stale
/// while a program is checked out, and is brought up to date only when a program ends. A driver
/// of one program never hands the machine over to another, which on every tick would cost a
/// focus and an unfocus.
///
/// The global tick is the checked-out program's `EmulatedKernel.StepCounter`.
///
/// A struct because every tick makes a new one, and three references copied by value cost a
/// one-program run nothing over the program record it would otherwise have held directly.
[<Struct>]
type internal MultiProgram =
    {
        /// The machine `Current`'s view was focused from; stale while `Current` runs.
        Machine : SimulatedMachine<ThreadId, NativeSignalHandler>
        /// The checked-out program.
        Current : RunningProgram
        /// How the machine's clock moves with each tick.
        Clock : MachineClock
    }

/// Where the driver's preamble to a tick leaves it.
[<Struct>]
[<RequireQualifiedAccess>]
type internal Advanced =
    /// A program has a Runnable thread, and the driver is ready for it to step
    /// (`MultiProgram.decide`).
    | Decide of driver : MultiProgram
    /// The checked-out program's process ended between instructions, as `outcome` says, and
    /// `remaining` is the machine without it.
    | Ended of outcome : RunOutcome * remaining : SimulatedMachine<ThreadId, NativeSignalHandler>
    /// No program has a Runnable thread, and no deadline is left to jump the clock to:
    /// `stuck` says where each thread is.
    | Deadlocked of stuckDriver : MultiProgram * stuck : string

/// What one tick of the driver did.
[<Struct>]
[<RequireQualifiedAccess>]
type internal DriverTick =
    /// `ranThread` retired an instruction, whose `StepEffect` is `effect`.
    | InstructionStepped of driver : MultiProgram * ranThread : ThreadId * whatWeDid : WhatWeDid * effect : StepEffect
    /// A worker thread's last frame returned.
    | WorkerTerminated of afterTermination : MultiProgram * terminatingThread : ThreadId
    /// The checked-out program's entry thread returned from an `EntryFrameKind.StartupCall`,
    /// which ends a phase of startup rather than the process.
    | StartupCallReturned of returned : MultiProgram
    /// The checked-out program's process ended, as `outcome` says, and `remaining` is the
    /// machine without it.
    | Ended of outcome : RunOutcome * remaining : SimulatedMachine<ThreadId, NativeSignalHandler>
    /// No program has a Runnable thread, and no deadline is left to jump the clock to.
    | Deadlocked of stuckDriver : MultiProgram * stuck : string

[<RequireQualifiedAccess>]
module internal MultiProgram =
    /// The machine whose only process is the newly booted `system`, and that process's view
    /// of it focused from the machine: the view a driver's first program runs in.
    let ofBooted
        (system : UnixSystem<ThreadId, NativeSignalHandler>)
        : SimulatedMachine<ThreadId, NativeSignalHandler> * UnixSystem<ThreadId, NativeSignalHandler>
        =
        let machine = SimulatedMachine.ofSystem system

        match SimulatedMachine.focus (UnixSystem.processId system) machine with
        | Some view -> machine, view
        | None ->
            failwith
                $"MultiProgram.ofBooted: the machine made of process %O{UnixSystem.processId system} does not hold it (this is a bug in PawPrint)."

    /// A driver of `program`, checked out of `machine`, whose clock moves as `clock` says.
    ///
    /// Fails loudly unless `program`'s view of the machine descends from one focused from
    /// `machine` (see `ofBooted`).
    let create
        (clock : MachineClock)
        (machine : SimulatedMachine<ThreadId, NativeSignalHandler>)
        (program : RunningProgram)
        : MultiProgram
        =
        // `unfocus` checks exactly the claim this driver's `Machine` makes of its program's
        // view: that writing the view back would undo nothing it did not see.
        SimulatedMachine.unfocus program.State.Kernel.System machine
        |> ignore<SimulatedMachine<ThreadId, NativeSignalHandler>>

        {
            Machine = machine
            Current = program
            Clock = clock
        }

    /// The driver with `program` checked out in place of its current program, whose process
    /// it must be: `program`'s view of the machine must descend from the current one's.
    let withCurrent (program : RunningProgram) (driver : MultiProgram) : MultiProgram =
        { driver with
            Current = program
        }

    /// <summary>
    /// Run <paramref name="tick" />, annotating any host failure with where the guest was at
    /// <paramref name="state" />.
    /// </summary>
    /// <remarks>
    /// <para>
    /// PawPrint fails by <c>failwith</c> in some 2,400 places, and almost none of them can name
    /// the guest: the least informative messages of all come from pure helpers inside the opcode
    /// implementations, which have no <c>IlMachineState</c> to consult and should not grow one
    /// just to describe a failure. Annotating at the tick covers all of them at once.
    /// </para>
    /// <para>
    /// A *whole* tick must be inside, not just the instruction. Work that can fail sits on both
    /// sides of it: <c>advance</c> applies spurious wakeups and moves the virtual clock — which
    /// faults at its horizon — before the instruction, and <c>dischargeYieldDebts</c>,
    /// <c>onStepOutcome</c> and <c>onThreadTerminated</c> run after. Those failures are every bit
    /// as guest-provoked: <c>onThreadTerminated</c> refusing a worker that exited still holding a
    /// monitor is a diagnostic *about the guest*, and is far less useful without knowing which
    /// guest code let go of it.
    /// </para>
    /// <para>
    /// Hence the invariant this combinator exists to make checkable: every call to
    /// <c>advance</c> or <c>decide</c> is inside an <c>annotating</c>. There are three such
    /// sites — <c>step</c>, and the two in <c>Program</c>'s fork-prefix sweep
    /// (<c>runToNextFork</c> and <c>runToFirstFork</c>) — and grepping for the two names finds
    /// exactly them. Adding a fourth call site outside a wrapper is the one way to reintroduce
    /// the gap.
    /// </para>
    /// <para>
    /// The state described is the one the tick *started* from. The failure happened partway
    /// through, so there is no consistent later state to report.
    /// </para>
    /// <para>
    /// <c>inline</c> with <c>InlineIfLambda</c> because <c>step</c> is per-tick: taking the body
    /// as a first-class function would allocate an <c>FSharpFunc</c> capturing the logger and the
    /// driver on every interpreted instruction — some 20 million of them in a bounded run —
    /// purely to serve a path that normally never fires. Inlined, the caller keeps the exception
    /// region and nothing else.
    /// </para>
    /// </remarks>
    let inline annotating (state : IlMachineState) ([<InlineIfLambda>] tick : unit -> 'a) : 'a =
        try
            tick ()
        with
        // Already annotated: a nested tick would otherwise repeat the thread summary once per
        // level, and the outermost frame's guest position is the least specific of them.
        | :? GuestFailureException -> reraise ()
        | e ->
            // `TryCreate` is total over both the lookup *and* the message construction, so a
            // failure to annotate reraises the original rather than replacing it.
            match GuestFailureException.TryCreate (e, state) with
            | Some annotated -> raise annotated
            | None -> reraise ()

    /// End the checked-out program's process on the driver's machine, as `ended`, the
    /// kernel's account of how the process ended, says: what a real kernel does at exit, where
    /// another process can see it, closing every descriptor the process held. The answer is
    /// `outcome` and the machine without the process.
    ///
    /// Fails loudly if the machine will not end the process (`ProcessEndRefusal`). What the
    /// process delivered to its standard streams stays readable from `outcome`'s state, which
    /// is the machine as the process left it.
    let private endProgram
        (driver : MultiProgram)
        (outcome : RunOutcome)
        (ended : EndedProcess<ThreadId, NativeSignalHandler>)
        : RunOutcome * SimulatedMachine<ThreadId, NativeSignalHandler>
        =
        match SimulatedMachine.endProcess ended driver.Machine with
        | Error refusal ->
            failwith
                $"MultiProgram: process %O{UnixSystem.processId ended.EndedIn} ended (%O{ended.Termination}), and the machine would not close what it held: %s{ProcessEndRefusal.describe refusal}"
        | Ok (_, remaining) -> outcome, remaining

    /// Wake every thread whose syscall could get further now.
    ///
    /// One sweep for every parking syscall rather than one each, because the
    /// question is the same for all of them, and the kernel answers it:
    /// `SimulatedMachine.wakes` reads each park's wake condition, in its own process's view,
    /// and decides which waiters it wakes, choosing the one waiter of a queue that wakes one at
    /// a time across every process on the machine. So a new parking syscall needs no sweep here
    /// at all.
    ///
    /// Runs every tick beside the deadline fires rather than being pushed by
    /// the syscalls that make conditions true: a sweep asks the same question of
    /// the same state each time, so a new producer cannot forget to wake anyone —
    /// where a push from each producer would fail silently, as a deadlock, on the
    /// first one that did. That matters most for `flock`, whose lock is released
    /// by more than the obvious call: the holder's last `close` drops it too, and
    /// nothing about `close` knows that somebody is waiting.
    ///
    /// A wake is not a promise. Two threads can be woken for one lock and only
    /// one of them get it; the loser re-enters, finds it taken, and parks again
    /// on the record it still holds. Of several threads waiting on one socket
    /// event port, the kernel wakes one per event.
    ///
    /// `state` is the checked-out program's, and `machine` the machine its view was focused
    /// from; the machine as it stands is that view written back, which is asked and then
    /// thrown away.
    let private wakeSyscalls (machine : SimulatedMachine<ThreadId, NativeSignalHandler>) (state : IlMachineState) =
        // Before projecting the kernel, which allocates: this runs on every tick of every
        // workload, and almost none of them ever park. With no thread in `BlockedInSyscall`
        // there is nothing to flip, whatever the dispatcher is doing.
        match Scheduler.syscallWaiters state with
        | [] -> state
        | _ ->

        let view = state.Kernel.System
        let processId = UnixSystem.processId view

        let woken =
            SimulatedMachine.wakes
                (Map.empty |> Map.add processId (Scheduler.asleepInSyscall state))
                (SimulatedMachine.unfocus view machine)
            |> List.map (fun ((wokenProcess, thread), _) ->
                if wokenProcess <> processId then
                    failwith
                        $"MultiProgram: the machine woke thread %O{thread} of process %O{wokenProcess}, but only process %O{processId} has a thread asleep in a syscall (this is a bug in PawPrint)."

                thread
            )

        RunningProgram.wakeFromSyscalls woken state

    /// Jump the clock forward if no thread is Runnable but at least one is parked with a
    /// finite-timeout wait outstanding: advance it to the nearest pending deadline and fire it.
    /// This is what keeps a guest like `WaitOne(50)` against an unsignalled handle from
    /// deadlocking — without the jump, the clock would advance only when there's something to
    /// step, and there is nothing to step. Answers the state, and whether it has a Runnable
    /// thread.
    ///
    /// Loops because a single fire may not make any thread Runnable:
    /// `LowLevelMonitor.fireTimeout` moves a waiter out of `WaitQueue`, but if the monitor is
    /// still owned by a separate thread (which itself may be parked on a *later* deadline), the
    /// waiter becomes `BlockedOnMonitorAcquire` rather than `Runnable`. Stopping after one jump
    /// in that shape would declare deadlock even though the owner's later finite wait can still
    /// resolve and release the monitor. Each iteration either produces a Runnable thread
    /// (terminating the loop) or strictly advances the clock to the next outstanding deadline;
    /// the set of finite-deadline threads is finite and monotonically shrinks (no fire creates
    /// a new deadline), so the loop terminates.
    ///
    /// Each iteration sweeps the syscall waiters as well as firing the runtime-level
    /// deadlines, because a jump to a kernel park's deadline is resolved only by the kernel's
    /// wake: without the sweep that deadline would stay outstanding and be jumped to forever.
    ///
    /// Only the clock is advanced (not `StepCounter`), so the spurious-wakeup schedule is
    /// untouched. A jump-driven wake is not a scheduler tick — it is the resolution of a
    /// timeout that would otherwise be invisible.
    let rec private jumpToDeadlines
        (machine : SimulatedMachine<ThreadId, NativeSignalHandler>)
        (state : IlMachineState)
        : struct (IlMachineState * bool)
        =
        // The policy-independent existence check rather than `chooseNext`: a stochastic policy
        // would otherwise advance its RNG once per deadline-jump probe, perturbing the
        // scheduling stream without ever observing the result.
        if Scheduler.hasAnyRunnable state then
            struct (state, true)
        else
            match RunningProgram.pendingDeadlines state with
            | [] -> struct (state, false)
            | deadlines ->
                let target = List.min deadlines

                let state =
                    // The path that *can* reach the horizon: this jumps the clock straight
                    // to a deadline without retiring a step, so a guest looping on
                    // `Thread.Sleep(Int32.MaxValue)` advances it ~2.1e13 ticks per cheap
                    // iteration. `withVirtualClockTicks` faults here, naming the wait that
                    // ran time off the end, instead of letting the addition wrap and hand
                    // some later sleeper a negative deadline that fires immediately.
                    state.MapKernel (EmulatedKernel.withVirtualClockTicks (max state.Kernel.VirtualClockTicks target))

                let state = RunningProgram.fireExpiredDeadlines state.Kernel.VirtualClockTicks state

                jumpToDeadlines machine (wakeSyscalls machine state)

    /// The first half of a scheduler tick: everything that happens before the policy is asked
    /// which thread runs next. Advancing the clocks, firing wait deadlines, letting the signal
    /// dispatcher wake, waking syscalls, and jumping the virtual clock forward if nothing is
    /// Runnable.
    ///
    /// Of these, the spurious wakeups, the deadline fires and the signal poll are each
    /// program's own; the clock's advance and its jitter, the syscall wakes, the jump to the
    /// earliest deadline and the deadlock are the machine's.
    ///
    /// Split out from `decide` because *this* is the moment at which "how many threads are
    /// Runnable" becomes the answer the policy will act on. Every phase here can create
    /// contention within the tick — a deadline firing, a spurious wake, the dispatcher becoming
    /// Runnable — so a fork detector that probed the inter-tick state instead would miss forks
    /// and hand a schedule-sweeping harness a prefix that is not actually forced. See
    /// `Program.runToNextFork`.
    ///
    /// Deliberately policy-independent: nothing here reads `state.Scheduling`, and a stochastic
    /// policy's RNG is not advanced by a probe. That is what makes it safe to run this, look at
    /// the result, and then run it again from the original driver on a later resume.
    ///
    /// Not idempotent: it advances `StepCounter` and the virtual clock. Callers hold the
    /// *inter-tick* driver if they want to be able to replay the tick.
    ///
    /// Ends the program instead if System.Native's dispatcher, handling a signal between
    /// instructions, re-raises it at a default that kills the process.
    let advance (driver : MultiProgram) : Advanced =
        let program = driver.Current

        // The tick this preamble is running, captured before the counter moves on, so that the
        // spurious wakeups and the clock jitter are keyed on the same number. All three are fuzz
        // dials a caller scripts by tick, and they would be treacherous to script against each
        // other if "tick N" meant a different moment to each.
        let tick = program.State.Kernel.StepCounter

        let state = RunningProgram.applySpuriousWakeups tick program.State

        // Threaded as a state rather than rebuilding the program at each stage below: every
        // stage from here to the return touches only its `State`, and this runs once per
        // interpreted instruction.

        // `MachineClock.InstructionCostTicks` of virtual time per scheduler tick — see that field
        // for the rate and why it is what it is. Bumping in lock-step with `StepCounter` keeps both
        // clocks pure functions of "how many scheduler ticks have elapsed", which is what tests
        // rely on when driving the strategies without a real driver.
        //
        // `retireStep` rather than a record-copy piped through `withVirtualClockTicks`: it applies
        // the same validation — so the horizon is still enforced at the writer, which this path
        // cannot realistically reach (it would take ~9.2e12 retired instructions) but which should
        // not hold only by coincidence — while costing one copy of the kernel record instead of
        // two.
        let state =
            state.WithKernel (EmulatedKernel.retireStep driver.Clock.InstructionCostTicks state.Kernel)

        // Clock jitter: with the configured strategy's blessing, jump the clock onto a deadline
        // some thread is already parked on, so that the timeout fires while other threads still
        // had work left in the window rather than at the instruction count the guest's own
        // arithmetic implies. Off by default, in which case this is one match on a DU case.
        //
        // Applied after the ordinary advance and before the expiry pass below, so a
        // jitter-reached deadline fires in the very same pass as one that came due on its own:
        // the rest of the tick cannot tell the two apart, which is the point.
        //
        // `StepCounter` is deliberately not bumped, exactly as the jump to the deadlines below
        // does not bump it: the jitter schedule and the spurious-wakeup schedules are both keyed
        // on that counter, and a jump is the resolution of a timeout rather than a retired step.
        let state =
            match driver.Clock.ClockJitter with
            // Taken before `pendingDeadlines`, which walks every thread and allocates: F#
            // evaluates arguments eagerly, so passing it to `chooseJump` unconditionally would
            // charge that walk to every tick of every run, in exchange for an answer that is
            // `None` by definition. Only the disabled case is special-cased here — any future
            // variant falls through to the full decision below rather than silently inheriting a
            // fast path meant for "switched off".
            | ClockJitterStrategy.Disabled -> state
            | strategy ->

            match
                ClockJitter.chooseJump
                    strategy
                    tick
                    state.Kernel.VirtualClockTicks
                    (RunningProgram.pendingDeadlines state)
            with
            | None -> state
            // Through the validating setter, which is what faults if a guest's own timeout
            // arithmetic has run the clock off the representable range; `chooseJump` guarantees
            // the target is ahead of the clock, so the monotonicity half of that check cannot
            // fire here.
            | Some target -> state.MapKernel (EmulatedKernel.withVirtualClockTicks target)

        // After advancing the clock, fire any wait deadlines that are now in the past. This runs
        // every tick (not just on deadlock) so a timeout against a thread holding a release lock
        // can still expire while other threads make progress: e.g. thread A is parked with a
        // 50 ms timeout on a semaphore, and thread B is busy computing something else — A's
        // deadline still fires when the clock reaches it, even though B keeps the scheduler from
        // ever stalling.
        let state = RunningProgram.fireExpiredDeadlines state.Kernel.VirtualClockTicks state

        // Run System.Native's signal handling before the scheduler picks its next thread: its
        // native handler writes whatever the kernel delivers to the leader into the signal pipe,
        // and a Parked dispatcher with a signal in the pipe reads it and is flipped to Runnable
        // onto the managed callback, so the scheduler can pick it on the same tick. Before the
        // syscall wakes, because writing to the pipe and reading from it change what a syscall
        // parked on it is waiting for.
        match SignalDispatch.poll program.BaseClassTypes state with
        | SignalPoll.ProcessKilled (state, ended) ->
            let outcome, remaining =
                endProgram driver (RunningProgram.signalTerminated state ended) ended

            Advanced.Ended (outcome, remaining)
        | SignalPoll.Continues state ->

        // Wake anything parked in a syscall whose wake condition now holds — a port that has
        // become deliverable, a lock that has become available. Before the jump to the
        // deadlines, so neither is mistaken for quiescence.
        let state = wakeSyscalls driver.Machine state

        let struct (state, runnable) = jumpToDeadlines driver.Machine state

        let driver =
            { driver with
                Current =
                    { program with
                        State = state
                    }
            }

        if runnable then
            Advanced.Decide driver
        else
            // No thread is Runnable, and no deadline is left to jump to: every remaining
            // thread is blocked, so progress is impossible. The status alone does not locate a
            // guest — every thread blocked on a monitor looks alike — so the report names the
            // frame each live thread is in and, where the guest was built with debug
            // information, the source line it is on.
            Advanced.Deadlocked (driver, GuestLocation.describe state)

    /// The second half of a scheduler tick: the checked-out program, which `advance` left with
    /// a Runnable thread, steps (`RunningProgram.stepDecided`). `driver` must already have been
    /// through `advance`.
    ///
    /// A process that ends is ended on the machine here (`endProgram`).
    let decide (loggerFactory : ILoggerFactory) (logger : ILogger) (driver : MultiProgram) : DriverTick =
        match RunningProgram.stepDecided loggerFactory logger driver.Current with
        | ProgramTick.InstructionStepped (program, ranThread, whatWeDid, effect) ->
            DriverTick.InstructionStepped (withCurrent program driver, ranThread, whatWeDid, effect)
        | ProgramTick.WorkerTerminated (program, terminatingThread) ->
            DriverTick.WorkerTerminated (withCurrent program driver, terminatingThread)
        | ProgramTick.StartupCallReturned state ->
            DriverTick.StartupCallReturned (
                withCurrent
                    { driver.Current with
                        State = state
                    }
                    driver
            )
        | ProgramTick.Ended (outcome, ended) ->
            let outcome, remaining = endProgram driver outcome ended
            DriverTick.Ended (outcome, remaining)

    /// One scheduler tick, inside `annotating`: `decide` after `advance`.
    let step (loggerFactory : ILoggerFactory) (logger : ILogger) (driver : MultiProgram) : DriverTick =
        annotating
            driver.Current.State
            (fun () ->
                match advance driver with
                | Advanced.Decide advanced -> decide loggerFactory logger advanced
                | Advanced.Ended (outcome, remaining) -> DriverTick.Ended (outcome, remaining)
                | Advanced.Deadlocked (stuckDriver, stuck) -> DriverTick.Deadlocked (stuckDriver, stuck)
            )

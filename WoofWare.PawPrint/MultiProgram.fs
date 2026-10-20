namespace WoofWare.PawPrint

open Microsoft.Extensions.Logging
open WoofWare.PosixKernel

/// What a driver (`MultiProgram`) knows of its programs besides the checked-out one.
type internal ProgramRoster =
    {
        /// Every other live program, by its process ID. The machine in each one's kernel is
        /// stale.
        Idle : Map<ProcessId, RunningProgram>
        /// Every program's process ID, in the order the programs were launched, ended ones
        /// included.
        Launched : ProcessId list
        /// How each program that has ended ended.
        Ended : Map<ProcessId, RunEnd>
        /// How the driver chooses which program runs at each tick.
        Choice : ProgramChoice
        /// The program chosen at the last tick, from which `ProgramChoice.RoundRobin`
        /// counts; before the first tick, the last program launched.
        LastChosen : ProcessId
    }

/// The driver that runs PawPrint programs, each a process, on one simulated machine: it owns
/// the machine, and runs the phases of a scheduler tick that are the machine's rather than any
/// one program's (`MultiProgram.advance`). `MultiProgram.start` makes one, and
/// `MultiProgram.step` runs it a tick at a time.
///
/// One program is checked out: `Current`. The machine in its kernel (`EmulatedKernel.System`)
/// is the machine, every process on it included. Every other live program is idle, and the
/// machine in its kernel is a stale copy: its own process there is as the machine holds it,
/// but for what the driver's syscall wakes have changed in its parks, since nothing but its own
/// steps changes the rest of it, while everything else may have moved on. Checking a program
/// out hands it the current machine. A driver of one program never checks out another.
///
/// The global tick is the checked-out program's `EmulatedKernel.StepCounter`; checking out
/// another program carries it across, as the clock is carried across inside the machine.
///
/// A struct because every tick makes a new one, and a few references copied by value cost a
/// one-program run nothing over the program record it would otherwise have held directly.
/// What changes only with several programs is in `Roster`, so that a tick of one program
/// copies three references and allocates nothing for the driver.
[<Struct>]
type MultiProgram =
    internal
        {
            /// The checked-out program, whose kernel holds the machine.
            Current : RunningProgram
            /// How the machine's clock moves with each tick.
            Clock : MachineClock
            /// The programs other than the checked-out one, and how the driver chooses among
            /// them all.
            Roster : ProgramRoster
        }

    /// See `ProgramRoster.Idle`.
    member internal this.Idle : Map<ProcessId, RunningProgram> = this.Roster.Idle
    /// See `ProgramRoster.Launched`.
    member internal this.Launched : ProcessId list = this.Roster.Launched

/// What one program did at a tick of `MultiProgram.step`.
[<Struct>]
[<RequireQualifiedAccess>]
type ProgramEvent =
    /// `thread` retired an instruction, whose `StepEffect` is `effect`: the bytes a write to
    /// a standard stream delivered, for a host that streams the program's output as it is made.
    | InstructionStepped of thread : ThreadId * whatWeDid : WhatWeDid * effect : StepEffect
    /// A worker thread's last frame returned.
    | WorkerTerminated of terminatingThread : ThreadId
    /// A call startup pumped on the entry thread returned, and startup moved to its next
    /// phase, or installed `Main`. No guest instruction retired.
    | PhaseAdvanced
    /// The program's process ended, as `runEnd` says, and the other programs run on.
    | Ended of runEnd : RunEnd

/// What one tick of `MultiProgram.step` did.
[<Struct>]
[<RequireQualifiedAccess>]
type MultiStepOutcome =
    /// The program whose process ID is `program` did what `event` says, and `driver` runs on.
    | Stepped of driver : MultiProgram * program : ProcessId * event : ProgramEvent
    /// The last program's process ended. `ends` is how each program's run ended, in launch
    /// order, and `machine` the machine with no process left on it.
    | Finished of ends : (ProcessId * RunEnd) list * machine : UnixSystem<ThreadId, NativeSignalHandler>
    /// No program has a thread that can run, and no deadline is left on the machine to jump
    /// the clock to: `stuck` says, for each live program in launch order, where each of its
    /// threads is. Real processes would hang here.
    | Deadlocked of stuckDriver : MultiProgram * stuck : (ProcessId * string) list

/// Where the driver's preamble to a tick leaves it.
[<Struct>]
[<RequireQualifiedAccess>]
type internal Advanced =
    /// A program has a Runnable thread, and the driver is ready to choose a program and step it
    /// (`MultiProgram.decide`). `tick` is the global tick the preamble ran.
    | Decide of driver : MultiProgram * tick : int64
    /// The tick ended in its preamble, as `outcome` says: a program's signal poll ended its
    /// process, or no program can run.
    | Settled of outcome : MultiStepOutcome

[<RequireQualifiedAccess>]
module MultiProgram =
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
    /// <c>advance</c> is inside an <c>annotating</c>, and <c>decide</c> annotates the program it
    /// chooses itself. There are three call sites of <c>advance</c> — <c>step</c>, and the two in
    /// <c>Program</c>'s fork-prefix sweep (<c>runToNextFork</c> and <c>runToFirstFork</c>) — and
    /// grepping for the name finds exactly them. Adding a fourth call site outside a wrapper is
    /// the one way to reintroduce the gap. A failure in the preamble is annotated with the
    /// program checked out when the tick began, and a failure in a step with the program that
    /// stepped.
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
    let inline internal annotating (state : IlMachineState) ([<InlineIfLambda>] tick : unit -> 'a) : 'a =
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

    let private processIdOf (program : RunningProgram) : ProcessId = program.State.Kernel.ProcessId

    let private withState (state : IlMachineState) (program : RunningProgram) : RunningProgram =
        { program with
            State = state
        }

    /// The driver with `program` checked out in place of its current program, whose process
    /// it must be: the machine in `program`'s kernel must descend from the current one's.
    let internal withCurrent (program : RunningProgram) (driver : MultiProgram) : MultiProgram =
        { driver with
            Current = program
        }

    /// The driver with `idle` as its idle programs. Answers `driver` itself, allocating nothing,
    /// when `idle` is the driver's own, which it is on every tick of a driver of one program.
    let private withIdle (idle : Map<ProcessId, RunningProgram>) (driver : MultiProgram) : MultiProgram =
        if obj.ReferenceEquals (idle, driver.Roster.Idle) then
            driver
        else
            { driver with
                Roster =
                    { driver.Roster with
                        Idle = idle
                    }
            }

    /// `f` of each idle program. Answers `idle` itself, allocating nothing, when there is none,
    /// which is every tick of a driver of one program.
    let inline private mapIdle
        ([<InlineIfLambda>] f : RunningProgram -> RunningProgram)
        (idle : Map<ProcessId, RunningProgram>)
        : Map<ProcessId, RunningProgram>
        =
        if idle.IsEmpty then
            idle
        else
            Map.map (fun _ program -> f program) idle

    /// Every live program in launch order, the checked-out one included, with its process ID.
    let private live (driver : MultiProgram) : (ProcessId * RunningProgram) list =
        let current = processIdOf driver.Current

        driver.Launched
        |> List.choose (fun pid ->
            if pid = current then
                Some (pid, driver.Current)
            else
                Map.tryFind pid driver.Idle |> Option.map (fun program -> pid, program)
        )

    /// `program`, idle, with `machine` in place of the stale copy in its kernel, and the global
    /// tick `stepCounter`.
    ///
    /// Fails loudly unless `machine` holds `program`'s process.
    let private handOver
        (stepCounter : int64)
        (machine : UnixSystem<ThreadId, NativeSignalHandler>)
        (program : RunningProgram)
        : RunningProgram
        =
        let pid = processIdOf program

        if not (Set.contains pid (UnixSystem.processIds machine)) then
            failwith
                $"MultiProgram: the machine does not hold process %O{pid}, whose program is idle (this is a bug in PawPrint)."

        program
        |> withState (
            program.State.WithKernel
                { EmulatedKernel.withUnix machine program.State.Kernel with
                    StepCounter = stepCounter
                }
        )

    /// The driver with the idle program `pid` checked out in place of the current one, which
    /// goes idle: the current program's machine is handed to `pid`'s, with the global tick.
    let private checkOut (pid : ProcessId) (driver : MultiProgram) : MultiProgram =
        let current = driver.Current

        let next =
            match Map.tryFind pid driver.Idle with
            | Some next -> next
            | None ->
                failwith
                    $"MultiProgram: asked to check out process %O{pid}, which is not an idle program (this is a bug in PawPrint)."

        { driver with
            Current = handOver current.State.Kernel.StepCounter current.State.Kernel.System next
        }
        |> withIdle (driver.Idle |> Map.remove pid |> Map.add (processIdOf current) current)

    /// The checked-out program's process has ended, as `ended`, the kernel's account of the end
    /// and the machine the end left, says. The program's run ended as `outcome` says
    /// (`RunningProgram.runEnd`), and the first live program left in launch order is checked
    /// out onto the machine the end left, if any is.
    ///
    /// What the process delivered to its standard streams stays readable from `outcome`'s
    /// state, which is the machine as the process left it.
    let private endCurrent (driver : MultiProgram) (outcome : RunOutcome) (ended : ProcessEnd) : MultiStepOutcome =
        let pid = processIdOf driver.Current
        let runEnd = RunningProgram.runEnd driver.Current outcome
        let endedRuns = Map.add pid runEnd driver.Roster.Ended

        match driver.Launched |> List.tryFind (fun pid -> Map.containsKey pid driver.Idle) with
        | None ->
            MultiStepOutcome.Finished (
                driver.Launched |> List.map (fun pid -> pid, Map.find pid endedRuns),
                ended.Remaining
            )
        | Some next ->
            let stepCounter = (RunOutcome.state outcome).Kernel.StepCounter

            let driver =
                { driver with
                    Current = handOver stepCounter ended.Remaining (Map.find next driver.Idle)
                    Roster =
                        { driver.Roster with
                            Idle = Map.remove next driver.Idle
                            Ended = endedRuns
                        }
                }

            MultiStepOutcome.Stepped (driver, pid, ProgramEvent.Ended runEnd)

    /// Fails loudly unless every program's kept syscall sleepers are those its thread states and
    /// parks show (`Scheduler.checkSyscallSleepers`).
    let private checkSleepers (driver : MultiProgram) : unit =
        Scheduler.checkSyscallSleepers driver.Current.State

        if not driver.Idle.IsEmpty then
            driver.Idle
            |> Map.iter (fun _ program -> Scheduler.checkSyscallSleepers program.State)

    /// Whether any program has a thread parked in `BlockedInSyscall`, which a sweep for syscall
    /// wakes could flip. A program with none has nothing for the sweep to flip, whatever its
    /// signal dispatcher is doing: `SignalDispatch.poll` finishes the dispatcher's read itself.
    let private anySyscallWaiter (driver : MultiProgram) : bool =
        not driver.Current.State._SyscallSleepers.Waiters.IsEmpty
        || (not driver.Idle.IsEmpty
            && driver.Idle
               |> Map.exists (fun _ program -> not program.State._SyscallSleepers.Waiters.IsEmpty))

    /// Flip to `Runnable`, in each program, its threads among `woken`.
    let private flipWoken (woken : Caller<ThreadId> list) (driver : MultiProgram) : MultiProgram =
        let wokenIn (pid : ProcessId) : ThreadId list =
            woken
            |> List.choose (fun caller ->
                if Caller.processId caller = pid then
                    Some (Caller.thread caller)
                else
                    None
            )

        let wake (program : RunningProgram) : RunningProgram =
            match wokenIn (processIdOf program) with
            | [] -> program
            | threads -> withState (RunningProgram.wakeFromSyscalls threads program.State) program

        { driver with
            Current = wake driver.Current
        }
        |> withIdle (mapIdle wake driver.Idle)

    /// `ended`, how a program's end left the driver, with `woken`, threads of other programs a
    /// poll woke before it ended its own process, flipped in the programs left: the kernel wakes
    /// a thread once, so a wake that is not flipped here is lost.
    let private flipWokenAfterEnd (woken : Caller<ThreadId> list) (ended : MultiStepOutcome) : MultiStepOutcome =
        match woken, ended with
        | [], _ -> ended
        | _, MultiStepOutcome.Stepped (driver, pid, event) ->
            MultiStepOutcome.Stepped (flipWoken woken driver, pid, event)
        // No program is left to have a thread woken.
        | _, MultiStepOutcome.Finished _ -> ended
        | _, MultiStepOutcome.Deadlocked _ ->
            failwith
                "MultiProgram: a program's end answered a deadlock, which only the preamble's end does (this is a bug in PawPrint)."

    /// Wake every thread whose syscall could get further now, in every program, and flip
    /// `alsoWoken` too, threads a program's signal poll woke in another program
    /// (`SignalDispatch.pollMachine`).
    ///
    /// One sweep for every parking syscall rather than one each, because the
    /// question is the same for all of them, and the kernel answers it:
    /// `UnixWait.wake` reads each park's wake condition, and decides which waiters it wakes,
    /// choosing the one waiter of a queue that wakes one at a time across every process on
    /// the machine. So a new parking syscall needs no sweep here at all.
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
    /// The machine swept is the checked-out program's, and keeps what the sweep remembers, so
    /// that at a tick which changed nothing a wake condition reads but the clock, short of
    /// every deadline, the kernel answers without asking any condition. A woken thread of an
    /// idle program is only flipped to Runnable: its call is finished when it next runs.
    ///
    /// Fails loudly, at every tick, if a thread any program keeps as parked in a syscall has no
    /// park on the machine (`Scheduler.requireWaitersParked`). A Debug build also checks every
    /// program's kept syscall sleepers against its thread states (`checkSleepers`).
    let private wakeSyscalls (alsoWoken : Caller<ThreadId> list) (driver : MultiProgram) : MultiProgram =
#if DEBUG
        checkSleepers driver
#endif

        let machine = driver.Current.State.Kernel.System
        Scheduler.requireWaitersParked machine driver.Current.State

        if not driver.Idle.IsEmpty then
            driver.Idle
            |> Map.iter (fun _ program -> Scheduler.requireWaitersParked machine program.State)

        let driver =
            if alsoWoken.IsEmpty then
                driver
            else
                flipWoken alsoWoken driver

        if not (anySyscallWaiter driver) then
            driver
        else

        let current = driver.Current
        let woken, machine = UnixWait.wake current.State.Kernel.System

        let driver =
            if obj.ReferenceEquals (machine, current.State.Kernel.System) then
                driver
            else
                driver
                |> withCurrent (withState (current.State.MapKernel (EmulatedKernel.withUnix machine)) current)

        match woken with
        | [] -> driver
        | woken -> flipWoken (List.map fst woken) driver

    /// Whether any program has a Runnable thread: the policy-independent existence check rather
    /// than any scheduler's choice, so that no stochastic policy advances its RNG for a probe.
    let private anyRunnable (driver : MultiProgram) : bool =
        Scheduler.hasAnyRunnable driver.Current.State
        || (not driver.Idle.IsEmpty
            && driver.Idle
               |> Map.exists (fun _ program -> Scheduler.hasAnyRunnable program.State))

    /// Every finite wait deadline any program has outstanding: each program's runtime-level
    /// waits' (`RunningProgram.pendingDeadlines`), program by program in launch order, the
    /// checked-out program's from `current`, and then those of every call asleep in `current`'s
    /// machine (`RunningProgram.syscallDeadlines`).
    let private pendingDeadlines (driver : MultiProgram) (current : IlMachineState) : int64 list =
        let waits =
            if driver.Idle.IsEmpty then
                RunningProgram.pendingDeadlines current
            else
                let currentPid = processIdOf driver.Current

                driver.Launched
                |> List.collect (fun pid ->
                    if pid = currentPid then
                        RunningProgram.pendingDeadlines current
                    else
                        match Map.tryFind pid driver.Idle with
                        | Some program -> RunningProgram.pendingDeadlines program.State
                        | None -> []
                )

        waits @ RunningProgram.syscallDeadlines current.Kernel.System

    /// Jump the clock forward if no program has a Runnable thread but at least one is parked
    /// with a finite-timeout wait outstanding: advance it to the nearest pending deadline across
    /// every program, and fire it. This is what keeps a guest like `WaitOne(50)` against an
    /// unsignalled handle from deadlocking — without the jump, the clock would advance only when
    /// there's something to step, and there is nothing to step. Answers the driver, and whether
    /// any program has a Runnable thread.
    ///
    /// The jump is the machine's: while any program can run, the clock moves only as
    /// instructions retire, however near another program's deadline is, since the clock is one
    /// for every process.
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
    let rec private jumpToDeadlines (driver : MultiProgram) : struct (MultiProgram * bool) =
        if anyRunnable driver then
            struct (driver, true)
        else
#if DEBUG
            // A thread asleep in the kernel that its program does not keep is never swept, and
            // shows up as no program being able to run.
            checkSleepers driver
#endif

            match pendingDeadlines driver driver.Current.State with
            | [] -> struct (driver, false)
            | deadlines ->
                let target = List.min deadlines
                let current = driver.Current

                let state =
                    // The path that *can* reach the horizon: this jumps the clock straight
                    // to a deadline without retiring a step, so a guest looping on
                    // `Thread.Sleep(Int32.MaxValue)` advances it ~2.1e13 ticks per cheap
                    // iteration. `withVirtualClockTicks` faults here, naming the wait that
                    // ran time off the end, instead of letting the addition wrap and hand
                    // some later sleeper a negative deadline that fires immediately.
                    current.State.MapKernel (
                        EmulatedKernel.withVirtualClockTicks (max current.State.Kernel.VirtualClockTicks target)
                    )

                let now = state.Kernel.VirtualClockTicks

                let driver =
                    { driver with
                        Current = withState (RunningProgram.fireExpiredDeadlines now state) current
                    }
                    |> withIdle (
                        mapIdle
                            (fun program -> withState (RunningProgram.fireExpiredDeadlines now program.State) program)
                            driver.Idle
                    )

                jumpToDeadlines (wakeSyscalls [] driver)

    /// Where each live program's threads are, in launch order, each read in the machine as it
    /// stands, so that what another process's calls did to an idle program shows.
    let private describeEvery (driver : MultiProgram) : (ProcessId * string) list =
        let currentPid = processIdOf driver.Current
        let machine = driver.Current.State.Kernel.System

        live driver
        |> List.map (fun (pid, program) ->
            if pid = currentPid then
                pid, GuestLocation.describe program.State
            else
                let handed = handOver driver.Current.State.Kernel.StepCounter machine program

                pid, GuestLocation.describe handed.State
        )

    /// The signal poll of every idle program, in launch order, whose poll may act
    /// (`SignalDispatch.mayAct`): each is checked out for its poll, which writes the signal pipe
    /// on the machine. Answers the driver, with the threads of other programs the polls woke
    /// after `woken`, or how the tick ended if a poll ended its process.
    let rec private pollIdle
        (loggerFactory : ILoggerFactory)
        (pids : ProcessId list)
        (woken : Caller<ThreadId> list)
        (driver : MultiProgram)
        : Result<MultiProgram * Caller<ThreadId> list, MultiStepOutcome>
        =
        match pids with
        | [] -> Ok (driver, woken)
        | pid :: pids ->

        match Map.tryFind pid driver.Idle with
        | Some program when SignalDispatch.mayAct driver.Current.State.Kernel.System program.State ->
            let driver = checkOut pid driver
            let program = driver.Current

            match SignalDispatch.pollMachine loggerFactory program.BaseClassTypes program.State with
            | SignalPoll.ProcessKilled (state, ended), elsewhere ->
                endCurrent
                    (withCurrent (withState state program) driver)
                    (RunningProgram.signalTerminated state ended)
                    ended
                |> flipWokenAfterEnd (woken @ elsewhere)
                |> Error
            | SignalPoll.Continues state, elsewhere ->
                pollIdle loggerFactory pids (woken @ elsewhere) (withCurrent (withState state program) driver)
        | _ -> pollIdle loggerFactory pids woken driver

    /// The first half of a scheduler tick: everything that happens before a program is chosen
    /// and its policy asked which thread runs next. Advancing the clocks, firing wait deadlines,
    /// letting each program's signal dispatcher wake, waking syscalls, and jumping the virtual
    /// clock forward if nothing is Runnable.
    ///
    /// Of these, the spurious wakeups, the deadline fires and the signal poll are each
    /// program's own, and run on every live program; the clock's advance and its jitter, the
    /// syscall wakes, the jump to the earliest deadline and the deadlock are the machine's.
    /// Only a program whose signal poll may act is checked out for it; the other phases run on
    /// idle programs as they are, reading nothing of the stale machines in their kernels.
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
    /// Ends a program instead if System.Native's dispatcher, handling a signal between
    /// instructions, re-raises it at a default that kills the process.
    let internal advance (loggerFactory : ILoggerFactory) (driver : MultiProgram) : Advanced =
        let program = driver.Current

        // The tick this preamble is running, captured before the counter moves on, so that the
        // spurious wakeups, the clock jitter and the program choice are keyed on the same
        // number. All are fuzz dials a caller scripts by tick, and they would be treacherous to
        // script against each other if "tick N" meant a different moment to each.
        let tick = program.State.Kernel.StepCounter

        let state = RunningProgram.applySpuriousWakeups tick program.State

        let idle =
            mapIdle (fun idle -> withState (RunningProgram.applySpuriousWakeups tick idle.State) idle) driver.Idle

        // Threaded as a state rather than rebuilding the program at each stage below: every
        // stage from here to the signal poll touches only its `State`, and this runs once per
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
        // some thread of some program is already parked on, so that the timeout fires while
        // other threads still had work left in the window rather than at the instruction count
        // the guest's own arithmetic implies. Off by default, in which case this is one match on
        // a DU case.
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
                    (pendingDeadlines (withIdle idle driver) state)
            with
            | None -> state
            // Through the validating setter, which is what faults if a guest's own timeout
            // arithmetic has run the clock off the representable range; `chooseJump` guarantees
            // the target is ahead of the clock, so the monotonicity half of that check cannot
            // fire here.
            | Some target -> state.MapKernel (EmulatedKernel.withVirtualClockTicks target)

        // After advancing the clock, fire any wait deadlines that are now in the past, in every
        // program. This runs every tick (not just on deadlock) so a timeout against a thread
        // holding a release lock can still expire while other threads make progress: e.g.
        // thread A is parked with a 50 ms timeout on a semaphore, and thread B is busy computing
        // something else — A's deadline still fires when the clock reaches it, even though B
        // keeps the scheduler from ever stalling.
        let now = state.Kernel.VirtualClockTicks
        let state = RunningProgram.fireExpiredDeadlines now state

        let idle =
            mapIdle (fun idle -> withState (RunningProgram.fireExpiredDeadlines now idle.State) idle) idle

        // Run System.Native's signal handling before the scheduler picks its next thread: its
        // native handler writes whatever the kernel delivers to the leader into the signal pipe,
        // and a Parked dispatcher with a signal in the pipe reads it and is flipped to Runnable
        // onto the managed callback, so the scheduler can pick it on the same tick. Before the
        // syscall wakes, because writing to the pipe and reading from it change what a syscall
        // parked on it is waiting for.
        match SignalDispatch.pollMachine loggerFactory program.BaseClassTypes state with
        | SignalPoll.ProcessKilled (state, ended), woken ->
            let driver =
                { driver with
                    Current = withState state program
                }
                |> withIdle idle

            endCurrent driver (RunningProgram.signalTerminated state ended) ended
            |> flipWokenAfterEnd woken
            |> Advanced.Settled
        | SignalPoll.Continues state, woken ->

        let driver =
            { driver with
                Current = withState state program
            }
            |> withIdle idle

        let polled =
            if idle.IsEmpty then
                Ok (driver, woken)
            else
                pollIdle loggerFactory driver.Launched woken driver

        match polled with
        | Error settled -> Advanced.Settled settled
        | Ok (driver, woken) ->

        // Wake anything parked in a syscall whose wake condition now holds — a port that has
        // become deliverable, a lock that has become available — and whatever the polls woke
        // in other programs. Before the jump to the deadlines, so neither is mistaken for
        // quiescence.
        let driver = wakeSyscalls woken driver

        let struct (driver, runnable) = jumpToDeadlines driver

        if runnable then
            Advanced.Decide (driver, tick)
        else
            // No program has a Runnable thread, and no deadline is left to jump to: every
            // remaining thread is blocked, so progress is impossible. The status alone does not
            // locate a guest — every thread blocked on a monitor looks alike — so the report
            // names the frame each live thread of each program is in and, where the guest was
            // built with debug information, the source line it is on.
            Advanced.Settled (MultiStepOutcome.Deadlocked (driver, describeEvery driver))

    /// The second half of a scheduler tick: choose a program among those with a Runnable thread
    /// (`ProgramChoice.choose`, keyed on `tick`), check it out if it is idle, and step it
    /// (`RunningProgram.stepDecided`), annotating a failure with where its guest was. `driver`
    /// must already have been through `advance`, which answered `tick`.
    ///
    /// A process that ends is ended on the machine here.
    let internal decide
        (loggerFactory : ILoggerFactory)
        (logger : ILogger)
        (driver : MultiProgram)
        (tick : int64)
        : MultiStepOutcome
        =
        let driver =
            if driver.Idle.IsEmpty then
                driver
            else
                let candidates =
                    live driver
                    |> List.choose (fun (pid, program) ->
                        if Scheduler.hasAnyRunnable program.State then
                            Some pid
                        else
                            None
                    )

                let chosen =
                    ProgramChoice.choose driver.Roster.Choice tick driver.Launched driver.Roster.LastChosen candidates

                let driver =
                    if chosen = processIdOf driver.Current then
                        driver
                    else
                        checkOut chosen driver

                { driver with
                    Roster =
                        { driver.Roster with
                            LastChosen = chosen
                        }
                }

        let pid = processIdOf driver.Current

        annotating
            driver.Current.State
            (fun () ->
                match RunningProgram.stepDecided loggerFactory logger driver.Current with
                | ProgramTick.InstructionStepped (program, ranThread, whatWeDid, effect) ->
                    MultiStepOutcome.Stepped (
                        withCurrent program driver,
                        pid,
                        ProgramEvent.InstructionStepped (ranThread, whatWeDid, effect)
                    )
                | ProgramTick.WorkerTerminated (program, terminatingThread) ->
                    MultiStepOutcome.Stepped (
                        withCurrent program driver,
                        pid,
                        ProgramEvent.WorkerTerminated terminatingThread
                    )
                | ProgramTick.StartupCallReturned program ->
                    MultiStepOutcome.Stepped (withCurrent program driver, pid, ProgramEvent.PhaseAdvanced)
                | ProgramTick.Ended (outcome, ended) -> endCurrent driver outcome ended
            )

    /// One scheduler tick of every program on the machine: the driver's preamble
    /// (`advance`), then a program chosen among those with a thread that can run, and one
    /// instruction of that program's (`decide`).
    let step (loggerFactory : ILoggerFactory) (logger : ILogger) (driver : MultiProgram) : MultiStepOutcome =
        annotating
            driver.Current.State
            (fun () ->
                match advance loggerFactory driver with
                | Advanced.Decide (advanced, tick) -> decide loggerFactory logger advanced tick
                | Advanced.Settled outcome -> outcome
            )

    /// A driver of `programs`, each a program read from its image and launched as a process of
    /// its own, in order, on the machine `machineConfig` describes: the first is the process
    /// the machine boots with, and each later one's process ID is the kernel's choice. Each
    /// program's entry thread holds its first call of startup, and every startup runs from the
    /// first tick, interleaved as `Main`s are.
    ///
    /// The machine boots, then every later process is launched onto it, then each program's
    /// userspace is set up in launch order, which on Darwin draws on the machine's entropy
    /// pool: so the launch order is part of the replay contract.
    ///
    /// A refusal names its knob as `machineKnobs.X` for the machine, and for the program at
    /// index `i` of `programs` as `.X` after `processKnobs i`.
    let internal launchAll
        (loggerFactory : ILoggerFactory)
        (machineKnobs : string)
        (processKnobs : int -> string)
        (machineConfig : MachineConfig)
        (programs : (ProgramLaunch * EntryImage) list)
        : MultiProgram
        =
        match programs with
        | [] -> failwith "MultiProgram: a machine needs at least one program to run."
        | (first, _) :: later ->

        // Checked before anything boots, so that a host that misconfigured it finds out first.
        let clock = MachineConfig.clock machineKnobs machineConfig

        let machine =
            MachineConfig.bootSystem machineKnobs (processKnobs 0) machineConfig first.Process

        let booted = Seq.exactlyOne (UnixSystem.processIds machine)

        let laterIds, machine =
            ((1, machine), later)
            ||> List.mapFold (fun (index, machine) (launch, _) ->
                let knobs = processKnobs index

                let processLaunch =
                    ProcessConfig.toLaunch knobs machineConfig.UnixPlatform launch.Process

                let pid, machine =
                    try
                        UnixSystem.launch processLaunch machine
                    with
                    | :? UnmodelledException
                    | :? System.ArgumentException as e ->
                        KernelRefusal.failAdvising
                            $"%s{knobs}: the machine will not start the process"
                            $"%s{knobs}.CurrentDirectory must name a directory in the machine's filesystem."
                            e

                pid, (index + 1, machine)
            )
            |> fun (ids, (_, machine)) -> ids, machine

        let launched = booted :: laterIds

        let started, machine =
            (machine, List.indexed (List.zip launched programs))
            ||> List.mapFold (fun machine (index, (pid, (launch, entry))) ->
                let kernel =
                    ProcessConfig.kernelOfProcess (processKnobs index) launch.Process pid machine

                let program = ProgramStartup.start loggerFactory launch entry kernel
                program, program.State.Kernel.System
            )

        let first = handOver 0L machine (List.head started)

        {
            Current = first
            Clock = clock
            Roster =
                {
                    Idle =
                        List.tail started
                        |> List.map (fun program -> processIdOf program, program)
                        |> Map.ofList
                    Launched = launched
                    Ended = Map.empty
                    Choice = machineConfig.ProgramChoice
                    LastChosen = List.last launched
                }
        }

    /// A driver of the programs `launches` describes, each launched as a process of its own,
    /// in order, on the machine `machineConfig` describes: the first is the process the machine
    /// boots with, with `machineConfig.ProcessId`, and each later one's process ID is the
    /// kernel's choice. Each image is read, and its entry point checked, before the machine
    /// boots. Every program's startup runs from the first tick, interleaved as `Main`s are, and
    /// `machineConfig.ProgramChoice` chooses which program runs at each tick.
    ///
    /// Refusals name the knob `MachineConfig.X`, or `launches[i].Process.X`.
    let start
        (loggerFactory : ILoggerFactory)
        (machineConfig : MachineConfig)
        (launches : ProgramLaunch list)
        : MultiProgram
        =
        let programs =
            launches
            |> List.map (fun launch -> launch, ProgramStartup.read loggerFactory launch.OriginalPath launch.Image)

        launchAll loggerFactory "MachineConfig" (fun index -> $"launches[%d{index}].Process") machineConfig programs

    /// Run the programs `launches` describes on the machine `machineConfig` describes, as
    /// `start` launches them, until the last ends: how each program's run ended, in launch
    /// order.
    ///
    /// Fails loudly if every program is stuck, naming where each one's threads are.
    let run
        (loggerFactory : ILoggerFactory)
        (machineConfig : MachineConfig)
        (launches : ProgramLaunch list)
        : (ProcessId * RunEnd) list
        =
        let logger = loggerFactory.CreateLogger "Program"

        let rec go (driver : MultiProgram) : (ProcessId * RunEnd) list =
            match step loggerFactory logger driver with
            | MultiStepOutcome.Stepped (driver, _, _) -> go driver
            | MultiStepOutcome.Finished (ends, _) -> ends
            | MultiStepOutcome.Deadlocked (_, stuck) ->
                let described =
                    stuck
                    |> List.map (fun (pid, threads) -> $"process %O{pid}: %s{threads}")
                    |> String.concat "\n"

                failwith
                    $"Deadlock: no program has a runnable thread and some process has not exited. Stuck:\n%s{described}"

        go (start loggerFactory machineConfig launches)

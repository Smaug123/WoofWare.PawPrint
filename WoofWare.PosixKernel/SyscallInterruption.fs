namespace WoofWare.PosixKernel

/// What a syscall a task is asleep in does when a signal with a handler is to
/// be delivered to that task before the syscall has anything else to answer.
[<RequireQualifiedAccess>]
type SignalRestartRule =
    /// The syscall fails with `EINTR`, whatever flags the handler was
    /// installed with.
    | FailsWithEintr
    /// The syscall is restarted if the handler was installed with
    /// `SA_RESTART`, and fails with `EINTR` otherwise.
    | RestartsUnderSaRestart

/// How a signal with a handler ends the syscall a task is asleep in.
[<RequireQualifiedAccess>]
type internal SyscallInterruption =
    /// The syscall fails with `EINTR`. The handlers run as the task returns
    /// from it.
    | Eintr
    /// The syscall does not return. The handlers run, and once they have all
    /// returned the task issues the syscall again, with the arguments it first
    /// made it with.
    | Restart

/// Why this library will not say how a signal ends the syscall a task is asleep
/// in.
[<RequireQualifiedAccess>]
type SyscallInterruptionRefusal =
    /// The library will not say what the task takes as it returns to user
    /// mode.
    | Receiver of SignalReceiverRefusal
    /// The first signal the task would take as it returns to user mode is at
    /// its default disposition, which terminates, stops or continues the
    /// process, rather than caught.
    | DefaultAction of signal : Signal
    /// The syscall is one that `SA_RESTART` restarts, and of the handlers the
    /// task would run, those for `restarting` were installed with it and those
    /// for `failing` without.
    | MixedRestartFlags of restarting : Signal list * failing : Signal list
    /// The syscall has its own answer to give and a signal with a handler is
    /// deliverable to the task too, under a flavour whose kernel answers
    /// whichever of the two reached the sleeping task first, which this
    /// library does not record.
    | SignalBesideCompletion of flavour : SimulatedUnixFlavour

[<RequireQualifiedAccess>]
module SyscallInterruptionRefusal =
    /// What this kernel knows about why it cannot answer. The client supplies
    /// its own half: which call was asleep, and in which task.
    let describe (refusal : SyscallInterruptionRefusal) : string =
        match refusal with
        | SyscallInterruptionRefusal.Receiver refusal ->
            $"a signal is pending for a task asleep in a syscall, and this kernel will not say what the task takes as it returns to user mode: %A{refusal}."
        | SyscallInterruptionRefusal.DefaultAction signal ->
            $"the first signal a task asleep in a syscall would take is %O{signal}, at a default disposition that terminates, stops or continues the process, and what that does to the syscall is unmeasured."
        | SyscallInterruptionRefusal.MixedRestartFlags (restarting, failing) ->
            $"a task asleep in a syscall that SA_RESTART restarts would run handlers for %A{restarting}, installed with SA_RESTART, and for %A{failing}, installed without it. Which of them decides whether the syscall restarts is unmeasured."
        | SyscallInterruptionRefusal.SignalBesideCompletion flavour ->
            $"a task asleep in a syscall has both the syscall's own answer and a signal with a handler waiting for it. Under the %O{flavour} flavour the kernel answers whichever of the two reached the sleeping task first, and this kernel does not record which did."

/// Why this library will not answer a `sigsuspend(2)` or `pause(2)`, or the
/// finishing of one: something it does not model, rather than an error a
/// kernel would report.
[<RequireQualifiedAccess>]
type SigsuspendRefusal =
    /// The library will not say what the task takes as it returns to user
    /// mode.
    | Receiver of SignalReceiverRefusal
    /// The first signal the task would take under the temporary mask is
    /// `signal`, at its default, which stops the process. Stopped and then
    /// continued with no handler run, a real kernel restarts the call, so that
    /// it sleeps on under the temporary mask (measured on both flavours); this
    /// library models no stopped process for the call to sleep on in.
    | DefaultStop of signal : Signal
    /// Under Darwin, the task could take a pending SIGCONT under the temporary
    /// mask, ignored or at its default. Darwin leaves one generated while the
    /// task blocked it pending under such a mask, without ending the call, and
    /// does not end it even once a handler is installed for the signal, until
    /// another signal ends the call and both are delivered (measured). This
    /// library wakes a sleeping task from the state alone, so cannot leave a
    /// signal it could deliver undelivered; and what Darwin does with one
    /// generated during the sleep is unmeasured.
    | DarwinPendingContinue

[<RequireQualifiedAccess>]
module SigsuspendRefusal =
    /// What this kernel knows about why it cannot answer. The client supplies
    /// its own half: which call was made, and by which task.
    let describe (refusal : SigsuspendRefusal) : string =
        match refusal with
        | SigsuspendRefusal.Receiver refusal ->
            $"a task in sigsuspend would take a signal, and this kernel will not say what it takes as it returns to user mode: %A{refusal}."
        | SigsuspendRefusal.DefaultStop signal ->
            $"the first signal a task in sigsuspend would take is %O{signal}, at its default, which stops the process. A real kernel restarts the call once the process is continued, if no handler has run, and this kernel models no stopped process."
        | SigsuspendRefusal.DarwinPendingContinue ->
            "under Darwin, a task in sigsuspend could take a pending SIGCONT, ignored or at its default. Darwin was measured to leave such a signal pending without ending the call until another signal does, which this kernel cannot express: it wakes a sleeping task whenever the state holds something for it to take."

[<RequireQualifiedAccess>]
module SyscallInterruption =

    /// How the syscall `parked` records ends when a signal with a handler
    /// interrupts it, on either flavour.
    let ruleOf (parked : ParkedSyscall) : SignalRestartRule =
        // Measured on Linux 6.18.5 and Darwin 25.6.0
        // (`docs/plans/2026-08-23-posix-kernel-extraction/signal-sigaction-flags.c`),
        // each call blocked and signalled 50 ms in: `flock` and `accept` returned
        // EINTR without SA_RESTART and completed with it; `poll` with a timeout
        // of -1 or 300 ms, and `epoll_wait` on Linux and `kevent` on Darwin,
        // returned EINTR either way. A `poll` that watches nothing fails with
        // EINTR either way too, measured on Linux 6.18.5 and Darwin 27.0.0
        // (`signal-interrupt-requeue.c`, section A).
        //
        // A receive with `SO_RCVTIMEO` set is where the flavours part (Linux
        // EINTR, Darwin restarts), and Linux's accept answers EINTR under a
        // timeout for the same reason (`sock_intr_errno`). A parked accept never
        // has one: `setsockopt` refuses to set `SO_RCVTIMEO`.
        //
        // Measured on Linux 6.18.5 and Darwin 27.0.0 (`pipe-blocking.c`,
        // sections C and D): a `read` of an empty pipe, and a `write` into a
        // full one that had put nothing in, returned EINTR without SA_RESTART
        // and went on sleeping with it. A write that had put bytes in returns
        // their count either way, which its finishing call answers before it
        // asks this.
        //
        // Measured on Linux 6.18.5 and Darwin 27.0.0 (`tcp-blocking.c`,
        // sections R-eintr, R-restart and W-empty): the same of a `read` of a
        // connected socket with nothing to answer, and of a `write` to one that
        // had taken nothing. `SO_RCVTIMEO` and `SO_SNDTIMEO`, under which
        // Linux's `sock_intr_errno` answers EINTR instead, cannot be set.
        match parked with
        | ParkedSyscall.Flock _
        | ParkedSyscall.Accept _
        | ParkedSyscall.PipeRead _
        | ParkedSyscall.PipeWrite _
        | ParkedSyscall.ConnectionRead _
        | ParkedSyscall.ConnectionWrite _ -> SignalRestartRule.RestartsUnderSaRestart
        | ParkedSyscall.EpollWait _
        | ParkedSyscall.Kevent _
        | ParkedSyscall.Poll _
        | ParkedSyscall.KqueuePoll _ -> SignalRestartRule.FailsWithEintr
        // Measured on Linux 6.18.5 and Darwin 27.0.0 (`sigsuspend-mask.c`, the
        // "handler" and "pause-handler" rows): EINTR under SA_RESTART too.
        | ParkedSyscall.SigSuspend -> SignalRestartRule.FailsWithEintr

    /// What a task in `sigsuspend` does now, its temporary mask in force.
    [<RequireQualifiedAccess>]
    type internal Suspension<'Task, 'Handler when 'Task : comparison and 'Handler : equality> =
        /// The call ends: the task's return to user mode runs a handler, or
        /// applies a default that terminates the process. The signals are as
        /// the call leaves them, for that return to take from.
        | Ends of SignalState<'Task, 'Handler>
        /// The call sleeps on, with these signals.
        | Sleeps of SignalState<'Task, 'Handler>

    /// What the `sigsuspend` `task` is in does, with `signals` holding its
    /// temporary mask and the mask to restore: what the task would take as it
    /// returned to user mode decides it.
    let internal suspension<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (task : 'Task)
        (system : UnixSystem<'Task, 'Handler>)
        (signals : SignalState<'Task, 'Handler>)
        : Result<Suspension<'Task, 'Handler>, SigsuspendRefusal>
        =
        let tasks = system.Tasks |> Map.keys |> Set.ofSeq
        let flavour = SimulatedUnixPlatform.flavour system.Machine.UnixPlatform

        let rec decide
            (signals : SignalState<'Task, 'Handler>)
            : Result<Suspension<'Task, 'Handler>, SigsuspendRefusal>
            =
            match SignalState.takeOnReturn system.Process.CoreDumps system.Leader tasks task signals with
            | Error refusal -> Error (SigsuspendRefusal.Receiver refusal)
            // Measured by `docs/plans/2026-08-23-posix-kernel-extraction/sigsuspend-mask.c`
            // on Linux 6.18.5 and Darwin 27.0.0: a caught signal ended the call
            // with EINTR once its handler had run, whether it was pending as the
            // call was made or sent during it, SA_RESTART or not; and a TERM at
            // its default killed the process, sent during the call or pending
            // before it and unblocked by the temporary mask.
            | Ok (Some (SignalDelivery.RunHandlers _), _)
            | Ok (Some (SignalDelivery.DefaultTerminate _), _) -> Ok (Suspension.Ends signals)
            // Measured: stopped and continued with no handler run, the call slept
            // on, on both flavours.
            | Ok (Some (SignalDelivery.DefaultStop signal), _) -> Error (SigsuspendRefusal.DefaultStop signal)
            | Ok (Some (SignalDelivery.DefaultContinue _), taken) ->
                match flavour with
                // Measured: a SIGCONT at its default, blocked and pending, which
                // the temporary mask unblocked, ended nothing, and was never
                // delivered to a handler installed afterwards: Linux takes and
                // discards it, then restarts the call (`-ERESTARTNOHAND` with no
                // handler run).
                | SimulatedUnixFlavour.Linux -> decide taken
                | SimulatedUnixFlavour.Darwin -> Error SigsuspendRefusal.DarwinPendingContinue
            | Ok (None, walked) ->
                match flavour with
                // Measured: an ignored signal, blocked and pending, which the
                // temporary mask unblocked, ended nothing, and was never delivered
                // to a handler installed afterwards. The return to user mode
                // discards it, as Linux's does, and the call restarts.
                | SimulatedUnixFlavour.Linux -> Ok (Suspension.Sleeps walked)
                // Darwin discards an ignored signal at generation, SIGCONT apart,
                // so a walk that discards something has found a SIGCONT, which
                // Darwin was measured to leave pending.
                | SimulatedUnixFlavour.Darwin ->
                    if walked = signals then
                        Ok (Suspension.Sleeps signals)
                    else
                        Error SigsuspendRefusal.DarwinPendingContinue

        decide signals

    /// The handler frames `task` would get, innermost first, were it to return
    /// to user mode now: empty when it would run no handler.
    let private framesOnReturn<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (task : 'Task)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<HandlerFrame<'Task, 'Handler> list, SyscallInterruptionRefusal>
        =
        let tasks = system.Tasks |> Map.keys |> Set.ofSeq

        // Only asked: the walk's own state (the frames pushed, the ignored
        // signals discarded) is dropped. The task returns to user mode for real
        // once its syscall has answered.
        match SignalState.onReturnToUser system.Process.CoreDumps system.Leader tasks task system.Process.Signals with
        | Error refusal -> Error (SyscallInterruptionRefusal.Receiver refusal)
        | Ok (None, _) -> Ok []
        | Ok (Some (SignalDelivery.RunHandlers frames), _) -> Ok frames
        | Ok (Some (SignalDelivery.DefaultTerminate (signal, _)), _)
        | Ok (Some (SignalDelivery.DefaultStop signal), _)
        | Ok (Some (SignalDelivery.DefaultContinue signal), _) ->
            Error (SyscallInterruptionRefusal.DefaultAction signal)

    /// Whether `task`, asleep in a syscall, is woken by a signal now: it would
    /// run a handler as it returned to user mode, or this library refuses to say
    /// what it would do, which the syscall's finishing call then reports. A task
    /// in `sigsuspend` is woken also when what it could take would change the
    /// signal state, which its finishing call commits before it sleeps again.
    let internal wakes<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (task : 'Task)
        (system : UnixSystem<'Task, 'Handler>)
        : bool
        =
        // Nothing pending is no handler to run, and every sleeping task is
        // asked this on every poll of its wake condition, so it answers without
        // walking the signal state.
        if List.isEmpty (SignalState.pending system.Process.Signals) then
            false
        else

        match UnixTaskTable.parkedFor task system.Tasks with
        // A sigsuspend decides for itself what a signal it could take does to
        // it: a woken one finishes, parks again having discarded what Linux
        // discards, or refuses what this library cannot say. Only a call that
        // would sleep on unchanged stays asleep, so that every refusal
        // `UnixSignal.finishSigsuspend` makes is reached.
        | Some ParkedSyscall.SigSuspend ->
            match suspension task system system.Process.Signals with
            | Ok (Suspension.Sleeps signals) -> signals <> system.Process.Signals
            | Ok (Suspension.Ends _)
            | Error _ -> true
        | Some _
        | None ->

        match framesOnReturn task system with
        | Ok [] -> false
        | Ok (_ :: _)
        | Error _ -> true

    /// Whether a transfer of `count` bytes from or to the entropy pool by `task`
    /// would stop short at a page boundary for a signal: a read or write of
    /// `/dev/urandom`, or `getrandom(2)`. Linux's `get_random_bytes_user` and
    /// `write_pool_user` (drivers/char/random.c, 6.18) move a 64-byte block at
    /// a time and, each time the bytes moved reach a multiple of a page with
    /// more still to move, stop if a signal is pending: so a transfer of more
    /// than a page by a task with one pending moves a page, short, and never
    /// answers EINTR. They count bytes moved, so where the buffer starts does
    /// not matter, and a transfer of a page or less never reaches a check.
    let internal stopsAtPageBoundary<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (task : 'Task)
        (count : int)
        (system : UnixSystem<'Task, 'Handler>)
        : bool
        =
        let pageSize =
            SimulatedPageSize.bytes (SimulatedUnixPlatform.pageSize system.Machine.UnixPlatform)

        count > pageSize && wakes task system

    /// Whether a signal with a handler ends the sleep of `task` now, whatever
    /// the handlers' flags: for a syscall that, interrupted, returns what it
    /// has done so far rather than failing or restarting, as a write that has
    /// put bytes into a pipe does. `Error` where this library will not say what
    /// the task takes as it returns to user mode, or where it is a signal's
    /// default action.
    let internal interrupts<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (task : 'Task)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<bool, SyscallInterruptionRefusal>
        =
        framesOnReturn task system |> Result.map (List.isEmpty >> not)

    /// Whether the syscall `task` is asleep in may give its own answer, now that
    /// it has one: a finishing call asks this before it answers, and refuses
    /// what this refuses.
    ///
    /// Under Linux it may, whatever signal is pending: the handlers run as the
    /// task returns. Under Darwin it may only if no signal with a handler is
    /// deliverable to the task too.
    let internal beforeCompleting<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (task : 'Task)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<unit, SyscallInterruptionRefusal>
        =
        // Measured (`signal-interrupt-requeue.c`): with the sleeper held off
        // the CPU until both held, Linux 6.18.5 answered with the readiness
        // every time, for epoll_wait, poll, accept and flock, whichever came
        // first (section D); Darwin 27.0.0 answered whichever reached the
        // sleeper first, for accept and flock (section F).
        match SimulatedUnixPlatform.flavour system.Machine.UnixPlatform with
        | SimulatedUnixFlavour.Linux -> Ok ()
        | SimulatedUnixFlavour.Darwin ->
            match framesOnReturn task system with
            | Ok [] -> Ok ()
            | Ok (_ :: _) -> Error (SyscallInterruptionRefusal.SignalBesideCompletion SimulatedUnixFlavour.Darwin)
            | Error refusal -> Error refusal

    /// How a signal ends the syscall `task` is asleep in, if one does now:
    /// `None` when returning to user mode now would run no handler.
    ///
    /// A finishing call asks this only once it has found that the syscall has
    /// nothing else to answer: a wait whose own condition holds answers that,
    /// and the handlers run as it returns.
    ///
    /// Fails loudly if `task` is not asleep in a syscall.
    let internal ofPark<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (task : 'Task)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SyscallInterruption option, SyscallInterruptionRefusal>
        =
        let parked =
            match UnixTaskTable.parkedFor task system.Tasks with
            | Some parked -> parked
            | None ->
                failwith
                    $"SyscallInterruption.ofPark: task %O{task} is not asleep in a syscall, so no signal can interrupt one (this is a bug in the caller)."

        match framesOnReturn task system with
        | Error refusal -> Error refusal
        | Ok [] -> Ok None
        | Ok frames ->

        match ruleOf parked with
        | SignalRestartRule.FailsWithEintr -> Ok (Some SyscallInterruption.Eintr)
        | SignalRestartRule.RestartsUnderSaRestart ->
            // Every frame is pushed before any handler runs, and a kernel decides
            // the syscall's fate as it pushes the first. While the handlers agree
            // the order cannot matter, and which one decides when they disagree
            // is unmeasured.
            let restarting, failing =
                frames |> List.partition (fun frame -> frame.Action.Restart)

            match restarting, failing with
            | _, [] -> Ok (Some SyscallInterruption.Restart)
            | [], _ -> Ok (Some SyscallInterruption.Eintr)
            | _, _ ->
                Error (
                    SyscallInterruptionRefusal.MixedRestartFlags (
                        restarting |> List.map (fun frame -> frame.Entry.Signal),
                        failing |> List.map (fun frame -> frame.Entry.Signal)
                    )
                )

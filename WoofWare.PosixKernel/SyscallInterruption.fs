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
type SyscallInterruption =
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
        match parked with
        | ParkedSyscall.Flock _
        | ParkedSyscall.Accept _
        | ParkedSyscall.PipeRead _
        | ParkedSyscall.PipeWrite _ -> SignalRestartRule.RestartsUnderSaRestart
        | ParkedSyscall.SocketWait _
        | ParkedSyscall.Poll _ -> SignalRestartRule.FailsWithEintr

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
    /// what it would do, which the syscall's finishing call then reports.
    let internal wakes<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (task : 'Task)
        (system : UnixSystem<'Task, 'Handler>)
        : bool
        =
        match framesOnReturn task system with
        | Ok [] -> false
        | Ok (_ :: _)
        | Error _ -> true

    /// Whether a signal with a handler ends the sleep of `task` now, whatever
    /// the handlers' flags: for a syscall that, interrupted, returns what it
    /// has done so far rather than failing or restarting, as a write that has
    /// put bytes into a pipe does. `Error` where this library will not say what
    /// the task takes as it returns to user mode, or where it is a signal's
    /// default action.
    let interrupts<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
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
    let beforeCompleting<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
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
    let ofPark<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
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

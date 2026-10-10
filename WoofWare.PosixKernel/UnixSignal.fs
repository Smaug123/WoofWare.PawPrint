namespace WoofWare.PosixKernel

/// Why this library will not answer a `kill(2)`: something it does not model,
/// rather than an error a kernel would report.
[<RequireQualifiedAccess>]
type KillRefusal =
    /// A positive process ID other than the calling process's own. A real
    /// kernel's answer depends on whether that process exists and on what the
    /// signal does to it, and a process's view holds no other process: sending
    /// a signal to another process is not modelled.
    | OtherProcess of pid : int32
    /// Zero or a negative number: a process group, or every process the caller
    /// may signal.
    | ProcessGroup of pid : int32
    /// The calling process is process ID 1. An init process ignores, from
    /// inside its own PID namespace, every signal it has not installed a
    /// handler for, SIGKILL included, and this library does not model that.
    | InitProcess
    /// The signal is sent to the calling process, and which of its tasks would
    /// receive it is not modelled.
    | Receiver of SignalReceiverRefusal

/// Why this library will not answer a `pthread_kill(3)`: something it does not
/// model, rather than an error a kernel would report.
[<RequireQualifiedAccess>]
type ThreadKillRefusal =
    /// The calling process is process ID 1. An init process ignores, from
    /// inside its own PID namespace, every signal it has not installed a
    /// handler for, SIGKILL included, and this library does not model that.
    | InitProcess
    /// What the signal would do to the process is not modelled.
    | Receiver of SignalReceiverRefusal

/// Why this library will not answer a `sigprocmask(2)`: something it does not
/// model, rather than an error a kernel would report.
[<RequireQualifiedAccess>]
type SigprocmaskRefusal<'Task> =
    /// Under Darwin, whose `sigprocmask` changes every task's mask, the call
    /// would unblock `signal` for `task`, which is not the caller, while
    /// `signal` is pending where `task` would take it: on `task` itself, or,
    /// for the process's leader, on the process.
    ///
    /// Darwin was measured not to wake such a task, if it is asleep in a
    /// system call, and not always to deliver the signal as the task next
    /// returns to user mode: whether it did depended on the call the task was
    /// in and on what the task had done since the signal was sent, which this
    /// library does not record. This library delivers every signal a task's
    /// mask lets through at its next return to user mode, and wakes a sleeping
    /// task for one, so it cannot express what the task does next.
    | DarwinUnblockedForAnotherTask of task : 'Task * signal : Signal

[<RequireQualifiedAccess>]
module SigprocmaskRefusal =
    /// What this kernel knows about why it cannot answer. The client supplies
    /// its own half: which task made the call.
    let describe<'Task> (refusal : SigprocmaskRefusal<'Task>) : string =
        match refusal with
        | SigprocmaskRefusal.DarwinUnblockedForAnotherTask (task, signal) ->
            $"under Darwin, sigprocmask changes every task's mask, and this call would unblock %O{signal} for task %O{task}, which is not the caller, while %O{signal} is pending for that task. Darwin was measured neither to wake that task from a sleep for it nor always to deliver it as the task next returns to user mode, which depended on the call the task was in and on what it had done since the signal was sent, and this kernel does not record that."

/// What a `kill(2)` or `pthread_kill(3)` the kernel answered did to the
/// calling process.
[<RequireQualifiedAccess>]
type KillOutcome<'Task, 'Handler when 'Task : comparison and 'Handler : equality> =
    /// The process carries on, as this system: the signal is pending, or was
    /// discarded, or there was no signal to send.
    | ProcessContinues of UnixSystem<'Task, 'Handler>
    /// The signal stops the whole process, which is this system.
    | ProcessStopped of signal : Signal * UnixSystem<'Task, 'Handler>
    /// The signal killed the process.
    | ProcessEnded of EndedProcess<'Task, 'Handler>

/// What a task's return to user mode did, as `UnixSignal.onReturnToUser`
/// answers it: run the handlers of the caught signals it took, or apply a
/// signal's kernel default.
[<RequireQualifiedAccess>]
type ReturnToUserOutcome<'Task, 'Handler when 'Task : comparison and 'Handler : equality> =
    /// The task took nothing the client acts on, and runs its own code next,
    /// in this system.
    | Resumes of UnixSystem<'Task, 'Handler>
    /// A frame for every caught signal the task takes now, pushed all at once,
    /// innermost first, in this system: the head's handler runs first, and
    /// each handler's `UnixSignal.sigreturn` is followed by another
    /// `UnixSignal.onReturnToUser`, which may push more frames before the next
    /// one down runs. Never empty.
    | RunHandlers of frames : HandlerFrame<'Task, 'Handler> list * UnixSystem<'Task, 'Handler>
    /// No handler claims the signal and its kernel default is to suspend the
    /// whole process, which is this system. The library holds no stopped
    /// state, so the client acts on it.
    | ProcessStopped of signal : Signal * UnixSystem<'Task, 'Handler>
    /// No handler claims the signal, its kernel default is to resume a stopped
    /// process, and the task does not block it: the kernel discarded it as it
    /// was delivered, and the process carries on, as this system. A blocked
    /// one stays pending, as any other signal does, and `sigpending` reports
    /// it.
    ///
    /// The task's return to user mode is not over: the client asks
    /// `onReturnToUser` again, and the task may take more signals, under the
    /// temporary mask of a `sigsuspend(2)` it is returning from. A client
    /// that let the task run its own code instead would leave it with that
    /// temporary mask.
    ///
    /// On a real kernel the resumption itself happens at generation, whatever
    /// any mask says. This library has no stopped process to resume (a stop is
    /// answered as `KillOutcome.ProcessStopped` or `ProcessStopped`, for the
    /// client to act on), so the generation has nothing to do, and what
    /// remains is the pending signal, gated by the mask like any other.
    | ContinueDiscarded of signal : Signal * UnixSystem<'Task, 'Handler>
    /// No handler claims the signal and its kernel default is to terminate
    /// the process, which it did: `EndedProcess.termination` is
    /// `ProcessTermination.Signaled` with the signal and the core flag. A
    /// parent's `wait` reports the process as killed by the signal
    /// (`WIFSIGNALED`, `WTERMSIG`), with the core flag set iff a core was
    /// dumped; `128 + signo` is only how a shell renders that as an exit
    /// status. No handler runs, not even one whose frame was pushed for
    /// another signal at the same return.
    | ProcessEnded of EndedProcess<'Task, 'Handler>

/// What became of a `sigsuspend(2)` or `pause(2)` this kernel could answer.
[<RequireQualifiedAccess>]
type SigsuspendOutcome =
    /// The call failed with this errno.
    ///
    /// `EINTR` is how every call that began ends: a signal the task takes under
    /// the temporary mask has ended it, and the task returns to user mode
    /// (`UnixSignal.onReturnToUser`) before the call's -1 is seen. That return
    /// runs the handlers, the first of whose frames saves the mask from before
    /// the call, or applies a default that terminates the process, which then
    /// never sees the -1 at all. Until that return the task's mask is still the
    /// temporary one, with the mask to restore beside it
    /// (`SignalState.maskToRestore`).
    ///
    /// Linux's `rt_sigsuspend` answers `EINVAL` for a set size other than 8,
    /// having changed nothing.
    | Failed of error : UnixError
    /// The call did not return. The calling task is parked, with the temporary
    /// mask in force, and sleeps until `UnixWait.wakes` wakes it for this
    /// condition; then `UnixSignal.finishSigsuspend` finishes the
    /// call.
    | WouldBlock of WakeCondition

[<RequireQualifiedAccess>]
module UnixSignal =

    let private tasksOf<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (system : UnixSystem<'Task, 'Handler>)
        : Set<'Task>
        =
        system.Tasks |> Map.keys |> Set.ofSeq

    /// `kill(2)`, sent by the calling process to `pid`, with `signo` read under
    /// the process's own signal numbering.
    ///
    /// The signal is pending on the process as a whole, so its leader receives
    /// it unless it blocks the signal. A signal that kills the process ends it,
    /// and the answer is then the ended process rather than a system to make
    /// another call in.
    ///
    /// What a signal does as it is generated, here and wherever else this
    /// library generates one (`pthreadKill`, and the `SIGPIPE` a write raises):
    ///   * at its default disposition, where some task could receive it, it
    ///     takes that default at once: it terminates the process
    ///     (`KillOutcome.ProcessEnded`, with the core flag set if its default
    ///     dumps core and the process's `CoreDumps` allows a dump), stops it
    ///     (`KillOutcome.ProcessStopped`), or, if the default is to discard it,
    ///     is discarded. One whose default is to continue the process is
    ///     pending instead (see `ReturnToUserOutcome.ContinueDiscarded`);
    ///   * ignored, where some task could receive it, it is discarded;
    ///   * otherwise it is pending, including when every task that could
    ///     receive it blocks it, and a task takes it as it returns to user mode
    ///     (`onReturnToUser`) once it can. A standard signal already pending in
    ///     the same pending set is not added again; a real-time one is.
    ///
    /// Before any of that, under Darwin's numbering an ignored signal other
    /// than SIGCONT is discarded, whatever any mask says; under Linux's, an
    /// ignored signal that every receiver blocks stays pending until one can
    /// take it (`Signal.blockedIgnoredSignalStaysPendingUnder`, and see
    /// `onReturnToUser`). A stop signal that is not discarded discards any
    /// pending SIGCONT, and SIGCONT discards every pending stop signal, on
    /// every task and on the process.
    ///
    /// Refuses (`KillRefusal.Receiver`) a caught signal sent to the process
    /// that only a task other than the leader could receive; and, under
    /// Darwin's numbering, a standard signal it would leave pending on the
    /// process while an instance is pending on the leader alone, or the other
    /// way round, which Darwin holds as one instance where this library holds
    /// two.
    ///
    /// Only a signal to the calling process itself is answered. Signal number 0
    /// sends nothing, and a number that is neither 0 nor a signal is `EINVAL`.
    let kill<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (pid : int32)
        (signo : int32)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<Result<KillOutcome<'Task, 'Handler>, UnixError>, KillRefusal>
        =
        let self = ProcessId.toInt32 (UnixSystem.processId system)

        // The target is screened before the number. Linux looks the target up
        // first, so a pid naming no process is ESRCH whatever the number, where
        // Darwin checks the number first and says EINVAL (measured,
        // `docs/plans/2026-08-23-posix-kernel-extraction/kill-arguments.c`).
        // Whether a pid names a process is what this kernel cannot know, so
        // every other target is refused before the number is looked at.
        if pid <= 0 then
            Error (KillRefusal.ProcessGroup pid)
        elif pid <> self then
            Error (KillRefusal.OtherProcess pid)
        elif self = 1 then
            Error KillRefusal.InitProcess
        elif signo = 0 then
            Ok (Ok (KillOutcome.ProcessContinues system))
        else

        match Signal.ofRawSignoUnder (SignalState.numbering system.Process.Signals) signo with
        | ValueNone -> Ok (Error UnixError.EINVAL)
        | ValueSome signal ->
            let withSignals (signals : SignalState<'Task, 'Handler>) : UnixSystem<'Task, 'Handler> =
                { system with
                    Process =
                        { system.Process with
                            Signals = signals
                        }
                }

            let generation =
                SignalState.generate
                    system.Process.CoreDumps
                    system.Leader
                    (tasksOf system)
                    {
                        Signal = signal
                        Target = ValueNone
                    }
                    system.Process.Signals

            match generation with
            | Error refusal -> Error (KillRefusal.Receiver refusal)
            | Ok (SignalGeneration.ProcessContinues signals) ->
                Ok (Ok (KillOutcome.ProcessContinues (withSignals signals)))
            | Ok (SignalGeneration.ProcessStopped (signal, signals)) ->
                Ok (Ok (KillOutcome.ProcessStopped (signal, withSignals signals)))
            | Ok (SignalGeneration.ProcessTerminated (signal, coreDumped)) ->
                let ended =
                    UnixTaskLifecycle.endProcess (ProcessTermination.Signaled (signal, coreDumped)) system

                Ok (Ok (KillOutcome.ProcessEnded ended))

    /// The signals the C library keeps for its own threads and screens out of
    /// the signal calls it wraps, before the kernel sees them: Linux's 32 and
    /// 33, the first two real-time signals, which glibc uses as SIGCANCEL and
    /// SIGSETXID. Darwin's C library keeps none.
    let private reservedByCLibrary (numbering : SignalNumbering) (signal : Signal) : bool =
        match numbering, signal with
        | SignalNumbering.Linux, Signal.RealTime 0
        | SignalNumbering.Linux, Signal.RealTime 1 -> true
        | _, _ -> false

    let private withSignals<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (signals : SignalState<'Task, 'Handler>)
        (system : UnixSystem<'Task, 'Handler>)
        : UnixSystem<'Task, 'Handler>
        =
        { system with
            Process =
                { system.Process with
                    Signals = signals
                }
        }

    /// What `task` takes as it returns to user mode: its own signals, and if it
    /// is the process's leader, the process's too. A client asks this before
    /// `task` next runs its own code: after every system call it makes,
    /// `sigreturn` included.
    ///
    /// Returns the possibly-updated system in every case, because an answer can
    /// change the state without producing an action (see the Ignore rule
    /// below), and a client that dropped the no-action system would replay
    /// those discards forever.
    ///
    /// The signals are walked in the order a task takes them, skipping any
    /// that `task`'s mask blocks, and what happens to each is its disposition
    /// *now*, not at generation:
    ///   * caught — a handler frame is pushed for it, and the walk goes on
    ///     under the mask the frame says, so that every caught signal the task
    ///     can take gets a frame before any handler runs. Their handlers run
    ///     innermost first, the reverse of the order they were taken in.
    ///     Under `SA_RESETHAND` the disposition returns to the default as the
    ///     frame is pushed;
    ///   * ignored, whether by `SIG_IGN` or by a default of Ignore — it is
    ///     discarded silently, the walk continuing past it. Under Linux's
    ///     numbering an ignored signal that was blocked as it was generated
    ///     stayed pending (see `kill`), and this is what un-pends it: it
    ///     becomes receivable while still ignored and is dropped, or a handler
    ///     arrives first and it is delivered;
    ///   * the default, where that is to terminate or stop — surfaced as its
    ///     case for the client to act on, a termination with its core flag as
    ///     the process's `CoreDumps` decides.
    ///
    /// Within one pending set, Darwin takes the lowest-numbered signal first,
    /// and Linux takes SIGILL, SIGTRAP, SIGBUS, SIGFPE, SIGSEGV and SIGSYS
    /// first and then the lowest-numbered; several instances of one real-time
    /// signal come in the order they were generated. Linux takes every signal
    /// of the task's own set before any of the process's; Darwin takes the two
    /// as one set.
    ///
    /// A pending signal whose default is to continue the process, at its
    /// default, surfaces as `ReturnToUserOutcome.ContinueDiscarded` if `task` does not
    /// block it, and stays pending if it does. The task's return is not over:
    /// the client asks again before the task runs its own code, and that answer
    /// may take more signals.
    ///
    /// A task returning from `sigsuspend` or `pause` (`SignalState.maskToRestore`
    /// is `Some`) takes its signals under the call's temporary mask, and gets
    /// the mask the call replaced back as it returns: the first frame pushed
    /// (the outermost) saves that mask rather than the temporary one, so the
    /// handler's `sigreturn` restores it, and a return that pushes no frame
    /// restores it at once, unless it answers `DefaultContinue`, whose return
    /// goes on under the temporary mask when the client asks again. Each frame
    /// after the first saves the mask the one before it set, as for any other
    /// return.
    ///
    /// Refuses, whichever task is asked, while any signal pending on the
    /// process could be received by a task other than the leader but not by
    /// the leader; the refusal names the first such signal in
    /// `SignalState.pending`'s order. Refuses a stop or continue default
    /// reached after frames were pushed. A refusal changes nothing, a mask to
    /// restore included.
    ///
    /// A signal whose default terminates the process ends it here, and the
    /// answer is then the ended process rather than a system to make another
    /// call in, as `kill` answers one that kills at once.
    ///
    /// A task asleep in a syscall is not in user mode, and returns to it only
    /// once the syscall has answered. A signal ends that sleep through the
    /// syscall's finishing call instead (see `SyscallInterruption`), after which
    /// the task returns to user mode here.
    ///
    /// Fails loudly if `task` names no task, or is asleep in a syscall, each of
    /// which is a bug in the client.
    let onReturnToUser<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (task : 'Task)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<ReturnToUserOutcome<'Task, 'Handler>, SignalReceiverRefusal>
        =
        // Looked up rather than asked of `UnixTaskTable.parkedFor`, so that a
        // name that is no task is left to `SignalState.onReturnToUser`'s own
        // check.
        match Map.tryFind task system.Tasks |> Option.bind (fun state -> state.Parked) with
        | Some {
                   Syscall = parked
               } ->
            failwith
                $"UnixSignal.onReturnToUser: task %O{task} is asleep in %A{parked}, so it is not returning to user mode. A signal ends that sleep through the syscall's finishing call (this is a bug in the client)."
        | None ->

        SignalState.onReturnToUser system.Process.CoreDumps system.Leader (tasksOf system) task system.Process.Signals
        |> Result.map (fun (delivery, signals) ->
            let after = withSignals signals system

            match delivery with
            | None -> ReturnToUserOutcome.Resumes after
            | Some (SignalDelivery.RunHandlers frames) -> ReturnToUserOutcome.RunHandlers (frames, after)
            | Some (SignalDelivery.DefaultStop signal) -> ReturnToUserOutcome.ProcessStopped (signal, after)
            | Some (SignalDelivery.DefaultContinue signal) -> ReturnToUserOutcome.ContinueDiscarded (signal, after)
            | Some (SignalDelivery.DefaultTerminate (signal, coreDumped)) ->
                // Ended as the walk left it: the signal that kills it is taken,
                // so it is not pending in the process's final state, as one
                // `kill` applies at once never is.
                ReturnToUserOutcome.ProcessEnded (
                    UnixTaskLifecycle.endProcess (ProcessTermination.Signaled (signal, coreDumped)) after
                )
        )

    /// `sigreturn(2)`: `task`'s handler for the frame `frame` has returned, and
    /// the frame is popped, restoring the mask that was in force before it was
    /// pushed (`HandlerFrame.SavedMask`), whatever the handler did to the mask
    /// meanwhile. The client then asks `onReturnToUser` again, since signals
    /// the frame blocked may now be deliverable.
    ///
    /// Fails loudly unless `frame` is `task`'s innermost frame: handlers return
    /// innermost first, so anything else is a bug in the client.
    let sigreturn<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (task : 'Task)
        (frame : HandlerFrameId)
        (system : UnixSystem<'Task, 'Handler>)
        : UnixSystem<'Task, 'Handler>
        =
        withSignals (SignalState.sigreturn task frame system.Process.Signals) system

    /// `pthread_kill(3)`, sent by the calling process to `target`, one of its
    /// own tasks, with `signo` read under the process's own signal numbering.
    /// `raise(3)` is this, aimed at the calling task.
    ///
    /// The signal is pending on `target` alone: no other task takes it, even
    /// while `target` blocks it and another task does not. See `kill` for what
    /// the signal then does: a caught one that `target` does not block is
    /// delivered as `target` next returns to user mode, which for `raise(3)` is
    /// before the call returns. A signal that
    /// kills the process ends it, and the answer is then the ended process
    /// rather than a system to make another call in.
    ///
    /// Signal number 0 sends nothing, and a number that is neither 0 nor a
    /// signal is `EINVAL`. So, under Linux's numbering, are 32 and 33, which
    /// the C library keeps for its own threads and will not send this way,
    /// though `kill(2)` sends them.
    ///
    /// Fails loudly if `target` is not one of the process's tasks, which is a
    /// bug in the client.
    let pthreadKill<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (target : 'Task)
        (signo : int32)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<Result<KillOutcome<'Task, 'Handler>, UnixError>, ThreadKillRefusal>
        =
        if not (Map.containsKey target system.Tasks) then
            failwith
                $"UnixSignal.pthreadKill: task %O{target} is not one of the process's tasks (this is a bug in the client)."

        let numbering = SignalState.numbering system.Process.Signals

        // Measured by `docs/plans/2026-08-23-posix-kernel-extraction/raise-sweep.c`
        // on Linux 6.18.5 (glibc 2.41) and Darwin 27.0.0, from -1 to two past the
        // highest signal and at 65, 66, 128, 1000, INT_MIN and INT_MAX, through
        // `raise` and through `pthread_kill(pthread_self())`, which agreed on
        // every row: 0 sends nothing; every signal but Linux's 32 and 33 is sent;
        // every other number is EINVAL. glibc refuses 32 and 33 before the
        // kernel sees them: `kill(2)` sends both (`kill-arguments.c`).
        if ProcessId.toInt32 (UnixSystem.processId system) = 1 then
            Error ThreadKillRefusal.InitProcess
        elif signo = 0 then
            Ok (Ok (KillOutcome.ProcessContinues system))
        else

        match Signal.ofRawSignoUnder numbering signo with
        | ValueNone -> Ok (Error UnixError.EINVAL)
        | ValueSome signal when reservedByCLibrary numbering signal -> Ok (Error UnixError.EINVAL)
        | ValueSome signal ->
            let generation =
                SignalState.generate
                    system.Process.CoreDumps
                    system.Leader
                    (tasksOf system)
                    {
                        Signal = signal
                        Target = ValueSome target
                    }
                    system.Process.Signals

            match generation with
            | Error refusal -> Error (ThreadKillRefusal.Receiver refusal)
            | Ok (SignalGeneration.ProcessContinues signals) ->
                Ok (Ok (KillOutcome.ProcessContinues (withSignals signals system)))
            | Ok (SignalGeneration.ProcessStopped (signal, signals)) ->
                Ok (Ok (KillOutcome.ProcessStopped (signal, withSignals signals system)))
            | Ok (SignalGeneration.ProcessTerminated (signal, coreDumped)) ->
                let ended =
                    UnixTaskLifecycle.endProcess (ProcessTermination.Signaled (signal, coreDumped)) system

                Ok (Ok (KillOutcome.ProcessEnded ended))

    /// `sigaction(2)` as the kernel answers it, with `signo` read under the
    /// process's own signal numbering. `newAction` is `None` to ask for the
    /// signal's disposition without changing it, and the answer is the
    /// disposition the signal had before the call.
    let private sigactionUnder<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (cLibrary : bool)
        (signo : int32)
        (newAction : SignalDisposition<'Handler> option)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SignalDisposition<'Handler> * UnixSystem<'Task, 'Handler>, UnixError>
        =
        let numbering = SignalState.numbering system.Process.Signals

        // Measured by `docs/plans/2026-08-23-posix-kernel-extraction/sigaction-sweep.c`
        // on Linux 6.18.5 (aarch64, glibc 2.41) and Darwin 27.0.0, from -1 to two
        // past the highest signal and at 65, 66, 128, 1000, INT_MIN and INT_MAX:
        // a query, a handler, SIG_IGN and SIG_DFL in turn, through the C library
        // and through the raw system call. Every number that is not a signal was
        // EINVAL for each. Linux reported SIGKILL and SIGSTOP as SIG_DFL and
        // refused to install anything for them; Darwin refused even to report
        // them, through either route. glibc refused 32 and 33 for every call,
        // the query included, where the raw `rt_sigaction` treated them as any
        // other signal.
        match Signal.ofRawSignoUnder numbering signo with
        | ValueNone -> Error UnixError.EINVAL
        | ValueSome signal when cLibrary && reservedByCLibrary numbering signal -> Error UnixError.EINVAL
        | ValueSome signal ->

        let onlyDefault = SignalState.kernelHoldsOnlyDefault signal

        match numbering, newAction with
        | SignalNumbering.Darwin, _ when onlyDefault -> Error UnixError.EINVAL
        | SignalNumbering.Linux, Some _ when onlyDefault -> Error UnixError.EINVAL
        | _, None -> Ok (SignalState.disposition signal system.Process.Signals, system)
        | _, Some action ->
            let old = SignalState.disposition signal system.Process.Signals

            Ok (old, withSignals (SignalState.setDisposition signal action system.Process.Signals) system)

    /// `sigaction(3)` as a program calls it, through its C library, which on
    /// Linux refuses glibc's own 32 and 33 before the kernel sees them: see
    /// `sigactionSyscall` for the rest of the answer.
    let sigaction<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (signo : int32)
        (newAction : SignalDisposition<'Handler> option)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SignalDisposition<'Handler> * UnixSystem<'Task, 'Handler>, UnixError>
        =
        sigactionUnder true signo newAction system

    /// `sigaction(2)`, made as a system call rather than through the C
    /// library, with `signo` read under the process's own signal numbering:
    /// Linux's `rt_sigaction`, and on Darwin the same call `sigaction` makes.
    /// `newAction` is `None` to ask for the signal's disposition without
    /// changing it, and `Some action` to install `action`. Either way the
    /// answer is the disposition the signal had before the call, with the
    /// system as the call leaves it.
    ///
    /// Installing a disposition that ignores the signal discards every pending
    /// instance of it, on every task and on the process: `SIG_IGN`, and
    /// `SIG_DFL` for a signal whose default is to discard it or to continue the
    /// process. A handler's mask is stored without SIGKILL and SIGSTOP, which
    /// nothing can block.
    ///
    /// A number that is not a signal is `EINVAL`, and so is installing
    /// anything for SIGKILL or SIGSTOP. Darwin refuses even to report those
    /// two, where Linux reports them as `SIG_DFL`.
    let sigactionSyscall<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (signo : int32)
        (newAction : SignalDisposition<'Handler> option)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SignalDisposition<'Handler> * UnixSystem<'Task, 'Handler>, UnixError>
        =
        sigactionUnder false signo newAction system

    /// `how` as the numbering's `<signal.h>` numbers it: `SIG_BLOCK`,
    /// `SIG_UNBLOCK` and `SIG_SETMASK` are 0, 1 and 2 on Linux and 1, 2 and 3
    /// on Darwin. Every other number names nothing.
    let private decodeHow (numbering : SignalNumbering) (how : int32) : SignalMaskChange voption =
        match numbering, how with
        | SignalNumbering.Linux, 0
        | SignalNumbering.Darwin, 1 -> ValueSome SignalMaskChange.Block
        | SignalNumbering.Linux, 1
        | SignalNumbering.Darwin, 2 -> ValueSome SignalMaskChange.Unblock
        | SignalNumbering.Linux, 2
        | SignalNumbering.Darwin, 3 -> ValueSome SignalMaskChange.SetMask
        | _, _ -> ValueNone

    /// Which tasks a mask call changes.
    [<RequireQualifiedAccess>]
    type private MaskScope =
        /// The calling task alone.
        | Caller
        /// Every task of the process: Darwin's `sigprocmask`.
        | EveryTask

    /// The mask calls' common answer. `screened` are the signals the C
    /// library takes out of `set` before the kernel sees it.
    let private changeMaskUnder<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (operation : string)
        (scope : MaskScope)
        (screened : Set<Signal>)
        (task : 'Task)
        (how : int32)
        (set : SignalMask option)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SignalMask * UnixSystem<'Task, 'Handler>, UnixError>
        =
        match Map.tryFind task system.Tasks with
        | None ->
            failwith
                $"UnixSignal.%s{operation}: task %O{task} is not one of the process's tasks (this is a bug in the client)."
        | Some {
                   Parked = Some park
               } ->
            failwith
                $"UnixSignal.%s{operation}: task %O{task} is asleep in %A{park.Syscall}, so it cannot be making this call (this is a bug in the client)."
        | Some _ ->

        let signals = system.Process.Signals
        let old = SignalState.maskOf task signals

        // Measured by `docs/plans/2026-08-23-posix-kernel-extraction/sigprocmask-ops.c`
        // on Linux 6.18.5 (glibc 2.41) and Darwin 27.0.0, through every route
        // each has: with a NULL set, every `how` (the three named, -1, every
        // small number, 100, INT_MAX and INT_MIN) answered the old mask and
        // changed nothing; with a set, every unnamed `how` was EINVAL, left the
        // mask as it was, and wrote nothing to `oldset`.
        match set with
        | None -> Ok (old, system)
        | Some set ->

        match decodeHow (SignalState.numbering signals) how with
        | ValueNone -> Error UnixError.EINVAL
        | ValueSome change ->
            let set = SignalMask.without screened set

            let targets =
                match scope with
                | MaskScope.Caller -> [ task ]
                | MaskScope.EveryTask -> system.Tasks |> Map.keys |> List.ofSeq

            let signals =
                (signals, targets)
                ||> List.fold (fun signals target -> SignalState.changeMask change set target signals)

            Ok (old, withSignals signals system)

    /// The signals Linux's C library takes out of every set its mask calls are
    /// handed: its own 32 and 33. Darwin's takes none.
    let private screenedByCLibrary (numbering : SignalNumbering) : Set<Signal> =
        match numbering with
        | SignalNumbering.Linux -> Set.ofList [ Signal.RealTime 0 ; Signal.RealTime 1 ]
        | SignalNumbering.Darwin -> Set.empty

    /// `pthread_sigmask(3)`, called by `task`: change its own signal mask as
    /// `how` says, with `how` read under the process's own signal numbering
    /// (`SIG_BLOCK`, `SIG_UNBLOCK` and `SIG_SETMASK` are 0, 1 and 2 on Linux,
    /// and 1, 2 and 3 on Darwin). `set` is `None` for a NULL set, which asks for
    /// the mask and changes nothing, whatever `how` is. The answer is the mask
    /// before the call, with the system as the call leaves it; a client that
    /// returns errno rather than setting it, as `pthread_sigmask` does, reads
    /// the error from the `Error` case.
    ///
    /// A set's SIGKILL and SIGSTOP are dropped silently, and on Linux so are
    /// the C library's own 32 and 33, before the kernel sees the set: blocking
    /// either does nothing, and `SIG_SETMASK` clears both from the mask, though
    /// the raw call can set them (`rtSigprocmask`). Every other bit is kept,
    /// Darwin's bit 31 included, which names no signal (see `SignalMask`).
    ///
    /// Unblocking can make a pending signal deliverable: to `task` as it
    /// returns from this call (`onReturnToUser`), which for several at once
    /// pushes a frame for each before any handler runs.
    ///
    /// Darwin's raw `__pthread_sigmask` system call answered exactly as this on
    /// every row measured, so this serves it too.
    ///
    /// An unnamed `how` with a set is `EINVAL`, and changes nothing.
    ///
    /// Fails loudly if `task` names no task, or is asleep in a syscall, each
    /// of which is a bug in the client; and on a set made under another
    /// numbering.
    let pthreadSigmask<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (task : 'Task)
        (how : int32)
        (set : SignalMask option)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SignalMask * UnixSystem<'Task, 'Handler>, UnixError>
        =
        // Measured by `sigprocmask-ops.c`: blocking every bit through glibc left
        // 0xfffffffe7ffbfeff (without 9, 19, 32 and 33), and through Darwin's
        // C library 0xfffefeff (without 9 and 17, with bit 31). Through glibc,
        // a SIG_SETMASK naming neither 32 nor 33 cleared both where the raw call
        // had set them, and a SIG_UNBLOCK naming only them left both set.
        let numbering = SignalState.numbering system.Process.Signals
        changeMaskUnder "pthreadSigmask" MaskScope.Caller (screenedByCLibrary numbering) task how set system

    /// The first task of `before` other than `caller`, with the first signal
    /// pending where it would take it, that its mask in `before` blocks and
    /// its mask in `after` does not.
    let private unblockedForAnother<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (caller : 'Task)
        (before : UnixSystem<'Task, 'Handler>)
        (after : UnixSystem<'Task, 'Handler>)
        : ('Task * Signal) option
        =
        let was = before.Process.Signals
        let now = after.Process.Signals

        before.Tasks
        |> Map.keys
        |> Seq.filter (fun other -> other <> caller)
        |> Seq.tryPick (fun other ->
            let blockedBefore = SignalState.maskOf other was
            let blockedAfter = SignalState.maskOf other now

            SignalState.pendingFor before.Leader other was
            |> List.tryFind (fun entry ->
                SignalMask.contains entry.Signal blockedBefore
                && not (SignalMask.contains entry.Signal blockedAfter)
            )
            |> Option.map (fun entry -> other, entry.Signal)
        )

    /// `sigprocmask(2)`, called by `task`: on Linux exactly `pthreadSigmask`.
    ///
    /// On Darwin it changes **every** task's mask, each as `how` says, and
    /// answers the calling task's mask before the call: measured by
    /// `docs/plans/2026-08-23-posix-kernel-extraction/sigprocmask-ops.c` on
    /// Darwin 27.0.0, with the main thread blocking SIGHUP and a second thread
    /// SIGINT, the main thread's `SIG_BLOCK` of SIGTERM left the second
    /// blocking SIGINT and SIGTERM, its `SIG_UNBLOCK` of both left both
    /// threads blocking nothing, and its `SIG_SETMASK` of SIGTERM left both
    /// blocking SIGTERM alone; Darwin's raw `sigprocmask` system call did the
    /// same. Linux's changed the caller's alone, as `pthread_sigmask` did on
    /// both.
    ///
    /// Refuses, on Darwin, a call that would unblock a pending signal for a
    /// task other than the caller (see
    /// `SigprocmaskRefusal.DarwinUnblockedForAnotherTask`), changing nothing.
    /// The caller's own pending signals it unblocks are deliverable to it as it
    /// returns from the call, as `pthreadSigmask` describes.
    ///
    /// Everything else is as `pthreadSigmask` answers.
    let sigprocmask<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (task : 'Task)
        (how : int32)
        (set : SignalMask option)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<Result<SignalMask * UnixSystem<'Task, 'Handler>, UnixError>, SigprocmaskRefusal<'Task>>
        =
        let numbering = SignalState.numbering system.Process.Signals

        match numbering with
        | SignalNumbering.Linux ->
            changeMaskUnder "sigprocmask" MaskScope.Caller (screenedByCLibrary numbering) task how set system
            |> Ok
        | SignalNumbering.Darwin ->

        match changeMaskUnder "sigprocmask" MaskScope.EveryTask (screenedByCLibrary numbering) task how set system with
        | Error error -> Ok (Error error)
        | Ok (old, after) ->
            // Measured by `docs/plans/2026-08-23-posix-kernel-extraction/unblock-wakes-sleeper.c`
            // on Darwin 27.0.0: a second thread's sigprocmask unblocked a signal
            // pending on a thread asleep in each of poll, kevent, read and write
            // of a pipe or a TCP socket, accept, flock, sigsuspend, pause and
            // nanosleep, and the sleeper slept on until its own condition ended
            // the call 500 ms later, whether the signal was caught or a TERM at
            // its default. Then read, write, accept and flock answered EINTR or
            // restarted, and sigsuspend and pause EINTR, each taking the signal;
            // poll, kevent and nanosleep answered as they would have, and took
            // it only if another thread had sent it, never one the sleeper had
            // sent itself before it slept. A thread running user code, with a
            // system call every 100 us, never took it at all.
            match unblockedForAnother task system after with
            | Some (other, signal) -> Error (SigprocmaskRefusal.DarwinUnblockedForAnotherTask (other, signal))
            | None -> Ok (Ok (old, after))

    /// Linux's `rt_sigprocmask(2)`, made as a system call rather than through
    /// the C library, by `task`: as `pthreadSigmask`, except that it takes
    /// `sigsetSize`, the size the caller says its sets are, and screens out
    /// only SIGKILL and SIGSTOP, so it can block the C library's own 32 and 33.
    ///
    /// A `sigsetSize` other than 8 is `EINVAL`, before anything else is
    /// looked at, a NULL set included.
    ///
    /// Fails loudly on a Darwin process, which has no such system call: its raw
    /// `sigprocmask` and `__pthread_sigmask` answered exactly as the C
    /// library's calls on every row measured, so `sigprocmask` and
    /// `pthreadSigmask` serve them.
    let rtSigprocmask<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (task : 'Task)
        (how : int32)
        (set : SignalMask option)
        (sigsetSize : uint64)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SignalMask * UnixSystem<'Task, 'Handler>, UnixError>
        =
        match SignalState.numbering system.Process.Signals with
        | SignalNumbering.Darwin ->
            failwith
                "UnixSignal.rtSigprocmask: a Darwin process has no rt_sigprocmask system call; its raw sigprocmask and __pthread_sigmask are UnixSignal.sigprocmask and UnixSignal.pthreadSigmask (this is a bug in the client)."
        | SignalNumbering.Linux ->

        // Measured by `sigprocmask-ops.c` on Linux 6.18.5: every size from 0
        // to 16 but 8, and 128 (glibc's sizeof(sigset_t)), was EINVAL with a
        // set, with a NULL set, and with a NULL set and an unnamed `how`, and
        // wrote nothing to `oldset`. Blocking every bit left 0xfffffffffffbfeff.
        if sigsetSize <> 8UL then
            Error UnixError.EINVAL
        else
            changeMaskUnder "rtSigprocmask" MaskScope.Caller Set.empty task how set system

    /// `sigpending(2)`, called by `task`: the signals pending that it sees and
    /// that its mask blocks (see `SignalState.pendingBlocked`).
    ///
    /// Fails loudly if `task` names no task, which is a bug in the client.
    let sigpending<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (task : 'Task)
        (system : UnixSystem<'Task, 'Handler>)
        : SignalMask
        =
        if not (Map.containsKey task system.Tasks) then
            failwith
                $"UnixSignal.sigpending: task %O{task} is not one of the process's tasks (this is a bug in the client)."

        SignalState.pendingBlocked system.Leader task system.Process.Signals

    /// `sigsuspend` with `temporary`, made by `task`.
    let private suspendWith<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (operation : string)
        (task : 'Task)
        (temporary : SignalState<'Task, 'Handler> -> SignalMask)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SigsuspendOutcome * UnixSystem<'Task, 'Handler>, SigsuspendRefusal>
        =
        match Map.tryFind task system.Tasks with
        | None ->
            failwith
                $"UnixSignal.%s{operation}: task %O{task} is not one of the process's tasks (this is a bug in the client)."
        | Some {
                   Parked = Some park
               } ->
            failwith
                $"UnixSignal.%s{operation}: task %O{task} is asleep in %A{park.Syscall}, so it cannot be making this call (this is a bug in the client)."
        | Some _ ->

        let signals =
            SignalState.suspend task (temporary system.Process.Signals) system.Process.Signals

        // A signal the temporary mask lets through ends the call as it is made,
        // without a sleep (the probe's "pending" and "two" rows answered within
        // 50 ms), as a `poll` with something to report answers at once.
        match SyscallInterruption.suspension task system signals with
        | Error refusal -> Error refusal
        | Ok (SyscallInterruption.Suspension.Ends signals) ->
            Ok (SigsuspendOutcome.Failed UnixError.EINTR, withSignals signals system)
        | Ok (SyscallInterruption.Suspension.Sleeps signals) ->
            Ok (
                SigsuspendOutcome.WouldBlock (WakeCondition.ofPark ParkedSyscall.SigSuspend),
                UnixWait.park task ParkedSyscall.SigSuspend (withSignals signals system)
            )

    /// `sigsuspend(2)`, called by `task`: replace its mask with `mask` until a
    /// signal it takes under that mask ends the call, and give it back the mask
    /// it had as it returns.
    ///
    /// `mask` is stored as a kernel stores any mask: without SIGKILL and
    /// SIGSTOP, which are dropped silently, and with every other bit, Linux's
    /// 32 and 33 included, which the C library does not screen out of this
    /// call as it does out of `pthreadSigmask`. Only `task`'s mask changes, on
    /// both flavours.
    ///
    /// The call ends, failing with `EINTR`, once `task` would run a handler as
    /// it returned to user mode, whatever `SA_RESTART` says: at once if a
    /// signal `mask` lets through is already pending, and otherwise from the
    /// sleep it parks in (`WouldBlock`, finished by `finishSigsuspend`). The
    /// return to user mode runs every handler due before the call's -1 is seen;
    /// the outermost frame saves the mask from before the call, so that its
    /// `sigreturn` restores it (see `onReturnToUser`). A signal at its default
    /// that terminates the process ends the call the same way, and the return
    /// to user mode then terminates the process.
    ///
    /// An ignored signal ends nothing. One sent during the call is discarded as
    /// it is sent. Under Linux, one pending as the call is made, which `mask`
    /// unblocks, is discarded then, as is a SIGCONT at its default, and the
    /// call sleeps on; such a SIGCONT sent during the call is discarded as the
    /// sleeping task is woken for it, and the call sleeps on, as Linux's does
    /// after a stop and continue.
    ///
    /// Refuses (see `SigsuspendRefusal`): a signal whose default stops the
    /// process, which the task would take; under Darwin, a SIGCONT the task
    /// could take, ignored or at its default; and whatever `onReturnToUser`
    /// refuses. A refusal changes nothing.
    ///
    /// Measured by `docs/plans/2026-08-23-posix-kernel-extraction/sigsuspend-mask.c`
    /// on Linux 6.18.5 (glibc 2.41) and Darwin 27.0.0.
    ///
    /// Fails loudly if `task` names no task, is asleep in a syscall, or has a
    /// mask to restore from an earlier call (it has not returned to user mode
    /// since), each of which is a bug in the client; and on a mask made under
    /// another numbering.
    let sigsuspend<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (task : 'Task)
        (mask : SignalMask)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SigsuspendOutcome * UnixSystem<'Task, 'Handler>, SigsuspendRefusal>
        =
        suspendWith "sigsuspend" task (fun _ -> mask) system

    /// `pause(2)`, called by `task`: `sigsuspend` with the mask it already has,
    /// which is how Darwin's C library makes it.
    ///
    /// Linux's `pause` is a system call of its own that saves no mask, and
    /// answers the same, since nothing but the task itself changes its mask on
    /// Linux. Darwin's restores the mask it was called with, undoing what a
    /// `sigprocmask` from another thread changed meanwhile, as `sigsuspend`
    /// does: measured by `sigsuspend-mask.c`'s "pause-spread" rows.
    let pause<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (task : 'Task)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SigsuspendOutcome * UnixSystem<'Task, 'Handler>, SigsuspendRefusal>
        =
        suspendWith "pause" task (SignalState.maskOf task) system

    /// Linux's `rt_sigsuspend(2)`, made as a system call rather than through
    /// the C library, by `task`: as `sigsuspend`, except that it takes
    /// `sigsetSize`, the size the caller says its set is.
    ///
    /// A `sigsetSize` other than 8 is `EINVAL`, before anything else is looked
    /// at, and changes nothing: measured by `sigsuspend-mask.c` with every size
    /// from 0 to 16 but 8, and 128.
    ///
    /// Fails loudly on a Darwin process, which has no such system call.
    let rtSigsuspend<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (task : 'Task)
        (mask : SignalMask)
        (sigsetSize : uint64)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SigsuspendOutcome * UnixSystem<'Task, 'Handler>, SigsuspendRefusal>
        =
        match SignalState.numbering system.Process.Signals with
        | SignalNumbering.Darwin ->
            failwith
                "UnixSignal.rtSigsuspend: a Darwin process has no rt_sigsuspend system call; its sigsuspend is UnixSignal.sigsuspend (this is a bug in the client)."
        | SignalNumbering.Linux ->

        if sigsetSize <> 8UL then
            Ok (SigsuspendOutcome.Failed UnixError.EINVAL, system)
        else
            sigsuspend task mask system

    /// Finish the `sigsuspend(2)` or `pause(2)` `task` is parked in, once
    /// `UnixWait.wakes` has woken it: `Failed EINTR` if a signal it takes under
    /// the temporary mask ends the call, as `sigsuspend` describes; and
    /// otherwise it parks again, as the call it was woken from, since what
    /// woke it has gone. Under Linux a SIGCONT at its default that woke it is
    /// discarded first.
    ///
    /// Until the task returns to user mode its mask is the temporary one, with
    /// the mask to restore beside it: the client asks
    /// `UnixSignal.onReturnToUser` before anything else runs on the task.
    ///
    /// Refuses as `sigsuspend` does, changing nothing. Fails loudly if `task` is
    /// not parked in one of these calls.
    let finishSigsuspend<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (task : 'Task)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SigsuspendOutcome * UnixSystem<'Task, 'Handler>, SigsuspendRefusal>
        =
        match UnixTaskTable.parkedFor task system.Tasks with
        | Some ParkedSyscall.SigSuspend -> ()
        | Some other ->
            failwith
                $"UnixSignal.finishSigsuspend: task %O{task} is parked in %A{other}, not in sigsuspend, so there is no sigsuspend to finish (this is a bug in the client)."
        | None ->
            failwith
                $"UnixSignal.finishSigsuspend: task %O{task} is not parked, so there is no sigsuspend to finish. Only a task `sigsuspend` or `pause` answered `WouldBlock` finishes here (this is a bug in the client)."

        match SyscallInterruption.suspension task system system.Process.Signals with
        | Error refusal -> Error refusal
        | Ok (SyscallInterruption.Suspension.Ends signals) ->
            Ok (SigsuspendOutcome.Failed UnixError.EINTR, withSignals signals (UnixParkState.unpark task system))
        | Ok (SyscallInterruption.Suspension.Sleeps signals) ->
            Ok (
                SigsuspendOutcome.WouldBlock (WakeCondition.ofPark ParkedSyscall.SigSuspend),
                UnixWait.park task ParkedSyscall.SigSuspend (withSignals signals system)
            )

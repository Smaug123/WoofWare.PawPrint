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
    /// it unless it blocks the signal. See `SignalState.generate` for what the
    /// signal then does. A signal that kills the process ends it, and the answer
    /// is then the ended process rather than a system to make another call in.
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

    /// What `task` takes as it returns to user mode, as
    /// `SignalState.onReturnToUser` decides: `task` takes its own signals, and if
    /// it is the process's leader, the process's too. Asked before `task` next
    /// runs its own code, including after `sigreturn`.
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
        : Result<SignalDelivery<'Task, 'Handler> option * UnixSystem<'Task, 'Handler>, SignalReceiverRefusal>
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
        |> Result.map (fun (delivery, signals) -> delivery, withSignals signals system)

    /// `sigreturn(2)`: the handler for `task`'s innermost frame, `frame`, has
    /// returned. See `SignalState.sigreturn`.
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
    /// while `target` blocks it and another task does not. See
    /// `SignalState.generate` for what the signal then does: a caught one that
    /// `target` does not block is delivered as `target` next returns to user
    /// mode, which for `raise(3)` is before the call returns. A signal that
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

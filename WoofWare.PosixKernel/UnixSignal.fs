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
    /// both. A signal another task blocked and no longer does may then be
    /// deliverable to it: that task takes it as it next returns to user mode,
    /// and if it is asleep in a syscall, `UnixWait.wakes` says so.
    ///
    /// Everything else is as `pthreadSigmask` answers.
    let sigprocmask<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (task : 'Task)
        (how : int32)
        (set : SignalMask option)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SignalMask * UnixSystem<'Task, 'Handler>, UnixError>
        =
        let numbering = SignalState.numbering system.Process.Signals

        let scope =
            match numbering with
            | SignalNumbering.Linux -> MaskScope.Caller
            | SignalNumbering.Darwin -> MaskScope.EveryTask

        changeMaskUnder "sigprocmask" scope (screenedByCLibrary numbering) task how set system

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

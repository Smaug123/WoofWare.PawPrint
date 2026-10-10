namespace WoofWare.PosixKernel

/// A handler as `sigaction(2)` installs it: the handler itself, and the mask
/// and flags that say what happens around it.
type SignalCatch<'Handler> =
    {
        /// What runs. What it is belongs to the client; this library stores it
        /// and hands it back in the handler frame of each delivery.
        Handler : 'Handler
        /// `sa_mask`: the signals blocked while the handler runs, on top of the
        /// mask in force when it was delivered. The kernel drops SIGKILL and
        /// SIGSTOP from it, so `SignalState` never stores either, and keeps
        /// every other bit, one that names no signal included (Darwin's bit 31;
        /// see `SignalMask`).
        Mask : SignalMask
        /// `SA_NODEFER`: the signal itself is not blocked while its handler
        /// runs, so a second instance can interrupt it.
        NoDefer : bool
        /// `SA_RESETHAND`: delivering the signal resets its disposition to the
        /// default, except where the flavour keeps the handler (Darwin's
        /// SIGILL and SIGTRAP). It does not imply `SA_NODEFER`.
        ResetHand : bool
        /// `SA_RESTART`: a system call the handler interrupts is restarted
        /// rather than failing with `EINTR`, where the call is one that can be.
        Restart : bool
    }

[<RequireQualifiedAccess>]
module SignalCatch =
    /// `handler` with an empty `sa_mask` and no flags.
    let ofHandler<'Handler> (handler : 'Handler) : SignalCatch<'Handler> =
        {
            Handler = handler
            Mask = SignalMask.empty
            NoDefer = false
            ResetHand = false
            Restart = false
        }

/// What a process does with a signal delivered to it: what `sigaction(2)`
/// sets.
[<RequireQualifiedAccess>]
type SignalDisposition<'Handler> =
    /// `SIG_DFL`: the signal's kernel default, `Signal.defaultDispositionUnder`.
    | Default
    /// `SIG_IGN`: the signal is discarded.
    | Ignore
    /// A handler is installed, with its mask and flags.
    | Catch of SignalCatch<'Handler>

/// Whether a process that a signal kills writes a core dump, when the
/// signal's default action dumps core (`Signal.dumpsCoreUnder`). A parent's
/// `wait(2)` reports the outcome as the status's core flag (`WCOREDUMP`).
///
/// On a real system this is the net effect of the process's `RLIMIT_CORE`,
/// the machine's core-file settings and whether a dump could be written where
/// they say, so it is configuration rather than a fact about the kernel.
///
/// Linux also writes no dump for a process whose dumpable flag is clear, which
/// a change of its effective IDs can set or clear. This library starts every
/// process dumpable, since it models no `exec(2)` of a set-ID program, and
/// `CoreDumps` is not that flag: see `Suppressed`.
[<RequireQualifiedAccess>]
type CoreDumps =
    /// No dump is written, as under an `RLIMIT_CORE` of 0: for a reason that a
    /// change of the process's IDs leaves in place, so `UnixCredentials`'
    /// calls keep a process at `Suppressed`.
    | Suppressed
    /// Every death by a signal whose default action dumps core writes one.
    | Written

/// A pending signal, and the set it is pending in. `Target = ValueNone` is the
/// process's own set, which `kill(2)` adds to; `ValueSome task` is `task`'s
/// own set, which `pthread_kill(3)` adds to, and which only `task` can take a
/// signal from.
type PendingSignal<'Task> =
    {
        Signal : Signal
        Target : 'Task voption
    }

/// Which handler frame a delivery pushed: the name `UnixSignal.sigreturn`
/// takes it back by. Unique within one `SignalState`, and never reused.
[<Struct>]
type HandlerFrameId = | HandlerFrameId of int64

/// One handler the kernel has set up to run on a task: the frame it pushes on
/// the task's stack when it delivers a caught signal, which the handler's
/// return (`sigreturn(2)`) pops.
type HandlerFrame<'Task, 'Handler> =
    {
        Id : HandlerFrameId
        /// The signal delivered, and the pending set it was taken from.
        Entry : PendingSignal<'Task>
        /// The disposition the signal was delivered under.
        Action : SignalCatch<'Handler>
        /// The task's signal mask when the signal was delivered, which
        /// `sigreturn` restores exactly, whatever the handler did to the mask
        /// while it ran. Delivery sets the task's mask to this, `Action.Mask`,
        /// and the signal itself unless `Action.NoDefer`; the mask a handler
        /// starts under is the task's mask (`SignalState.maskOf`) when it
        /// begins to run.
        SavedMask : SignalMask
    }

/// What the kernel does with the signals a task takes as it returns to user
/// mode, as decided by `UnixSignal.onReturnToUser`: run handlers, or apply a
/// signal's kernel default. The client interprets it: runs the handlers,
/// terminates the simulated process by the signal, or refuses what it does not
/// model.
[<RequireQualifiedAccess>]
type SignalDelivery<'Task, 'Handler> =
    /// A frame for every caught signal the task takes now, pushed all at once,
    /// innermost first: the head's handler runs first, and each handler's
    /// `UnixSignal.sigreturn` is followed by another
    /// `UnixSignal.onReturnToUser`, which may push more frames before the next
    /// one down runs. Never empty.
    | RunHandlers of frames : HandlerFrame<'Task, 'Handler> list
    /// No handler claims the signal and its kernel default is to terminate
    /// the process. A parent's `wait` then reports the process as killed by
    /// the signal (`WIFSIGNALED`, `WTERMSIG`), with the core flag set iff
    /// `coreDumped`; `128 + signo` is only how a shell renders that as an
    /// exit status. Any frames pushed for other signals at the same return
    /// never run.
    | DefaultTerminate of signal : Signal * coreDumped : bool
    /// No handler claims the signal and its kernel default is to suspend
    /// the whole process.
    | DefaultStop of Signal
    /// No handler claims the signal and its kernel default is to resume a
    /// stopped process, and the task does not block it: the kernel discards
    /// it as it is delivered, and nothing else happens. A blocked one stays
    /// pending, as any other signal does, and `sigpending` reports it.
    ///
    /// The task's return to user mode is not over: the client asks
    /// `onReturnToUser` again, and the task may take more signals, under the
    /// temporary mask of a `sigsuspend(2)` it is returning from. A client
    /// that let the task run its own code instead would leave it with that
    /// temporary mask.
    ///
    /// On a real kernel the resumption itself happens at generation,
    /// whatever any mask says. This library has no stopped process to resume
    /// (a stop is answered as `KillOutcome.ProcessStopped` or `DefaultStop`,
    /// for the client to act on), so the generation has nothing to do, and
    /// what remains is the pending signal, gated by the mask like any other.
    /// That is exact under this model.
    | DefaultContinue of Signal

/// How `sigprocmask(2)` changes a mask, which its `how` argument names.
[<RequireQualifiedAccess>]
type internal SignalMaskChange =
    /// `SIG_BLOCK`: add the set to the mask.
    | Block
    /// `SIG_UNBLOCK`: take the set out of the mask.
    | Unblock
    /// `SIG_SETMASK`: the mask becomes the set.
    | SetMask

/// Pure, deterministic model of the simulator's signal-handling state.
///
/// The shape is deliberately small:
///   * `Dispositions` — what `sigaction(2)` has set for each signal: its
///     default, ignored, or caught by a client's handler. A signal absent
///     from the map has its default.
///   * `Blocked` — each task's signal mask, which `sigprocmask(2)` and
///     delivery set and `sigreturn(2)` restores. A task absent from the map
///     blocks nothing.
///   * `Frames` — each task's stack of handler frames, each holding the mask
///     its `sigreturn` restores.
///   * `MasksToRestore` — for each task in `sigsuspend(2)`, the mask it had
///     before the call replaced it, which it gets back as the call returns.
///   * `Pending` — the signals generated and not yet delivered: the process's
///     own set, and each task's.
///
/// One instance of this type belongs to each simulated process. A client reads
/// it out of a system (`UnixSystem.signals`), and changes it only through the
/// signal calls (`UnixSignal`) and a task's leaving (`UnixTaskLifecycle`);
/// the data shape is exercised by property tests against a
/// structurally-different reference oracle.
///
/// Every `Signal` stored here is one `Numbering` has, and every operation
/// checks the signal it is handed before touching the state. The operations
/// are the only route in (the representation is private), so the tables and
/// queue never hold SIGKILL or SIGSTOP in a mask, a disposition for SIGKILL
/// or SIGSTOP, a stored `Default`, or a signal the numbering does not have.
type SignalState<'Task, 'Handler when 'Task : comparison and 'Handler : equality> =
    private
        {
            /// Whose `<signal.h>` this process's signals are read under. Fixed
            /// at construction, like the platform it is derived from: 17 is
            /// SIGCHLD on Linux and SIGSTOP on Darwin, so no operation on this
            /// state is meaningful without it.
            Numbering : SignalNumbering
            /// Never holds `SignalDisposition.Default`: an absent key is the
            /// default, and a stored one would make two states that behave
            /// identically compare unequal.
            Dispositions : Map<Signal, SignalDisposition<'Handler>>
            /// Each task's signal mask. Never holds an empty mask, for the
            /// same reason `Dispositions` holds no default; and never SIGKILL
            /// or SIGSTOP, which a kernel drops from every mask it stores.
            Blocked : Map<'Task, SignalMask>
            /// For each task in `sigsuspend(2)` or `pause(2)`, from the call
            /// until the task next returns to user mode: the mask the call
            /// replaced with its temporary one. Linux's `saved_sigmask`, Darwin's
            /// `uu_oldmask`. An absent task is in neither call; a present one may
            /// hold an empty mask, which it is restored to.
            MasksToRestore : Map<'Task, SignalMask>
            /// Each task's handler frames, innermost first. Never holds an
            /// empty stack, for the same reason `Dispositions` holds no
            /// default.
            Frames : Map<'Task, HandlerFrame<'Task, 'Handler> list>
            /// The id the next frame pushed gets.
            NextFrame : int64
            /// Every pending entry, sorted by its set and then by `pickRank`,
            /// which is the order a task takes the signals of one set in; the
            /// instances of one real-time signal, the only entries that can tie,
            /// stay in the order they were generated. Kept sorted rather than in
            /// generation order because no kernel delivers in generation order,
            /// so two states holding the same signals would otherwise compare
            /// unequal while behaving identically. Pending sets are tiny in
            /// practice, so a sorted list does.
            Pending : PendingSignal<'Task> list
        }

/// What generating a signal does to the process at once, as decided by
/// `SignalState.generate`. Anything else it does happens later, through the
/// pending sets and `SignalState.onReturnToUser`.
[<RequireQualifiedAccess>]
type internal SignalGeneration<'Task, 'Handler when 'Task : comparison and 'Handler : equality> =
    /// The process carries on, with these signals. The signal is pending, or
    /// coalesced into an instance already pending, or discarded because it is
    /// ignored.
    | ProcessContinues of SignalState<'Task, 'Handler>
    /// The signal terminates the process: its disposition is the default,
    /// which is to terminate, and some task could receive it. A
    /// parent's `wait` reports the core flag iff `coreDumped`. There is no
    /// state to carry on with, because the process has ended.
    | ProcessTerminated of signal : Signal * coreDumped : bool
    /// The signal stops the whole process, which has these signals: its
    /// disposition is the default, which is to stop, and some task could
    /// receive it.
    | ProcessStopped of signal : Signal * SignalState<'Task, 'Handler>

/// Why this library will not say what a process's signal does: a case it does
/// not model, rather than one a kernel refuses.
[<RequireQualifiedAccess>]
type SignalReceiverRefusal =
    /// `signal` is sent to the process as a whole and caught, and its leader
    /// blocks it while another task does not, so some task other than the leader
    /// would take it. Which one differs between flavours, and on Linux depends on
    /// which tasks took earlier signals; this library delivers a process's own
    /// signals to its leader only.
    | LeaderBlocks of signal : Signal
    /// Returning to user mode, a task would take `signal`, whose default stops
    /// or continues the process, after handler frames were pushed for other
    /// signals at the same return. What a stopped process does with frames
    /// already pushed has not been measured.
    | DefaultBehindHandlers of signal : Signal
    /// Under Darwin's numbering, `signal`, a standard signal, would be left
    /// pending on the process as a whole while an instance of it is pending on
    /// the leader alone, or the other way round. Darwin holds the two as one
    /// instance: it puts a signal sent to the process in the pending set of
    /// the thread it chooses to take it, which is the leader whenever this
    /// library answers. This library keeps the process's set apart from each
    /// task's.
    | PendingForProcessAndLeader of signal : Signal

[<RequireQualifiedAccess>]
module SignalState =
    /// Validate a signal at the operation boundary: a loud failure if it is
    /// not a signal under this numbering at all. A client can build a signal
    /// one numbering lacks (`SIGPWR` for a Darwin process, or a `RealTime`
    /// out of range), which no kernel mask or disposition table could hold;
    /// the callers that produced a signal from a raw signo honestly went
    /// through `Signal.ofRawSignoUnder`, which refuses those, so reaching this
    /// failure means a client built one some other way.
    let private parseUnder (operation : string) (numbering : SignalNumbering) (signal : Signal) : Signal =
        if Signal.existsUnder numbering signal then
            signal
        else
            failwith
                $"SignalState.%s{operation}: %O{signal} is not a signal under the %O{numbering} numbering; a raw signo should have been refused at the caller's own boundary, via Signal.ofRawSignoUnder."

    let private parse (operation : string) (state : SignalState<'Task, 'Handler>) (signal : Signal) : Signal =
        parseUnder operation state.Numbering signal

    /// SIGKILL and SIGSTOP, for which the kernel holds no disposition but the
    /// default. Narrower than `Signal.isUncatchableUnder`, which adds the two
    /// numbers glibc's `sigaction` refuses on top of the kernel's refusal.
    let internal kernelHoldsOnlyDefault (signal : Signal) : bool =
        match signal with
        | Signal.SIGKILL
        | Signal.SIGSTOP -> true
        | _ -> false

    /// Whether a signal generated under `disposition` is ignored at
    /// generation: `SIG_IGN`, or `SIG_DFL` for a signal whose default is to
    /// discard it.
    let private ignoredAtGeneration
        (numbering : SignalNumbering)
        (disposition : SignalDisposition<'Handler>)
        (signal : Signal)
        : bool
        =
        match disposition with
        | SignalDisposition.Ignore -> true
        | SignalDisposition.Catch _ -> false
        | SignalDisposition.Default -> Signal.defaultDispositionUnder numbering signal = DefaultDisposition.Ignore

    /// Whether setting `disposition` discards the signal's pending instances.
    /// Wider than `ignoredAtGeneration` by one signal: a default SIGCONT
    /// counts as ignored here, though its default is to continue.
    let private discardsPendingWhenSet
        (numbering : SignalNumbering)
        (disposition : SignalDisposition<'Handler>)
        (signal : Signal)
        : bool
        =
        match disposition with
        | SignalDisposition.Ignore -> true
        | SignalDisposition.Catch _ -> false
        | SignalDisposition.Default ->
            match Signal.defaultDispositionUnder numbering signal with
            | DefaultDisposition.Ignore
            | DefaultDisposition.Continue -> true
            | DefaultDisposition.Terminate
            | DefaultDisposition.Stop -> false

    /// `mask` as a kernel holds it: without SIGKILL and SIGSTOP, which it drops
    /// silently, and with every other bit, one that names no signal included.
    /// Fails loudly on a mask made under another numbering, whose bits name
    /// other signals.
    ///
    /// Measured by the signal fuzzer's harness
    /// (`WoofWare.PosixKernel.Test/signalFuzz/harness.c`) on Linux 6.18.5 and
    /// Darwin 27.0.0: a handler whose `sa_mask` named both read neither back in
    /// its mask while it ran. And by
    /// `docs/plans/2026-08-23-posix-kernel-extraction/sigaction-mask-bits.c` on
    /// the same two: an `sa_mask` of every bit was stored and held while the
    /// handler ran less exactly those two, so with Darwin's bit 31 and Linux's
    /// 32 and 33, through glibc and the raw `rt_sigaction` alike.
    let private maskable (operation : string) (numbering : SignalNumbering) (mask : SignalMask) : SignalMask =
        match SignalMask.numbering mask with
        | ValueSome other when other <> numbering ->
            failwith
                $"SignalState.%s{operation}: the mask %O{mask} was made under the %O{other} numbering, and this process reads signals under %O{numbering}."
        | _ -> SignalMask.without (Set.ofList [ Signal.SIGKILL ; Signal.SIGSTOP ]) mask

    let private withDisposition
        (signal : Signal)
        (disposition : SignalDisposition<'Handler>)
        (dispositions : Map<Signal, SignalDisposition<'Handler>>)
        : Map<Signal, SignalDisposition<'Handler>>
        =
        match disposition with
        | SignalDisposition.Default -> Map.remove signal dispositions
        | SignalDisposition.Ignore
        | SignalDisposition.Catch _ -> Map.add signal disposition dispositions

    /// The signal state a process starts with: no handler frames, so nothing
    /// blocked, nothing pending, and every signal at its default except those in
    /// `inheritedIgnores`, which are ignored. `execve(2)` resets every caught
    /// signal to its default but keeps ignored ones ignored, so a launcher
    /// can start a process with signals already ignored (as `nohup` does).
    ///
    /// `numbering` is the platform's — see
    /// `SimulatedUnixPlatform.signalNumbering` — and is fixed for the state's
    /// life; `UnixSystem.checkInvariants` refuses a system whose process reads
    /// signals under a numbering other than its machine's.
    ///
    /// Fails loud on a signal the numbering does not have, and on SIGKILL or
    /// SIGSTOP, which no process can have ignored.
    let internal initial (numbering : SignalNumbering) (inheritedIgnores : Set<Signal>) : SignalState<'Task, 'Handler> =
        let dispositions =
            (Map.empty, inheritedIgnores)
            ||> Set.fold (fun dispositions signal ->
                let signal = parseUnder "initial" numbering signal

                if kernelHoldsOnlyDefault signal then
                    failwith
                        $"SignalState.initial: %O{signal} under the %O{numbering} numbering cannot have been left ignored; the kernel holds no disposition for it but the default."

                Map.add signal SignalDisposition.Ignore dispositions
            )

        {
            Numbering = numbering
            Dispositions = dispositions
            Blocked = Map.empty
            MasksToRestore = Map.empty
            Frames = Map.empty
            NextFrame = 0L
            Pending = []
        }

    /// The numbering every signal in this state is read under: the platform's
    /// (`SimulatedUnixPlatform.signalNumbering`), fixed for the process's life.
    let numbering (state : SignalState<'Task, 'Handler>) : SignalNumbering = state.Numbering

    /// What a delivery of `signal` would do now. A client asks
    /// `UnixSignal.sigaction`.
    let internal disposition (signal : Signal) (state : SignalState<'Task, 'Handler>) : SignalDisposition<'Handler> =
        match Map.tryFind (parse "disposition" state signal) state.Dispositions with
        | Some disposition -> disposition
        | None -> SignalDisposition.Default

    /// Every signal whose disposition is not the default.
    let internal dispositions (state : SignalState<'Task, 'Handler>) : Map<Signal, SignalDisposition<'Handler>> =
        state.Dispositions

    /// Set `signal`'s disposition, as `sigaction(2)` does. A client calls
    /// `UnixSignal.sigaction`, which refuses what the kernel refuses before
    /// it gets here.
    ///
    /// A disposition that ignores the signal discards every instance of it
    /// already pending, for every thread and for the process: `SIG_IGN`, and
    /// `SIG_DFL` for a signal whose default is to discard it or to continue
    /// the process (so a pending SIGCONT is discarded by restoring its
    /// default). Any other disposition leaves pending instances to be
    /// delivered under it.
    ///
    /// Fails loud on SIGKILL and SIGSTOP, for which the kernel refuses any
    /// disposition, and `UnixSignal.sigaction` answers EINVAL.
    let internal setDisposition
        (signal : Signal)
        (disposition : SignalDisposition<'Handler>)
        (state : SignalState<'Task, 'Handler>)
        : SignalState<'Task, 'Handler>
        =
        let signal = parse "setDisposition" state signal

        if kernelHoldsOnlyDefault signal then
            failwith
                $"SignalState.setDisposition: no kernel disposition but the default can exist for %O{signal} under the %O{state.Numbering} numbering; UnixSignal.sigaction refuses it with EINVAL before it reaches here."

        let disposition =
            match disposition with
            | SignalDisposition.Default
            | SignalDisposition.Ignore -> disposition
            | SignalDisposition.Catch action ->
                SignalDisposition.Catch
                    { action with
                        Mask = maskable "setDisposition" state.Numbering action.Mask
                    }

        // Measured on Linux 6.18.5 and Darwin 25.6.0 (2026-09-23), and swept
        // again on Linux 6.18.5 and Darwin 27.0.0 (2026-09-26) over every
        // signal sigaction accepts, from each of SIG_DFL, SIG_IGN and a
        // handler to each of those and a second handler, blocked and pending
        // in either direction
        // (docs/plans/2026-08-23-posix-kernel-extraction/signal-disposition-table.c):
        // exactly the changes `discardsPendingWhenSet` names emptied
        // sigpending, whatever the disposition before, and the rest were
        // delivered once unblocked.
        let pending =
            if discardsPendingWhenSet state.Numbering disposition signal then
                state.Pending |> List.filter (fun entry -> entry.Signal <> signal)
            else
                state.Pending

        { state with
            Dispositions = withDisposition signal disposition state.Dispositions
            Pending = pending
        }

    /// `task`'s handler frames, innermost first: the ones
    /// `UnixSignal.onReturnToUser` has pushed and `UnixSignal.sigreturn` has not
    /// yet popped.
    let framesOf (task : 'Task) (state : SignalState<'Task, 'Handler>) : HandlerFrame<'Task, 'Handler> list =
        match Map.tryFind task state.Frames with
        | Some frames -> frames
        | None -> []

    /// Every task with a handler frame.
    let tasksWithFrames (state : SignalState<'Task, 'Handler>) : Set<'Task> = state.Frames |> Map.keys |> Set.ofSeq

    /// `task`'s signal mask: what `sigprocmask(2)` would answer as its old
    /// mask.
    let maskOf (task : 'Task) (state : SignalState<'Task, 'Handler>) : SignalMask =
        match Map.tryFind task state.Blocked with
        | Some mask -> mask
        | None -> SignalMask.empty

    /// Every task whose mask blocks anything.
    let tasksWithMasks (state : SignalState<'Task, 'Handler>) : Set<'Task> = state.Blocked |> Map.keys |> Set.ofSeq

    /// `state` with `task`'s mask `mask`, already screened by `maskable`.
    let private withMask
        (task : 'Task)
        (mask : SignalMask)
        (state : SignalState<'Task, 'Handler>)
        : SignalState<'Task, 'Handler>
        =
        { state with
            Blocked =
                if SignalMask.isEmpty mask then
                    Map.remove task state.Blocked
                else
                    Map.add task mask state.Blocked
        }

    /// `task`'s mask changed by `change` with `set`, as `sigprocmask(2)`
    /// changes it: SIGKILL and SIGSTOP are dropped from the result silently,
    /// and every other bit is kept, one that names no signal included. A
    /// client calls `UnixSignal.pthreadSigmask` or its neighbours, which decode
    /// `how` and screen what the C library screens first.
    ///
    /// Fails loudly on a set made under another numbering.
    let internal changeMask
        (change : SignalMaskChange)
        (set : SignalMask)
        (task : 'Task)
        (state : SignalState<'Task, 'Handler>)
        : SignalState<'Task, 'Handler>
        =
        let set = maskable "changeMask" state.Numbering set
        let current = maskOf task state

        let changed =
            match change with
            | SignalMaskChange.Block -> SignalMask.union current set
            | SignalMaskChange.Unblock -> SignalMask.difference current set
            | SignalMaskChange.SetMask -> set

        withMask task changed state

    /// `state` with `child` given `parent`'s mask, as a new thread starts with
    /// its creator's.
    let internal inheritMask
        (parent : 'Task)
        (child : 'Task)
        (state : SignalState<'Task, 'Handler>)
        : SignalState<'Task, 'Handler>
        =
        withMask child (maskOf parent state) state

    /// The mask `task` gets back once the `sigsuspend(2)` or `pause(2)` it is
    /// in returns: `Some` from the call until the task next returns to user
    /// mode (`UnixSignal.onReturnToUser`), and `None` otherwise.
    let maskToRestore (task : 'Task) (state : SignalState<'Task, 'Handler>) : SignalMask option =
        Map.tryFind task state.MasksToRestore

    /// Every task with a mask to restore: those in `sigsuspend(2)` or
    /// `pause(2)`, and those whose call has ended and which have not yet
    /// returned to user mode.
    let tasksWithMasksToRestore (state : SignalState<'Task, 'Handler>) : Set<'Task> =
        state.MasksToRestore |> Map.keys |> Set.ofSeq

    /// `sigsuspend(2)`'s change to `task`'s mask: `temporary` replaces it, SIGKILL
    /// and SIGSTOP dropped silently and every other bit kept, and the mask it
    /// replaces is kept to be restored as the task returns to user mode
    /// (`onReturnToUser`). Only `task`'s mask changes, on both flavours.
    ///
    /// Fails loudly if `task` already has a mask to restore (a client that
    /// made a second call without returning to user mode from the first), and
    /// on a mask made under another numbering.
    let internal suspend
        (task : 'Task)
        (temporary : SignalMask)
        (state : SignalState<'Task, 'Handler>)
        : SignalState<'Task, 'Handler>
        =
        if Map.containsKey task state.MasksToRestore then
            failwith
                $"SignalState.suspend: task %O{task} already has a mask to restore from an earlier sigsuspend, so it has not returned to user mode since, and cannot be making another call (this is a bug in the client)."

        let temporary = maskable "suspend" state.Numbering temporary

        { state with
            MasksToRestore = Map.add task (maskOf task state) state.MasksToRestore
        }
        |> withMask task temporary

    /// Drop everything held for `thread` alone: its mask, the mask a
    /// `sigsuspend(2)` it was in would have restored, its handler frames, and
    /// the signals pending on it alone, which are discarded rather than
    /// passed on to another thread.
    /// Signals pending on the process stay for another thread to take.
    ///
    /// For a thread that has exited.
    let internal forgetTask (thread : 'Task) (state : SignalState<'Task, 'Handler>) : SignalState<'Task, 'Handler> =
        // Discarded, not passed to the process: measured on Linux 6.18.5 and Darwin
        // 27.0.0 by `docs/plans/2026-08-23-posix-kernel-extraction/thread-exit-pending.c`.
        // A signal pending on an exiting thread alone was never delivered afterwards,
        // nor pending on the thread that remained, whether that thread blocked it or not.
        { state with
            Blocked = Map.remove thread state.Blocked
            MasksToRestore = Map.remove thread state.MasksToRestore
            Frames = Map.remove thread state.Frames
            Pending = state.Pending |> List.filter (fun entry -> entry.Target <> ValueSome thread)
        }

    /// The first half of generating `signal` (checked), common to `enqueue`
    /// and `generate`: `None` if the kernel discards it before it has any
    /// effect at all, and otherwise the state once its generation has
    /// discarded any pending instance of the opposite kind of signal.
    ///
    /// Darwin discards an ignored signal first, whatever any mask says, and
    /// SIGCONT alone is never discarded there. Linux discards nothing yet: an
    /// ignored signal is dropped later, once it is known some thread could
    /// receive it, and a blocked one stays pending.
    ///
    /// What survives that discards the opposite kind, on every thread and on
    /// the process: a stop signal (default Stop) discards a pending SIGCONT,
    /// and SIGCONT discards every pending stop signal, whatever the
    /// dispositions of either.
    let private beginGeneration
        (signal : Signal)
        (state : SignalState<'Task, 'Handler>)
        : SignalState<'Task, 'Handler> option
        =
        // Measured 2026-09-26 on Linux 6.18.5 and Darwin 27.0.0
        // (docs/plans/2026-08-23-posix-kernel-extraction/signal-disposition-table.c,
        // parts "fl" and "fu"): every ordered pair of distinct signals from
        // SIGTSTP, SIGTTIN, SIGTTOU, SIGCONT and a SIGUSR1 control, under
        // every pair of SIG_DFL, SIG_IGN and a handler, each generated
        // process- or thread-directed, both blocked; and each stop signal and
        // SIGCONT generated unblocked against a pending one of the other
        // kind. The only departure from "the opposite kind is discarded" is
        // Darwin's ignored stop signal, which discards nothing, being itself
        // discarded first. The same Darwin rows on 25.6.0, with handlers
        // only, agree.
        let disposition =
            match Map.tryFind signal state.Dispositions with
            | Some disposition -> disposition
            | None -> SignalDisposition.Default

        if
            ignoredAtGeneration state.Numbering disposition signal
            && not (Signal.blockedIgnoredSignalStaysPendingUnder state.Numbering signal)
        then
            None
        else

        let opposite : DefaultDisposition option =
            match Signal.defaultDispositionUnder state.Numbering signal with
            | DefaultDisposition.Stop -> Some DefaultDisposition.Continue
            | DefaultDisposition.Continue -> Some DefaultDisposition.Stop
            | DefaultDisposition.Terminate
            | DefaultDisposition.Ignore -> None

        match opposite with
        | None -> Some state
        | Some opposite ->
            let pending =
                state.Pending
                |> List.filter (fun entry -> Signal.defaultDispositionUnder state.Numbering entry.Signal <> opposite)

            if List.length pending = List.length state.Pending then
                Some state
            else
                Some
                    { state with
                        Pending = pending
                    }

    /// Where `signal` comes in the order a task takes the signals of one pending
    /// set, lowest first.
    let private pickRank (numbering : SignalNumbering) (signal : Signal) : int * int =
        // Measured by `docs/plans/2026-08-23-posix-kernel-extraction/signal-pick-order.c`
        // on Linux 6.18.5 (aarch64) and Darwin 25.6.0 and 27.0.0, whose rows
        // `TestSignalPickOrder` replays: every standard catchable signal, blocked,
        // generated in 42 orders, process- and thread-directed, then drained by
        // sigwait and by handlers that block each other; on Linux also with every
        // real-time signal, in 20 orders. Generation order never mattered. Darwin
        // took the lowest number first; Linux took SIGILL, SIGTRAP, SIGBUS, SIGFPE,
        // SIGSEGV and SIGSYS first (its `next_signal` calls them synchronous), then
        // the lowest number, so every real-time signal after every standard one.
        let signo = Signal.toRawSignoUnder numbering signal

        match numbering with
        | SignalNumbering.Linux ->
            match signal with
            | Signal.SIGILL
            | Signal.SIGTRAP
            | Signal.SIGBUS
            | Signal.SIGFPE
            | Signal.SIGSEGV
            | Signal.SIGSYS -> 0, signo
            | _ -> 1, signo
        | SignalNumbering.Darwin -> 0, signo

    /// The queueing half of generation, on an entry `beginGeneration` let
    /// through: coalesce a standard signal already pending in its set, and
    /// otherwise add it to its set in its place in the pick order, after any
    /// instance of the same real-time signal already there.
    let private admit
        (entry : PendingSignal<'Task>)
        (state : SignalState<'Task, 'Handler>)
        : SignalState<'Task, 'Handler>
        =
        let coalesced =
            not (Signal.isRealTimeUnder state.Numbering entry.Signal)
            && state.Pending
               |> List.exists (fun pending -> pending.Signal = entry.Signal && pending.Target = entry.Target)

        if coalesced then
            state
        else
            let key (pending : PendingSignal<'Task>) : 'Task voption * (int * int) =
                pending.Target, pickRank state.Numbering pending.Signal

            let entryKey = key entry

            let precedes (pending : PendingSignal<'Task>) : bool = compare (key pending) entryKey <= 0

            { state with
                Pending =
                    List.takeWhile precedes state.Pending
                    @ (entry :: List.skipWhile precedes state.Pending)
            }

    /// Add a generated signal to its pending set, checking it first, without
    /// deciding whether it takes effect at once (see `generate`,
    /// which does).
    ///
    /// A signal whose disposition at generation is "ignore" — `SIG_IGN`, or
    /// the default where that discards it — never becomes pending under
    /// Darwin's rule, whatever any mask says, unless it is SIGCONT; under
    /// Linux's it becomes pending and stays so exactly as long as no receiver
    /// could take it (see `onReturnToUser` for the delivery half of the rule).
    ///
    /// A stop signal discards any pending SIGCONT as it is generated, and
    /// SIGCONT discards any pending stop signal, on every thread and on the
    /// process; under Darwin's rule an ignored stop signal is discarded
    /// before it can.
    ///
    /// A standard signal that is already pending in the same pending set is
    /// discarded: a kernel holds at most one pending instance of a standard
    /// signal per set, and `Target` is the set — `ValueNone` the process-wide
    /// one, `ValueSome t` the thread's own. Measured on Linux 6.18.5 and
    /// Darwin 25.6.0: three process-directed `SIGUSR1` while blocked deliver
    /// once; and the sets are separate keys — a process-directed plus a
    /// thread-directed `SIGUSR2` deliver twice on Linux, and on Darwin when
    /// the thread is not the leader. A real-time signal (Linux's 32..64; see
    /// `Signal.isRealTimeUnder`) queues without coalescing.
    ///
    /// One measured divergence is not modelled here: Darwin delivers a
    /// process-directed and a thread-directed instance once, not twice, when
    /// the thread is the leader (measured on Darwin 27.0.0 by
    /// `docs/plans/2026-08-23-posix-kernel-extraction/raise-sweep.c`), because it puts a signal sent to the process
    /// in the pending set of the thread it chooses to take it. `generate`
    /// refuses to leave such a pair pending
    /// (`SignalReceiverRefusal.PendingForProcessAndLeader`); this function,
    /// which is not told the leader, holds them apart.
    let internal enqueue
        (entry : PendingSignal<'Task>)
        (state : SignalState<'Task, 'Handler>)
        : SignalState<'Task, 'Handler>
        =
        let entry =
            { entry with
                Signal = parse "enqueue" state entry.Signal
            }

        match beginGeneration entry.Signal state with
        | None -> state
        | Some state -> admit entry state

    /// Which task could take a pending signal now.
    [<RequireQualifiedAccess>]
    type private Receiver<'Task> =
        /// This task would take it.
        | Task of 'Task
        /// It is the process's own, the leader blocks it, and another task does not.
        | BeyondLeader
        /// Every task that could take it blocks it.
        | Nobody

    /// Fails loudly unless `leader` and `task` are both among `tasks`: each is a
    /// bug in the client.
    let private checkTask (operation : string) (tasks : Set<'Task>) (role : string) (task : 'Task) : unit =
        if not (Set.contains task tasks) then
            failwith $"SignalState.%s{operation}: the %s{role} %O{task} is not one of the process's tasks %O{tasks}."

    /// Which task could take `entry` now: its target, for a signal pending on
    /// one task; and for one pending on the process, the leader, unless it blocks
    /// the signal.
    let private receiverFor
        (leader : 'Task)
        (tasks : Set<'Task>)
        (entry : PendingSignal<'Task>)
        (state : SignalState<'Task, 'Handler>)
        : Receiver<'Task>
        =
        let blocks (task : 'Task) : bool =
            SignalMask.contains entry.Signal (maskOf task state)

        match entry.Target with
        | ValueSome target ->
            if not (Set.contains target tasks) then
                failwith
                    $"SignalState: %O{entry.Signal} is pending on %O{target} alone, which is not one of the process's tasks %O{tasks}; a task's own signals should have left with it (forgetTask)."

            if blocks target then
                Receiver.Nobody
            else
                Receiver.Task target
        | ValueNone ->
            // Measured by `docs/plans/2026-08-23-posix-kernel-extraction/signal-receiver.c`
            // on Linux 6.18.5 (aarch64, and x86-64 under Rosetta) and Darwin 25.6.0
            // and 27.0.0: with four threads handling SIGUSR1, each set of them
            // blocking it, each sender, and eight sends in a row, every send went to
            // the main thread whenever it did not block the signal (192 rows, 1536
            // sends, per flavour), whoever sent it and whether the others slept or
            // spun. When the main thread blocked it, Darwin chose the first-created
            // thread that did not, and Linux the next such thread from a cursor that
            // earlier deliveries move (`curr_target`).
            if not (blocks leader) then
                Receiver.Task leader
            elif tasks |> Set.exists (fun task -> not (blocks task)) then
                Receiver.BeyondLeader
            else
                Receiver.Nobody

    /// Whether a death by `signal` writes a core dump under `coreDumps`.
    let private dumpsCore (coreDumps : CoreDumps) (numbering : SignalNumbering) (signal : Signal) : bool =
        match coreDumps with
        | CoreDumps.Suppressed -> false
        | CoreDumps.Written -> Signal.dumpsCoreUnder numbering signal

    /// Generate `entry`: decide what it does to the process at once, and queue
    /// it (see `enqueue`) if its effect, if any, comes later.
    ///
    /// `tasks` are the process's tasks, and `leader` is the one of them that
    /// takes the signals sent to the process as a whole: a signal pending on the
    /// process can be received by the leader, and by no other task while the
    /// leader does not block it.
    ///
    /// What the signal does, and what is refused, are as `UnixSignal.kill`
    /// states; a termination's core flag is as `coreDumps` decides.
    ///
    /// Fails loudly unless `leader` is among `tasks`, and on a signal aimed at a
    /// task that is not.
    let internal generate
        (coreDumps : CoreDumps)
        (leader : 'Task)
        (tasks : Set<'Task>)
        (entry : PendingSignal<'Task>)
        (state : SignalState<'Task, 'Handler>)
        : Result<SignalGeneration<'Task, 'Handler>, SignalReceiverRefusal>
        =
        checkTask "generate" tasks "leader" leader

        let entry =
            { entry with
                Signal = parse "generate" state entry.Signal
            }

        match beginGeneration entry.Signal state with
        | None -> Ok (SignalGeneration.ProcessContinues state)
        | Some state ->

        // Linux decides this at generation (`complete_signal` takes the whole
        // thread group down for a fatal signal as soon as it has found a thread
        // that wants it), and SIGKILL is immediate on Darwin too. For any other
        // fatal or stopping signal, both kernels act when the receiving thread
        // next returns to user mode, which for a self-directed signal is the
        // return from the very call that generated it. They differ only in what
        // *other* threads could run in between, which this approximates as
        // nothing. Which task receives such a signal does not matter, since it
        // acts on the whole process, so it is answered even when the leader
        // blocks it.
        // Measured by `docs/plans/2026-08-23-posix-kernel-extraction/raise-sweep.c`
        // on Darwin 27.0.0, with every catchable signal blocked by every thread:
        // a `kill(2)` of the process and a `raise(3)` on the main thread, in
        // either order, by either thread, in a process of one thread or two,
        // were delivered once, to the main thread; a `kill` and a
        // `pthread_kill` of the second thread were delivered once to each.
        // Linux 6.18.5 delivered the first pair twice.
        let leftPending () : Result<SignalGeneration<'Task, 'Handler>, SignalReceiverRefusal> =
            let mergesOnDarwin =
                match state.Numbering with
                | SignalNumbering.Linux -> false
                | SignalNumbering.Darwin ->
                    let partner =
                        match entry.Target with
                        | ValueNone -> ValueSome (ValueSome leader)
                        | ValueSome target when target = leader -> ValueSome ValueNone
                        | ValueSome _ -> ValueNone

                    match partner with
                    | ValueNone -> false
                    | ValueSome partner ->
                        state.Pending
                        |> List.exists (fun pending -> pending.Signal = entry.Signal && pending.Target = partner)

            if mergesOnDarwin then
                Error (SignalReceiverRefusal.PendingForProcessAndLeader entry.Signal)
            else
                Ok (SignalGeneration.ProcessContinues (admit entry state))

        let receiver = receiverFor leader tasks entry state

        match receiver, disposition entry.Signal state with
        | Receiver.Nobody, _ -> leftPending ()
        | Receiver.BeyondLeader, SignalDisposition.Catch _ -> Error (SignalReceiverRefusal.LeaderBlocks entry.Signal)
        | Receiver.Task _, SignalDisposition.Catch _ -> leftPending ()
        // Discarded without ever being pending, on both kernels. Were it
        // queued instead, it would sit there until the client next asked
        // `onReturnToUser`, and a handler installed in between would receive a
        // signal that was ignored when it was sent.
        | _, SignalDisposition.Ignore -> Ok (SignalGeneration.ProcessContinues state)
        | _, SignalDisposition.Default ->
            match Signal.defaultDispositionUnder state.Numbering entry.Signal with
            | DefaultDisposition.Terminate ->
                Ok (SignalGeneration.ProcessTerminated (entry.Signal, dumpsCore coreDumps state.Numbering entry.Signal))
            | DefaultDisposition.Stop -> Ok (SignalGeneration.ProcessStopped (entry.Signal, state))
            | DefaultDisposition.Ignore -> Ok (SignalGeneration.ProcessContinues state)
            | DefaultDisposition.Continue ->
                // Pending until a task takes it (see `DefaultContinue`); which
                // task does is `onReturnToUser`'s, and only the leader is
                // asked.
                match receiver with
                | Receiver.BeyondLeader -> Error (SignalReceiverRefusal.LeaderBlocks entry.Signal)
                | Receiver.Task _
                | Receiver.Nobody -> leftPending ()

    /// Every pending entry: the process's own set first, then each task's, each set in the order a task
    /// takes its signals (see `pendingFor`).
    let pending (state : SignalState<'Task, 'Handler>) : PendingSignal<'Task> list = state.Pending

    /// The pending signals `task` could take, in the order it would take them
    /// were it to block none: its own, and if it is the process's `leader`, the
    /// process's too.
    ///
    /// Within one set, Darwin takes the lowest-numbered signal first, and Linux
    /// takes SIGILL, SIGTRAP, SIGBUS, SIGFPE, SIGSEGV and SIGSYS first and then
    /// the lowest-numbered; several instances of one real-time signal come in
    /// the order they were generated. Linux takes every signal of the task's own
    /// set before any of the process's; Darwin takes the two as one set.
    let pendingFor (leader : 'Task) (task : 'Task) (state : SignalState<'Task, 'Handler>) : PendingSignal<'Task> list =
        let own = state.Pending |> List.filter (fun entry -> entry.Target = ValueSome task)

        let shared =
            if task = leader then
                state.Pending |> List.filter (fun entry -> entry.Target.IsNone)
            else
                []

        match state.Numbering with
        // Measured by the pair sweep of `signal-pick-order.c` (see `pickRank`):
        // every ordered pair of distinct standard signals, the first directed at
        // the main thread and the second at the process, both blocked by it.
        // Linux delivered the thread's first in all 806 pairs that the
        // stop/continue flush left both of; Darwin, the lower-numbered first.
        | SignalNumbering.Linux -> own @ shared
        // A stable sort, so the task's own instance of a signal comes before the
        // process's; nothing measured decides between the two, and no other
        // signal can fall between them.
        | SignalNumbering.Darwin -> own @ shared |> List.sortBy (fun entry -> pickRank state.Numbering entry.Signal)

    /// What `sigpending(2)` answers for `task`: the signals pending that it
    /// would see and that its mask blocks. It sees its own set, and the
    /// process's: on Linux whichever task asks, and on Darwin only if it is the
    /// process's `leader`.
    ///
    /// Measured on Linux 6.18.5 and Darwin 27.0.0 by
    /// `docs/plans/2026-08-23-posix-kernel-extraction/sigpending-scope.c`: with
    /// a main thread and a second both blocking SIGUSR1, a `kill` of the
    /// process by either showed SIGUSR1 to both on Linux, and to the main
    /// thread alone on Darwin, which puts a signal sent to the process in the
    /// set of the thread it chooses, the main thread whenever every thread
    /// blocks it. A `pthread_kill` showed only to its target, on both.
    ///
    /// A pending signal the task does not block is left out, as Linux's
    /// `do_sigpending` leaves it out. No task can see one: it takes such a
    /// signal as it returns to user mode, before it can ask.
    let pendingBlocked (leader : 'Task) (task : 'Task) (state : SignalState<'Task, 'Handler>) : SignalMask =
        let sees (entry : PendingSignal<'Task>) : bool =
            match entry.Target with
            | ValueSome target -> target = task
            | ValueNone ->
                match state.Numbering with
                | SignalNumbering.Linux -> true
                | SignalNumbering.Darwin -> task = leader

        let mask = maskOf task state

        state.Pending
        |> List.filter (fun entry -> sees entry && SignalMask.contains entry.Signal mask)
        |> List.map (fun entry -> entry.Signal)
        |> Set.ofList
        |> SignalMask.ofSignals state.Numbering

    /// Where a task's mask stands once a handler for `signal` is delivered to it
    /// under `action`, from `mask`.
    let private maskDuring
        (numbering : SignalNumbering)
        (mask : SignalMask)
        (signal : Signal)
        (action : SignalCatch<'Handler>)
        : SignalMask
        =
        // Measured by the signal fuzzer's harness
        // (`WoofWare.PosixKernel.Test/signalFuzz/harness.c`) on Linux 6.18.5 and
        // Darwin 27.0.0, and by
        // `docs/plans/2026-08-23-posix-kernel-extraction/signal-sigaction-flags.c`
        // on Linux 6.18.5 and Darwin 25.6.0: the mask inside a handler is the
        // mask at delivery, `sa_mask`, and the signal unless `SA_NODEFER`;
        // `SA_RESETHAND` does not imply `SA_NODEFER`.
        let blocked =
            if action.NoDefer then
                SignalMask.union mask action.Mask
            else
                SignalMask.union mask action.Mask |> SignalMask.add numbering signal

        maskable "onReturnToUser" numbering blocked

    /// Whether delivering `signal` under `SA_RESETHAND` resets its disposition.
    let private resetsHand (numbering : SignalNumbering) (signal : Signal) : bool =
        // Measured by
        // `docs/plans/2026-08-23-posix-kernel-extraction/signal-sigaction-flags.c`
        // over every catchable standard signal, and by the signal fuzzer's
        // harness: Linux resets every one; Darwin every one but SIGILL and
        // SIGTRAP, which keep their handler, as POSIX permits.
        match numbering with
        | SignalNumbering.Linux -> true
        | SignalNumbering.Darwin ->
            match signal with
            | Signal.SIGILL
            | Signal.SIGTRAP -> false
            | _ -> true

    /// What `task` takes as it returns to user mode, as `onReturnToUser`
    /// decides, except that a mask to restore is left where it is: the
    /// outermost frame saves it all the same. For a caller that asks what the
    /// return would do, and keeps the walk's discards, without the task
    /// returning yet.
    let internal takeOnReturn
        (coreDumps : CoreDumps)
        (leader : 'Task)
        (tasks : Set<'Task>)
        (task : 'Task)
        (state : SignalState<'Task, 'Handler>)
        : Result<SignalDelivery<'Task, 'Handler> option * SignalState<'Task, 'Handler>, SignalReceiverRefusal>
        =
        checkTask "onReturnToUser" tasks "leader" leader
        checkTask "onReturnToUser" tasks "task asked" task

        let beyondLeader =
            state.Pending
            |> List.tryFind (fun entry ->
                match receiverFor leader tasks entry state with
                | Receiver.BeyondLeader -> true
                | Receiver.Task _
                | Receiver.Nobody -> false
            )

        match beyondLeader with
        | Some entry -> Error (SignalReceiverRefusal.LeaderBlocks entry.Signal)
        | None ->

        // The first entry equal to `entry` gone from `pending`: within one set,
        // the order `pendingFor` walks is the order `Pending` holds, so this is
        // the instance the walk reached.
        let rec without
            (entry : PendingSignal<'Task>)
            (pending : PendingSignal<'Task> list)
            : PendingSignal<'Task> list
            =
            match pending with
            | [] -> failwith $"SignalState.onReturnToUser: walked to %O{entry}, which is not pending."
            | head :: tail when head = entry -> tail
            | head :: tail -> head :: without entry tail

        let taken (entry : PendingSignal<'Task>) (state : SignalState<'Task, 'Handler>) =
            { state with
                Pending = without entry state.Pending
            }

        // A single pass is enough: pushing a frame only ever adds to the mask,
        // so an entry the walk has skipped as blocked stays blocked until the
        // walk is over.
        let rec walk
            (pushed : HandlerFrame<'Task, 'Handler> list)
            (state : SignalState<'Task, 'Handler>)
            (candidates : PendingSignal<'Task> list)
            : Result<SignalDelivery<'Task, 'Handler> option * SignalState<'Task, 'Handler>, SignalReceiverRefusal>
            =
            let finished () =
                match pushed with
                | [] -> Ok (None, state)
                | _ -> Ok (Some (SignalDelivery.RunHandlers pushed), state)

            let unlessFramesPushed (entry : PendingSignal<'Task>) (delivery : SignalDelivery<'Task, 'Handler>) =
                match pushed with
                | [] -> Ok (Some delivery, taken entry state)
                | _ -> Error (SignalReceiverRefusal.DefaultBehindHandlers entry.Signal)

            match candidates with
            | [] -> finished ()
            | entry :: rest ->

            // A blocked SIGCONT at its default stays pending too: measured on
            // Linux 6.18.5 and Darwin 27.0.0 by
            // `docs/plans/2026-08-23-posix-kernel-extraction/sigpending-scope.c`,
            // whose `sigpending` reported it after the `kill` that generated
            // it had returned.
            if SignalMask.contains entry.Signal (maskOf task state) then
                walk pushed state rest
            else

            match disposition entry.Signal state with
            | SignalDisposition.Catch action ->
                let current = maskOf task state

                // The first frame of a return from `sigsuspend` saves the mask
                // the call replaced; see `onReturnToUser`.
                let saved =
                    match pushed, Map.tryFind task state.MasksToRestore with
                    | [], Some restore -> restore
                    | _, _ -> current

                let frame =
                    {
                        Id = HandlerFrameId state.NextFrame
                        Entry = entry
                        Action = action
                        SavedMask = saved
                    }

                let dispositions =
                    if action.ResetHand && resetsHand state.Numbering entry.Signal then
                        Map.remove entry.Signal state.Dispositions
                    else
                        state.Dispositions

                let state =
                    { taken entry state with
                        Dispositions = dispositions
                        Frames = Map.add task (frame :: framesOf task state) state.Frames
                        NextFrame = state.NextFrame + 1L
                    }
                    |> withMask task (maskDuring state.Numbering current entry.Signal action)

                walk (frame :: pushed) state rest
            // Discarded with no action: drop the entry and keep walking — a
            // later entry may still deliver now.
            | SignalDisposition.Ignore -> walk pushed (taken entry state) rest
            | SignalDisposition.Default ->
                match Signal.defaultDispositionUnder state.Numbering entry.Signal with
                | DefaultDisposition.Ignore -> walk pushed (taken entry state) rest
                | DefaultDisposition.Terminate ->
                    // Fatal whatever frames were pushed before it: measured by
                    // the signal fuzzer, which delivers a caught signal and a
                    // fatal one at the same return and sees no handler run.
                    Ok (
                        Some (
                            SignalDelivery.DefaultTerminate (
                                entry.Signal,
                                dumpsCore coreDumps state.Numbering entry.Signal
                            )
                        ),
                        taken entry state
                    )
                | DefaultDisposition.Stop -> unlessFramesPushed entry (SignalDelivery.DefaultStop entry.Signal)
                | DefaultDisposition.Continue -> unlessFramesPushed entry (SignalDelivery.DefaultContinue entry.Signal)

        walk [] state (pendingFor leader task state)

    /// What `task` takes as it returns to user mode, by the rules
    /// `UnixSignal.onReturnToUser` states: `tasks` and `leader` are as
    /// `generate` takes them, so `task` takes the process's own signals as well
    /// as its own only if it is the leader, and a termination's core flag is as
    /// `coreDumps` decides.
    ///
    /// Returns the possibly-updated state in every case, because an answer can
    /// change the state without producing an action (an ignored signal is
    /// discarded), and a caller that dropped the no-action state would replay
    /// those discards forever.
    ///
    /// Fails loudly unless `leader` and `task` are among `tasks`, and on a
    /// signal pending on a task that is not.
    let internal onReturnToUser
        (coreDumps : CoreDumps)
        (leader : 'Task)
        (tasks : Set<'Task>)
        (task : 'Task)
        (state : SignalState<'Task, 'Handler>)
        : Result<SignalDelivery<'Task, 'Handler> option * SignalState<'Task, 'Handler>, SignalReceiverRefusal>
        =
        takeOnReturn coreDumps leader tasks task state
        |> Result.map (fun (delivery, after) ->
            match Map.tryFind task after.MasksToRestore with
            | None -> delivery, after
            | Some restore ->
                let cleared =
                    { after with
                        MasksToRestore = Map.remove task after.MasksToRestore
                    }

                // Measured by
                // `docs/plans/2026-08-23-posix-kernel-extraction/sigsuspend-mask.c`
                // on Linux 6.18.5 and Darwin 27.0.0: the outermost frame saved the
                // mask from before the call, and the mask after the call was that
                // mask. A return that runs no handler restores it too (Linux's
                // `restore_saved_sigmask`); the frames then hold it otherwise.
                // A default SIGCONT is no action, and Linux's `get_signal`
                // goes on past it, under the temporary mask, to the next
                // signal, which the client's next ask takes.
                match delivery with
                | Some (SignalDelivery.DefaultContinue _) -> delivery, after
                | Some (SignalDelivery.RunHandlers _)
                | Some (SignalDelivery.DefaultTerminate _)
                | Some (SignalDelivery.DefaultStop _)
                | None ->
                    if after.NextFrame <> state.NextFrame then
                        delivery, cleared
                    else
                        delivery, withMask task restore cleared
        )

    /// `sigreturn(2)`: pop `task`'s innermost frame, `frame`, as
    /// `UnixSignal.sigreturn` states.
    ///
    /// Fails loudly unless `frame` is `task`'s innermost frame: handlers return
    /// innermost first, so anything else is a bug in the client.
    let internal sigreturn
        (task : 'Task)
        (frame : HandlerFrameId)
        (state : SignalState<'Task, 'Handler>)
        : SignalState<'Task, 'Handler>
        =
        match framesOf task state with
        | innermost :: rest when innermost.Id = frame ->
            // Measured by the signals research's `sigaction_flags.c` on Linux
            // 6.18.5 and Darwin 25.6.0: a handler that unblocked SIGHUP and
            // blocked SIGTERM returned to exactly the mask at delivery.
            { state with
                Frames =
                    match rest with
                    | [] -> Map.remove task state.Frames
                    | _ -> Map.add task rest state.Frames
            }
            |> withMask task innermost.SavedMask
        | frames ->
            failwith
                $"SignalState.sigreturn: %O{frame} is not task %O{task}'s innermost handler frame (its frames, innermost first, are %O{frames |> List.map (fun f -> f.Id)})."

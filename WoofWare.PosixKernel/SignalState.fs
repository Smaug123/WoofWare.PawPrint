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
        /// SIGSTOP from it, so `SignalState` never stores either.
        Mask : Set<Signal>
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
            Mask = Set.empty
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
[<RequireQualifiedAccess>]
type CoreDumps =
    /// No dump is written, as under an `RLIMIT_CORE` of 0.
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

/// Which handler frame a delivery pushed: the name `SignalState.sigreturn`
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
        /// The task's signal mask while the handler runs: the mask in force
        /// when it was delivered, `Action.Mask`, and the signal itself unless
        /// `Action.NoDefer`. `sigreturn` restores the mask in force before.
        Mask : Set<Signal>
    }

/// What the kernel does with the signals a task takes as it returns to user
/// mode, as decided by `SignalState.onReturnToUser`: run handlers, or apply a
/// signal's kernel default. The client interprets it: runs the handlers,
/// terminates the simulated process by the signal, or refuses what it does not
/// model.
[<RequireQualifiedAccess>]
type SignalDelivery<'Task, 'Handler> =
    /// A frame for every caught signal the task takes now, pushed all at once,
    /// innermost first: the head's handler runs first, and each handler's
    /// `sigreturn` is followed by another `onReturnToUser`, which may push more
    /// frames before the next one down runs. Never empty.
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
    /// stopped process. Unlike the other cases this one is not gated on the
    /// task's mask: resumption happens at generation on a real kernel,
    /// whatever any thread's mask says — a mask defers only the handler
    /// delivery.
    | DefaultContinue of Signal

/// Pure, deterministic model of the simulator's signal-handling state.
///
/// The shape is deliberately small:
///   * `Dispositions` — what `sigaction(2)` has set for each signal: its
///     default, ignored, or caught by a client's handler. A signal absent
///     from the map has its default.
///   * `Frames` — each task's stack of handler frames. A task's signal mask is
///     the mask its innermost frame says, and empty with no frame: nothing
///     else sets one, because this library models no `sigprocmask(2)`.
///   * `Pending` — the signals generated and not yet delivered: the process's
///     own set, and each task's.
///
/// One instance of this type belongs to each simulated process. A client
/// asks it, one task at a time, what that task takes as it returns to user
/// mode; the data shape is exercised by property tests against a
/// structurally-different reference oracle.
///
/// Every `Signal` stored here is canonical under `Numbering`, and every
/// operation canonicalises the signal it is handed before touching the state:
/// `Signal.Other` is a second spelling for a named signal's number, and a
/// state that kept both spellings would let `setDisposition (Other 17)` and
/// `disposition SIGCHLD` disagree about one Linux signal. The operations are
/// the only route in (the representation is private), so the tables and
/// queue never hold an `Other` that names a case, SIGKILL or SIGSTOP in a
/// mask, a disposition for SIGKILL or SIGSTOP, a stored `Default`, or a
/// number that is not a signal under the numbering at all.
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
type SignalGeneration<'Task, 'Handler when 'Task : comparison and 'Handler : equality> =
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
    /// Validate and canonicalise a signal at the operation boundary: the named
    /// spelling if `signal` is an `Other` carrying a named signal's number,
    /// and a loud failure if it is not a signal under this numbering at all.
    /// `Signal.Other` is public and enforces nothing, so a client can hand
    /// this state a number no kernel mask or disposition table could hold;
    /// the callers that produced a raw signo honestly went through
    /// `Signal.ofRawSignoUnder`, which refuses those, so reaching this
    /// failure means a client built an `Other` some other way.
    let private parseUnder (operation : string) (numbering : SignalNumbering) (signal : Signal) : Signal =
        match signal with
        | Signal.Other rawSignal ->
            match Signal.ofRawSignoUnder numbering rawSignal with
            | ValueSome canonical -> canonical
            | ValueNone ->
                failwith
                    $"SignalState.%s{operation}: %d{rawSignal} is not a signal under the %O{numbering} numbering (signos run 1..%d{Signal.highestSignoUnder numbering}); a raw signo should have been refused at the caller's own boundary, via Signal.ofRawSignoUnder."
        | named -> named

    let private parse (operation : string) (state : SignalState<'Task, 'Handler>) (signal : Signal) : Signal =
        parseUnder operation state.Numbering signal

    /// SIGKILL and SIGSTOP, for which the kernel holds no disposition but the
    /// default. Narrower than `Signal.isUncatchableUnder`, which adds the two
    /// numbers glibc's `sigaction` refuses on top of the kernel's refusal.
    let internal kernelHoldsOnlyDefault (numbering : SignalNumbering) (signal : Signal) : bool =
        match Signal.toRawSignoUnder numbering signal, numbering with
        | 9, _
        | 19, SignalNumbering.Linux
        | 17, SignalNumbering.Darwin -> true
        | _, _ -> false

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

    /// `signals` as a kernel holds them in a mask: canonical, and without
    /// SIGKILL and SIGSTOP, which it drops silently. Measured by the signal
    /// fuzzer's harness (`WoofWare.PosixKernel.Test/signalFuzz/harness.c`) on
    /// Linux 6.18.5 and Darwin 27.0.0: a handler whose `sa_mask` named both
    /// read neither back in its mask while it ran.
    let private maskable (operation : string) (numbering : SignalNumbering) (signals : Set<Signal>) : Set<Signal> =
        signals
        |> Set.map (parseUnder operation numbering)
        |> Set.filter (fun signal -> not (kernelHoldsOnlyDefault numbering signal))

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
    /// Fails loud on a number that is not a signal under the numbering, and
    /// on SIGKILL or SIGSTOP, which no process can have ignored.
    let initial (numbering : SignalNumbering) (inheritedIgnores : Set<Signal>) : SignalState<'Task, 'Handler> =
        let dispositions =
            (Map.empty, inheritedIgnores)
            ||> Set.fold (fun dispositions signal ->
                let signal = parseUnder "initial" numbering signal

                if kernelHoldsOnlyDefault numbering signal then
                    failwith
                        $"SignalState.initial: %O{signal} under the %O{numbering} numbering cannot have been left ignored; the kernel holds no disposition for it but the default."

                Map.add signal SignalDisposition.Ignore dispositions
            )

        {
            Numbering = numbering
            Dispositions = dispositions
            Frames = Map.empty
            NextFrame = 0L
            Pending = []
        }

    /// The numbering every signal in this state is read under, as given to
    /// `initial`.
    let numbering (state : SignalState<'Task, 'Handler>) : SignalNumbering = state.Numbering

    /// What a delivery of `signal` would do now. A client asks
    /// `UnixSignal.sigaction`.
    let internal disposition (signal : Signal) (state : SignalState<'Task, 'Handler>) : SignalDisposition<'Handler> =
        match Map.tryFind (parse "disposition" state signal) state.Dispositions with
        | Some disposition -> disposition
        | None -> SignalDisposition.Default

    /// Every signal whose disposition is not the default, keyed by its
    /// canonical spelling.
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

        if kernelHoldsOnlyDefault state.Numbering signal then
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

    /// `task`'s handler frames, innermost first: the ones `onReturnToUser` has
    /// pushed and `sigreturn` has not yet popped.
    let framesOf (task : 'Task) (state : SignalState<'Task, 'Handler>) : HandlerFrame<'Task, 'Handler> list =
        match Map.tryFind task state.Frames with
        | Some frames -> frames
        | None -> []

    /// Every task with a handler frame.
    let tasksWithFrames (state : SignalState<'Task, 'Handler>) : Set<'Task> = state.Frames |> Map.keys |> Set.ofSeq

    /// `task`'s signal mask, every member in its canonical spelling: its
    /// innermost frame's, or empty.
    let maskOf (task : 'Task) (state : SignalState<'Task, 'Handler>) : Set<Signal> =
        match framesOf task state with
        | innermost :: _ -> innermost.Mask
        | [] -> Set.empty

    /// Drop everything held for `thread` alone: its handler frames, and the
    /// signals pending on it alone, which are discarded rather than passed on to
    /// another thread.
    /// Signals pending on the process stay for another thread to take.
    ///
    /// For a thread that has exited.
    let forgetTask (thread : 'Task) (state : SignalState<'Task, 'Handler>) : SignalState<'Task, 'Handler> =
        // Discarded, not passed to the process: measured on Linux 6.18.5 and Darwin
        // 27.0.0 by `docs/plans/2026-08-23-posix-kernel-extraction/thread-exit-pending.c`.
        // A signal pending on an exiting thread alone was never delivered afterwards,
        // nor pending on the thread that remained, whether that thread blocked it or not.
        { state with
            Frames = Map.remove thread state.Frames
            Pending = state.Pending |> List.filter (fun entry -> entry.Target <> ValueSome thread)
        }

    /// The first half of generating `signal` (canonical), common to `enqueue`
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
            match signo with
            | 4
            | 5
            | 7
            | 8
            | 11
            | 31 -> 0, signo
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

    /// Add a generated signal to its pending set, canonicalising its spelling
    /// first, without deciding whether it takes effect at once (see `generate`,
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
    let enqueue (entry : PendingSignal<'Task>) (state : SignalState<'Task, 'Handler>) : SignalState<'Task, 'Handler> =
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
            Set.contains entry.Signal (maskOf task state)

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
    /// A signal at its default disposition, which some task could receive,
    /// takes that default here rather than being queued: it terminates the
    /// process (writing a core dump if its default dumps core and `coreDumps`
    /// allows one), stops it, or, if the default is to discard it, is
    /// discarded. An ignored signal some task could receive is discarded too.
    /// Every other signal is queued, including one that every task blocks,
    /// which takes effect once a task can receive it; generation discards
    /// pending signals of the opposite kind first, as `enqueue` describes.
    ///
    /// Refuses a caught signal sent to the process that only a task other than
    /// the leader could receive; and, under Darwin's numbering, a standard
    /// signal it would leave pending on the process while an instance is
    /// pending on the leader alone, or the other way round, which Darwin holds
    /// as one instance where this library holds two.
    ///
    /// Fails loudly unless `leader` is among `tasks`, and on a signal aimed at a
    /// task that is not.
    let generate
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

        match receiverFor leader tasks entry state, disposition entry.Signal state with
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
            | DefaultDisposition.Continue -> leftPending ()

    /// Every pending entry, every signal in its canonical spelling: the
    /// process's own set first, then each task's, each set in the order a task
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

    /// Where a task's mask stands once a handler for `signal` is delivered to it
    /// under `action`, from `mask`.
    let private maskDuring
        (numbering : SignalNumbering)
        (mask : Set<Signal>)
        (signal : Signal)
        (action : SignalCatch<'Handler>)
        : Set<Signal>
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
                Set.union mask action.Mask
            else
                Set.union mask action.Mask |> Set.add signal

        blocked
        |> Set.filter (fun signal -> not (kernelHoldsOnlyDefault numbering signal))

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
            match Signal.toRawSignoUnder numbering signal with
            | 4
            | 5 -> false
            | _ -> true

    /// What `task` takes as it returns to user mode: `tasks` and `leader` are as
    /// `generate` takes them, so `task` takes the process's own signals as well
    /// as its own only if it is the leader. A client asks this before `task`
    /// next runs its own code: after every system call it makes, `sigreturn`
    /// included.
    ///
    /// Returns the possibly-updated state in every case, because an answer can
    /// change the state without producing an action (see the Ignore rule
    /// below), and a client that dropped the no-action state would replay those
    /// discards forever.
    ///
    /// The signals are walked in `pendingFor`'s order, skipping any that `task`'s
    /// mask blocks, and what happens to each is its disposition *now*, not at
    /// generation:
    ///   * caught — a handler frame is pushed for it, and the walk goes on
    ///     under the mask the frame says, so that every caught signal the task
    ///     can take gets a frame before any handler runs. Their handlers run
    ///     innermost first, the reverse of the order they were taken in.
    ///     Under `SA_RESETHAND` the disposition returns to the default as the
    ///     frame is pushed;
    ///   * ignored, whether by `SIG_IGN` or by a default of Ignore — it is
    ///     discarded silently, the walk continuing past it. That discard is
    ///     the delivery half of the generation rule on `enqueue`: under
    ///     Linux numbering an ignored-but-blocked signal stays pending
    ///     (measured; `Signal.blockedIgnoredSignalStaysPendingUnder`), and
    ///     what un-pends it is exactly this — it becomes receivable while
    ///     still ignored and is dropped, or a handler arrives first and it
    ///     is delivered;
    ///   * the default, where that is to terminate or stop — surfaced as its
    ///     case for the client to act on, a termination with its core flag
    ///     as `coreDumps` decides.
    ///
    /// A pending signal whose default is to continue the process, at its
    /// default, surfaces as `DefaultContinue` whether or not `task` blocks it,
    /// because resumption is a generation-time effect no mask can hold back
    /// (see the case's own docstring).
    ///
    /// Refuses, whichever task is asked, while any signal pending on the process
    /// could be received by a task other than the leader but not by the leader,
    /// unless it is one that continues the process at its default; the refusal
    /// names the first such signal in `pending`'s order. Refuses a stop or
    /// continue default reached after frames were pushed.
    ///
    /// Fails loudly unless `leader` and `task` are among `tasks`, and on a
    /// signal pending on a task that is not.
    let onReturnToUser
        (coreDumps : CoreDumps)
        (leader : 'Task)
        (tasks : Set<'Task>)
        (task : 'Task)
        (state : SignalState<'Task, 'Handler>)
        : Result<SignalDelivery<'Task, 'Handler> option * SignalState<'Task, 'Handler>, SignalReceiverRefusal>
        =
        checkTask "onReturnToUser" tasks "leader" leader
        checkTask "onReturnToUser" tasks "task asked" task

        let continuesAtDefault (state : SignalState<'Task, 'Handler>) (signal : Signal) : bool =
            disposition signal state = SignalDisposition.Default
            && Signal.defaultDispositionUnder state.Numbering signal = DefaultDisposition.Continue

        let beyondLeader =
            state.Pending
            |> List.tryFind (fun entry ->
                match receiverFor leader tasks entry state with
                | Receiver.BeyondLeader -> not (continuesAtDefault state entry.Signal)
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

            if continuesAtDefault state entry.Signal then
                // Resumption is a generation-time effect and unmaskable: a
                // kernel continues a stopped process the moment the signal is
                // generated, whatever any thread's mask says — the mask defers
                // only the *handler* delivery, which the caught arm below gates
                // correctly. One approximation until a stopped-process state
                // exists: a real kernel keeps a blocked instance pending after
                // the resume (measured on Linux 6.18.5 and Darwin 25.6.0,
                // visible to `sigpending`), where this consumes the entry with
                // the event.
                unlessFramesPushed entry (SignalDelivery.DefaultContinue entry.Signal)
            elif Set.contains entry.Signal (maskOf task state) then
                walk pushed state rest
            else

            match disposition entry.Signal state with
            | SignalDisposition.Catch action ->
                let frame =
                    {
                        Id = HandlerFrameId state.NextFrame
                        Entry = entry
                        Action = action
                        Mask = maskDuring state.Numbering (maskOf task state) entry.Signal action
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
                | DefaultDisposition.Continue ->
                    failwith
                        "SignalState.onReturnToUser: a signal that continues the process at its default was walked past its own arm."

        walk [] state (pendingFor leader task state)

    /// `sigreturn(2)`: `task`'s handler for the frame `frame` has returned, and
    /// the frame is popped, restoring the mask that was in force before it was
    /// pushed. The client then asks `onReturnToUser` again, since signals the
    /// frame blocked may now be deliverable.
    ///
    /// Fails loudly unless `frame` is `task`'s innermost frame: handlers return
    /// innermost first, so anything else is a bug in the client.
    let sigreturn
        (task : 'Task)
        (frame : HandlerFrameId)
        (state : SignalState<'Task, 'Handler>)
        : SignalState<'Task, 'Handler>
        =
        match framesOf task state with
        | innermost :: rest when innermost.Id = frame ->
            { state with
                Frames =
                    match rest with
                    | [] -> Map.remove task state.Frames
                    | _ -> Map.add task rest state.Frames
            }
        | frames ->
            failwith
                $"SignalState.sigreturn: %O{frame} is not task %O{task}'s innermost handler frame (its frames, innermost first, are %O{frames |> List.map (fun f -> f.Id)})."

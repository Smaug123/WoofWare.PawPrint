namespace WoofWare.PosixKernel

open System.Collections.Immutable

/// What a process does with a signal delivered to it: the handler half of
/// what `sigaction(2)` sets, without its mask or flags.
[<RequireQualifiedAccess>]
type SignalDisposition<'Handler> =
    /// `SIG_DFL`: the signal's kernel default, `Signal.defaultDispositionUnder`.
    | Default
    /// `SIG_IGN`: the signal is discarded.
    | Ignore
    /// A handler is installed. What `handler` is belongs to the client; this
    /// library stores it and hands it back when the signal is delivered.
    | Catch of handler : 'Handler

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

/// A signal sitting in the simulator's pending queue, waiting to be
/// dispatched. `Target = ValueNone` is the POSIX "kill the process" case
/// (any live thread that isn't blocking the signal may receive it);
/// `ValueSome` is `pthread_kill`-style directed delivery, where only the
/// named thread is eligible.
type PendingSignal<'Task> =
    {
        Signal : Signal
        Target : 'Task voption
    }

/// What the kernel does with the next receivable pending signal, as decided
/// by `SignalState.nextDelivery`: deliver it to a chosen receiver thread, so
/// that the handler the client installed for it runs, or apply the signal's
/// kernel default. The client interprets it — runs its handler, terminates
/// the simulated process by the signal, or refuses what it does not model.
[<RequireQualifiedAccess>]
type SignalDelivery<'Task, 'Handler> =
    /// `handler` is installed for `entry`'s signal: the client runs it.
    /// `receiver` is the thread the kernel chose to take the signal, which is
    /// the thread the handler interrupts.
    | RunHandler of entry : PendingSignal<'Task> * receiver : 'Task * handler : 'Handler
    /// No handler claims the signal and its kernel default is to terminate
    /// the process. A parent's `wait` then reports the process as killed by
    /// the signal (`WIFSIGNALED`, `WTERMSIG`), with the core flag set iff
    /// `coreDumped`; `128 + signo` is only how a shell renders that as an
    /// exit status.
    | DefaultTerminate of signal : Signal * coreDumped : bool
    /// No handler claims the signal and its kernel default is to suspend
    /// the whole process.
    | DefaultStop of Signal
    /// No handler claims the signal and its kernel default is to resume a
    /// stopped process. Unlike the other cases this one is not gated on
    /// masks or receivers: resumption happens at generation on a real
    /// kernel, whatever any thread's mask says — a mask defers only the
    /// handler delivery.
    | DefaultContinue of Signal

/// What generating a signal does to the process at once, as decided by
/// `SignalState.generate`. Anything else it does happens later, through the
/// pending queue and `SignalState.nextDelivery`.
[<RequireQualifiedAccess>]
type SignalGeneration =
    /// The process carries on. The signal is pending, or coalesced into an
    /// instance already pending, or discarded because it is ignored.
    | ProcessContinues
    /// The signal terminates the process: its disposition is the default,
    /// which is to terminate, and some live thread could receive it. A
    /// parent's `wait` reports the core flag iff `coreDumped`.
    | ProcessTerminated of signal : Signal * coreDumped : bool
    /// The signal stops the whole process: its disposition is the default,
    /// which is to stop, and some live thread could receive it.
    | ProcessStopped of Signal

/// Pure, deterministic model of the simulator's signal-handling state.
///
/// The shape is deliberately small:
///   * `Dispositions` — what `sigaction(2)` has set for each signal: its
///     default, ignored, or caught by a client's handler. A signal absent
///     from the map has its default.
///   * `Blocked` — per-thread sigprocmask. A signal in a thread's set is
///     blocked for that thread and cannot be delivered to it.
///   * `Pending` — FIFO queue of generated signals waiting for delivery.
///
/// One instance of this type belongs to each simulated process. A client
/// polls it for deliverable signals and dispatches out of it; the data shape
/// is exercised by property tests against a structurally-different reference
/// oracle.
///
/// Every `Signal` stored here is canonical under `Numbering`, and every
/// operation canonicalises the signal it is handed before touching the state:
/// `Signal.Other` is a second spelling for a named signal's number, and a
/// state that kept both spellings would let `setDisposition (Other 17)` and
/// `disposition SIGCHLD` disagree about one Linux signal. The operations are
/// the only route in (the representation is private), so the tables and
/// queue never hold an `Other` that names a case, an unblockable signal in a
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
            Blocked : Map<'Task, Set<Signal>>
            /// Pending entries in FIFO order (head = next candidate for
            /// dispatch). A plain list rather than `ImmutableQueue<T>`
            /// because the queue type uses reference equality, which would
            /// break the structural equality a client relies on to decide
            /// whether a step changed anything. Enqueue is O(n) on append,
            /// which is fine: signal queues are tiny in practice (typically
            /// 0–3 entries), and this model trades performance for determinism
            /// throughout.
            Pending : PendingSignal<'Task> list
        }

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
    let private kernelHoldsOnlyDefault (numbering : SignalNumbering) (signal : Signal) : bool =
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

    /// The signal state a process starts with: nothing blocked, nothing
    /// pending, and every signal at its default except those in
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
            Blocked = Map.empty
            Pending = []
        }

    /// The numbering every signal in this state is read under, as given to
    /// `initial`.
    let numbering (state : SignalState<'Task, 'Handler>) : SignalNumbering = state.Numbering

    /// What a delivery of `signal` would do now.
    let disposition (signal : Signal) (state : SignalState<'Task, 'Handler>) : SignalDisposition<'Handler> =
        match Map.tryFind (parse "disposition" state signal) state.Dispositions with
        | Some disposition -> disposition
        | None -> SignalDisposition.Default

    /// Every signal whose disposition is not the default, keyed by its
    /// canonical spelling.
    let dispositions (state : SignalState<'Task, 'Handler>) : Map<Signal, SignalDisposition<'Handler>> =
        state.Dispositions

    /// Set `signal`'s disposition, as `sigaction(2)` does.
    ///
    /// A disposition that ignores the signal discards every instance of it
    /// already pending, for every thread and for the process: `SIG_IGN`, and
    /// `SIG_DFL` for a signal whose default is to discard it or to continue
    /// the process (so a pending SIGCONT is discarded by restoring its
    /// default). Any other disposition leaves pending instances to be
    /// delivered under it.
    ///
    /// Fails loud on SIGKILL and SIGSTOP, for which the kernel refuses any
    /// disposition: a client's own `sigaction` answers those with EINVAL
    /// before any state changes. glibc's `sigaction` also refuses Linux's 32
    /// and 33 (see `Signal.isUncatchableUnder`), which the kernel itself
    /// accepts; a client modelling a call through glibc screens those first.
    let setDisposition
        (signal : Signal)
        (disposition : SignalDisposition<'Handler>)
        (state : SignalState<'Task, 'Handler>)
        : SignalState<'Task, 'Handler>
        =
        let signal = parse "setDisposition" state signal

        if kernelHoldsOnlyDefault state.Numbering signal then
            failwith
                $"SignalState.setDisposition: no kernel disposition but the default can exist for %O{signal} under the %O{state.Numbering} numbering — sigaction(2) refuses it with EINVAL, and the client should have refused it there."

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

    let isBlocked (thread : 'Task) (signal : Signal) (state : SignalState<'Task, 'Handler>) : bool =
        let signal = parse "isBlocked" state signal

        match Map.tryFind thread state.Blocked with
        | None -> false
        | Some set -> Set.contains signal set

    /// Every task with a non-empty signal mask.
    let blockedTasks (state : SignalState<'Task, 'Handler>) : Set<'Task> =
        state.Blocked |> Map.toSeq |> Seq.map fst |> Set.ofSeq

    /// `thread`'s sigprocmask, every member in its canonical spelling.
    let blockedFor (thread : 'Task) (state : SignalState<'Task, 'Handler>) : Set<Signal> =
        match Map.tryFind thread state.Blocked with
        | None -> Set.empty
        | Some set -> set

    /// Add `signal` to `thread`'s sigprocmask. Idempotent: a second `block`
    /// of an already-blocked signal is a no-op. `thread` should be one of the
    /// process's tasks: `UnixSystem.checkInvariants` reports a mask held for
    /// anything else, and `forgetTask` drops a task's mask when it exits.
    ///
    /// A signal the kernel refuses to let a thread block — SIGKILL and
    /// SIGSTOP, plus the two glibc screens out on Linux; see
    /// `Signal.isUnblockableUnder` — is silently dropped, which is
    /// `sigprocmask(2)`'s own shape: the call succeeds and the mask is simply
    /// missing the signal. Not a loud failure, because a real caller cannot
    /// tell either way except by reading the mask back.
    let block (thread : 'Task) (signal : Signal) (state : SignalState<'Task, 'Handler>) : SignalState<'Task, 'Handler> =
        let signal = parse "block" state signal

        if Signal.isUnblockableUnder state.Numbering signal then
            state
        else

        let existing =
            match Map.tryFind thread state.Blocked with
            | None -> Set.empty
            | Some set -> set

        if Set.contains signal existing then
            state
        else
            { state with
                Blocked = Map.add thread (Set.add signal existing) state.Blocked
            }

    /// Remove `signal` from `thread`'s sigprocmask. No-op if the signal
    /// wasn't blocked. When the resulting mask is empty, the thread's entry
    /// is dropped from the map so two states are structurally equal if they
    /// differ only in "absent" vs "empty" masks.
    let unblock
        (thread : 'Task)
        (signal : Signal)
        (state : SignalState<'Task, 'Handler>)
        : SignalState<'Task, 'Handler>
        =
        let signal = parse "unblock" state signal

        match Map.tryFind thread state.Blocked with
        | None -> state
        | Some set ->
            if not (Set.contains signal set) then
                state
            else
                let set' = Set.remove signal set

                let blocked =
                    if Set.isEmpty set' then
                        Map.remove thread state.Blocked
                    else
                        Map.add thread set' state.Blocked

                { state with
                    Blocked = blocked
                }

    /// Drop everything held for `thread` alone: its mask, and the signals pending
    /// on it alone, which are discarded rather than passed on to another thread.
    /// Signals pending on the process stay for another thread to take.
    ///
    /// For a thread that has exited.
    let forgetTask (thread : 'Task) (state : SignalState<'Task, 'Handler>) : SignalState<'Task, 'Handler> =
        // Discarded, not passed to the process: measured on Linux 6.18.5 and Darwin
        // 27.0.0 by `docs/plans/2026-08-23-posix-kernel-extraction/thread-exit-pending.c`.
        // A signal pending on an exiting thread alone was never delivered afterwards,
        // nor pending on the thread that remained, whether that thread blocked it or not.
        { state with
            Blocked = Map.remove thread state.Blocked
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

    /// The queueing half of generation, on an entry `beginGeneration` let
    /// through: coalesce a standard signal already pending in its set, and
    /// append anything else.
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
            { state with
                Pending = state.Pending @ [ entry ]
            }

    /// Add a generated signal to the pending queue, canonicalising its
    /// spelling first, without deciding whether it takes effect at once (see
    /// `generate`, which does).
    ///
    /// A signal whose disposition at generation is "ignore" — `SIG_IGN`, or
    /// the default where that discards it — never enters the queue under
    /// Darwin's rule, whatever any mask says, unless it is SIGCONT; under
    /// Linux's it enters and survives exactly as long as no receiver could
    /// take it (see `nextDelivery` for the delivery half of the rule).
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
    /// thread-directed `SIGUSR2` deliver twice, on Linux and on a two-thread
    /// Darwin process alike. A real-time signal (Linux's 32..64; see
    /// `Signal.isRealTimeUnder`) queues without coalescing.
    ///
    /// One measured divergence is deliberately not modelled: a
    /// *single-threaded* Darwin process delivers that process-plus-thread
    /// pair once, not twice, because xnu assigns a process-directed signal to
    /// a thread at generation time and the two instances then coalesce in
    /// that thread's set. Modelling it would need this function to resolve
    /// `ValueNone` to a thread at enqueue time, importing xnu's assignment
    /// policy for a difference nothing can yet generate; revisit when
    /// `kill(2)` or `pthread_kill(2)` is modelled for Darwin flavours.
    let enqueue (entry : PendingSignal<'Task>) (state : SignalState<'Task, 'Handler>) : SignalState<'Task, 'Handler> =
        let entry =
            { entry with
                Signal = parse "enqueue" state entry.Signal
            }

        match beginGeneration entry.Signal state with
        | None -> state
        | Some state -> admit entry state

    /// Whether a live thread could take `entry` now: its target, if it names
    /// one, is live and not blocking the signal; or, for a process-directed
    /// signal, some live thread is not blocking it. The receiver is the target,
    /// or the lowest-ordered eligible thread.
    let private receiverFor
        (liveThreads : ImmutableArray<'Task>)
        (entry : PendingSignal<'Task>)
        (state : SignalState<'Task, 'Handler>)
        : 'Task option
        =
        let blocks (thread : 'Task) : bool =
            match Map.tryFind thread state.Blocked with
            | None -> false
            | Some set -> Set.contains entry.Signal set

        match entry.Target with
        | ValueSome target ->
            if liveThreads.Contains target && not (blocks target) then
                Some target
            else
                None
        | ValueNone ->
            liveThreads
            |> Seq.filter (fun t -> not (blocks t))
            |> Seq.sortWith compare
            |> Seq.tryHead

    /// Whether a death by `signal` writes a core dump under `coreDumps`.
    let private dumpsCore (coreDumps : CoreDumps) (numbering : SignalNumbering) (signal : Signal) : bool =
        match coreDumps with
        | CoreDumps.Suppressed -> false
        | CoreDumps.Written -> Signal.dumpsCoreUnder numbering signal

    /// Generate `entry`: decide what it does to the process at once, and queue
    /// it (see `enqueue`) if its effect, if any, comes later.
    ///
    /// A signal at its default disposition, which some thread in
    /// `liveThreads` could receive, takes that default here rather than being
    /// queued: it terminates the process (writing a core dump if its default
    /// dumps core and `coreDumps` allows one), stops it, or, if the default
    /// is to discard it, is discarded. An ignored signal some thread could
    /// receive is discarded too. Every other signal is queued, including one
    /// that every thread blocks, which takes effect once a thread can receive
    /// it; generation discards pending signals of the opposite kind first,
    /// as `enqueue` describes.
    let generate
        (coreDumps : CoreDumps)
        (liveThreads : ImmutableArray<'Task>)
        (entry : PendingSignal<'Task>)
        (state : SignalState<'Task, 'Handler>)
        : SignalGeneration * SignalState<'Task, 'Handler>
        =
        let entry =
            { entry with
                Signal = parse "generate" state entry.Signal
            }

        match beginGeneration entry.Signal state with
        | None -> SignalGeneration.ProcessContinues, state
        | Some state ->

        // Linux decides this at generation (`complete_signal` takes the whole
        // thread group down for a fatal signal as soon as it has found a thread
        // that wants it), and SIGKILL is immediate on Darwin too. For any other
        // fatal or stopping signal, both kernels act when the receiving thread
        // next returns to user mode, which for a self-directed signal is the
        // return from the very call that generated it. They differ only in what
        // *other* threads could run in between, which this approximates as
        // nothing.
        let immediate =
            if (receiverFor liveThreads entry state).IsNone then
                None
            else
                match disposition entry.Signal state with
                | SignalDisposition.Catch _ -> None
                // Discarded without ever being pending, on both kernels. Were it
                // queued instead, it would sit there until the client next
                // polled `nextDelivery`, and a handler installed in between
                // would receive a signal that was ignored when it was sent.
                | SignalDisposition.Ignore -> Some SignalGeneration.ProcessContinues
                | SignalDisposition.Default ->
                    match Signal.defaultDispositionUnder state.Numbering entry.Signal with
                    | DefaultDisposition.Terminate ->
                        Some (
                            SignalGeneration.ProcessTerminated (
                                entry.Signal,
                                dumpsCore coreDumps state.Numbering entry.Signal
                            )
                        )
                    | DefaultDisposition.Stop -> Some (SignalGeneration.ProcessStopped entry.Signal)
                    | DefaultDisposition.Ignore -> Some SignalGeneration.ProcessContinues
                    | DefaultDisposition.Continue -> None

        match immediate with
        | Some effect -> effect, state
        | None -> SignalGeneration.ProcessContinues, admit entry state

    /// Snapshot of the pending queue, in FIFO order (head = next candidate),
    /// every entry's signal in its canonical spelling.
    let pending (state : SignalState<'Task, 'Handler>) : PendingSignal<'Task> list = state.Pending

    /// Walk the pending queue in FIFO order and decide what the kernel does
    /// next: deliver a caught signal to a chosen receiver, or apply a default
    /// disposition. Returns the possibly-updated state in every case, because
    /// a scan can change the state without producing an action (see the
    /// Ignore rule below), and a client that dropped the no-action state
    /// would replay those discards forever.
    ///
    /// An entry is *receivable* iff either it is `pthread_kill`-directed at a
    /// thread that is live and not blocking the signal, or it is
    /// process-directed (`Target = ValueNone`) and at least one live thread
    /// is not blocking it — in which case the lowest-id eligible thread
    /// receives it (the choice is arbitrary but must be deterministic, and
    /// "lowest id" composes well with the existing thread-scheduling
    /// conventions). A non-receivable entry stays queued; skipped entries
    /// keep their relative order.
    ///
    /// What happens to a receivable entry is its signal's disposition *now*,
    /// not at generation:
    ///   * caught — `RunHandler`, with the handler;
    ///   * ignored, whether by `SIG_IGN` or by a default of Ignore — it is
    ///     discarded silently, the scan continuing past it. That discard is
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
    /// A pending default-disposition `Continue` signal is the exception to
    /// receivability: it surfaces as `DefaultContinue` without consulting
    /// masks or receivers, because resumption is a generation-time effect no
    /// mask can hold back (see the case's own docstring).
    let nextDelivery
        (coreDumps : CoreDumps)
        (liveThreads : ImmutableArray<'Task>)
        (state : SignalState<'Task, 'Handler>)
        : SignalDelivery<'Task, 'Handler> option * SignalState<'Task, 'Handler>
        =
        let pickReceiver (entry : PendingSignal<'Task>) : 'Task option = receiverFor liveThreads entry state

        let rec scan
            (skipped : PendingSignal<'Task> list)
            (rest : PendingSignal<'Task> list)
            : SignalDelivery<'Task, 'Handler> option * PendingSignal<'Task> list
            =
            match rest with
            | [] -> None, List.rev skipped
            | head :: tail ->

            let remaining () : PendingSignal<'Task> list = List.rev skipped @ tail

            match disposition head.Signal state with
            | SignalDisposition.Catch handler ->
                match pickReceiver head with
                | None -> scan (head :: skipped) tail
                | Some receiver -> Some (SignalDelivery.RunHandler (head, receiver, handler)), remaining ()
            | SignalDisposition.Ignore ->
                match pickReceiver head with
                | None -> scan (head :: skipped) tail
                // Discarded with no action: drop the entry and keep scanning —
                // a later entry may still deliver this tick.
                | Some _ -> scan skipped tail
            | SignalDisposition.Default ->
                match Signal.defaultDispositionUnder state.Numbering head.Signal with
                | DefaultDisposition.Continue ->
                    // Resumption is a generation-time effect and unmaskable:
                    // a kernel continues a stopped process the moment the
                    // signal is generated, whatever any thread's mask says —
                    // the mask defers only the *handler* delivery, which the
                    // caught arm above gates correctly. So this surfaces
                    // without consulting masks or receivers at all. One
                    // approximation until a stopped-process state exists: a
                    // real kernel keeps a blocked instance pending after the
                    // resume (measured on Linux 6.18.5 and Darwin 25.6.0,
                    // visible to `sigpending`), where this consumes the entry
                    // with the event.
                    Some (SignalDelivery.DefaultContinue head.Signal), remaining ()
                | DefaultDisposition.Ignore ->
                    match pickReceiver head with
                    | None -> scan (head :: skipped) tail
                    | Some _ -> scan skipped tail
                | DefaultDisposition.Terminate ->
                    match pickReceiver head with
                    | None -> scan (head :: skipped) tail
                    | Some _ ->
                        Some (
                            SignalDelivery.DefaultTerminate (
                                head.Signal,
                                dumpsCore coreDumps state.Numbering head.Signal
                            )
                        ),
                        remaining ()
                | DefaultDisposition.Stop ->
                    match pickReceiver head with
                    | None -> scan (head :: skipped) tail
                    | Some _ -> Some (SignalDelivery.DefaultStop head.Signal), remaining ()

        let delivery, pending = scan [] state.Pending

        let state =
            if List.length pending = List.length state.Pending then
                state
            else
                { state with
                    Pending = pending
                }

        delivery, state

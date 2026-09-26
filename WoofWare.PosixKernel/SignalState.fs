namespace WoofWare.PosixKernel

open System.Collections.Immutable

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
/// by `SignalState.nextDelivery`: hand it to the client's installed handler
/// on a chosen receiver thread, or apply the signal's kernel default. The
/// client interprets it — runs the handler, terminates the simulated
/// process by the signal, or refuses what it does not model.
[<RequireQualifiedAccess>]
type SignalDelivery<'Task, 'Handler> =
    /// Deliver `entry` to the client's dispatch callback. `receiver` is the
    /// thread the kernel chose to take the signal; a client whose handlers
    /// all run on a dedicated dispatcher task (as CoreCLR's do) may ignore
    /// it today, but it is the thread a `pthread_kill`-style branch would
    /// interrupt.
    | RunHandler of entry : PendingSignal<'Task> * receiver : 'Task
    /// No handler claims the signal and its kernel default is to terminate
    /// the process. A parent's `wait` then reports the process as killed by
    /// the signal (`WIFSIGNALED`, `WTERMSIG`); `128 + signo` is only how a
    /// shell renders that as an exit status.
    | DefaultTerminate of Signal
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
    /// The signal terminates the process: no handler claims it, its kernel
    /// default is to terminate, and some live thread could receive it.
    | ProcessTerminated of Signal
    /// The signal stops the whole process: no handler claims it, its kernel
    /// default is to stop, and some live thread could receive it.
    | ProcessStopped of Signal

/// Pure, deterministic model of the simulator's signal-handling state.
///
/// The shape is deliberately small:
///   * `Enabled` — the set of signals the client has asked to have
///     delivered to it. This mirrors the enable bits a real signal shim
///     keeps; the mapping from a signal to whatever the client runs for it
///     is the client's own, and is none of this module's concern. A pending
///     entry whose signal is not enabled falls to its kernel default; see
///     `nextDelivery`.
///   * `Blocked` — per-thread sigprocmask. A signal in a thread's set is
///     blocked for that thread and cannot be delivered to it.
///   * `Pending` — FIFO queue of generated signals waiting for dispatch.
///
/// One instance of this type belongs to each simulated process. A client
/// polls it for deliverable signals and dispatches out of it; the data shape
/// is exercised by property tests against a structurally-different reference
/// oracle.
///
/// Every `Signal` stored here is canonical under `Numbering`, and every
/// operation canonicalises the signal it is handed before touching the state:
/// `Signal.Other` is a second spelling for a named signal's number, and a
/// state that kept both spellings would let `enable (Other 17)` and
/// `isEnabled SIGCHLD` disagree about one Linux signal. The operations are
/// the only route in (the representation is private), so the sets and queue
/// never hold an `Other` that names a case, an unblockable signal in a mask,
/// or a number that is not a signal under the numbering at all.
type SignalState<'Task, 'Handler when 'Task : comparison and 'Handler : equality> =
    private
        {
            /// Whose `<signal.h>` this process's signals are read under. Fixed
            /// at construction, like the platform it is derived from: 17 is
            /// SIGCHLD on Linux and SIGSTOP on Darwin, so no operation on this
            /// state is meaningful without it.
            Numbering : SignalNumbering
            Enabled : Set<Signal>
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
    /// The signal state a freshly-execed process starts with: nothing enabled,
    /// nothing blocked, nothing pending. `numbering` is the platform's — see
    /// `SimulatedUnixPlatform.signalNumbering` — and is fixed for the state's
    /// life; `UnixSystem.checkInvariants` refuses a system whose process reads
    /// signals under a numbering other than its machine's.
    let initial (numbering : SignalNumbering) : SignalState<'Task, 'Handler> =
        {
            Numbering = numbering
            Enabled = Set.empty
            Blocked = Map.empty
            Pending = []
        }

    /// The numbering every signal in this state is read under, as given to
    /// `initial`.
    let numbering (state : SignalState<'Task, 'Handler>) : SignalNumbering = state.Numbering

    /// Validate and canonicalise a signal at the operation boundary: the named
    /// spelling if `signal` is an `Other` carrying a named signal's number,
    /// and a loud failure if it is not a signal under this state's numbering
    /// at all. `Signal.Other` is public and enforces nothing, so a client can
    /// hand this state a number no kernel mask or disposition table could
    /// hold; the callers that produced a raw signo honestly went through
    /// `Signal.ofRawSignoUnder`, which refuses those, so reaching this
    /// failure means a client built an `Other` some other way.
    let private parse (operation : string) (state : SignalState<'Task, 'Handler>) (signal : Signal) : Signal =
        match signal with
        | Signal.Other rawSignal ->
            match Signal.ofRawSignoUnder state.Numbering rawSignal with
            | ValueSome canonical -> canonical
            | ValueNone ->
                failwith
                    $"SignalState.%s{operation}: %d{rawSignal} is not a signal under the %O{state.Numbering} numbering (signos run 1..%d{Signal.highestSignoUnder state.Numbering}); a raw signo should have been refused at the caller's own boundary, via Signal.ofRawSignoUnder."
        | named -> named

    let isEnabled (signal : Signal) (state : SignalState<'Task, 'Handler>) : bool =
        Set.contains (parse "isEnabled" state signal) state.Enabled

    /// The enabled set, every member in its canonical spelling.
    let enabled (state : SignalState<'Task, 'Handler>) : Set<Signal> = state.Enabled

    /// Mark `signal` as enabled for managed dispatch. Idempotent: a second
    /// `enable` of an already-enabled signal is a no-op. Mirrors
    /// `SystemNative_EnablePosixSignalHandling` on the C side, which flips
    /// a per-signo enable bit; the actual handler dictionary lives on the
    /// simulated managed heap.
    ///
    /// Fails loud on a signal `sigaction(2)` refuses to install a handler
    /// for: the enable bit stands for a disposition the kernel holds, and no
    /// kernel can hold one for SIGKILL or SIGSTOP. A client's own `sigaction`
    /// refuses those with EINVAL before any state changes, so reaching this
    /// failure means the client skipped that screening.
    let enable (signal : Signal) (state : SignalState<'Task, 'Handler>) : SignalState<'Task, 'Handler> =
        let signal = parse "enable" state signal

        if Signal.isUncatchableUnder state.Numbering signal then
            failwith
                $"SignalState.enable: no kernel disposition can exist for %O{signal} under the %O{state.Numbering} numbering — sigaction(2) refuses it with EINVAL, and the client should have refused it there."

        if Set.contains signal state.Enabled then
            state
        else
            { state with
                Enabled = Set.add signal state.Enabled
            }

    /// Clear the enable bit for `signal`. No-op if not enabled. Pending
    /// entries for the signal remain queued, but `nextDelivery` now applies
    /// the signal's kernel default to them rather than running the handler:
    /// a disposition is read at delivery, not at generation.
    ///
    /// Unlike `enable`, an uncatchable signal is *not* refused here: it is
    /// provably absent from the enabled set (`enable` cannot admit one), so
    /// it falls into the ordinary not-enabled no-op, and a client sweeping
    /// "disable everything" need not restate the sigaction screening.
    let disable (signal : Signal) (state : SignalState<'Task, 'Handler>) : SignalState<'Task, 'Handler> =
        let signal = parse "disable" state signal

        if Set.contains signal state.Enabled then
            { state with
                Enabled = Set.remove signal state.Enabled
            }
        else
            state

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
    /// of an already-blocked signal is a no-op. The thread does not need to
    /// be live; masks for non-live threads are harmless because dispatch
    /// already filters to the live set.
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

    /// Add a generated signal to the pending queue, canonicalising its
    /// spelling first.
    ///
    /// A signal whose disposition at generation is "ignore" — not enabled,
    /// and a kernel default of Ignore — never enters the queue under Darwin's
    /// rule, whatever any mask says; under Linux's it enters and survives
    /// exactly as long as no receiver could take it (see the comment in the
    /// body, and `nextDelivery` for the delivery half of the rule).
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

        // The generation half of the ignore rule. A signal whose disposition
        // at generation is "ignore" — no handler enabled for it, and a kernel
        // default of Ignore — is discarded at generation under Darwin's rule,
        // masks notwithstanding (measured;
        // `Signal.blockedIgnoredSignalStaysPendingUnder`). Under Linux's rule
        // it enters the queue: if every receiver blocks it, it genuinely
        // stays pending — a later handler can still claim it — and if one
        // does not, `nextDelivery`'s scan discards it. A real Linux kernel
        // drops that receivable case at generation, which is what `generate`
        // does; this function has no live threads to decide receivability
        // with, so it leaves the case to the scan.
        let ignoredNow =
            not (Set.contains entry.Signal state.Enabled)
            && Signal.defaultDispositionUnder state.Numbering entry.Signal = DefaultDisposition.Ignore

        if ignoredNow && not (Signal.blockedIgnoredSignalStaysPendingUnder state.Numbering) then
            state
        else

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

    /// Generate `entry`: decide what it does to the process at once, and queue
    /// it (see `enqueue`) if its effect, if any, comes later.
    ///
    /// A signal that no handler claims, and which some thread in `liveThreads`
    /// could receive, takes its kernel default here rather than being queued:
    /// it terminates the process, stops it, or, if the default is to ignore it,
    /// is discarded. Every other signal is queued, including one that every
    /// thread blocks, which takes effect once a thread can receive it.
    let generate
        (liveThreads : ImmutableArray<'Task>)
        (entry : PendingSignal<'Task>)
        (state : SignalState<'Task, 'Handler>)
        : SignalGeneration * SignalState<'Task, 'Handler>
        =
        let entry =
            { entry with
                Signal = parse "generate" state entry.Signal
            }

        // Linux decides this at generation (`complete_signal` takes the whole
        // thread group down for a fatal signal as soon as it has found a thread
        // that wants it), and SIGKILL is immediate on Darwin too. For any other
        // fatal or stopping signal, both kernels act when the receiving thread
        // next returns to user mode, which for a self-directed signal is the
        // return from the very call that generated it. They differ only in what
        // *other* threads could run in between, which this approximates as
        // nothing.
        let claimedByHandler = Set.contains entry.Signal state.Enabled

        let immediate =
            if claimedByHandler || (receiverFor liveThreads entry state).IsNone then
                None
            else
                match Signal.defaultDispositionUnder state.Numbering entry.Signal with
                | DefaultDisposition.Terminate -> Some (SignalGeneration.ProcessTerminated entry.Signal)
                | DefaultDisposition.Stop -> Some (SignalGeneration.ProcessStopped entry.Signal)
                // Discarded without ever being pending, on both kernels. Were it
                // queued instead, it would sit there until the client next
                // polled `nextDelivery`, and a handler installed in between
                // would receive a signal that was ignored when it was sent.
                | DefaultDisposition.Ignore -> Some SignalGeneration.ProcessContinues
                | DefaultDisposition.Continue -> None

        match immediate with
        | Some effect -> effect, state
        | None -> SignalGeneration.ProcessContinues, enqueue entry state

    /// Snapshot of the pending queue, in FIFO order (head = next candidate),
    /// every entry's signal in its canonical spelling.
    let pending (state : SignalState<'Task, 'Handler>) : PendingSignal<'Task> list = state.Pending

    /// Walk the pending queue in FIFO order and decide what the kernel does
    /// next: hand a signal to the installed handler on a chosen receiver, or
    /// apply a default disposition. Returns the possibly-updated state in
    /// every case, because a scan can change the state without producing an
    /// action (see the Ignore rule below), and a client that dropped the
    /// no-action state would replay those discards forever.
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
    ///   * enabled — `RunHandler`;
    ///   * not enabled — the kernel default applies: `Terminate` and `Stop`
    ///     surface as their cases for the client to act on, and Ignore is
    ///     discarded silently, the scan continuing past it. That discard is
    ///     the delivery half of the generation rule on `enqueue`: under
    ///     Linux numbering an ignored-but-blocked signal stays pending
    ///     (measured; `Signal.blockedIgnoredSignalStaysPendingUnder`), and
    ///     what un-pends it is exactly this — it becomes receivable while
    ///     still ignored and is dropped, or a handler arrives first and it
    ///     is delivered.
    ///
    /// A pending non-enabled `Continue`-default signal is the exception to
    /// receivability: it surfaces as `DefaultContinue` without consulting
    /// masks or receivers, because resumption is a generation-time effect no
    /// mask can hold back (see the case's own docstring).
    let nextDelivery
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

            if Set.contains head.Signal state.Enabled then
                match pickReceiver head with
                | None -> scan (head :: skipped) tail
                | Some receiver -> Some (SignalDelivery.RunHandler (head, receiver)), remaining ()
            else
                match Signal.defaultDispositionUnder state.Numbering head.Signal with
                | DefaultDisposition.Continue ->
                    // Resumption is a generation-time effect and unmaskable:
                    // a kernel continues a stopped process the moment the
                    // signal is generated, whatever any thread's mask says —
                    // the mask defers only the *handler* delivery, which the
                    // enabled arm above gates correctly. So this surfaces
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
                    | Some _ ->
                        // Discarded with no action: drop the entry and keep
                        // scanning — a later entry may still deliver this
                        // tick.
                        scan skipped tail
                | DefaultDisposition.Terminate ->
                    match pickReceiver head with
                    | None -> scan (head :: skipped) tail
                    | Some _ -> Some (SignalDelivery.DefaultTerminate head.Signal), remaining ()
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

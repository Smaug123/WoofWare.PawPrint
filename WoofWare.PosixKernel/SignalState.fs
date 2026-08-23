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

/// Initialisation state of the simulator's signal subsystem. Mirrors
/// real CoreCLR's lazy setup: the C side spins up a dedicated
/// `SignalHandlerLoop` pthread the first time
/// its signal handling is initialised. A client mirrors that contract by
/// allocating a single parked dispatcher
/// task at the same moment and stashing it here. Encoding the
/// pair as a DU rather than `Initialized : bool + DispatcherTask :
/// 'Task option` makes the invariant — "the dispatcher
/// task exists iff signal handling is initialised" — unrepresentable
/// to violate. Idempotent re-initialisation is a transition this DU
/// observes (the existing dispatcher is preserved); the QCall site
/// must check `isInitialized` before allocating a thread, otherwise a
/// second init call would mint a dead second dispatcher.
[<RequireQualifiedAccess>]
type SignalInitState<'Task> =
    /// Signal handling has not yet been set up; no dispatcher thread
    /// exists. `SignalState.empty` starts here.
    | NotInitialized
    /// The client has initialised signal handling at least
    /// `SystemNative_InitializeTerminalAndSignalHandling` at least
    /// once; `dispatcher` identifies the client's signal-dispatch task,
    /// allocated at that moment. What that task *is* is the client's
    /// business — this type only records which one it was, so that the
    /// "exists iff initialised" invariant has somewhere to live.
    | Initialized of dispatcher : 'Task

/// Pure, deterministic model of the simulator's signal-handling state.
///
/// The shape is deliberately small:
///   * `Init` — has the client initialised signal handling, and
///     `SystemNative_InitializeTerminalAndSignalHandling` yet, and
///     which task is the dispatcher? Several console paths gate
///     work behind initialisation, and read the dispatcher off this
///     field.
///   * `Enabled` — the set of signals the client has asked to have
///     delivered to it. This mirrors the enable bits a real signal shim
///     keeps; the mapping from a signal to whatever the client runs for it
///     is the client's own, and is none of this module's concern. A pending
///     entry whose signal is not enabled stays queued; the consumer decides
///     whether to drop it or wait for an enable.
///   * `Blocked` — per-thread sigprocmask. A signal in a thread's set is
///     blocked for that thread and cannot be delivered to it.
///   * `Pending` — FIFO queue of generated signals waiting for dispatch.
///   * `Handler` — the client's dispatch callback, whatever identifies one
///     to it. `None` until the client installs one; a real signal shim
///     likewise leaves its global handler pointer NULL until the first
///     registration and ignores delivered signals while it remains so, so a
///     consumer should leave pending entries queued while this is `None`.
///
/// One instance of this type belongs to each simulated process. A client
/// polls it for deliverable signals and dispatches out of it; the data shape
/// is exercised by property tests against a structurally-different reference
/// oracle.
type SignalState<'Task, 'Handler when 'Task : comparison and 'Handler : equality> =
    private
        {
            Init : SignalInitState<'Task>
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
            Handler : 'Handler option
        }

[<RequireQualifiedAccess>]
module SignalState =
    let empty : SignalState<'Task, 'Handler> =
        {
            Init = SignalInitState.NotInitialized
            Enabled = Set.empty
            Blocked = Map.empty
            Pending = []
            Handler = None
        }

    let isInitialized (state : SignalState<'Task, 'Handler>) : bool =
        match state.Init with
        | SignalInitState.NotInitialized -> false
        | SignalInitState.Initialized _ -> true

    /// `Some dispatcher` once signal handling has been initialised, where
    /// `dispatcher` identifies the client's signal-dispatch task, spawned at
    /// that moment. `None` until the client first initialises signal
    /// handling.
    /// Mirrors real CoreCLR's `SignalHandlerLoop` pthread, which is
    /// created at the same point in startup.
    let signalThread (state : SignalState<'Task, 'Handler>) : 'Task option =
        match state.Init with
        | SignalInitState.NotInitialized -> None
        | SignalInitState.Initialized dispatcher -> Some dispatcher

    /// Idempotent: a second call preserves the existing dispatcher and
    /// does *not* swap in the caller-supplied one. The
    /// caller is expected to guard with `isInitialized` and skip thread
    /// allocation entirely on the second call; the idempotency here is a
    /// defence in depth so a defensive caller does not accidentally
    /// orphan an already-allocated dispatcher task. Mirrors the usual
    /// client shape, where an `EnsureInitialized` may run more than once but
    /// the underlying signal apparatus is set up exactly once.
    let markInitialized (dispatcher : 'Task) (state : SignalState<'Task, 'Handler>) : SignalState<'Task, 'Handler> =
        match state.Init with
        | SignalInitState.Initialized _ -> state
        | SignalInitState.NotInitialized ->
            { state with
                Init = SignalInitState.Initialized dispatcher
            }

    /// The currently-installed dispatch callback, or `None` if the client has
    /// not yet registered one. A consumer reads it at the moment of dispatch.
    let handler (state : SignalState<'Task, 'Handler>) : 'Handler option = state.Handler

    /// Install (or replace) the client's signal-dispatch callback.
    /// A real signal shim stores the pointer into its global handler slot
    /// unconditionally, overwriting any prior value, so the contract is "last
    /// writer wins". Note the consequence for a client whose handler identity
    /// is a wrapper it re-constructs: two installs of the same handler are
    /// equal, so the state transition is idempotent, which is what lets a
    /// caller re-register without perturbing a state that is compared for
    /// equality.
    let setHandler (handler : 'Handler) (state : SignalState<'Task, 'Handler>) : SignalState<'Task, 'Handler> =
        { state with
            Handler = Some handler
        }

    let isEnabled (signal : Signal) (state : SignalState<'Task, 'Handler>) : bool = Set.contains signal state.Enabled

    let enabled (state : SignalState<'Task, 'Handler>) : Set<Signal> = state.Enabled

    /// Mark `signal` as enabled for managed dispatch. Idempotent: a second
    /// `enable` of an already-enabled signal is a no-op. Mirrors
    /// `SystemNative_EnablePosixSignalHandling` on the C side, which flips
    /// a per-signo enable bit; the actual handler dictionary lives on the
    /// simulated managed heap.
    let enable (signal : Signal) (state : SignalState<'Task, 'Handler>) : SignalState<'Task, 'Handler> =
        if Set.contains signal state.Enabled then
            state
        else
            { state with
                Enabled = Set.add signal state.Enabled
            }

    /// Clear the enable bit for `signal`. No-op if not enabled. Pending
    /// entries for the signal remain queued (a future `enable` makes them
    /// deliverable) but `tryDeliverable` will not dispatch them in the
    /// meantime.
    let disable (signal : Signal) (state : SignalState<'Task, 'Handler>) : SignalState<'Task, 'Handler> =
        if Set.contains signal state.Enabled then
            { state with
                Enabled = Set.remove signal state.Enabled
            }
        else
            state

    let isBlocked (thread : 'Task) (signal : Signal) (state : SignalState<'Task, 'Handler>) : bool =
        match Map.tryFind thread state.Blocked with
        | None -> false
        | Some set -> Set.contains signal set

    let blockedFor (thread : 'Task) (state : SignalState<'Task, 'Handler>) : Set<Signal> =
        match Map.tryFind thread state.Blocked with
        | None -> Set.empty
        | Some set -> set

    /// Add `signal` to `thread`'s sigprocmask. Idempotent: a second `block`
    /// of an already-blocked signal is a no-op. The thread does not need to
    /// be live; masks for non-live threads are harmless because dispatch
    /// already filters to the live set.
    let block (thread : 'Task) (signal : Signal) (state : SignalState<'Task, 'Handler>) : SignalState<'Task, 'Handler> =
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

    /// Append a signal to the back of the pending queue. Two enqueues of the
    /// same signal are not coalesced; POSIX allows duplicates for real-time
    /// signals and we preserve identity here so callers can detect collapse
    /// elsewhere if they want it.
    let enqueue (entry : PendingSignal<'Task>) (state : SignalState<'Task, 'Handler>) : SignalState<'Task, 'Handler> =
        { state with
            Pending = state.Pending @ [ entry ]
        }

    /// Snapshot of the pending queue, in FIFO order (head = next candidate).
    let pending (state : SignalState<'Task, 'Handler>) : PendingSignal<'Task> list = state.Pending

    /// Walk the pending queue in FIFO order and return the first entry that
    /// can be delivered now. An entry is deliverable iff:
    ///   * its `Signal` is enabled (the client has asked to receive it);
    ///   * either it is `pthread_kill`-directed at a thread that is live and
    ///     not blocking the signal, or
    ///   * it is process-directed (`Target = ValueNone`) and at least one
    ///     live thread is not blocking the signal.
    ///
    /// For process-directed delivery, the lowest-id eligible thread receives
    /// the signal — the choice is arbitrary but must be deterministic, and
    /// "lowest id" composes well with the existing thread-scheduling
    /// conventions.
    ///
    /// Skipped (non-deliverable) entries keep their relative order in the
    /// queue. Returns `None` if no entry is deliverable.
    let tryDeliverable
        (liveThreads : ImmutableArray<'Task>)
        (state : SignalState<'Task, 'Handler>)
        : (PendingSignal<'Task> * 'Task * SignalState<'Task, 'Handler>) option
        =
        let liveSet : Set<'Task> = liveThreads |> Seq.toList |> Set.ofList

        let sortedLive : 'Task list = liveThreads |> Seq.toList |> List.sort

        let pickReceiver (entry : PendingSignal<'Task>) : 'Task option =
            match entry.Target with
            | ValueSome tid ->
                if Set.contains tid liveSet && not (isBlocked tid entry.Signal state) then
                    Some tid
                else
                    None
            | ValueNone -> sortedLive |> List.tryFind (fun tid -> not (isBlocked tid entry.Signal state))

        let rec scan (skipped : PendingSignal<'Task> list) (rest : PendingSignal<'Task> list) =
            match rest with
            | [] -> None
            | head :: tail ->
                if not (Set.contains head.Signal state.Enabled) then
                    scan (head :: skipped) tail
                else
                    match pickReceiver head with
                    | None -> scan (head :: skipped) tail
                    | Some tid ->
                        let remaining : PendingSignal<'Task> list = List.rev skipped @ tail

                        Some (
                            head,
                            tid,
                            { state with
                                Pending = remaining
                            }
                        )

        scan [] state.Pending

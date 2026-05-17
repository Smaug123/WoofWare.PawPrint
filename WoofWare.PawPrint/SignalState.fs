namespace WoofWare.PawPrint

open System.Collections.Immutable

/// A signal sitting in the simulator's pending queue, waiting to be
/// dispatched. `Target = ValueNone` is the POSIX "kill the process" case
/// (any live thread that isn't blocking the signal may receive it);
/// `ValueSome` is `pthread_kill`-style directed delivery, where only the
/// named thread is eligible.
type PendingSignal =
    {
        Signal : Signal
        Target : ThreadId voption
    }

/// Pure, deterministic model of the simulator's signal-handling state.
///
/// The shape is deliberately small:
///   * `Initialized` — has the BCL invoked
///     `SystemNative_InitializeTerminalAndSignalHandling` yet? Several
///     console paths gate work behind this flag.
///   * `Registrations` — the per-signal dispatch target. A signal that is
///     not in this map has no handler installed; pending entries for such
///     signals stay queued (the consumer decides whether to drop or wait
///     for a registration).
///   * `Blocked` — per-thread sigprocmask. A signal in a thread's set is
///     blocked for that thread and cannot be delivered to it.
///   * `Pending` — FIFO queue of generated signals waiting for dispatch.
///
/// The `EmulatedKernel.Signals` field carries an instance of this type per
/// simulated process, but no code dispatches signals out of it yet —
/// downstream slices will plug in the harness-side injection point, the
/// between-instruction delivery hook, and the native P/Invoke arms that
/// register and block. The data shape itself is exercised end-to-end by
/// property tests against a reference oracle.
type SignalState =
    private
        {
            Initialized : bool
            Registrations : Map<Signal, SignalHandler>
            Blocked : Map<ThreadId, Set<Signal>>
            /// Pending entries in FIFO order (head = next candidate for
            /// dispatch). A plain list rather than `ImmutableQueue<T>`
            /// because the queue type uses reference equality, which would
            /// break the structural equality `EmulatedKernel` relies on for
            /// deterministic state comparison. Enqueue is O(n) on append,
            /// which is fine: signal queues are tiny in practice (typically
            /// 0–3 entries) and PawPrint trades performance for determinism
            /// throughout.
            Pending : PendingSignal list
        }

[<RequireQualifiedAccess>]
module SignalState =
    let empty : SignalState =
        {
            Initialized = false
            Registrations = Map.empty
            Blocked = Map.empty
            Pending = []
        }

    let isInitialized (state : SignalState) : bool = state.Initialized

    /// Idempotent: a second call is a no-op. Mirrors the BCL behaviour where
    /// `EnsureInitialized` may run more than once across the BCL surface but
    /// the underlying signal apparatus is set up exactly once.
    let markInitialized (state : SignalState) : SignalState =
        if state.Initialized then
            state
        else
            { state with
                Initialized = true
            }

    let tryGetHandler (signal : Signal) (state : SignalState) : SignalHandler option =
        Map.tryFind signal state.Registrations

    let registrations (state : SignalState) : Map<Signal, SignalHandler> = state.Registrations

    /// Install or replace the handler for `signal`. POSIX permits exactly
    /// one disposition per signal at a time; a second `register` overwrites
    /// the first.
    let register (signal : Signal) (handler : SignalHandler) (state : SignalState) : SignalState =
        { state with
            Registrations = Map.add signal handler state.Registrations
        }

    /// Remove the handler for `signal`. After this, pending entries for the
    /// signal remain queued (a future `register` will make them deliverable)
    /// but `tryDeliverable` will not dispatch them in the meantime. No-op if
    /// no handler was installed.
    let unregister (signal : Signal) (state : SignalState) : SignalState =
        { state with
            Registrations = Map.remove signal state.Registrations
        }

    let isBlocked (thread : ThreadId) (signal : Signal) (state : SignalState) : bool =
        match Map.tryFind thread state.Blocked with
        | None -> false
        | Some set -> Set.contains signal set

    let blockedFor (thread : ThreadId) (state : SignalState) : Set<Signal> =
        match Map.tryFind thread state.Blocked with
        | None -> Set.empty
        | Some set -> set

    /// Add `signal` to `thread`'s sigprocmask. Idempotent: a second `block`
    /// of an already-blocked signal is a no-op. The thread does not need to
    /// be live; masks for non-live threads are harmless because dispatch
    /// already filters to the live set.
    let block (thread : ThreadId) (signal : Signal) (state : SignalState) : SignalState =
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
    let unblock (thread : ThreadId) (signal : Signal) (state : SignalState) : SignalState =
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
    let enqueue (entry : PendingSignal) (state : SignalState) : SignalState =
        { state with
            Pending = state.Pending @ [ entry ]
        }

    /// Snapshot of the pending queue, in FIFO order (head = next candidate).
    let pending (state : SignalState) : PendingSignal list = state.Pending

    /// Walk the pending queue in FIFO order and return the first entry that
    /// can be delivered now. An entry is deliverable iff:
    ///   * a handler is registered for its `Signal`;
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
        (liveThreads : ImmutableArray<ThreadId>)
        (state : SignalState)
        : (PendingSignal * ThreadId * SignalHandler * SignalState) option
        =
        let liveSet : Set<ThreadId> = liveThreads |> Seq.toList |> Set.ofList

        let sortedLive : ThreadId list =
            liveThreads |> Seq.toList |> List.sortBy (fun (ThreadId.ThreadId tid) -> tid)

        let pickReceiver (entry : PendingSignal) : ThreadId option =
            match entry.Target with
            | ValueSome tid ->
                if Set.contains tid liveSet && not (isBlocked tid entry.Signal state) then
                    Some tid
                else
                    None
            | ValueNone -> sortedLive |> List.tryFind (fun tid -> not (isBlocked tid entry.Signal state))

        let rec scan (skipped : PendingSignal list) (rest : PendingSignal list) =
            match rest with
            | [] -> None
            | head :: tail ->
                match Map.tryFind head.Signal state.Registrations with
                | None -> scan (head :: skipped) tail
                | Some handler ->
                    match pickReceiver head with
                    | None -> scan (head :: skipped) tail
                    | Some tid ->
                        let remaining : PendingSignal list = List.rev skipped @ tail

                        Some (
                            head,
                            tid,
                            handler,
                            { state with
                                Pending = remaining
                            }
                        )

        scan [] state.Pending

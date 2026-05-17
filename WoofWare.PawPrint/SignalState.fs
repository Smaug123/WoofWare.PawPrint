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
///   * `Enabled` — the set of signals the BCL has asked libSystem.Native to
///     deliver to managed code via `SystemNative_EnablePosixSignalHandling`.
///     This mirrors the enable bits that the real native side keeps; the
///     mapping from signals to managed handlers lives in the simulated
///     managed heap (the BCL maintains its own `static Dictionary<int, ...>`
///     keyed by signo), and is none of this module's concern. A pending
///     entry whose signal is not enabled stays queued; the consumer decides
///     whether to drop or wait for an enable.
///   * `Blocked` — per-thread sigprocmask. A signal in a thread's set is
///     blocked for that thread and cannot be delivered to it.
///   * `Pending` — FIFO queue of generated signals waiting for dispatch.
///
/// The `EmulatedKernel.Signals` field carries an instance of this type per
/// simulated process, but no code dispatches signals out of it yet —
/// downstream slices will plug in the harness-side injection point and the
/// between-instruction delivery hook. The data shape itself is exercised
/// end-to-end by property tests against a reference oracle.
type SignalState =
    private
        {
            Initialized : bool
            Enabled : Set<Signal>
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
            Enabled = Set.empty
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

    let isEnabled (signal : Signal) (state : SignalState) : bool = Set.contains signal state.Enabled

    let enabled (state : SignalState) : Set<Signal> = state.Enabled

    /// Mark `signal` as enabled for managed dispatch. Idempotent: a second
    /// `enable` of an already-enabled signal is a no-op. Mirrors
    /// `SystemNative_EnablePosixSignalHandling` on the C side, which flips
    /// a per-signo enable bit; the actual handler dictionary lives on the
    /// simulated managed heap.
    let enable (signal : Signal) (state : SignalState) : SignalState =
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
    let disable (signal : Signal) (state : SignalState) : SignalState =
        if Set.contains signal state.Enabled then
            { state with
                Enabled = Set.remove signal state.Enabled
            }
        else
            state

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
    ///   * its `Signal` is enabled (the BCL has asked for managed dispatch);
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
        : (PendingSignal * ThreadId * SignalState) option
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
                if not (Set.contains head.Signal state.Enabled) then
                    scan (head :: skipped) tail
                else
                    match pickReceiver head with
                    | None -> scan (head :: skipped) tail
                    | Some tid ->
                        let remaining : PendingSignal list = List.rev skipped @ tail

                        Some (
                            head,
                            tid,
                            { state with
                                Pending = remaining
                            }
                        )

        scan [] state.Pending

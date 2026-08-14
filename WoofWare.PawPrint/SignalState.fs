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

/// Identity of the managed callback the BCL installed via
/// `SystemNative_SetPosixSignalHandler(&OnPosixSignal)`. Wraps the
/// `MethodInfo` of the target so a later signal-delivery slice has the
/// call site pre-resolved — no need to round-trip through raw pointer
/// bits.
///
/// The wrapper exists purely so `SignalState` keeps clean structural
/// equality: `MethodInfo<_,_,_>` carries `ImmutableArray` fields and a
/// `MethodBody` DU whose payloads use reference equality, so naked
/// `MethodInfo` equality is unstable. `MethodInfo.NominallyEqual` is the
/// stable identity contract (assembly + type identity + type generics +
/// method handle + method generics) and is exactly what we need here:
/// two `SignalHandler`s denote the same callback iff they would dispatch
/// to the same managed method. Mirrors the same pattern used by
/// `NativeIntSource.FunctionPointer`'s custom equality at the eval-stack
/// layer.
[<CustomEquality>]
[<NoComparison>]
type SignalHandler =
    private
        {
            Method : MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
        }

    member this.MethodInfo : MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> =
        this.Method

    override this.Equals (other : obj) : bool =
        match other with
        | :? SignalHandler as other -> MethodInfo.NominallyEqual this.Method other.Method
        | _ -> false

    override this.GetHashCode () : int =
        hash (this.Method.Owner, this.Method.IdentityKey, this.Method.Generics)

[<RequireQualifiedAccess>]
module SignalHandler =
    let ofMethodInfo (mi : MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>) : SignalHandler =
        {
            Method = mi
        }

    let methodInfo (handler : SignalHandler) : MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> =
        handler.Method

/// Initialisation state of the simulator's signal subsystem. Mirrors
/// real CoreCLR's lazy setup: the C side spins up a dedicated
/// `SignalHandlerLoop` pthread the first time
/// `SystemNative_InitializeTerminalAndSignalHandling` runs. PawPrint
/// follows that contract by allocating a single Parked dispatcher
/// `ThreadId` at the same moment and stashing it here. Encoding the
/// pair as a DU rather than `Initialized : bool + DispatcherThread :
/// ThreadId option` makes the load-bearing invariant — "the dispatcher
/// thread exists iff signal handling is initialised" — unrepresentable
/// to violate. Idempotent re-initialisation is a transition this DU
/// observes (the existing dispatcher is preserved); the QCall site
/// must check `isInitialized` before allocating a thread, otherwise a
/// second init call would mint a dead second dispatcher.
[<RequireQualifiedAccess>]
type SignalInitState =
    /// Signal handling has not yet been set up; no dispatcher thread
    /// exists. `SignalState.empty` starts here.
    | NotInitialized
    /// The BCL has invoked
    /// `SystemNative_InitializeTerminalAndSignalHandling` at least
    /// once; `dispatcher` is the `ThreadId` of the PawPrint-internal
    /// signal-dispatch thread allocated at that moment. The thread
    /// lives in `IlMachineState.ThreadState` with status
    /// `ThreadStatus.Parked`; a future slice will introduce a wakeup
    /// edge that transitions it to `Runnable` to actually invoke a
    /// handler. Until then the dispatcher thread is a placeholder
    /// that pins down the structural shape ahead of dispatch wiring.
    | Initialized of dispatcher : ThreadId

/// Pure, deterministic model of the simulator's signal-handling state.
///
/// The shape is deliberately small:
///   * `Init` — has the BCL invoked
///     `SystemNative_InitializeTerminalAndSignalHandling` yet, and
///     which `ThreadId` is the dispatcher? Several console paths gate
///     work behind initialisation; downstream signal-delivery slices
///     read the dispatcher id off this field.
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
///   * `Handler` — the BCL-supplied dispatch callback (the function
///     pointer passed to `SystemNative_SetPosixSignalHandler`, in
///     practice `PosixSignalRegistration.OnPosixSignal`). `None` until
///     the BCL installs one; on a real Unix box the native side leaves
///     `g_posixSignalHandler` NULL until the first
///     `SetPosixSignalHandler` call and ignores delivered signals while
///     it remains so. PawPrint will mirror that semantics once signal
///     delivery is wired.
///
/// The `EmulatedKernel.Signals` field carries an instance of this type per
/// simulated process, but no code dispatches signals out of it yet —
/// downstream slices will plug in the harness-side injection point and the
/// between-instruction delivery hook. The data shape itself is exercised
/// end-to-end by property tests against a reference oracle.
type SignalState =
    private
        {
            Init : SignalInitState
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
            Handler : SignalHandler option
        }

[<RequireQualifiedAccess>]
module SignalState =
    let empty : SignalState =
        {
            Init = SignalInitState.NotInitialized
            Enabled = Set.empty
            Blocked = Map.empty
            Pending = []
            Handler = None
        }

    let isInitialized (state : SignalState) : bool =
        match state.Init with
        | SignalInitState.NotInitialized -> false
        | SignalInitState.Initialized _ -> true

    /// `Some dispatcher` once signal handling has been initialised, where
    /// `dispatcher` is the `ThreadId` of the PawPrint-internal Parked
    /// signal-dispatch thread spawned at that moment. `None` until the BCL
    /// first calls `SystemNative_InitializeTerminalAndSignalHandling`.
    /// Mirrors real CoreCLR's `SignalHandlerLoop` pthread, which is
    /// created at the same point in startup.
    let signalThread (state : SignalState) : ThreadId option =
        match state.Init with
        | SignalInitState.NotInitialized -> None
        | SignalInitState.Initialized dispatcher -> Some dispatcher

    /// Idempotent: a second call preserves the existing dispatcher
    /// `ThreadId` and does *not* swap in the caller-supplied one. The
    /// caller is expected to guard with `isInitialized` and skip thread
    /// allocation entirely on the second call; the idempotency here is a
    /// defence in depth so a defensive caller does not accidentally
    /// orphan an already-allocated dispatcher thread. Mirrors the BCL
    /// behaviour where `EnsureInitialized` may run more than once across
    /// the BCL surface but the underlying signal apparatus is set up
    /// exactly once.
    let markInitialized (dispatcher : ThreadId) (state : SignalState) : SignalState =
        match state.Init with
        | SignalInitState.Initialized _ -> state
        | SignalInitState.NotInitialized ->
            { state with
                Init = SignalInitState.Initialized dispatcher
            }

    /// The currently-installed dispatch callback, or `None` if the BCL
    /// has not yet called `SystemNative_SetPosixSignalHandler`. PawPrint
    /// has no signal delivery wired yet, so this is purely a record of
    /// what the BCL asked for; later slices will read it at the moment
    /// of dispatch.
    let handler (state : SignalState) : SignalHandler option = state.Handler

    /// Install (or replace) the BCL-supplied signal-dispatch callback.
    /// The real native side stores the pointer into `g_posixSignalHandler`
    /// unconditionally, overwriting any prior value — which CoreLib only
    /// ever does once from `PosixSignalRegistration.Initialize`, but the
    /// underlying contract is "last writer wins". Two calls with the same
    /// nominal handler are equality-equal via `SignalHandler.Equals` even
    /// though the second one re-constructs the wrapper, so the state
    /// transition is idempotent in practice.
    let setHandler (handler : SignalHandler) (state : SignalState) : SignalState =
        { state with
            Handler = Some handler
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

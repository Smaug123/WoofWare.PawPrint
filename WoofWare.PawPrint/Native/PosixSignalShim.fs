namespace WoofWare.PawPrint

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
    /// exists. `SignalState.initial` starts here.
    | NotInitialized
    /// The client has initialised signal handling at least once;
    /// `dispatcher` identifies the client's signal-dispatch task,
    /// allocated at that moment. What that task *is* is the client's
    /// business — this type only records which one it was, so that the
    /// "exists iff initialised" invariant has somewhere to live.
    | Initialized of dispatcher : 'Task

/// What System.Native's signal code keeps in its own globals rather than
/// asking the kernel for: whether signal handling is initialised, with the
/// dispatcher thread that initialisation started, and the managed callback
/// installed by `SystemNative_SetPosixSignalHandler`.
type PosixSignalShim =
    private
        {
            Init : SignalInitState<ThreadId>
            Handler : SignalHandler option
        }

[<RequireQualifiedAccess>]
module PosixSignalShim =
    /// The shim as a process starts it: not initialised, no callback.
    let initial : PosixSignalShim =
        {
            Init = SignalInitState.NotInitialized
            Handler = None
        }

    let isInitialized (state : PosixSignalShim) : bool =
        match state.Init with
        | SignalInitState.NotInitialized -> false
        | SignalInitState.Initialized _ -> true

    /// `Some dispatcher` once signal handling has been initialised, where
    /// `dispatcher` identifies the client's signal-dispatch task, spawned at
    /// that moment. `None` until the client first initialises signal
    /// handling.
    /// Mirrors real CoreCLR's `SignalHandlerLoop` pthread, which is
    /// created at the same point in startup.
    let signalThread (state : PosixSignalShim) : ThreadId option =
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
    let markInitialized (dispatcher : ThreadId) (state : PosixSignalShim) : PosixSignalShim =
        match state.Init with
        | SignalInitState.Initialized _ -> state
        | SignalInitState.NotInitialized ->
            { state with
                Init = SignalInitState.Initialized dispatcher
            }

    /// The currently-installed dispatch callback, or `None` if the client has
    /// not yet registered one. A consumer reads it at the moment of dispatch.
    let handler (state : PosixSignalShim) : SignalHandler option = state.Handler

    /// Install (or replace) the client's signal-dispatch callback.
    /// A real signal shim stores the pointer into its global handler slot
    /// unconditionally, overwriting any prior value, so the contract is "last
    /// writer wins". Note the consequence for a client whose handler identity
    /// is a wrapper it re-constructs: two installs of the same handler are
    /// equal, so the state transition is idempotent, which is what lets a
    /// caller re-register without perturbing a state that is compared for
    /// equality.
    let setHandler (handler : SignalHandler) (state : PosixSignalShim) : PosixSignalShim =
        { state with
            Handler = Some handler
        }

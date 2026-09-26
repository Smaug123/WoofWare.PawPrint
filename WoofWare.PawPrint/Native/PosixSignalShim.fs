namespace WoofWare.PawPrint

/// Whether System.Native's signal handling has been initialised, and if so
/// which thread is its dispatcher. The first
/// `SystemNative_InitializeTerminalAndSignalHandling` starts the shim's
/// `SignalHandlerLoop` pthread, which reads each caught signal off the shim's
/// pipe and calls the managed callback; PawPrint allocates a parked thread at
/// the same moment to play that part (`IlMachineState.allocateParkedThread`)
/// and records it here.
///
/// A DU rather than `Initialized : bool` beside `Dispatcher : ThreadId option`,
/// so that "the dispatcher exists iff signal handling is initialised" cannot be
/// violated. Re-initialising preserves the existing dispatcher, so the
/// P/Invoke's handler must check `PosixSignalShim.isInitialized` before it
/// allocates a thread, or a second call would mint a second dispatcher that
/// nothing ever wakes.
[<RequireQualifiedAccess>]
type SignalInitState =
    /// Signal handling has not yet been set up; no dispatcher thread
    /// exists. `PosixSignalShim.initial` starts here.
    | NotInitialized
    /// Signal handling has been initialised at least once, and `dispatcher`
    /// is the parked thread allocated then.
    | Initialized of dispatcher : ThreadId

/// What System.Native's signal code keeps in its own globals rather than
/// asking the kernel for: whether signal handling is initialised, with the
/// dispatcher thread that initialisation started, and the managed callback
/// installed by `SystemNative_SetPosixSignalHandler` (the shim's
/// `g_posixSignalHandler`).
///
/// The kernel's half of signal handling (which signals have a handler, what
/// is pending, what each thread blocks) is `SignalState`, on the process.
/// `EmulatedKernel.checkInvariants` refuses a dispatcher that is not one of
/// the kernel's tasks.
type PosixSignalShim =
    private
        {
            Init : SignalInitState
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

    /// Whether `SystemNative_InitializeTerminalAndSignalHandling` has run.
    let isInitialized (state : PosixSignalShim) : bool =
        match state.Init with
        | SignalInitState.NotInitialized -> false
        | SignalInitState.Initialized _ -> true

    /// `Some dispatcher` once signal handling has been initialised, where
    /// `dispatcher` is the parked thread that plays the shim's
    /// `SignalHandlerLoop`. `None` until then.
    let signalThread (state : PosixSignalShim) : ThreadId option =
        match state.Init with
        | SignalInitState.NotInitialized -> None
        | SignalInitState.Initialized dispatcher -> Some dispatcher

    /// Record `dispatcher` as the thread initialisation started. Idempotent:
    /// once initialised, a second call preserves the existing dispatcher and
    /// does *not* swap in the one supplied. The caller is expected to check
    /// `isInitialized` and skip allocating a thread entirely on a second
    /// initialisation, as the shim starts its `SignalHandlerLoop` exactly
    /// once however often the BCL's initialisers call it; the idempotency
    /// here means a caller that allocated anyway does not orphan the thread
    /// already running.
    let markInitialized (dispatcher : ThreadId) (state : PosixSignalShim) : PosixSignalShim =
        match state.Init with
        | SignalInitState.Initialized _ -> state
        | SignalInitState.NotInitialized ->
            { state with
                Init = SignalInitState.Initialized dispatcher
            }

    /// The managed callback `SystemNative_SetPosixSignalHandler` installed, or
    /// `None` if it has not been called. Read at the moment of dispatch.
    let handler (state : PosixSignalShim) : SignalHandler option = state.Handler

    /// Install (or replace) the managed callback, as
    /// `SystemNative_SetPosixSignalHandler` does. The shim stores the pointer
    /// into `g_posixSignalHandler` unconditionally, so the last writer wins
    /// (a debug build of the shim also asserts that the slot was empty or
    /// already held this callback; the BCL sets it once). Two installs of the
    /// same method are equal `SignalHandler`s, so re-installing one leaves the
    /// shim equal to what it was, which is what lets a caller re-register
    /// without perturbing a state that is compared for equality.
    let setHandler (handler : SignalHandler) (state : PosixSignalShim) : PosixSignalShim =
        { state with
            Handler = Some handler
        }

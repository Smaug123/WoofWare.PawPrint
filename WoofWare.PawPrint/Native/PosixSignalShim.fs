namespace WoofWare.PawPrint

open WoofWare.PosixKernel

/// The two descriptors of System.Native's signal pipe, as its signal
/// initialisation's `pipe()` returned them. The shim's native handler writes
/// each signal it catches to `WriteEnd`, as the signal's number in one byte;
/// its dispatcher thread reads them from `ReadEnd` one at a time, in the order
/// they were written.
///
/// These are the numbers the shim holds on to, which are not necessarily what
/// the descriptor table still says: a guest can close or replace either one,
/// and the shim goes on using the number.
type SignalPipe =
    {
        ReadEnd : int
        WriteEnd : int
    }

/// Whether System.Native's signal handling has been initialised, and if so
/// which thread is its dispatcher and which pipe feeds it. The first
/// `SystemNative_InitializeTerminalAndSignalHandling` makes the pipe and
/// starts the shim's `SignalHandlerLoop` pthread, which reads each caught
/// signal off the pipe and calls the managed callback; PawPrint allocates a
/// parked thread at the same moment to play that part
/// (`IlMachineState.allocateParkedThread`) and records it here.
///
/// A DU rather than `Initialized : bool` beside `Dispatcher : ThreadId option`,
/// so that "the dispatcher and the pipe exist iff signal handling is
/// initialised" cannot be violated. Re-initialising preserves the existing
/// dispatcher and pipe, so the P/Invoke's handler must check
/// `PosixSignalShim.isInitialized` before it makes either, or a second call
/// would mint a second dispatcher that nothing ever wakes.
[<RequireQualifiedAccess>]
type SignalInitState =
    /// Signal handling has not yet been set up; no dispatcher thread
    /// exists. `PosixSignalShim.initial` starts here.
    | NotInitialized
    /// Signal handling has been initialised at least once: `dispatcher` is
    /// the parked thread allocated then, and `pipe` the pipe made then.
    | Initialized of dispatcher : ThreadId * pipe : SignalPipe

/// What System.Native's signal code keeps in its own globals rather than
/// asking the kernel for: whether signal handling is initialised, with the
/// dispatcher thread and the pipe that initialisation made
/// (`g_signalPipe`), the managed callback installed by
/// `SystemNative_SetPosixSignalHandler` (`g_posixSignalHandler`), which
/// signals have a managed registration (`g_hasPosixSignalRegistrations`),
/// and the disposition each signal had when the shim last looked, which it
/// restores when it gives the signal up (`g_origSigHandler`).
///
/// The kernel's half of signal handling (each signal's disposition, what is
/// pending, what each thread blocks) is `SignalState`, on the process; the
/// signals the native handler has taken and the dispatcher has not yet read
/// are the bytes in the pipe, in the kernel's pipe table.
/// `EmulatedKernel.checkInvariants` refuses a dispatcher that is not one of
/// the kernel's tasks.
type PosixSignalShim =
    private
        {
            Init : SignalInitState
            Handler : SignalHandler option
            /// Every signal with a managed registration, in its canonical
            /// spelling: set by `SystemNative_EnablePosixSignalHandling` when it
            /// installs the handler, cleared by
            /// `SystemNative_DisablePosixSignalHandling`. The dispatcher reads it
            /// for each signal it takes off the pipe, and passes a signal without
            /// one to `SystemNative_HandleNonCanceledPosixSignal` rather than to
            /// the callback.
            Registered : Set<Signal>
            /// The signal number the dispatcher is calling the managed callback
            /// for, while it is: the loop's local `signalCode`, which it hands
            /// to `SystemNative_HandleNonCanceledPosixSignal` if the callback
            /// reports the signal unhandled. `None` while the dispatcher is
            /// reading the pipe.
            Calling : int option
            /// Never holds `SignalDisposition.Default`: the shim allocates the
            /// array zeroed, which is `SIG_DFL`, so an absent key is the
            /// default.
            Originals : Map<Signal, SignalDisposition<NativeSignalHandler>>
            /// The signals `installHandler` has handled since `restoreHandler`
            /// last restored them (`g_handlerIsInstalled`), in their canonical
            /// spelling: an ignored signal, left ignored, as well as one it put
            /// its handler on. A record of the shim's own, not of the kernel's
            /// dispositions: the runtime's fault handler, run first by the
            /// shim's, can restore a default over the shim's handler, and the
            /// signal stays installed as far as the shim knows. The console
            /// signals `saveConsoleSignals` records are not here, because their
            /// installation is not modelled.
            Installed : Set<Signal>
        }

[<RequireQualifiedAccess>]
module PosixSignalShim =
    /// The shim as a process starts it: not initialised, no callback, and
    /// every saved disposition the default.
    let initial : PosixSignalShim =
        {
            Init = SignalInitState.NotInitialized
            Handler = None
            Registered = Set.empty
            Calling = None
            Originals = Map.empty
            Installed = Set.empty
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
        | SignalInitState.Initialized (dispatcher, _) -> Some dispatcher

    /// `Some pipe` once signal handling has been initialised, where `pipe` is
    /// the pipe initialisation made. `None` until then, when the shim's
    /// `g_signalPipe` holds -1 for both ends.
    let signalPipe (state : PosixSignalShim) : SignalPipe option =
        match state.Init with
        | SignalInitState.NotInitialized -> None
        | SignalInitState.Initialized (_, pipe) -> Some pipe

    /// Record `dispatcher` as the thread initialisation started, and `pipe` as
    /// the pipe it made. Idempotent: once initialised, a second call preserves
    /// the existing dispatcher and pipe and does *not* swap in the ones
    /// supplied. The caller is expected to check `isInitialized` and skip
    /// making either entirely on a second initialisation, as the shim makes its
    /// pipe and starts its `SignalHandlerLoop` exactly once however often the
    /// BCL's initialisers call it; the idempotency here means a caller that
    /// made them anyway does not orphan the ones already in use.
    let markInitialized (dispatcher : ThreadId) (pipe : SignalPipe) (state : PosixSignalShim) : PosixSignalShim =
        match state.Init with
        | SignalInitState.Initialized _ -> state
        | SignalInitState.NotInitialized ->
            { state with
                Init = SignalInitState.Initialized (dispatcher, pipe)
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

    /// The disposition the shim saved for `signal` when it last installed its
    /// handler for it, which is what it restores when it gives the signal up,
    /// and which its handler runs first (see `chainsToNativeHandler`). The
    /// default if it never has.
    let original
        (numbering : SignalNumbering)
        (signal : Signal)
        (state : PosixSignalShim)
        : SignalDisposition<NativeSignalHandler>
        =
        match Map.tryFind (Signal.canonicalUnder numbering signal) state.Originals with
        | Some disposition -> disposition
        | None -> SignalDisposition.Default

    /// The signals whose default the shim treats as a termination a managed
    /// handler may cancel (`IsCancelableTerminationSignal`): its handler
    /// does not run the disposition it replaced for these.
    let isCancelableTermination (numbering : SignalNumbering) (signal : Signal) : bool =
        match Signal.canonicalUnder numbering signal with
        | Signal.SIGINT
        | Signal.SIGQUIT
        | Signal.SIGTERM -> true
        | _ -> false

    /// The disposition System.Native's handler for `signal` runs before it
    /// hands the signal to the dispatcher: the one it replaced, if that was a
    /// handler and `signal` is not a cancellable termination. `None` if it
    /// runs nothing first.
    let chainsToNativeHandler
        (numbering : SignalNumbering)
        (signal : Signal)
        (state : PosixSignalShim)
        : NativeSignalHandler option
        =
        match original numbering signal state with
        | SignalDisposition.Catch action when not (isCancelableTermination numbering signal) -> Some action.Handler
        | SignalDisposition.Catch _
        | SignalDisposition.Default
        | SignalDisposition.Ignore -> None

    /// `sigaction(signal, newAction)` through the C library, as the shim calls
    /// it.
    let private sigaction<'Task when 'Task : comparison>
        (numbering : SignalNumbering)
        (signal : Signal)
        (newAction : SignalDisposition<NativeSignalHandler> option)
        (system : UnixSystem<'Task, NativeSignalHandler>)
        : Result<SignalDisposition<NativeSignalHandler> * UnixSystem<'Task, NativeSignalHandler>, UnixError>
        =
        UnixSignal.sigaction (Signal.toRawSignoUnder numbering signal) newAction system

    /// `InstallSignalHandler`: install System.Native's handler for `signal`,
    /// saving the disposition it replaces. The shim respects an ignored
    /// signal, and leaves it ignored (saving that too); and it installs its
    /// handler only once, so a signal it has handled since it last restored
    /// it is left alone, whatever its disposition now.
    ///
    /// `Error` is the errno `sigaction` refused the signal with, for SIGKILL
    /// and SIGSTOP and for a number the C library keeps for itself; the shim
    /// then records nothing.
    let installHandler<'Task when 'Task : comparison>
        (numbering : SignalNumbering)
        (signal : Signal)
        (system : UnixSystem<'Task, NativeSignalHandler>)
        (state : PosixSignalShim)
        : Result<UnixSystem<'Task, NativeSignalHandler> * PosixSignalShim, UnixError>
        =
        let signal = Signal.canonicalUnder numbering signal

        let save (disposition : SignalDisposition<NativeSignalHandler>) : PosixSignalShim =
            let installed = Set.add signal state.Installed

            match disposition with
            | SignalDisposition.Default ->
                { state with
                    Originals = Map.remove signal state.Originals
                    Installed = installed
                }
            | SignalDisposition.Ignore
            | SignalDisposition.Catch _ ->
                { state with
                    Originals = Map.add signal disposition state.Originals
                    Installed = installed
                }

        if Set.contains signal state.Installed then
            Ok (system, state)
        else

        // `sigaction(sig, NULL, orig)` first, to respect an ignore.
        match sigaction numbering signal None system with
        | Error errno -> Error errno
        | Ok (current, _) ->

        match current with
        | SignalDisposition.Catch {
                                      Handler = NativeSignalHandler.SystemNative
                                  } ->
            failwith
                $"PosixSignalShim.installHandler: %O{signal} is caught by System.Native's handler, which only this function installs, but the shim does not record installing it."
        | SignalDisposition.Ignore -> Ok (system, save SignalDisposition.Ignore)
        | SignalDisposition.Default
        | SignalDisposition.Catch _ ->
            // `InstallSignalHandler` takes `SA_RESTART | SA_SIGINFO` and an
            // empty `sa_mask` over `SIG_DFL`; over a handler it keeps that
            // handler's mask and flags, less `SA_RESTART` and `SA_RESETHAND`,
            // and then adds `SA_RESTART` back.
            let action =
                match current with
                | SignalDisposition.Catch replaced ->
                    { replaced with
                        Handler = NativeSignalHandler.SystemNative
                        ResetHand = false
                        Restart = true
                    }
                | SignalDisposition.Default
                | SignalDisposition.Ignore ->
                    { SignalCatch.ofHandler NativeSignalHandler.SystemNative with
                        Restart = true
                    }

            match sigaction numbering signal (Some (SignalDisposition.Catch action)) system with
            | Error errno -> Error errno
            | Ok (replaced, system) -> Ok (system, save replaced)

    /// What `InitializeSignalHandlingCore` does to the shim's saved
    /// dispositions: it installs System.Native's handler for SIGINT, SIGQUIT
    /// and SIGCONT, for the console, saving each one's disposition as
    /// `installHandler` does, so that a later `restoreHandler` or non-cancelled
    /// handling of one of them finds it. An ignored one stays ignored.
    ///
    /// Only the saving is modelled: the kernel's dispositions for the three
    /// are left as they were rather than becoming System.Native's handler, so
    /// that a signal sent to the process before the guest registers one
    /// still takes its disposition directly. Signals the shim initialises
    /// only once; call this on its first initialisation.
    let saveConsoleSignals<'Task when 'Task : comparison>
        (numbering : SignalNumbering)
        (system : UnixSystem<'Task, NativeSignalHandler>)
        (state : PosixSignalShim)
        : PosixSignalShim
        =
        (state, [ Signal.SIGINT ; Signal.SIGQUIT ; Signal.SIGCONT ])
        ||> List.fold (fun state signal ->
            let disposition =
                match sigaction numbering signal None system with
                | Ok (disposition, _) -> disposition
                | Error errno ->
                    failwith
                        $"PosixSignalShim.saveConsoleSignals: sigaction will not report %O{signal}'s disposition (%O{errno}), though every flavour has the signal and lets it be caught."

            match disposition with
            | SignalDisposition.Default ->
                { state with
                    Originals = Map.remove (Signal.canonicalUnder numbering signal) state.Originals
                }
            | SignalDisposition.Catch {
                                          Handler = NativeSignalHandler.SystemNative
                                      } ->
                failwith
                    $"PosixSignalShim.saveConsoleSignals: %O{signal} is already caught by System.Native's handler before the shim is initialised."
            | SignalDisposition.Ignore
            | SignalDisposition.Catch _ ->
                { state with
                    Originals = Map.add (Signal.canonicalUnder numbering signal) disposition state.Originals
                }
        )

    /// `RestoreSignalHandler`: put back the disposition the shim saved for
    /// `signal`, which is the default if it never installed a handler for it,
    /// and forget that it installed one, so that `installHandler` installs it
    /// afresh.
    ///
    /// The shim does not check its `sigaction`: where that is refused (SIGKILL
    /// and SIGSTOP, and a number the C library keeps for itself) the
    /// disposition stays as it was, the shim forgets the installation all the
    /// same, and `Some errno` is the errno the refusal leaves behind.
    let restoreHandler<'Task when 'Task : comparison>
        (numbering : SignalNumbering)
        (signal : Signal)
        (system : UnixSystem<'Task, NativeSignalHandler>)
        (state : PosixSignalShim)
        : UnixSystem<'Task, NativeSignalHandler> * PosixSignalShim * UnixError option
        =
        let state =
            { state with
                Installed = Set.remove (Signal.canonicalUnder numbering signal) state.Installed
            }

        match sigaction numbering signal (Some (original numbering signal state)) system with
        | Ok (_, system) -> system, state, None
        | Error errno -> system, state, Some errno

    /// Whether `signal` has a managed registration: whether the dispatcher
    /// hands it to the callback (`g_hasPosixSignalRegistrations`).
    let isRegistered (numbering : SignalNumbering) (signal : Signal) (state : PosixSignalShim) : bool =
        Set.contains (Signal.canonicalUnder numbering signal) state.Registered

    /// `SystemNative_EnablePosixSignalHandling`: `installHandler`, and then the
    /// registration, which is set if the handler was installed and cleared if
    /// it was not. `Error` is the errno `installHandler` failed with.
    let enable<'Task when 'Task : comparison>
        (numbering : SignalNumbering)
        (signal : Signal)
        (system : UnixSystem<'Task, NativeSignalHandler>)
        (state : PosixSignalShim)
        : Result<UnixSystem<'Task, NativeSignalHandler>, UnixError> * PosixSignalShim
        =
        let signal = Signal.canonicalUnder numbering signal

        match installHandler numbering signal system state with
        | Ok (system, state) ->
            Ok system,
            { state with
                Registered = Set.add signal state.Registered
            }
        | Error errno ->
            Error errno,
            { state with
                Registered = Set.remove signal state.Registered
            }

    /// `SystemNative_DisablePosixSignalHandling`: the registration goes, and
    /// then `restoreHandler`, whose unchecked failure this passes on.
    let disable<'Task when 'Task : comparison>
        (numbering : SignalNumbering)
        (signal : Signal)
        (system : UnixSystem<'Task, NativeSignalHandler>)
        (state : PosixSignalShim)
        : UnixSystem<'Task, NativeSignalHandler> * PosixSignalShim * UnixError option
        =
        restoreHandler
            numbering
            signal
            system
            { state with
                Registered = Set.remove (Signal.canonicalUnder numbering signal) state.Registered
            }

    /// Record that the dispatcher is calling the managed callback for
    /// `signo`. Fails if it already is: the loop calls it for one signal at a
    /// time.
    let beginCallback (signo : int) (state : PosixSignalShim) : PosixSignalShim =
        match state.Calling with
        | Some calling ->
            failwith
                $"PosixSignalShim.beginCallback: the dispatcher is already calling the callback for signal %d{calling}, and cannot begin one for %d{signo}."
        | None ->
            { state with
                Calling = Some signo
            }

    /// The signal number the dispatcher was calling the managed callback for,
    /// now that the callback has returned, and the shim with the call over.
    /// Fails if no call was in progress.
    let endCallback (state : PosixSignalShim) : int * PosixSignalShim =
        match state.Calling with
        | None -> failwith "PosixSignalShim.endCallback: the dispatcher is not calling the callback for any signal."
        | Some signo ->
            signo,
            { state with
                Calling = None
            }

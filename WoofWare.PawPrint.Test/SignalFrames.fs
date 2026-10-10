namespace WoofWare.PawPrint.Test

open WoofWare.PawPrint
open WoofWare.PosixKernel

/// A thread inside a signal handler, for a test that needs a thread to block
/// signals the way a handler's mask blocks them. PawPrint itself never leaves
/// a frame pushed between instructions, so this is a state only a test builds.
[<RequireQualifiedAccess>]
module SignalFrames =

    /// The signal delivered to put a thread in a handler: SIGPROF. A test
    /// using this does not use it otherwise.
    let private carrier : Signal = Signal.SIGPROF

    /// `system` with `task` inside a handler whose `sa_mask` is `mask`, with
    /// `SA_NODEFER`, for `carrier`, whose disposition is put back as it was.
    /// Fails the test unless the return to user mode delivers the carrier
    /// alone.
    let enterSystem<'Task when 'Task : comparison>
        (task : 'Task)
        (mask : Set<Signal>)
        (system : UnixSystem<'Task, NativeSignalHandler>)
        : UnixSystem<'Task, NativeSignalHandler>
        =
        let before = KernelSignals.disposition carrier system

        let numbering = SimulatedUnixPlatform.signalNumbering (UnixSystem.platform system)

        let action =
            // A handler the poll refuses to run, so a test that lets the
            // frame's handler run by mistake fails rather than passing.
            { SignalCatch.ofHandler NativeSignalHandler.CoreClrPalActivation with
                Mask = SignalMask.ofSignals numbering mask
                NoDefer = true
            }

        let caught =
            KernelSignals.setDisposition carrier (SignalDisposition.Catch action) system

        let sent =
            match UnixSignal.pthreadKill task (Signal.toRawSignoUnder numbering carrier) caught with
            | Ok (Ok (KillOutcome.ProcessContinues system)) -> system
            | other -> failwith $"expected the carrier to be left pending on %O{task}, got %A{other}"

        match UnixSignal.onReturnToUser task sent with
        | Ok (ReturnToUserOutcome.RunHandlers ([ frame ], system)) when frame.Entry.Signal = carrier ->
            KernelSignals.setDisposition carrier before system
        | other -> failwith $"expected the carrier alone to be delivered to %O{task}, got %A{other}"

    /// `system` once `task`'s innermost handler has returned: `sigreturn(2)`,
    /// which puts back the mask in force before that frame was pushed.
    /// Delivers nothing; fails the test if `task` has no frame.
    let leaveSystem<'Task when 'Task : comparison>
        (task : 'Task)
        (system : UnixSystem<'Task, NativeSignalHandler>)
        : UnixSystem<'Task, NativeSignalHandler>
        =
        match SignalState.framesOf task (UnixSystem.signals system) with
        | innermost :: _ -> UnixSignal.sigreturn task innermost.Id system
        | [] -> failwith $"%O{task} is inside no handler"

    /// `kernel` with `thread` inside a handler whose `sa_mask` is `mask`: see
    /// `enterSystem`.
    let enter (thread : ThreadId) (mask : Set<Signal>) (kernel : EmulatedKernel) : EmulatedKernel =
        EmulatedKernel.withUnix (enterSystem thread mask kernel.System) kernel

    /// `kernel` once `thread`'s innermost handler has returned: see `leaveSystem`.
    let leave (thread : ThreadId) (kernel : EmulatedKernel) : EmulatedKernel =
        EmulatedKernel.withUnix (leaveSystem thread kernel.System) kernel

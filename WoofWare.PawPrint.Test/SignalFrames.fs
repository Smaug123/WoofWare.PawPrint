namespace WoofWare.PawPrint.Test

open WoofWare.PawPrint
open WoofWare.PosixKernel

/// A thread inside a signal handler, for a test that needs a thread to block
/// signals: a thread's mask is its innermost handler frame's, and nothing else
/// sets one. PawPrint itself never leaves a frame pushed between instructions,
/// so this is a state only a test builds.
[<RequireQualifiedAccess>]
module SignalFrames =

    /// The signal delivered to put a thread in a handler: SIGPROF. A test
    /// using this does not use it otherwise.
    let private carrier : Signal = Signal.SIGPROF

    /// `kernel` with `thread` inside a handler whose `sa_mask` is `mask`, with
    /// `SA_NODEFER`, for `carrier`, whose disposition is put back as it was.
    /// Fails the test unless the return to user mode delivers the carrier
    /// alone.
    let enter (thread : ThreadId) (mask : Set<Signal>) (kernel : EmulatedKernel) : EmulatedKernel =
        let system = EmulatedKernel.unix kernel
        let before = KernelSignals.disposition carrier system

        let action =
            // A handler the poll refuses to run, so a test that lets the
            // frame's handler run by mistake fails rather than passing.
            { SignalCatch.ofHandler NativeSignalHandler.CoreClrPalActivation with
                Mask = mask
                NoDefer = true
            }

        let caught =
            KernelSignals.setDisposition carrier (SignalDisposition.Catch action) system

        let sent =
            match
                UnixSignal.pthreadKill
                    thread
                    (Signal.toRawSignoUnder (SimulatedUnixPlatform.signalNumbering kernel.UnixPlatform) carrier)
                    caught
            with
            | Ok (Ok (KillOutcome.ProcessContinues system)) -> system
            | other -> failwith $"expected the carrier to be left pending on %O{thread}, got %A{other}"

        match UnixSignal.onReturnToUser thread sent with
        | Ok (Some (SignalDelivery.RunHandlers [ frame ]), system) when frame.Entry.Signal = carrier ->
            EmulatedKernel.withUnix (KernelSignals.setDisposition carrier before system) kernel
        | other -> failwith $"expected the carrier alone to be delivered to %O{thread}, got %A{other}"

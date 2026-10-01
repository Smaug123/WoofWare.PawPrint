namespace WoofWare.PawPrint.Test

open WoofWare.PawPrint
open WoofWare.PosixKernel

/// A thread inside a signal handler, for a test that needs a thread to block
/// signals: a thread's mask is its innermost handler frame's, and nothing else
/// sets one. PawPrint itself never leaves a frame pushed between instructions,
/// so this is a state only a test builds.
[<RequireQualifiedAccess>]
module SignalFrames =

    /// The signal delivered to put a thread in a handler: SIGPROF, 27 under
    /// both numberings. A test using this does not use it otherwise.
    let private carrier : Signal = Signal.Other 27

    /// `kernel` with `thread` inside a handler whose `sa_mask` is `mask`, with
    /// `SA_NODEFER`, for `carrier`, whose disposition is put back as it was.
    /// Fails the test unless the return to user mode delivers the carrier
    /// alone.
    let enter (thread : ThreadId) (mask : Set<Signal>) (kernel : EmulatedKernel) : EmulatedKernel =
        let before = SignalState.disposition carrier kernel.Process.Signals

        let action =
            // A handler the poll refuses to run, so a test that lets the
            // frame's handler run by mistake fails rather than passing.
            { SignalCatch.ofHandler NativeSignalHandler.CoreClrPalActivation with
                Mask = mask
                NoDefer = true
            }

        let sent =
            kernel.Process.Signals
            |> SignalState.setDisposition carrier (SignalDisposition.Catch action)
            |> SignalState.enqueue
                {
                    Signal = carrier
                    Target = ValueSome thread
                }

        let system =
            EmulatedKernel.unix kernel
            |> fun system ->
                { system with
                    Process =
                        { system.Process with
                            Signals = sent
                        }
                }

        match UnixSignal.onReturnToUser thread system with
        | Ok (Some (SignalDelivery.RunHandlers [ frame ]), system) when frame.Entry.Signal = carrier ->
            let system =
                { system with
                    Process =
                        { system.Process with
                            Signals = SignalState.setDisposition carrier before system.Process.Signals
                        }
                }

            EmulatedKernel.withUnix system kernel
        | other -> failwith $"expected the carrier alone to be delivered to %O{thread}, got %A{other}"

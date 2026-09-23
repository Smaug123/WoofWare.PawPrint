namespace WoofWare.PawPrint

open WoofWare.PosixKernel

/// The BCL's `Interop.Sys.Signals`, the enum `Process.Kill` passes to
/// `SystemNative_Kill`, and the shim's screen of it.
///
/// A different alphabet from `PosixSignalPal`'s `PosixSignal`: three
/// non-negative values, private to System.Diagnostics.Process, which the shim
/// turns into a signo before calling `kill(2)`. `TestKillSignalPal` reads the
/// enum from the running BCL.
[<RequireQualifiedAccess>]
module KillSignalPal =

    /// `SystemNative_Kill`'s screen (`pal_process.c`): `PAL_NONE` (0) is the
    /// null signal, and `PAL_SIGKILL` (9) and `PAL_SIGSTOP` (19) are those
    /// signals, read under `numbering`. `None` for anything else, which the
    /// shim refuses with EINVAL without calling `kill(2)` at all.
    ///
    /// So no catchable signal can be sent through this entry point.
    let toKillSignal (numbering : SignalNumbering) (pal : int) : KillSignal option =
        let signal (signo : int) : KillSignal =
            match Signal.ofRawSignoUnder numbering signo with
            | ValueSome signal -> KillSignal.Signal signal
            | ValueNone ->
                failwith
                    $"KillSignalPal: %d{signo} is not a signal under the %O{numbering} numbering, but it is that numbering's SIGKILL or SIGSTOP"

        match pal with
        | 0 -> Some KillSignal.Null
        | 9 -> Some (signal 9)
        | 19 ->
            // The PAL's number is Linux's; the shim hands the host's SIGSTOP
            // to kill(2), which is 17 on Darwin.
            match numbering with
            | SignalNumbering.Linux -> Some (signal 19)
            | SignalNumbering.Darwin -> Some (signal 17)
        | _ -> None

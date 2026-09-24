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

    /// `SystemNative_Kill`'s screen (`pal_process.c`): the signal number it
    /// hands to `kill(2)`. `PAL_NONE` (0) is the null signal 0, and
    /// `PAL_SIGKILL` (9) and `PAL_SIGSTOP` (19) are those signals' numbers
    /// under `numbering`. `None` for anything else, which the shim refuses with
    /// EINVAL without calling `kill(2)` at all.
    ///
    /// So no catchable signal can be sent through this entry point.
    let toSigno (numbering : SignalNumbering) (pal : int) : int option =
        match pal with
        | 0 -> Some 0
        | 9 -> Some 9
        | 19 ->
            // The PAL's number is Linux's; the shim hands the host's SIGSTOP
            // to kill(2), which is 17 on Darwin.
            match numbering with
            | SignalNumbering.Linux -> Some 19
            | SignalNumbering.Darwin -> Some 17
        | _ -> None

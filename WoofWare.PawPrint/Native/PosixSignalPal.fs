namespace WoofWare.PawPrint

open WoofWare.PosixKernel

/// The BCL's `System.Runtime.InteropServices.PosixSignal` enum, and the
/// conversions the signal shims perform across it.
///
/// This is PawPrint's half of the signal boundary, and the last of the four
/// (`UnixErrorPal`, `SocketEventsPal`, `SocketArgumentsPal` are the others).
/// The enum carries no kernel content whatever: it is a managed alphabet of ten
/// negative integers naming the signals .NET considers portable, and a kernel
/// knows only signos. So `WoofWare.PosixKernel` states the signo, under a
/// `SignalNumbering` that says whose `<signal.h>` it is read from, and these
/// functions are where a guest's `PosixSignal` becomes one and back.
///
/// A transcription, so the compiler cannot keep it correct. Its oracle is the
/// enum itself, which `TestPosixSignalPal` reads from the running BCL — a
/// stronger position than the other three, whose upstream vocabulary is
/// internal and had to be parsed out of pinned source — and, for the
/// numbering, the host's own `SystemNative_GetPlatformSignalNumber`.
[<RequireQualifiedAccess>]
module PosixSignalPal =

    /// `PosixSignal.SIGHUP` … `PosixSignal.SIGTSTP`, the ten members the enum
    /// defines. Every other signal a guest can name reaches these functions as
    /// a raw signo instead.
    let private enumIdentities : (int * Signal) list =
        [
            -1, Signal.SIGHUP
            -2, Signal.SIGINT
            -3, Signal.SIGQUIT
            -4, Signal.SIGTERM
            -5, Signal.SIGCHLD
            -6, Signal.SIGCONT
            -7, Signal.SIGWINCH
            -8, Signal.SIGTTIN
            -9, Signal.SIGTTOU
            -10, Signal.SIGTSTP
        ]

    /// `pal_signal.c`'s `GetSignalMax()`: the largest positive number
    /// `SystemNative_GetPlatformSignalNumber` echoes back as a signo, and the
    /// bound the enable/disable/handle entry points assert their argument
    /// against.
    ///
    /// `SIGRTMAX` where the header defines one, and `NSIG` otherwise. On Linux
    /// that is 64, the kernel's highest signal; on Darwin, which has no
    /// `SIGRTMAX`, it is `NSIG` = 32 — one *past* the highest signal, 31. So
    /// under Darwin the shim admits a 32 that `sigaction(2)` then refuses,
    /// which is why this is the shim's number and `Signal.highestSignoUnder`
    /// is the kernel's, and the two are stated separately.
    let signalMax (numbering : SignalNumbering) : int =
        match numbering with
        | SignalNumbering.Linux -> Signal.highestSignoUnder SignalNumbering.Linux
        | SignalNumbering.Darwin -> Signal.highestSignoUnder SignalNumbering.Darwin + 1

    /// `SystemNative_GetPlatformSignalNumber`: the signo the BCL registers a
    /// managed `PosixSignal` value under.
    ///
    /// The enum's own members are negative and map to their signo under the
    /// given numbering. A caller may equally pass a positive native signo
    /// directly, which `PosixSignalRegistration.Register` allows; the shim
    /// echoes any such number in `(0, signalMax]` back without interpreting
    /// it, and this does the same. Anything else — including 0, which is
    /// `PosixSignalInvalid` — answers 0, which `Register` reports as
    /// `PlatformNotSupportedException`.
    let platformSignalNumber (numbering : SignalNumbering) (raw : int) : int =
        match List.tryFind (fun (value, _) -> value = raw) enumIdentities with
        | Some (_, signal) -> Signal.toRawSignoUnder numbering signal
        | None -> if raw > 0 && raw <= signalMax numbering then raw else 0

    /// Whether `SystemNative_HandleNonCanceledPosixSignal` has an explicit arm
    /// for this signal, so that running the kernel's default disposition costs
    /// the shim nothing.
    ///
    /// `pal_signal.c` names SIGCONT, SIGTSTP, SIGTTIN, SIGTTOU, SIGCHLD, SIGURG
    /// and SIGWINCH, and for each does nothing to the process and leaves its
    /// own handler installed, so the next occurrence still reaches managed
    /// code. Every other signal takes the `default:` arm, which restores the
    /// original `sigaction` and re-raises the signal with `kill(2)` — so the
    /// process gets whatever the kernel does by default, and the shim's
    /// handler is *gone* even when that default is to discard the signal
    /// (Darwin's SIGIO and SIGINFO). The two are told apart here rather than
    /// by the kernel's disposition, because which signals have an arm is the
    /// shim's choice: SIGURG and SIGWINCH are ignored by default and have one,
    /// Darwin's SIGIO is ignored by default and has none.
    ///
    /// Answers for the signal the value *is* under the numbering, so an
    /// `Other` carrying SIGURG's number counts.
    let handledWithoutRestoring (numbering : SignalNumbering) (signal : Signal) : bool =
        match Signal.canonicalUnder numbering signal with
        | Signal.SIGCONT
        | Signal.SIGTSTP
        | Signal.SIGTTIN
        | Signal.SIGTTOU
        | Signal.SIGCHLD
        | Signal.SIGURG
        | Signal.SIGWINCH -> true
        | Signal.SIGHUP
        | Signal.SIGINT
        | Signal.SIGQUIT
        | Signal.SIGTERM
        | Signal.SIGPIPE
        | Signal.SIGUSR1
        | Signal.SIGUSR2
        | Signal.SIGABRT
        | Signal.Other _ -> false

    /// The `PosixSignal` value the dispatcher hands a registered handler as its
    /// second argument.
    ///
    /// Signals with no member of the enum — SIGPIPE, SIGABRT, SIGUSR1, SIGUSR2,
    /// and any raw signo — produce `PosixSignalInvalid` (0), which is what real
    /// CoreCLR passes: `pal_signal.c` calls `TryConvertSignalCodeToPosixSignal`
    /// and, on its `false` return, overwrites the out-parameter with
    /// `PosixSignalInvalid` before invoking the handler. The raw signo that
    /// conversion helper writes for unmapped codes is dropped by that
    /// overwrite; only the *first* argument carries the signal number.
    let toEnum (signal : Signal) : int =
        match List.tryFind (fun (_, candidate) -> candidate = signal) enumIdentities with
        | Some (value, _) -> value
        | None -> 0

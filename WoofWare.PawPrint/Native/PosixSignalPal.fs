namespace WoofWare.PawPrint

open WoofWare.PosixKernel

/// The BCL's `System.Runtime.InteropServices.PosixSignal` enum, and the
/// conversions the signal shims perform across it.
///
/// This is PawPrint's half of the signal boundary, and the last of the four
/// (`UnixErrorPal`, `SocketEventsPal`, `SocketArgumentsPal` are the others).
/// The enum carries no kernel content whatever: it is a managed alphabet of ten
/// negative integers naming the signals .NET considers portable, and a kernel
/// knows only signos. So `WoofWare.PosixKernel` states the signo, and these two
/// functions are where a guest's `PosixSignal` becomes one and back.
///
/// A transcription, so the compiler cannot keep it correct. Its oracle is the
/// enum itself, which `TestPosixSignalPal` reads from the running BCL — a
/// stronger position than the other three, whose upstream vocabulary is
/// internal and had to be parsed out of pinned source.
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

    /// Map a managed `PosixSignal` value, as the BCL passes it across the
    /// P/Invoke boundary, to a `Signal`.
    ///
    /// The enum's own members are negative; a caller may equally pass a
    /// positive native signo directly, which
    /// `PosixSignalRegistration.Register` allows and which
    /// `SystemNative_GetPlatformSignalNumber` echoes back when it is in range.
    /// Anything this does not recognise — including 0, which is
    /// `PosixSignalInvalid` — answers `ValueNone`, and the caller reports the
    /// shim's 0 sentinel.
    let ofEnum (raw : int) : Signal voption =
        match List.tryFind (fun (value, _) -> value = raw) enumIdentities with
        | Some (_, signal) -> ValueSome signal
        | None ->
            // Not one of the enum's ten, so it can only be a raw signo — which
            // screens out 0 and every negative, since no signo is either.
            Signal.ofPlatformSigno raw

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

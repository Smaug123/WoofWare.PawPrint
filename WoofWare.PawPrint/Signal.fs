namespace WoofWare.PawPrint

/// A POSIX signal recognised by the simulator. The named cases cover the
/// signals the .NET BCL surfaces via the `PosixSignal` managed enum plus the
/// common ones the runtime emits at process-startup (SIGPIPE, SIGABRT,
/// SIGUSR1/2). Anything we don't have a case for is carried as `Other` along
/// with the raw managed `PosixSignal` value so callers retain identity
/// information across PawPrint even when we don't model the signal's
/// semantics.
///
/// Note: this type intentionally does NOT mention native (POSIX) signal
/// numbers, which are platform-specific. The native↔domain mapping lives at
/// the P/Invoke seam and is platform-aware; the domain type is the
/// platform-neutral identity that the rest of the simulator works with.
/// PawPrint always uses the Linux signal-number table, regardless of host
/// OS — see `Signal.toLinuxSigno`. This keeps the simulator deterministic
/// across hosts.
type Signal =
    | SIGHUP
    | SIGINT
    | SIGQUIT
    | SIGTERM
    | SIGCHLD
    | SIGCONT
    | SIGWINCH
    | SIGTSTP
    | SIGTTIN
    | SIGTTOU
    | SIGPIPE
    | SIGUSR1
    | SIGUSR2
    | SIGABRT
    /// Catch-all for signals the simulator doesn't model semantically yet.
    /// Carries the raw managed `PosixSignal` enum value (negative for
    /// cross-platform signals, positive when callers supply a native signo
    /// directly). Two `Other` values are equal iff their raw values match.
    | Other of rawSignal : int

[<RequireQualifiedAccess>]
module Signal =
    /// Map a domain `Signal` to its Linux native signo. PawPrint uses the
    /// Linux table on every host, so simulation traces don't depend on the
    /// host OS. Callers crossing the P/Invoke seam must use this — never the
    /// host's own headers — for deterministic output.
    let toLinuxSigno (signal : Signal) : int =
        match signal with
        | Signal.SIGHUP -> 1
        | Signal.SIGINT -> 2
        | Signal.SIGQUIT -> 3
        | Signal.SIGABRT -> 6
        | Signal.SIGUSR1 -> 10
        | Signal.SIGUSR2 -> 12
        | Signal.SIGPIPE -> 13
        | Signal.SIGTERM -> 15
        | Signal.SIGCHLD -> 17
        | Signal.SIGCONT -> 18
        | Signal.SIGTSTP -> 20
        | Signal.SIGTTIN -> 21
        | Signal.SIGTTOU -> 22
        | Signal.SIGWINCH -> 28
        | Signal.Other rawSignal -> rawSignal

    /// Inverse of `toLinuxSigno` on the named cases. Returns `ValueNone` for
    /// signos that don't correspond to a modelled signal — the caller can
    /// then decide whether to fail loudly or wrap in `Other`.
    let ofLinuxSigno (signo : int) : Signal voption =
        match signo with
        | 1 -> ValueSome Signal.SIGHUP
        | 2 -> ValueSome Signal.SIGINT
        | 3 -> ValueSome Signal.SIGQUIT
        | 6 -> ValueSome Signal.SIGABRT
        | 10 -> ValueSome Signal.SIGUSR1
        | 12 -> ValueSome Signal.SIGUSR2
        | 13 -> ValueSome Signal.SIGPIPE
        | 15 -> ValueSome Signal.SIGTERM
        | 17 -> ValueSome Signal.SIGCHLD
        | 18 -> ValueSome Signal.SIGCONT
        | 20 -> ValueSome Signal.SIGTSTP
        | 21 -> ValueSome Signal.SIGTTIN
        | 22 -> ValueSome Signal.SIGTTOU
        | 28 -> ValueSome Signal.SIGWINCH
        | _ -> ValueNone

    /// Map a managed `PosixSignal` enum value (as the BCL passes it across
    /// the seam) to a domain `Signal`. The managed enum uses negative
    /// values for cross-platform identities and positive values when the
    /// caller has supplied a raw native signo directly; this helper
    /// translates either form. Returns `ValueNone` for values we don't
    /// recognise — `SystemNative_GetPlatformSignalNumber` returns 0 in that
    /// case, and `PosixSignalRegistration.Register` then throws.
    let ofPosixSignalEnum (raw : int) : Signal voption =
        match raw with
        | -1 -> ValueSome Signal.SIGHUP
        | -2 -> ValueSome Signal.SIGINT
        | -3 -> ValueSome Signal.SIGQUIT
        | -4 -> ValueSome Signal.SIGTERM
        | -5 -> ValueSome Signal.SIGCHLD
        | -6 -> ValueSome Signal.SIGCONT
        | -7 -> ValueSome Signal.SIGWINCH
        | -8 -> ValueSome Signal.SIGTTIN
        | -9 -> ValueSome Signal.SIGTTOU
        | -10 -> ValueSome Signal.SIGTSTP
        | n when n > 0 -> ofLinuxSigno n
        | _ -> ValueNone

namespace WoofWare.PosixKernel

/// <summary>
/// A POSIX signal recognised by the simulator.
/// </summary>
/// <remarks>
/// The named cases cover some specific signals we've had reason to handle explicitly.
/// Use the <c>Other</c> case for anything we've missed.
///
/// This type intentionally does not model an assignment of signals to numbers, which are platform-specific.
/// Use <c>Signal.toLinuxSigno</c> to render a number for Linux; we haven't yet done this for Darwin.
/// </remarks>
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
    /// Carries a raw signo. `ofPlatformSigno` is the only thing here that
    /// builds one, and it produces only signos in `(0, linuxSignalMax]` — but
    /// the case is public and enforces nothing, so a client can put any number
    /// in it and `toLinuxSigno` will hand that number straight back. Two
    /// `Other` values are equal iff their raw values match.
    | Other of rawSignal : int

/// <summary>
/// The kernel-level default action for a POSIX signal when no managed
/// handler claims it.
/// </summary>
/// <remarks>
/// Mirrors the POSIX 1003.1 categories.
/// </remarks>
[<RequireQualifiedAccess>]
type DefaultDisposition =
    /// <summary>
    /// Specify that the kernel default is to terminate the process.
    /// </summary>
    | Terminate
    /// <summary>
    /// Specify that the kernel default is to ignore the signal entirely. No state changes.
    /// </summary>
    | Ignore
    /// <summary>
    /// Specify that the kernel default is to suspend (stop) the process.
    /// </summary>
    | Stop
    /// <summary>
    /// Specify that the kernel default is to resume a stopped process.
    /// </summary>
    | Continue

[<RequireQualifiedAccess>]
module Signal =
    /// <summary>
    /// The highest signal number the simulated kernel accepts from a guest.
    /// </summary>
    /// <remarks>
    /// Matches Linux's <c>SIGRTMAX</c> on every modern glibc build (the real
    /// native side reads <c>GetSignalMax()</c> from <c>&lt;signal.h&gt;</c>, which expands
    /// to <c>SIGRTMAX</c> whenever that macro is defined).
    ///
    /// Used as the ceiling for our "is this a plausible native signo?" check:
    /// a guest can pass a <c>(PosixSignal)42</c> and WoofWare.PosixKernel will
    /// round-trip the raw value through <c>Signal.Other</c>, but a <c>(PosixSignal)200</c>
    /// is firmly outside any kernel's table and returns 0.
    /// </remarks>
    let linuxSignalMax : int = 64

    /// Map a domain `Signal` to its Linux native signo. PawPrint uses the
    /// Linux table on every host, so simulation traces don't depend on the
    /// host OS. Callers crossing the P/Invoke boundary must use this — never
    /// the host's own headers — for deterministic output.
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

    /// <summary>
    /// Convert a raw signo to a modelled signal description.
    /// </summary>
    /// <remarks>
    /// This is the inverse of <c>toLinuxSigno</c> where defined.
    /// </remarks>
    /// <returns>
    /// <c>ValueNone</c> for signos which don't correspond to a modelled signal.
    /// You might want to wrap such a signal in <c>Signal.Other</c>.
    /// </returns>
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

    /// <summary>
    /// Signals that cannot be caught, blocked, or ignored on POSIX.
    /// </summary>
    /// <remarks>
    /// The kernel rejects <c>sigaction(SIGKILL, ...)</c> and <c>sigaction(SIGSTOP, ...)</c>
    /// with <c>EINVAL</c>.
    /// </remarks>
    let isUncatchable (signal : Signal) : bool =
        match signal with
        | Signal.Other 9 -> true // SIGKILL
        | Signal.Other 19 -> true // SIGSTOP
        | _ -> false

    /// Map a positive native signo to a domain `Signal`. Modelled signos
    /// produce their named case; unmodelled-but-valid signos (positive and
    /// `<= linuxSignalMax`) round-trip through `Signal.Other` so identity is
    /// preserved across the P/Invoke boundary — matching the real native semantics, where
    /// `SystemNative_GetPlatformSignalNumber` returns the raw value when
    /// `signal > 0 && signal <= GetSignalMax()` and `PosixSignalRegistration`
    /// then forwards it to `Enable/DisablePosixSignalHandling` unchanged.
    /// Values outside `(0, linuxSignalMax]` return `ValueNone`; callers
    /// translate that to the "unknown signal" sentinel (0 for the BCL
    /// boundary, `ArgumentOutOfRangeException` higher up).
    let ofPlatformSigno (signo : int) : Signal voption =
        match ofLinuxSigno signo with
        | ValueSome signal -> ValueSome signal
        | ValueNone ->
            if signo > 0 && signo <= linuxSignalMax then
                ValueSome (Signal.Other signo)
            else
                ValueNone

    /// <summary>
    /// The kernel-level default disposition for <c>signal</c>.
    /// </summary>
    /// <remarks>
    /// Unmodelled-but-known signals carried as <c>Signal.Other rawSigno</c>
    /// still classify correctly: <c>Signal.Other 23</c> (<c>SIGURG</c>, signo 23 on
    /// Linux) returns <c>Ignore</c>, matching the kernel default.
    ///
    /// Unmodelled-but-<i>unknown</i> signals classify conservatively as <c>Terminate</c>,
    /// which is the POSIX default for unrecognised signals.
    /// </remarks>
    let defaultDisposition (signal : Signal) : DefaultDisposition =
        // Key off the Linux signo, not the `Signal` DU case directly, so
        // unmodelled-but-known signals carried as `Signal.Other` classify
        // by their raw signo.
        match toLinuxSigno signal with
        // Ignore-by-default: SIGCHLD (17), SIGURG (23), SIGWINCH (28).
        // SIGURG isn't in our `Signal` DU but reaches us via
        // `Signal.Other 23`; surfacing it here keeps the disposition
        // table source-of-truth for arbitrary signos.
        | 17
        | 23
        | 28 -> DefaultDisposition.Ignore
        // Stop-by-default: SIGSTOP (19, uncatchable; included for
        // completeness), SIGTSTP (20), SIGTTIN (21), SIGTTOU (22).
        | 19
        | 20
        | 21
        | 22 -> DefaultDisposition.Stop
        // Continue-by-default: SIGCONT (18).
        | 18 -> DefaultDisposition.Continue
        // Everything else terminates. Covers the modelled signals
        // SIGHUP, SIGINT, SIGQUIT, SIGABRT, SIGUSR1, SIGUSR2, SIGPIPE,
        // SIGTERM, plus arbitrary unrecognised signos. Matches the
        // `default:` branch in `SystemNative_HandleNonCanceledPosixSignal`.
        | _ -> DefaultDisposition.Terminate

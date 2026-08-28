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
    /// Specify that the kernel default is to terminate the process.
    | Terminate
    /// Specify that the kernel default is to ignore the signal entirely. No state changes.
    | Ignore
    /// Specify that the kernel default is to suspend (stop) the process.
    | Stop
    /// Specify that the kernel default is to resume a stopped process.
    | Continue

[<RequireQualifiedAccess>]
module Signal =
    /// Highest signal number the simulator accepts from P/Invoke arguments.
    /// Matches Linux's `SIGRTMAX` on every modern glibc build (the real
    /// native side reads `GetSignalMax()` from `<signal.h>`, which expands
    /// to `SIGRTMAX` whenever that macro is defined). Used as the ceiling
    /// for the "is this a plausible native signo?" check: a guest can pass
    /// a `(PosixSignal)42` and PawPrint will round-trip the raw value
    /// through `Signal.Other`, but a `(PosixSignal)200` is firmly outside
    /// any kernel's table and returns 0 like the real native function.
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

    /// Signals that cannot be caught, blocked, or ignored on POSIX. The
    /// kernel rejects `sigaction(SIGKILL, ...)` / `sigaction(SIGSTOP, ...)`
    /// with `EINVAL`, so `SystemNative_EnablePosixSignalHandling` returns
    /// `false` (install failed) and `PosixSignalRegistration.Create` throws
    /// rather than recording an impossible handler. PawPrint mirrors this
    /// at the P/Invoke boundary; the simulator never delivers either signal, regardless
    /// of what's in the pending queue, because no one can legally install
    /// a handler for them.
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

    /// The POSIX kernel-level default disposition for `signal`. Used by
    /// `SystemNative_HandleNonCanceledPosixSignal` to decide whether the
    /// dispatcher path should treat the signal as a no-op or fall through
    /// to process termination, and (in a later slice) by the dispatcher's
    /// handler-return-0 path that mirrors the same decision.
    ///
    /// Unmodelled-but-known signals carried as `Signal.Other rawSigno`
    /// still classify correctly: `Signal.Other 23` (SIGURG, signo 23 on
    /// Linux) returns `Ignore`, matching the kernel default rather than
    /// falling through to the conservative `Terminate` catch-all. Unknown
    /// signos that don't correspond to a kernel default we recognise
    /// classify as `Terminate`, which is the POSIX default for
    /// unrecognised signals and matches the trailing `default:` branch in
    /// `pal_signal.c`'s `SystemNative_HandleNonCanceledPosixSignal` switch.
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

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

/// The kernel-level default action for a POSIX signal when no managed
/// handler claims it. PawPrint mirrors the POSIX 1003.1 categories rather
/// than collapsing everything to "terminate vs. no-op", so callers that
/// want to render the disposition (e.g. trace output) retain the source
/// information. `SystemNative_HandleNonCanceledPosixSignal` treats
/// `Ignore`, `Stop`, and `Continue` all as no-ops in the dispatcher path
/// — the runtime cannot stop or continue itself, and Ignore is literally
/// nothing to do — but the classification is still load-bearing for
/// future slices that want to render or assert the disposition.
[<RequireQualifiedAccess>]
type DefaultDisposition =
    /// Kernel default is to terminate the process. PawPrint does not yet
    /// model signal-driven process termination; the dispatcher path that
    /// reaches this disposition fails loudly until a follow-up slice
    /// wires it through to a `RunOutcome`.
    | Terminate
    /// Kernel default is to ignore the signal entirely. No state changes.
    | Ignore
    /// Kernel default is to suspend (stop) the process. PawPrint cannot
    /// stop its own simulator threads from the inside, so this is a
    /// no-op for `SystemNative_HandleNonCanceledPosixSignal` purposes —
    /// the kernel-level distinction from `Ignore` is preserved so a
    /// trace/UI consumer can still tell the two apart.
    | Stop
    /// Kernel default is to resume a stopped process. No state changes;
    /// PawPrint has nothing to resume.
    | Continue

[<RequireQualifiedAccess>]
module Signal =
    /// Highest signal number the simulator accepts at the P/Invoke seam.
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

    /// Signals that cannot be caught, blocked, or ignored on POSIX. The
    /// kernel rejects `sigaction(SIGKILL, ...)` / `sigaction(SIGSTOP, ...)`
    /// with `EINVAL`, so `SystemNative_EnablePosixSignalHandling` returns
    /// `false` (install failed) and `PosixSignalRegistration.Create` throws
    /// rather than recording an impossible handler. PawPrint mirrors this
    /// at the seam; the simulator never delivers either signal, regardless
    /// of what's in the pending queue, because no one can legally install
    /// a handler for them.
    let isUncatchable (signal : Signal) : bool =
        match signal with
        | Signal.Other 9 -> true // SIGKILL
        | Signal.Other 19 -> true // SIGSTOP
        | _ -> false

    /// Map a positive native signo to a domain `Signal`. Modelled signos
    /// produce their named case; unmodelled-but-valid signos (positive and
    /// `<= linuxSignalMax`) round-trip through `Signal.Other` so the seam
    /// preserves identity — matching the real native semantics, where
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
        | n -> ofPlatformSigno n

    /// The `PosixSignal` enum value that the dispatcher should hand to the
    /// registered handler as its second argument. Modelled cross-platform
    /// signals produce their negative enum identity (so `SIGINT -> -2`
    /// round-trips through `ofPosixSignalEnum`).
    ///
    /// Signals that have no managed `PosixSignal` enum identity (SIGPIPE,
    /// SIGABRT, SIGUSR1, SIGUSR2, and arbitrary `Signal.Other` raw signos)
    /// produce `PosixSignalInvalid` (0). This matches what the real CoreCLR
    /// dispatcher actually passes: `pal_signal.c` calls
    /// `TryConvertSignalCodeToPosixSignal`, and on its `false` return path
    /// the caller overwrites the out-parameter with `PosixSignalInvalid`
    /// before invoking `g_posixSignalHandler(signalCode, signal)`. The raw
    /// positive signo the conversion helper writes for unmapped codes is
    /// dropped on the floor by that overwrite — only the first `signo`
    /// argument carries the raw signal number to the managed handler.
    let toPosixSignalEnum (signal : Signal) : int =
        match signal with
        | Signal.SIGHUP -> -1
        | Signal.SIGINT -> -2
        | Signal.SIGQUIT -> -3
        | Signal.SIGTERM -> -4
        | Signal.SIGCHLD -> -5
        | Signal.SIGCONT -> -6
        | Signal.SIGWINCH -> -7
        | Signal.SIGTTIN -> -8
        | Signal.SIGTTOU -> -9
        | Signal.SIGTSTP -> -10
        | Signal.SIGABRT -> 0
        | Signal.SIGUSR1 -> 0
        | Signal.SIGUSR2 -> 0
        | Signal.SIGPIPE -> 0
        | Signal.Other _ -> 0

    /// The POSIX kernel-level default disposition for `signal`. Used by
    /// `SystemNative_HandleNonCanceledPosixSignal` to decide whether the
    /// dispatcher path should treat the signal as a no-op or fall through
    /// to process termination, and (in a later slice) by the dispatcher's
    /// handler-return-0 path that mirrors the same decision.
    ///
    /// The lookup keys off the Linux signo, not the `Signal` DU case
    /// directly, so unmodelled-but-known signals carried as
    /// `Signal.Other rawSigno` still classify correctly: `Signal.Other 23`
    /// (SIGURG, signo 23 on Linux) returns `Ignore`, matching the kernel
    /// default rather than falling through to the conservative
    /// `Terminate` catch-all. Unknown signos that don't correspond to a
    /// kernel default we recognise classify as `Terminate`, which is the
    /// POSIX default for unrecognised signals and matches the trailing
    /// `default:` branch in `pal_signal.c`'s
    /// `SystemNative_HandleNonCanceledPosixSignal` switch.
    let defaultDisposition (signal : Signal) : DefaultDisposition =
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

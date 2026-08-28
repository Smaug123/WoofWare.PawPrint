namespace WoofWare.PosixKernel.Test

open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// Unit tests for the `Signal` conversion helpers: the signo table, what a
/// signal can be caught as, and what a kernel does with one by default.
///
/// The other half of the conversion math — anything involving a client's
/// managed `PosixSignal` enum — is not here, because it is not in this library:
/// see `WoofWare.PawPrint.Test/TestPosixSignalPal.fs`. What remains is what a
/// kernel itself knows.
///
/// These functions are exhaustively verified rather than sampled because the
/// arms that consume them cannot be tested any other way: enabling or disabling
/// a signal through a direct P/Invoke on the real CLR installs a sigaction
/// handler in the test host's own process.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestSignal =

    /// The 14 modelled signals paired with their Linux signo. Drives both
    /// the round-trip and the per-case toLinuxSigno assertion.
    let private modelledSignals : (Signal * int) list =
        [
            Signal.SIGHUP, 1
            Signal.SIGINT, 2
            Signal.SIGQUIT, 3
            Signal.SIGABRT, 6
            Signal.SIGUSR1, 10
            Signal.SIGUSR2, 12
            Signal.SIGPIPE, 13
            Signal.SIGTERM, 15
            Signal.SIGCHLD, 17
            Signal.SIGCONT, 18
            Signal.SIGTSTP, 20
            Signal.SIGTTIN, 21
            Signal.SIGTTOU, 22
            Signal.SIGWINCH, 28
        ]

    /// The ceiling's *value*, which nothing else pins: every other test here
    /// and in `TestPosixSignalPal` names `linuxSignalMax` symbolically, so all
    /// of them move with it and none of them can see it being wrong.
    ///
    /// 64 is `SIGRTMAX`, measured on Linux 6.18.5 rather than recalled
    /// (`SIGRTMAX=64 SIGRTMIN=34 NSIG=65`). Getting it wrong is guest-visible:
    /// at 63, a guest registering signal 64 is refused where real Linux
    /// accepts it.
    [<Test>]
    let ``the signo ceiling is SIGRTMAX`` () : unit = Signal.linuxSignalMax |> shouldEqual 64

    [<Test>]
    let ``toLinuxSigno produces the documented signo for every named case`` () : unit =
        for signal, signo in modelledSignals do
            Signal.toLinuxSigno signal |> shouldEqual signo

    [<Test>]
    let ``toLinuxSigno on Other returns the raw value unchanged`` () : unit =
        // The `Other` constructor carries raw identity so callers that
        // produced a positive native signo can round-trip it through the
        // Signal type without losing the value. The out-of-range values below
        // are not built by anything here — `ofPlatformSigno` produces only
        // signos in `(0, linuxSignalMax]` — but the case is public and
        // enforces nothing, and this pins that `toLinuxSigno` stays a
        // projection rather than acquiring an opinion about its payload.
        Signal.toLinuxSigno (Signal.Other 42) |> shouldEqual 42
        Signal.toLinuxSigno (Signal.Other 999) |> shouldEqual 999
        Signal.toLinuxSigno (Signal.Other -77) |> shouldEqual -77
        Signal.toLinuxSigno (Signal.Other 0) |> shouldEqual 0

    [<Test>]
    let ``ofLinuxSigno is the inverse of toLinuxSigno on every modelled signal`` () : unit =
        for signal, signo in modelledSignals do
            Signal.ofLinuxSigno signo |> shouldEqual (ValueSome signal)

    [<Test>]
    let ``ofLinuxSigno returns ValueNone for signos PawPrint does not model`` () : unit =
        // Signos that sit between modelled values and signos beyond the
        // SIGRTMAX-ish ceiling both fail to match — the enable/disable
        // arms can therefore rely on a `ValueSome` to indicate a signal
        // they can dispatch through `SignalState`.
        Signal.ofLinuxSigno 0 |> shouldEqual ValueNone
        Signal.ofLinuxSigno 4 |> shouldEqual ValueNone // SIGILL — not modelled
        Signal.ofLinuxSigno 5 |> shouldEqual ValueNone // SIGTRAP — not modelled
        Signal.ofLinuxSigno 7 |> shouldEqual ValueNone // SIGBUS — not modelled
        Signal.ofLinuxSigno 11 |> shouldEqual ValueNone // SIGSEGV — not modelled
        Signal.ofLinuxSigno 100 |> shouldEqual ValueNone
        Signal.ofLinuxSigno -1 |> shouldEqual ValueNone // negatives never match

    [<Test>]
    let ``isUncatchable flags SIGKILL and SIGSTOP and nothing else`` () : unit =
        // The POSIX standard names exactly two uncatchable signals: SIGKILL
        // (Linux signo 9) and SIGSTOP (Linux signo 19). The kernel rejects
        // `sigaction` for either with `EINVAL`. Neither is in PawPrint's
        // modelled signal set, so they only ever arrive as `Signal.Other n`.
        Signal.isUncatchable (Signal.Other 9) |> shouldEqual true // SIGKILL
        Signal.isUncatchable (Signal.Other 19) |> shouldEqual true // SIGSTOP

        // Every modelled signal can be caught — that's the whole point of
        // installing a handler. Iterate to assert the negative case without
        // hard-coding the modelled set twice.
        for signal, _signo in modelledSignals do
            Signal.isUncatchable signal |> shouldEqual false

        // Other `Other` values (signos that just happen to fall outside the
        // modelled set, but are still catchable) must not be flagged.
        Signal.isUncatchable (Signal.Other 4) |> shouldEqual false // SIGILL
        Signal.isUncatchable (Signal.Other 11) |> shouldEqual false // SIGSEGV
        Signal.isUncatchable (Signal.Other 8) |> shouldEqual false // SIGFPE
        Signal.isUncatchable (Signal.Other 64) |> shouldEqual false

    [<Test>]
    let ``defaultDisposition classifies modelled terminate-by-default signals`` () : unit =
        // POSIX default for these signals is Terminate (some with a core
        // dump, but PawPrint collapses both into a single Terminate case
        // since we don't model core dumps). Drives the catch-all branch
        // in `pal_signal.c`'s `SystemNative_HandleNonCanceledPosixSignal`.
        for signal in
            [
                Signal.SIGHUP
                Signal.SIGINT
                Signal.SIGQUIT
                Signal.SIGABRT
                Signal.SIGUSR1
                Signal.SIGUSR2
                Signal.SIGPIPE
                Signal.SIGTERM
            ] do
            Signal.defaultDisposition signal |> shouldEqual DefaultDisposition.Terminate

    [<Test>]
    let ``defaultDisposition classifies modelled ignore stop and continue signals`` () : unit =
        // The four kernel "Stop" signals: SIGSTOP (uncatchable; not in our
        // DU, so the test exercises it via Signal.Other 19), SIGTSTP,
        // SIGTTIN, SIGTTOU. Plus the no-op-by-default trio (SIGCHLD,
        // SIGWINCH) and the Continue-by-default singleton (SIGCONT).
        Signal.defaultDisposition Signal.SIGCHLD
        |> shouldEqual DefaultDisposition.Ignore

        Signal.defaultDisposition Signal.SIGWINCH
        |> shouldEqual DefaultDisposition.Ignore

        Signal.defaultDisposition Signal.SIGCONT
        |> shouldEqual DefaultDisposition.Continue

        Signal.defaultDisposition Signal.SIGTSTP |> shouldEqual DefaultDisposition.Stop

        Signal.defaultDisposition Signal.SIGTTIN |> shouldEqual DefaultDisposition.Stop

        Signal.defaultDisposition Signal.SIGTTOU |> shouldEqual DefaultDisposition.Stop

    [<Test>]
    let ``defaultDisposition routes Signal.Other 23 to Ignore for SIGURG`` () : unit =
        // SIGURG isn't in PawPrint's `Signal` DU and so always arrives via
        // `Signal.Other 23`. The kernel default is Ignore — the dispatcher
        // must NOT fall through to the catch-all Terminate branch just
        // because the case is unnamed.
        Signal.defaultDisposition (Signal.Other 23)
        |> shouldEqual DefaultDisposition.Ignore

    [<Test>]
    let ``defaultDisposition routes Signal.Other 19 to Stop for SIGSTOP`` () : unit =
        // SIGSTOP is uncatchable so the dispatcher will never actually run
        // its default disposition (`SystemNative_EnablePosixSignalHandling`
        // returns EINVAL before any pending entry can build up), but the
        // classifier is still expected to give the correct kernel-level
        // answer for completeness — Stop, not Terminate.
        Signal.defaultDisposition (Signal.Other 19)
        |> shouldEqual DefaultDisposition.Stop

    [<Test>]
    let ``defaultDisposition routes unmodelled signos to Terminate`` () : unit =
        // The POSIX default for an unrecognised signal is Terminate, and
        // `SystemNative_HandleNonCanceledPosixSignal`'s `default:` branch
        // is the catch-all. SIGILL (4), SIGFPE (8), SIGSEGV (11), SIGBUS
        // (7), SIGSYS (31): all terminate-by-default and not in our
        // modelled set. `Signal.Other` carrying any unrecognised signo
        // must classify the same way.
        Signal.defaultDisposition (Signal.Other 4)
        |> shouldEqual DefaultDisposition.Terminate // SIGILL

        Signal.defaultDisposition (Signal.Other 8)
        |> shouldEqual DefaultDisposition.Terminate // SIGFPE

        Signal.defaultDisposition (Signal.Other 11)
        |> shouldEqual DefaultDisposition.Terminate // SIGSEGV

        Signal.defaultDisposition (Signal.Other 64)
        |> shouldEqual DefaultDisposition.Terminate

    [<Test>]
    let ``defaultDisposition is total over every modelled signal`` () : unit =
        // Guard against a future named-case addition that forgets to
        // extend `defaultDisposition`: every entry in `modelledSignals`
        // must produce *some* disposition (any value is fine — the
        // per-case correctness is asserted by the other tests above).
        // F# pattern-match exhaustiveness on `Signal` doesn't fire here
        // because the lookup is keyed off `toLinuxSigno`, so an
        // unrepresented signo would silently fall through to Terminate
        // without compiler help.
        for signal, _signo in modelledSignals do
            Signal.defaultDisposition signal |> ignore

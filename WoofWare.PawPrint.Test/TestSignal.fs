namespace WoofWare.PawPrint.Test

open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// Unit tests for the `Signal` conversion helpers that the SystemNative
/// signal-handling arms rely on. These functions are the only piece of
/// "business logic" in the four arms (`Initialize`,
/// `GetPlatformSignalNumber`, `Enable`/`DisablePosixSignalHandling`); the
/// rest of each arm is plumbing into `SignalState`, which is exercised by
/// `TestSignalState`. End-to-end coverage for the GetPlatformSignalNumber
/// arm lives in `sourcesPure/SystemNativeGetPlatformSignalNumber.cs`; the
/// Enable/Disable arms can't be safely tested via direct P/Invoke on the
/// real CLR (they install host-process sigaction handlers), so the
/// conversion math is exhaustively verified here instead.
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

    /// PosixSignal cross-platform enum values (negative, defined by the
    /// managed BCL) paired with their PawPrint Signal identity. The .NET
    /// `PosixSignal` enum only assigns negative values to 10 of the 14
    /// modelled signals; the remaining four (SIGPIPE, SIGABRT, SIGUSR1/2)
    /// have no cross-platform enum member and must be supplied as positive
    /// native signos directly.
    let private namedPosixEnumValues : (int * Signal) list =
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

    [<Test>]
    let ``toLinuxSigno produces the documented signo for every named case`` () : unit =
        for signal, signo in modelledSignals do
            Signal.toLinuxSigno signal |> shouldEqual signo

    [<Test>]
    let ``toLinuxSigno on Other returns the raw value unchanged`` () : unit =
        // The `Other` constructor carries raw identity so callers that
        // produced a positive native signo can round-trip it through the
        // Signal type without losing the value. Negative raws come from
        // cross-platform PosixSignal enum values the simulator doesn't
        // model — those are also preserved verbatim, even though there's
        // no useful signo to send back across the seam.
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
    let ``ofPosixSignalEnum maps every cross-platform negative to the right case`` () : unit =
        for raw, signal in namedPosixEnumValues do
            Signal.ofPosixSignalEnum raw |> shouldEqual (ValueSome signal)

    [<Test>]
    let ``ofPosixSignalEnum treats positives as Linux signos`` () : unit =
        // The BCL allows guests to construct a `PosixSignal` from a raw
        // native signo (`(PosixSignal)signo`). When that happens the value
        // arrives at GetPlatformSignalNumber as a positive int; the
        // real native code accepts it iff it's a recognised host signal,
        // and PawPrint accepts it iff it's a modelled Linux signo.
        Signal.ofPosixSignalEnum 1 |> shouldEqual (ValueSome Signal.SIGHUP)
        Signal.ofPosixSignalEnum 6 |> shouldEqual (ValueSome Signal.SIGABRT)
        Signal.ofPosixSignalEnum 13 |> shouldEqual (ValueSome Signal.SIGPIPE)
        Signal.ofPosixSignalEnum 28 |> shouldEqual (ValueSome Signal.SIGWINCH)

    [<Test>]
    let ``ofPosixSignalEnum returns ValueNone for 0 and unrecognised values`` () : unit =
        // The cross-platform negative range only covers -1..-10; values
        // outside that range with no positive interpretation must produce
        // ValueNone so the arm returns 0 and the BCL raises
        // ArgumentOutOfRangeException.
        Signal.ofPosixSignalEnum 0 |> shouldEqual ValueNone // PosixSignalInvalid sentinel
        Signal.ofPosixSignalEnum -11 |> shouldEqual ValueNone
        Signal.ofPosixSignalEnum -100 |> shouldEqual ValueNone
        Signal.ofPosixSignalEnum System.Int32.MinValue |> shouldEqual ValueNone
        // Positive signos PawPrint doesn't model also fail — same path as
        // ofLinuxSigno's ValueNone branch.
        Signal.ofPosixSignalEnum 100 |> shouldEqual ValueNone
        Signal.ofPosixSignalEnum System.Int32.MaxValue |> shouldEqual ValueNone

    [<Test>]
    let ``ofPosixSignalEnum and toLinuxSigno round-trip through GetPlatformSignalNumber semantics`` () : unit =
        // Simulates the full GetPlatformSignalNumber arm:
        //   raw -> Signal.ofPosixSignalEnum -> toLinuxSigno -> signo
        // for every cross-platform enum value and for every positive
        // modelled signo. A non-zero result means "the BCL can call
        // Enable/DisablePosixSignalHandling with this signo and PawPrint
        // will accept it"; a zero result means the BCL will throw
        // ArgumentOutOfRangeException at the Register call site.
        let armBehaviour (raw : int) : int =
            match Signal.ofPosixSignalEnum raw with
            | ValueSome s -> Signal.toLinuxSigno s
            | ValueNone -> 0

        for raw, _signal in namedPosixEnumValues do
            // Every negative cross-platform enum value must produce a non-zero
            // signo — these are the values `PosixSignalRegistration.Register`
            // is documented to accept.
            armBehaviour raw |> shouldNotEqual 0

        for _signal, signo in modelledSignals do
            armBehaviour signo |> shouldEqual signo

        armBehaviour 0 |> shouldEqual 0
        armBehaviour -11 |> shouldEqual 0
        armBehaviour 100 |> shouldEqual 0

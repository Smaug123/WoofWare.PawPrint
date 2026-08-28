namespace WoofWare.PawPrint.Test

open System.Runtime.InteropServices
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PosixKernel

/// `PosixSignalPal` is a transcription of a managed enum, so its oracle is that
/// enum: every row below reads `System.Runtime.InteropServices.PosixSignal`
/// from the BCL this test host runs rather than restating its values.
///
/// That is a better position than the other three `*Pal` modules are in.
/// `Interop.Error`, the `SocketEvents` bits and the `AF_*` numbering are all
/// internal to the runtime, so their tests have to parse pinned source;
/// `PosixSignal` is public, so the real thing is callable here.
///
/// The tests marked as moved came from `WoofWare.PosixKernel.Test/TestSignal.fs`
/// with the functions they exercise. `ofEnum` composed with the library's
/// `toLinuxSigno` is exactly the `SystemNative_GetPlatformSignalNumber` arm, so
/// the rows that check that composition belong on this side of the boundary too.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestPosixSignalPal =

    /// Every member of the enum, read from the BCL, paired with the `Signal`
    /// this repo says it names. The *values* are the oracle; the pairing is
    /// this repo's claim, and `the enum has exactly the members this maps`
    /// below is what stops the two drifting apart.
    let private enumMembers : (PosixSignal * Signal) list =
        [
            PosixSignal.SIGHUP, Signal.SIGHUP
            PosixSignal.SIGINT, Signal.SIGINT
            PosixSignal.SIGQUIT, Signal.SIGQUIT
            PosixSignal.SIGTERM, Signal.SIGTERM
            PosixSignal.SIGCHLD, Signal.SIGCHLD
            PosixSignal.SIGCONT, Signal.SIGCONT
            PosixSignal.SIGWINCH, Signal.SIGWINCH
            PosixSignal.SIGTTIN, Signal.SIGTTIN
            PosixSignal.SIGTTOU, Signal.SIGTTOU
            PosixSignal.SIGTSTP, Signal.SIGTSTP
        ]

    /// Signals this repo models that the enum has no member for. They reach a
    /// guest handler as `PosixSignalInvalid`.
    let private withoutEnumMember : Signal list =
        [ Signal.SIGPIPE ; Signal.SIGABRT ; Signal.SIGUSR1 ; Signal.SIGUSR2 ]

    /// A new member appearing upstream is a real divergence rather than a
    /// curiosity: `ofEnum` would send it to `ofPlatformSigno`, which refuses
    /// every non-positive number, so PawPrint would answer 0 and
    /// `PosixSignalRegistration.Register` would throw where real .NET
    /// registered the signal happily.
    [<Test>]
    let ``the enum has exactly the members this maps`` () : unit =
        System.Enum.GetValues<PosixSignal> ()
        |> Set.ofArray
        |> shouldEqual (enumMembers |> List.map fst |> Set.ofList)

    // ---------------------------------------------------------------------
    // `ofEnum`.
    // ---------------------------------------------------------------------

    /// Moved. The oracle is the enum's own value rather than a written-out -1.
    [<Test>]
    let ``ofEnum maps every member of the enum to the right case`` () : unit =
        for value, signal in enumMembers do
            PosixSignalPal.ofEnum (int value) |> shouldEqual (ValueSome signal)

    /// Moved. The BCL allows a guest to construct a `PosixSignal` from a raw
    /// native signo (`(PosixSignal)signo`). When that happens the value arrives
    /// at `GetPlatformSignalNumber` as a positive int; the real native code
    /// accepts it iff it is a recognised host signal, and PawPrint accepts it
    /// iff it is a modelled Linux signo.
    [<Test>]
    let ``ofEnum treats positives as Linux signos`` () : unit =
        PosixSignalPal.ofEnum 1 |> shouldEqual (ValueSome Signal.SIGHUP)
        PosixSignalPal.ofEnum 6 |> shouldEqual (ValueSome Signal.SIGABRT)
        PosixSignalPal.ofEnum 13 |> shouldEqual (ValueSome Signal.SIGPIPE)
        PosixSignalPal.ofEnum 28 |> shouldEqual (ValueSome Signal.SIGWINCH)

    /// Moved. Values outside the enum's range with no positive interpretation
    /// must produce `ValueNone`, so the arm returns 0 and the BCL raises
    /// `ArgumentOutOfRangeException`.
    [<Test>]
    let ``ofEnum returns ValueNone for 0 and unrecognised values`` () : unit =
        PosixSignalPal.ofEnum 0 |> shouldEqual ValueNone // PosixSignalInvalid sentinel
        PosixSignalPal.ofEnum -11 |> shouldEqual ValueNone
        PosixSignalPal.ofEnum -100 |> shouldEqual ValueNone
        PosixSignalPal.ofEnum System.Int32.MinValue |> shouldEqual ValueNone
        // Positive signos beyond `linuxSignalMax` (Linux's SIGRTMAX = 64) sit
        // outside any kernel's table and fail the same way real native code
        // does — the `if (signal > 0 && signal <= GetSignalMax()) return signal;`
        // branch falls through to `return 0;`.
        PosixSignalPal.ofEnum 65 |> shouldEqual ValueNone
        PosixSignalPal.ofEnum 100 |> shouldEqual ValueNone
        PosixSignalPal.ofEnum System.Int32.MaxValue |> shouldEqual ValueNone

    /// Moved. When a guest casts an arbitrary native signo to `PosixSignal`,
    /// `SystemNative_GetPlatformSignalNumber` returns the raw value unchanged
    /// if it sits within `GetSignalMax()`. Identity is preserved through
    /// `Signal.Other`, so the value round-trips and
    /// `PosixSignalRegistration.Register` accepts the registration.
    [<Test>]
    let ``ofEnum preserves raw identity for valid unmodelled positives`` () : unit =
        PosixSignalPal.ofEnum 4 |> shouldEqual (ValueSome (Signal.Other 4)) // SIGILL
        PosixSignalPal.ofEnum 5 |> shouldEqual (ValueSome (Signal.Other 5)) // SIGTRAP
        PosixSignalPal.ofEnum 7 |> shouldEqual (ValueSome (Signal.Other 7)) // SIGBUS
        PosixSignalPal.ofEnum 11 |> shouldEqual (ValueSome (Signal.Other 11)) // SIGSEGV

        PosixSignalPal.ofEnum Signal.linuxSignalMax
        |> shouldEqual (ValueSome (Signal.Other Signal.linuxSignalMax))

    /// Moved, and it is the boundary itself: `ofPlatformSigno` is what the
    /// enable/disable arms use for a signo arriving from
    /// `GetPlatformSignalNumber`, and it must produce the same `Signal` the
    /// enum path would, or the enable bit keys on a different case from the
    /// registration request.
    [<Test>]
    let ``the library's signo path agrees with the enum path on positives`` () : unit =
        for signo in 1 .. Signal.linuxSignalMax do
            Signal.ofPlatformSigno signo |> shouldEqual (PosixSignalPal.ofEnum signo)

    // ---------------------------------------------------------------------
    // `toEnum`, which had no test of any kind before this fixture.
    // ---------------------------------------------------------------------

    [<Test>]
    let ``toEnum is the inverse of ofEnum on every member`` () : unit =
        for value, signal in enumMembers do
            PosixSignalPal.toEnum signal |> shouldEqual (int value)

            PosixSignalPal.ofEnum (PosixSignalPal.toEnum signal)
            |> shouldEqual (ValueSome signal)

    /// `PosixSignalInvalid` is 0, and it is what real CoreCLR passes a handler
    /// for a signal the enum cannot name — `pal_signal.c` overwrites the
    /// out-parameter with it when `TryConvertSignalCodeToPosixSignal` fails.
    /// Every one of these is asserted, not just a representative: a mutation of
    /// one arm cannot be caught by a test of another.
    [<Test>]
    let ``toEnum answers PosixSignalInvalid for signals the enum cannot name`` () : unit =
        for signal in withoutEnumMember do
            PosixSignalPal.toEnum signal |> shouldEqual 0

        PosixSignalPal.toEnum (Signal.Other 4) |> shouldEqual 0 // SIGILL
        PosixSignalPal.toEnum (Signal.Other 9) |> shouldEqual 0 // SIGKILL
        PosixSignalPal.toEnum (Signal.Other Signal.linuxSignalMax) |> shouldEqual 0

    /// And nothing else answers 0, which is what stops the row above passing
    /// for a `toEnum` that had simply stopped working.
    [<Test>]
    let ``toEnum answers 0 only for signals the enum cannot name`` () : unit =
        for _, signal in enumMembers do
            PosixSignalPal.toEnum signal |> shouldNotEqual 0

    // ---------------------------------------------------------------------
    // The whole arm.
    // ---------------------------------------------------------------------

    /// Moved. Simulates `SystemNative_GetPlatformSignalNumber` end to end:
    /// `raw -> ofEnum -> toLinuxSigno -> signo`. Non-zero means the BCL can
    /// call `Enable/DisablePosixSignalHandling` with this signo and PawPrint
    /// will accept it; zero means `Register` throws.
    [<Test>]
    let ``the GetPlatformSignalNumber arm round-trips every accepted signal`` () : unit =
        let armBehaviour (raw : int) : int =
            match PosixSignalPal.ofEnum raw with
            | ValueSome s -> Signal.toLinuxSigno s
            | ValueNone -> 0

        for value, _ in enumMembers do
            armBehaviour (int value) |> shouldNotEqual 0

        // Unmodelled-but-valid positive signos round-trip via `Signal.Other`:
        // the guest casts `(PosixSignal)4` (SIGILL), the arm returns 4, and the
        // BCL forwards 4 unchanged to `EnablePosixSignalHandling`.
        armBehaviour 4 |> shouldEqual 4
        armBehaviour 11 |> shouldEqual 11
        armBehaviour Signal.linuxSignalMax |> shouldEqual Signal.linuxSignalMax

        armBehaviour 0 |> shouldEqual 0
        armBehaviour -11 |> shouldEqual 0
        armBehaviour (Signal.linuxSignalMax + 1) |> shouldEqual 0
        armBehaviour 100 |> shouldEqual 0

namespace WoofWare.PawPrint.Test

open System.Runtime.InteropServices
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PosixKernel

/// `PosixSignalPal` is a transcription of a managed enum and of one rule in
/// `pal_signal.c`, so its oracles are those: every row below reads
/// `System.Runtime.InteropServices.PosixSignal` from the BCL this test host
/// runs rather than restating its values, and the numbering rows call the
/// host's own `SystemNative_GetPlatformSignalNumber`.
///
/// That is a better position than the other three `*Pal` modules are in.
/// `Interop.Error`, the `SocketEvents` bits and the `AF_*` numbering are all
/// internal to the runtime, so their tests have to parse pinned source;
/// `PosixSignal` is public, so the real thing is callable here.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestPosixSignalPal =

    /// The real export, in the shim this test host runs against. Pure: it reads
    /// the shim's compiled-in `<signal.h>` constants and installs nothing, which
    /// is why it is safe to call in-process where the enable/disable entry
    /// points are not.
    [<DllImport("libSystem.Native", EntryPoint = "SystemNative_GetPlatformSignalNumber")>]
    extern int private hostGetPlatformSignalNumber(int posixSignal)

    let private everyNumbering : SignalNumbering list =
        [ SignalNumbering.Linux ; SignalNumbering.Darwin ]

    /// The numbering the shim this test host runs against was compiled with,
    /// or `None` on a host PawPrint does not model.
    let private hostNumbering () : SignalNumbering option =
        if RuntimeInformation.IsOSPlatform OSPlatform.OSX then
            Some SignalNumbering.Darwin
        elif RuntimeInformation.IsOSPlatform OSPlatform.Linux then
            Some SignalNumbering.Linux
        else
            None

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
    /// curiosity: `platformSignalNumber` would treat its negative value as an
    /// unknown number and answer 0, so `PosixSignalRegistration.Register`
    /// would throw `PlatformNotSupportedException` where real .NET registered
    /// the signal happily.
    [<Test>]
    let ``the enum has exactly the members this maps`` () : unit =
        System.Enum.GetValues<PosixSignal> ()
        |> Set.ofArray
        |> shouldEqual (enumMembers |> List.map fst |> Set.ofList)

    // ---------------------------------------------------------------------
    // `signalMax`.
    // ---------------------------------------------------------------------

    /// The values, which nothing else pins: `GetSignalMax()` is `SIGRTMAX`
    /// where the header defines it (glibc: 64) and `NSIG` otherwise (Darwin:
    /// 32). The Darwin number is one past the last signal Darwin has, and the
    /// host-equality test below is what shows the shim really does admit it.
    [<Test>]
    let ``signalMax is SIGRTMAX on Linux and NSIG on Darwin`` () : unit =
        PosixSignalPal.signalMax SignalNumbering.Linux |> shouldEqual 64
        PosixSignalPal.signalMax SignalNumbering.Darwin |> shouldEqual 32

    [<Test>]
    let ``signalMax is never below the kernel's own ceiling`` () : unit =
        // The shim asserts `signalCode <= GetSignalMax()` on every signo it
        // is handed, so a ceiling below the kernel's would refuse real
        // signals.
        for numbering in everyNumbering do
            (PosixSignalPal.signalMax numbering >= Signal.highestSignoUnder numbering)
            |> shouldEqual true

    // ---------------------------------------------------------------------
    // `platformSignalNumber`.
    // ---------------------------------------------------------------------

    [<Test>]
    let ``platformSignalNumber maps every member of the enum to its signo under each numbering`` () : unit =
        for numbering in everyNumbering do
            for value, signal in enumMembers do
                PosixSignalPal.platformSignalNumber numbering (int value)
                |> shouldEqual (Signal.toRawSignoUnder numbering signal)

    /// The three members whose number differs are asserted as literals too,
    /// so that this fixture sees a transposed table without going through the
    /// library's own function.
    [<Test>]
    let ``platformSignalNumber answers the divergent rows per numbering`` () : unit =
        PosixSignalPal.platformSignalNumber SignalNumbering.Linux (int PosixSignal.SIGCHLD)
        |> shouldEqual 17

        PosixSignalPal.platformSignalNumber SignalNumbering.Darwin (int PosixSignal.SIGCHLD)
        |> shouldEqual 20

        PosixSignalPal.platformSignalNumber SignalNumbering.Linux (int PosixSignal.SIGCONT)
        |> shouldEqual 18

        PosixSignalPal.platformSignalNumber SignalNumbering.Darwin (int PosixSignal.SIGCONT)
        |> shouldEqual 19

        PosixSignalPal.platformSignalNumber SignalNumbering.Linux (int PosixSignal.SIGTSTP)
        |> shouldEqual 20

        PosixSignalPal.platformSignalNumber SignalNumbering.Darwin (int PosixSignal.SIGTSTP)
        |> shouldEqual 18

    /// The BCL allows a guest to construct a `PosixSignal` from a raw native
    /// signo (`(PosixSignal)signo`). The real native code echoes it back iff
    /// `signal > 0 && signal <= GetSignalMax()`, without asking whether it is
    /// a signal it knows — so Darwin's 32 comes back as 32.
    [<Test>]
    let ``platformSignalNumber echoes every positive within signalMax and nothing beyond`` () : unit =
        for numbering in everyNumbering do
            let signalMax = PosixSignalPal.signalMax numbering

            for signo in 1..signalMax do
                PosixSignalPal.platformSignalNumber numbering signo |> shouldEqual signo

            PosixSignalPal.platformSignalNumber numbering (signalMax + 1) |> shouldEqual 0
            PosixSignalPal.platformSignalNumber numbering 100 |> shouldEqual 0

            PosixSignalPal.platformSignalNumber numbering System.Int32.MaxValue
            |> shouldEqual 0

    /// Values outside the enum's range with no positive interpretation must
    /// produce 0, so the BCL raises `PlatformNotSupportedException`.
    [<Test>]
    let ``platformSignalNumber answers 0 for PosixSignalInvalid and unrecognised negatives`` () : unit =
        for numbering in everyNumbering do
            PosixSignalPal.platformSignalNumber numbering 0 |> shouldEqual 0
            PosixSignalPal.platformSignalNumber numbering -11 |> shouldEqual 0
            PosixSignalPal.platformSignalNumber numbering -100 |> shouldEqual 0

            PosixSignalPal.platformSignalNumber numbering System.Int32.MinValue
            |> shouldEqual 0

    /// The host-equality row, and the one that can falsify a column rather
    /// than restate it: the shim this test host runs against was compiled
    /// against one platform's `<signal.h>`, so its answer for every input is
    /// what PawPrint must answer under that platform's numbering. macOS
    /// locally and Linux in CI each check one column.
    [<Test>]
    let ``platformSignalNumber agrees with this host's SystemNative_GetPlatformSignalNumber`` () : unit =
        match hostNumbering () with
        | None -> Assert.Ignore $"no Unix host to measure (%s{RuntimeInformation.OSDescription})"
        | Some numbering ->

        let inputs : int list =
            [ System.Int32.MinValue ; System.Int32.MaxValue ] @ [ -100 .. 100 ]

        for raw in inputs do
            let host = hostGetPlatformSignalNumber raw
            let modelled = PosixSignalPal.platformSignalNumber numbering raw

            if host <> modelled then
                failwith
                    $"%O{numbering}: for input %d{raw} PawPrint answers %d{modelled} but this host's SystemNative_GetPlatformSignalNumber answers %d{host}"

    // ---------------------------------------------------------------------
    // `toEnum`.
    // ---------------------------------------------------------------------

    [<Test>]
    let ``toEnum names every member of the enum`` () : unit =
        for value, signal in enumMembers do
            PosixSignalPal.toEnum signal |> shouldEqual (int value)

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
        PosixSignalPal.toEnum (Signal.Other 64) |> shouldEqual 0

    /// And nothing else answers 0, which is what stops the row above passing
    /// for a `toEnum` that had simply stopped working.
    [<Test>]
    let ``toEnum answers 0 only for signals the enum cannot name`` () : unit =
        for _, signal in enumMembers do
            PosixSignalPal.toEnum signal |> shouldNotEqual 0

    /// The round trip the dispatcher relies on: the enum value it hands a
    /// handler, fed back through the registration arm under any numbering,
    /// names the signal the handler was registered for.
    [<Test>]
    let ``platformSignalNumber inverts toEnum on every member under each numbering`` () : unit =
        for numbering in everyNumbering do
            for _, signal in enumMembers do
                PosixSignalPal.platformSignalNumber numbering (PosixSignalPal.toEnum signal)
                |> shouldEqual (Signal.toRawSignoUnder numbering signal)

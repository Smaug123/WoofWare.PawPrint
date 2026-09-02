namespace WoofWare.PosixKernel.Test

open FsUnitTyped
open Microsoft.FSharp.Reflection
open NUnit.Framework
open WoofWare.PosixKernel

/// Unit tests for the `Signal` conversion helpers: the signo table under each
/// numbering, what a signal can be caught as, and what a kernel does with one
/// by default.
///
/// The other half of the conversion math — anything involving a client's
/// managed `PosixSignal` enum — is not here, because it is not in this library:
/// see `WoofWare.PawPrint.Test/TestPosixSignalPal.fs`. What remains is what a
/// kernel itself knows.
///
/// Both columns are written out as literals here, so that a table swapped or
/// transposed in `Signal.fs` is caught on any machine. `TestSignalAgainstHost`
/// checks whichever column belongs to the machine the suite runs on against
/// that machine's own `kill -l`, which is what keeps these literals honest.
///
/// These functions are exhaustively verified rather than sampled because the
/// arms that consume them cannot be tested any other way: enabling or disabling
/// a signal through a direct P/Invoke on the real CLR installs a sigaction
/// handler in the test host's own process.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestSignal =

    let private everyNumbering : SignalNumbering list =
        [ SignalNumbering.Linux ; SignalNumbering.Darwin ]

    /// The 15 modelled signals paired with their Linux signo, measured on Linux
    /// 6.18.5 / glibc 2.41 with a C probe that printed each `SIG*` macro.
    let private linuxColumn : (Signal * int) list =
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
            Signal.SIGURG, 23
            Signal.SIGWINCH, 28
        ]

    /// The same 15 with their Darwin signo, measured the same way on Darwin
    /// 25.6.0. Six rows differ from the Linux column: SIGUSR1, SIGUSR2,
    /// SIGCHLD, SIGCONT, SIGTSTP and SIGURG.
    let private darwinColumn : (Signal * int) list =
        [
            Signal.SIGHUP, 1
            Signal.SIGINT, 2
            Signal.SIGQUIT, 3
            Signal.SIGABRT, 6
            Signal.SIGPIPE, 13
            Signal.SIGTERM, 15
            Signal.SIGURG, 16
            Signal.SIGTSTP, 18
            Signal.SIGCONT, 19
            Signal.SIGCHLD, 20
            Signal.SIGTTIN, 21
            Signal.SIGTTOU, 22
            Signal.SIGWINCH, 28
            Signal.SIGUSR1, 30
            Signal.SIGUSR2, 31
        ]

    let private column (numbering : SignalNumbering) : (Signal * int) list =
        match numbering with
        | SignalNumbering.Linux -> linuxColumn
        | SignalNumbering.Darwin -> darwinColumn

    /// A new named case reaches `toRawSignoUnder` through the compiler's
    /// exhaustiveness check, but nothing forces it into the two tables above
    /// or into `ofRawSignoUnder`'s search list; this does.
    [<Test>]
    let ``both columns name every case but Other`` () : unit =
        let cases = FSharpType.GetUnionCases typeof<Signal> |> Array.length

        for numbering in everyNumbering do
            column numbering |> List.length |> shouldEqual (cases - 1)

            column numbering
            |> List.map fst
            |> List.distinct
            |> List.length
            |> shouldEqual (cases - 1)

    /// The ceilings' *values*, which nothing else pins: every other test here
    /// names `highestSignoUnder` symbolically, so all of them move with it and
    /// none of them can see it being wrong.
    ///
    /// 64 is glibc's `SIGRTMAX` and 31 is one less than Darwin's `NSIG`, both
    /// measured by installing `SIG_DFL` for every number up to `NSIG + 1` and
    /// reporting the refusals. Getting either wrong is guest-visible: at 63, a
    /// Linux guest registering signal 64 is refused where real Linux accepts
    /// it.
    [<Test>]
    let ``the highest signo is SIGRTMAX on Linux and NSIG minus one on Darwin`` () : unit =
        Signal.highestSignoUnder SignalNumbering.Linux |> shouldEqual 64
        Signal.highestSignoUnder SignalNumbering.Darwin |> shouldEqual 31

    [<Test>]
    let ``toRawSignoUnder produces the measured signo for every named case under each numbering`` () : unit =
        for numbering in everyNumbering do
            for signal, signo in column numbering do
                Signal.toRawSignoUnder numbering signal |> shouldEqual signo

    /// The six rows that differ are the whole reason the numbering exists,
    /// so they are asserted by name as well as through the tables.
    [<Test>]
    let ``the six divergent rows are the ones measured`` () : unit =
        let divergent : (Signal * int * int) list =
            [
                Signal.SIGUSR1, 10, 30
                Signal.SIGUSR2, 12, 31
                Signal.SIGCHLD, 17, 20
                Signal.SIGCONT, 18, 19
                Signal.SIGTSTP, 20, 18
                Signal.SIGURG, 23, 16
            ]

        for signal, linux, darwin in divergent do
            Signal.toRawSignoUnder SignalNumbering.Linux signal |> shouldEqual linux
            Signal.toRawSignoUnder SignalNumbering.Darwin signal |> shouldEqual darwin

        // And nothing else does.
        for signal, _ in linuxColumn do
            if not (divergent |> List.exists (fun (s, _, _) -> s = signal)) then
                Signal.toRawSignoUnder SignalNumbering.Linux signal
                |> shouldEqual (Signal.toRawSignoUnder SignalNumbering.Darwin signal)

    [<Test>]
    let ``toRawSignoUnder on Other returns the raw value unchanged`` () : unit =
        // The `Other` constructor carries raw identity so callers that
        // produced a positive native signo can round-trip it through the
        // Signal type without losing the value. The out-of-range values below
        // are not built by anything here — `ofRawSignoUnder` produces only
        // signos in `(0, highestSignoUnder]` — but the case is public and
        // enforces nothing, and this pins that `toRawSignoUnder` stays a
        // projection rather than acquiring an opinion about its payload.
        for numbering in everyNumbering do
            Signal.toRawSignoUnder numbering (Signal.Other 42) |> shouldEqual 42
            Signal.toRawSignoUnder numbering (Signal.Other 999) |> shouldEqual 999
            Signal.toRawSignoUnder numbering (Signal.Other -77) |> shouldEqual -77
            Signal.toRawSignoUnder numbering (Signal.Other 0) |> shouldEqual 0

    [<Test>]
    let ``ofRawSignoUnder is the inverse of toRawSignoUnder on every named signal`` () : unit =
        for numbering in everyNumbering do
            for signal, signo in column numbering do
                Signal.ofRawSignoUnder numbering signo |> shouldEqual (ValueSome signal)

    /// The round trip in the other direction, over every number the kernel
    /// has: a signo that names no case comes back as `Other` carrying itself,
    /// so the enable/disable arms can key on it and hand it back unchanged.
    [<Test>]
    let ``every signo the kernel has round-trips through ofRawSignoUnder`` () : unit =
        for numbering in everyNumbering do
            for signo in 1 .. Signal.highestSignoUnder numbering do
                match Signal.ofRawSignoUnder numbering signo with
                | ValueNone -> failwith $"%O{numbering}: signo %d{signo} is within the kernel's range but was refused"
                | ValueSome signal ->
                    Signal.toRawSignoUnder numbering signal |> shouldEqual signo

                    // Named iff the column says so.
                    match column numbering |> List.tryFind (fun (_, n) -> n = signo) with
                    | Some (named, _) -> signal |> shouldEqual named
                    | None -> signal |> shouldEqual (Signal.Other signo)

    [<Test>]
    let ``ofRawSignoUnder refuses numbers that are not signals on that platform`` () : unit =
        for numbering in everyNumbering do
            let highest = Signal.highestSignoUnder numbering
            Signal.ofRawSignoUnder numbering 0 |> shouldEqual ValueNone
            Signal.ofRawSignoUnder numbering -1 |> shouldEqual ValueNone
            Signal.ofRawSignoUnder numbering (highest + 1) |> shouldEqual ValueNone
            Signal.ofRawSignoUnder numbering 100 |> shouldEqual ValueNone
            Signal.ofRawSignoUnder numbering System.Int32.MaxValue |> shouldEqual ValueNone

        // The Darwin ceiling is the one that bites: 32 is a real-time signal
        // on Linux and nothing at all on Darwin, even though CoreCLR's shim
        // admits it there.
        Signal.ofRawSignoUnder SignalNumbering.Linux 32
        |> shouldEqual (ValueSome (Signal.Other 32))

        Signal.ofRawSignoUnder SignalNumbering.Darwin 32 |> shouldEqual ValueNone

    /// The same number is a different signal under each numbering; this is
    /// what the enable arm gets wrong if it reads a Darwin guest's signo under
    /// Linux's table.
    [<Test>]
    let ``a raw signo names a signal only under its own numbering`` () : unit =
        Signal.ofRawSignoUnder SignalNumbering.Linux 17
        |> shouldEqual (ValueSome Signal.SIGCHLD)

        Signal.ofRawSignoUnder SignalNumbering.Darwin 17
        |> shouldEqual (ValueSome (Signal.Other 17)) // SIGSTOP

        Signal.ofRawSignoUnder SignalNumbering.Linux 19
        |> shouldEqual (ValueSome (Signal.Other 19)) // SIGSTOP

        Signal.ofRawSignoUnder SignalNumbering.Darwin 19
        |> shouldEqual (ValueSome Signal.SIGCONT)

        Signal.ofRawSignoUnder SignalNumbering.Linux 30
        |> shouldEqual (ValueSome (Signal.Other 30)) // SIGPWR

        Signal.ofRawSignoUnder SignalNumbering.Darwin 30
        |> shouldEqual (ValueSome Signal.SIGUSR1)

        Signal.ofRawSignoUnder SignalNumbering.Linux 23
        |> shouldEqual (ValueSome Signal.SIGURG)

        Signal.ofRawSignoUnder SignalNumbering.Darwin 23
        |> shouldEqual (ValueSome (Signal.Other 23)) // SIGIO

    [<Test>]
    let ``canonicalUnder names an Other carrying a named signal's number and leaves the rest alone`` () : unit =
        for numbering in everyNumbering do
            for signal, signo in column numbering do
                Signal.canonicalUnder numbering (Signal.Other signo) |> shouldEqual signal
                Signal.canonicalUnder numbering signal |> shouldEqual signal

            Signal.canonicalUnder numbering (Signal.Other 9) |> shouldEqual (Signal.Other 9) // SIGKILL

            Signal.canonicalUnder numbering (Signal.Other 0) |> shouldEqual (Signal.Other 0)

            Signal.canonicalUnder numbering (Signal.Other 999)
            |> shouldEqual (Signal.Other 999)

    [<Test>]
    let ``isUncatchableUnder flags exactly the signos sigaction refuses`` () : unit =
        // Measured by installing SIG_DFL for every number up to NSIG + 1:
        // SIGKILL and SIGSTOP on both (POSIX), plus glibc's reserved 32 and
        // 33 on Linux. Neither SIGKILL nor SIGSTOP is a named case, so they
        // only ever arrive as `Signal.Other n`.
        let refused (numbering : SignalNumbering) : int list =
            match numbering with
            | SignalNumbering.Linux -> [ 9 ; 19 ; 32 ; 33 ]
            | SignalNumbering.Darwin -> [ 9 ; 17 ]

        for numbering in everyNumbering do
            for signo in 1 .. Signal.highestSignoUnder numbering do
                let signal =
                    match Signal.ofRawSignoUnder numbering signo with
                    | ValueSome signal -> signal
                    | ValueNone -> failwith $"%O{numbering}: signo %d{signo} is within range"

                Signal.isUncatchableUnder numbering signal
                |> shouldEqual (List.contains signo (refused numbering))

            // Every named case can be caught — that's the whole point of
            // installing a handler.
            for signal, _ in column numbering do
                Signal.isUncatchableUnder numbering signal |> shouldEqual false

    /// SIGSTOP is 17 on Darwin and 19 on Linux, so each of those numbers is
    /// uncatchable under exactly one numbering. A classifier that read the
    /// payload under the wrong table would refuse a Darwin guest's SIGCONT.
    [<Test>]
    let ``SIGSTOP's number is uncatchable under its own numbering only`` () : unit =
        Signal.isUncatchableUnder SignalNumbering.Darwin (Signal.Other 17)
        |> shouldEqual true

        Signal.isUncatchableUnder SignalNumbering.Linux (Signal.Other 17)
        |> shouldEqual false

        Signal.isUncatchableUnder SignalNumbering.Linux (Signal.Other 19)
        |> shouldEqual true

        Signal.isUncatchableUnder SignalNumbering.Darwin (Signal.Other 19)
        |> shouldEqual false

    [<Test>]
    let ``defaultDispositionUnder classifies every named signal the same way everywhere`` () : unit =
        // POSIX default for these signals is Terminate (some with a core
        // dump, but PawPrint collapses both into a single Terminate case
        // since we don't model core dumps). Drives the catch-all branch
        // in `pal_signal.c`'s `SystemNative_HandleNonCanceledPosixSignal`.
        let expected : (Signal * DefaultDisposition) list =
            [
                Signal.SIGHUP, DefaultDisposition.Terminate
                Signal.SIGINT, DefaultDisposition.Terminate
                Signal.SIGQUIT, DefaultDisposition.Terminate
                Signal.SIGABRT, DefaultDisposition.Terminate
                Signal.SIGUSR1, DefaultDisposition.Terminate
                Signal.SIGUSR2, DefaultDisposition.Terminate
                Signal.SIGPIPE, DefaultDisposition.Terminate
                Signal.SIGTERM, DefaultDisposition.Terminate
                Signal.SIGCHLD, DefaultDisposition.Ignore
                Signal.SIGWINCH, DefaultDisposition.Ignore
                Signal.SIGURG, DefaultDisposition.Ignore
                Signal.SIGCONT, DefaultDisposition.Continue
                Signal.SIGTSTP, DefaultDisposition.Stop
                Signal.SIGTTIN, DefaultDisposition.Stop
                Signal.SIGTTOU, DefaultDisposition.Stop
            ]

        expected |> List.length |> shouldEqual (List.length linuxColumn)

        for numbering in everyNumbering do
            for signal, disposition in expected do
                Signal.defaultDispositionUnder numbering signal |> shouldEqual disposition

    /// The unnamed signals whose kernel default is not Terminate, measured by
    /// having a forked child raise each signal on itself under SIG_DFL: SIGIO
    /// (29 on Linux terminates; 23 on Darwin is discarded), SIGINFO (Darwin's
    /// 29, discarded), and SIGSTOP (which stops). The dispatcher must not fall
    /// through to Terminate for these just because the case is unnamed — and
    /// must not discard Linux's 29 because Darwin's is discarded.
    [<Test>]
    let ``defaultDispositionUnder classifies unnamed signos under their own numbering`` () : unit =
        let expected (numbering : SignalNumbering) : (int * DefaultDisposition) list =
            match numbering with
            | SignalNumbering.Linux ->
                [
                    19, DefaultDisposition.Stop // SIGSTOP
                    23, DefaultDisposition.Ignore // SIGURG, named: reached via ofRawSignoUnder below
                    29, DefaultDisposition.Terminate // SIGIO
                    16, DefaultDisposition.Terminate // SIGSTKFLT
                ]
            | SignalNumbering.Darwin ->
                [
                    17, DefaultDisposition.Stop // SIGSTOP
                    16, DefaultDisposition.Ignore // SIGURG, named: reached via ofRawSignoUnder below
                    23, DefaultDisposition.Ignore // SIGIO
                    29, DefaultDisposition.Ignore // SIGINFO
                    19, DefaultDisposition.Continue // SIGCONT, named: reached via ofRawSignoUnder below
                ]

        for numbering in everyNumbering do
            for signo, disposition in expected numbering do
                let signal =
                    match Signal.ofRawSignoUnder numbering signo with
                    | ValueSome signal -> signal
                    | ValueNone -> failwith $"%O{numbering}: signo %d{signo} is within range"

                Signal.defaultDispositionUnder numbering signal |> shouldEqual disposition

    /// `Other` is public, so a client can spell a named signal as `Other`
    /// carrying its number; both classifiers must answer for the signal that
    /// number is under the numbering, not for "some unnamed signal".
    [<Test>]
    let ``classifiers read an Other carrying a named signal's number as that signal`` () : unit =
        for numbering in everyNumbering do
            for signal, signo in column numbering do
                Signal.defaultDispositionUnder numbering (Signal.Other signo)
                |> shouldEqual (Signal.defaultDispositionUnder numbering signal)

                Signal.isUncatchableUnder numbering (Signal.Other signo)
                |> shouldEqual (Signal.isUncatchableUnder numbering signal)

        // The rows a Terminate fallback would get wrong.
        Signal.defaultDispositionUnder SignalNumbering.Linux (Signal.Other 17)
        |> shouldEqual DefaultDisposition.Ignore // SIGCHLD

        Signal.defaultDispositionUnder SignalNumbering.Darwin (Signal.Other 20)
        |> shouldEqual DefaultDisposition.Ignore // SIGCHLD

        Signal.defaultDispositionUnder SignalNumbering.Linux (Signal.Other 18)
        |> shouldEqual DefaultDisposition.Continue // SIGCONT

        Signal.defaultDispositionUnder SignalNumbering.Darwin (Signal.Other 19)
        |> shouldEqual DefaultDisposition.Continue // SIGCONT

        Signal.defaultDispositionUnder SignalNumbering.Darwin (Signal.Other 18)
        |> shouldEqual DefaultDisposition.Stop // SIGTSTP

    [<Test>]
    let ``defaultDispositionUnder routes every other unnamed signo to Terminate`` () : unit =
        // The POSIX default for an unrecognised signal is Terminate, and
        // `SystemNative_HandleNonCanceledPosixSignal`'s `default:` branch
        // is the catch-all. SIGILL (4), SIGFPE (8), SIGSEGV (11) and the
        // real-time signals all terminate by default and are not in the
        // modelled set; the measured exceptions are the ones in the test
        // above, and everything else in range must classify as Terminate.
        let exceptions (numbering : SignalNumbering) : int list =
            match numbering with
            | SignalNumbering.Linux -> [ 19 ]
            | SignalNumbering.Darwin -> [ 17 ; 23 ; 29 ]

        for numbering in everyNumbering do
            for signo in 1 .. Signal.highestSignoUnder numbering do
                match Signal.ofRawSignoUnder numbering signo with
                | ValueSome (Signal.Other _ as signal) when not (List.contains signo (exceptions numbering)) ->
                    Signal.defaultDispositionUnder numbering signal
                    |> shouldEqual DefaultDisposition.Terminate
                | _ -> ()

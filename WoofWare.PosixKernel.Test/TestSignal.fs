namespace WoofWare.PosixKernel.Test

open FsCheck
open FsCheck.FSharp
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

    /// Every standard signal Linux's `<signal.h>` defines, with its number:
    /// `docs/plans/2026-08-23-posix-kernel-extraction/signal-names.c`, run
    /// 2026-10-02 on Linux 6.18.5 with glibc 2.41 for aarch64 and for x86-64,
    /// which agreed on every row (`signal-names.linux-*.txt`).
    let private linuxColumn : (Signal * int) list =
        [
            Signal.SIGHUP, 1
            Signal.SIGINT, 2
            Signal.SIGQUIT, 3
            Signal.SIGILL, 4
            Signal.SIGTRAP, 5
            Signal.SIGABRT, 6
            Signal.SIGBUS, 7
            Signal.SIGFPE, 8
            Signal.SIGKILL, 9
            Signal.SIGUSR1, 10
            Signal.SIGSEGV, 11
            Signal.SIGUSR2, 12
            Signal.SIGPIPE, 13
            Signal.SIGALRM, 14
            Signal.SIGTERM, 15
            Signal.SIGSTKFLT, 16
            Signal.SIGCHLD, 17
            Signal.SIGCONT, 18
            Signal.SIGSTOP, 19
            Signal.SIGTSTP, 20
            Signal.SIGTTIN, 21
            Signal.SIGTTOU, 22
            Signal.SIGURG, 23
            Signal.SIGXCPU, 24
            Signal.SIGXFSZ, 25
            Signal.SIGVTALRM, 26
            Signal.SIGPROF, 27
            Signal.SIGWINCH, 28
            Signal.SIGIO, 29
            Signal.SIGPWR, 30
            Signal.SIGSYS, 31
        ]

    /// The same for Darwin's `<signal.h>`, from the same probe on Darwin 27.0.0
    /// arm64 (`signal-names.darwin-27.0-arm64.txt`).
    let private darwinColumn : (Signal * int) list =
        [
            Signal.SIGHUP, 1
            Signal.SIGINT, 2
            Signal.SIGQUIT, 3
            Signal.SIGILL, 4
            Signal.SIGTRAP, 5
            Signal.SIGABRT, 6
            Signal.SIGEMT, 7
            Signal.SIGFPE, 8
            Signal.SIGKILL, 9
            Signal.SIGBUS, 10
            Signal.SIGSEGV, 11
            Signal.SIGSYS, 12
            Signal.SIGPIPE, 13
            Signal.SIGALRM, 14
            Signal.SIGTERM, 15
            Signal.SIGURG, 16
            Signal.SIGSTOP, 17
            Signal.SIGTSTP, 18
            Signal.SIGCONT, 19
            Signal.SIGCHLD, 20
            Signal.SIGTTIN, 21
            Signal.SIGTTOU, 22
            Signal.SIGIO, 23
            Signal.SIGXCPU, 24
            Signal.SIGXFSZ, 25
            Signal.SIGVTALRM, 26
            Signal.SIGPROF, 27
            Signal.SIGWINCH, 28
            Signal.SIGINFO, 29
            Signal.SIGUSR1, 30
            Signal.SIGUSR2, 31
        ]

    let private column (numbering : SignalNumbering) : (Signal * int) list =
        match numbering with
        | SignalNumbering.Linux -> linuxColumn
        | SignalNumbering.Darwin -> darwinColumn

    /// Every signal the numbering has: its column, and on Linux the 33
    /// real-time signals, 32 to 64.
    let private everySignalUnder (numbering : SignalNumbering) : (Signal * int) list =
        match numbering with
        | SignalNumbering.Linux -> linuxColumn @ [ for signo in 32..64 -> Signal.RealTime (signo - 32), signo ]
        | SignalNumbering.Darwin -> darwinColumn

    /// The flavour-specific rows, by name: a signal one platform lacks has no
    /// number there at all.
    let private onlyUnder (numbering : SignalNumbering) : Signal list =
        match numbering with
        | SignalNumbering.Linux -> [ Signal.SIGSTKFLT ; Signal.SIGPWR ]
        | SignalNumbering.Darwin -> [ Signal.SIGEMT ; Signal.SIGINFO ]

    /// A new named case reaches `toRawSignoUnder` through the compiler's
    /// exhaustiveness check, but nothing forces it into the two tables above;
    /// this does.
    [<Test>]
    let ``between them the columns name every case but RealTime, each once`` () : unit =
        let named =
            FSharpType.GetUnionCases typeof<Signal>
            |> Array.filter (fun case -> case.Name <> "RealTime")
            |> Array.map (fun case -> case.Name)
            |> Set.ofArray

        let inColumns =
            linuxColumn @ darwinColumn
            |> List.map (fun (signal, _) -> $"%O{signal}")
            |> Set.ofList

        inColumns |> shouldEqual named

        for numbering in everyNumbering do
            let signals = column numbering |> List.map fst
            signals |> List.distinct |> List.length |> shouldEqual (List.length signals)

            let numbers = column numbering |> List.map snd
            numbers |> shouldEqual [ 1..31 ]

        // The cases one column lacks are exactly the other flavour's own.
        let linux = linuxColumn |> List.map fst |> Set.ofList
        let darwin = darwinColumn |> List.map fst |> Set.ofList

        Set.difference linux darwin
        |> shouldEqual (Set.ofList (onlyUnder SignalNumbering.Linux))

        Set.difference darwin linux
        |> shouldEqual (Set.ofList (onlyUnder SignalNumbering.Darwin))

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
    let ``toRawSignoUnder produces the measured signo for every signal under each numbering`` () : unit =
        for numbering in everyNumbering do
            for signal, signo in everySignalUnder numbering do
                (signal, Signal.toRawSignoUnder numbering signal) |> shouldEqual (signal, signo)

    /// The rows that differ between the platforms are the reason the
    /// numbering exists, so they are asserted by name as well as through the
    /// tables.
    [<Test>]
    let ``the divergent rows are the ones measured`` () : unit =
        let divergent : (Signal * int * int) list =
            [
                Signal.SIGBUS, 7, 10
                Signal.SIGUSR1, 10, 30
                Signal.SIGUSR2, 12, 31
                Signal.SIGCHLD, 17, 20
                Signal.SIGCONT, 18, 19
                Signal.SIGSTOP, 19, 17
                Signal.SIGTSTP, 20, 18
                Signal.SIGURG, 23, 16
                Signal.SIGIO, 29, 23
                Signal.SIGSYS, 31, 12
            ]

        for signal, linux, darwin in divergent do
            Signal.toRawSignoUnder SignalNumbering.Linux signal |> shouldEqual linux
            Signal.toRawSignoUnder SignalNumbering.Darwin signal |> shouldEqual darwin

        // And nothing else that both platforms have does.
        for signal, _ in linuxColumn do
            if
                Signal.existsUnder SignalNumbering.Darwin signal
                && not (divergent |> List.exists (fun (s, _, _) -> s = signal))
            then
                Signal.toRawSignoUnder SignalNumbering.Linux signal
                |> shouldEqual (Signal.toRawSignoUnder SignalNumbering.Darwin signal)

    [<Test>]
    let ``ofRawSignoUnder is the inverse of toRawSignoUnder on every signal`` () : unit =
        for numbering in everyNumbering do
            for signal, signo in everySignalUnder numbering do
                (signo, Signal.ofRawSignoUnder numbering signo)
                |> shouldEqual (signo, ValueSome signal)

    /// The round trip in the other direction, over every number the kernel
    /// has and a margin either side: a number is a signal exactly when it is
    /// in range, and then it names the case the measured table says.
    [<Test>]
    let ``every signo the kernel has round-trips through ofRawSignoUnder, and nothing else parses`` () : unit =
        for numbering in everyNumbering do
            let table = everySignalUnder numbering

            for signo in -200 .. 200 do
                match Signal.ofRawSignoUnder numbering signo with
                | ValueNone ->
                    if signo >= 1 && signo <= Signal.highestSignoUnder numbering then
                        failwith $"%O{numbering}: signo %d{signo} is within the kernel's range but was refused"
                | ValueSome signal ->
                    (signo, Signal.toRawSignoUnder numbering signal) |> shouldEqual (signo, signo)
                    Signal.existsUnder numbering signal |> shouldEqual true

                    match table |> List.tryFind (fun (_, n) -> n = signo) with
                    | Some (named, _) -> signal |> shouldEqual named
                    | None ->
                        failwith $"%O{numbering}: signo %d{signo} parsed as %O{signal}, but no measured row has it"

    /// Arbitrary signals, `RealTime` with any offset included, against both
    /// numberings: a signal the numbering has renders to a number that parses
    /// back to it, and one it lacks has no number at all.
    [<Test>]
    let ``a signal round-trips through its number exactly when the numbering has it`` () : unit =
        let realTimeOffsets =
            Gen.oneof [ Gen.choose (-3, 36) ; ArbMap.defaults |> ArbMap.generate<int> ]

        let signalGen : Gen<Signal> =
            let named =
                FSharpType.GetUnionCases typeof<Signal>
                |> Array.filter (fun case -> case.Name <> "RealTime")
                |> Array.map (fun case -> FSharpValue.MakeUnion (case, [||]) :?> Signal)

            Gen.oneof [ Gen.elements named ; realTimeOffsets |> Gen.map Signal.RealTime ]

        let property (numbering : SignalNumbering) (signal : Signal) : unit =
            let expected = everySignalUnder numbering |> List.tryFind (fun (s, _) -> s = signal)

            Signal.existsUnder numbering signal |> shouldEqual expected.IsSome

            match expected with
            | Some (_, signo) ->
                Signal.toRawSignoUnder numbering signal |> shouldEqual signo
                Signal.ofRawSignoUnder numbering signo |> shouldEqual (ValueSome signal)
            | None ->
                Assert.Throws<exn> (fun () -> Signal.toRawSignoUnder numbering signal |> ignore)
                |> ignore

        let config = Config.QuickThrowOnFailure.WithMaxTest 2000

        Check.One (
            config,
            Prop.forAll
                (Arb.fromGen (Gen.zip (Gen.elements everyNumbering) signalGen))
                (fun (numbering, signal) -> property numbering signal)
        )

    [<Test>]
    let ``existsUnder admits each flavour's own signals and real-time offsets 0 to 32 only`` () : unit =
        for numbering in everyNumbering do
            for signal in onlyUnder numbering do
                Signal.existsUnder numbering signal |> shouldEqual true

        for signal in onlyUnder SignalNumbering.Linux do
            Signal.existsUnder SignalNumbering.Darwin signal |> shouldEqual false

        for signal in onlyUnder SignalNumbering.Darwin do
            Signal.existsUnder SignalNumbering.Linux signal |> shouldEqual false

        Signal.existsUnder SignalNumbering.Linux (Signal.RealTime 0) |> shouldEqual true

        Signal.existsUnder SignalNumbering.Linux (Signal.RealTime 32)
        |> shouldEqual true

        Signal.existsUnder SignalNumbering.Linux (Signal.RealTime -1)
        |> shouldEqual false

        Signal.existsUnder SignalNumbering.Linux (Signal.RealTime 33)
        |> shouldEqual false

        Signal.existsUnder SignalNumbering.Darwin (Signal.RealTime 0)
        |> shouldEqual false

    [<Test>]
    let ``ofRawSignoUnder refuses numbers that are not signals on that platform`` () : unit =
        for numbering in everyNumbering do
            let highest = Signal.highestSignoUnder numbering
            Signal.ofRawSignoUnder numbering 0 |> shouldEqual ValueNone
            Signal.ofRawSignoUnder numbering -1 |> shouldEqual ValueNone
            Signal.ofRawSignoUnder numbering (highest + 1) |> shouldEqual ValueNone
            Signal.ofRawSignoUnder numbering 100 |> shouldEqual ValueNone
            Signal.ofRawSignoUnder numbering System.Int32.MaxValue |> shouldEqual ValueNone
            Signal.ofRawSignoUnder numbering System.Int32.MinValue |> shouldEqual ValueNone

        // The Darwin ceiling is the one that bites: 32 is a real-time signal
        // on Linux and nothing at all on Darwin, even though CoreCLR's shim
        // admits it there.
        Signal.ofRawSignoUnder SignalNumbering.Linux 32
        |> shouldEqual (ValueSome (Signal.RealTime 0))

        Signal.ofRawSignoUnder SignalNumbering.Darwin 32 |> shouldEqual ValueNone

    /// The same number is a different signal under each numbering; this is
    /// what the enable arm gets wrong if it reads a Darwin guest's signo under
    /// Linux's table.
    [<Test>]
    let ``a raw signo names a signal only under its own numbering`` () : unit =
        Signal.ofRawSignoUnder SignalNumbering.Linux 17
        |> shouldEqual (ValueSome Signal.SIGCHLD)

        Signal.ofRawSignoUnder SignalNumbering.Darwin 17
        |> shouldEqual (ValueSome Signal.SIGSTOP)

        Signal.ofRawSignoUnder SignalNumbering.Linux 19
        |> shouldEqual (ValueSome Signal.SIGSTOP)

        Signal.ofRawSignoUnder SignalNumbering.Darwin 19
        |> shouldEqual (ValueSome Signal.SIGCONT)

        Signal.ofRawSignoUnder SignalNumbering.Linux 30
        |> shouldEqual (ValueSome Signal.SIGPWR)

        Signal.ofRawSignoUnder SignalNumbering.Darwin 30
        |> shouldEqual (ValueSome Signal.SIGUSR1)

        Signal.ofRawSignoUnder SignalNumbering.Linux 23
        |> shouldEqual (ValueSome Signal.SIGURG)

        Signal.ofRawSignoUnder SignalNumbering.Darwin 23
        |> shouldEqual (ValueSome Signal.SIGIO)

    /// Every number the kernel has under `numbering`, with the signal it names.
    let private everySigno (numbering : SignalNumbering) : (int * Signal) list =
        [
            for signo in 1 .. Signal.highestSignoUnder numbering do
                match Signal.ofRawSignoUnder numbering signo with
                | ValueSome signal -> signo, signal
                | ValueNone -> failwith $"%O{numbering}: signo %d{signo} is within range"
        ]

    [<Test>]
    let ``isUncatchableUnder flags exactly the signos sigaction refuses`` () : unit =
        // Measured by installing SIG_DFL for every number up to NSIG + 1:
        // SIGKILL and SIGSTOP on both (POSIX), plus glibc's reserved 32 and
        // 33 on Linux.
        let refused (numbering : SignalNumbering) : int list =
            match numbering with
            | SignalNumbering.Linux -> [ 9 ; 19 ; 32 ; 33 ]
            | SignalNumbering.Darwin -> [ 9 ; 17 ]

        for numbering in everyNumbering do
            for signo, signal in everySigno numbering do
                (signo, Signal.isUncatchableUnder numbering signal)
                |> shouldEqual (signo, List.contains signo (refused numbering))

    /// SIGSTOP is 17 on Darwin and 19 on Linux; the classifier answers for the
    /// signal, so neither number's other reading is refused.
    [<Test>]
    let ``SIGKILL and SIGSTOP are uncatchable under both numberings, and their numbers only under their own``
        ()
        : unit
        =
        for numbering in everyNumbering do
            Signal.isUncatchableUnder numbering Signal.SIGKILL |> shouldEqual true
            Signal.isUncatchableUnder numbering Signal.SIGSTOP |> shouldEqual true

        Signal.isUncatchableUnder SignalNumbering.Linux Signal.SIGCHLD
        |> shouldEqual false // 17

        Signal.isUncatchableUnder SignalNumbering.Darwin Signal.SIGCONT
        |> shouldEqual false // 19

    [<Test>]
    let ``isUnblockableUnder flags exactly the signos the mask calls silently drop`` () : unit =
        // Measured by setting each signo's bit directly (bypassing
        // sigaddset's own screening), blocking via pthread_sigmask and
        // sigprocmask, and reading the mask back: SIGKILL and SIGSTOP on
        // both (POSIX), plus glibc's reserved 32 and 33 on Linux — the raw
        // rt_sigprocmask syscall accepts those two, so that pair is the
        // libc's screening, not the kernel's. `TestSignalMaskAgainstHost`
        // repeats the measurement on whichever flavour runs the suite.
        let dropped (numbering : SignalNumbering) : int list =
            match numbering with
            | SignalNumbering.Linux -> [ 9 ; 19 ; 32 ; 33 ]
            | SignalNumbering.Darwin -> [ 9 ; 17 ]

        for numbering in everyNumbering do
            for signo, signal in everySigno numbering do
                (signo, Signal.isUnblockableUnder numbering signal)
                |> shouldEqual (signo, List.contains signo (dropped numbering))

    [<Test>]
    let ``isRealTimeUnder flags exactly Linux's 32 to 64`` () : unit =
        // Measured on Linux 6.18.5: three generations of signo 36 while
        // blocked deliver three times where three of SIGUSR1 deliver once.
        // The threshold is the kernel's own 32 (glibc makes 32 and 33
        // unobservable, so those two rows follow the kernel's rule); the
        // ceiling is the kernel's 64. Darwin has no real-time signals.
        for numbering in everyNumbering do
            for signo, signal in everySigno numbering do
                (signo, Signal.isRealTimeUnder numbering signal)
                |> shouldEqual (signo, numbering = SignalNumbering.Linux && signo >= 32)

        // A signal the numbering lacks is not a real-time signal either: this
        // classifier is public in a standalone package, so a client can reach
        // it without going through ofRawSignoUnder first.
        for offset in [ -1 ; 33 ; 100 ; System.Int32.MaxValue ; System.Int32.MinValue ] do
            Signal.isRealTimeUnder SignalNumbering.Linux (Signal.RealTime offset)
            |> shouldEqual false

        Signal.isRealTimeUnder SignalNumbering.Darwin (Signal.RealTime 0)
        |> shouldEqual false

    [<Test>]
    let ``a blocked ignored signal stays pending under Linux numbering, and only SIGCONT does under Darwin`` () : unit =
        // Probe-pinned like the sigaction facts (measuring in-process would
        // mean changing the test host's own dispositions): every signal
        // sigaction accepts, ignored at generation by SIG_IGN or by its
        // default, generated by kill and by pthread_kill while blocked
        // (docs/plans/2026-08-23-posix-kernel-extraction/signal-disposition-table.c,
        // 2026-09-26). On Linux 6.18.5 each stayed pending and was delivered
        // to a handler installed before the unblock; on Darwin 27.0.0 none
        // was pending but SIGCONT (19), which was.
        for numbering in everyNumbering do
            for signo, signal in everySigno numbering do
                let expected =
                    match numbering with
                    | SignalNumbering.Linux -> true
                    | SignalNumbering.Darwin -> signo = 19

                (signo, Signal.blockedIgnoredSignalStaysPendingUnder numbering signal)
                |> shouldEqual (signo, expected)

    [<Test>]
    let ``dumpsCoreUnder answers the measured core class`` () : unit =
        // docs/plans/2026-08-23-posix-kernel-extraction/core-class.c,
        // 2026-09-26, every signal number under SIG_DFL. Linux 6.18.5 with
        // RLIMIT_CORE infinite: these carried WCOREDUMP. Darwin 27.0.0, where
        // an unprivileged probe cannot write the dump: these raised EXC_CRASH,
        // which xnu raises for a death by exactly the signals whose SA_CORE
        // property also gates the dump. Darwin's XCPU (24) and XFSZ (25) did
        // not, where Linux's do.
        let expected (numbering : SignalNumbering) : Set<int> =
            match numbering with
            | SignalNumbering.Linux -> Set.ofList [ 3 ; 4 ; 5 ; 6 ; 7 ; 8 ; 11 ; 24 ; 25 ; 31 ]
            | SignalNumbering.Darwin -> Set.ofList [ 3 ; 4 ; 5 ; 6 ; 7 ; 8 ; 10 ; 11 ; 12 ]

        for numbering in everyNumbering do
            for signo, signal in everySigno numbering do
                (signo, Signal.dumpsCoreUnder numbering signal)
                |> shouldEqual (signo, Set.contains signo (expected numbering))

    /// Every number's default, measured by having a forked child raise each
    /// signal on itself under SIG_DFL on Linux 6.18.5 and Darwin 25.6.0, and
    /// re-swept on 2026-09-26 by `signal-disposition-table.c`. Keyed by number,
    /// so that it pins the table independently of how `Signal.fs` names the
    /// rows. SIGTSTP, SIGTTIN and SIGTTOU, which both kernels discarded in the
    /// probe's orphaned process group, take POSIX's Stop.
    [<Test>]
    let ``defaultDispositionUnder classifies every signo under its own numbering`` () : unit =
        let expected (numbering : SignalNumbering) (signo : int) : DefaultDisposition =
            match numbering, signo with
            // SIGCHLD, SIGURG, SIGWINCH
            | SignalNumbering.Linux, (17 | 23 | 28) -> DefaultDisposition.Ignore
            // SIGCONT
            | SignalNumbering.Linux, 18 -> DefaultDisposition.Continue
            // SIGSTOP, SIGTSTP, SIGTTIN, SIGTTOU
            | SignalNumbering.Linux, (19 | 20 | 21 | 22) -> DefaultDisposition.Stop
            // SIGURG, SIGCHLD, SIGIO, SIGWINCH, SIGINFO
            | SignalNumbering.Darwin, (16 | 20 | 23 | 28 | 29) -> DefaultDisposition.Ignore
            // SIGCONT
            | SignalNumbering.Darwin, 19 -> DefaultDisposition.Continue
            // SIGSTOP, SIGTSTP, SIGTTIN, SIGTTOU
            | SignalNumbering.Darwin, (17 | 18 | 21 | 22) -> DefaultDisposition.Stop
            | _, _ -> DefaultDisposition.Terminate

        for numbering in everyNumbering do
            for signo, signal in everySigno numbering do
                (signo, Signal.defaultDispositionUnder numbering signal)
                |> shouldEqual (signo, expected numbering signo)

    /// SIGIO is the one named signal whose default differs between the
    /// platforms: Linux's 29 terminates, and Darwin's 23 is discarded.
    [<Test>]
    let ``SIGIO's default is the only one that depends on the numbering`` () : unit =
        Signal.defaultDispositionUnder SignalNumbering.Linux Signal.SIGIO
        |> shouldEqual DefaultDisposition.Terminate

        Signal.defaultDispositionUnder SignalNumbering.Darwin Signal.SIGIO
        |> shouldEqual DefaultDisposition.Ignore

        for signal, _ in linuxColumn do
            if signal <> Signal.SIGIO && Signal.existsUnder SignalNumbering.Darwin signal then
                Signal.defaultDispositionUnder SignalNumbering.Linux signal
                |> shouldEqual (Signal.defaultDispositionUnder SignalNumbering.Darwin signal)

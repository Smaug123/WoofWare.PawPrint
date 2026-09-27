namespace WoofWare.PosixKernel.Test

open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// How a process ended, and what its parent reads of that: `ExitStatus` and
/// `ProcessTermination`.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestProcessTermination =

    /// One row of `docs/plans/2026-08-23-posix-kernel-extraction/wait-status.c`'s
    /// output: how the child ended, the argument (an exit status or a signal
    /// number), the raw status `waitpid(2)` stored, the probe's decoding of it, and
    /// `waitid(2)`'s `si_code` and `si_status`.
    type private Row = string * int * int * string * string * int

    /// Linux 6.18.5 x86-64 (under Rosetta in Apple's `container`, root), measured
    /// 2026-09-27; aarch64 printed the same rows. The `signal` rows are with
    /// `RLIMIT_CORE` 0, and the last three with it unlimited.
    let private linuxRows : Row list =
        [
            "exit", 0, 0x0, "exited(0)", "CLD_EXITED", 0
            "exit", 1, 0x100, "exited(1)", "CLD_EXITED", 1
            "exit", 7, 0x700, "exited(7)", "CLD_EXITED", 7
            "exit", 127, 0x7f00, "exited(127)", "CLD_EXITED", 127
            "exit", 128, 0x8000, "exited(128)", "CLD_EXITED", 128
            "exit", 255, 0xff00, "exited(255)", "CLD_EXITED", 255
            "exit", 256, 0x0, "exited(0)", "CLD_EXITED", 0
            "exit", 257, 0x100, "exited(1)", "CLD_EXITED", 1
            "exit", 263, 0x700, "exited(7)", "CLD_EXITED", 7
            "exit", 511, 0xff00, "exited(255)", "CLD_EXITED", 255
            "exit", 65535, 0xff00, "exited(255)", "CLD_EXITED", 255
            "exit", 65543, 0x700, "exited(7)", "CLD_EXITED", 7
            "exit", -1, 0xff00, "exited(255)", "CLD_EXITED", 255
            "exit", -256, 0x0, "exited(0)", "CLD_EXITED", 0
            "exit", 2147483647, 0xff00, "exited(255)", "CLD_EXITED", 255
            "exit", System.Int32.MinValue, 0x0, "exited(0)", "CLD_EXITED", 0
            "_exit", 0, 0x0, "exited(0)", "CLD_EXITED", 0
            "_exit", 1, 0x100, "exited(1)", "CLD_EXITED", 1
            "_exit", 7, 0x700, "exited(7)", "CLD_EXITED", 7
            "_exit", 127, 0x7f00, "exited(127)", "CLD_EXITED", 127
            "_exit", 128, 0x8000, "exited(128)", "CLD_EXITED", 128
            "_exit", 255, 0xff00, "exited(255)", "CLD_EXITED", 255
            "_exit", 256, 0x0, "exited(0)", "CLD_EXITED", 0
            "_exit", 257, 0x100, "exited(1)", "CLD_EXITED", 1
            "_exit", 263, 0x700, "exited(7)", "CLD_EXITED", 7
            "_exit", 511, 0xff00, "exited(255)", "CLD_EXITED", 255
            "_exit", 65535, 0xff00, "exited(255)", "CLD_EXITED", 255
            "_exit", 65543, 0x700, "exited(7)", "CLD_EXITED", 7
            "_exit", -1, 0xff00, "exited(255)", "CLD_EXITED", 255
            "_exit", -256, 0x0, "exited(0)", "CLD_EXITED", 0
            "_exit", 2147483647, 0xff00, "exited(255)", "CLD_EXITED", 255
            "_exit", System.Int32.MinValue, 0x0, "exited(0)", "CLD_EXITED", 0
            "signal", 1, 0x1, "signaled(1)", "CLD_KILLED", 1
            "signal", 2, 0x2, "signaled(2)", "CLD_KILLED", 2
            "signal", 3, 0x3, "signaled(3)", "CLD_KILLED", 3
            "signal", 4, 0x4, "signaled(4)", "CLD_KILLED", 4
            "signal", 5, 0x5, "signaled(5)", "CLD_KILLED", 5
            "signal", 6, 0x6, "signaled(6)", "CLD_KILLED", 6
            "signal", 7, 0x7, "signaled(7)", "CLD_KILLED", 7
            "signal", 8, 0x8, "signaled(8)", "CLD_KILLED", 8
            "signal", 9, 0x9, "signaled(9)", "CLD_KILLED", 9
            "signal", 10, 0xa, "signaled(10)", "CLD_KILLED", 10
            "signal", 11, 0xb, "signaled(11)", "CLD_KILLED", 11
            "signal", 12, 0xc, "signaled(12)", "CLD_KILLED", 12
            "signal", 13, 0xd, "signaled(13)", "CLD_KILLED", 13
            "signal", 14, 0xe, "signaled(14)", "CLD_KILLED", 14
            "signal", 15, 0xf, "signaled(15)", "CLD_KILLED", 15
            "signal", 16, 0x10, "signaled(16)", "CLD_KILLED", 16
            "signal", 17, 0xc900, "exited(201)", "CLD_EXITED", 201
            "signal", 18, 0xc900, "exited(201)", "CLD_EXITED", 201
            "signal", 19, 0x137f, "stopped(19)", "CLD_STOPPED", 19
            "signal", 20, 0xc900, "exited(201)", "CLD_EXITED", 201
            "signal", 21, 0xc900, "exited(201)", "CLD_EXITED", 201
            "signal", 22, 0xc900, "exited(201)", "CLD_EXITED", 201
            "signal", 23, 0xc900, "exited(201)", "CLD_EXITED", 201
            "signal", 24, 0x18, "signaled(24)", "CLD_KILLED", 24
            "signal", 25, 0x19, "signaled(25)", "CLD_KILLED", 25
            "signal", 26, 0x1a, "signaled(26)", "CLD_KILLED", 26
            "signal", 27, 0x1b, "signaled(27)", "CLD_KILLED", 27
            "signal", 28, 0xc900, "exited(201)", "CLD_EXITED", 201
            "signal", 29, 0x1d, "signaled(29)", "CLD_KILLED", 29
            "signal", 30, 0x1e, "signaled(30)", "CLD_KILLED", 30
            "signal", 31, 0x1f, "signaled(31)", "CLD_KILLED", 31
            "signal", 34, 0x22, "signaled(34)", "CLD_KILLED", 34
            "signal", 35, 0x23, "signaled(35)", "CLD_KILLED", 35
            "signal", 36, 0x24, "signaled(36)", "CLD_KILLED", 36
            "signal", 37, 0x25, "signaled(37)", "CLD_KILLED", 37
            "signal", 38, 0x26, "signaled(38)", "CLD_KILLED", 38
            "signal", 39, 0x27, "signaled(39)", "CLD_KILLED", 39
            "signal", 40, 0x28, "signaled(40)", "CLD_KILLED", 40
            "signal", 41, 0x29, "signaled(41)", "CLD_KILLED", 41
            "signal", 42, 0x2a, "signaled(42)", "CLD_KILLED", 42
            "signal", 43, 0x2b, "signaled(43)", "CLD_KILLED", 43
            "signal", 44, 0x2c, "signaled(44)", "CLD_KILLED", 44
            "signal", 45, 0x2d, "signaled(45)", "CLD_KILLED", 45
            "signal", 46, 0x2e, "signaled(46)", "CLD_KILLED", 46
            "signal", 47, 0x2f, "signaled(47)", "CLD_KILLED", 47
            "signal", 48, 0x30, "signaled(48)", "CLD_KILLED", 48
            "signal", 49, 0x31, "signaled(49)", "CLD_KILLED", 49
            "signal", 50, 0x32, "signaled(50)", "CLD_KILLED", 50
            "signal", 51, 0x33, "signaled(51)", "CLD_KILLED", 51
            "signal", 52, 0x34, "signaled(52)", "CLD_KILLED", 52
            "signal", 53, 0x35, "signaled(53)", "CLD_KILLED", 53
            "signal", 54, 0x36, "signaled(54)", "CLD_KILLED", 54
            "signal", 55, 0x37, "signaled(55)", "CLD_KILLED", 55
            "signal", 56, 0x38, "signaled(56)", "CLD_KILLED", 56
            "signal", 57, 0x39, "signaled(57)", "CLD_KILLED", 57
            "signal", 58, 0x3a, "signaled(58)", "CLD_KILLED", 58
            "signal", 59, 0x3b, "signaled(59)", "CLD_KILLED", 59
            "signal", 60, 0x3c, "signaled(60)", "CLD_KILLED", 60
            "signal", 61, 0x3d, "signaled(61)", "CLD_KILLED", 61
            "signal", 62, 0x3e, "signaled(62)", "CLD_KILLED", 62
            "signal", 63, 0x3f, "signaled(63)", "CLD_KILLED", 63
            "signal", 64, 0x40, "signaled(64)", "CLD_KILLED", 64
            "signal_rlimit_core_inf", 3, 0x83, "signaled(3,core)", "CLD_DUMPED", 3
            "signal_rlimit_core_inf", 6, 0x86, "signaled(6,core)", "CLD_DUMPED", 6
            "signal_rlimit_core_inf", 11, 0x8b, "signaled(11,core)", "CLD_DUMPED", 11
        ]

    /// Darwin 27.0.0 arm64 (uid 501), measured 2026-09-27. No core dump could be
    /// written there, so the last three rows, with `RLIMIT_CORE` unlimited, say
    /// `signaled` without the core flag.
    let private darwinRows : Row list =
        [
            "exit", 0, 0x0, "exited(0)", "CLD_EXITED", 0
            "exit", 1, 0x100, "exited(1)", "CLD_EXITED", 1
            "exit", 7, 0x700, "exited(7)", "CLD_EXITED", 7
            "exit", 127, 0x7f00, "exited(127)", "CLD_EXITED", 127
            "exit", 128, 0x8000, "exited(128)", "CLD_EXITED", 128
            "exit", 255, 0xff00, "exited(255)", "CLD_EXITED", 255
            "exit", 256, 0x0, "exited(0)", "CLD_EXITED", 256
            "exit", 257, 0x100, "exited(1)", "CLD_EXITED", 257
            "exit", 263, 0x700, "exited(7)", "CLD_EXITED", 263
            "exit", 511, 0xff00, "exited(255)", "CLD_EXITED", 511
            "exit", 65535, 0xff00, "exited(255)", "CLD_EXITED", 65535
            "exit", 65543, 0x700, "exited(7)", "CLD_EXITED", 65543
            "exit", -1, 0xff00, "exited(255)", "CLD_EXITED", 16777215
            "exit", -256, 0x0, "exited(0)", "CLD_EXITED", 16776960
            "exit", 2147483647, 0xff00, "exited(255)", "CLD_EXITED", 16777215
            "exit", System.Int32.MinValue, 0x0, "exited(0)", "CLD_EXITED", 0
            "_exit", 0, 0x0, "exited(0)", "CLD_EXITED", 0
            "_exit", 1, 0x100, "exited(1)", "CLD_EXITED", 1
            "_exit", 7, 0x700, "exited(7)", "CLD_EXITED", 7
            "_exit", 127, 0x7f00, "exited(127)", "CLD_EXITED", 127
            "_exit", 128, 0x8000, "exited(128)", "CLD_EXITED", 128
            "_exit", 255, 0xff00, "exited(255)", "CLD_EXITED", 255
            "_exit", 256, 0x0, "exited(0)", "CLD_EXITED", 256
            "_exit", 257, 0x100, "exited(1)", "CLD_EXITED", 257
            "_exit", 263, 0x700, "exited(7)", "CLD_EXITED", 263
            "_exit", 511, 0xff00, "exited(255)", "CLD_EXITED", 511
            "_exit", 65535, 0xff00, "exited(255)", "CLD_EXITED", 65535
            "_exit", 65543, 0x700, "exited(7)", "CLD_EXITED", 65543
            "_exit", -1, 0xff00, "exited(255)", "CLD_EXITED", 16777215
            "_exit", -256, 0x0, "exited(0)", "CLD_EXITED", 16776960
            "_exit", 2147483647, 0xff00, "exited(255)", "CLD_EXITED", 16777215
            "_exit", System.Int32.MinValue, 0x0, "exited(0)", "CLD_EXITED", 0
            "signal", 1, 0x1, "signaled(1)", "CLD_KILLED", 1
            "signal", 2, 0x2, "signaled(2)", "CLD_KILLED", 2
            "signal", 3, 0x3, "signaled(3)", "CLD_KILLED", 3
            "signal", 4, 0x4, "signaled(4)", "CLD_KILLED", 4
            "signal", 5, 0x5, "signaled(5)", "CLD_KILLED", 5
            "signal", 6, 0x6, "signaled(6)", "CLD_KILLED", 6
            "signal", 7, 0x7, "signaled(7)", "CLD_KILLED", 7
            "signal", 8, 0x8, "signaled(8)", "CLD_KILLED", 8
            "signal", 9, 0x9, "signaled(9)", "CLD_KILLED", 9
            "signal", 10, 0xa, "signaled(10)", "CLD_KILLED", 10
            "signal", 11, 0xb, "signaled(11)", "CLD_KILLED", 11
            "signal", 12, 0xc, "signaled(12)", "CLD_KILLED", 12
            "signal", 13, 0xd, "signaled(13)", "CLD_KILLED", 13
            "signal", 14, 0xe, "signaled(14)", "CLD_KILLED", 14
            "signal", 15, 0xf, "signaled(15)", "CLD_KILLED", 15
            "signal", 16, 0xc900, "exited(201)", "CLD_EXITED", 201
            "signal", 17, 0x117f, "stopped(17)", "CLD_STOPPED", 17
            "signal", 18, 0xc900, "exited(201)", "CLD_EXITED", 201
            "signal", 19, 0xc900, "exited(201)", "CLD_EXITED", 201
            "signal", 20, 0xc900, "exited(201)", "CLD_EXITED", 201
            "signal", 21, 0xc900, "exited(201)", "CLD_EXITED", 201
            "signal", 22, 0xc900, "exited(201)", "CLD_EXITED", 201
            "signal", 23, 0xc900, "exited(201)", "CLD_EXITED", 201
            "signal", 24, 0x18, "signaled(24)", "CLD_KILLED", 24
            "signal", 25, 0x19, "signaled(25)", "CLD_KILLED", 25
            "signal", 26, 0x1a, "signaled(26)", "CLD_KILLED", 26
            "signal", 27, 0x1b, "signaled(27)", "CLD_KILLED", 27
            "signal", 28, 0xc900, "exited(201)", "CLD_EXITED", 201
            "signal", 29, 0xc900, "exited(201)", "CLD_EXITED", 201
            "signal", 30, 0x1e, "signaled(30)", "CLD_KILLED", 30
            "signal", 31, 0x1f, "signaled(31)", "CLD_KILLED", 31
            "signal_rlimit_core_inf", 3, 0x3, "signaled(3)", "CLD_KILLED", 3
            "signal_rlimit_core_inf", 6, 0x6, "signaled(6)", "CLD_KILLED", 6
            "signal_rlimit_core_inf", 11, 0xb, "signaled(11)", "CLD_KILLED", 11
        ]

    let private platformFor (flavour : SimulatedUnixFlavour) : SimulatedUnixPlatform =
        match flavour with
        | SimulatedUnixFlavour.Linux -> SimulatedUnixPlatform.linuxX64
        | SimulatedUnixFlavour.Darwin -> SimulatedUnixPlatform.macOsArm64

    let private codeOf (name : string) : WaitIdCode =
        match name with
        | "CLD_EXITED" -> WaitIdCode.Exited
        | "CLD_KILLED" -> WaitIdCode.Killed
        | "CLD_DUMPED" -> WaitIdCode.Dumped
        | other -> failwith $"a row names si_code %s{other}, which no row about a child that ended should"

    /// Check `termination`'s renderings against a row that says the child ended so.
    let private assertRendersAs
        (platform : SimulatedUnixPlatform)
        (row : Row)
        (termination : ProcessTermination)
        : unit
        =
        let _, _, raw, _, code, status = row

        (row, ProcessTermination.waitpidStatus platform termination)
        |> shouldEqual (row, Ok raw)

        (row, ProcessTermination.waitidStatus platform termination)
        |> shouldEqual (row, Ok (codeOf code, status))

    let private signalOf (flavour : SimulatedUnixFlavour) (row : Row) : Signal =
        let _, signo, _, _, _, _ = row

        match Signal.ofRawSignoUnder (SimulatedUnixPlatform.signalNumbering (platformFor flavour)) signo with
        | ValueSome signal -> signal
        | ValueNone -> failwith $"row %A{row} names a signal %O{flavour} has not got"

    let private assertRows (flavour : SimulatedUnixFlavour) (rows : Row list) : unit =
        let platform = platformFor flavour
        let numbering = SimulatedUnixPlatform.signalNumbering platform
        let mutable signalled = 0
        let mutable dumped = 0

        for row in rows do
            let how, arg, raw, decoded, _, _ = row

            match how with
            | "exit"
            | "_exit" ->
                let termination = ProcessTermination.Exited (ExitStatus.ofExitArgument flavour arg)

                assertRendersAs platform row termination

                (row, ProcessTermination.shellStatus numbering termination)
                |> shouldEqual (row, raw >>> 8)
            | "signal" ->
                let signal = signalOf flavour row
                let defaultDisposition = Signal.defaultDispositionUnder numbering signal

                if decoded = $"signaled(%d{arg})" then
                    (row, defaultDisposition) |> shouldEqual (row, DefaultDisposition.Terminate)
                    let termination = ProcessTermination.Signaled (signal, false)
                    assertRendersAs platform row termination

                    (row, ProcessTermination.shellStatus numbering termination)
                    |> shouldEqual (row, 128 + arg)

                    signalled <- signalled + 1
                elif decoded = "exited(201)" then
                    // The probe's child survived its own signal and exited 201.
                    (row, defaultDisposition = DefaultDisposition.Terminate)
                    |> shouldEqual (row, false)
                elif decoded = $"stopped(%d{arg})" then
                    (row, defaultDisposition) |> shouldEqual (row, DefaultDisposition.Stop)
                else
                    failwith $"unexpected row %A{row}"
            | "signal_rlimit_core_inf" ->
                let signal = signalOf flavour row

                (row, Signal.dumpsCoreUnder numbering signal) |> shouldEqual (row, true)

                match flavour with
                | SimulatedUnixFlavour.Linux ->
                    (row, decoded) |> shouldEqual (row, $"signaled(%d{arg},core)")
                    assertRendersAs platform row (ProcessTermination.Signaled (signal, true))
                    dumped <- dumped + 1
                | SimulatedUnixFlavour.Darwin ->
                    // No dump was written, so the row is a death without one; a death
                    // with one is what is unmeasured.
                    (row, decoded) |> shouldEqual (row, $"signaled(%d{arg})")
                    assertRendersAs platform row (ProcessTermination.Signaled (signal, false))

                    ProcessTermination.waitpidStatus platform (ProcessTermination.Signaled (signal, true))
                    |> shouldEqual (Error (WaitStatusRefusal.CoreDumpOnDarwin signal))

                    ProcessTermination.waitidStatus platform (ProcessTermination.Signaled (signal, true))
                    |> shouldEqual (Error (WaitStatusRefusal.CoreDumpOnDarwin signal))
            | other -> failwith $"unexpected row kind %s{other}"

        // Every signal whose default terminates, bar glibc's two reserved ones.
        match flavour with
        | SimulatedUnixFlavour.Linux ->
            signalled |> shouldEqual 54
            dumped |> shouldEqual 3
        | SimulatedUnixFlavour.Darwin -> signalled |> shouldEqual 21

    [<Test>]
    let ``each ending renders as the Linux rows of wait-status.c say`` () : unit =
        assertRows SimulatedUnixFlavour.Linux linuxRows

    [<Test>]
    let ``each ending renders as the Darwin rows of wait-status.c say`` () : unit =
        assertRows SimulatedUnixFlavour.Darwin darwinRows

    [<Test>]
    let ``an exit keeps the flavour's bits, and waitpid reports the low eight of them`` () : unit =
        let property (flavourIsLinux : bool) (argument : int32) : unit =
            let flavour =
                if flavourIsLinux then
                    SimulatedUnixFlavour.Linux
                else
                    SimulatedUnixFlavour.Darwin

            let status = ExitStatus.ofExitArgument flavour argument
            let retained = ExitStatus.waitidStatus status

            // What each flavour keeps, stated apart from the implementation.
            let expected =
                match flavour with
                | SimulatedUnixFlavour.Linux -> argument &&& 0xff
                | SimulatedUnixFlavour.Darwin -> argument &&& 0xffffff

            retained |> shouldEqual expected
            ExitStatus.waitpidExitCode status |> shouldEqual (retained &&& 0xff)

            let termination = ProcessTermination.Exited status
            let platform = platformFor flavour

            ProcessTermination.waitpidStatus platform termination
            |> shouldEqual (Ok ((retained &&& 0xff) <<< 8))

            ProcessTermination.waitidStatus platform termination
            |> shouldEqual (Ok (WaitIdCode.Exited, retained))

            ProcessTermination.shellStatus (SimulatedUnixPlatform.signalNumbering platform) termination
            |> shouldEqual (retained &&& 0xff)

        let gen =
            Gen.zip
                (ArbMap.defaults |> ArbMap.generate<bool>)
                (Gen.oneof
                    [
                        ArbMap.defaults |> ArbMap.generate<int32>
                        Gen.choose (-300, 300)
                        Gen.elements [ 0xffffff ; 0x1000000 ; System.Int32.MaxValue ; System.Int32.MinValue ]
                    ])

        Check.One (
            Config.QuickThrowOnFailure.WithMaxTest 2000,
            Prop.forAll (Arb.fromGen gen) (fun (isLinux, argument) -> property isLinux argument)
        )

    [<Test>]
    let ``a death by signal reads as that signal, with the core flag only where it was measured`` () : unit =
        for flavour in [ SimulatedUnixFlavour.Linux ; SimulatedUnixFlavour.Darwin ] do
            let platform = platformFor flavour
            let numbering = SimulatedUnixPlatform.signalNumbering platform

            let terminating =
                [ 1 .. Signal.highestSignoUnder numbering ]
                |> List.choose (fun signo ->
                    match Signal.ofRawSignoUnder numbering signo with
                    | ValueSome signal when
                        Signal.defaultDispositionUnder numbering signal = DefaultDisposition.Terminate
                        ->
                        Some (signo, signal)
                    | _ -> None
                )

            for signo, signal in terminating do
                for coreDumped in [ false ; true ] do
                    let termination = ProcessTermination.Signaled (signal, coreDumped)

                    ProcessTermination.shellStatus numbering termination
                    |> shouldEqual (128 + signo)

                    match flavour, coreDumped with
                    | _, false ->
                        ProcessTermination.waitpidStatus platform termination |> shouldEqual (Ok signo)

                        ProcessTermination.waitidStatus platform termination
                        |> shouldEqual (Ok (WaitIdCode.Killed, signo))
                    | SimulatedUnixFlavour.Linux, true ->
                        ProcessTermination.waitpidStatus platform termination
                        |> shouldEqual (Ok (signo ||| 0x80))

                        ProcessTermination.waitidStatus platform termination
                        |> shouldEqual (Ok (WaitIdCode.Dumped, signo))
                    | SimulatedUnixFlavour.Darwin, true ->
                        ProcessTermination.waitpidStatus platform termination
                        |> shouldEqual (Error (WaitStatusRefusal.CoreDumpOnDarwin signal))

namespace WoofWare.PawPrint.Test

open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PosixKernel

/// Guests that a signal kills, compared against the same guest on the real
/// runtime.
///
/// Kept apart from `DifferentialOracle.compareOutcomes`, which refuses every
/// signal-terminated outcome: the real runtime reports a signalled child's
/// exit status as `128 + signo` (`SystemNative_WaitPidExitedNoHang`), exactly as
/// a guest returning that number would, so the only comparison available is of
/// the exit code. That is a weaker claim than the rest of the corpus makes, and
/// a case has to say it wants it. PawPrint's side stays structured: its outcome
/// must be `SignalTerminated` with the case's signal.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
// Runs guests under the interpreter; see AGENTS.md for why that makes it
// `Explicit`, and CI selects it by category.
[<Category("Guest")>]
[<Explicit>]
module TestSignalTerminatedCases =
    let private assy = typeof<RunResult>.Assembly

    type SignalTerminatedCase =
        {
            FileName : string
            /// The signo the guest is killed by, the same under both
            /// numberings for every case here, so that the host's flavour
            /// does not change the comparison.
            Signo : int
        }

        override this.ToString () : string = this.FileName

    let cases : SignalTerminatedCase list =
        [
            {
                // `Process.Kill` on itself: SIGKILL, through SystemNative_Kill.
                FileName = "KillSelf.cs"
                Signo = 9
            }
            {
                // libc's kill(2) of SIGTERM to itself, with a
                // PosixSignalRegistration handler that does not cancel it.
                FileName = "PosixSignalKillNotCancelled.cs"
                Signo = 15
            }
        ]

    [<TestCaseSource(nameof cases)>]
    let ``A guest killed by a signal dies of it on both runtimes`` (case : SignalTerminatedCase) : unit =
        let source = Assembly.getEmbeddedResourceAsString case.FileName assy
        let image = Roslyn.compile [ source ]

        let _messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", case.FileName ]

        use _loggerFactoryResource = loggerFactory

        let dotnetRuntimes = FrameworkUnderTest.runtimeDirs ()

        use peImage = new MemoryStream (image)

        let realResult, pawPrintResult =
            DifferentialOracle.alongsideInterpreted
                (fun () -> RealRuntime.executeWithRealRuntime [||] image)
                (fun () ->
                    BoundedRun.run
                        loggerFactory
                        case.FileName
                        (Some case.FileName)
                        peImage
                        (HostConfig.Default dotnetRuntimes)
                )

        match pawPrintResult with
        | RunOutcome.SignalTerminated (state, signal) ->
            let numbering = SimulatedUnixPlatform.signalNumbering state.Kernel.UnixPlatform
            Signal.toRawSignoUnder numbering signal |> shouldEqual case.Signo
        | other -> failwith $"%s{case.FileName}: expected PawPrint to report a signal termination, got %O{other}"

        match realResult with
        | RealRuntimeResult.NormalExit exitCode -> exitCode |> shouldEqual (128 + case.Signo)
        | other ->
            failwith $"%s{case.FileName}: expected the real runtime to exit with 128 + %d{case.Signo}, got %O{other}"

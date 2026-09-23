namespace WoofWare.PawPrint.Test

open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// A float's width at a control-flow join is decided over every incoming path, as CoreCLR's
/// importer decides it: when one arm delivers a double, the float32 arm is widened on arrival and
/// arithmetic after the join is double even when the float32 arm executed. No compiler emits that
/// shape, so the guest emits it with `Reflection.Emit`, which needs dynamic code enabled and so
/// cannot be a pure case, so the test checks it against real .NET itself.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
[<Category("Guest")>]
[<Explicit>]
module TestFloatWidthJoin =
    let private assy = typeof<RunResult>.Assembly

    [<Test>]
    let ``a float32 arriving at a join another arm reaches with a double is widened`` () : unit =
        let source = Assembly.getEmbeddedResourceAsString "FloatWidthJoin.cs" assy
        let image = Roslyn.compile [ source ]

        match RealRuntime.executeWithRealRuntime [||] image with
        | RealRuntimeResult.NormalExit exitCode -> exitCode |> shouldEqual 0
        | RealRuntimeResult.UnhandledException report ->
            failwith $"real runtime terminated with an unhandled exception:\n%s{report}"
        | RealRuntimeResult.Aborted (code, report) -> failwith $"real runtime aborted (%O{code}):\n%s{report}"

        let dotnetRuntimes = FrameworkUnderTest.runtimeDirs ()

        use peImage = new MemoryStream (image)

        let hostConfig =
            { HostConfig.Default dotnetRuntimes with
                Guest =
                    { GuestConfig.Default dotnetRuntimes with
                        AppContext =
                            AppContextProperties.ofMap (
                                Map.ofList
                                    [
                                        "System.Runtime.CompilerServices.RuntimeFeature.IsDynamicCodeSupported", "true"
                                    ]
                            )
                    }
            }

        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let terminalState =
            match BoundedRun.run loggerFactory "FloatWidthJoin.cs" None peImage hostConfig with
            | RunOutcome.NormalExit (state, _) -> state
            | RunOutcome.ProcessExit (state, _) -> state
            | RunOutcome.GuestUnhandledException (finalState, _, exn) ->
                failwith $"Guest threw unhandled exception:\n%s{UnhandledExceptionReport.describe finalState exn}"
            | RunOutcome.Aborted (_, _, fatal) ->
                let message = fatal.Message |> Option.defaultValue "<no message>"
                failwith $"Guest aborted (%O{fatal.Code}): %s{message}"
            | RunOutcome.SignalTerminated (_, signal) -> failwith $"Guest was terminated by POSIX signal %O{signal}"

        terminalState.LatchedExitCode |> shouldEqual 0

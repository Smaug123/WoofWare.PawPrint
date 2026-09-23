namespace WoofWare.PawPrint.Test

open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// A float32 entering a block the stack-shape analysis could not type is refused, even though
/// CoreCLR runs the guest: whether the importer widens it depends on which arms it imported,
/// which the analysis does not decide. The guest emits its IL with `Reflection.Emit`, since no
/// compiler leaves an int32 and a float32 on the stack at one join.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
[<Category("Guest")>]
[<Explicit>]
module TestFloatWidthUntypedBlock =
    let private assy = typeof<RunResult>.Assembly

    [<Test>]
    let ``a float32 entering an untyped block is refused where CoreCLR runs it`` () : unit =
        let source = Assembly.getEmbeddedResourceAsString "FloatWidthUntypedBlock.cs" assy
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

        let exn =
            Assert.Catch (fun () ->
                BoundedRun.run loggerFactory "FloatWidthUntypedBlock.cs" None peImage hostConfig
                |> ignore<RunOutcome>
            )

        exn.Message |> shouldContainText "UntypedJoin"
        exn.Message |> shouldContainText "the analysis could not type that block"

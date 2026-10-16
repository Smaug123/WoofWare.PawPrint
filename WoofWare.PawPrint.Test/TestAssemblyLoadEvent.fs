namespace WoofWare.PawPrint.Test

open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// What PawPrint does where it does not yet model CoreCLR's handling of `AppDomain.AssemblyLoad`.
/// The modelled behaviour is compared against real .NET by `sourcesPure/AssemblyLoadEventImage.cs`.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestAssemblyLoadEvent =

    let private source (directory : string) (fileName : string) : string =
        let resource = $"WoofWare.PawPrint.Test.%s{directory}.%s{fileName}"

        use stream =
            typeof<RunResult>.Assembly.GetManifestResourceStream resource
            |> Option.ofObj
            |> Option.defaultWith (fun () -> failwith $"no embedded resource %s{resource}")

        use reader = new StreamReader (stream)
        reader.ReadToEnd ()

    /// CoreCLR discards an exception escaping the handler (`EX_CATCH {}` in
    /// `RaiseLoadingAssemblyEvent`); PawPrint does not yet, and must not instead let it unwind into
    /// the guest frame beneath, whose `catch` would then see an exception CoreCLR never shows it.
    /// `AssemblyLoadHandlerThrowIsSwallowed.cs` is parked on this, and is what real .NET does.
    [<Test>]
    let ``an exception escaping an AssemblyLoad handler ends the run rather than reaching the guest`` () : unit =
        let name = "AssemblyLoadHandlerThrowIsSwallowed.cs"
        let image = Roslyn.compile [ source "sourcesPure" name ]

        let _messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", name ]

        use _loggerFactoryResource = loggerFactory

        let dotnetRuntimes = FrameworkUnderTest.runtimeDirs ()

        use peImage = new MemoryStream (image)

        let exn =
            Assert.Catch (fun () ->
                BoundedRun.run loggerFactory name (Some name) peImage (HostConfig.Default dotnetRuntimes)
                |> ExpectRun.ended
                |> ignore<RunOutcome>
            )

        exn.Message |> shouldContainText "AppDomain::RaiseLoadingAssemblyEvent"
        exn.Message |> shouldContainText "System.InvalidOperationException"

    /// CoreCLR raises the event for a dynamic assembly from inside `AppDomain_CreateDynamicAssembly`;
    /// PawPrint announces an assembly by discarding and re-running the step that loaded it, which
    /// would create a dynamic assembly twice, so with something subscribed it refuses instead. The
    /// guest is what real .NET does, and must keep exiting 0 there for the refusal to be worth
    /// replacing with it.
    [<Test>]
    let ``creating a dynamic assembly with AssemblyLoad subscribed is refused`` () : unit =
        let name = "AssemblyLoadEventDynamic.cs"
        let image = Roslyn.compile [ source "sourcesImpure" name ]

        match RealRuntime.executeWithRealRuntime [||] image with
        | RealRuntimeResult.NormalExit exitCode -> exitCode |> shouldEqual 0
        | other -> failwith $"expected the real runtime to exit normally, got %A{other}"

        let _messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", name ]

        use _loggerFactoryResource = loggerFactory

        let dotnetRuntimes = FrameworkUnderTest.runtimeDirs ()
        let defaultConfig = HostConfig.Default dotnetRuntimes

        let config =
            { defaultConfig with
                Guest =
                    { defaultConfig.Guest with
                        AppContext =
                            AppContextProperties.ofMap (
                                Map.ofList
                                    [
                                        "System.Runtime.CompilerServices.RuntimeFeature.IsDynamicCodeSupported", "true"
                                    ]
                            )
                    }
            }

        use peImage = new MemoryStream (image)

        let exn =
            Assert.Catch (fun () ->
                BoundedRun.run loggerFactory name (Some name) peImage config
                |> ExpectRun.ended
                |> ignore<RunOutcome>
            )

        exn.Message |> shouldContainText "created the dynamic assembly Evented"
        exn.Message |> shouldContainText "AppDomain_CreateDynamicAssembly"

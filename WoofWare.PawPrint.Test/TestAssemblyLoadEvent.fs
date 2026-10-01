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

    let private source (fileName : string) : string =
        let resource = $"WoofWare.PawPrint.Test.sourcesPure.%s{fileName}"

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
        let image = Roslyn.compile [ source name ]

        let _messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", name ]

        use _loggerFactoryResource = loggerFactory

        let dotnetRuntimes = FrameworkUnderTest.runtimeDirs ()

        use peImage = new MemoryStream (image)

        let exn =
            Assert.Catch (fun () ->
                BoundedRun.run loggerFactory name (Some name) peImage (HostConfig.Default dotnetRuntimes)
                |> ignore<RunOutcome>
            )

        exn.Message |> shouldContainText "AppDomain::RaiseLoadingAssemblyEvent"
        exn.Message |> shouldContainText "System.InvalidOperationException"

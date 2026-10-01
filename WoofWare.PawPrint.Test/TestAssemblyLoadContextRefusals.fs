namespace WoofWare.PawPrint.Test

open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// A guest constructing an `AssemblyLoadContext` of its own. PawPrint binds every assembly in the
/// default context and has one loader allocator, so `AssemblyNative_InitializeAssemblyLoadContext`
/// refuses both kinds of custom context rather than handing back the default binder for them;
/// `sourcesPure/AssemblyLoadContextDefault.cs` is the context it does construct.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestAssemblyLoadContextRefusals =

    let private source (isCollectible : bool) : string =
        let flag = if isCollectible then "true" else "false"

        $$"""
using System.Runtime.Loader;

public class Program
{
    public static int Main(string[] args)
    {
        AssemblyLoadContext context = new AssemblyLoadContext("custom", isCollectible: {{flag}});
        return context.IsCollectible ? 1 : 2;
    }
}
"""

    let private runRefused (name : string) (isCollectible : bool) : GuestFailureException =
        let image = Roslyn.compile [ source isCollectible ]

        let _messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", name ]

        use _loggerFactoryResource = loggerFactory
        use peImage = new MemoryStream (image)

        Assert.Throws<GuestFailureException> (fun () ->
            BoundedRun.runWith
                loggerFactory
                BoundedRun.defaultMaxSteps
                name
                (Some name)
                peImage
                (HostConfig.Default (FrameworkUnderTest.runtimeDirs ()))
            |> ExpectRun.ended
            |> ignore<RunOutcome>
        )

    [<Test>]
    let ``a custom non-collectible context is refused`` () : unit =
        let exc = runRefused "CustomAssemblyLoadContext.cs" false

        exc.Message
        |> shouldContainText "a custom AssemblyLoadContext needs a second assembly binder"

    [<Test>]
    let ``a collectible context is refused`` () : unit =
        let exc = runRefused "CollectibleAssemblyLoadContext.cs" true

        exc.Message
        |> shouldContainText "a collectible AssemblyLoadContext needs a second loader allocator"

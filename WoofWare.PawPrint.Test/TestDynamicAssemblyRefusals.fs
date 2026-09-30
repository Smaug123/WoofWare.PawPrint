namespace WoofWare.PawPrint.Test

open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// The dynamic assemblies `AppDomain_CreateDynamicAssembly` refuses to make, although real .NET makes
/// each of them (measured on .NET 10). `sourcesImpure/DynamicAssemblyHosting.cs` is the ones it does.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestDynamicAssemblyRefusals =

    /// A guest whose `Main` runs `body` inside the default context's contextual-reflection scope,
    /// which `DefineDynamicAssembly` needs to find its load context without
    /// `AssemblyNative_GetLoadContextForAssembly`.
    let private source (body : string) : string =
        $$"""
using System.Reflection;
using System.Reflection.Emit;
using System.Runtime.Loader;

public class Program
{
    public static int Main(string[] args)
    {
        using AssemblyLoadContext.ContextualReflectionScope scope = AssemblyLoadContext.Default.EnterContextualReflection();
        {{body}}
        return 0;
    }
}
"""

    let private runRefused (name : string) (body : string) : GuestFailureException =
        let image = Roslyn.compile [ source body ]

        let _messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", name ]

        use _loggerFactoryResource = loggerFactory
        use peImage = new MemoryStream (image)

        let host = HostConfig.Default (FrameworkUnderTest.runtimeDirs ())

        let host =
            { host with
                Guest =
                    { host.Guest with
                        AppContext =
                            AppContextProperties.ofMap (
                                Map.ofList
                                    [
                                        "System.Runtime.CompilerServices.RuntimeFeature.IsDynamicCodeSupported", "true"
                                    ]
                            )
                    }
            }

        Assert.Throws<GuestFailureException> (fun () ->
            BoundedRun.runWith loggerFactory BoundedRun.defaultMaxSteps name (Some name) peImage host
            |> ignore<RunOutcome>
        )

    [<Test>]
    let ``a collectible dynamic assembly is refused`` () : unit =
        let exc =
            runRefused
                "DynamicAssemblyRunAndCollect.cs"
                """AssemblyBuilder.DefineDynamicAssembly(new AssemblyName("Collectible"), AssemblyBuilderAccess.RunAndCollect);"""

        exc.Message |> shouldContainText "asks for a collectible dynamic assembly"

    [<Test>]
    let ``a dynamic assembly with a public key is refused`` () : unit =
        let exc =
            runRefused
                "DynamicAssemblyPublicKey.cs"
                """
        AssemblyName keyed = new AssemblyName("Keyed");
        keyed.SetPublicKey(typeof(object).Assembly.GetName().GetPublicKey());
        AssemblyBuilder.DefineDynamicAssembly(keyed, AssemblyBuilderAccess.Run);"""

        exc.Message |> shouldContainText "the dynamic assembly 'Keyed' has a public key"

    [<Test>]
    let ``two dynamic assemblies of one name are refused`` () : unit =
        let exc =
            runRefused
                "DynamicAssemblyTwins.cs"
                """
        AssemblyBuilder.DefineDynamicAssembly(new AssemblyName("Twin"), AssemblyBuilderAccess.Run);
        AssemblyBuilder.DefineDynamicAssembly(new AssemblyName("Twin"), AssemblyBuilderAccess.Run);"""

        exc.Message
        |> shouldContainText
            "a dynamic assembly named Twin, Version=0.0.0.0, Culture=neutral, PublicKeyToken=null would share its identity"

    [<Test>]
    let ``a dynamic assembly named like a loaded one is refused`` () : unit =
        // The guest's own assembly is loaded under the name Roslyn gives it; naming a dynamic
        // assembly after it collides with an image rather than with another dynamic assembly.
        let exc =
            runRefused
                "DynamicAssemblyShadow.cs"
                """AssemblyBuilder.DefineDynamicAssembly(typeof(Program).Assembly.GetName(), AssemblyBuilderAccess.Run);"""

        exc.Message
        |> shouldContainText "would share its identity with the assembly already loaded"

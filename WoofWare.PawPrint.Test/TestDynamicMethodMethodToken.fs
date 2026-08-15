namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint

/// The limit of the `call`-against-a-`DynamicScope` support, pinned as a test because otherwise
/// nothing runs it: a refusal is invisible to the suite, since a guest that trips one just does not
/// get written.
///
/// CoreCLR's `ResolveToken` arm for a `DynamicMethod` entry is
/// `methodHandle = dm.GetMethodDescriptor().Value` (`DynamicILGenerator.cs:798`), and
/// `GetMethodDescriptor` *mints* the callee if it is not minted — taking `lock (this)` and running
/// the guest's `GetCallableMethod`, which reaches the very QCall PawPrint implements. So resolving
/// one token can run guest managed code. Doing that means an IL op suspending for a managed call
/// and re-executing, which the interpreter cannot do yet, so a callee the guest never minted itself
/// is refused here. Measured on real .NET, the refused program answers 42.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestDynamicMethodMethodToken =

    let private assy = typeof<RunResult>.Assembly

    /// `outer` calls `inner`, and nothing ever mints `inner`: no `CreateDelegate`, no `Invoke`. The
    /// callee is deliberately a *working* method, so the only thing wrong with this program is the
    /// one thing under test.
    let private callsUnmintedCallee =
        """
using System;
using System.Reflection.Emit;

class CallsUnmintedCallee
{
    static int Main()
    {
        DynamicMethod inner = new DynamicMethod("Inner", typeof(int), new Type[] { typeof(int) }, typeof(CallsUnmintedCallee).Module);
        ILGenerator ii = inner.GetILGenerator();
        ii.Emit(OpCodes.Ldarg_0);
        ii.Emit(OpCodes.Ldc_I4_1);
        ii.Emit(OpCodes.Add);
        ii.Emit(OpCodes.Ret);

        DynamicMethod outer = new DynamicMethod("Outer", typeof(int), new Type[0], typeof(CallsUnmintedCallee).Module);
        ILGenerator il = outer.GetILGenerator();
        il.Emit(OpCodes.Ldc_I4, 41);
        il.Emit(OpCodes.Call, inner);
        il.Emit(OpCodes.Ret);

        Func<int> f = (Func<int>) outer.CreateDelegate(typeof(Func<int>));
        return f() == 42 ? 0 : 11;
    }
}
"""

    let private runToFailure (name : string) (source : string) : exn =
        let image = Roslyn.compileWithSymbols [ source ]

        let _messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", name ]

        use _loggerFactoryResource = loggerFactory

        let dotnetRuntimes =
            DotnetRuntime.SelectForDll assy.Location |> ImmutableArray.CreateRange

        use peImage = new MemoryStream (image)

        // Reflection.Emit is off by default under PawPrint, so a guest that used it would otherwise
        // die of `PlatformNotSupportedException` long before reaching the refusal under test.
        let hostConfig = HostConfig.Default dotnetRuntimes

        let hostConfig =
            { hostConfig with
                Guest =
                    { hostConfig.Guest with
                        AppContext =
                            AppContextProperties.ofMap (
                                Map.ofList
                                    [
                                        "System.Runtime.CompilerServices.RuntimeFeature.IsDynamicCodeSupported", "true"
                                    ]
                            )
                    }
            }

        Assert.Catch (fun () ->
            BoundedRun.runWith loggerFactory BoundedRun.defaultMaxSteps name (Some name) peImage hostConfig
            |> ignore<RunOutcome>
        )

    /// The refusal must name the condition, because it is the only thing standing between whoever
    /// hits it and a mystery: "your callee was never minted" is actionable ("bind a delegate to it
    /// first"), where a bare null-dereference somewhere in a field walk is not.
    [<Test>]
    let ``a call naming an unminted dynamic method is refused by name`` () : unit =
        let exn = runToFailure "CallsUnmintedCallee.cs" callsUnmintedCallee

        exn.Message |> shouldContainText "has not been minted"
        exn.Message |> shouldContainText "_methodHandle is null"
        // The scope index, so the failure identifies *which* operand of a body with several.
        exn.Message |> shouldContainText "DynamicScope entry 2"
        // And that this is a gap rather than a guest error, since real .NET runs this program.
        exn.Message |> shouldContainText "Real .NET mints it on demand"

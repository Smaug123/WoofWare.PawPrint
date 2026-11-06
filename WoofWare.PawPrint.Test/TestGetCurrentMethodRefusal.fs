namespace WoofWare.PawPrint.Test

open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `MethodBase.GetCurrentMethod()` reached through reflection. CoreCLR's walk for it does not skip
/// reflection infrastructure, so it names whichever CoreLib frame CoreCLR's invoker interposes:
/// `MethodBaseInvoker.InterpretedInvoke_Method` at first, and an `InvokeStub_*` dynamic method once
/// CoreCLR has emitted one. PawPrint's invoker interposes different frames, so it refuses the
/// question rather than name one of its own. `sourcesPure/MethodBaseGetCurrentMethod.cs` covers the
/// callers PawPrint answers.
///
/// Each guest exits 0 on real .NET only if the answer is a CoreLib method, which is what makes the
/// refusal warranted: a guest whose real answer were its own method would be one PawPrint should
/// answer.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestGetCurrentMethodRefusal =

    let private guest (call : string) : string =
        $$"""
using System;
using System.Reflection;

public static class Program
{
    public static int Main(string[] args)
    {
        MethodInfo getCurrentMethod = typeof(MethodBase).GetMethod("GetCurrentMethod");
        Func<MethodBase> direct = MethodBase.GetCurrentMethod;
        MethodBase m = {{call}};
        if (m == null) return 1;
        return m.Module.Assembly == typeof(object).Assembly ? 0 : 2;
    }
}
"""

    /// Each case: its name, and the expression that reaches `GetCurrentMethod` through reflection.
    let private cases : (string * string) list =
        [
            "MethodInfo.Invoke", "(MethodBase)getCurrentMethod.Invoke(null, null)"
            "Delegate.DynamicInvoke", "(MethodBase)direct.DynamicInvoke()"
        ]

    let private caseNames : string list = cases |> List.map fst

    [<TestCaseSource(nameof caseNames)>]
    let ``GetCurrentMethod reached through reflection is refused`` (name : string) : unit =
        let call = cases |> List.find (fun (n, _) -> n = name) |> snd
        let image = Roslyn.compile [ guest call ]

        match RealRuntime.executeWithRealRuntime [||] image with
        | RealRuntimeResult.NormalExit 0 -> ()
        | other -> failwith $"real .NET did not name a CoreLib method for %s{name}: %O{other}"

        let _messages, loggerFactory = LoggerFactory.makeTestWithProperties [ "case", name ]

        use _loggerFactoryResource = loggerFactory
        use peImage = new MemoryStream (image)

        let exn =
            Assert.Catch (fun () ->
                BoundedRun.run
                    loggerFactory
                    name
                    None
                    peImage
                    (HostConfig.Default (FrameworkUnderTest.runtimeDirsFor KernelConfig.Default.UnixPlatform))
                |> ExpectRun.ended
                |> ignore<RunOutcome>
            )

        exn.Message |> shouldContainText "MethodBase_GetCurrentMethod"
        exn.Message |> shouldContainText "reached through reflection"

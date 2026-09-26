namespace WoofWare.PawPrint.Test

open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `x.GetType()` on a null `ref T` for a value-type `T`: `ldarg.0; constrained. !!T; callvirt
/// Object::GetType()`, which ECMA-335 III.2.1 answers by boxing `*x`.
///
/// Real .NET 10 gives two answers. With the method unoptimised (`DOTNET_JITMinOpts=1`, or tier 0)
/// the box loads through the null byref and raises `NullReferenceException`; with it optimised
/// (`DOTNET_TieredCompilation=0`) the JIT folds `box T; GetType` into `T`'s type handle and returns
/// `typeof(T)` without loading anything. Which a guest sees depends on its method's tier, so there
/// is no differential oracle for this row and PawPrint refuses it; the sibling rows, which agree
/// across tiers, are `sourcesPure/ConstrainedCallvirtNullByref.cs`.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestConstrainedGetTypeNullByref =

    let private source : string =
        """
using System;
using System.Runtime.CompilerServices;

public class Program
{
    struct Payload
    {
        public int X;
    }

    static Type TypeOf<T>(ref T x)
    {
        return x.GetType();
    }

    public static int Main(string[] args)
    {
        try
        {
            TypeOf(ref Unsafe.NullRef<Payload>());
            return 1;
        }
        catch (NullReferenceException)
        {
            return 2;
        }
    }
}
"""

    [<Test>]
    let ``GetType through a null byref to a value type is refused`` () : unit =
        let image = Roslyn.compile [ source ]

        let _messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", "ConstrainedGetTypeNullByref.cs" ]

        use _loggerFactoryResource = loggerFactory
        use peImage = new MemoryStream (image)

        let exc =
            Assert.Throws<GuestFailureException> (fun () ->
                BoundedRun.runWith
                    loggerFactory
                    BoundedRun.defaultMaxSteps
                    "ConstrainedGetTypeNullByref.cs"
                    (Some "ConstrainedGetTypeNullByref.cs")
                    peImage
                    (HostConfig.Default (FrameworkUnderTest.runtimeDirs ()))
                |> ignore<RunOutcome>
            )

        exc.Message
        |> shouldContainText "refusing constrained.callvirt Object::GetType through a null byref to value type"

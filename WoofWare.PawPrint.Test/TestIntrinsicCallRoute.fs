namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint

/// Pins the half of `IlMachineStateExecution.CallRoute` that no passing guest can: an
/// unimplemented *method-level* intrinsic reached through an entry point still stops at the
/// unimplemented-intrinsic gate, rather than running its IL as an unimplemented member of a
/// type-level `[Intrinsic]` type would.
///
/// The runtime may substitute a method-level intrinsic's body on every route
/// (`getILIntrinsicImplementationForUnsafe`, jitinterface.cpp), and CoreLib's own body for such a
/// method is often a placeholder. Running it would hand the guest a catchable exception that real
/// .NET never raises, where the gate fails loudly and names the missing intrinsic.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestIntrinsicCallRoute =

    let private assy = typeof<RunResult>.Assembly

    /// `Unsafe.IsAddressGreaterThan<T>` is method-level `[Intrinsic]`, PawPrint does not implement
    /// it, and its CoreLib body is `throw new PlatformNotSupportedException()`: real .NET
    /// substitutes `ldarg.0; ldarg.1; cgt.un; ret` and this guest returns 2. If PawPrint comes to
    /// implement it, this guest needs another unimplemented method-level intrinsic whose CoreLib
    /// body is a placeholder.
    let private callsUnimplementedIntrinsicThroughCalli =
        """
using System;
using System.Runtime.CompilerServices;

unsafe class CallsUnimplementedIntrinsicThroughCalli
{
    static int Main()
    {
        int a = 0;
        int b = 0;
        delegate*<in int, in int, bool> greater = &Unsafe.IsAddressGreaterThan<int>;
        try
        {
            return greater(in a, in b) ? 1 : 2;
        }
        catch (PlatformNotSupportedException)
        {
            return 3;
        }
    }
}
"""

    [<Test>]
    let ``an unimplemented method-level intrinsic reached by calli stops at the gate`` () : unit =
        let name = "CallsUnimplementedIntrinsicThroughCalli.cs"
        let image = Roslyn.compileWithSymbols [ callsUnimplementedIntrinsicThroughCalli ]

        let _messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", name ]

        use _loggerFactoryResource = loggerFactory

        let dotnetRuntimes =
            DotnetRuntime.SelectForDll assy.Location |> ImmutableArray.CreateRange

        use peImage = new MemoryStream (image)

        let exn =
            Assert.Catch (fun () ->
                BoundedRun.runWith
                    loggerFactory
                    BoundedRun.defaultMaxSteps
                    name
                    (Some name)
                    peImage
                    (HostConfig.Default dotnetRuntimes)
                |> ignore<RunOutcome>
            )

        exn.Message |> shouldContainText "TODO: implement JIT intrinsic"
        exn.Message |> shouldContainText "Unsafe.IsAddressGreaterThan"

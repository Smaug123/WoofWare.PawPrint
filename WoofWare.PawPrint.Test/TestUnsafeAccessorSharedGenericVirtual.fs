namespace WoofWare.PawPrint.Test

open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// An `[UnsafeAccessor]` naming a value type's generic virtual method, called with type arguments
/// that CoreCLR's `ClassLoader::CanonicalizeGenericArg` shares over `System.__Canon` and with ones
/// it does not. The accessor's receiver is a `ref` to the struct, and CoreCLR strips that byref
/// from the owning type before it emits the member token (unsafeaccessors.cpp:1089), so a shared
/// instantiation finds its generic context like an unshared one, and every case runs the method on
/// the caller's struct.
///
/// Each case is run on real .NET as well, so the expectation is measured rather than asserted: a
/// case is checked against the real runtime before PawPrint is held to it.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestUnsafeAccessorSharedGenericVirtual =

    let private source (typeArgument : string) : string =
        $$"""
using System;
using System.Collections.Generic;
using System.Runtime.CompilerServices;

public interface IEcho
{
    U Echo<U>(U u);
}

public struct Shape : IEcho
{
    public int X;

    public U Echo<U>(U u)
    {
        X++;
        return u;
    }
}

public struct Wrap<T>
{
    public T V;
}

public static class Program
{
    [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "Echo")]
    private static extern U Echo<U>(ref Shape s, U u);

    public static int Main()
    {
        Shape s = default;
        Echo<{{typeArgument}}>(ref s, default);
        return s.X == 1 ? 0 : 1;
    }
}
"""

    // The first six are not shared over `System.__Canon`; the rest are, by a reference type at the
    // top or inside a value type.
    [<TestCase("long")>]
    [<TestCase("DayOfWeek")>]
    [<TestCase("int?")>]
    [<TestCase("ValueTuple<int>")>]
    [<TestCase("Wrap<Wrap<int>>")>]
    [<TestCase("KeyValuePair<int, long>")>]
    [<TestCase("string")>]
    [<TestCase("IDisposable")>]
    [<TestCase("int[]")>]
    [<TestCase("ValueTuple<string>")>]
    [<TestCase("KeyValuePair<int, string>")>]
    [<TestCase("Wrap<Wrap<string>>")>]
    [<TestCase("Wrap<int[]>")>]
    let ``every instantiation runs the method on the caller's struct`` (typeArgument : string) : unit =
        let image = Roslyn.compile [ source typeArgument ]

        match RealRuntime.executeWithRealRuntime [||] image with
        | RealRuntimeResult.NormalExit code -> code |> shouldEqual 0
        | other -> failwith $"real .NET did not exit normally on Echo<%s{typeArgument}>: %O{other}"

        let name = "SharedGenericVirtual.cs"

        let _messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", name ]

        use _loggerFactoryResource = loggerFactory

        let dotnetRuntimes = FrameworkUnderTest.runtimeDirs ()

        let run () : RunOutcome =
            use peImage = new MemoryStream (image)

            BoundedRun.runWith
                loggerFactory
                BoundedRun.defaultMaxSteps
                name
                (Some name)
                peImage
                (HostConfig.Default dotnetRuntimes)
            |> ExpectRun.ended

        match run () with
        | RunOutcome.NormalExit (state, _, _) -> state.LatchedExitCode |> shouldEqual 0
        | other -> failwith $"expected a normal exit, got %O{other}"

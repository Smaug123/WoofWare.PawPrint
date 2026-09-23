namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint

/// An `[UnsafeAccessor]` naming a value type's generic virtual method, called with a type argument
/// that CoreCLR's `ClassLoader::CanonicalizeGenericArg` shares over `System.__Canon`, kills the
/// real process with SIGSEGV. That is not an answer PawPrint can give, so it must refuse exactly
/// those instantiations and run every other one.
///
/// Each case is run on real .NET as well, so which instantiations crash is measured rather than
/// asserted: a case's expectation is checked against the real runtime before PawPrint is held to
/// it.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestUnsafeAccessorSharedGenericVirtual =

    let private assy = typeof<RunResult>.Assembly

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

    /// The exit code real .NET reports for a process killed by SIGSEGV: 128 + 11.
    [<Literal>]
    let private SigsegvExitCode = 139

    [<TestCase("long", false)>]
    [<TestCase("DayOfWeek", false)>]
    [<TestCase("int?", false)>]
    [<TestCase("ValueTuple<int>", false)>]
    [<TestCase("Wrap<Wrap<int>>", false)>]
    [<TestCase("KeyValuePair<int, long>", false)>]
    [<TestCase("string", true)>]
    [<TestCase("IDisposable", true)>]
    [<TestCase("int[]", true)>]
    [<TestCase("ValueTuple<string>", true)>]
    [<TestCase("KeyValuePair<int, string>", true)>]
    [<TestCase("Wrap<Wrap<string>>", true)>]
    [<TestCase("Wrap<int[]>", true)>]
    let ``a shared instantiation is refused and an unshared one runs`` (typeArgument : string) (shared : bool) : unit =
        let image = Roslyn.compile [ source typeArgument ]

        match RealRuntime.executeWithRealRuntime [||] image with
        | RealRuntimeResult.NormalExit code -> code |> shouldEqual (if shared then SigsegvExitCode else 0)
        | other -> failwith $"real .NET neither ran nor crashed on Echo<%s{typeArgument}>: %O{other}"

        let name = "SharedGenericVirtual.cs"

        let _messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", name ]

        use _loggerFactoryResource = loggerFactory

        let dotnetRuntimes =
            DotnetRuntime.SelectForDll assy.Location |> ImmutableArray.CreateRange

        let run () : RunOutcome =
            use peImage = new MemoryStream (image)

            BoundedRun.runWith
                loggerFactory
                BoundedRun.defaultMaxSteps
                name
                (Some name)
                peImage
                (HostConfig.Default dotnetRuntimes)

        if shared then
            let exn = Assert.Catch (fun () -> run () |> ignore<RunOutcome>)
            exn.Message |> shouldContainText "SIGSEGV"
        else
            match run () with
            | RunOutcome.NormalExit (state, _) -> state.LatchedExitCode |> shouldEqual 0
            | other -> failwith $"expected a normal exit, got %O{other}"

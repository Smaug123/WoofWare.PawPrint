namespace WoofWare.PawPrint.Test

open NUnit.Framework

/// `AppDomain.AssemblyLoad` for an assembly whose first use is an instruction that also starts a
/// type initialiser in it. CoreCLR announces an assembly before anything in it runs, so the handler
/// sees the type uninitialised and, touching it, initialises it as any first access would.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestCrossAssemblyAssemblyLoadEvent =

    let private library : CrossAssemblySpec =
        CrossAssemblySpec.library
            "AssemblyLoadEvent.Lib"
            []
            [
                """
namespace AssemblyLoadEventLib;

public static class Recorder
{
    public static bool TargetInitialised;
}

public static class Target
{
    public static int Value;

    static Target()
    {
        Value = 42;
        Recorder.TargetInitialised = true;
    }
}
"""
            ]

    let private entry (name : string) (handlerBody : string) : CrossAssemblySpec =
        CrossAssemblySpec.entryPoint
            name
            [ "AssemblyLoadEvent.Lib" ]
            [
                $$"""
using System;
using System.Runtime.CompilerServices;
using AssemblyLoadEventLib;

class Program
{
    static int s_seen = -1;

    static void OnLoad(object sender, AssemblyLoadEventArgs args)
    {
        if (args.LoadedAssembly.GetName().Name == "AssemblyLoadEvent.Lib")
        {
            {{handlerBody}}
        }
    }

    // Kept out of line so that compiling `Main` cannot bind the library early.
    [MethodImpl(MethodImplOptions.NoInlining)]
    static int Read() => Target.Value;

    static int Main(string[] argv)
    {
        AppDomain.CurrentDomain.AssemblyLoad += OnLoad;

        if (Read() != 42)
            return 100;

        return s_seen;
    }
}
"""
            ]

    /// The `ldsfld` that first needs the library also starts `Target`'s initialiser. Announced after
    /// that start, a handler reading `Target.Value` would be let through as a recursive access by
    /// the initialising thread, and read 0.
    [<Test>]
    let ``a handler reading a static of the loaded assembly sees it initialised`` () : unit =
        {
            Assemblies =
                [
                    library
                    entry "AssemblyLoadEvent.ReadsStatic" "s_seen = Target.Value == 42 ? 0 : 1 + Target.Value;"
                ]
            EntryAssemblyName = "AssemblyLoadEvent.ReadsStatic"
            ExpectedReturnCode = 0
        }
        |> CrossAssemblyHarness.runTest

    /// The handler runs before the initialiser of the type whose first use loaded the assembly, as
    /// it does on CoreCLR, where the load happens while the JIT compiles `Read`.
    [<Test>]
    let ``the handler runs before the initialiser the loading instruction triggers`` () : unit =
        {
            Assemblies =
                [
                    library
                    entry "AssemblyLoadEvent.ObservesOrder" "s_seen = Recorder.TargetInitialised ? 1 : 0;"
                ]
            EntryAssemblyName = "AssemblyLoadEvent.ObservesOrder"
            ExpectedReturnCode = 0
        }
        |> CrossAssemblyHarness.runTest

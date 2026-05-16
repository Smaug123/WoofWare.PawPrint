namespace WoofWare.PawPrint.Test

open NUnit.Framework

[<TestFixture>]
module TestCrossAssemblyIsAssignableFrom =

    [<Test>]
    let ``IsAssignableFrom open generic with cross-assembly partially substituted base`` () : unit =
        // Three assemblies, deliberately fanning out the type references the cast oracle
        // has to thread through:
        //   ArgLib  defines `Arg`              (used as the closed slot of the partial substitution)
        //   BaseLib defines `Anchor<X>` and `Base<X,Y> : Anchor<X>` (BaseLib does NOT reference ArgLib)
        //   Entry   defines `Derived<T> : Base<Arg,T>` and runs the IsAssignableFrom check.
        //
        // When the open walk starts at `Derived<>` it threads `[Arg, GP(0)]` into Base's
        // walk. The `Arg` TypeRef is interpreted in Entry's reference tables, so deep down
        // when the walk asks BaseLib to materialise `Anchor<Arg>` the substitution must
        // already have been canonicalised to a `FromDefinition` carrying ArgLib's identity;
        // otherwise BaseLib's TypeRef tables (which never name ArgLib) would fail to
        // resolve `Arg`. Regression test for that cross-assembly thread.
        {
            Assemblies =
                [
                    CrossAssemblySpec.library
                        "IsAssignableFromCrossOpenGeneric.ArgLib"
                        []
                        [
                            """
namespace IsAssignableFromCrossOpenGeneric.ArgLib;

public class Arg
{
}
"""
                        ]
                    CrossAssemblySpec.library
                        "IsAssignableFromCrossOpenGeneric.BaseLib"
                        []
                        [
                            """
namespace IsAssignableFromCrossOpenGeneric.BaseLib;

public class Anchor<X>
{
}

public class Base<X, Y> : Anchor<X>
{
}
"""
                        ]
                    CrossAssemblySpec.entryPoint
                        "IsAssignableFromCrossOpenGeneric.Entry"
                        [
                            "IsAssignableFromCrossOpenGeneric.ArgLib"
                            "IsAssignableFromCrossOpenGeneric.BaseLib"
                        ]
                        [
                            """
using IsAssignableFromCrossOpenGeneric.ArgLib;
using IsAssignableFromCrossOpenGeneric.BaseLib;

public class Derived<T> : Base<Arg, T>
{
}

class Program
{
    static int Main(string[] args)
    {
        if (!typeof(Anchor<Arg>).IsAssignableFrom(typeof(Derived<>))) return 1;
        // Negative direction: Derived's binding fixes Base's X to Arg, not some other type;
        // asking against Anchor<int> must reject the same chain.
        if (typeof(Anchor<int>).IsAssignableFrom(typeof(Derived<>))) return 2;
        return 0;
    }
}
"""
                        ]
                ]
            EntryAssemblyName = "IsAssignableFromCrossOpenGeneric.Entry"
            ExpectedReturnCode = 0
            NativeImpls = MockEnv.make ()
        }
        |> CrossAssemblyHarness.runTest

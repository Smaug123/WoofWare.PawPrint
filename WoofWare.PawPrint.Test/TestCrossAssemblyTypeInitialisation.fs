namespace WoofWare.PawPrint.Test

open NUnit.Framework

[<TestFixture>]
module TestCrossAssemblyTypeInitialisation =

    let private baseLibrary : CrossAssemblySpec =
        CrossAssemblySpec.library
            "CrossAssemblyTypeInitialisation.BaseLib"
            []
            [
                """
namespace CrossAssemblyTypeInitialisation;

public static class Recorder
{
    public static int Value;
}

public class Base
{
    public static int BaseField = 5;

    static Base()
    {
        Recorder.Value = Recorder.Value * 10 + 2;
    }
}
"""
            ]

    [<Test>]
    let ``constructing a derived type runs its cctor before a base type from another assembly`` () : unit =
        {
            Assemblies =
                [
                    baseLibrary
                    CrossAssemblySpec.entryPoint
                        "CrossAssemblyTypeInitialisation.DerivedNewobj"
                        [ "CrossAssemblyTypeInitialisation.BaseLib" ]
                        [
                            """
using CrossAssemblyTypeInitialisation;

class Derived : Base
{
    static Derived()
    {
        Recorder.Value = Recorder.Value * 10 + 1;
    }
}

class Program
{
    static int Main(string[] argv)
    {
        new Derived();
        return Recorder.Value == 12 ? 0 : Recorder.Value;
    }
}
"""
                        ]
                ]
            EntryAssemblyName = "CrossAssemblyTypeInitialisation.DerivedNewobj"
            ExpectedReturnCode = 0
            NativeImpls = MockEnv.make ()
        }
        |> CrossAssemblyHarness.runTest

    [<Test>]
    [<Ignore "Pre-existing: Ldsfld on a type whose base is in a foreign assembly fails because getTypeRef assumes the foreign assembly is already loaded">]
    let ``derived cctor reading base static triggers base cctor across assemblies`` () : unit =
        {
            Assemblies =
                [
                    baseLibrary
                    CrossAssemblySpec.entryPoint
                        "CrossAssemblyTypeInitialisation.DerivedReadsBase"
                        [ "CrossAssemblyTypeInitialisation.BaseLib" ]
                        [
                            """
using CrossAssemblyTypeInitialisation;

class Derived : Base
{
    public static int Sum;

    static Derived()
    {
        Recorder.Value = Recorder.Value * 10 + 1;
        Sum = BaseField + 3;
    }
}

class Program
{
    static int Main(string[] argv)
    {
        int sum = Derived.Sum;
        return sum == 8 && Recorder.Value == 12 ? 0 : Recorder.Value * 100 + sum;
    }
}
"""
                        ]
                ]
            EntryAssemblyName = "CrossAssemblyTypeInitialisation.DerivedReadsBase"
            ExpectedReturnCode = 0
            NativeImpls = MockEnv.make ()
        }
        |> CrossAssemblyHarness.runTest

    [<Test>]
    [<Ignore "Pre-existing: Ldsfld on a type whose base is in a foreign assembly fails because getTypeRef assumes the foreign assembly is already loaded">]
    let ``accessing derived static does not initialise base from another assembly`` () : unit =
        {
            Assemblies =
                [
                    baseLibrary
                    CrossAssemblySpec.entryPoint
                        "CrossAssemblyTypeInitialisation.DerivedStaticOnly"
                        [ "CrossAssemblyTypeInitialisation.BaseLib" ]
                        [
                            """
using CrossAssemblyTypeInitialisation;

class Derived : Base
{
    public static int Y = 7;

    static Derived()
    {
        Recorder.Value = Recorder.Value * 10 + 1;
    }
}

class Program
{
    static int Main(string[] argv)
    {
        int y = Derived.Y;
        return y == 7 && Recorder.Value == 1 ? 0 : Recorder.Value * 10 + y;
    }
}
"""
                        ]
                ]
            EntryAssemblyName = "CrossAssemblyTypeInitialisation.DerivedStaticOnly"
            ExpectedReturnCode = 0
            NativeImpls = MockEnv.make ()
        }
        |> CrossAssemblyHarness.runTest

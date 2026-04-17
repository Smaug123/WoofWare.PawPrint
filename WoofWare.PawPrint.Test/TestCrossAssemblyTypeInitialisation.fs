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

    [<Test>]
    let ``constructing a type with a generic foreign base can lazy-load the whole base chain`` () : unit =
        {
            Assemblies =
                [
                    CrossAssemblySpec.library
                        "CrossAssemblyTypeInitialisation.RootBaseLib"
                        []
                        [
                            """
namespace CrossAssemblyTypeInitialisation;

public class RootBase
{
}
"""
                        ]
                    CrossAssemblySpec.library
                        "CrossAssemblyTypeInitialisation.GenericBaseLib"
                        [ "CrossAssemblyTypeInitialisation.RootBaseLib" ]
                        [
                            """
using CrossAssemblyTypeInitialisation;

namespace CrossAssemblyTypeInitialisation.GenericBaseLib;

public class Root<T> : RootBase
{
}
"""
                        ]
                    CrossAssemblySpec.entryPoint
                        "CrossAssemblyTypeInitialisation.GenericDerivedNewobj"
                        [
                            "CrossAssemblyTypeInitialisation.GenericBaseLib"
                            "CrossAssemblyTypeInitialisation.RootBaseLib"
                        ]
                        [
                            """
using CrossAssemblyTypeInitialisation.GenericBaseLib;

class Leaf : Root<int>
{
}

class Program
{
    static int Main(string[] argv)
    {
        new Leaf();
        return 0;
    }
}
"""
                        ]
                ]
            EntryAssemblyName = "CrossAssemblyTypeInitialisation.GenericDerivedNewobj"
            ExpectedReturnCode = 0
            NativeImpls = MockEnv.make ()
        }
        |> CrossAssemblyHarness.runTest

namespace WoofWare.PawPrint.Test

open NUnit.Framework

[<TestFixture>]
module TestCrossAssemblyTypeInitialisation =

    [<Test>]
    let ``constructing a derived type runs its cctor before a base type from another assembly`` () : unit =
        {
            Assemblies =
                [
                    CrossAssemblySpec.library
                        "CrossAssemblyTypeInitialisation.BaseNewobj"
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
    static Base()
    {
        Recorder.Value = Recorder.Value * 10 + 2;
    }
}
"""
                        ]
                    CrossAssemblySpec.entryPoint
                        "CrossAssemblyTypeInitialisation.DerivedNewobj"
                        [ "CrossAssemblyTypeInitialisation.BaseNewobj" ]
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

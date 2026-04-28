namespace WoofWare.PawPrint.Test

open NUnit.Framework

[<TestFixture>]
module TestCrossAssemblyDispatch =

    [<Test>]
    let ``interface dispatch uses implementation assembly for method body metadata`` () : unit =
        {
            Assemblies =
                [
                    CrossAssemblySpec.library
                        "CrossAssemblyDispatch.Contract"
                        []
                        [
                            """
namespace CrossAssemblyDispatch;

public interface IProbe
{
    int Measure();
}

public static class ContractStrings
{
    public static string FirstUserString()
    {
        return "bad";
    }
}
"""
                        ]
                    CrossAssemblySpec.library
                        "CrossAssemblyDispatch.Implementation"
                        [ "CrossAssemblyDispatch.Contract" ]
                        [
                            """
using CrossAssemblyDispatch;

namespace CrossAssemblyDispatch.Implementation;

public sealed class Probe : IProbe
{
    public int Measure()
    {
        string value = Identity("implementation");
        return value.Length;
    }

    private static string Identity(string value)
    {
        return value;
    }
}
"""
                        ]
                    CrossAssemblySpec.entryPoint
                        "CrossAssemblyDispatch.Entry"
                        [ "CrossAssemblyDispatch.Contract" ; "CrossAssemblyDispatch.Implementation" ]
                        [
                            """
using CrossAssemblyDispatch;
using CrossAssemblyDispatch.Implementation;

class Program
{
    static int Main(string[] argv)
    {
        IProbe probe = new Probe();
        int measured = probe.Measure();
        return measured == 14 ? 0 : measured;
    }
}
"""
                        ]
                ]
            EntryAssemblyName = "CrossAssemblyDispatch.Entry"
            ExpectedReturnCode = 0
            NativeImpls = MockEnv.make ()
        }
        |> CrossAssemblyHarness.runTest

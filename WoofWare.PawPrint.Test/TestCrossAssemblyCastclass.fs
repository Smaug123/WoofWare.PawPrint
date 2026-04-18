namespace WoofWare.PawPrint.Test

open NUnit.Framework

[<TestFixture>]
module TestCrossAssemblyCastclass =

    [<Test>]
    let ``castclass to a type defined in another assembly`` () : unit =
        {
            Assemblies =
                [
                    CrossAssemblySpec.library
                        "CastclassCross.BaseLib"
                        []
                        [
                            """
namespace CastclassCross;

public class Animal
{
}

public class Dog : Animal
{
}
"""
                        ]
                    CrossAssemblySpec.entryPoint
                        "CastclassCross.Entry"
                        [ "CastclassCross.BaseLib" ]
                        [
                            """
using CastclassCross;

class Program
{
    static int Main(string[] argv)
    {
        // The castclass target type (Animal) is in CastclassCross.BaseLib,
        // not in this assembly. If the interpreter concretizes the target
        // type against the wrong assembly, this will fail.
        object o = new Dog();
        Animal a = (Animal)o;
        if (!ReferenceEquals(o, a)) return 1;
        return 0;
    }
}
"""
                        ]
                ]
            EntryAssemblyName = "CastclassCross.Entry"
            ExpectedReturnCode = 0
            NativeImpls = MockEnv.make ()
        }
        |> CrossAssemblyHarness.runTest

    [<Test>]
    let ``castclass to a generic type from another assembly`` () : unit =
        {
            Assemblies =
                [
                    CrossAssemblySpec.library
                        "CastclassCrossGeneric.BaseLib"
                        []
                        [
                            """
namespace CastclassCrossGeneric;

public class Container<T>
{
}

public class SpecialContainer : Container<int>
{
}
"""
                        ]
                    CrossAssemblySpec.entryPoint
                        "CastclassCrossGeneric.Entry"
                        [ "CastclassCrossGeneric.BaseLib" ]
                        [
                            """
using CastclassCrossGeneric;

class Program
{
    static int Main(string[] argv)
    {
        // Cast to a generic instantiation Container<int> from another assembly.
        // This exercises TypeSpec resolution in the correct assembly context.
        object o = new SpecialContainer();
        Container<int> c = (Container<int>)o;
        if (!ReferenceEquals(o, c)) return 1;
        return 0;
    }
}
"""
                        ]
                ]
            EntryAssemblyName = "CastclassCrossGeneric.Entry"
            ExpectedReturnCode = 0
            NativeImpls = MockEnv.make ()
        }
        |> CrossAssemblyHarness.runTest

    [<Test>]
    let ``castclass to an interface defined in another assembly`` () : unit =
        {
            Assemblies =
                [
                    CrossAssemblySpec.library
                        "CastclassCrossInterface.BaseLib"
                        []
                        [
                            """
namespace CastclassCrossInterface;

public interface IFlyable
{
}

public class Bird : IFlyable
{
}
"""
                        ]
                    CrossAssemblySpec.entryPoint
                        "CastclassCrossInterface.Entry"
                        [ "CastclassCrossInterface.BaseLib" ]
                        [
                            """
using CastclassCrossInterface;

class Program
{
    static int Main(string[] argv)
    {
        object o = new Bird();
        IFlyable f = (IFlyable)o;
        if (!ReferenceEquals(o, f)) return 1;
        return 0;
    }
}
"""
                        ]
                ]
            EntryAssemblyName = "CastclassCrossInterface.Entry"
            ExpectedReturnCode = 0
            NativeImpls = MockEnv.make ()
        }
        |> CrossAssemblyHarness.runTest

    [<Test>]
    let ``castclass across a three-assembly chain`` () : unit =
        // Type defined in assembly A, derived in assembly B, cast performed in assembly C.
        {
            Assemblies =
                [
                    CrossAssemblySpec.library
                        "CastclassChain.Root"
                        []
                        [
                            """
namespace CastclassChain;

public class Root
{
}
"""
                        ]
                    CrossAssemblySpec.library
                        "CastclassChain.Middle"
                        [ "CastclassChain.Root" ]
                        [
                            """
using CastclassChain;

namespace CastclassChain.Middle;

public class Middle : Root
{
}
"""
                        ]
                    CrossAssemblySpec.entryPoint
                        "CastclassChain.Entry"
                        [ "CastclassChain.Root" ; "CastclassChain.Middle" ]
                        [
                            """
using CastclassChain;
using CastclassChain.Middle;

class Program
{
    static int Main(string[] argv)
    {
        // The object is Middle (assembly B), cast target is Root (assembly A),
        // performed in assembly C.
        object o = new Middle();
        Root r = (Root)o;
        if (!ReferenceEquals(o, r)) return 1;
        return 0;
    }
}
"""
                        ]
                ]
            EntryAssemblyName = "CastclassChain.Entry"
            ExpectedReturnCode = 0
            NativeImpls = MockEnv.make ()
        }
        |> CrossAssemblyHarness.runTest

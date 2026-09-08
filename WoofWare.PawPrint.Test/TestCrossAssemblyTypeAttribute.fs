namespace WoofWare.PawPrint.Test

open NUnit.Framework

[<TestFixture>]
module TestCrossAssemblyTypeAttribute =

    /// `sourcesPure/CustomAttributeTypeArg.cs` covers `System.Type`-valued attribute arguments
    /// whose attribute is declared in the decorated assembly, so the constructor's assembly and
    /// the decorated assembly are the same and nothing can tell which one a name is resolved
    /// against. Here they differ: CoreCLR resolves against the *decorated* module's assembly
    /// (`GetTypeHandleFromBlob` is handed `pModule->GetAssembly()`), and Roslyn writes a type of
    /// the assembly being compiled into the blob unqualified, so `typeof(Decorated)` resolves only
    /// if the handler takes its scope from the `QCallModule` argument rather than from the ctor.
    [<Test>]
    let ``System.Type-valued attribute argument resolves against the decorated assembly`` () : unit =
        {
            Assemblies =
                [
                    CrossAssemblySpec.library
                        "TypeAttrCross.AttrLib"
                        []
                        [
                            """
using System;

namespace TypeAttrCross;

[AttributeUsage(AttributeTargets.Class)]
public class TargetAttribute : Attribute
{
    public TargetAttribute(Type target, int tail)
    {
        Target = target;
        Tail = tail;
    }

    public Type Target { get; }
    public int Tail { get; }
}
"""
                        ]
                    CrossAssemblySpec.entryPoint
                        "TypeAttrCross.Entry"
                        [ "TypeAttrCross.AttrLib" ]
                        [
                            """
using System;
using TypeAttrCross;

// The blob names `Decorated` unqualified: it is a type of this assembly, which is neither the
// attribute's assembly nor CoreLib, so resolving it against either of those fails. (A type of
// TypeAttrCross.AttrLib would be written assembly-qualified and bind through
// `RuntimeAssembly.InternalLoad`, which is out of reach; see CustomAttributeTypeArgForeign.cs.)
[Target(typeof(Decorated), 1)]
public class Decorated
{
}

class Program
{
    static int Main(string[] argv)
    {
        var attr = (TargetAttribute)Attribute.GetCustomAttribute(typeof(Decorated), typeof(TargetAttribute));
        if (attr == null) return 1;
        if (attr.Target != typeof(Decorated)) return 2;
        if (attr.Tail != 1) return 3;
        return 0;
    }
}
"""
                        ]
                ]
            EntryAssemblyName = "TypeAttrCross.Entry"
            ExpectedReturnCode = 0
        }
        |> CrossAssemblyHarness.runTest

namespace WoofWare.PawPrint.Test

open NUnit.Framework

/// A C# `init` accessor is emitted as `set_X` returning
/// `void modreq(System.Runtime.CompilerServices.IsExternalInit)`. Called from the assembly that
/// declares it, the call site is a MethodDef token; called from another assembly it is a MemberRef,
/// and the modifier travels in the MemberRef's own signature blob.
///
/// That is a different route into the same question, and it is the one route that exercises
/// MemberRef overload resolution on such a signature: `IlMachineMemberResolution` concretises the
/// MemberRef signature and each same-named candidate MethodDef signature, then compares them. Both
/// sides have to agree about what a modified `void` return means, which they only do if both went
/// through the one signature-concretisation function.
[<TestFixture>]
module TestCrossAssemblyInitSetter =

    [<Test>]
    let ``an init-only property may be set across an assembly boundary`` () : unit =
        {
            Assemblies =
                [
                    CrossAssemblySpec.library
                        "CrossAssemblyInit.ConfigLib"
                        []
                        [
                            """
namespace CrossAssemblyInit.ConfigLib;

public sealed class Config
{
    public int Value { get; init; }

    public string Label { get; init; }

    // An ordinary void setter alongside, so a failure is attributable to the modifier rather
    // than to property setters in general.
    public int Mutable { get; set; }
}
"""
                        ]
                    CrossAssemblySpec.entryPoint
                        "CrossAssemblyInit.ConfigEntry"
                        [ "CrossAssemblyInit.ConfigLib" ]
                        [
                            """
using CrossAssemblyInit.ConfigLib;

class Program
{
    static int Main(string[] args)
    {
        Config c = new Config
        {
            Value = 11,
            Label = "hi",
            Mutable = 5,
        };

        if (c.Value != 11)
        {
            return 1;
        }

        if (c.Label != "hi")
        {
            return 2;
        }

        if (c.Mutable != 5)
        {
            return 3;
        }

        return 7;
    }
}
"""
                        ]
                ]
            EntryAssemblyName = "CrossAssemblyInit.ConfigEntry"
            // Distinct from every early-return code above, so "the guest ran to the end" and "the
            // guest bailed out at check N" cannot be confused.
            ExpectedReturnCode = 7
        }
        |> CrossAssemblyHarness.runTest

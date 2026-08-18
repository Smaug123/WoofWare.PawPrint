namespace WoofWare.PawPrint.Test

open NUnit.Framework

/// Picking which MethodDef a MemberRef names compares two signature blobs, and they are written in
/// *different* token spaces: the reference's in the referring assembly, the candidate's in the
/// declaring one. A `FromReference` in either carries a `TypeRefResolutionScope.Assembly`, which is a
/// row index into its own assembly's `AssemblyRef` table, so reading the candidate's spellings
/// against the referrer resolves a different row.
///
/// Observing that needs the overload set to be distinguished by a *nominal* type, and one declared in
/// a third assembly: a parameter type declared alongside the method decodes as a self-describing
/// `FromDefinition`, whose identity needs no scope at all, so the token space is never consulted.
///
/// `DecoyLib` exists only to push `PayloadLib` onto a different `AssemblyRef` row in `ApiLib` than it
/// occupies in the entry assembly. Without it the two tables coincide and a mis-scoped resolution
/// succeeds by accident; `runTestRequiring` asserts the divergence rather than assuming it.
[<TestFixture>]
module TestCrossAssemblyOverloadTokenSpace =

    let private payloadLibrary : CrossAssemblySpec =
        CrossAssemblySpec.library
            "CrossAssemblyOverload.PayloadLib"
            []
            [
                """
namespace CrossAssemblyOverload;

public sealed class Payload
{
}

public sealed class Other
{
}
"""
            ]

    let private decoyLibrary : CrossAssemblySpec =
        CrossAssemblySpec.library
            "CrossAssemblyOverload.DecoyLib"
            []
            [
                """
namespace CrossAssemblyOverload;

public class Decoy
{
    public int Nudge (int x) => x + 1;
}
"""
            ]

    let private apiLibrary : CrossAssemblySpec =
        CrossAssemblySpec.library
            "CrossAssemblyOverload.ApiLib"
            [ "CrossAssemblyOverload.PayloadLib" ; "CrossAssemblyOverload.DecoyLib" ]
            [
                """
namespace CrossAssemblyOverload;

// Declared before `Sink` so that `DecoyLib` claims the first AssemblyRef row, displacing
// `PayloadLib` to a row that denotes a different assembly in the entry assembly's table.
public class UsesDecoy : Decoy
{
}

public static class Sink
{
    // Two overloads separated only by which foreign type they take, so choosing between them
    // requires each candidate's parameter reference to be resolved in this assembly's token space.
    public static int Take (Payload p) => 1;

    public static int Take (Other o) => 2;
}
"""
            ]

    let private payloadRowsMustDiverge : AssemblyRefRowDivergence list =
        [
            {
                DeclaringAssembly = "CrossAssemblyOverload.ApiLib"
                ExecutingAssembly = "CrossAssemblyOverload.OverloadEntry"
                ForeignAssembly = "CrossAssemblyOverload.PayloadLib"
            }
        ]

    [<Test>]
    let ``a MemberRef overload is chosen by resolving each candidate in its own assembly`` () : unit =
        {
            Assemblies =
                [
                    decoyLibrary
                    payloadLibrary
                    apiLibrary
                    CrossAssemblySpec.entryPoint
                        "CrossAssemblyOverload.OverloadEntry"
                        [ "CrossAssemblyOverload.ApiLib" ; "CrossAssemblyOverload.PayloadLib" ]
                        [
                            """
using CrossAssemblyOverload;

class Program
{
    static int Main(string[] args)
    {
        if (Sink.Take(new Payload()) != 1)
        {
            return 1;
        }

        if (Sink.Take(new Other()) != 2)
        {
            return 2;
        }

        return 7;
    }
}
"""
                        ]
                ]
            EntryAssemblyName = "CrossAssemblyOverload.OverloadEntry"
            // Distinct from every early-return code above, so "the guest ran to the end" and "the
            // guest bailed out at check N" cannot be confused.
            ExpectedReturnCode = 7
        }
        |> CrossAssemblyHarness.runTestRequiring payloadRowsMustDiverge

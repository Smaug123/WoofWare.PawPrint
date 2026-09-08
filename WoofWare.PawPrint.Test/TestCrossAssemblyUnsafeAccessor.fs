namespace WoofWare.PawPrint.Test

open NUnit.Framework

/// `[UnsafeAccessor]` reaching members of *another* assembly, which is what the attribute exists
/// for: the target is inaccessible to the accessor's own assembly, so no ordinary call or field
/// token could name it, and the accessor's resolution is the only thing that binds the two.
///
/// Three assemblies rather than two, following `TestCrossAssemblyLdsflda`. A member's signature is
/// decoded against the *declaring* assembly's `AssemblyRef` table, and comparing it against the
/// accessor's declaration means reading two signatures in two different token spaces. Putting the
/// member's own type in a third assembly is what makes the two spellings genuinely different
/// (`TypeDefn.FromReference` on the declaring side, a different `TypeRef` on the accessor's side)
/// rather than both being self-describing `FromDefinition`s that would compare equal by accident.
[<TestFixture>]
module TestCrossAssemblyUnsafeAccessor =

    let private payloadLibrary : CrossAssemblySpec =
        CrossAssemblySpec.library
            "CrossAssemblyUnsafeAccessor.PayloadLib"
            []
            [
                """
namespace CrossAssemblyUnsafeAccessor;

public struct Payload
{
    public int Value;
}
"""
            ]

    /// Referenced by `SecretLib` but not by the entry assembly, purely so that `PayloadLib` lands
    /// on a different `AssemblyRef` row in each — see `AssemblyRefRowDivergence`. Declared ahead of
    /// `Secret` because Roslyn emits `AssemblyRef` rows in order of first use.
    let private decoyLibrary : CrossAssemblySpec =
        CrossAssemblySpec.library
            "CrossAssemblyUnsafeAccessor.DecoyLib"
            []
            [
                """
namespace CrossAssemblyUnsafeAccessor;

public class Decoy
{
    public int Nudge(int x) => x + 1;
}
"""
            ]

    let private secretLibrary : CrossAssemblySpec =
        CrossAssemblySpec.library
            "CrossAssemblyUnsafeAccessor.SecretLib"
            [
                "CrossAssemblyUnsafeAccessor.PayloadLib"
                "CrossAssemblyUnsafeAccessor.DecoyLib"
            ]
            [
                """
namespace CrossAssemblyUnsafeAccessor;

// Declared first so that `DecoyLib` takes the first `AssemblyRef` row.
public class UsesDecoy : Decoy
{
}

public class Secret
{
    private Payload _payload;

    private static Payload _sharedPayload;

    private Secret(int seed)
    {
        _payload.Value = seed;
    }

    private int Read() => _payload.Value;

    private static int ReadShared() => _sharedPayload.Value;

    // An overload set, so that the accessor's signature has to pick one rather than matching the
    // only candidate by name alone.
    private int Overloaded(int x) => x + 1;

    private int Overloaded(Payload p) => p.Value + 2;
}
"""
            ]

    let private payloadRowsMustDiverge (entryAssembly : string) : AssemblyRefRowDivergence list =
        [
            {
                DeclaringAssembly = "CrossAssemblyUnsafeAccessor.SecretLib"
                ExecutingAssembly = entryAssembly
                ForeignAssembly = "CrossAssemblyUnsafeAccessor.PayloadLib"
            }
        ]

    [<Test>]
    let ``an accessor reaches a foreign assembly's private members`` () : unit =
        {
            Assemblies =
                [
                    decoyLibrary
                    payloadLibrary
                    secretLibrary
                    CrossAssemblySpec.entryPoint
                        "CrossAssemblyUnsafeAccessor.Entry"
                        [
                            "CrossAssemblyUnsafeAccessor.SecretLib"
                            "CrossAssemblyUnsafeAccessor.PayloadLib"
                        ]
                        [
                            """
using System.Runtime.CompilerServices;
using CrossAssemblyUnsafeAccessor;

class Program
{
    // Mentioned before anything from `SecretLib` so that `PayloadLib` takes an earlier
    // `AssemblyRef` row here than it does there; the harness checks that the rows really diverge.
    private static Payload _unused;

    [UnsafeAccessor(UnsafeAccessorKind.Constructor)]
    private static extern Secret NewSecret(int seed);

    [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "Read")]
    private static extern int Read(Secret s);

    [UnsafeAccessor(UnsafeAccessorKind.StaticMethod, Name = "ReadShared")]
    private static extern int ReadShared(Secret s);

    [UnsafeAccessor(UnsafeAccessorKind.Field, Name = "_payload")]
    private static extern ref Payload PayloadOf(Secret s);

    [UnsafeAccessor(UnsafeAccessorKind.StaticField, Name = "_sharedPayload")]
    private static extern ref Payload SharedPayload(Secret s);

    [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "Overloaded")]
    private static extern int OverloadedInt(Secret s, int x);

    [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "Overloaded")]
    private static extern int OverloadedPayload(Secret s, Payload p);

    static int Main(string[] argv)
    {
        Secret s = NewSecret(11);
        if (s == null) { return 1; }
        if (Read(s) != 11) { return 2; }

        // The field's own type lives in a third assembly, so the byref addresses a foreign struct.
        if (PayloadOf(s).Value != 11) { return 3; }
        PayloadOf(s).Value = 12;
        if (Read(s) != 12) { return 4; }

        if (ReadShared(s) != 0) { return 5; }
        SharedPayload(s).Value = 33;
        if (ReadShared(s) != 33) { return 6; }

        // The two accessors differ only in the parameter type, and each must pick its own
        // overload: a comparison blind to the difference would find both and be ambiguous.
        if (OverloadedInt(s, 5) != 6) { return 7; }

        Payload p = default;
        p.Value = 5;
        if (OverloadedPayload(s, p) != 7) { return 8; }

        return 0;
    }
}
"""
                        ]
                ]
            EntryAssemblyName = "CrossAssemblyUnsafeAccessor.Entry"
            ExpectedReturnCode = 0
        }
        |> CrossAssemblyHarness.runTestRequiring (payloadRowsMustDiverge "CrossAssemblyUnsafeAccessor.Entry")

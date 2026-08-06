namespace WoofWare.PawPrint.Test

open NUnit.Framework

/// `ldsflda` against a static field declared in *another* assembly (issue #723).
///
/// Two distinct things are under test here, and they need three assemblies to
/// separate them:
///
/// * the token is a `MemberReference` rather than a `FieldDefinition`, which
///   `executeLdsflda` used to reject outright;
/// * the field's *own type* is expressed in the declaring assembly's metadata as
///   a `TypeDefn.FromReference`, i.e. a `TypeRefResolutionScope.Assembly` row
///   index into the *declaring* assembly's `AssemblyRef` table. Concretizing that
///   signature against the executing assembly instead resolves the wrong row, so
///   `PayloadLib` has to be a third assembly: were `Payload` declared alongside
///   the field, its signature would decode as a self-describing `FromDefinition`
///   and the scope would never be consulted.
[<TestFixture>]
module TestCrossAssemblyLdsflda =

    let private payloadLibrary : CrossAssemblySpec =
        CrossAssemblySpec.library
            "CrossAssemblyLdsflda.PayloadLib"
            []
            [
                """
namespace CrossAssemblyLdsflda;

public struct Payload
{
    public int Value;
}
"""
            ]

    /// Referenced by `HolderLib` but *not* by the entry assembly, purely so that
    /// `PayloadLib` occupies a different `AssemblyRef` row in `HolderLib` than it
    /// does in the entry assembly. Without that divergence both tables happen to
    /// put `PayloadLib` in the same row, and concretizing the field signature
    /// against the wrong assembly accidentally still resolves — verified by
    /// experiment, not assumed.
    ///
    /// Roslyn emits `AssemblyRef` rows in order of first use in the metadata, so
    /// `UsesDecoy` is declared ahead of `Holder` in `HolderLib` to get `DecoyLib`
    /// in first. That is the only lever we have over row numbering, and it is a
    /// Roslyn implementation detail: if it ever changes, these tests keep passing
    /// but stop covering the scope hazard (they can only fail to catch a
    /// regression, never spuriously fail).
    let private decoyLibrary : CrossAssemblySpec =
        CrossAssemblySpec.library
            "CrossAssemblyLdsflda.DecoyLib"
            []
            [
                """
namespace CrossAssemblyLdsflda;

public class Decoy
{
    public int Nudge(int x) => x + 1;
}
"""
            ]

    let private holderLibrary : CrossAssemblySpec =
        CrossAssemblySpec.library
            "CrossAssemblyLdsflda.HolderLib"
            [ "CrossAssemblyLdsflda.PayloadLib" ; "CrossAssemblyLdsflda.DecoyLib" ]
            [
                """
namespace CrossAssemblyLdsflda;

// Declared before `Holder` so that `DecoyLib` is the first assembly reference
// emitted into this assembly's `AssemblyRef` table, pushing `PayloadLib` to a
// row that means something else in the entry assembly's table.
public class UsesDecoy : Decoy
{
}

public static class Holder
{
    public static Payload Slot;

    public static int Counter;
}

public static class GenericHolder<T>
{
    public static Payload Slot;
}
"""
            ]

    /// `PayloadLib` must sit on different `AssemblyRef` rows in `HolderLib` (which scopes the field
    /// signature) and in the entry assembly (which executes the token); otherwise resolving the
    /// signature against the wrong assembly would accidentally still succeed and these tests would
    /// prove nothing. Checked rather than assumed — see `AssemblyRefRowDivergence`.
    let private payloadRowsMustDiverge (entryAssembly : string) : AssemblyRefRowDivergence list =
        [
            {
                DeclaringAssembly = "CrossAssemblyLdsflda.HolderLib"
                ExecutingAssembly = entryAssembly
                ForeignAssembly = "CrossAssemblyLdsflda.PayloadLib"
            }
        ]

    [<Test>]
    let ``ldsflda of a foreign static field whose type is itself foreign`` () : unit =
        {
            Assemblies =
                [
                    decoyLibrary
                    payloadLibrary
                    holderLibrary
                    CrossAssemblySpec.entryPoint
                        "CrossAssemblyLdsflda.ForeignFieldType"
                        [ "CrossAssemblyLdsflda.HolderLib" ; "CrossAssemblyLdsflda.PayloadLib" ]
                        [
                            """
using CrossAssemblyLdsflda;

class Program
{
    static int Main(string[] argv)
    {
        ref Payload slot = ref Holder.Slot;
        if (slot.Value != 0)
        {
            return 1;
        }

        slot.Value = 42;
        if (Holder.Slot.Value != 42)
        {
            return 2;
        }

        return 0;
    }
}
"""
                        ]
                ]
            EntryAssemblyName = "CrossAssemblyLdsflda.ForeignFieldType"
            ExpectedReturnCode = 0
        }
        |> CrossAssemblyHarness.runTestRequiring (payloadRowsMustDiverge "CrossAssemblyLdsflda.ForeignFieldType")

    [<Test>]
    let ``ldsflda of a primitive foreign static field`` () : unit =
        {
            Assemblies =
                [
                    decoyLibrary
                    payloadLibrary
                    holderLibrary
                    CrossAssemblySpec.entryPoint
                        "CrossAssemblyLdsflda.PrimitiveFieldType"
                        [ "CrossAssemblyLdsflda.HolderLib" ; "CrossAssemblyLdsflda.PayloadLib" ]
                        [
                            """
using System.Threading;
using CrossAssemblyLdsflda;

class Program
{
    static int Main(string[] argv)
    {
        Interlocked.Increment(ref Holder.Counter);
        Interlocked.Increment(ref Holder.Counter);
        return Holder.Counter == 2 ? 0 : 3;
    }
}
"""
                        ]
                ]
            EntryAssemblyName = "CrossAssemblyLdsflda.PrimitiveFieldType"
            ExpectedReturnCode = 0
        }
        |> CrossAssemblyHarness.runTest

    /// A `MemberReference` whose parent is a `TypeSpecification` *and* whose
    /// declaring type lives in another assembly: the generic-instantiation and
    /// cross-assembly cases composed.
    [<Test>]
    let ``ldsflda of a foreign generic type's static field`` () : unit =
        {
            Assemblies =
                [
                    decoyLibrary
                    payloadLibrary
                    holderLibrary
                    CrossAssemblySpec.entryPoint
                        "CrossAssemblyLdsflda.ForeignGenericStatic"
                        [ "CrossAssemblyLdsflda.HolderLib" ; "CrossAssemblyLdsflda.PayloadLib" ]
                        [
                            """
using CrossAssemblyLdsflda;

class Program
{
    static int Main(string[] argv)
    {
        ref Payload ints = ref GenericHolder<int>.Slot;
        ints.Value = 11;

        ref Payload strings = ref GenericHolder<string>.Slot;
        strings.Value = 13;

        if (GenericHolder<int>.Slot.Value != 11)
        {
            return 4;
        }

        if (GenericHolder<string>.Slot.Value != 13)
        {
            return 5;
        }

        return 0;
    }
}
"""
                        ]
                ]
            EntryAssemblyName = "CrossAssemblyLdsflda.ForeignGenericStatic"
            ExpectedReturnCode = 0
        }
        |> CrossAssemblyHarness.runTestRequiring (payloadRowsMustDiverge "CrossAssemblyLdsflda.ForeignGenericStatic")

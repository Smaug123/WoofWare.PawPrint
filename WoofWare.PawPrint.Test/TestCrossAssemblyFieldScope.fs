namespace WoofWare.PawPrint.Test

open NUnit.Framework

/// Issue #737: `ldsfld`, `stsfld` and `stfld` passed the *executing* assembly to `cliTypeZeroOf`
/// where the field's *declaring* assembly is required.
///
/// A field's `Signature` is decoded from its declaring assembly's metadata, so a `TypeDefn` in it
/// that is a `FromReference` carries a `TypeRefResolutionScope.Assembly` — a row index into *that*
/// assembly's `AssemblyRef` table. Resolving it against the executing assembly reads a different
/// row. This needs three assemblies to observe:
///
/// * `PayloadLib` declares the field's type. It has to be separate from the holder: a field whose
///   type is declared alongside it decodes as a self-describing `FromDefinition`, and the
///   resolution scope is then never consulted at all.
/// * `HolderLib` declares the fields, so its metadata is what scopes their signatures.
/// * the entry assembly executes the field tokens.
///
/// `DecoyLib` exists only to push `PayloadLib` onto a different `AssemblyRef` row in `HolderLib`
/// than it occupies in the entry assembly — without that the two tables coincide and a mis-scoped
/// resolution accidentally succeeds. That requirement is asserted by `runTestRequiring` rather than
/// assumed; see `AssemblyRefRowDivergence`.
[<TestFixture>]
module TestCrossAssemblyFieldScope =

    let private payloadLibrary : CrossAssemblySpec =
        CrossAssemblySpec.library
            "CrossAssemblyFieldScope.PayloadLib"
            []
            [
                """
namespace CrossAssemblyFieldScope;

public struct Payload
{
    public int Value;
}
"""
            ]

    let private decoyLibrary : CrossAssemblySpec =
        CrossAssemblySpec.library
            "CrossAssemblyFieldScope.DecoyLib"
            []
            [
                """
namespace CrossAssemblyFieldScope;

public class Decoy
{
    public int Nudge(int x) => x + 1;
}
"""
            ]

    let private holderLibrary : CrossAssemblySpec =
        CrossAssemblySpec.library
            "CrossAssemblyFieldScope.HolderLib"
            [ "CrossAssemblyFieldScope.PayloadLib" ; "CrossAssemblyFieldScope.DecoyLib" ]
            [
                """
namespace CrossAssemblyFieldScope;

// Declared before `Holder` so that `DecoyLib` claims the first AssemblyRef row, displacing
// `PayloadLib` to a row that denotes a different assembly in the entry assembly's table.
public class UsesDecoy : Decoy
{
}

public static class Holder
{
    // Deliberately no initialiser: an initialiser would make Roslyn emit a `.cctor`, which would
    // populate the static before the guest reads it and send `ldsfld` down its `Some` branch,
    // skipping the `cliTypeZeroOf` call that is the subject of this test.
    public static Payload Slot;

    public static Payload Other;
}

public class Instance
{
    public Payload Field;
}
"""
            ]

    let private payloadRowsMustDiverge (entryAssembly : string) : AssemblyRefRowDivergence list =
        [
            {
                DeclaringAssembly = "CrossAssemblyFieldScope.HolderLib"
                ExecutingAssembly = entryAssembly
                ForeignAssembly = "CrossAssemblyFieldScope.PayloadLib"
            }
        ]

    /// `ldsfld` reaches `cliTypeZeroOf` only on the first read of an as-yet-unwritten static, since
    /// PawPrint zero-fills statics lazily in that branch rather than eagerly at class load.
    [<Test>]
    let ``ldsfld of a foreign static field whose type is itself foreign`` () : unit =
        {
            Assemblies =
                [
                    decoyLibrary
                    payloadLibrary
                    holderLibrary
                    CrossAssemblySpec.entryPoint
                        "CrossAssemblyFieldScope.LdsfldEntry"
                        [ "CrossAssemblyFieldScope.HolderLib" ; "CrossAssemblyFieldScope.PayloadLib" ]
                        [
                            """
using CrossAssemblyFieldScope;

class Program
{
    static int Main(string[] argv)
    {
        Payload read = Holder.Slot;
        return read.Value == 0 ? 0 : 1;
    }
}
"""
                        ]
                ]
            EntryAssemblyName = "CrossAssemblyFieldScope.LdsfldEntry"
            ExpectedReturnCode = 0
        }
        |> CrossAssemblyHarness.runTestRequiring (payloadRowsMustDiverge "CrossAssemblyFieldScope.LdsfldEntry")

    [<Test>]
    let ``stsfld to a foreign static field whose type is itself foreign`` () : unit =
        {
            Assemblies =
                [
                    decoyLibrary
                    payloadLibrary
                    holderLibrary
                    CrossAssemblySpec.entryPoint
                        "CrossAssemblyFieldScope.StsfldEntry"
                        [ "CrossAssemblyFieldScope.HolderLib" ; "CrossAssemblyFieldScope.PayloadLib" ]
                        [
                            """
using CrossAssemblyFieldScope;

class Program
{
    static int Main(string[] argv)
    {
        Payload p = new Payload();
        p.Value = 17;
        Holder.Other = p;
        return Holder.Other.Value == 17 ? 0 : 2;
    }
}
"""
                        ]
                ]
            EntryAssemblyName = "CrossAssemblyFieldScope.StsfldEntry"
            ExpectedReturnCode = 0
        }
        |> CrossAssemblyHarness.runTestRequiring (payloadRowsMustDiverge "CrossAssemblyFieldScope.StsfldEntry")

    [<Test>]
    let ``stfld to a foreign instance field whose type is itself foreign`` () : unit =
        {
            Assemblies =
                [
                    decoyLibrary
                    payloadLibrary
                    holderLibrary
                    CrossAssemblySpec.entryPoint
                        "CrossAssemblyFieldScope.StfldEntry"
                        [ "CrossAssemblyFieldScope.HolderLib" ; "CrossAssemblyFieldScope.PayloadLib" ]
                        [
                            """
using CrossAssemblyFieldScope;

class Program
{
    static int Main(string[] argv)
    {
        // `Payload` is mentioned before `Instance` so that PayloadLib claims an earlier
        // AssemblyRef row here than it has in HolderLib. Reverse these two and the rows coincide,
        // at which case the test silently stops discriminating — `runTestRequiring` asserts it.
        Payload p = new Payload();
        p.Value = 23;
        Instance instance = new Instance();
        instance.Field = p;
        return instance.Field.Value == 23 ? 0 : 3;
    }
}
"""
                        ]
                ]
            EntryAssemblyName = "CrossAssemblyFieldScope.StfldEntry"
            ExpectedReturnCode = 0
        }
        |> CrossAssemblyHarness.runTestRequiring (payloadRowsMustDiverge "CrossAssemblyFieldScope.StfldEntry")

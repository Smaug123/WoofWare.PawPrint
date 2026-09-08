using System;
using System.Reflection;

// `FieldInfo.MetadataToken`, which `RtFieldInfo` answers with the
// `RuntimeFieldHandle::GetToken(IntPtr)` InternalCall: CoreCLR returns
// `pField->GetMemberDef()` (runtimehandles.cpp:2205), the field's `mdFieldDef` row token.
//
// Every check here is a relation between tokens rather than a literal value, so the guest
// says the same thing on both runtimes without depending on which rows Roslyn happened to
// emit. Checks 7 and 8 do depend on Roslyn emitting a type's FieldDef rows in declaration
// order, and say why that is worth the dependency.
//
// Returns 0 on success, or the number of the first check that failed.
[AttributeUsage (AttributeTargets.Field)]
public sealed class MarkerAttribute : Attribute
{
    public MarkerAttribute (string which) => Which = which;

    public string Which { get; }
}

public struct Point
{
    public int X;
    public int Y;
}

public class Gen<T>
{
    public T Value;
    public int Count;
}

public class Holder
{
    [Marker ("instance")]
    public int Instance;
    private string Secret;

    [Marker ("static")]
    public static long Static;

    // Declared immediately after `Static`, and a literal, so its own token comes from
    // metadata rather than from the InternalCall under test. Check 8 leans on both facts.
    public const int Constant = 7;

    public Holder ()
    {
        Secret = "s";
    }

    public string Read () => Secret;
}

public class Program
{
    const int FieldDefTag = 0x04000000;

    static bool IsFieldDef (int token) => (token & unchecked ((int) 0xff000000)) == FieldDefTag;

    public static int Main (string[] args)
    {
        // 1: the tag byte is `mdtFieldDef`. A token carrying any other table's tag would be
        // resolved against the wrong table by anyone who took it at its word.
        FieldInfo instance = typeof (Holder).GetField ("Instance");
        FieldInfo secret = typeof (Holder).GetField ("Secret", BindingFlags.NonPublic | BindingFlags.Instance);
        FieldInfo stat = typeof (Holder).GetField ("Static");
        FieldInfo constant = typeof (Holder).GetField ("Constant");

        foreach (FieldInfo f in new[] { instance, secret, stat, constant })
        {
            if (!IsFieldDef (f.MetadataToken))
                return 1;
        }

        // 2: distinct fields of one type are distinct rows, including across the
        // instance/static/literal split, which is where a handler that answered from the
        // declaring type rather than the field would collapse them all together.
        int[] tokens = { instance.MetadataToken, secret.MetadataToken, stat.MetadataToken, constant.MetadataToken };

        for (int i = 0; i < tokens.Length; i++)
        {
            for (int j = i + 1; j < tokens.Length; j++)
            {
                if (tokens[i] == tokens[j])
                    return 2;
            }
        }

        // 3: the token is a property of the FieldDef row alone, so every instantiation of a
        // generic type reports the *same* token for the same field -- including the open
        // definition. CoreCLR gets this from FieldDescs living on the canonical MethodTable;
        // a runtime that keyed the answer on its own per-instantiation field handle would
        // hand back three different numbers here. Measured on real .NET: all 0x04000003.
        int openValue = typeof (Gen<>).GetField ("Value").MetadataToken;
        int intValue = typeof (Gen<int>).GetField ("Value").MetadataToken;
        int stringValue = typeof (Gen<string>).GetField ("Value").MetadataToken;

        if (openValue != intValue || intValue != stringValue)
            return 3;

        if (!IsFieldDef (openValue))
            return 3;

        // 4: ... but two fields of the *same* instantiation are still two rows.
        if (typeof (Gen<int>).GetField ("Count").MetadataToken == intValue)
            return 4;

        // 5: the same field reached twice, and through two separately-obtained FieldInfo
        // objects, reports one token.
        if (instance.MetadataToken != instance.MetadataToken)
            return 5;

        if (typeof (Holder).GetField ("Instance").MetadataToken != instance.MetadataToken)
            return 5;

        // 6: a corelib field answers too, so the token is not something only the guest's own
        // assembly can produce.
        if (!IsFieldDef (typeof (string).GetField ("Empty").MetadataToken))
            return 6;

        if (typeof (int).GetField ("MaxValue").MetadataToken == typeof (int).GetField ("MinValue").MetadataToken)
            return 6;

        // 7: the first of the two checks that depend on how the image was laid out. Everything
        // above would still pass if the token were any injective function of the field rather
        // than its metadata row; this rules out the ones that do not preserve the *spacing* of
        // rows, such as a scaled or hashed row number. Roslyn emits a type's FieldDef rows in
        // declaration order and does not interleave two types' fields, so `Point`'s two fields
        // occupy consecutive rows. If a future Roslyn stops doing that this check fails on both
        // runtimes at once, which is a visible break rather than a silent one. Note that both
        // tokens here came from the native, so a *uniform* shift survives this: check 8 is what
        // catches that.
        int x = typeof (Point).GetField ("X").MetadataToken;
        int y = typeof (Point).GetField ("Y").MetadataToken;

        if (y != x + 1)
            return 7;

        // 8: the absolute anchor, with no image constant in it. A literal field has no
        // FieldDesc, so CoreCLR gives it an `MdFieldInfo`, whose `MetadataToken` is the token it
        // read straight out of metadata (MdFieldInfo.cs:44) and which therefore never reaches
        // this InternalCall at all. `Constant` is declared immediately after `Static`, so the
        // native's answer for one and the metadata's own answer for the other must be adjacent
        // rows. Everything above this line would still hold if every token were uniformly
        // shifted; this is what pins the native to the real row number.
        if (constant.MetadataToken != stat.MetadataToken + 1)
            return 8;

        // 9: the consumer that reaches this InternalCall in the first place. `CustomAttribute`
        // looks the field's token up in the CustomAttribute table by `Parent`, so a token
        // naming a neighbouring row does not fail -- it quietly returns some *other* member's
        // attributes. Two adjacent fields therefore carry differently-argumented attributes,
        // and a third carries none, so both directions of a wrong row are visible here.
        object[] onInstance = instance.GetCustomAttributes (typeof (MarkerAttribute), false);

        if (onInstance.Length != 1 || ((MarkerAttribute) onInstance[0]).Which != "instance")
            return 9;

        object[] onStatic = stat.GetCustomAttributes (typeof (MarkerAttribute), false);

        if (onStatic.Length != 1 || ((MarkerAttribute) onStatic[0]).Which != "static")
            return 10;

        if (secret.GetCustomAttributes (typeof (MarkerAttribute), false).Length != 0)
            return 11;

        return 0;
    }
}

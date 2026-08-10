using System;
using System.Reflection;

// `Type.GetNestedTypes` reaches `MetadataImport.EnumNestedTypes`, which is the `mdtTypeDef` case of
// the `MetadataImport_Enum` QCall. CoreCLR answers that by enumerating the NestedClass table for
// the parent, so this covers the whole chain: enumeration, token-to-type resolution, and the
// binding-flags filtering the BCL layers on top.
public class NestedOuter
{
    public class PublicFirst
    {
    }

    private class PrivateSecond
    {
    }

    public class PublicThirdWithOwnNested
    {
        public class Deeper
        {
        }
    }
}

public class NestedNone
{
}

public class NestedTypeEnumeration
{
    public static int Main(string[] argv)
    {
        // The default overload is public-only.
        Type[] publicNested = typeof(NestedOuter).GetNestedTypes();
        if (publicNested.Length != 2) return 1;
        if (publicNested[0].Name != "PublicFirst") return 2;
        if (publicNested[1].Name != "PublicThirdWithOwnNested") return 3;

        // NonPublic reaches the private one, proving the enumeration itself is unfiltered and only
        // the managed layer applies visibility. Order still follows the metadata.
        Type[] allNested = typeof(NestedOuter).GetNestedTypes(BindingFlags.Public | BindingFlags.NonPublic);
        if (allNested.Length != 3) return 4;
        if (allNested[0].Name != "PublicFirst") return 5;
        if (allNested[1].Name != "PrivateSecond") return 6;
        if (allNested[2].Name != "PublicThirdWithOwnNested") return 7;

        // Nesting is not transitive: `Deeper` is nested in PublicThirdWithOwnNested, not in
        // NestedOuter. A transitive walk would have made the counts above 3 and 4.
        Type[] deeper = typeof(NestedOuter.PublicThirdWithOwnNested).GetNestedTypes();
        if (deeper.Length != 1) return 8;
        if (deeper[0].Name != "Deeper") return 9;

        if (typeof(NestedNone).GetNestedTypes().Length != 0) return 10;

        // A nested type's DeclaringType is the enclosing one, so the two directions agree.
        if (publicNested[0].DeclaringType != typeof(NestedOuter)) return 11;

        return 0;
    }
}

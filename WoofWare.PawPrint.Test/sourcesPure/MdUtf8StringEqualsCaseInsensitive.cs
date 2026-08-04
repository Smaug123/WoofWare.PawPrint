using System.Reflection;

// `BindingFlags.IgnoreCase` member lookup is filtered by
// `RuntimeType.RuntimeTypeCache.Filter.Match`, which for a case-insensitive list
// type calls `MdUtf8String.EqualsCaseInsensitive` -- the managed wrapper around
// CoreCLR's `MdUtf8String_EqualsCaseInsensitive` QCall. `GetField` does no
// further name comparison of its own (there is no prefix lookup on the singular
// getters), so this test observes the QCall's semantics directly.
//
// Note that `RuntimeType.FilterHelper` lowercases the *requested* name with
// `ToLowerInvariant` before handing it to the QCall, so a metadata name must
// carry the uppercase form for a lookup to exercise case folding at all.

class Holder
{
    public static int FieldA;
    public static int AB;
    public static int ABC;

    // Roslyn emits the backing field as `<Prop>k__BackingField`, which gives us a
    // metadata name containing non-letter ASCII in the 0x3C/0x3E range.
    public static int Prop { get; set; }
}

class UpperAcute
{
    public static int CafÉ; // U+00C9 LATIN CAPITAL LETTER E WITH ACUTE
}

class FinalSigma
{
    public static int Sigmaς; // U+03C2 GREEK SMALL LETTER FINAL SIGMA
}

class Turkish
{
    public static int Dotlessı; // U+0131 LATIN SMALL LETTER DOTLESS I
}

class Program
{
    const BindingFlags Ci =
        BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Static | BindingFlags.Instance | BindingFlags.IgnoreCase;

    const BindingFlags Cs =
        BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Static | BindingFlags.Instance;

    static int Main(string[] args)
    {
        // Exact match.
        FieldInfo exact = typeof(Holder).GetField("FieldA", Ci);
        if (exact == null || exact.Name != "FieldA") return 1;

        // Differing case, both directions.
        FieldInfo lower = typeof(Holder).GetField("fielda", Ci);
        if (lower == null || lower.Name != "FieldA") return 2;

        FieldInfo upper = typeof(Holder).GetField("FIELDA", Ci);
        if (upper == null || upper.Name != "FieldA") return 3;

        // A case-sensitive lookup must not fold.
        if (typeof(Holder).GetField("fielda", Cs) != null) return 4;

        // Genuine non-match of the same length.
        if (typeof(Holder).GetField("fieldz", Ci) != null) return 5;

        // Names differing only after a shared prefix.
        if (typeof(Holder).GetField("abd", Ci) != null) return 6;

        // Differing lengths with a shared prefix: each must select its own field,
        // and an over-long name must miss entirely.
        FieldInfo ab = typeof(Holder).GetField("ab", Ci);
        if (ab == null || ab.Name != "AB") return 7;

        FieldInfo abc = typeof(Holder).GetField("abc", Ci);
        if (abc == null || abc.Name != "ABC") return 8;

        if (typeof(Holder).GetField("abcd", Ci) != null) return 9;

        // Non-letter ASCII is not folded. A naive `byte | 0x20` fold would make
        // '<' (0x3C) equal '\\' (0x5C) and '>' (0x3E) equal '^' (0x5E).
        FieldInfo backing = typeof(Holder).GetField("<prop>k__backingfield", Ci);
        if (backing == null || backing.Name != "<Prop>k__BackingField") return 10;

        if (typeof(Holder).GetField("\\prop^k__backingfield", Ci) != null) return 11;

        return 0;
    }
}

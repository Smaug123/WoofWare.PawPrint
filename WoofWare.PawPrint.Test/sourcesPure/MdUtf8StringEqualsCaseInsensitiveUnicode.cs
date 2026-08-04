using System.Reflection;

// The non-ASCII half of `MdUtf8StringEqualsCaseInsensitive.cs`. See that file for
// why `GetField` observes `MdUtf8String_EqualsCaseInsensitive` directly.
//
// CoreCLR's comparison is Unicode-aware, not ASCII-only: `SString::EqualsCaseInsensitive`
// transcodes both counted UTF-8 buffers to UTF-16 as soon as either has a byte >= 0x80 and
// folds each code unit through the invariant simple uppercase mapping.
//
// `RuntimeType.FilterHelper` lowercases the *requested* name with `ToLowerInvariant` before
// handing it to the QCall, so the metadata names below carry the uppercase form; otherwise
// the lookups would not exercise case folding at all.

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

// CoreCLR's casing table (`minipal_toupper_invariant`, generated from `UnicodeData.txt`) maps
// U+0131 -> 'I' and U+017F -> 'S'. Those mappings are only *reachable* through reflection when
// the two names have the same UTF-8 byte length and the same UTF-16 length, which needs a
// compensating one-byte/two-byte swap elsewhere in the name -- hence these deliberately odd
// two-character names. `.NET`'s own `Char.ToUpperInvariant` declines the U+0131 mapping and
// `StringComparison.OrdinalIgnoreCase` declines the U+017F one, so both are genuine
// discriminators for "which casing table is this?".
class Compensating
{
    public static int ıi; // U+0131 then ASCII 'i': 3 UTF-8 bytes, 2 UTF-16 units
    public static int ſs; // U+017F then ASCII 's': 3 UTF-8 bytes, 2 UTF-16 units
}

class Program
{
    const BindingFlags Ci =
        BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Static | BindingFlags.Instance | BindingFlags.IgnoreCase;

    static int Main(string[] args)
    {
        // U+00E9 folds to U+00C9, so a lowercase request finds an uppercase metadata name.
        FieldInfo acute = typeof(UpperAcute).GetField("café", Ci);
        if (acute == null || acute.Name != "CafÉ") return 1;

        // ... but this is a real case fold, not "ignore the non-ASCII bytes": U+00E8 (grave)
        // is a different letter from U+00C9 (acute), and both encode to two UTF-8 bytes
        // sharing a leading 0xC3.
        if (typeof(UpperAcute).GetField("cafè", Ci) != null) return 2;

        // U+03C2 (final sigma) and U+03C3 (sigma) share the uppercase mapping U+03A3, so the
        // fold makes them equal even though they are distinct code points. Nothing ASCII-only
        // could produce this answer.
        FieldInfo sigma = typeof(FinalSigma).GetField("sigmaσ", Ci);
        if (sigma == null || sigma.Name != "Sigmaς") return 3;

        FieldInfo finalSigma = typeof(FinalSigma).GetField("sigmaς", Ci);
        if (finalSigma == null || finalSigma.Name != "Sigmaς") return 4;

        // Differing UTF-8 byte lengths never match: U+0131 is two bytes but 'i' is one, so
        // `MdUtf8String.EqualsCaseInsensitive` short-circuits before reaching the QCall.
        if (typeof(Turkish).GetField("dotlessi", Ci) != null) return 5;

        FieldInfo dotless = typeof(Turkish).GetField("dotlessı", Ci);
        if (dotless == null || dotless.Name != "Dotlessı") return 6;

        // "Iı" lowercases to "iı", which matches metadata "ıi" only if U+0131 folds to 'I'.
        FieldInfo dotlessFold = typeof(Compensating).GetField("Iı", Ci);
        if (dotlessFold == null || dotlessFold.Name != "ıi") return 7;

        // "Sſ" lowercases to "sſ", which matches metadata "ſs" only if U+017F folds to 'S'.
        FieldInfo longSFold = typeof(Compensating).GetField("Sſ", Ci);
        if (longSFold == null || longSFold.Name != "ſs") return 8;

        return 0;
    }
}

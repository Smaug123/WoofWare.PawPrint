// `String.StartsWith(string, StringComparison)` and `String.EndsWith(string, StringComparison)`
// are `[Intrinsic]` only so the JIT can unroll/vectorise a half-constant Ordinal comparison; the
// managed body is the semantic definition, and it is what PawPrint executes.
//
// The Ordinal arms bottom out in `String._firstChar` plus
// `SpanHelpers.SequenceEqual(ref byte, ref byte, nuint)` (StartsWith) or
// `AsSpan(offset).SequenceEqual(value)` (EndsWith); the OrdinalIgnoreCase arms bottom out in
// `Ordinal.EqualsIgnoreCase`, whose scalar body walks both strings with an `IntPtr byteOffset`
// cursor seeded from `IntPtr.Zero` and advanced 8/4 bytes at a time. The lengths below are
// chosen to cover every step of that unrolled walk (the 4-char loop, the 2-char step, and the
// 1-char tail), and to land a mismatch in each.
//
// Deliberately not covered here: non-ASCII OrdinalIgnoreCase comparison. Both scalar paths
// bail out of the ASCII fast path into `Ordinal.CompareStringIgnoreCase`, which is a separate
// (unimplemented) casing-table boundary; the ASCII paths above never reach it.

using System;
using System.Runtime.CompilerServices;

public class Program
{
    // An opaque identity, so the real runtime's JIT cannot see a constant operand and expand
    // the `[Intrinsic]` into unrolled/vectorised code. That keeps most of the cases below
    // comparing PawPrint's execution of the managed body against the real runtime's execution
    // of the *same* managed body; `TestLiteralOperands` deliberately does the opposite.
    [MethodImpl(MethodImplOptions.NoInlining)]
    private static string Id(string s)
    {
        return s;
    }

    private static int TestOrdinalStartsWith()
    {
        string s = Id("Hello, world");

        if (!s.StartsWith(Id("Hello"), StringComparison.Ordinal)) return 1;
        if (s.StartsWith(Id("hello"), StringComparison.Ordinal)) return 2;
        if (s.StartsWith(Id("world"), StringComparison.Ordinal)) return 3;
        // Empty prefix short-circuits before the switch.
        if (!s.StartsWith(Id(""), StringComparison.Ordinal)) return 4;
        // Same interned literal on both sides, so this is the `ReferenceEquals(this, value)`
        // short-circuit; the non-reference-equal equal-strings case is covered by
        // TestNonInterned below.
        if (!s.StartsWith(Id("Hello, world"), StringComparison.Ordinal)) return 5;
        // Prefix longer than the string.
        if (s.StartsWith(Id("Hello, world!"), StringComparison.Ordinal)) return 6;
        // Single-char prefix: the body returns true on the `_firstChar` compare alone.
        if (!s.StartsWith(Id("H"), StringComparison.Ordinal)) return 7;
        if (s.StartsWith(Id("h"), StringComparison.Ordinal)) return 8;
        // First char matches, later char does not.
        if (s.StartsWith(Id("Hellp"), StringComparison.Ordinal)) return 9;
        if (!s.StartsWith(Id("Hello, worl"), StringComparison.Ordinal)) return 10;

        if (!Id("").StartsWith(Id(""), StringComparison.Ordinal)) return 11;
        if (Id("").StartsWith(Id("a"), StringComparison.Ordinal)) return 12;

        return 0;
    }

    private static int TestOrdinalEndsWith()
    {
        string s = Id("Hello, world");

        if (!s.EndsWith(Id("world"), StringComparison.Ordinal)) return 1;
        if (s.EndsWith(Id("World"), StringComparison.Ordinal)) return 2;
        if (s.EndsWith(Id("Hello"), StringComparison.Ordinal)) return 3;
        if (!s.EndsWith(Id(""), StringComparison.Ordinal)) return 4;
        if (!s.EndsWith(Id("Hello, world"), StringComparison.Ordinal)) return 5;
        // Suffix longer than the string: `(uint)offset <= (uint)Length` catches the negative
        // offset by wrapping it into the huge-unsigned range.
        if (s.EndsWith(Id("xHello, world"), StringComparison.Ordinal)) return 6;
        if (!s.EndsWith(Id("d"), StringComparison.Ordinal)) return 7;
        if (s.EndsWith(Id("D"), StringComparison.Ordinal)) return 8;
        // Last char matches, earlier char does not.
        if (s.EndsWith(Id("worlp"), StringComparison.Ordinal)) return 9;
        if (s.EndsWith(Id("xorld"), StringComparison.Ordinal)) return 10;
        if (!s.EndsWith(Id("ello, world"), StringComparison.Ordinal)) return 11;

        if (!Id("").EndsWith(Id(""), StringComparison.Ordinal)) return 12;
        if (Id("").EndsWith(Id("a"), StringComparison.Ordinal)) return 13;

        return 0;
    }

    private static int TestOrdinalIgnoreCaseStartsWith()
    {
        string s = Id("Hello, world");

        // Every prefix length from 1 to 12, upper-cased: covers the 4-char loop (lengths >= 4),
        // the 2-char step, and the 1-char tail, at every residue.
        if (!s.StartsWith(Id("H"), StringComparison.OrdinalIgnoreCase)) return 1;
        if (!s.StartsWith(Id("h"), StringComparison.OrdinalIgnoreCase)) return 2;
        if (!s.StartsWith(Id("HE"), StringComparison.OrdinalIgnoreCase)) return 3;
        if (!s.StartsWith(Id("HEL"), StringComparison.OrdinalIgnoreCase)) return 4;
        if (!s.StartsWith(Id("HELL"), StringComparison.OrdinalIgnoreCase)) return 5;
        if (!s.StartsWith(Id("HELLO"), StringComparison.OrdinalIgnoreCase)) return 6;
        if (!s.StartsWith(Id("HELLO,"), StringComparison.OrdinalIgnoreCase)) return 7;
        if (!s.StartsWith(Id("HELLO, "), StringComparison.OrdinalIgnoreCase)) return 8;
        if (!s.StartsWith(Id("HELLO, W"), StringComparison.OrdinalIgnoreCase)) return 9;
        if (!s.StartsWith(Id("HELLO, WO"), StringComparison.OrdinalIgnoreCase)) return 10;
        if (!s.StartsWith(Id("HELLO, WOR"), StringComparison.OrdinalIgnoreCase)) return 11;
        if (!s.StartsWith(Id("HELLO, WORL"), StringComparison.OrdinalIgnoreCase)) return 12;
        if (!s.StartsWith(Id("HELLO, WORLD"), StringComparison.OrdinalIgnoreCase)) return 13;

        // A mismatch in each of the three step sizes.
        if (s.StartsWith(Id("X"), StringComparison.OrdinalIgnoreCase)) return 14;
        if (s.StartsWith(Id("HX"), StringComparison.OrdinalIgnoreCase)) return 15;
        if (s.StartsWith(Id("HELX"), StringComparison.OrdinalIgnoreCase)) return 16;
        if (s.StartsWith(Id("HELLX"), StringComparison.OrdinalIgnoreCase)) return 17;
        if (s.StartsWith(Id("HELLO, WORLX"), StringComparison.OrdinalIgnoreCase)) return 18;
        if (s.StartsWith(Id("HELLO, WORLD!"), StringComparison.OrdinalIgnoreCase)) return 19;
        if (!s.StartsWith(Id(""), StringComparison.OrdinalIgnoreCase)) return 20;

        // Non-letters must not be folded. '[' (0x5B) and '{' (0x7B) differ by exactly the 0x20
        // case bit but are outside [A-Za-z], so they must compare unequal — at each of the
        // 1-char, 2-char and 4-char step sizes.
        if (Id("[").StartsWith(Id("{"), StringComparison.OrdinalIgnoreCase)) return 21;
        if (Id("[[").StartsWith(Id("{{"), StringComparison.OrdinalIgnoreCase)) return 22;
        if (Id("[[[[").StartsWith(Id("{{{{"), StringComparison.OrdinalIgnoreCase)) return 23;
        if (Id("[[[[[[").StartsWith(Id("{{{{{{"), StringComparison.OrdinalIgnoreCase)) return 24;
        if (!Id("[[[[[[").StartsWith(Id("[[[[[["), StringComparison.OrdinalIgnoreCase)) return 25;

        // Digits and punctuation compare exactly.
        if (!Id("abc123!").StartsWith(Id("ABC123!"), StringComparison.OrdinalIgnoreCase)) return 26;
        if (Id("abc123!").StartsWith(Id("ABC124!"), StringComparison.OrdinalIgnoreCase)) return 27;

        if (!Id("").StartsWith(Id(""), StringComparison.OrdinalIgnoreCase)) return 28;
        if (Id("").StartsWith(Id("a"), StringComparison.OrdinalIgnoreCase)) return 29;

        return 0;
    }

    private static int TestOrdinalIgnoreCaseEndsWith()
    {
        string s = Id("Hello, world");

        if (!s.EndsWith(Id("D"), StringComparison.OrdinalIgnoreCase)) return 1;
        if (!s.EndsWith(Id("LD"), StringComparison.OrdinalIgnoreCase)) return 2;
        if (!s.EndsWith(Id("RLD"), StringComparison.OrdinalIgnoreCase)) return 3;
        if (!s.EndsWith(Id("ORLD"), StringComparison.OrdinalIgnoreCase)) return 4;
        if (!s.EndsWith(Id("WORLD"), StringComparison.OrdinalIgnoreCase)) return 5;
        if (!s.EndsWith(Id(" WORLD"), StringComparison.OrdinalIgnoreCase)) return 6;
        if (!s.EndsWith(Id(", WORLD"), StringComparison.OrdinalIgnoreCase)) return 7;
        if (!s.EndsWith(Id("O, WORLD"), StringComparison.OrdinalIgnoreCase)) return 8;
        if (!s.EndsWith(Id("HELLO, WORLD"), StringComparison.OrdinalIgnoreCase)) return 9;

        if (s.EndsWith(Id("X"), StringComparison.OrdinalIgnoreCase)) return 10;
        if (s.EndsWith(Id("XD"), StringComparison.OrdinalIgnoreCase)) return 11;
        if (s.EndsWith(Id("XRLD"), StringComparison.OrdinalIgnoreCase)) return 12;
        if (s.EndsWith(Id("XORLD"), StringComparison.OrdinalIgnoreCase)) return 13;
        if (s.EndsWith(Id("XHELLO, WORLD"), StringComparison.OrdinalIgnoreCase)) return 14;
        if (!s.EndsWith(Id(""), StringComparison.OrdinalIgnoreCase)) return 15;

        if (Id("[").EndsWith(Id("{"), StringComparison.OrdinalIgnoreCase)) return 16;
        if (Id("[[[[[[").EndsWith(Id("{{{{{{"), StringComparison.OrdinalIgnoreCase)) return 17;

        if (!Id("").EndsWith(Id(""), StringComparison.OrdinalIgnoreCase)) return 18;
        if (Id("").EndsWith(Id("a"), StringComparison.OrdinalIgnoreCase)) return 19;

        return 0;
    }

    // Everything above compares interned literals, so `ReferenceEquals(this, value)` fires
    // whenever the two strings are equal. These cases build the argument on the heap instead,
    // so the equal-strings comparisons really walk the character data.
    private static int TestNonInterned()
    {
        string s = Id("Hello, world");
        string sameContent = new string(Id("Hello, world").AsSpan());

        if (ReferenceEquals(s, sameContent)) return 1;

        if (!s.StartsWith(sameContent, StringComparison.Ordinal)) return 2;
        if (!s.EndsWith(sameContent, StringComparison.Ordinal)) return 3;
        if (!s.StartsWith(sameContent, StringComparison.OrdinalIgnoreCase)) return 4;
        if (!s.EndsWith(sameContent, StringComparison.OrdinalIgnoreCase)) return 5;
        if (!sameContent.StartsWith(Id("Hello"), StringComparison.Ordinal)) return 6;
        if (!sameContent.EndsWith(Id("world"), StringComparison.Ordinal)) return 7;
        if (!sameContent.StartsWith(Id("HELLO"), StringComparison.OrdinalIgnoreCase)) return 8;
        if (!sameContent.EndsWith(Id("WORLD"), StringComparison.OrdinalIgnoreCase)) return 9;

        // A prefix/suffix that is equal to a *proper* part of the receiver, so neither
        // short-circuit fires and the character walk must do the work.
        string prefix = new string(Id("Hello").AsSpan());
        string suffix = new string(Id("world").AsSpan());
        if (!s.StartsWith(prefix, StringComparison.Ordinal)) return 10;
        if (!s.EndsWith(suffix, StringComparison.Ordinal)) return 11;
        if (s.EndsWith(prefix, StringComparison.Ordinal)) return 12;
        if (s.StartsWith(suffix, StringComparison.Ordinal)) return 13;

        return 0;
    }

    // The mirror image of everything above: constant operands, which is precisely the shape the
    // `[Intrinsic]` exists for. The real runtime answers these from JIT-expanded unrolled code
    // rather than from the managed body, so agreeing here is the stronger statement — PawPrint's
    // execution of the IL matches what the JIT's replacement for that IL computes.
    private static int TestLiteralOperands()
    {
        string s = "Hello, world";

        if (!s.StartsWith("Hello", StringComparison.Ordinal)) return 1;
        if (s.StartsWith("hello", StringComparison.Ordinal)) return 2;
        if (!s.EndsWith("world", StringComparison.Ordinal)) return 3;
        if (s.EndsWith("World", StringComparison.Ordinal)) return 4;
        if (!s.StartsWith("HELLO", StringComparison.OrdinalIgnoreCase)) return 5;
        if (s.StartsWith("HELLX", StringComparison.OrdinalIgnoreCase)) return 6;
        if (!s.EndsWith("WORLD", StringComparison.OrdinalIgnoreCase)) return 7;
        if (s.EndsWith("XORLD", StringComparison.OrdinalIgnoreCase)) return 8;

        return 0;
    }

    // The culture-sensitive arms route through `CompareInfo.IsPrefix`/`IsSuffix`, which is a
    // different (already working) boundary.
    //
    // Only plain ASCII is asserted here: PawPrint's emulated environment sets
    // DOTNET_SYSTEM_GLOBALIZATION_INVARIANT=1, so the guest collates with the invariant backend
    // while the real runtime this is differenced against collates with ICU under the host's
    // current culture. The two agree on ASCII prefix/suffix matching and on ASCII case folding
    // (no letter below is one of the culture-sensitive ones, e.g. Turkish dotless i), but
    // anything with real collation — an ignorable character, an expansion like 'æ', a
    // locale-specific ordering — would be asserting the host's ICU data against PawPrint's
    // invariant tables.
    private static int TestCultureSensitive()
    {
        string s = Id("Hello, world");

        if (!s.StartsWith(Id("Hello"), StringComparison.CurrentCulture)) return 1;
        if (s.StartsWith(Id("hello"), StringComparison.CurrentCulture)) return 2;
        if (!s.StartsWith(Id("hello"), StringComparison.CurrentCultureIgnoreCase)) return 3;
        if (!s.StartsWith(Id("Hello"), StringComparison.InvariantCulture)) return 4;
        if (s.StartsWith(Id("hello"), StringComparison.InvariantCulture)) return 5;
        if (!s.StartsWith(Id("hello"), StringComparison.InvariantCultureIgnoreCase)) return 6;

        if (!s.EndsWith(Id("world"), StringComparison.CurrentCulture)) return 7;
        if (s.EndsWith(Id("World"), StringComparison.CurrentCulture)) return 8;
        if (!s.EndsWith(Id("World"), StringComparison.CurrentCultureIgnoreCase)) return 9;
        if (!s.EndsWith(Id("world"), StringComparison.InvariantCulture)) return 10;
        if (s.EndsWith(Id("World"), StringComparison.InvariantCulture)) return 11;
        if (!s.EndsWith(Id("World"), StringComparison.InvariantCultureIgnoreCase)) return 12;

        return 0;
    }

    private static int TestExceptions()
    {
        string s = Id("Hello, world");

        // `ArgumentNullException.ThrowIfNull(value)` runs before anything else, so a null value
        // wins even when the comparison type is also invalid.
        try
        {
            s.StartsWith(null, StringComparison.Ordinal);
            return 1;
        }
        catch (ArgumentNullException)
        {
        }

        try
        {
            s.EndsWith(null, StringComparison.Ordinal);
            return 2;
        }
        catch (ArgumentNullException)
        {
        }

        try
        {
            s.StartsWith(null, (StringComparison)42);
            return 3;
        }
        catch (ArgumentNullException)
        {
        }

        // An out-of-range comparison type is an `ArgumentException` (not the derived
        // `ArgumentNullException`), thrown from the switch's default arm.
        try
        {
            s.StartsWith(Id("Hello"), (StringComparison)42);
            return 4;
        }
        catch (ArgumentException e)
        {
            if (e is ArgumentNullException) return 5;
        }

        try
        {
            s.EndsWith(Id("world"), (StringComparison)42);
            return 6;
        }
        catch (ArgumentException e)
        {
            if (e is ArgumentNullException) return 7;
        }

        // The two short-circuits that return `true` without consulting the switch still validate
        // the comparison type first, via `CheckStringComparison`.
        try
        {
            s.StartsWith(Id(""), (StringComparison)42);
            return 8;
        }
        catch (ArgumentException e)
        {
            if (e is ArgumentNullException) return 9;
        }

        try
        {
            s.EndsWith(Id(""), (StringComparison)42);
            return 10;
        }
        catch (ArgumentException e)
        {
            if (e is ArgumentNullException) return 11;
        }

        try
        {
            s.StartsWith(s, (StringComparison)42);
            return 12;
        }
        catch (ArgumentException e)
        {
            if (e is ArgumentNullException) return 13;
        }

        try
        {
            s.EndsWith(s, (StringComparison)42);
            return 14;
        }
        catch (ArgumentException e)
        {
            if (e is ArgumentNullException) return 15;
        }

        // Negative values are out of range too: the check is a single unsigned compare.
        try
        {
            s.StartsWith(Id("Hello"), (StringComparison)(-1));
            return 16;
        }
        catch (ArgumentException e)
        {
            if (e is ArgumentNullException) return 17;
        }

        return 0;
    }

    public static int Main(string[] args)
    {
        int result = TestOrdinalStartsWith();
        if (result != 0) return result;

        result = TestOrdinalEndsWith();
        if (result != 0) return 20 + result;

        result = TestOrdinalIgnoreCaseStartsWith();
        if (result != 0) return 40 + result;

        result = TestOrdinalIgnoreCaseEndsWith();
        if (result != 0) return 80 + result;

        result = TestNonInterned();
        if (result != 0) return 120 + result;

        result = TestLiteralOperands();
        if (result != 0) return 140 + result;

        result = TestCultureSensitive();
        if (result != 0) return 160 + result;

        result = TestExceptions();
        if (result != 0) return 180 + result;

        return 0;
    }
}

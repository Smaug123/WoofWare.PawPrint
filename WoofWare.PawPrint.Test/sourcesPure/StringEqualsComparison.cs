// `String.Equals(string, StringComparison)` and the static
// `String.Equals(string, string, StringComparison)` are `[Intrinsic]` only so the JIT can
// unroll/vectorise a half-constant Ordinal comparison; the managed body is the semantic
// definition, and it is what PawPrint executes.
//
// This file covers the two ordinal arms, the null handling and the argument validation. The
// four culture-sensitive arms are a separate boundary (`CompareInfo.Compare`) with its own
// constraints on what may safely be asserted, so they live in
// `StringEqualsComparisonCulture.cs`. Splitting them also keeps every exit code below 128,
// which a single file's worth of checks would not: an exit code is 8 bits, and 128 and above
// collide with the band the real-runtime oracle uses for signalled termination.
//
// Both ordinal arms compare `Length` first and only then walk characters, so every
// character-walking case here is a *same-length* pair — the difference from
// `StringStartsWithComparison.cs`, where the operand length varied and the walk started from a
// known offset. Ordinal bottoms out in `EqualsHelper`, i.e.
// `SpanHelpers.SequenceEqual(ref byte, ref byte, nuint)` over `GetRawStringDataAsUInt8()`.
// OrdinalIgnoreCase bottoms out in `Ordinal.EqualsIgnoreCase(ref char, ref char, int)`, whose
// scalar body walks an `IntPtr byteOffset` cursor 8 then 4 bytes at a time; lengths 1 to 8
// below cover every combination of those steps (floor(L/4) turns of the 4-char loop, then at
// most one 2-char step, then at most a 1-char tail), with a mismatch landing in each.
//
// Non-ASCII *Ordinal* comparison is covered: it is a bitwise compare, so the two runtimes
// agree by construction. Non-ASCII *OrdinalIgnoreCase* is deliberately not asserted. PawPrint
// does answer it — it leaves `EqualsIgnoreCase_Scalar`'s ASCII fast path for
// `Ordinal.CompareStringIgnoreCase`, which under the guest's
// DOTNET_SYSTEM_GLOBALIZATION_INVARIANT=1 bottoms out in `InvariantModeCasing`'s managed
// `CharUnicodeInfo` tables, and PawPrint runs those — but the runtime this is differenced
// against runs without that variable and casts with ICU instead, so agreement would be a fact
// about two casing tables rather than about `String.Equals`.

using System;
using System.Runtime.CompilerServices;

public class Program
{
    // An opaque identity, so the real runtime's JIT cannot see a constant operand. That is not
    // about intrinsic expansion here — `NI_System_String_Equals` is absent from the JIT's
    // Tier0 lightweight-intrinsic list, so a guest's `Main`, which runs once at Tier0, gets an
    // ordinary call in both runtimes either way — but it does stop Roslyn and the JIT from
    // constant-folding the comparison itself and eliding the call.
    [MethodImpl(MethodImplOptions.NoInlining)]
    private static string Id(string s)
    {
        return s;
    }

    // A heap copy, so `ReferenceEquals` is false and the character walk has to do the work.
    // Without this, interning would make every equal-pair case below take the short-circuit
    // and the walk would never be exercised at all.
    [MethodImpl(MethodImplOptions.NoInlining)]
    private static string Copy(string s)
    {
        return new string(s.AsSpan());
    }

    private static int TestOrdinalInstance()
    {
        string s = Id("Hello, world");
        string copy = Copy(s);

        if (ReferenceEquals(s, copy)) return 1;

        if (!s.Equals(copy, StringComparison.Ordinal)) return 2;
        // Same interned literal on both sides: the `ReferenceEquals` short-circuit, rather than
        // the character walk that case 2 forces.
        if (!s.Equals(Id("Hello, world"), StringComparison.Ordinal)) return 3;
        // Length mismatch in each direction, so the `Length != Length` early return is reached
        // from both sides.
        if (s.Equals(Id("Hello, world!"), StringComparison.Ordinal)) return 4;
        if (s.Equals(Id("Hello, worl"), StringComparison.Ordinal)) return 5;
        // Same length, mismatch at the first and at the last character respectively.
        if (s.Equals(Id("hello, world"), StringComparison.Ordinal)) return 6;
        if (s.Equals(Id("Hello, worlD"), StringComparison.Ordinal)) return 7;
        if (!Id("").Equals(Id(""), StringComparison.Ordinal)) return 8;
        if (Id("").Equals(Id("a"), StringComparison.Ordinal)) return 9;
        if (Id("a").Equals(Id(""), StringComparison.Ordinal)) return 10;
        // Non-ASCII, Ordinal: a bitwise compare, so both runtimes agree by construction.
        if (!Id("é中ß").Equals(Copy(Id("é中ß")), StringComparison.Ordinal)) return 11;
        if (Id("é中ß").Equals(Id("é中à"), StringComparison.Ordinal)) return 12;
        // A surrogate pair differing only in its low surrogate, so the byte walk must reach the
        // end of the data rather than stopping at the first code unit.
        if (Id("😀").Equals(Id("😁"), StringComparison.Ordinal)) return 13;

        return 0;
    }

    private static int TestOrdinalStatic()
    {
        string s = Id("Hello, world");

        if (!String.Equals(s, Copy(s), StringComparison.Ordinal)) return 1;
        if (!String.Equals(s, Id("Hello, world"), StringComparison.Ordinal)) return 2;
        if (String.Equals(s, Id("Hello, world!"), StringComparison.Ordinal)) return 3;
        if (String.Equals(s, Id("Hello, worl"), StringComparison.Ordinal)) return 4;
        if (String.Equals(s, Id("hello, world"), StringComparison.Ordinal)) return 5;
        if (String.Equals(s, Id("Hello, worlD"), StringComparison.Ordinal)) return 6;
        if (!String.Equals(Id(""), Id(""), StringComparison.Ordinal)) return 7;
        if (String.Equals(Id(""), Id("a"), StringComparison.Ordinal)) return 8;

        return 0;
    }

    // Every length from 1 to 8, so every combination of `EqualsIgnoreCase_Scalar`'s steps is
    // exercised: L=1 is the 1-char tail alone, L=2 the 2-char step alone, L=3 both, L=4 one
    // turn of the 4-char loop, L=5..7 that loop plus each smaller combination, L=8 two turns.
    // Each length appears once agreeing and once mismatching in its *final* step; the two
    // longest also mismatch in their *first* step, so a walk that compared only one end of the
    // data would fail here.
    private static int TestOrdinalIgnoreCaseLengths()
    {
        if (!Id("a").Equals(Id("A"), StringComparison.OrdinalIgnoreCase)) return 1;
        if (Id("a").Equals(Id("B"), StringComparison.OrdinalIgnoreCase)) return 2;
        if (!Id("ab").Equals(Id("AB"), StringComparison.OrdinalIgnoreCase)) return 3;
        if (Id("ab").Equals(Id("AX"), StringComparison.OrdinalIgnoreCase)) return 4;
        if (!Id("abc").Equals(Id("ABC"), StringComparison.OrdinalIgnoreCase)) return 5;
        if (Id("abc").Equals(Id("ABX"), StringComparison.OrdinalIgnoreCase)) return 6;
        if (!Id("abcd").Equals(Id("ABCD"), StringComparison.OrdinalIgnoreCase)) return 7;
        if (Id("abcd").Equals(Id("ABCX"), StringComparison.OrdinalIgnoreCase)) return 8;
        if (!Id("abcde").Equals(Id("ABCDE"), StringComparison.OrdinalIgnoreCase)) return 9;
        if (Id("abcde").Equals(Id("ABCDX"), StringComparison.OrdinalIgnoreCase)) return 10;
        if (!Id("abcdef").Equals(Id("ABCDEF"), StringComparison.OrdinalIgnoreCase)) return 11;
        if (Id("abcdef").Equals(Id("ABCDEX"), StringComparison.OrdinalIgnoreCase)) return 12;
        if (!Id("abcdefg").Equals(Id("ABCDEFG"), StringComparison.OrdinalIgnoreCase)) return 13;
        if (Id("abcdefg").Equals(Id("ABCDEFX"), StringComparison.OrdinalIgnoreCase)) return 14;
        if (!Id("abcdefgh").Equals(Id("ABCDEFGH"), StringComparison.OrdinalIgnoreCase)) return 15;
        if (Id("abcdefgh").Equals(Id("ABCDEFGX"), StringComparison.OrdinalIgnoreCase)) return 16;
        if (Id("abcdefgh").Equals(Id("XBCDEFGH"), StringComparison.OrdinalIgnoreCase)) return 17;
        if (Id("abcde").Equals(Id("XBCDE"), StringComparison.OrdinalIgnoreCase)) return 18;
        // Length mismatch short-circuits before the walk, in each direction.
        if (Id("abcd").Equals(Id("ABCDE"), StringComparison.OrdinalIgnoreCase)) return 19;
        if (Id("abcde").Equals(Id("ABCD"), StringComparison.OrdinalIgnoreCase)) return 20;
        if (!Id("").Equals(Id(""), StringComparison.OrdinalIgnoreCase)) return 21;
        // Equal content on the heap, so the walk runs rather than the reference short-circuit.
        if (!Id("abcdefgh").Equals(Copy(Id("abcdefgh")), StringComparison.OrdinalIgnoreCase)) return 22;

        return 0;
    }

    private static int TestOrdinalIgnoreCaseFolding()
    {
        // Non-letters must not be folded. '[' (0x5B) and '{' (0x7B) differ by exactly the 0x20
        // case bit but are outside [A-Za-z], so they must compare unequal — at the 1-char,
        // 2-char and 4-char step sizes and inside the 8-byte loop. A fold written as a blanket
        // `| 0x20` rather than a letter-gated one would call all of these equal.
        if (Id("[").Equals(Id("{"), StringComparison.OrdinalIgnoreCase)) return 1;
        if (Id("[[").Equals(Id("{{"), StringComparison.OrdinalIgnoreCase)) return 2;
        if (Id("[[[[").Equals(Id("{{{{"), StringComparison.OrdinalIgnoreCase)) return 3;
        if (Id("[[[[[[").Equals(Id("{{{{{{"), StringComparison.OrdinalIgnoreCase)) return 4;
        if (!Id("[[[[[[").Equals(Copy(Id("[[[[[[")), StringComparison.OrdinalIgnoreCase)) return 5;
        // The other pairs adjacent to [A-Za-z], one on each side of each half of the range:
        // '@' (0x40) vs '`' (0x60), and '_' (0x5F) vs DEL (0x7F). DEL is still on the ASCII
        // fast path, since the tail's bail-out test is `> 0x7F`.
        if (Id("@").Equals(Id("`"), StringComparison.OrdinalIgnoreCase)) return 6;
        if (Id("_").Equals(Id("\u007F"), StringComparison.OrdinalIgnoreCase)) return 7;
        // A letter against a non-letter that shares its low five bits.
        if (Id("a").Equals(Id("{"), StringComparison.OrdinalIgnoreCase)) return 8;
        // Digits and punctuation compare exactly.
        if (!Id("abc123!").Equals(Id("ABC123!"), StringComparison.OrdinalIgnoreCase)) return 9;
        if (Id("abc123!").Equals(Id("ABC124!"), StringComparison.OrdinalIgnoreCase)) return 10;
        // Mixed case on both sides, not just one side upper-cased.
        if (!Id("HeLLo, WoRld").Equals(Id("hEllO, wOrLD"), StringComparison.OrdinalIgnoreCase)) return 11;

        if (!String.Equals(Id("abcdefgh"), Id("ABCDEFGH"), StringComparison.OrdinalIgnoreCase)) return 12;
        if (String.Equals(Id("abcdefgh"), Id("ABCDEFGX"), StringComparison.OrdinalIgnoreCase)) return 13;
        if (String.Equals(Id("[[[[[["), Id("{{{{{{"), StringComparison.OrdinalIgnoreCase)) return 14;
        if (String.Equals(Id("abcd"), Id("ABCDE"), StringComparison.OrdinalIgnoreCase)) return 15;

        return 0;
    }

    private static int TestNulls()
    {
        string s = Id("Hello");

        // The instance overload's null arm returns false after validating the comparison type.
        // The arm is shared by every comparison type, so one case per family suffices.
        if (s.Equals(null, StringComparison.Ordinal)) return 1;
        if (s.Equals(null, StringComparison.OrdinalIgnoreCase)) return 2;
        if (s.Equals(null, StringComparison.CurrentCulture)) return 3;

        // The static overload sorts null equal to null and unequal to everything else,
        // including the empty string. Note the two-null case is the *reference-equality*
        // short-circuit, not the null check: `ReferenceEquals(null, null)` is true.
        if (!String.Equals(null, null, StringComparison.Ordinal)) return 4;
        if (!String.Equals(null, null, StringComparison.OrdinalIgnoreCase)) return 5;
        if (!String.Equals(null, null, StringComparison.CurrentCulture)) return 6;
        if (String.Equals(null, s, StringComparison.Ordinal)) return 7;
        if (String.Equals(s, null, StringComparison.Ordinal)) return 8;
        if (String.Equals(null, s, StringComparison.OrdinalIgnoreCase)) return 9;
        if (String.Equals(s, null, StringComparison.CurrentCulture)) return 10;
        if (String.Equals(Id(""), null, StringComparison.Ordinal)) return 11;
        if (Id("").Equals(null, StringComparison.Ordinal)) return 12;

        return 0;
    }

    // Constant operands, which is the shape the `[Intrinsic]` exists for. The oracle does not
    // actually reach the JIT's expansion here (see `Id` above), so this is not a stronger
    // claim than the rest of the file — it is coverage of a different *operand provenance*:
    // both sides are `ldstr`-interned literals, so the reference-equality short-circuit fires
    // wherever the strings are equal, and the switch is reached only where they differ.
    private static int TestLiteralOperands()
    {
        string s = "Hello, world";

        if (!s.Equals("Hello, world", StringComparison.Ordinal)) return 1;
        if (s.Equals("hello, world", StringComparison.Ordinal)) return 2;
        if (!s.Equals("HELLO, WORLD", StringComparison.OrdinalIgnoreCase)) return 3;
        if (s.Equals("HELLO, WORLX", StringComparison.OrdinalIgnoreCase)) return 4;
        if (!String.Equals(s, "Hello, world", StringComparison.Ordinal)) return 5;
        if (String.Equals(s, "hello, world", StringComparison.Ordinal)) return 6;
        if (!String.Equals(s, "HELLO, WORLD", StringComparison.OrdinalIgnoreCase)) return 7;
        if (String.Equals(s, "HELLO, WORLX", StringComparison.OrdinalIgnoreCase)) return 8;

        return 0;
    }

    // Every route into the `ArgumentException`. `CheckStringComparison` is a single unsigned
    // compare against `StringComparison.OrdinalIgnoreCase` (5), and it is reached from the two
    // short-circuits — reference equality and null — each of which validates *before*
    // returning its answer. The third route is the switch's `default` arm, which constructs
    // the `ArgumentException` directly rather than going through `ThrowHelper`.
    //
    // Where a null is involved the catch also rules out `ArgumentNullException`. Neither
    // overload has an `ArgumentNullException.ThrowIfNull` — null is a legal argument to both —
    // so a derived `ArgumentNullException` would mean PawPrint threw from a site CoreLib does
    // not have, and a bare `catch (ArgumentException)` would have swallowed it.
    private static int TestExceptions()
    {
        string s = Id("Hello, world");
        string copy = Copy(s);

        // Via the switch's default arm: two distinct, non-null strings.
        try
        {
            s.Equals(copy, (StringComparison)42);
            return 1;
        }
        catch (ArgumentException e)
        {
            if (e is ArgumentNullException) return 2;
        }

        // Via the `ReferenceEquals` short-circuit, which returns true only *after* validating,
        // so this throws rather than returning true.
        try
        {
            s.Equals(s, (StringComparison)42);
            return 3;
        }
        catch (ArgumentException e)
        {
            if (e is ArgumentNullException) return 4;
        }

        // Via the null short-circuit, which likewise returns false only after validating.
        try
        {
            s.Equals(null, (StringComparison)42);
            return 5;
        }
        catch (ArgumentException e)
        {
            if (e is ArgumentNullException) return 6;
        }

        // Negative values are out of range too, because the check is a single unsigned compare
        // rather than a pair of signed ones.
        try
        {
            s.Equals(copy, (StringComparison)(-1));
            return 7;
        }
        catch (ArgumentException e)
        {
            if (e is ArgumentNullException) return 8;
        }

        // The value one past the top of the range. 42 does not pin the boundary; 6 does.
        try
        {
            s.Equals(copy, (StringComparison)6);
            return 9;
        }
        catch (ArgumentException e)
        {
            if (e is ArgumentNullException) return 10;
        }

        // ... and the top of the range itself must not throw, which is what stops the check
        // from being satisfied by something that rejects one value too many.
        if (!s.Equals(copy, StringComparison.OrdinalIgnoreCase)) return 11;

        // The same routes through the static overload.
        try
        {
            String.Equals(s, copy, (StringComparison)42);
            return 12;
        }
        catch (ArgumentException e)
        {
            if (e is ArgumentNullException) return 13;
        }

        try
        {
            String.Equals(s, s, (StringComparison)42);
            return 14;
        }
        catch (ArgumentException e)
        {
            if (e is ArgumentNullException) return 15;
        }

        try
        {
            String.Equals(s, null, (StringComparison)42);
            return 16;
        }
        catch (ArgumentException e)
        {
            if (e is ArgumentNullException) return 17;
        }

        try
        {
            String.Equals(null, s, (StringComparison)42);
            return 18;
        }
        catch (ArgumentException e)
        {
            if (e is ArgumentNullException) return 19;
        }

        // Both null: the reference-equality short-circuit, which still validates.
        try
        {
            String.Equals(null, null, (StringComparison)42);
            return 20;
        }
        catch (ArgumentException e)
        {
            if (e is ArgumentNullException) return 21;
        }

        try
        {
            String.Equals(s, copy, (StringComparison)6);
            return 22;
        }
        catch (ArgumentException e)
        {
            if (e is ArgumentNullException) return 23;
        }

        if (!String.Equals(s, copy, StringComparison.OrdinalIgnoreCase)) return 24;

        return 0;
    }

    // The offsets keep every failure distinguishable while staying inside the 8 bits a process
    // exit code has: the largest value any branch below can return is 114.
    public static int Main(string[] args)
    {
        int result = TestOrdinalInstance();
        if (result != 0) return result;

        result = TestOrdinalStatic();
        if (result != 0) return 15 + result;

        result = TestOrdinalIgnoreCaseLengths();
        if (result != 0) return 25 + result;

        result = TestOrdinalIgnoreCaseFolding();
        if (result != 0) return 50 + result;

        result = TestNulls();
        if (result != 0) return 67 + result;

        result = TestLiteralOperands();
        if (result != 0) return 80 + result;

        result = TestExceptions();
        if (result != 0) return 90 + result;

        return 0;
    }
}

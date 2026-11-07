// `MemoryExtensions.Equals(ReadOnlySpan<char>, ReadOnlySpan<char>, StringComparison)` is
// `[Intrinsic]` only so the JIT can unroll a half-constant Ordinal comparison; its managed body
// is the semantic definition. That body is `string.CheckStringComparison` and then a switch onto
// the same comparers `String.Equals(string, string, StringComparison)` reaches:
// `CompareInfo.Compare(ReadOnlySpan<char>, ReadOnlySpan<char>, CompareOptions)` for the four
// culture arms (the string overload of `Compare` forwards to that span overload once neither
// operand is null), `SequenceEqual` for Ordinal, and `Ordinal.EqualsIgnoreCase` for
// OrdinalIgnoreCase.
//
// So on any runtime, whatever its casing tables, collation or current culture, the span overload
// and the string overload give the same verdict for every comparison type. That agreement is
// what `TestAgreement` asserts, and it is why the operands there may be non-ASCII: the
// differential oracle casts with the host's ICU while PawPrint's guest runs under
// DOTNET_SYSTEM_GLOBALIZATION_INVARIANT=1 and casts with CoreLib's own `CharUnicodeInfo`
// tables, so the verdicts themselves may differ between the two runtimes, but within each runtime
// the two overloads must agree. A runtime that answered the span overload from a different
// casing table than the string overload breaks it. U+A7D5 and U+A7D4 are such a pair today:
// they are a Unicode 17 case pair, which a host ICU of that vintage folds together while .NET
// 10's tables call U+A7D5 unassigned.
//
// The remaining sections assert verdicts directly, so they keep to operands on which every
// casing table and collation agrees: ASCII without 'i'/'I', and no control characters.

using System;
using System.Runtime.CompilerServices;

public class Program
{
    // Opaque, so neither Roslyn nor the real runtime's JIT sees a constant operand.
    [MethodImpl(MethodImplOptions.NoInlining)]
    private static string Id(string s)
    {
        return s;
    }

    // A heap copy, so the two operands never share storage and no reference-identity
    // short-circuit answers in place of the comparer.
    [MethodImpl(MethodImplOptions.NoInlining)]
    private static string Copy(string s)
    {
        return new string(s.AsSpan());
    }

    private static readonly string[][] Pairs =
    {
        new[] { "ꟕ", "꟔" },
        new[] { "꟏", "꟎" },
        new[] { "é", "É" },
        new[] { "K", "K" },
        new[] { "ı", "I" },
        new[] { "Σ", "ς" },
        new[] { "ǅ", "ǆ" },
        new[] { "ß", "ẞ" },
        new[] { "Straße", "STRASSE" },
        new[] { "😀", "😁" },
        // A supplementary-plane case pair (DESERET CAPITAL LETTER LONG I and its small letter).
        new[] { "𐐀", "𐐨" },
        new[] { "abc", "ABC" },
        new[] { "a\u0001", "a" },
        new[] { "", "" },
    };

    // For pair `p` and comparison type `c`, a disagreement is reported as `6 * p + c + 1`, so the
    // exit code alone names both. With 14 pairs that is at most 84.
    private static int TestAgreement()
    {
        for (int p = 0; p < Pairs.Length; p++)
        {
            string left = Id(Pairs[p][0]);
            string right = Copy(Pairs[p][1]);

            for (int c = 0; c <= 5; c++)
            {
                StringComparison comparison = (StringComparison)c;
                bool viaSpan = left.AsSpan().Equals(right.AsSpan(), comparison);
                bool viaString = String.Equals(left, right, comparison);

                if (viaSpan != viaString) return 6 * p + c + 1;
            }
        }

        return 0;
    }

    private static int TestVerdicts()
    {
        ReadOnlySpan<char> hello = Id("Hello, world").AsSpan();

        if (!hello.Equals(Copy("Hello, world").AsSpan(), StringComparison.Ordinal)) return 1;
        if (hello.Equals(Copy("hello, world").AsSpan(), StringComparison.Ordinal)) return 2;
        if (!hello.Equals(Copy("hELLO, WORLD").AsSpan(), StringComparison.OrdinalIgnoreCase)) return 3;
        if (hello.Equals(Copy("hELLO, WORLX").AsSpan(), StringComparison.OrdinalIgnoreCase)) return 4;
        if (hello.Equals(Copy("Hello, world!").AsSpan(), StringComparison.OrdinalIgnoreCase)) return 5;
        if (!hello.Equals(Copy("Hello, world").AsSpan(), StringComparison.InvariantCulture)) return 6;
        if (hello.Equals(Copy("hello, world").AsSpan(), StringComparison.InvariantCulture)) return 7;
        if (!hello.Equals(Copy("hello, world").AsSpan(), StringComparison.InvariantCultureIgnoreCase)) return 8;
        if (!hello.Equals(Copy("Hello, world").AsSpan(), StringComparison.CurrentCulture)) return 9;
        if (!ReadOnlySpan<char>.Empty.Equals(ReadOnlySpan<char>.Empty, StringComparison.OrdinalIgnoreCase)) return 10;

        return 0;
    }

    // `CheckStringComparison` runs before the switch, so an invalid value throws whatever the
    // operands are, empty ones included. It throws `ArgumentException` itself, never the derived
    // `ArgumentNullException`, which is why the catch rules that out.
    private static int ThrowsArgumentException(ReadOnlySpan<char> left, ReadOnlySpan<char> right, int comparison)
    {
        try
        {
            left.Equals(right, (StringComparison)comparison);
            return 1;
        }
        catch (ArgumentException e)
        {
            if (e is ArgumentNullException) return 2;
            return 0;
        }
    }

    private static int TestInvalidComparison()
    {
        int result = ThrowsArgumentException(Id("abc").AsSpan(), Copy("abc").AsSpan(), 6);
        if (result != 0) return result;

        result = ThrowsArgumentException(Id("abc").AsSpan(), Copy("abd").AsSpan(), -1);
        if (result != 0) return 2 + result;

        result = ThrowsArgumentException(ReadOnlySpan<char>.Empty, ReadOnlySpan<char>.Empty, 42);
        if (result != 0) return 4 + result;

        return 0;
    }

    public static int Main(string[] args)
    {
        int result = TestAgreement();
        if (result != 0) return result;

        result = TestVerdicts();
        if (result != 0) return 90 + result;

        result = TestInvalidComparison();
        if (result != 0) return 110 + result;

        return 0;
    }
}

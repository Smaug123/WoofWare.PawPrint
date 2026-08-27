using System;

public class UInt128EqualityTests
{
    // `UInt128` carries a *type-level* [Intrinsic], so every member of it is a JIT intrinsic
    // as far as PawPrint's call path is concerned. Only the members this file exercises are
    // allowlisted, so nothing here may call `ToString`, `Equals`, `GetHashCode`, arithmetic,
    // or the explicit narrowing conversions.

    public static int TestDefaultAndMinValue()
    {
        UInt128 a = default;
        UInt128 b = default;
        if (!(a == b)) return 1;
        if (!(UInt128.MinValue == default(UInt128))) return 2;
        if (!(new UInt128(0, 0) == UInt128.MinValue)) return 3;
        // The one non-trivial thing `get_MinValue` could get wrong is producing something
        // other than zero, which the two checks above already exclude; this pins that it is
        // not merely equal to *itself*.
        if (new UInt128(0, 1) == UInt128.MinValue) return 4;
        return 0;
    }

    public static int TestMaxValue()
    {
        if (!(UInt128.MaxValue == new UInt128(ulong.MaxValue, ulong.MaxValue))) return 1;
        if (UInt128.MaxValue == UInt128.MinValue) return 2;
        // MaxValue's IL is `ldc.i4.m1; conv.i8` twice, so a sign-extension bug would show
        // up as one half being right and the other wrong only if the halves disagreed;
        // these two catch a MaxValue that filled just one half.
        if (UInt128.MaxValue == new UInt128(ulong.MaxValue, 0)) return 3;
        if (UInt128.MaxValue == new UInt128(0, ulong.MaxValue)) return 4;
        return 0;
    }

    public static int TestBothHalvesAreCompared()
    {
        // op_Equality's IL branches on `_lower` first and only then compares `_upper`, so
        // each half needs a case where it alone differs — otherwise an implementation that
        // ignored one half would still pass.
        UInt128 baseline = new UInt128(0x0123456789ABCDEFul, 0xFEDCBA9876543210ul);

        if (!(baseline == new UInt128(0x0123456789ABCDEFul, 0xFEDCBA9876543210ul))) return 1;
        // Lower differs, upper agrees.
        if (baseline == new UInt128(0x0123456789ABCDEFul, 0xFEDCBA9876543211ul)) return 2;
        // Upper differs, lower agrees.
        if (baseline == new UInt128(0x0123456789ABCDEEul, 0xFEDCBA9876543210ul)) return 3;
        // Both differ.
        if (baseline == new UInt128(0x0123456789ABCDEEul, 0xFEDCBA9876543211ul)) return 4;
        return 0;
    }

    public static int TestHalvesAreNotInterchangeable()
    {
        // A implementation that stored both ctor arguments into the same field, or that
        // compared `_lower` against `_upper`, would satisfy every check above. These two
        // values are each other's halves swapped, so they agree under any such confusion.
        UInt128 upperOnly = new UInt128(1, 0);
        UInt128 lowerOnly = new UInt128(0, 1);
        if (upperOnly == lowerOnly) return 1;
        if (!(upperOnly == new UInt128(1, 0))) return 2;
        if (!(lowerOnly == new UInt128(0, 1))) return 3;
        return 0;
    }

    public static int TestWideningConversions()
    {
        // Every `op_Implicit` overload is `ldc.i4.0; conv.i8; <arg>; conv.u8; newobj .ctor`,
        // i.e. zero upper half and a zero-extended lower half. The `uint64` overload is the
        // one that skips the `conv.u8`. Each case here checks both halves, so a conversion
        // that sign-extended into the upper half, or that landed in the wrong half, fails.
        if (!((UInt128)(byte)0xABu == new UInt128(0, 0xABul))) return 1;
        if (!((UInt128)'A' == new UInt128(0, 65ul))) return 2;
        if (!((UInt128)(ushort)0xBEEFu == new UInt128(0, 0xBEEFul))) return 3;
        if (!((UInt128)0xDEADBEEFu == new UInt128(0, 0xDEADBEEFul))) return 4;
        if (!((UInt128)0xFEDCBA9876543210ul == new UInt128(0, 0xFEDCBA9876543210ul))) return 5;

        // `nuint` is pointer-width, so derive the expectation rather than assuming 64 bits.
        nuint native = unchecked((nuint)0x0123456789ABCDEFul);
        if (!((UInt128)native == new UInt128(0, (ulong)native))) return 6;

        // The widest value of each source type: `conv.u8` must zero-extend, so none of these
        // may set any bit of the upper half.
        if (!((UInt128)byte.MaxValue == new UInt128(0, byte.MaxValue))) return 7;
        if (!((UInt128)char.MaxValue == new UInt128(0, char.MaxValue))) return 8;
        if (!((UInt128)ushort.MaxValue == new UInt128(0, ushort.MaxValue))) return 9;
        if (!((UInt128)uint.MaxValue == new UInt128(0, uint.MaxValue))) return 10;
        if (!((UInt128)ulong.MaxValue == new UInt128(0, ulong.MaxValue))) return 11;
        if (!((UInt128)nuint.MaxValue == new UInt128(0, (ulong)nuint.MaxValue))) return 12;

        // A conversion that put its argument in the upper half would satisfy nothing above
        // for a nonzero value, but would satisfy all of it for zero; pin the nonzero case
        // negatively too.
        if ((UInt128)1u == new UInt128(1, 0)) return 13;
        return 0;
    }

    public static int TestInequality()
    {
        // op_Inequality is op_Equality's body with `ldc.i4.0; ceq` appended to negate the
        // result, so it must agree with negated equality on both outcomes and must still
        // consult both halves.
        UInt128 baseline = new UInt128(0x0123456789ABCDEFul, 0xFEDCBA9876543210ul);

        if (baseline != new UInt128(0x0123456789ABCDEFul, 0xFEDCBA9876543210ul)) return 1;
        // Lower differs, upper agrees.
        if (!(baseline != new UInt128(0x0123456789ABCDEFul, 0xFEDCBA9876543211ul))) return 2;
        // Upper differs, lower agrees.
        if (!(baseline != new UInt128(0x0123456789ABCDEEul, 0xFEDCBA9876543210ul))) return 3;
        if (default(UInt128) != new UInt128(0, 0)) return 4;
        if (!(UInt128.MinValue != UInt128.MaxValue)) return 5;
        // An op_Inequality that returned its argument comparison unnegated would pass every
        // check whose expected answer is "unequal"; the two `if (a != b) return` cases above
        // are the ones that pin the negation.
        if (UInt128.MaxValue != new UInt128(ulong.MaxValue, ulong.MaxValue)) return 6;
        return 0;
    }

    public static int TestEveryBitPosition()
    {
        // One value per set bit of the 128, in both halves. The comparison against zero is
        // the one that pins each bit individually: a value with only bit i set differs from
        // zero in exactly that bit, so a comparison ignoring bit i of that half reports a
        // match. The neighbour comparisons alone would not catch it — a pair differing in
        // bits i and i+1 still differs in bit i+1 once bit i is ignored — and neither would
        // `lowerBit == upperBit`, which stays unequal on the strength of the *other* half.
        for (int i = 0; i < 64; i++)
        {
            ulong bit = 1ul << i;
            ulong next = 1ul << ((i + 1) % 64);

            UInt128 lowerBit = new UInt128(0, bit);
            UInt128 upperBit = new UInt128(bit, 0);

            if (!(lowerBit == new UInt128(0, bit))) return 1;
            if (!(upperBit == new UInt128(bit, 0))) return 2;
            if (lowerBit == upperBit) return 3;
            if (lowerBit == new UInt128(0, next)) return 4;
            if (upperBit == new UInt128(next, 0)) return 5;
            if (lowerBit == default(UInt128)) return 6;
            if (upperBit == default(UInt128)) return 7;
        }

        return 0;
    }
}

class Program
{
    static int Main(string[] args)
    {
        // Failure codes stay inside a single byte: a process exit code is truncated to its
        // low 8 bits, and a code congruent to 0 would be indistinguishable from success.
        int result;

        result = UInt128EqualityTests.TestDefaultAndMinValue();
        if (result != 0) return result;

        result = UInt128EqualityTests.TestMaxValue();
        if (result != 0) return 10 + result;

        result = UInt128EqualityTests.TestBothHalvesAreCompared();
        if (result != 0) return 20 + result;

        result = UInt128EqualityTests.TestHalvesAreNotInterchangeable();
        if (result != 0) return 30 + result;

        result = UInt128EqualityTests.TestWideningConversions();
        if (result != 0) return 40 + result;

        result = UInt128EqualityTests.TestEveryBitPosition();
        if (result != 0) return 60 + result;

        result = UInt128EqualityTests.TestInequality();
        if (result != 0) return 70 + result;

        return 0;
    }
}

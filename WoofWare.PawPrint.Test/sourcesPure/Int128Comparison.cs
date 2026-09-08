using System;

public class Int128ComparisonTests
{
    // `Int128` carries a *type-level* [Intrinsic], so every member of it reaches PawPrint's
    // intrinsic dispatcher. Only the allowlisted members may appear here: `.ctor(ulong, ulong)`,
    // `op_Equality`, `op_Inequality`, `get_MinValue`, `get_MaxValue`, the widening `op_Implicit`
    // overloads, `op_Addition`, and now `op_LessThan` and `op_GreaterThan`. In particular
    // `op_LessThanOrEqual` and `op_GreaterThanOrEqual` are *not* allowlisted, so this file may
    // never write `<=` or `>=` between two `Int128`s; it spells those `!(a > b)` and `!(a < b)`.
    // (Comparisons between `int` loop counters are ordinary int32 comparisons and are fine.)
    //
    // Both operators have the same shape. op_LessThan is
    //   ldarg.0; ldfld _upper; ldarg.1; ldfld _upper; blt  TRUE
    //   ldarg.0; ldfld _upper; ldarg.1; ldfld _upper; bne.un FALSE
    //   ldarg.0; ldfld _lower; ldarg.1; ldfld _lower; clt.un; ret
    //   FALSE: ldc.i4.0; ret
    //   TRUE:  ldc.i4.1; ret
    // and op_GreaterThan is that with `bgt` and `cgt.un`. So the high halves are compared
    // *signed* -- they carry the sign of the whole 128-bit value in two's complement -- and the
    // low halves *unsigned*, and the low halves are consulted only when the high halves are equal.

    // Thirteen values in strictly ascending order, spanning both wrap points and both signs.
    // Reading them as (upper, lower) pairs: -2^127, -2^127+1, -2^65, -2, -1, 0, 1, 2^63-1, 2^63,
    // 2^64-1, 2^64, (2^63-1)*2^64, 2^127-1.
    private static Int128[] Ascending()
    {
        return new Int128[]
        {
            Int128.MinValue,
            new Int128(0x8000000000000000ul, 1ul),
            new Int128(0xFFFFFFFFFFFFFFFEul, 0ul),
            (Int128)(-2L),
            (Int128)(-1L),
            default(Int128),
            (Int128)1L,
            new Int128(0ul, 0x7FFFFFFFFFFFFFFFul),
            new Int128(0ul, 0x8000000000000000ul),
            new Int128(0ul, ulong.MaxValue),
            new Int128(1ul, 0ul),
            new Int128(0x7FFFFFFFFFFFFFFFul, 0ul),
            Int128.MaxValue,
        };
    }

    public static int TestUpperHalfIsSigned()
    {
        // The high halves are compared with `blt`/`bgt`, which are signed. Every pair here has
        // a negative value on one side and a non-negative one on the other, so an unsigned high
        // comparison reverses each of them: as raw bits a negative value's high half is the
        // *larger* one.
        if (!((Int128)(-1L) < (Int128)1L)) return 1;
        if ((Int128)(-1L) > (Int128)1L) return 2;
        if (!((Int128)1L > (Int128)(-1L))) return 3;
        if (!(Int128.MinValue < default(Int128))) return 4;
        if (!(default(Int128) > Int128.MinValue)) return 5;
        if (!(Int128.MinValue < Int128.MaxValue)) return 6;
        if (!(Int128.MaxValue > Int128.MinValue)) return 7;
        // Both negative, high halves differing: -2^65 is below -2. This one agrees under either
        // reading of the high half, so it is not what pins the signedness; it is here because the
        // differing-high-half path for two negatives is otherwise unexercised.
        if (!(new Int128(0xFFFFFFFFFFFFFFFEul, 0ul) < (Int128)(-2L))) return 8;
        return 0;
    }

    public static int TestLowerHalfIsUnsigned()
    {
        // When the high halves are equal the low halves decide, with `clt.un`/`cgt.un`. Each pair
        // here has low halves whose signed and unsigned orderings are opposite, so a signed low
        // comparison reverses it.
        //
        // High halves both zero: 2^64-1 is above 1, though as a signed int64 it is -1.
        if (!(new Int128(0ul, ulong.MaxValue) > new Int128(0ul, 1ul))) return 1;
        if (new Int128(0ul, ulong.MaxValue) < new Int128(0ul, 1ul)) return 2;
        // 2^63 is above 2^63-1, though as a signed int64 it is negative and the other is not.
        if (!(new Int128(0ul, 0x8000000000000000ul) > new Int128(0ul, 0x7FFFFFFFFFFFFFFFul)))
            return 3;
        if (!(new Int128(0ul, 0x7FFFFFFFFFFFFFFFul) < new Int128(0ul, 0x8000000000000000ul)))
            return 4;
        // High halves both nonzero and equal, so the comparison cannot be passing by both being
        // the default value.
        if (!(new Int128(7ul, ulong.MaxValue) > new Int128(7ul, 1ul))) return 5;
        // High halves equal and *negative*: the low half is still unsigned even when the value
        // as a whole is negative. -(2^64-1) is below -1, and their low halves are 1 and 2^64-1.
        if (!(new Int128(ulong.MaxValue, 1ul) < new Int128(ulong.MaxValue, ulong.MaxValue)))
            return 6;
        if (!(new Int128(ulong.MaxValue, ulong.MaxValue) > new Int128(ulong.MaxValue, 1ul)))
            return 7;
        return 0;
    }

    public static int TestUnequalUpperHalvesDecideAlone()
    {
        // The `bne.un` arm returns the constant `false` without consulting the low halves. These
        // pairs are the ones where doing so would give the opposite answer: the high half orders
        // them one way and the low half the other. An implementation that fell through to the
        // low comparison whenever the first branch was not taken fails every one of them.
        //
        // 2^64 > 2^64-1, but their low halves are 0 and 2^64-1.
        if (!(new Int128(1ul, 0ul) > new Int128(0ul, ulong.MaxValue))) return 1;
        if (new Int128(1ul, 0ul) < new Int128(0ul, ulong.MaxValue)) return 2;
        if (!(new Int128(0ul, ulong.MaxValue) < new Int128(1ul, 0ul))) return 3;
        if (new Int128(0ul, ulong.MaxValue) > new Int128(1ul, 0ul)) return 4;
        // The same shape with both high halves negative.
        if (!(new Int128(0xFFFFFFFFFFFFFFFFul, 0ul) > new Int128(0xFFFFFFFFFFFFFFFEul, ulong.MaxValue)))
            return 5;
        if (!(new Int128(0xFFFFFFFFFFFFFFFEul, ulong.MaxValue) < new Int128(0xFFFFFFFFFFFFFFFFul, 0ul)))
            return 6;
        return 0;
    }

    public static int TestIrreflexive()
    {
        // Neither operator may report a value as below or above itself. An implementation that
        // used `ble`/`bge` for the first branch, or `cle.un`/`cge.un` for the last, passes
        // everything above and fails here.
        Int128[] values = Ascending();

        for (int i = 0; i < values.Length; i++)
        {
            if (values[i] < values[i]) return 1;
            if (values[i] > values[i]) return 2;
            if (!(values[i] == values[i])) return 3;
        }

        return 0;
    }

    public static int TestAscendingOrder()
    {
        // The corpus is in strictly ascending order by construction -- the values are written out
        // as (upper, lower) pairs and the ordering is asserted from the arithmetic they denote,
        // not from anything the operators say. Checking every ordered pair rather than only
        // neighbours makes this transitive and total as well as pairwise, and pins mirroring:
        // `a < b` and `b > a` must agree.
        Int128[] values = Ascending();

        for (int i = 0; i < values.Length; i++)
        {
            for (int j = i + 1; j < values.Length; j++)
            {
                if (!(values[i] < values[j])) return 1;
                if (!(values[j] > values[i])) return 2;
                if (values[j] < values[i]) return 3;
                if (values[i] > values[j]) return 4;
                // Trichotomy: distinct values are neither equal nor unordered.
                if (values[i] == values[j]) return 5;
            }
        }

        return 0;
    }

    public static int TestAgreesWithAdditionAcrossTheWrap()
    {
        // Ordering and the already-allowlisted addition have to tell one consistent story about
        // where the low half's wrap point is: adding 1 to a value whose low half is all ones must
        // produce something the comparison also calls larger.
        Int128 justBelowWrap = new Int128(0ul, ulong.MaxValue);
        Int128 justAbove = justBelowWrap + new Int128(0ul, 1ul);

        if (!(justAbove > justBelowWrap)) return 1;
        if (!(justAbove == new Int128(1ul, 0ul))) return 2;
        // Adding 1 to Int128.MaxValue wraps to Int128.MinValue, so the sum is *below* the operand
        // -- the one place where addition and ordering are meant to disagree, and a check that
        // the comparison is not quietly saturating.
        if (!(Int128.MaxValue + new Int128(0ul, 1ul) < Int128.MaxValue)) return 3;
        if (!(Int128.MaxValue + new Int128(0ul, 1ul) == Int128.MinValue)) return 4;
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

        result = Int128ComparisonTests.TestUpperHalfIsSigned();
        if (result != 0) return result;

        result = Int128ComparisonTests.TestLowerHalfIsUnsigned();
        if (result != 0) return 10 + result;

        result = Int128ComparisonTests.TestUnequalUpperHalvesDecideAlone();
        if (result != 0) return 20 + result;

        result = Int128ComparisonTests.TestIrreflexive();
        if (result != 0) return 30 + result;

        result = Int128ComparisonTests.TestAscendingOrder();
        if (result != 0) return 40 + result;

        result = Int128ComparisonTests.TestAgreesWithAdditionAcrossTheWrap();
        if (result != 0) return 50 + result;

        return 0;
    }
}

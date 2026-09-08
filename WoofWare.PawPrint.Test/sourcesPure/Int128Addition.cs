using System;

public class Int128AdditionTests
{
    // `Int128` carries a *type-level* [Intrinsic], so every member of it reaches PawPrint's
    // intrinsic dispatcher. Only the allowlisted members may appear here: `.ctor(ulong, ulong)`,
    // `op_Equality`, `op_Inequality`, `get_MinValue`, `get_MaxValue`, the widening `op_Implicit`
    // overloads, and `op_Addition` itself. Nothing here may call `ToString`, `Equals`,
    // `GetHashCode`, the comparison operators, `get_Zero`/`get_One`, or the narrowing
    // `op_Explicit` conversions.
    //
    // op_Addition's body is
    //   ldarg.0; ldfld _lower; ldarg.1; ldfld _lower; add; stloc.0
    //   ldloc.0; ldarg.0; ldfld _lower; clt.un; conv.i8; stloc.1
    //   ldarg.0; ldfld _upper; ldarg.1; ldfld _upper; add; ldloc.1; add; ldloc.0; newobj .ctor
    // i.e. a wrapping 64-bit add of the low halves, a carry detected by the *unsigned* compare
    // `sum <u left._lower`, and a wrapping add of the high halves plus that carry.

    public static int TestZeroAndIdentity()
    {
        Int128 zero = default(Int128);
        Int128 value = new Int128(0x0123456789ABCDEFul, 0xFEDCBA9876543210ul);

        if (!(zero + zero == zero)) return 1;
        if (!(value + zero == value)) return 2;
        if (!(zero + value == value)) return 3;
        return 0;
    }

    public static int TestHalvesStayInTheirHalves()
    {
        // An addition that added a half into the wrong half, or that dropped one half, would
        // survive any check whose operands are zero in that half.
        if (!(new Int128(0, 1) + new Int128(0, 1) == new Int128(0, 2))) return 1;
        if (new Int128(0, 1) + new Int128(0, 1) == new Int128(2, 0)) return 2;
        if (!(new Int128(1, 0) + new Int128(1, 0) == new Int128(2, 0))) return 3;
        if (new Int128(1, 0) + new Int128(1, 0) == new Int128(0, 2)) return 4;
        // Both halves at once, with no carry between them.
        if (!(new Int128(3, 5) + new Int128(7, 11) == new Int128(10, 16))) return 5;
        return 0;
    }

    public static int TestCarryOutOfTheLowHalf()
    {
        // The whole point of the body: the low halves' sum wraps, and the 1 it lost has to
        // reappear in the high half. An implementation that added the halves independently
        // gets every case above right and every case here wrong.
        if (!(new Int128(0, ulong.MaxValue) + new Int128(0, 1) == new Int128(1, 0))) return 1;
        if (!(new Int128(0, 1) + new Int128(0, ulong.MaxValue) == new Int128(1, 0))) return 2;
        if (!(new Int128(0, ulong.MaxValue) + new Int128(0, ulong.MaxValue)
              == new Int128(1, ulong.MaxValue - 1))) return 3;
        // Carry into a high half that is itself already nonzero.
        if (!(new Int128(5, ulong.MaxValue) + new Int128(6, 3) == new Int128(12, 2))) return 4;
        // ... and the same sum with the carry suppressed, so a "always carry 1" bug fails too.
        if (!(new Int128(5, ulong.MaxValue - 3) + new Int128(6, 3) == new Int128(11, ulong.MaxValue)))
            return 5;
        return 0;
    }

    public static int TestCarryCompareIsUnsigned()
    {
        // `clt.un` compares the low sum against `left._lower` as *unsigned*. These pairs are the
        // minimal ones on which a signed `clt` disagrees -- the two compared values' sign bits
        // differ, so the two orderings are opposite -- one in each direction. Other cases in this
        // file happen to discriminate too, because their operands have the high bit set
        // incidentally; these are the ones that do it on purpose.
        //
        // No carry, but signed comparison says there is one: 1 + 2^63 does not wrap, yet the sum
        // read as a signed int64 is negative and so compares below 1.
        if (!(new Int128(0, 1) + new Int128(0, 0x8000000000000000ul)
              == new Int128(0, 0x8000000000000001ul))) return 1;
        // A carry, but signed comparison says there is none: 2^63 + 2^63 wraps to 0, and 0 read
        // signed is *above* 2^63 read signed.
        if (!(new Int128(0, 0x8000000000000000ul) + new Int128(0, 0x8000000000000000ul)
              == new Int128(1, 0))) return 2;
        // The same two shapes with a nonzero high half, so neither can pass by both sides being
        // zero there.
        if (!(new Int128(9, 1) + new Int128(4, 0x8000000000000000ul)
              == new Int128(13, 0x8000000000000001ul))) return 3;
        if (!(new Int128(9, 0x8000000000000000ul) + new Int128(4, 0x8000000000000000ul)
              == new Int128(14, 0))) return 4;
        return 0;
    }

    public static int TestHighHalfWrapsRatherThanFaulting()
    {
        // op_Addition is the unchecked operator (`op_CheckedAddition` is a separate method that
        // this file does not reach), so the high half's `add` wraps modulo 2^64 and the whole
        // operation wraps modulo 2^128. MaxValue + 1 is therefore MinValue, not an exception.
        if (!(Int128.MaxValue + new Int128(0, 1) == Int128.MinValue)) return 1;
        if (!(Int128.MinValue + new Int128(ulong.MaxValue, ulong.MaxValue) == Int128.MaxValue)) return 2;
        // (-1) + 1 == 0: the carry out of the low half propagates into a high half that is itself
        // all ones, so the high add wraps to zero as well. This is the one case where both wraps
        // have to happen for the answer to come out right.
        if (!(new Int128(ulong.MaxValue, ulong.MaxValue) + new Int128(0, 1) == default(Int128)))
            return 3;
        if (!(new Int128(ulong.MaxValue, ulong.MaxValue) + new Int128(ulong.MaxValue, ulong.MaxValue)
              == new Int128(ulong.MaxValue, ulong.MaxValue - 1))) return 4;
        return 0;
    }

    public static int TestSignedOperands()
    {
        // Reached through the widening conversions, whose signed overloads broadcast the sign bit
        // across the high half. Addition itself is sign-agnostic, so these check that the two
        // features compose rather than that addition does anything new.
        Int128 minusOne = (Int128)(-1L);
        Int128 one = (Int128)1L;

        if (!(minusOne + one == default(Int128))) return 1;
        if (!(minusOne + minusOne == (Int128)(-2L))) return 2;
        // long.MaxValue + 1 crosses the low half's signed boundary without wrapping it, so this
        // is the `clt.un` case again, reached through the signed conversion rather than spelled
        // out as halves.
        if (!((Int128)long.MaxValue + one == new Int128(0, 0x8000000000000000ul))) return 3;
        if (!((Int128)(-5L) + (Int128)3L == (Int128)(-2L))) return 4;
        if (!((Int128)5L + (Int128)(-3L) == (Int128)2L)) return 5;
        // A signed operand's high half is all ones, so adding two of them exercises a high-half
        // wrap that the unsigned spellings above reach only at MaxValue.
        if (!((Int128)(-1L) + (Int128)(-1L) == new Int128(ulong.MaxValue, ulong.MaxValue - 1))) return 6;
        return 0;
    }

    public static int TestEveryBitPosition()
    {
        // Doubling a one-bit value moves that bit up one place, so this walks a single carry all
        // the way along the 128-bit chain — including across the seam between the two halves,
        // which is the only place the `clt.un` result matters, and off the top, where the result
        // is zero.
        for (int i = 0; i < 64; i++)
        {
            ulong bit = 1ul << i;

            Int128 lowerBit = new Int128(0, bit);
            Int128 upperBit = new Int128(bit, 0);

            if (i < 63)
            {
                if (!(lowerBit + lowerBit == new Int128(0, bit << 1))) return 1;
                if (!(upperBit + upperBit == new Int128(bit << 1, 0))) return 2;
            }
            else
            {
                // The top bit of the low half carries into the bottom bit of the high half.
                if (!(lowerBit + lowerBit == new Int128(1, 0))) return 3;
                // The top bit of the whole value carries off the end and is lost.
                if (!(upperBit + upperBit == default(Int128))) return 4;
            }

            // Adding a bit to the value that is all of the *other* bits of that half gives that
            // half all ones with no carry, which pins that the carry is not raised spuriously.
            if (!(lowerBit + new Int128(0, ~bit) == new Int128(0, ulong.MaxValue))) return 5;
            if (!(upperBit + new Int128(~bit, 0) == new Int128(ulong.MaxValue, 0))) return 6;
        }

        return 0;
    }

    public static int TestCommutativeAndAssociative()
    {
        // A small corpus chosen to straddle both wrap points, checked for the two laws the
        // operation has. Neither law is what the body could plausibly get wrong on its own, but
        // together they rule out an implementation that treats its two arguments differently —
        // the body reads `left._lower` twice and `right._lower` once, so an argument mix-up is a
        // live failure mode.
        ulong[] halves = new ulong[]
        {
            0ul, 1ul, 2ul, 0x7FFFFFFFFFFFFFFFul, 0x8000000000000000ul,
            0xFFFFFFFFFFFFFFFEul, ulong.MaxValue, 0x0123456789ABCDEFul,
        };

        for (int i = 0; i < halves.Length; i++)
        {
            for (int j = 0; j < halves.Length; j++)
            {
                Int128 a = new Int128(halves[i], halves[j]);
                Int128 b = new Int128(halves[j], halves[i]);
                Int128 c = new Int128(halves[(i + 1) % halves.Length], halves[j]);

                if (!(a + b == b + a)) return 1;
                if (!(a + c == c + a)) return 2;
                if (!((a + b) + c == a + (b + c))) return 3;
                // x + (-x) == 0, with -x spelled as its two's complement so no operator beyond
                // addition is needed: ~x + 1.
                Int128 negA = new Int128(~halves[i], ~halves[j]) + new Int128(0, 1);
                if (!(a + negA == default(Int128))) return 4;
            }
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

        result = Int128AdditionTests.TestZeroAndIdentity();
        if (result != 0) return result;

        result = Int128AdditionTests.TestHalvesStayInTheirHalves();
        if (result != 0) return 10 + result;

        result = Int128AdditionTests.TestCarryOutOfTheLowHalf();
        if (result != 0) return 20 + result;

        result = Int128AdditionTests.TestCarryCompareIsUnsigned();
        if (result != 0) return 30 + result;

        result = Int128AdditionTests.TestHighHalfWrapsRatherThanFaulting();
        if (result != 0) return 40 + result;

        result = Int128AdditionTests.TestSignedOperands();
        if (result != 0) return 50 + result;

        result = Int128AdditionTests.TestEveryBitPosition();
        if (result != 0) return 60 + result;

        result = Int128AdditionTests.TestCommutativeAndAssociative();
        if (result != 0) return 70 + result;

        return 0;
    }
}

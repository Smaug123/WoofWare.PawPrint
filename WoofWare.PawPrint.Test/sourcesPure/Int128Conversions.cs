using System;

public class Int128ConversionsTests
{
    // `Int128` carries a *type-level* [Intrinsic], so every member of it reaches PawPrint's
    // intrinsic dispatcher. Only the members this file exercises are allowlisted, so nothing
    // here may call `ToString`, `Equals`, `GetHashCode`, arithmetic, the comparison
    // operators, or the explicit narrowing conversions.
    //
    // The subject is the eleven widening `op_Implicit` overloads, which come in two body
    // shapes. The six from unsigned sources are `ldc.i4.0; conv.i8; <arg>; conv.u8; newobj`
    // — a zero upper half. The five from signed sources instead sign-extend the argument to
    // `int64` and broadcast its sign bit across the upper half with an arithmetic
    // `ldc.i4.s 63; shr`. Getting that shift wrong (logical instead of arithmetic) is the
    // failure this file is built to catch, so every signed case below is paired with the
    // unsigned source of the same width whose all-ones value has the identical bit pattern:
    // the two must not produce the same `Int128`.

    public static int TestDefaultAndZero()
    {
        Int128 a = default;
        Int128 b = default;
        if (!(a == b)) return 1;
        if (!(a == (Int128)0)) return 2;
        if (!(new Int128(0, 0) == default(Int128))) return 3;
        // Unlike `UInt128`, `Int128.MinValue` is *not* the default value: it is the sign bit
        // alone. A `get_MinValue` that returned zero would pass the UInt128-shaped checks.
        if (Int128.MinValue == default(Int128)) return 4;
        if (new Int128(0, 1) == default(Int128)) return 5;
        return 0;
    }

    public static int TestMinMaxValue()
    {
        // MinValue is `ldc.i8 long.MinValue` for the upper half and `ldc.i4.0; conv.i8` for
        // the lower: the sign bit set and nothing else.
        if (!(Int128.MinValue == new Int128(0x8000_0000_0000_0000ul, 0ul))) return 1;
        // MaxValue is `ldc.i8 long.MaxValue` upper, `ldc.i4.m1; conv.i8` lower: every bit
        // except the sign bit.
        if (!(Int128.MaxValue == new Int128(0x7FFF_FFFF_FFFF_FFFFul, ulong.MaxValue))) return 2;

        if (Int128.MinValue == Int128.MaxValue) return 3;
        // Each of these catches a constant that filled only one half, or that filled both
        // halves with the same word.
        if (Int128.MinValue == new Int128(0x8000_0000_0000_0000ul, ulong.MaxValue)) return 4;
        if (Int128.MaxValue == new Int128(0x7FFF_FFFF_FFFF_FFFFul, 0ul)) return 5;
        if (Int128.MaxValue == new Int128(ulong.MaxValue, ulong.MaxValue)) return 6;
        // MinValue and MaxValue differ in every bit, so a comparison that ignored either
        // half would still separate them; pin the halves against handmade values instead.
        if (Int128.MinValue == new Int128(0ul, 0x8000_0000_0000_0000ul)) return 7;
        return 0;
    }

    public static int TestBothHalvesAreCompared()
    {
        // op_Equality branches on `_lower` first and only then compares `_upper`, so each
        // half needs a case where it alone differs.
        Int128 baseline = new Int128(0x0123456789ABCDEFul, 0xFEDCBA9876543210ul);

        if (!(baseline == new Int128(0x0123456789ABCDEFul, 0xFEDCBA9876543210ul))) return 1;
        if (baseline == new Int128(0x0123456789ABCDEFul, 0xFEDCBA9876543211ul)) return 2;
        if (baseline == new Int128(0x0123456789ABCDEEul, 0xFEDCBA9876543210ul)) return 3;
        if (baseline == new Int128(0x0123456789ABCDEEul, 0xFEDCBA9876543211ul)) return 4;
        return 0;
    }

    public static int TestHalvesAreNotInterchangeable()
    {
        // These two values are each other's halves swapped, so they agree under any
        // implementation that stored both ctor arguments into one field or that compared
        // `_lower` against `_upper`.
        Int128 upperOnly = new Int128(1ul, 0ul);
        Int128 lowerOnly = new Int128(0ul, 1ul);
        if (upperOnly == lowerOnly) return 1;
        if (!(upperOnly == new Int128(1ul, 0ul))) return 2;
        if (!(lowerOnly == new Int128(0ul, 1ul))) return 3;
        return 0;
    }

    public static int TestZeroExtendingConversions()
    {
        // The six overloads whose source type is unsigned: upper half zero, lower half
        // zero-extended. Identical body shape to UInt128's.
        if (!((Int128)(byte)0xABu == new Int128(0ul, 0xABul))) return 1;
        if (!((Int128)'A' == new Int128(0ul, 65ul))) return 2;
        if (!((Int128)(ushort)0xBEEFu == new Int128(0ul, 0xBEEFul))) return 3;
        if (!((Int128)0xDEADBEEFu == new Int128(0ul, 0xDEADBEEFul))) return 4;
        if (!((Int128)0xFEDCBA9876543210ul == new Int128(0ul, 0xFEDCBA9876543210ul))) return 5;

        nuint native = unchecked((nuint)0x0123456789ABCDEFul);
        if (!((Int128)native == new Int128(0ul, (ulong)native))) return 6;

        // The widest value of each unsigned source: `conv.u8` must zero-extend, so none of
        // these may set any bit of the upper half. `ulong.MaxValue` is the important one --
        // it has the bit pattern of -1, and must still widen to a *positive* Int128.
        if (!((Int128)byte.MaxValue == new Int128(0ul, byte.MaxValue))) return 7;
        if (!((Int128)char.MaxValue == new Int128(0ul, char.MaxValue))) return 8;
        if (!((Int128)ushort.MaxValue == new Int128(0ul, ushort.MaxValue))) return 9;
        if (!((Int128)uint.MaxValue == new Int128(0ul, uint.MaxValue))) return 10;
        if (!((Int128)ulong.MaxValue == new Int128(0ul, ulong.MaxValue))) return 11;
        if (!((Int128)nuint.MaxValue == new Int128(0ul, (ulong)nuint.MaxValue))) return 12;

        // A conversion that put its argument in the upper half satisfies nothing above for a
        // nonzero value, but satisfies all of it for zero; pin the nonzero case negatively.
        if ((Int128)1u == new Int128(1ul, 0ul)) return 13;
        return 0;
    }

    public static int TestSignExtendingConversions()
    {
        ulong ones = ulong.MaxValue;

        // -1 from every signed source: the whole 128 bits must be set.
        if (!((Int128)(sbyte)(-1) == new Int128(ones, ones))) return 1;
        if (!((Int128)(short)(-1) == new Int128(ones, ones))) return 2;
        if (!((Int128)(-1) == new Int128(ones, ones))) return 3;
        if (!((Int128)(-1L) == new Int128(ones, ones))) return 4;
        if (!((Int128)(nint)(-1) == new Int128(ones, ones))) return 5;

        // The paired discriminators. Each unsigned source below has the same bit pattern as
        // the signed -1 above it, so a conversion that zero-extended the signed source (a
        // logical `shr` instead of an arithmetic one) would make these equal.
        if ((Int128)(sbyte)(-1) == (Int128)byte.MaxValue) return 6;
        if ((Int128)(short)(-1) == (Int128)ushort.MaxValue) return 7;
        if ((Int128)(-1) == (Int128)uint.MaxValue) return 8;
        if ((Int128)(-1L) == (Int128)ulong.MaxValue) return 9;
        if ((Int128)(nint)(-1) == (Int128)nuint.MaxValue) return 10;

        // Positive values from signed sources must leave the upper half clear — an
        // implementation that unconditionally filled it would pass every check above.
        if (!((Int128)(sbyte)1 == new Int128(0ul, 1ul))) return 11;
        if (!((Int128)(short)0x1234 == new Int128(0ul, 0x1234ul))) return 12;
        if (!((Int128)0x12345678 == new Int128(0ul, 0x12345678ul))) return 13;
        if (!((Int128)0x1234567890ABCDEFL == new Int128(0ul, 0x1234567890ABCDEFul))) return 14;
        if (!((Int128)(nint)0x1234 == new Int128(0ul, 0x1234ul))) return 15;

        // The extreme of each signed source. `sbyte.MinValue` etc. exercise the `conv.i8`
        // that widens the argument to int64 *before* the shift: the lower half must be the
        // sign-extended 64-bit value, not the raw narrow bit pattern zero-extended.
        if (!((Int128)sbyte.MinValue == new Int128(ones, 0xFFFF_FFFF_FFFF_FF80ul))) return 16;
        if (!((Int128)short.MinValue == new Int128(ones, 0xFFFF_FFFF_FFFF_8000ul))) return 17;
        if (!((Int128)int.MinValue == new Int128(ones, 0xFFFF_FFFF_8000_0000ul))) return 18;
        if (!((Int128)long.MinValue == new Int128(ones, 0x8000_0000_0000_0000ul))) return 19;

        if (!((Int128)sbyte.MaxValue == new Int128(0ul, 0x7Ful))) return 20;
        if (!((Int128)short.MaxValue == new Int128(0ul, 0x7FFFul))) return 21;
        if (!((Int128)int.MaxValue == new Int128(0ul, 0x7FFF_FFFFul))) return 22;
        if (!((Int128)long.MaxValue == new Int128(0ul, 0x7FFF_FFFF_FFFF_FFFFul))) return 23;

        // `long.MinValue` widened must be Int128.MinValue shifted right, not Int128.MinValue
        // itself: the sign bit of a 64-bit value does not land on bit 127.
        if ((Int128)long.MinValue == Int128.MinValue) return 24;
        // ...and the largest signed 64-bit value is nowhere near Int128.MaxValue.
        if ((Int128)long.MaxValue == Int128.MaxValue) return 25;

        // `nint` is pointer-width, so `-1` is the only value whose widening is width-agnostic
        // in closed form; derive the rest from the platform.
        nint negNative = unchecked((nint)(-0x1234L));
        if (!((Int128)negNative == new Int128(ones, unchecked((ulong)(long)negNative)))) return 26;
        return 0;
    }

    public static int TestSignExtensionSweep()
    {
        // The upper half of a widened signed value is determined entirely by the source's
        // sign: all ones when negative, all zeroes when not. Sweep both directions across
        // every bit position rather than trusting the handful of constants above.
        for (int i = 0; i < 64; i++)
        {
            // Every bit from `i` upwards set: negative for all i in [0, 64).
            long negative = -1L << i;
            if (!((Int128)negative == new Int128(ulong.MaxValue, unchecked((ulong)negative)))) return 1;
            if ((Int128)negative == new Int128(0ul, unchecked((ulong)negative))) return 2;

            // Every bit from `i` downwards of long.MaxValue: non-negative for all i.
            long positive = long.MaxValue >> i;
            if (!((Int128)positive == new Int128(0ul, (ulong)positive))) return 3;
            if ((Int128)positive == new Int128(ulong.MaxValue, (ulong)positive)) return 4;

            // The same 64 bits reached through the unsigned overload must always land in the
            // lower half with a clear upper half, whatever the top bit says.
            ulong raw = unchecked((ulong)negative);
            if (!((Int128)raw == new Int128(0ul, raw))) return 5;
            // ...which is a different value from the signed widening whenever i > 0 leaves
            // the top bit set — i.e. always, since `-1L << i` is negative throughout.
            if ((Int128)raw == (Int128)negative) return 6;

            // 32-bit sources, to exercise the `conv.i8` ahead of the shift at every width.
            int narrow = -1 << (i % 32);
            if (!((Int128)narrow == new Int128(ulong.MaxValue, unchecked((ulong)(long)narrow)))) return 7;
        }

        return 0;
    }

    public static int TestInequality()
    {
        // op_Inequality is op_Equality's body with `ldc.i4.0; ceq` appended, so it must
        // agree with negated equality on both outcomes and on both halves.
        Int128 baseline = new Int128(0x0123456789ABCDEFul, 0xFEDCBA9876543210ul);

        if (baseline != new Int128(0x0123456789ABCDEFul, 0xFEDCBA9876543210ul)) return 1;
        if (!(baseline != new Int128(0x0123456789ABCDEFul, 0xFEDCBA9876543211ul))) return 2;
        if (!(baseline != new Int128(0x0123456789ABCDEEul, 0xFEDCBA9876543210ul))) return 3;
        if (default(Int128) != new Int128(0ul, 0ul)) return 4;
        if (!(Int128.MinValue != Int128.MaxValue)) return 5;
        if (!((Int128)(-1L) != (Int128)ulong.MaxValue)) return 6;
        return 0;
    }
}

class Program
{
    static int Main(string[] args)
    {
        // Failure codes stay inside a single byte: a process exit code is truncated to its
        // low 8 bits, and a code congruent to 0 would be indistinguishable from success.
        // The largest reachable code is 160 + 26 = 186.
        int result;

        result = Int128ConversionsTests.TestDefaultAndZero();
        if (result != 0) return result;

        result = Int128ConversionsTests.TestMinMaxValue();
        if (result != 0) return 10 + result;

        result = Int128ConversionsTests.TestBothHalvesAreCompared();
        if (result != 0) return 20 + result;

        result = Int128ConversionsTests.TestHalvesAreNotInterchangeable();
        if (result != 0) return 30 + result;

        result = Int128ConversionsTests.TestZeroExtendingConversions();
        if (result != 0) return 40 + result;

        result = Int128ConversionsTests.TestSignExtensionSweep();
        if (result != 0) return 100 + result;

        result = Int128ConversionsTests.TestInequality();
        if (result != 0) return 120 + result;

        result = Int128ConversionsTests.TestSignExtendingConversions();
        if (result != 0) return 160 + result;

        return 0;
    }
}

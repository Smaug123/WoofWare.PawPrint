using System;
using System.Numerics;

public class BitOperationsPopCountTests
{
    // Reference implementation: count set bits one at a time, using nothing but the
    // shift and mask primitives the interpreter has supported since long before
    // BitOperations existed. Every BitOperations.PopCount answer below is checked
    // against this rather than against a hand-computed constant.
    private static int NaivePopCount(ulong value)
    {
        int count = 0;
        for (int i = 0; i < 64; i++)
        {
            if (((value >> i) & 1ul) != 0ul) count++;
        }
        return count;
    }

    // A deterministic 64-bit xorshift, so the sweep below covers a wide spread of bit
    // patterns without the guest needing a random source.
    private static ulong NextXorShift(ulong state)
    {
        state ^= state << 13;
        state ^= state >> 7;
        state ^= state << 17;
        return state;
    }

    // Failure codes are globally unique and all below 256, because a guest's exit code is
    // only eight bits wide: an offset scheme that wrapped past 255 could report 0.
    public static int TestUInt32Constants()
    {
        if (BitOperations.PopCount(0u) != 0) return 1;
        if (BitOperations.PopCount(1u) != 1) return 2;
        if (BitOperations.PopCount(2u) != 1) return 3;
        if (BitOperations.PopCount(3u) != 2) return 4;
        if (BitOperations.PopCount(7u) != 3) return 5;
        if (BitOperations.PopCount(8u) != 1) return 6;
        if (BitOperations.PopCount(255u) != 8) return 7;
        if (BitOperations.PopCount(256u) != 1) return 8;
        if (BitOperations.PopCount(0x55555555u) != 16) return 9;
        if (BitOperations.PopCount(0xAAAAAAAAu) != 16) return 10;
        if (BitOperations.PopCount(0x0F0F0F0Fu) != 16) return 11;
        if (BitOperations.PopCount(0x80000000u) != 1) return 12;
        if (BitOperations.PopCount(0x7FFFFFFFu) != 31) return 13;
        if (BitOperations.PopCount(uint.MaxValue) != 32) return 14;
        return 0;
    }

    public static int TestUInt32SingleBits()
    {
        for (int bit = 0; bit < 32; bit++)
        {
            uint pow = 1u << bit;
            if (BitOperations.PopCount(pow) != 1) return 21 + bit;
            // A run of `bit` low bits has exactly `bit` of them set.
            if (BitOperations.PopCount(pow - 1u) != bit) return 21 + bit;
        }
        return 0;
    }

    public static int TestUInt64Constants()
    {
        if (BitOperations.PopCount(0ul) != 0) return 53;
        if (BitOperations.PopCount(1ul) != 1) return 54;
        if (BitOperations.PopCount(3ul) != 2) return 55;
        if (BitOperations.PopCount(0xFFFFFFFFul) != 32) return 56;
        if (BitOperations.PopCount(0x100000000ul) != 1) return 57;
        if (BitOperations.PopCount(0x5555555555555555ul) != 32) return 58;
        if (BitOperations.PopCount(0xAAAAAAAAAAAAAAAAul) != 32) return 59;
        if (BitOperations.PopCount(0x0F0F0F0F0F0F0F0Ful) != 32) return 60;
        if (BitOperations.PopCount(0x8000000000000000ul) != 1) return 61;
        if (BitOperations.PopCount(0x7FFFFFFFFFFFFFFFul) != 63) return 62;
        if (BitOperations.PopCount(ulong.MaxValue) != 64) return 63;
        // The 64-bit software fallback multiplies by 0x0101010101010101 and shifts right
        // by 56; a value whose byte-wise partial sums overflow into neighbouring bytes
        // would break that. All bits set is the extreme case, checked above; this checks
        // the high half in isolation, which the 32-bit path can never see.
        if (BitOperations.PopCount(0xFFFFFFFF00000000ul) != 32) return 64;
        return 0;
    }

    public static int TestUInt64SingleBits()
    {
        for (int bit = 0; bit < 64; bit++)
        {
            ulong pow = 1ul << bit;
            if (BitOperations.PopCount(pow) != 1) return 71 + bit;
            if (BitOperations.PopCount(pow - 1ul) != bit) return 71 + bit;
        }
        return 0;
    }

    private struct ZeroNuintHolder { public nuint Value; }

    public static int TestNUInt()
    {
        int width = IntPtr.Size * 8;

        if (BitOperations.PopCount((nuint)0) != 0) return 135;
        if (BitOperations.PopCount((nuint)1) != 1) return 136;
        if (BitOperations.PopCount((nuint)255) != 8) return 137;
        if (BitOperations.PopCount((nuint)256) != 1) return 138;
        if (BitOperations.PopCount(nuint.MaxValue) != width) return 139;

        // Default-initialised nuint values can arrive on PawPrint's eval stack as
        // NativeInt(ManagedPointerSource.Null), not Verbatim 0; check both shapes.
        if (BitOperations.PopCount(default(nuint)) != 0) return 140;
        var zeroHolder = new ZeroNuintHolder();
        if (BitOperations.PopCount(zeroHolder.Value) != 0) return 141;

        return 0;
    }

    public static int TestThroughIBinaryIntegerWrappers()
    {
        // The wrappers Int32.PopCount / UInt32.PopCount / Int64.PopCount / UInt64.PopCount /
        // IntPtr.PopCount / UIntPtr.PopCount are themselves [Intrinsic] in CoreLib, but their
        // IL bodies just forward to BitOperations.PopCount. Exercise them directly, because
        // they are separate intrinsic keys from the BitOperations methods they call.
        int width = IntPtr.Size * 8;

        if (int.PopCount(0) != 0) return 146;
        if (int.PopCount(1) != 1) return 147;
        if (int.PopCount(-1) != 32) return 148;
        if (int.PopCount(int.MinValue) != 1) return 149;
        if (int.PopCount(int.MaxValue) != 31) return 150;

        if (uint.PopCount(0u) != 0u) return 151;
        if (uint.PopCount(uint.MaxValue) != 32u) return 152;

        if (long.PopCount(0L) != 0L) return 153;
        if (long.PopCount(-1L) != 64L) return 154;
        if (long.PopCount(long.MinValue) != 1L) return 155;
        if (long.PopCount(long.MaxValue) != 63L) return 156;

        if (ulong.PopCount(0ul) != 0ul) return 157;
        if (ulong.PopCount(ulong.MaxValue) != 64ul) return 158;

        if (nint.PopCount((nint)0) != (nint)0) return 159;
        if (nint.PopCount((nint)(-1)) != (nint)width) return 160;
        if (nint.PopCount((nint)256) != (nint)1) return 161;

        if (nuint.PopCount((nuint)0) != (nuint)0) return 162;
        if (nuint.PopCount(nuint.MaxValue) != (nuint)width) return 163;

        return 0;
    }

    public static int TestUInt128Wrapper()
    {
        // UInt128's PopCount carries no method-level [Intrinsic], but its declaring type does,
        // and a type-level marker routes every member -- so this needs its own allowlist entry.
        // Its body is ulong.PopCount(_lower) + ulong.PopCount(_upper).
        //
        // There is deliberately no Int128 counterpart: a guest cannot construct an Int128 at all
        // (Int128.op_Implicit is itself unimplemented), so nothing here could reach it.
        // Conversions are spelled from `ulong` so they bind to the widening op_Implicit that
        // PR #1132 allowlisted; `(UInt128)0` would instead emit op_Explicit(Int32), which is
        // not part of that cluster. Comparisons are spelled `!(a == b)` for the same reason:
        // that cluster has op_Equality but no op_Inequality.
        if (!(UInt128.PopCount((UInt128)0ul) == (UInt128)0ul)) return 164;
        if (!(UInt128.PopCount((UInt128)1ul) == (UInt128)1ul)) return 165;
        if (!(UInt128.PopCount((UInt128)ulong.MaxValue) == (UInt128)64ul)) return 166;
        if (!(UInt128.PopCount(UInt128.MaxValue) == (UInt128)128ul)) return 167;

        // A value with bits in both halves, built through the public (upper, lower) ctor: the
        // upper half is what the `_upper` term contributes, so a body that read only `_lower`
        // would answer 8 rather than 16.
        UInt128 both = new UInt128(0xFFul, 0xFFul);
        if (!(UInt128.PopCount(both) == (UInt128)16ul)) return 168;

        // Only the upper half is set, so a body that read only `_lower` would answer 0.
        UInt128 upperOnly = new UInt128(0xFFul, 0ul);
        if (!(UInt128.PopCount(upperOnly) == (UInt128)8ul)) return 169;

        return 0;
    }

    public static int TestAgainstNaiveOracle()
    {
        ulong state = 0x9E3779B97F4A7C15ul;

        for (int i = 0; i < 200; i++)
        {
            state = NextXorShift(state);

            if (BitOperations.PopCount(state) != NaivePopCount(state)) return 171;

            uint low = (uint)state;
            if (BitOperations.PopCount(low) != NaivePopCount(low)) return 172;

            // PopCount is additive over a partition of the bits: splitting a 64-bit value
            // into halves must give the same total as counting it whole.
            uint high = (uint)(state >> 32);
            if (BitOperations.PopCount(low) + BitOperations.PopCount(high) != BitOperations.PopCount(state)) return 173;

            nuint asNative = (nuint)state;
            if (BitOperations.PopCount(asNative) != NaivePopCount(asNative)) return 174;

            // Complementing a value must complement its population count.
            if (BitOperations.PopCount(state) + BitOperations.PopCount(~state) != 64) return 175;
            if (BitOperations.PopCount(low) + BitOperations.PopCount(~low) != 32) return 176;
        }

        return 0;
    }
}

class Program
{
    static int Main(string[] args)
    {
        int result;

        result = BitOperationsPopCountTests.TestUInt32Constants();
        if (result != 0) return result;

        result = BitOperationsPopCountTests.TestUInt32SingleBits();
        if (result != 0) return result;

        result = BitOperationsPopCountTests.TestUInt64Constants();
        if (result != 0) return result;

        result = BitOperationsPopCountTests.TestUInt64SingleBits();
        if (result != 0) return result;

        result = BitOperationsPopCountTests.TestNUInt();
        if (result != 0) return result;

        result = BitOperationsPopCountTests.TestThroughIBinaryIntegerWrappers();
        if (result != 0) return result;

        result = BitOperationsPopCountTests.TestUInt128Wrapper();
        if (result != 0) return result;

        result = BitOperationsPopCountTests.TestAgainstNaiveOracle();
        if (result != 0) return result;

        return 0;
    }
}

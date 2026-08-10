using System;
using System.Numerics;

public class BitOperationsLeadingZeroCountTests
{
    public static int TestUInt32()
    {
        if (BitOperations.LeadingZeroCount(0u) != 32) return 1;
        if (BitOperations.LeadingZeroCount(1u) != 31) return 2;
        if (BitOperations.LeadingZeroCount(2u) != 30) return 3;
        if (BitOperations.LeadingZeroCount(3u) != 30) return 4;
        if (BitOperations.LeadingZeroCount(0xFFu) != 24) return 5;
        if (BitOperations.LeadingZeroCount(0x100u) != 23) return 6;
        if (BitOperations.LeadingZeroCount(0x7FFFFFFFu) != 1) return 7;
        if (BitOperations.LeadingZeroCount(0x80000000u) != 0) return 8;
        if (BitOperations.LeadingZeroCount(uint.MaxValue) != 0) return 9;

        for (int bit = 0; bit < 32; bit++)
        {
            uint pow = 1u << bit;
            // A single set bit at index `bit` leaves 31 - bit zeros above it.
            if (BitOperations.LeadingZeroCount(pow) != 31 - bit) return 10;
            // Setting every bit below it does not move the highest set bit.
            if (BitOperations.LeadingZeroCount(pow | (pow - 1u)) != 31 - bit) return 11;
            // Clearing it moves the highest set bit down one place; at bit 0 the value
            // becomes zero, and the documented answer there is the full width, 32.
            if (BitOperations.LeadingZeroCount(pow - 1u) != 32 - bit) return 12;
        }

        return 0;
    }

    public static int TestUInt64()
    {
        if (BitOperations.LeadingZeroCount(0ul) != 64) return 1;
        if (BitOperations.LeadingZeroCount(1ul) != 63) return 2;
        if (BitOperations.LeadingZeroCount(2ul) != 62) return 3;
        if (BitOperations.LeadingZeroCount(3ul) != 62) return 4;
        if (BitOperations.LeadingZeroCount(0xFFFFFFFFul) != 32) return 5;
        if (BitOperations.LeadingZeroCount(0x100000000ul) != 31) return 6;
        if (BitOperations.LeadingZeroCount(0x7FFFFFFFFFFFFFFFul) != 1) return 7;
        if (BitOperations.LeadingZeroCount(0x8000000000000000ul) != 0) return 8;
        if (BitOperations.LeadingZeroCount(ulong.MaxValue) != 0) return 9;

        for (int bit = 0; bit < 64; bit++)
        {
            ulong pow = 1ul << bit;
            if (BitOperations.LeadingZeroCount(pow) != 63 - bit) return 10;
            if (BitOperations.LeadingZeroCount(pow | (pow - 1ul)) != 63 - bit) return 11;
            if (BitOperations.LeadingZeroCount(pow - 1ul) != 64 - bit) return 12;
        }

        return 0;
    }

    private struct ZeroNuintHolder { public nuint Value; }

    public static int TestNUInt()
    {
        // nuint is 32 bits wide on a 32-bit platform and 64 on a 64-bit one, and the answer
        // depends on that width throughout, so derive it rather than hard-coding 64.
        int width = IntPtr.Size * 8;

        if (BitOperations.LeadingZeroCount((nuint)0) != width) return 1;
        if (BitOperations.LeadingZeroCount((nuint)1) != width - 1) return 2;
        if (BitOperations.LeadingZeroCount((nuint)2) != width - 2) return 3;
        if (BitOperations.LeadingZeroCount((nuint)3) != width - 2) return 4;
        if (BitOperations.LeadingZeroCount((nuint)255) != width - 8) return 5;
        if (BitOperations.LeadingZeroCount((nuint)256) != width - 9) return 6;
        if (BitOperations.LeadingZeroCount(nuint.MaxValue) != 0) return 7;

        // Default-initialised nuint values can arrive on PawPrint's eval stack as
        // NativeInt(ManagedPointerSource.Null), not Verbatim 0; check both shapes, since
        // zero is exactly the input whose answer is the full width.
        if (BitOperations.LeadingZeroCount(default(nuint)) != width) return 8;
        var zeroHolder = new ZeroNuintHolder();
        if (BitOperations.LeadingZeroCount(zeroHolder.Value) != width) return 9;

        for (int bit = 0; bit < width; bit++)
        {
            nuint pow = (nuint)1 << bit;
            if (BitOperations.LeadingZeroCount(pow) != width - 1 - bit) return 10;
            if (BitOperations.LeadingZeroCount(pow | (pow - 1)) != width - 1 - bit) return 11;
            if (BitOperations.LeadingZeroCount(pow - 1) != width - bit) return 12;
        }

        return 0;
    }

    public static int TestThroughIBinaryIntegerWrappers()
    {
        // The wrappers UInt32/UInt64/UIntPtr/Int32/Int64/IntPtr.LeadingZeroCount are
        // themselves [Intrinsic] in CoreLib, but their IL bodies are simple
        // `(T)BitOperations.LeadingZeroCount(value)` calls. Exercise them directly to
        // guard the safeIntrinsics entries that let their IL run.
        int width = IntPtr.Size * 8;

        if (uint.LeadingZeroCount(0u) != 32u) return 1;
        if (uint.LeadingZeroCount(1u) != 31u) return 2;
        if (uint.LeadingZeroCount(uint.MaxValue) != 0u) return 3;

        if (ulong.LeadingZeroCount(0ul) != 64ul) return 4;
        if (ulong.LeadingZeroCount(1ul) != 63ul) return 5;
        if (ulong.LeadingZeroCount(ulong.MaxValue) != 0ul) return 6;

        if (nuint.LeadingZeroCount((nuint)0) != (nuint)width) return 7;
        if (nuint.LeadingZeroCount((nuint)1) != (nuint)(width - 1)) return 8;
        if (nuint.LeadingZeroCount(nuint.MaxValue) != (nuint)0) return 9;

        // The signed wrappers reinterpret the bits rather than taking the magnitude, so a
        // negative input has its sign bit set and therefore no leading zeros at all.
        if (int.LeadingZeroCount(0) != 32) return 10;
        if (int.LeadingZeroCount(1) != 31) return 11;
        if (int.LeadingZeroCount(-1) != 0) return 12;
        if (int.LeadingZeroCount(int.MinValue) != 0) return 13;
        if (int.LeadingZeroCount(int.MaxValue) != 1) return 14;

        if (long.LeadingZeroCount(0L) != 64L) return 15;
        if (long.LeadingZeroCount(1L) != 63L) return 16;
        if (long.LeadingZeroCount(-1L) != 0L) return 17;
        if (long.LeadingZeroCount(long.MinValue) != 0L) return 18;
        if (long.LeadingZeroCount(long.MaxValue) != 1L) return 19;

        if (nint.LeadingZeroCount((nint)0) != (nint)width) return 20;
        if (nint.LeadingZeroCount((nint)1) != (nint)(width - 1)) return 21;
        if (nint.LeadingZeroCount((nint)(-1)) != (nint)0) return 22;

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

        result = BitOperationsLeadingZeroCountTests.TestUInt32();
        if (result != 0) return result;

        result = BitOperationsLeadingZeroCountTests.TestUInt64();
        if (result != 0) return 20 + result;

        result = BitOperationsLeadingZeroCountTests.TestNUInt();
        if (result != 0) return 40 + result;

        result = BitOperationsLeadingZeroCountTests.TestThroughIBinaryIntegerWrappers();
        if (result != 0) return 60 + result;

        return 0;
    }
}

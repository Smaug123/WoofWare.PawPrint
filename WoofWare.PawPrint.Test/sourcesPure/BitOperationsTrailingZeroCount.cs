using System;
using System.Numerics;

public class BitOperationsTrailingZeroCountTests
{
    public static int TestUInt32()
    {
        if (BitOperations.TrailingZeroCount(0u) != 32) return 1;
        if (BitOperations.TrailingZeroCount(1u) != 0) return 2;
        if (BitOperations.TrailingZeroCount(2u) != 1) return 3;
        if (BitOperations.TrailingZeroCount(3u) != 0) return 4;
        if (BitOperations.TrailingZeroCount(0x100u) != 8) return 5;
        if (BitOperations.TrailingZeroCount(0x80000000u) != 31) return 6;
        if (BitOperations.TrailingZeroCount(uint.MaxValue) != 0) return 7;

        for (int bit = 0; bit < 32; bit++)
        {
            uint pow = 1u << bit;
            // A single set bit at index `bit` has exactly `bit` zeros below it.
            if (BitOperations.TrailingZeroCount(pow) != bit) return 8;
            // Setting every bit above it does not move the lowest set bit.
            if (BitOperations.TrailingZeroCount(~(pow - 1u)) != bit) return 9;
            // Clearing it moves the lowest set bit up one place; at bit 31 the value
            // becomes zero, and the documented answer there is the full width, 32.
            if (BitOperations.TrailingZeroCount(pow << 1) != bit + 1) return 10;
        }

        return 0;
    }

    public static int TestInt32()
    {
        // The signed overload reinterprets the bits, so a negative value's trailing zeros
        // are those of its two's-complement pattern.
        if (BitOperations.TrailingZeroCount(0) != 32) return 1;
        if (BitOperations.TrailingZeroCount(1) != 0) return 2;
        if (BitOperations.TrailingZeroCount(-1) != 0) return 3;
        if (BitOperations.TrailingZeroCount(int.MinValue) != 31) return 4;
        if (BitOperations.TrailingZeroCount(-2) != 1) return 5;
        if (BitOperations.TrailingZeroCount(int.MaxValue) != 0) return 6;
        return 0;
    }

    public static int TestUInt64()
    {
        if (BitOperations.TrailingZeroCount(0ul) != 64) return 1;
        if (BitOperations.TrailingZeroCount(1ul) != 0) return 2;
        if (BitOperations.TrailingZeroCount(2ul) != 1) return 3;
        if (BitOperations.TrailingZeroCount(3ul) != 0) return 4;
        // Straddling the 32-bit halves is the case the BCL's own fallback splits on.
        if (BitOperations.TrailingZeroCount(0x100000000ul) != 32) return 5;
        if (BitOperations.TrailingZeroCount(0x80000000ul) != 31) return 6;
        if (BitOperations.TrailingZeroCount(0x8000000000000000ul) != 63) return 7;
        if (BitOperations.TrailingZeroCount(ulong.MaxValue) != 0) return 8;

        for (int bit = 0; bit < 64; bit++)
        {
            ulong pow = 1ul << bit;
            if (BitOperations.TrailingZeroCount(pow) != bit) return 9;
            if (BitOperations.TrailingZeroCount(~(pow - 1ul)) != bit) return 10;
            if (BitOperations.TrailingZeroCount(pow << 1) != bit + 1) return 11;
        }

        return 0;
    }

    public static int TestInt64()
    {
        if (BitOperations.TrailingZeroCount(0L) != 64) return 1;
        if (BitOperations.TrailingZeroCount(1L) != 0) return 2;
        if (BitOperations.TrailingZeroCount(-1L) != 0) return 3;
        if (BitOperations.TrailingZeroCount(long.MinValue) != 63) return 4;
        if (BitOperations.TrailingZeroCount(-2L) != 1) return 5;
        if (BitOperations.TrailingZeroCount(long.MaxValue) != 0) return 6;
        return 0;
    }

    private struct ZeroNuintHolder { public nuint Value; }
    private struct ZeroNintHolder { public nint Value; }

    public static int TestNativeInt()
    {
        // nuint is 32 bits wide on a 32-bit platform and 64 on a 64-bit one, and the answer
        // for zero is the full width, so derive it rather than hard-coding 64.
        int width = IntPtr.Size * 8;

        if (BitOperations.TrailingZeroCount((nuint)0) != width) return 1;
        if (BitOperations.TrailingZeroCount((nuint)1) != 0) return 2;
        if (BitOperations.TrailingZeroCount((nuint)2) != 1) return 3;
        if (BitOperations.TrailingZeroCount((nuint)256) != 8) return 4;
        if (BitOperations.TrailingZeroCount(nuint.MaxValue) != 0) return 5;

        if (BitOperations.TrailingZeroCount((nint)0) != width) return 6;
        if (BitOperations.TrailingZeroCount((nint)1) != 0) return 7;
        if (BitOperations.TrailingZeroCount((nint)(-1)) != 0) return 8;
        if (BitOperations.TrailingZeroCount((nint)(-2)) != 1) return 9;

        // Default-initialised native-int values can arrive on PawPrint's eval stack as
        // NativeInt(ManagedPointerSource.Null), not Verbatim 0; check both shapes, since
        // zero is exactly the input whose answer is the full width.
        if (BitOperations.TrailingZeroCount(default(nuint)) != width) return 10;
        var zeroNuint = new ZeroNuintHolder();
        if (BitOperations.TrailingZeroCount(zeroNuint.Value) != width) return 11;
        if (BitOperations.TrailingZeroCount(default(nint)) != width) return 12;
        var zeroNint = new ZeroNintHolder();
        if (BitOperations.TrailingZeroCount(zeroNint.Value) != width) return 13;

        for (int bit = 0; bit < width; bit++)
        {
            nuint pow = (nuint)1 << bit;
            if (BitOperations.TrailingZeroCount(pow) != bit) return 14;
        }

        return 0;
    }

    public static int TestThroughIBinaryIntegerWrappers()
    {
        // The wrappers UInt32/UInt64/UIntPtr/Int32/Int64/IntPtr.TrailingZeroCount are
        // themselves [Intrinsic] in CoreLib, but their IL bodies are simple
        // `(T)BitOperations.TrailingZeroCount(value)` calls. Exercise them directly to
        // guard the safeIntrinsics entries that let their IL run.
        int width = IntPtr.Size * 8;

        if (uint.TrailingZeroCount(0u) != 32u) return 1;
        if (uint.TrailingZeroCount(0x80000000u) != 31u) return 2;
        if (uint.TrailingZeroCount(uint.MaxValue) != 0u) return 3;

        if (ulong.TrailingZeroCount(0ul) != 64ul) return 4;
        if (ulong.TrailingZeroCount(0x100000000ul) != 32ul) return 5;
        if (ulong.TrailingZeroCount(ulong.MaxValue) != 0ul) return 6;

        if (nuint.TrailingZeroCount((nuint)0) != (nuint)width) return 7;
        if (nuint.TrailingZeroCount((nuint)256) != (nuint)8) return 8;
        if (nuint.TrailingZeroCount(nuint.MaxValue) != (nuint)0) return 9;

        if (int.TrailingZeroCount(0) != 32) return 10;
        if (int.TrailingZeroCount(int.MinValue) != 31) return 11;
        if (int.TrailingZeroCount(-1) != 0) return 12;

        if (long.TrailingZeroCount(0L) != 64L) return 13;
        if (long.TrailingZeroCount(long.MinValue) != 63L) return 14;
        if (long.TrailingZeroCount(-1L) != 0L) return 15;

        if (nint.TrailingZeroCount((nint)0) != (nint)width) return 16;
        if (nint.TrailingZeroCount((nint)(-2)) != (nint)1) return 17;
        if (nint.TrailingZeroCount((nint)(-1)) != (nint)0) return 18;

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

        result = BitOperationsTrailingZeroCountTests.TestUInt32();
        if (result != 0) return result;

        result = BitOperationsTrailingZeroCountTests.TestInt32();
        if (result != 0) return 20 + result;

        result = BitOperationsTrailingZeroCountTests.TestUInt64();
        if (result != 0) return 40 + result;

        result = BitOperationsTrailingZeroCountTests.TestInt64();
        if (result != 0) return 60 + result;

        result = BitOperationsTrailingZeroCountTests.TestNativeInt();
        if (result != 0) return 80 + result;

        result = BitOperationsTrailingZeroCountTests.TestThroughIBinaryIntegerWrappers();
        if (result != 0) return 110 + result;

        return 0;
    }
}

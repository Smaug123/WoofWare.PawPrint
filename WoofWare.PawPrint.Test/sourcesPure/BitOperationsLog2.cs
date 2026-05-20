using System;
using System.Numerics;

public class BitOperationsLog2Tests
{
    public static int TestUInt32()
    {
        if (BitOperations.Log2(0u) != 0) return 1;
        if (BitOperations.Log2(1u) != 0) return 2;
        if (BitOperations.Log2(2u) != 1) return 3;
        if (BitOperations.Log2(3u) != 1) return 4;
        if (BitOperations.Log2(4u) != 2) return 5;
        if (BitOperations.Log2(7u) != 2) return 6;
        if (BitOperations.Log2(8u) != 3) return 7;
        if (BitOperations.Log2(255u) != 7) return 8;
        if (BitOperations.Log2(256u) != 8) return 9;
        if (BitOperations.Log2(0x7FFFFFFFu) != 30) return 10;
        if (BitOperations.Log2(0x80000000u) != 31) return 11;
        if (BitOperations.Log2(uint.MaxValue) != 31) return 12;

        for (int bit = 0; bit < 32; bit++)
        {
            uint pow = 1u << bit;
            if (BitOperations.Log2(pow) != bit) return 100 + bit;
            if (bit > 0 && BitOperations.Log2(pow - 1u) != bit - 1) return 200 + bit;
        }

        return 0;
    }

    public static int TestUInt64()
    {
        if (BitOperations.Log2(0ul) != 0) return 1;
        if (BitOperations.Log2(1ul) != 0) return 2;
        if (BitOperations.Log2(2ul) != 1) return 3;
        if (BitOperations.Log2(3ul) != 1) return 4;
        if (BitOperations.Log2(0xFFFFFFFFul) != 31) return 5;
        if (BitOperations.Log2(0x100000000ul) != 32) return 6;
        if (BitOperations.Log2(0x7FFFFFFFFFFFFFFFul) != 62) return 7;
        if (BitOperations.Log2(0x8000000000000000ul) != 63) return 8;
        if (BitOperations.Log2(ulong.MaxValue) != 63) return 9;

        for (int bit = 0; bit < 64; bit++)
        {
            ulong pow = 1ul << bit;
            if (BitOperations.Log2(pow) != bit) return 100 + bit;
            if (bit > 0 && BitOperations.Log2(pow - 1ul) != bit - 1) return 200 + bit;
        }

        return 0;
    }

    private struct ZeroNuintHolder { public nuint Value; }

    public static int TestNUInt()
    {
        if (BitOperations.Log2((nuint)0) != 0) return 1;
        if (BitOperations.Log2((nuint)1) != 0) return 2;
        if (BitOperations.Log2((nuint)2) != 1) return 3;
        if (BitOperations.Log2((nuint)3) != 1) return 4;
        if (BitOperations.Log2((nuint)255) != 7) return 5;
        if (BitOperations.Log2((nuint)256) != 8) return 6;

        // nuint.MaxValue is 2^32 - 1 on 32-bit, 2^64 - 1 on 64-bit. Compute the expected
        // log directly from the platform width so the test works on either.
        int expectedMax = (IntPtr.Size * 8) - 1;
        if (BitOperations.Log2(nuint.MaxValue) != expectedMax) return 7;

        // Default-initialised nuint values can arrive on PawPrint's eval stack as
        // NativeInt(ManagedPointerSource.Null), not Verbatim 0; check both shapes.
        if (BitOperations.Log2(default(nuint)) != 0) return 8;
        var zeroHolder = new ZeroNuintHolder();
        if (BitOperations.Log2(zeroHolder.Value) != 0) return 9;

        return 0;
    }

    public static int TestThroughIBinaryNumberWrappers()
    {
        // The wrappers UInt32.Log2 / UInt64.Log2 / UIntPtr.Log2 are themselves [Intrinsic]
        // in CoreLib but their IL bodies are simple `(T)BitOperations.Log2(value)` calls.
        // Exercise them directly to guard the safeIntrinsics entries that let their IL run.
        if (uint.Log2(8u) != 3u) return 1;
        if (uint.Log2(0u) != 0u) return 2;
        if (uint.Log2(uint.MaxValue) != 31u) return 3;
        if (ulong.Log2(0x100000000ul) != 32ul) return 4;
        if (ulong.Log2(ulong.MaxValue) != 63ul) return 5;
        if (nuint.Log2((nuint)256) != (nuint)8) return 6;
        return 0;
    }
}

class Program
{
    static int Main(string[] args)
    {
        int result;

        result = BitOperationsLog2Tests.TestUInt32();
        if (result != 0) return 1000 + result;

        result = BitOperationsLog2Tests.TestUInt64();
        if (result != 0) return 2000 + result;

        result = BitOperationsLog2Tests.TestNUInt();
        if (result != 0) return 3000 + result;

        result = BitOperationsLog2Tests.TestThroughIBinaryNumberWrappers();
        if (result != 0) return 4000 + result;

        return 0;
    }
}

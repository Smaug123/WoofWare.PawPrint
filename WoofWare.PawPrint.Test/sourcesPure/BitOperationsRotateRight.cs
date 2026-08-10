using System;
using System.Numerics;

public class BitOperationsRotateRightTests
{
    public static int TestUInt32()
    {
        if (BitOperations.RotateRight(0x12345678u, 0) != 0x12345678u) return 1;
        if (BitOperations.RotateRight(0x12345678u, 8) != 0x78123456u) return 2;
        if (BitOperations.RotateRight(0x12345678u, 16) != 0x56781234u) return 3;
        // The bits leaving the bottom must reappear at the top, not be discarded.
        if (BitOperations.RotateRight(1u, 1) != 0x80000000u) return 4;
        if (BitOperations.RotateRight(0x80000000u, 31) != 1u) return 5;
        if (BitOperations.RotateRight(uint.MaxValue, 13) != uint.MaxValue) return 6;

        // A full turn is the identity. The BCL body is
        // `(value >> offset) | (value << (32 - offset))`, so offsets 0 and 32 are the two
        // that reach a shift of the full width — whose result ECMA-335 III.3.58 leaves
        // unspecified. Both are asserted here because the *rotate* is well-defined either
        // way: whichever of `value` or `0` the wide shift yields, the OR gives `value`.
        // Offsets outside [0, 32] are deliberately not asserted: the BCL documents them as
        // congruent mod 32, but the body reaches that only via a negative shift count,
        // whose behaviour is unspecified and so is not a cross-runtime fact.
        if (BitOperations.RotateRight(0x12345678u, 32) != 0x12345678u) return 7;

        for (int offset = 0; offset < 32; offset++)
        {
            // A single set bit rotates to a predictable place, and rotating back returns it.
            uint rotated = BitOperations.RotateRight(1u, offset);
            uint expected = offset == 0 ? 1u : 1u << (32 - offset);
            if (rotated != expected) return 10;
            if (BitOperations.RotateLeft(rotated, offset) != 1u) return 11;
        }

        return 0;
    }

    public static int TestUInt64()
    {
        if (BitOperations.RotateRight(0x0123456789ABCDEFul, 0) != 0x0123456789ABCDEFul) return 1;
        if (BitOperations.RotateRight(0x0123456789ABCDEFul, 8) != 0xEF0123456789ABCDul) return 2;
        if (BitOperations.RotateRight(0x0123456789ABCDEFul, 32) != 0x89ABCDEF01234567ul) return 3;
        if (BitOperations.RotateRight(1ul, 1) != 0x8000000000000000ul) return 4;
        if (BitOperations.RotateRight(0x8000000000000000ul, 63) != 1ul) return 5;
        if (BitOperations.RotateRight(ulong.MaxValue, 29) != ulong.MaxValue) return 6;
        // A full turn is the identity, for the same reason as the 32-bit case above.
        if (BitOperations.RotateRight(0x0123456789ABCDEFul, 64) != 0x0123456789ABCDEFul) return 7;

        for (int offset = 0; offset < 64; offset++)
        {
            ulong rotated = BitOperations.RotateRight(1ul, offset);
            ulong expected = offset == 0 ? 1ul : 1ul << (64 - offset);
            if (rotated != expected) return 8;
            if (BitOperations.RotateLeft(rotated, offset) != 1ul) return 9;
        }

        return 0;
    }

    public static int TestNativeUInt()
    {
        // nuint forwards to the 64-bit overload on a 64-bit process and the 32-bit one
        // otherwise, so derive the width rather than assuming it.
        int width = IntPtr.Size * 8;

        if (BitOperations.RotateRight((nuint)0, 5) != (nuint)0) return 1;
        if (BitOperations.RotateRight((nuint)1, 1) != (nuint)1 << (width - 1)) return 2;
        if (BitOperations.RotateRight(nuint.MaxValue, 7) != nuint.MaxValue) return 3;
        if (BitOperations.RotateRight((nuint)0x12345678, 0) != (nuint)0x12345678) return 4;

        for (int offset = 0; offset < width; offset++)
        {
            nuint rotated = BitOperations.RotateRight((nuint)1, offset);
            nuint expected = offset == 0 ? (nuint)1 : (nuint)1 << (width - offset);
            if (rotated != expected) return 5;
            if (BitOperations.RotateLeft(rotated, offset) != (nuint)1) return 6;
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

        result = BitOperationsRotateRightTests.TestUInt32();
        if (result != 0) return result;

        result = BitOperationsRotateRightTests.TestUInt64();
        if (result != 0) return 20 + result;

        result = BitOperationsRotateRightTests.TestNativeUInt();
        if (result != 0) return 40 + result;

        return 0;
    }
}

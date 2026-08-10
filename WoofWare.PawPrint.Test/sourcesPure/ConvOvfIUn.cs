using System;

public class ConvOvfIUn
{
    // Exercises the `conv.ovf.i.un` IL opcode (checked conversion to native
    // signed int, treating the source as *unsigned*) from the three source
    // operand types that emit it: ulong, uint and nuint.
    //
    // The contrast with `conv.ovf.i` is the source interpretation, and it is
    // observable in both directions:
    //
    //  * A 32-bit source with its top bit set (`uint.MaxValue`) is zero-
    //    extended, not sign-extended, so it converts to 4294967295 rather than
    //    -1. This is the case that distinguishes `conv.ovf.i.un` from
    //    `conv.ovf.i` on an int32 stack slot, and no int32 source can overflow
    //    because every uint32 fits in a 64-bit native int.
    //  * A 64-bit source with its top bit set is a number at least 2^63, which
    //    does *not* fit in a signed native int, so it overflows — whereas
    //    `conv.ovf.i` would happily pass the same bits through as a negative
    //    number.
    //
    // Doubles are not covered: a double is signed by construction, so C# emits
    // `conv.ovf.i` for `checked((nint)someDouble)` and the float behaviour is
    // already covered by `ConvOvfI.cs`.

    private static ulong s_ulongSmall = 12345UL;
    private static ulong s_ulongLongMax = (ulong)long.MaxValue;
    private static ulong s_ulongTwoToThe63 = 9223372036854775808UL;
    private static ulong s_ulongMax = ulong.MaxValue;
    private static uint s_uintZero = 0U;
    private static uint s_uintOne = 1U;
    private static uint s_uintMax = uint.MaxValue;
    private static nuint s_nuintSmall = 42;
    private static nuint s_nuintLongMax = unchecked((nuint)long.MaxValue);
    private static nuint s_nuintTwoToThe63 = unchecked((nuint)9223372036854775808UL);
    private static nuint s_nuintMax = unchecked((nuint)ulong.MaxValue);

    public static int Main(string[] args)
    {
        // A small ulong is in range.
        if (checked((nint)s_ulongSmall) != (nint)12345)
        {
            return 1;
        }

        // The largest in-range ulong is long.MaxValue itself.
        if (checked((nint)s_ulongLongMax) != unchecked((nint)long.MaxValue))
        {
            return 2;
        }

        // 2^63 is the smallest ulong that does not fit in a signed native int.
        try
        {
            nint _ = checked((nint)s_ulongTwoToThe63);
            return 3;
        }
        catch (OverflowException)
        {
            // Expected.
        }

        // ulong.MaxValue is 0xFFFF_FFFF_FFFF_FFFF: `conv.ovf.i` would read that
        // as -1, but read as unsigned it is far out of range.
        try
        {
            nint _ = checked((nint)s_ulongMax);
            return 4;
        }
        catch (OverflowException)
        {
            // Expected.
        }

        if (checked((nint)s_uintZero) != (nint)0)
        {
            return 5;
        }

        if (checked((nint)s_uintOne) != (nint)1)
        {
            return 6;
        }

        // The 32-bit source whose stack slot holds 0xFFFF_FFFF: zero-extended
        // to 4294967295, not sign-extended to -1. No uint can overflow.
        if (checked((nint)s_uintMax) != unchecked((nint)4294967295L))
        {
            return 7;
        }

        // Same three cases again through a native-int-width unsigned source.
        if (checked((nint)s_nuintSmall) != (nint)42)
        {
            return 8;
        }

        if (checked((nint)s_nuintLongMax) != unchecked((nint)long.MaxValue))
        {
            return 9;
        }

        try
        {
            nint _ = checked((nint)s_nuintTwoToThe63);
            return 10;
        }
        catch (OverflowException)
        {
            // Expected.
        }

        try
        {
            nint _ = checked((nint)s_nuintMax);
            return 11;
        }
        catch (OverflowException)
        {
            // Expected.
        }

        return 0;
    }
}

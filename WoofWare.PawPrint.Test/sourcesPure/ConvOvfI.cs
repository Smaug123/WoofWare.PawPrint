using System;

public class ConvOvfI
{
    // Exercises the `conv.ovf.i` IL opcode (checked conversion to native
    // signed int) from the source operand types that actually emit it: long
    // and double. (`int` -> `nint` cannot overflow on any platform, so Roslyn
    // emits plain `conv.i` for it and it is not covered here.)
    //
    // The contrast with `conv.ovf.u` is that negative sources are
    // representable here, so the only integer overflow would be a
    // long that doesn't fit in a native int — impossible on a 64-bit runtime,
    // which is what both PawPrint and the real runtime under test are. Floats
    // are therefore the only overflow source: `conv.ovf.i` truncates toward
    // zero and then range-checks, so `(nint)-0.5` succeeds (truncates to 0)
    // while `(nint)1e30` and NaN overflow.

    private static long s_positiveLong = 1_000_000_000_000L;
    private static long s_negativeLong = -5L;
    private static long s_minLong = long.MinValue;
    private static long s_maxLong = long.MaxValue;
    private static double s_positiveDouble = 100.5;
    private static double s_negativeDouble = -100.5;
    private static double s_smallNegativeDouble = -0.5;
    private static double s_twoToThe63 = 9223372036854775808.0;
    private static double s_minusTwoToThe63 = -9223372036854775808.0;
    private static double s_hugeDouble = 1e30;
    private static double s_hugeNegativeDouble = -1e30;
    private static double s_nanDouble = double.NaN;

    public static int Main(string[] args)
    {
        // Positive long -> nint succeeds.
        nint a = checked((nint)s_positiveLong);
        if (a != (nint)1_000_000_000_000L)
        {
            return 1;
        }

        // Negative long -> nint succeeds (unlike conv.ovf.u).
        nint b = checked((nint)s_negativeLong);
        if (b != (nint)(-5))
        {
            return 2;
        }

        // The full int64 range round-trips on a 64-bit runtime.
        if (checked((nint)s_minLong) != unchecked((nint)long.MinValue))
        {
            return 3;
        }

        if (checked((nint)s_maxLong) != unchecked((nint)long.MaxValue))
        {
            return 4;
        }

        // Positive double truncates toward zero.
        nint c = checked((nint)s_positiveDouble);
        if (c != (nint)100)
        {
            return 5;
        }

        // Negative double truncates toward zero, not down.
        nint d = checked((nint)s_negativeDouble);
        if (d != (nint)(-100))
        {
            return 6;
        }

        // Double in (-1, 0) truncates to 0.
        nint e = checked((nint)s_smallNegativeDouble);
        if (e != (nint)0)
        {
            return 7;
        }

        // -2^63 is exactly representable as a double and is exactly
        // long.MinValue, so it is in range.
        nint f = checked((nint)s_minusTwoToThe63);
        if (f != unchecked((nint)long.MinValue))
        {
            return 8;
        }

        // +2^63 is the smallest double greater than long.MaxValue: overflow.
        try
        {
            nint _ = checked((nint)s_twoToThe63);
            return 9;
        }
        catch (OverflowException)
        {
            // Expected.
        }

        // Far out of range in both directions: overflow.
        try
        {
            nint _ = checked((nint)s_hugeDouble);
            return 10;
        }
        catch (OverflowException)
        {
            // Expected.
        }

        try
        {
            nint _ = checked((nint)s_hugeNegativeDouble);
            return 11;
        }
        catch (OverflowException)
        {
            // Expected.
        }

        // NaN -> nint overflows.
        try
        {
            nint _ = checked((nint)s_nanDouble);
            return 12;
        }
        catch (OverflowException)
        {
            // Expected.
        }

        return 0;
    }
}

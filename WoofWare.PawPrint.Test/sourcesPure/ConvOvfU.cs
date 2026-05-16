using System;

public class ConvOvfU
{
    // Exercises the `conv.ovf.u` IL opcode (checked conversion to native
    // unsigned int) from each of the standard source operand types: int,
    // long, and double. Covers both the success and overflow paths.
    //
    // Note on float semantics: `conv.ovf.u` truncates toward zero before
    // overflow-checking. So `(nuint)-0.5` succeeds (truncates to 0) but
    // `(nuint)-1.5` overflows (truncates to -1, which is unrepresentable
    // as nuint). NaN overflows too.

    private static volatile int s_positiveInt = 42;
    private static volatile int s_negativeInt = -1;
    private static long s_positiveLong = 1_000_000_000_000L;
    private static long s_negativeLong = -5L;
    private static double s_positiveDouble = 100.5;
    private static double s_smallNegativeDouble = -0.5;
    private static double s_bigNegativeDouble = -1.5;
    private static double s_nanDouble = double.NaN;

    public static int Main(string[] args)
    {
        // Non-negative int -> nuint succeeds.
        nuint a = checked((nuint)s_positiveInt);
        if (a != (nuint)42)
        {
            return 1;
        }

        // Non-negative long -> nuint succeeds.
        nuint b = checked((nuint)s_positiveLong);
        if (b != (nuint)1_000_000_000_000L)
        {
            return 2;
        }

        // Non-negative double -> nuint truncates toward zero.
        nuint c = checked((nuint)s_positiveDouble);
        if (c != (nuint)100)
        {
            return 3;
        }

        // Double in (-1, 0): truncates to 0, no overflow.
        nuint d = checked((nuint)s_smallNegativeDouble);
        if (d != (nuint)0)
        {
            return 4;
        }

        // Negative int -> nuint overflows.
        try
        {
            nuint _ = checked((nuint)s_negativeInt);
            return 5;
        }
        catch (OverflowException)
        {
            // Expected.
        }

        // Negative long -> nuint overflows.
        try
        {
            nuint _ = checked((nuint)s_negativeLong);
            return 6;
        }
        catch (OverflowException)
        {
            // Expected.
        }

        // Double <= -1: truncates to a negative integer, overflows.
        try
        {
            nuint _ = checked((nuint)s_bigNegativeDouble);
            return 7;
        }
        catch (OverflowException)
        {
            // Expected.
        }

        // NaN -> nuint overflows.
        try
        {
            nuint _ = checked((nuint)s_nanDouble);
            return 8;
        }
        catch (OverflowException)
        {
            // Expected.
        }

        return 0;
    }
}

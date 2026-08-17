using System;

public class ConvOvfU2
{
    // Exercises the `conv.ovf.u2` IL opcode (checked conversion to unsigned
    // int16) from each source stack type, covering both the success and the
    // overflow path of each.
    //
    // Only *signed* sources belong in this file. Roslyn emits `conv.ovf.u2` for
    // `checked((ushort)x)` and `checked((char)x)` from int, long and double, but
    // it emits `conv.ovf.u2.un` from uint or ulong, and that opcode is not
    // implemented. An unsigned source added here would abort the interpreter and
    // take every assertion below it down with it.
    //
    // Note that `conv.ovf.u2` range-checks the source's full signed width rather
    // than truncating it, which is why `s_lowBitsFitLong` overflows despite its
    // low 16 bits being in range.
    //
    // Note on float semantics: `conv.ovf.u2` truncates toward zero before
    // overflow-checking. So `(ushort)-0.5` succeeds (truncates to 0) but
    // `(ushort)-1.5` overflows (truncates to -1, which is unrepresentable as
    // ushort). NaN overflows too.

    private static volatile int s_topOfRangeInt = 65535;
    private static volatile int s_negativeInt = -1;
    private static volatile int s_tooBigInt = 65536;
    private static long s_inRangeLong = 65534L;
    private static long s_lowBitsFitLong = 0x1_0000_FFFFL;
    private static long s_negativeLong = -5L;
    private static double s_inRangeDouble = 100.5;
    private static double s_smallNegativeDouble = -0.5;
    private static double s_bigNegativeDouble = -1.5;
    private static double s_tooBigDouble = 65536.0;
    private static double s_nanDouble = double.NaN;
    private static volatile int s_charCodePoint = 65;

    public static int Main(string[] args)
    {
        // A source at the very top of the range converts and zero-extends; an
        // implementation that sign-extended the 16-bit result would answer -1.
        ushort a = checked((ushort)s_topOfRangeInt);
        if (a != 65535)
        {
            return 1;
        }

        // In-range long -> ushort succeeds.
        ushort b = checked((ushort)s_inRangeLong);
        if (b != 65534)
        {
            return 2;
        }

        // In-range double -> ushort truncates toward zero.
        ushort c = checked((ushort)s_inRangeDouble);
        if (c != 100)
        {
            return 3;
        }

        // Double in (-1, 0): truncates to 0, no overflow.
        ushort d = checked((ushort)s_smallNegativeDouble);
        if (d != 0)
        {
            return 4;
        }

        // char is a ushort at the stack-type level and emits the same opcode.
        char e = checked((char)s_charCodePoint);
        if (e != 'A')
        {
            return 5;
        }

        // Negative int -> ushort overflows.
        try
        {
            ushort _ = checked((ushort)s_negativeInt);
            return 6;
        }
        catch (OverflowException)
        {
            // Expected.
        }

        // Int just above the range overflows.
        try
        {
            ushort _ = checked((ushort)s_tooBigInt);
            return 7;
        }
        catch (OverflowException)
        {
            // Expected.
        }

        // Negative long -> ushort overflows.
        try
        {
            ushort _ = checked((ushort)s_negativeLong);
            return 8;
        }
        catch (OverflowException)
        {
            // Expected.
        }

        // A long whose low 16 bits are in range but whose value is not: the
        // opcode range-checks rather than truncating, so this overflows.
        try
        {
            ushort _ = checked((ushort)s_lowBitsFitLong);
            return 9;
        }
        catch (OverflowException)
        {
            // Expected.
        }

        // Double <= -1: truncates to a negative integer, overflows.
        try
        {
            ushort _ = checked((ushort)s_bigNegativeDouble);
            return 10;
        }
        catch (OverflowException)
        {
            // Expected.
        }

        // Double at 2^16, the smallest double above the range: overflows.
        try
        {
            ushort _ = checked((ushort)s_tooBigDouble);
            return 11;
        }
        catch (OverflowException)
        {
            // Expected.
        }

        // NaN -> ushort overflows.
        try
        {
            ushort _ = checked((ushort)s_nanDouble);
            return 12;
        }
        catch (OverflowException)
        {
            // Expected.
        }

        return 0;
    }
}

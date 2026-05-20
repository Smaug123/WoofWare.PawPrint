using System;

public class Program
{
    public static int Main(string[] args)
    {
        int intValue = 123;
        if (-intValue != -123)
        {
            return 1;
        }

        int intMin = int.MinValue;
        if (-intMin != int.MinValue)
        {
            return 2;
        }

        long longValue = 1234567890123L;
        if (-longValue != -1234567890123L)
        {
            return 3;
        }

        long longMin = long.MinValue;
        if (-longMin != long.MinValue)
        {
            return 4;
        }

        nint nativeValue = (nint)123;
        if (-nativeValue != (nint)(-123))
        {
            return 5;
        }

        double positiveZero = 0.0;
        if (BitConverter.DoubleToInt64Bits(-positiveZero) != unchecked((long)0x8000000000000000UL))
        {
            return 6;
        }

        double negativeZero = -0.0;
        if (BitConverter.DoubleToInt64Bits(-negativeZero) != 0L)
        {
            return 7;
        }

        double positiveInfinity = double.PositiveInfinity;
        if (-positiveInfinity != double.NegativeInfinity)
        {
            return 8;
        }

        double nan = double.NaN;
        double negNan = -nan;
        if (negNan == negNan)
        {
            return 9;
        }

        return 0;
    }
}

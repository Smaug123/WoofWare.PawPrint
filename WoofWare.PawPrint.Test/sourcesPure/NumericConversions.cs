public class Program
{
    public static int Main(string[] args)
    {
        int zero = args.Length;
        int minusOne = zero - 1;
        int twoFiveFive = zero + 255;
        int sixtyFiveFiveThreeFive = zero + 65535;
        double tenPointNine = 10.9 + zero;

        if ((int)tenPointNine != 10)
        {
            return 1;
        }

        if ((long)tenPointNine != 10L)
        {
            return 2;
        }

        if ((sbyte)twoFiveFive != -1)
        {
            return 3;
        }

        if ((short)sixtyFiveFiveThreeFive != -1)
        {
            return 4;
        }

        if ((byte)minusOne != 255)
        {
            return 5;
        }

        if ((ushort)minusOne != 65535)
        {
            return 6;
        }

        uint u32 = (uint)minusOne;

        if (u32 != 4294967295u)
        {
            return 7;
        }

        ulong u64 = (ulong)(long)minusOne;

        if (u64 != ulong.MaxValue)
        {
            return 8;
        }

        double tooPreciseForSingle = 16777217.0 + zero;
        float roundedToSingle = (float)tooPreciseForSingle;

        if (roundedToSingle != 16777216.0f)
        {
            return 9;
        }

        double fromInt = zero + 123;

        if (fromInt != 123.0)
        {
            return 10;
        }

        double fromUnsigned64 = u64;

        if (fromUnsigned64 != 18446744073709551616.0)
        {
            return 11;
        }

        return 0;
    }
}

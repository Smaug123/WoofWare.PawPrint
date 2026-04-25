using System;

public class TestSpanMemmoveBlocks
{
    static int CheckLength(int length, int failureBase)
    {
        byte[] src = new byte[length + 16];
        byte[] dest = new byte[length + 16];

        for (int i = 0; i < src.Length; i++)
        {
            src[i] = (byte)(i * 17 + 23);
            dest[i] = 0xA5;
        }

        src.AsSpan(3, length).CopyTo(dest.AsSpan(5, length));

        for (int i = 0; i < dest.Length; i++)
        {
            byte expected = 0xA5;
            if (i >= 5 && i < 5 + length)
            {
                expected = src[3 + i - 5];
            }

            if (dest[i] != expected)
            {
                return failureBase + i;
            }
        }

        return 0;
    }

    public static int Main(string[] argv)
    {
        int result = CheckLength(17, 1000);
        if (result != 0)
        {
            return result;
        }

        result = CheckLength(65, 2000);
        if (result != 0)
        {
            return result;
        }

        result = CheckLength(255, 3000);
        if (result != 0)
        {
            return result;
        }

        return 0;
    }
}

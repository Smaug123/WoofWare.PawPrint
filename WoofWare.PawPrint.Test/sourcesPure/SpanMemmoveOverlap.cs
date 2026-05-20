using System;

public class TestSpanMemmoveOverlap
{
    static void Fill(byte[] data, int seed)
    {
        for (int i = 0; i < data.Length; i++)
        {
            data[i] = (byte)(seed + i * 17);
        }
    }

    static int CheckCopy(int dataLength, int sourceStart, int destStart, int count, int failureBase)
    {
        byte[] data = new byte[dataLength];
        byte[] original = new byte[dataLength];

        Fill(data, failureBase);
        Fill(original, failureBase);

        data.AsSpan(sourceStart, count).CopyTo(data.AsSpan(destStart, count));

        for (int i = 0; i < data.Length; i++)
        {
            byte expected = original[i];
            if (i >= destStart && i < destStart + count)
            {
                expected = original[sourceStart + i - destStart];
            }

            if (data[i] != expected)
            {
                return failureBase + (i % 97);
            }
        }

        return 0;
    }

    static int CheckDestAfterSource()
    {
        return CheckCopy(24, 0, 4, 16, 100);
    }

    static int CheckDestBeforeSource()
    {
        return CheckCopy(24, 4, 0, 16, 200);
    }

    static int CheckLargeDestAfterSource()
    {
        return CheckCopy(5000, 0, 997, 3000, 300);
    }

    static int CheckLargeDestBeforeSource()
    {
        return CheckCopy(5000, 997, 0, 3000, 400);
    }

    static int CheckLargeDisjointCopy()
    {
        return CheckCopy(7000, 0, 5000, 1500, 500);
    }

    public static int Main(string[] argv)
    {
        int result = CheckDestAfterSource();
        if (result != 0)
        {
            return result;
        }

        result = CheckDestBeforeSource();
        if (result != 0)
        {
            return result;
        }

        result = CheckLargeDestAfterSource();
        if (result != 0)
        {
            return result;
        }

        result = CheckLargeDestBeforeSource();
        if (result != 0)
        {
            return result;
        }

        result = CheckLargeDisjointCopy();
        if (result != 0)
        {
            return result;
        }

        return 0;
    }
}

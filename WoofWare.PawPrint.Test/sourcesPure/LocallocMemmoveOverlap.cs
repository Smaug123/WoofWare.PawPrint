using System;

unsafe class Program
{
    static void Fill(Span<byte> data, int seed)
    {
        for (int i = 0; i < data.Length; i++)
        {
            data[i] = (byte)(seed + i * 17);
        }
    }

    static int CheckCopy(int dataLength, int sourceStart, int destStart, int count, int failureBase)
    {
        Span<byte> data = stackalloc byte[dataLength];
        Span<byte> original = stackalloc byte[dataLength];

        Fill(data, failureBase);
        Fill(original, failureBase);

        data.Slice(sourceStart, count).CopyTo(data.Slice(destStart, count));

        for (int i = 0; i < data.Length; i++)
        {
            byte expected = original[i];
            if (i >= destStart && i < destStart + count)
            {
                expected = original[sourceStart + i - destStart];
            }

            if (data[i] != expected)
            {
                return failureBase + i;
            }
        }

        return 0;
    }

    static int Main(string[] args)
    {
        int result = CheckCopy(24, 0, 4, 16, 100);
        if (result != 0)
        {
            return result;
        }

        result = CheckCopy(24, 4, 0, 16, 200);
        if (result != 0)
        {
            return result;
        }

        result = CheckCopy(24, 0, 12, 6, 300);
        if (result != 0)
        {
            return result;
        }

        return 0;
    }
}

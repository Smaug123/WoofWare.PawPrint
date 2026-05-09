using System;

public class TestSpanClear
{
    static int ClearWholeIntArray()
    {
        int[] arr = new int[8];
        for (int i = 0; i < arr.Length; i++)
        {
            arr[i] = i * 17 + 23;
        }

        arr.AsSpan().Clear();

        for (int i = 0; i < arr.Length; i++)
        {
            if (arr[i] != 0)
            {
                return 100 + i;
            }
        }

        return 0;
    }

    static int ClearSlicedIntArray()
    {
        int[] arr = new int[8];
        for (int i = 0; i < arr.Length; i++)
        {
            arr[i] = i * 17 + 23;
        }

        arr.AsSpan(2, 4).Clear();

        for (int i = 0; i < arr.Length; i++)
        {
            int expected = (i >= 2 && i < 6) ? 0 : (i * 17 + 23);
            if (arr[i] != expected)
            {
                return 200 + i;
            }
        }

        return 0;
    }

    static int ClearEmptySpan()
    {
        int[] arr = new int[4];
        for (int i = 0; i < arr.Length; i++)
        {
            arr[i] = i + 1;
        }

        arr.AsSpan(2, 0).Clear();

        for (int i = 0; i < arr.Length; i++)
        {
            if (arr[i] != i + 1)
            {
                return 300 + i;
            }
        }

        return 0;
    }

    public static int Main(string[] argv)
    {
        int result = ClearWholeIntArray();
        if (result != 0)
        {
            return result;
        }

        result = ClearSlicedIntArray();
        if (result != 0)
        {
            return result;
        }

        result = ClearEmptySpan();
        if (result != 0)
        {
            return result;
        }

        return 0;
    }
}

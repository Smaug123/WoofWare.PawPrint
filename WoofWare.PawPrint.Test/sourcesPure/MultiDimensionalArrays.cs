using System;

public class TestMultiDimensionalArrays
{
    public static int TestMultiDim2d()
    {
        // 2D array
        int[,] arr2d = new int[3, 4];

        // Set values
        for (int i = 0; i < 3; i++)
        {
            for (int j = 0; j < 4; j++)
            {
                arr2d[i, j] = i * 10 + j;
            }
        }

        // Verify values
        for (int i = 0; i < 3; i++)
        {
            for (int j = 0; j < 4; j++)
            {
                if (arr2d[i, j] != i * 10 + j) return 150 + i * 4 + j;
            }
        }

        // Length of multi-dimensional array
        if (arr2d.Length != 12) return 170;

        return 0;
    }

    public static int Main(string[] argv)
    {
        int result = TestMultiDim2d();
        if (result != 0) return 8400 + result;

        return 0;
    }
}

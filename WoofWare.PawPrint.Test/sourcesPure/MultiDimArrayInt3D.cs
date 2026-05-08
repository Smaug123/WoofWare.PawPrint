public class Program
{
    public static int Main(string[] args)
    {
        int[,,] cube = new int[2, 3, 4];

        // Default-initialised cells are zero.
        for (int i = 0; i < 2; i++)
        {
            for (int j = 0; j < 3; j++)
            {
                for (int k = 0; k < 4; k++)
                {
                    if (cube[i, j, k] != 0)
                    {
                        return 1;
                    }
                }
            }
        }

        // Row-major round-trip: each cell holds a unique value derived from its index.
        for (int i = 0; i < 2; i++)
        {
            for (int j = 0; j < 3; j++)
            {
                for (int k = 0; k < 4; k++)
                {
                    cube[i, j, k] = (i * 100) + (j * 10) + k;
                }
            }
        }

        for (int i = 0; i < 2; i++)
        {
            for (int j = 0; j < 3; j++)
            {
                for (int k = 0; k < 4; k++)
                {
                    if (cube[i, j, k] != (i * 100) + (j * 10) + k)
                    {
                        return 2;
                    }
                }
            }
        }

        // Distinct slabs/rows/cells must not alias under row-major flattening.
        cube[0, 0, 0] = 999;
        if (cube[1, 0, 0] != 100)
        {
            return 3;
        }

        if (cube[0, 1, 0] != 10)
        {
            return 4;
        }

        if (cube[0, 0, 1] != 1)
        {
            return 5;
        }

        return 0;
    }
}

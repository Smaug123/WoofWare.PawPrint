public class Program
{
    public static int Main(string[] args)
    {
        int[,] grid = new int[3, 4];

        for (int i = 0; i < 3; i++)
        {
            for (int j = 0; j < 4; j++)
            {
                if (grid[i, j] != 0)
                {
                    return 1;
                }
            }
        }

        for (int i = 0; i < 3; i++)
        {
            for (int j = 0; j < 4; j++)
            {
                grid[i, j] = (i * 10) + j;
            }
        }

        for (int i = 0; i < 3; i++)
        {
            for (int j = 0; j < 4; j++)
            {
                if (grid[i, j] != (i * 10) + j)
                {
                    return 2;
                }
            }
        }

        // Distinct rows must not alias.
        grid[0, 0] = 999;
        if (grid[1, 0] != 10)
        {
            return 3;
        }

        if (grid[0, 1] != 1)
        {
            return 4;
        }

        return 0;
    }
}

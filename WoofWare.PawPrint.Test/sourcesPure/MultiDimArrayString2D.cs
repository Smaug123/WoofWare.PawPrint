public class Program
{
    public static int Main(string[] args)
    {
        string[,] grid = new string[2, 3];

        // Default-initialised cells are null reference.
        for (int i = 0; i < 2; i++)
        {
            for (int j = 0; j < 3; j++)
            {
                if (grid[i, j] != null)
                {
                    return 1;
                }
            }
        }

        grid[0, 0] = "a";
        grid[0, 1] = "b";
        grid[0, 2] = "c";
        grid[1, 0] = "d";
        grid[1, 1] = "e";
        grid[1, 2] = "f";

        if (!ReferenceEquals(grid[0, 0], "a"))
        {
            return 2;
        }

        if (!ReferenceEquals(grid[1, 2], "f"))
        {
            return 3;
        }

        // Distinct slots must not alias.
        grid[0, 0] = "X";
        if (!ReferenceEquals(grid[1, 0], "d"))
        {
            return 4;
        }

        if (!ReferenceEquals(grid[0, 1], "b"))
        {
            return 5;
        }

        return 0;
    }
}

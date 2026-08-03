using System.Collections.Generic;

public class Program
{
    public static int Main(string[] args)
    {
        int[] arr = new[] { 1, 2, 3, 4 };

        IEnumerable<int> e = arr;

        int total = 0;
        foreach (int x in e)
        {
            total += x;
        }

        if (total != 10)
        {
            return 1;
        }

        int[] empty = new int[0];
        foreach (int x in (IEnumerable<int>)empty)
        {
            return 2;
        }

        return 0;
    }
}

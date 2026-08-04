using System.Collections.Generic;

public class Program
{
    public static int Main(string[] args)
    {
        int[] arr = new[] { 5, 6, 7 };

        ICollection<int> c = arr;
        if (!c.IsReadOnly)
        {
            // SZArrayHelper.get_IsReadOnly<T> unconditionally returns true.
            return 1;
        }

        if (!c.Contains(6))
        {
            return 2;
        }

        if (c.Contains(42))
        {
            return 3;
        }

        IList<int> l = arr;
        if (l.IndexOf(7) != 2)
        {
            return 4;
        }

        if (l.IndexOf(42) != -1)
        {
            return 5;
        }

        int[] destination = new int[5];
        c.CopyTo(destination, 1);

        if (destination[0] != 0 || destination[1] != 5 || destination[2] != 6 || destination[3] != 7 ||
            destination[4] != 0)
        {
            return 6;
        }

        return 0;
    }
}

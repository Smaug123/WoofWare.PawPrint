using System;
using System.Collections.Generic;

public class Program
{
    public static int Main(string[] args)
    {
        int[] arr = new[] { 1, 2, 3 };

        ICollection<int> c = arr;
        IList<int> l = arr;

        // Arrays are fixed-size and, through these interfaces, read-only: every mutating
        // slot on SZArrayHelper throws NotSupportedException.
        try
        {
            c.Add(4);
            return 1;
        }
        catch (NotSupportedException)
        {
        }

        try
        {
            c.Remove(1);
            return 2;
        }
        catch (NotSupportedException)
        {
        }

        try
        {
            c.Clear();
            return 3;
        }
        catch (NotSupportedException)
        {
        }

        try
        {
            l.Insert(0, 4);
            return 4;
        }
        catch (NotSupportedException)
        {
        }

        try
        {
            l.RemoveAt(0);
            return 5;
        }
        catch (NotSupportedException)
        {
        }

        // None of the failed mutations should have disturbed the array.
        if (arr.Length != 3 || arr[0] != 1 || arr[1] != 2 || arr[2] != 3)
        {
            return 6;
        }

        return 0;
    }
}

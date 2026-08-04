using System.Collections.Generic;

public class Program
{
    // The interface constraint does not imply a class constraint, so the C# compiler emits
    // `constrained. !!T` before the callvirt. An array argument must take ECMA-335 III.2.1
    // case 1 (dereference to the object reference) and then land on the SZArrayHelper
    // redirect via ordinary virtual dispatch.
    private static int CountOf<T>(T collection) where T : ICollection<int>
    {
        return collection.Count;
    }

    private static bool ContainsVia<T>(T collection, int value) where T : ICollection<int>
    {
        return collection.Contains(value);
    }

    public static int Main(string[] args)
    {
        int[] arr = new[] { 4, 5, 6, 7 };

        if (CountOf(arr) != 4)
        {
            return 1;
        }

        if (!ContainsVia(arr, 6))
        {
            return 2;
        }

        if (ContainsVia(arr, 99))
        {
            return 3;
        }

        // The same generic method over a non-array receiver must still work.
        List<int> list = new List<int>();
        list.Add(1);
        list.Add(2);

        if (CountOf(list) != 2)
        {
            return 4;
        }

        return 0;
    }
}

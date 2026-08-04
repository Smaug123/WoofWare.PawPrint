using System;
using System.Collections.Generic;

public class Program
{
    public static int Main(string[] args)
    {
        int[] arr = new[] { 10, 20, 30 };

        IList<int> l = arr;
        if (l[0] != 10 || l[2] != 30)
        {
            return 1;
        }

        l[1] = 99;
        if (arr[1] != 99)
        {
            return 2;
        }

        IReadOnlyList<int> rol = arr;
        if (rol[1] != 99)
        {
            return 3;
        }

        // SZArrayHelper.get_Item does its own bounds check before indexing, throwing
        // ArgumentOutOfRangeException rather than the IndexOutOfRangeException a raw
        // ldelem would produce.
        try
        {
            int _ = l[3];
            return 4;
        }
        catch (ArgumentOutOfRangeException)
        {
        }

        try
        {
            l[-1] = 0;
            return 5;
        }
        catch (ArgumentOutOfRangeException)
        {
        }

        string[] strings = new[] { "a", "b" };
        IList<string> sl = strings;
        if (sl[1] != "b")
        {
            return 6;
        }

        sl[0] = "z";
        if (strings[0] != "z")
        {
            return 7;
        }

        return 0;
    }
}

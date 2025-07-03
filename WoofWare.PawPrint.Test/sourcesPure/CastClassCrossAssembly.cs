using System;
using System.Collections.Generic;

public class Program
{
    public static int Main(string[] args)
    {
        // Using types from System.Collections.Generic assembly
        List<int> list = new List<int> { 1, 2, 3, 4, 5 };

        // Cast to interface from another assembly
        IEnumerable<int> enumerable = (IEnumerable<int>)list;

        int count = 0;
        foreach (var item in enumerable)
        {
            count++;
        }

        return count == 5 ? 42 : 0;
    }
}

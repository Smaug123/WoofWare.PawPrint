using System.Collections.Generic;

public class Program
{
    public static int Main(string[] args)
    {
        int[] arr = new[] { 1, 2, 3 };

        ICollection<int> c = arr;
        if (c.Count != 3)
        {
            return 1;
        }

        IReadOnlyCollection<int> roc = arr;
        if (roc.Count != 3)
        {
            return 2;
        }

        IList<int> l = arr;
        if (l.Count != 3)
        {
            return 3;
        }

        IReadOnlyList<int> rol = arr;
        if (rol.Count != 3)
        {
            return 4;
        }

        string[] strings = new string[5];
        if (((ICollection<string>)strings).Count != 5)
        {
            return 5;
        }

        int[] empty = new int[0];
        if (((ICollection<int>)empty).Count != 0)
        {
            return 6;
        }

        return 0;
    }
}

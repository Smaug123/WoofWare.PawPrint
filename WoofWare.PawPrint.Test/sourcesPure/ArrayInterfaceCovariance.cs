using System;
using System.Collections.Generic;

public class Program
{
    public static int Main(string[] args)
    {
        string[] strings = new[] { "a", "b", "c" };

        // Array covariance widens string[] to object[], which implements ICollection<object>.
        // The dispatch instantiates SZArrayHelper over the *interface's* type argument
        // (object), not the array's element type (string).
        ICollection<object> c = strings;
        if (c.Count != 3)
        {
            return 1;
        }

        IList<object> l = strings;
        if (!"b".Equals(l[1]))
        {
            return 2;
        }

        // Storing a compatible element through the widened view is fine...
        l[0] = "z";
        if (strings[0] != "z")
        {
            return 3;
        }

        // ...but the store check consults the array's real element type, not the interface's
        // type argument, so storing a bare object into a string[] still fails.
        try
        {
            l[0] = new object();
            return 4;
        }
        catch (ArrayTypeMismatchException)
        {
        }

        if (strings[0] != "z")
        {
            return 5;
        }

        return 0;
    }
}

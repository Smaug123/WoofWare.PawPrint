// ECMA-335 III.4.16: a multi-dim array `Set` call (the analogue of `stelem.ref`
// for rectangular arrays) must throw ArrayTypeMismatchException when the value
// being stored is not assignment-compatible with the array's runtime element
// type. Without the check a covariantly-cast `object[,]` whose runtime
// allocation is `string[,]` would let a non-string be installed in the
// underlying storage.

using System;

public class Program
{
    public static int Main(string[] args)
    {
        string[,] strs = new string[1, 1];
        object[,] objs = strs;

        try
        {
            objs[0, 0] = new object();
            return 1;
        }
        catch (ArrayTypeMismatchException)
        {
            return 0;
        }
    }
}

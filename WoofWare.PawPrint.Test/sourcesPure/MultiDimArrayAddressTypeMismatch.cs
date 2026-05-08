// ECMA-335 III.4.10: a multi-dim array `Address` call (the analogue of `ldelema`
// for rectangular arrays) must throw ArrayTypeMismatchException when the
// metadata-declared element type does not exactly match the array's runtime
// element type. Without the check a writable byref into a covariantly-cast
// `object[,]` referencing a `string[,]` would let a non-string be stored
// in the underlying storage.

using System;

public class Program
{
    public static int Main(string[] args)
    {
        string[,] strs = new string[1, 1];
        object[,] objs = strs;

        try
        {
            ref object slot = ref objs[0, 0];
            slot = "irrelevant";
            return 1;
        }
        catch (ArrayTypeMismatchException)
        {
            return 0;
        }
    }
}

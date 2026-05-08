// CoreCLR's AllocateArrayEx (gchelpers.cpp) raises OverflowException when any
// rectangular-array dimension is negative. The runtime-synthesized .ctor on
// `int[,]` shares the same path.

using System;

public class Program
{
    public static int Main(string[] args)
    {
        int n = -1;

        try
        {
            int[,] arr = new int[n, 2];
            return 1;
        }
        catch (OverflowException)
        {
            return 0;
        }
    }
}

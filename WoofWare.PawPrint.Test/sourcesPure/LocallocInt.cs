using System;

unsafe class Program
{
    static int Main(string[] args)
    {
        int* ints = stackalloc int[3];
        ints[0] = 11;
        ints[1] = 22;
        ints[2] = ints[0] + ints[1];

        return ints[2] == 33 ? 0 : 1;
    }
}

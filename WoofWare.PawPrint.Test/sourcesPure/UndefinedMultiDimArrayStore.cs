using System.Runtime.CompilerServices;

// A value read from `stackalloc` memory nothing wrote, stored into a multi-dimensional array and
// then overwritten before anything reads it. The store only moves the value, so the exit code is
// the same whatever the stack held.

[module: SkipLocalsInit]

unsafe class Program
{
    static int Main(string[] args)
    {
        int* source = stackalloc int[1];
        int[,] array = new int[2, 2];
        array[1, 0] = *source;
        array[1, 0] = 5;
        return array[1, 0] == 5 ? 0 : 1;
    }
}

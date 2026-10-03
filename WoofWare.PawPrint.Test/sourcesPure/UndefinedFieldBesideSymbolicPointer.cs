using System;
using System.Runtime.CompilerServices;

// A struct holding a type handle's pointer beside an int copied from `stackalloc` memory nothing
// wrote, copied into another struct. Only the pointer is read back, so the exit code is the same
// whatever the stack held.

[module: SkipLocalsInit]

struct Pair
{
    public IntPtr Handle;
    public int Unwritten;
}

unsafe class Program
{
    static int Main(string[] args)
    {
        int* source = stackalloc int[1];

        Pair pair;
        pair.Handle = typeof(int).TypeHandle.Value;
        pair.Unwritten = *source;

        Pair copy = default;
        Unsafe.CopyBlock(&copy, &pair, (uint)sizeof(Pair));
        if (copy.Handle != typeof(int).TypeHandle.Value) return 1;

        return 0;
    }
}

using System;
using System.Runtime.CompilerServices;
using System.Threading;

// `Interlocked.Exchange` only moves the value it replaces: it returns it, and neither compares
// nor computes with it. So a location holding a value copied from unwritten `stackalloc` memory
// can be exchanged, and the old value discarded, whatever the stack held.

[module: SkipLocalsInit]

unsafe class Program
{
    static int Main(string[] args)
    {
        int* ints = stackalloc int[1];
        int i = ints[0];
        Interlocked.Exchange(ref i, 42);
        if (i != 42) return 1;

        long* longs = stackalloc long[1];
        long l = longs[0];
        Interlocked.Exchange(ref l, 43L);
        if (l != 43L) return 2;

        nint* nints = stackalloc nint[1];
        nint n = nints[0];
        Interlocked.Exchange(ref n, (nint)44);
        if (n != 44) return 3;

        // The old value is moved into a local, which is overwritten before it is used.
        int* more = stackalloc int[1];
        int j = more[0];
        int old = Interlocked.Exchange(ref j, 45);
        old = j;
        if (old != 45) return 4;

        return 0;
    }
}

using System.Runtime.InteropServices;

// Addresses of fields of a struct pointer into natively allocated memory, taken before the memory is
// freed and compared after. Comparing two addresses reads neither of them, so it needs no live
// allocation.

struct S
{
    public int A;
    public int B;
}

unsafe class Program
{
    static int Main(string[] args)
    {
        S* p = (S*)NativeMemory.AllocZeroed((nuint)(2 * sizeof(S)));
        nint a = (nint)(&p[0].A);
        nint b = (nint)(&p[1].B);
        nint c = (nint)((byte*)p + 12);
        NativeMemory.Free(p);

        if (a == b) return 1;
        if (b != c) return 2;

        return 0;
    }
}

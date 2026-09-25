using System.Threading;

// `Interlocked.CompareExchange` on a native int holding the address of a field of a struct pointer
// into stackalloc'd memory, with comparands that are and are not that address, and the addresses
// compared after widening to `long`.

struct S
{
    public int A;
    public int B;
}

unsafe class Program
{
    static int Main(string[] args)
    {
        S* p = stackalloc S[2];
        nint slot = (nint)(&p[0].A);

        nint seen = Interlocked.CompareExchange(ref slot, (nint)(&p[1].A), (nint)(&p[1].B));
        if (seen != (nint)(&p[0].A)) return 1;
        if (slot != (nint)(&p[0].A)) return 2;

        seen = Interlocked.CompareExchange(ref slot, (nint)(&p[1].A), (nint)(int*)p);
        if (seen != (nint)(&p[0].A)) return 3;
        if (slot != (nint)(&p[1].A)) return 4;

        if ((long)(&p[0].B) == (long)(&p[1].A)) return 5;
        if ((long)(&p[1].A) != (long)((byte*)p + 8)) return 6;

        return 0;
    }
}

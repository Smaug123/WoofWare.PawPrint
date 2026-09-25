using System.Runtime.InteropServices;

// A pointer-typed field of a struct pointer into stackalloc'd or natively allocated memory, stored
// over a struct that was stored whole, then loaded back and dereferenced, and the struct's other
// fields left as they were.

unsafe struct S
{
    public int* P;
    public int A;
    public int B;
}

unsafe class Program
{
    static int Check(S* p, int* x, int* y)
    {
        p[0] = new S { P = x, A = 1, B = 2 };
        p[1] = new S { P = x, A = 3, B = 4 };
        p->P = y;
        p[1].P = y;

        if (p->P != y) return 1;
        if (*p->P != 8) return 2;
        if (p->A != 1 || p->B != 2) return 3;
        if (*p[1].P != 8) return 4;
        if (p[1].A != 3 || p[1].B != 4) return 5;
        return 0;
    }

    static int Main(string[] args)
    {
        int x = 7;
        int y = 8;

        S* s = stackalloc S[2];
        int r = Check(s, &x, &y);
        if (r != 0) return r;

        S* n = (S*)NativeMemory.AllocZeroed((nuint)(2 * sizeof(S)));
        try
        {
            r = Check(n, &x, &y);
            if (r != 0) return 10 + r;
        }
        finally
        {
            NativeMemory.Free(n);
        }

        return 0;
    }
}

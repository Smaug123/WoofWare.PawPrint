using System.Runtime.InteropServices;

// Fields of an explicit-layout struct holding a pointer, stored whole into stackalloc'd or natively
// allocated memory, where two fields overlap: loads and stores through each of them by name.

[StructLayout(LayoutKind.Explicit)]
unsafe struct E
{
    [FieldOffset(0)] public int* P;
    [FieldOffset(8)] public int A;
    [FieldOffset(8)] public float B;
    [FieldOffset(12)] public int C;
}

unsafe class Program
{
    static int Check(E* p, int* x)
    {
        p[0] = new E { P = x, A = 5, C = 6 };
        if (p->A != 5) return 1;
        if (p->C != 6) return 2;
        p->A = 0x3f800000;
        if (p->B != 1.0f) return 3;
        p->B = 2.0f;
        if (p->A != 0x40000000) return 4;
        if (*p->P != 7) return 5;
        return 0;
    }

    static int Main(string[] args)
    {
        int x = 7;
        E* s = stackalloc E[1];
        int r = Check(s, &x);
        if (r != 0) return r;

        E* n = (E*)NativeMemory.AllocZeroed((nuint)sizeof(E));
        try
        {
            r = Check(n, &x);
            if (r != 0) return 10 + r;
        }
        finally
        {
            NativeMemory.Free(n);
        }

        return 0;
    }
}

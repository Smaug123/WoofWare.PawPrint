using System.Runtime.InteropServices;

// Stores and loads through a nested field of a struct pointer into stackalloc'd or natively
// allocated memory that nothing has written, read back through the field and through a flat
// `int*` at the same address.

struct Inner
{
    public int A;
    public int B;
}

struct Outer
{
    public int Lead;
    public Inner I;
    public int Tail;
}

unsafe class Program
{
    static int Check(Outer* p)
    {
        p[1].I.B = 7;
        if (p[1].I.B != 7) return 1;
        if (*((int*)p + 6) != 7) return 2;
        if (p[1].I.A != 0) return 3;
        p->Tail = 9;
        if (*((int*)p + 3) != 9) return 4;
        Inner i = p[1].I;
        if (i.A != 0 || i.B != 7) return 5;
        return 0;
    }

    static int Main(string[] args)
    {
        Outer* s = stackalloc Outer[2];
        int r = Check(s);
        if (r != 0) return r;

        Outer* n = (Outer*)NativeMemory.AllocZeroed(32);
        try
        {
            r = Check(n);
            if (r != 0) return 10 + r;
        }
        finally
        {
            NativeMemory.Free(n);
        }

        return 0;
    }
}

using System.Runtime.InteropServices;

// A field of a struct pointer into stackalloc'd or natively allocated memory, where the memory at
// the pointer holds a value of some other type: a `long`, or a struct of a different type. The
// field is its declaring type's offset from the pointer, whatever the memory holds, so the access
// reads and writes the bytes at that address.

struct T
{
    public int A;
    public int B;
    public int C;
    public int D;
}

struct U
{
    public int X;
    public int Y;
}

unsafe class Program
{
    static int OverLong(T* p)
    {
        *(long*)p = 0x0000_0002_0000_0001L;
        if (p->A != 1) return 1;
        if (p->B != 2) return 2;
        p->B = 20;
        if (*((int*)p + 1) != 20) return 3;
        if (*(long*)p != 0x0000_0014_0000_0001L) return 4;
        return 0;
    }

    static int OverOtherStruct(T* p)
    {
        p[0] = new T { A = 1, B = 2, C = 3, D = 4 };
        U* u = (U*)p;
        if (u->X != 1) return 11;
        if (u->Y != 2) return 12;
        if (u[1].Y != 4) return 13;
        u[1].X = 30;
        if (p->C != 30) return 14;
        if (p->D != 4) return 15;
        return 0;
    }

    static int Main(string[] args)
    {
        T* s = stackalloc T[2];
        int r = OverLong(s);
        if (r != 0) return r;

        T* s2 = stackalloc T[2];
        r = OverOtherStruct(s2);
        if (r != 0) return r;

        T* n = (T*)NativeMemory.AllocZeroed(32);
        try
        {
            r = OverLong(n);
            if (r != 0) return 100 + r;
            r = OverOtherStruct(n);
            if (r != 0) return 100 + r;
        }
        finally
        {
            NativeMemory.Free(n);
        }

        return 0;
    }
}

using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

// A byref to a field of a struct pointer into stackalloc'd or natively allocated memory, compared
// with a byref to the same address or a neighbouring one reached by flat pointer arithmetic. A
// field is at its declaring type's offset from the pointer, so the two spellings of one address
// are the same byref, whether or not anything has been stored there, and the same field of two
// structs is two addresses.

struct T
{
    public int A;
    public int B;
    public int C;
    public int D;
}

unsafe class Program
{
    static int Check(T* p)
    {
        if (!Unsafe.AreSame(ref p->B, ref *(int*)((byte*)p + 4))) return 1;
        if (Unsafe.AreSame(ref p->B, ref *(int*)((byte*)p + 8))) return 2;
        if (!Unsafe.AreSame(ref p[1].B, ref *(int*)((byte*)p + 20))) return 3;
        if (Unsafe.ByteOffset(ref p->A, ref p[1].C) != (nint)24) return 4;
        if (Unsafe.ByteOffset(ref *(int*)((byte*)p + 4), ref p[1].A) != (nint)12) return 5;
        // The same field of two structs, compared as pointers: the same chain off two bytes of
        // the block, so as far apart as those bytes.
        if (&p[0].A == &p[1].A) return 6;
        if (&p[1].B != &p[1].B) return 7;
        return 0;
    }

    static int Main(string[] args)
    {
        T* fresh = stackalloc T[2];
        int r = Check(fresh);
        if (r != 0) return r;

        T* written = stackalloc T[2];
        written[0] = new T { A = 1, B = 2, C = 3, D = 4 };
        written[1] = new T { A = 5, B = 6, C = 7, D = 8 };
        r = Check(written);
        if (r != 0) return 10 + r;

        T* n = (T*)NativeMemory.AllocZeroed(32);
        try
        {
            r = Check(n);
            if (r != 0) return 20 + r;
        }
        finally
        {
            NativeMemory.Free(n);
        }

        return 0;
    }
}

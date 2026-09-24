using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

// `ref p->A` through a pointer into stackalloc'd or natively allocated memory, stepped by
// `Unsafe.Add` to a sibling field of the same struct or to the same field of the next struct in the
// block, for loads, stores and byref identity. The block holds whatever was stored at each address,
// so the step must land on what is there: another `T`, an `S` holding a pointer, or a `long`.

struct T
{
    public int A;
    public int B;
    public int C;
    public int D;
}

unsafe struct S
{
    public int* P;
    public int A;
    public int B;
}

unsafe class Program
{
    static int CheckT(T* p)
    {
        p[0] = new T { A = 1, B = 2, C = 3, D = 4 };
        p[1] = new T { A = 5, B = 6, C = 7, D = 8 };
        ref int a = ref p->A;

        if (Unsafe.Add(ref a, 1) != 2) return 1;
        if (Unsafe.Add(ref a, 4) != 5) return 2;
        if (Unsafe.Add(ref a, 5) != 6) return 3;
        Unsafe.Add(ref a, 4) = 50;
        if (p[1].A != 50) return 4;
        Unsafe.Add(ref a, 1) = 20;
        if (p[0].B != 20) return 5;

        if (!Unsafe.AreSame(ref Unsafe.Add(ref a, 1), ref p->B)) return 6;
        if (Unsafe.AreSame(ref Unsafe.Add(ref a, 2), ref p->B)) return 7;
        if (!Unsafe.AreSame(ref Unsafe.Add(ref a, 4), ref p[1].A)) return 8;

        // A cell of another type where the next `T` would be.
        *(long*)(p + 1) = 0x0000_0007_0000_0009L;
        if (Unsafe.Add(ref a, 4) != 9) return 9;
        if (Unsafe.Add(ref a, 5) != 7) return 10;

        return 0;
    }

    static int CheckS(S* p, int* target)
    {
        p[0] = new S { P = target, A = 1, B = 2 };
        p[1] = new S { P = target, A = 3, B = 4 };
        ref int a = ref p->A;

        if (Unsafe.Add(ref a, 1) != 2) return 1;
        Unsafe.Add(ref a, 1) = 20;
        if (p[0].B != 20) return 2;

        ref int b = ref Unsafe.Add(ref a, 4);
        ref int c = ref p[1].A;
        if (!Unsafe.AreSame(ref b, ref c)) return 3;
        if (Unsafe.AreSame(ref Unsafe.Add(ref a, 1), ref c)) return 4;
        if (!Unsafe.AreSame(ref Unsafe.Add(ref a, 1), ref p->B)) return 5;
        if (Unsafe.ByteOffset(ref a, ref c) != (nint)16) return 6;

        if (p[0].P != target) return 7;
        if (*p[1].P != 7) return 8;

        return 0;
    }

    static int Main(string[] args)
    {
        int x = 7;

        T* stackT = stackalloc T[2];
        int result = CheckT(stackT);
        if (result != 0) return result;

        S* stackS = stackalloc S[2];
        result = CheckS(stackS, &x);
        if (result != 0) return 20 + result;

        T* nativeT = (T*) NativeMemory.AllocZeroed ((nuint) (2 * sizeof (T)));
        S* nativeS = (S*) NativeMemory.AllocZeroed ((nuint) (2 * sizeof (S)));

        try
        {
            result = CheckT(nativeT);
            if (result != 0) return 40 + result;

            result = CheckS(nativeS, &x);
            if (result != 0) return 60 + result;
        }
        finally
        {
            NativeMemory.Free (nativeT);
            NativeMemory.Free (nativeS);
        }

        return 0;
    }
}

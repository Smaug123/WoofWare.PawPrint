using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

// A stackalloc'd or natively allocated block holding structs that have a pointer field, read and
// written through plain `int*` arithmetic and through `Unsafe.Add` from one of their fields. A
// struct holding a pointer has no byte image, so each access has to find the field of the stored
// struct that it lands on. Every value asserted is one the struct's own fields hold, so it is the
// same on the real runtime.

unsafe struct S
{
    public int* P;
    public int A;
    public int B;
}

unsafe class Program
{
    static int Check(S* p, int* target)
    {
        p[0] = new S { P = target, A = 1, B = 2 };
        p[1] = new S { P = target, A = 3, B = 4 };
        // A cell of another type after the two structs.
        *(long*)(p + 2) = 0x0000_0006_0000_0005L;

        // `P` occupies `int` slots 0 and 1 of each `S`.
        int* q = (int*)p;

        if (q[2] != 1) return 1;
        if (q[3] != 2) return 2;
        if (q[6] != 3) return 3;
        if (q[7] != 4) return 4;
        if (q[8] != 5) return 5;
        if (q[9] != 6) return 6;

        q[2] = 10;
        q[7] = 40;
        q[9] = 60;

        if (p[0].A != 10) return 7;
        if (p[0].B != 2) return 8;
        if (p[1].A != 3) return 9;
        if (p[1].B != 40) return 10;
        if (*(long*)(p + 2) != 0x0000_003C_0000_0005L) return 11;

        // A whole-stride step from a field of one struct to the same field of the next.
        ref int a = ref p->A;

        if (Unsafe.Add(ref a, 4) != 3) return 12;
        Unsafe.Add(ref a, 4) = 30;
        if (p[1].A != 30) return 13;

        if (p[0].P != target) return 14;
        if (p[1].P != target) return 15;
        if (*p[1].P != 7) return 16;

        return 0;
    }

    static int Main(string[] args)
    {
        int x = 7;

        S* stack = stackalloc S[3];
        int result = Check(stack, &x);
        if (result != 0) return result;

        S* native = (S*) NativeMemory.AllocZeroed ((nuint) (3 * sizeof (S)));

        try
        {
            result = Check(native, &x);
            if (result != 0) return 20 + result;
        }
        finally
        {
            NativeMemory.Free (native);
        }

        return 0;
    }
}

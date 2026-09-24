using System.Runtime.CompilerServices;

// `Unsafe.AreSame` between `Unsafe.Add(ref p->A, 4)` and `ref p[1].A` for a `T*` into stackalloc'd
// memory where `p[1]` has never been written: both byrefs name the same address, so they are the
// same.

struct T
{
    public int A;
    public int B;
    public int C;
    public int D;
}

unsafe class Program
{
    static int Main(string[] args)
    {
        T* p = stackalloc T[2];
        p[0] = new T { A = 1, B = 2, C = 3, D = 4 };
        ref int a = ref p->A;

        if (!Unsafe.AreSame(ref Unsafe.Add(ref a, 4), ref p[1].A)) return 1;
        if (Unsafe.ByteOffset(ref a, ref p[1].A) != (nint)16) return 2;

        return 0;
    }
}

using System.Runtime.CompilerServices;

// `Unsafe.AsPointer(ref Unsafe.Add(ref p->A, 4)) == &p[1].A` for a `T*` into stackalloc'd memory:
// the same address reached two ways, compared as native ints rather than as byrefs.

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
        p[1] = new T { A = 5, B = 6, C = 7, D = 8 };
        ref int a = ref p->A;

        if (Unsafe.AsPointer(ref Unsafe.Add(ref a, 4)) != &p[1].A) return 1;
        if (Unsafe.AsPointer(ref Unsafe.Add(ref a, 1)) != &p->B) return 2;

        return 0;
    }
}

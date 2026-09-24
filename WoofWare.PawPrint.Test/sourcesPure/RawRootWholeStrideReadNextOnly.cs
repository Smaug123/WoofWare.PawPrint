using System.Runtime.CompilerServices;

// `Unsafe.Add(ref p->A, 4)` for a `T*` into stackalloc'd memory where only `p[1]` has been written:
// the step lands on `p[1].A`, which holds a value, although `p[0]`, where the byref starts, does not.

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
        p[1] = new T { A = 5, B = 6, C = 7, D = 8 };
        ref int a = ref p->A;

        if (Unsafe.Add(ref a, 4) != 5) return 1;
        Unsafe.Add(ref a, 5) = 60;
        if (p[1].B != 60) return 2;

        return 0;
    }
}

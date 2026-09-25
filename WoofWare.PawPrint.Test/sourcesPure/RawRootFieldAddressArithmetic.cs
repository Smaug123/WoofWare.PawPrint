// The address of a field of a struct pointer into stackalloc'd memory that nothing has written,
// taken as an `int*` and indexed within the struct and into the next one.

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
        int* q = &p->A;
        q[1] = 2;
        q[5] = 6;
        if (p->B != 2) return 1;
        if (p[1].B != 6) return 2;
        if (q[5] != 6) return 3;
        return 0;
    }
}

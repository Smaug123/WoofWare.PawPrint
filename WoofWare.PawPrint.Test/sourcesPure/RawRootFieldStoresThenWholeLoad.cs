// Every field of a struct stored one at a time through a pointer into stackalloc'd memory that
// nothing had written, then the whole struct loaded back through the pointer.

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
        T* p = stackalloc T[1];
        p->A = 1;
        p->B = 2;
        p->C = 3;
        p->D = 4;
        T t = *p;
        if (t.A != 1 || t.B != 2 || t.C != 3 || t.D != 4) return 1;
        return 0;
    }
}

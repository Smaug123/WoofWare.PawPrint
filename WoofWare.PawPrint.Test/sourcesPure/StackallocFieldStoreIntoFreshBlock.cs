// A store through a field of a struct pointer into stackalloc'd memory that nothing has written
// yet, then a load of it back.

unsafe struct S
{
    public int* P;
    public int A;
    public int B;
}

unsafe class Program
{
    static int Main(string[] args)
    {
        S* p = stackalloc S[2];
        p->A = 1;
        p[1].B = 2;

        if (p->A != 1) return 1;
        if (p[1].B != 2) return 2;

        return 0;
    }
}

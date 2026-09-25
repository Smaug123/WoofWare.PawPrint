// A pointer-typed field of a struct pointer into stackalloc'd memory that nothing has written,
// stored and then loaded back and dereferenced, beside an `int` field of the same struct.

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
        int x = 7;
        S* p = stackalloc S[2];
        p->P = &x;
        p->A = 5;
        p[1].P = &x;

        if (p->P != &x) return 1;
        if (*p->P != 7) return 2;
        if (p->A != 5) return 3;
        if (*p[1].P != 7) return 4;
        *p[1].P = 8;
        if (x != 8) return 5;

        return 0;
    }
}

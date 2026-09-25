// Addresses of fields of explicit-layout structs whose pointer-holding fields overlap other
// fields, directly or through a nested struct, through a pointer into stackalloc'd memory,
// compared as pointers. Only the fields' offsets matter, never their values.

using System.Runtime.InteropServices;

[StructLayout(LayoutKind.Explicit)]
unsafe struct U
{
    [FieldOffset(0)] public int* P;
    [FieldOffset(0)] public nint N;
    [FieldOffset(8)] public int A;
}

unsafe struct Inner
{
    public int* P;
    public int A;
}

[StructLayout(LayoutKind.Explicit)]
unsafe struct V
{
    [FieldOffset(0)] public Inner I;
    [FieldOffset(0)] public long L;
    [FieldOffset(16)] public int Tail;
}

unsafe class Program
{
    static int Main(string[] args)
    {
        U* p = stackalloc U[2];

        if ((void*)&p[0].P == (void*)&p[1].A) return 1;
        if ((void*)&p[1].P != (void*)&p[1].N) return 2;
        if ((void*)&p[0].A != (void*)((byte*)p + 8)) return 3;
        if ((void*)&p[1].P != (void*)((byte*)p + 16)) return 4;

        V* v = stackalloc V[2];

        if ((void*)&v[0].I.A == (void*)&v[1].Tail) return 5;
        if ((void*)&v[0].I.A != (void*)((byte*)v + 8)) return 6;
        if ((void*)&v[1].I.P != (void*)&v[1].L) return 7;

        return 0;
    }
}

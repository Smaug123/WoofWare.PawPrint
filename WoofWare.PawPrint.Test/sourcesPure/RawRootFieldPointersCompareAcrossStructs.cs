using System.Runtime.InteropServices;

// Addresses of fields of struct pointers into stackalloc'd or natively allocated memory, compared
// as pointers: the same field or different fields, of the same struct or of different structs,
// and the address of a field against a flat pointer to the same byte.

struct T
{
    public int A;
    public int B;
}

unsafe class Program
{
    static int Check(T* p)
    {
        if (&p[0].A == &p[1].B) return 1;
        if (&p[0].B == &p[1].A) return 2;
        if (&p[1].A != &p[1].A) return 3;
        if (&p[0].B != (int*)((byte*)p + 4)) return 4;
        if (&p[1].A != (int*)((byte*)p + 8)) return 5;
        if (&p[1].A == (int*)((byte*)p + 12)) return 6;

        int sum = 0;
        for (int* q = &p[0].A; q != &p[2].A; q += 2) sum += *q;
        if (sum != 1 + 3) return 7;

        return 0;
    }

    static int Main(string[] args)
    {
        T* s = stackalloc T[3];
        s[0] = new T { A = 1, B = 2 };
        s[1] = new T { A = 3, B = 4 };
        int r = Check(s);
        if (r != 0) return r;

        T* n = (T*)NativeMemory.AllocZeroed((nuint)(3 * sizeof(T)));
        try
        {
            n[0] = new T { A = 1, B = 2 };
            n[1] = new T { A = 3, B = 4 };
            r = Check(n);
            if (r != 0) return 10 + r;
        }
        finally
        {
            NativeMemory.Free(n);
        }

        return 0;
    }
}

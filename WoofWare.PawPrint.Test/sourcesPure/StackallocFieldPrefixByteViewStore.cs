using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

// `p->I` through a pointer into stackalloc'd or natively allocated memory is a byref whose root is
// raw byte storage and whose projections begin with a structural `Field` step. A byte-view store
// through it (`Unsafe.As<Inner, T>(ref p->I)`) lands inside `Inner`, which holds a pointer and so
// has no byte image: the store has to name the cell it covers rather than splice bytes into
// `Inner`. Everything asserted here is a value read back through the struct's own fields, so it
// holds identically on the real runtime.

unsafe struct Inner
{
    public byte Lead;
    public int Cell;
    public int* P;
}

// The same leading layout as `Inner`, so that `Unsafe.As<Inner, Shadow>(ref ...).Cell` addresses
// `Inner.Cell` through a `Field` step taken *after* the reinterpret.
struct Shadow
{
    public byte Lead;
    public int Cell;
}

struct Outer
{
    public Inner I;
}

unsafe class Program
{
    static int Check(Outer* p, int* target)
    {
        *p = new Outer { I = new Inner { Lead = 1, Cell = 2, P = target } };

        // `stind.i1` through `[Field I; ReinterpretAs byte]`, naming `Lead`.
        Unsafe.As<Inner, byte>(ref p->I) = 42;

        if (p->I.Lead != 42) return 1;
        if (p->I.Cell != 2) return 2;
        if (p->I.P != target) return 3;

        // `stfld` through `[Field I; ReinterpretAs Shadow; Field Cell]`, naming `Cell` at a
        // non-zero offset.
        Unsafe.As<Inner, Shadow>(ref p->I).Cell = 99;

        if (p->I.Lead != 42) return 4;
        if (p->I.Cell != 99) return 5;
        if (p->I.P != target) return 6;
        if (*p->I.P != 7) return 7;

        return 0;
    }

    static int Main(string[] args)
    {
        int x = 7;

        Outer* stack = stackalloc Outer[1];
        int result = Check(stack, &x);

        if (result != 0) return result;

        Outer* native = (Outer*) NativeMemory.AllocZeroed ((nuint) sizeof (Outer));

        try
        {
            result = Check(native, &x);

            if (result != 0) return 10 + result;
        }
        finally
        {
            NativeMemory.Free (native);
        }

        return 0;
    }
}

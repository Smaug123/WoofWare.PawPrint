using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

// The shape of `RawRootFieldPrefixByteOffset.cs` over a struct holding a pointer, which has no byte
// image: a load or store through `Unsafe.Add(ref Unsafe.As<Inner, int>(ref p->I), k)` has to name
// the field of `Inner` it lands on. `P` occupies bytes 0..7 of `Inner`, so `A` is at `int` index 2
// and `B` at 3.

unsafe struct Inner
{
    public int* P;
    public int A;
    public int B;
}

unsafe struct Outer
{
    public int Lead;
    public Inner I;
}

unsafe struct Nest
{
    public long Head;
    public Outer O;
}

unsafe class Program
{
    static int Check(ref Inner inner, int* target)
    {
        ref int i0 = ref Unsafe.As<Inner, int>(ref inner);

        if (Unsafe.Add(ref i0, 2) != 3) return 1;
        if (Unsafe.Add(ref i0, 3) != 4) return 2;
        Unsafe.Add(ref i0, 2) = 42;
        Unsafe.Add(ref i0, 3) = 43;
        if (inner.A != 42) return 3;
        if (inner.B != 43) return 4;
        if (inner.P != target) return 5;
        if (*inner.P != 7) return 6;

        return 0;
    }

    static int Main(string[] args)
    {
        int x = 7;

        Outer* stack = stackalloc Outer[1];
        *stack = new Outer { Lead = 1, I = new Inner { P = &x, A = 3, B = 4 } };
        int result = Check(ref stack->I, &x);
        if (result != 0) return result;
        if (stack->Lead != 1) return 9;

        Nest* stackNest = stackalloc Nest[1];
        *stackNest = new Nest { Head = 100, O = new Outer { Lead = 1, I = new Inner { P = &x, A = 3, B = 4 } } };
        result = Check(ref stackNest->O.I, &x);
        if (result != 0) return 10 + result;
        if (stackNest->Head != 100) return 19;

        Outer* native = (Outer*) NativeMemory.AllocZeroed ((nuint) sizeof (Outer));
        Nest* nativeNest = (Nest*) NativeMemory.AllocZeroed ((nuint) sizeof (Nest));

        try
        {
            *native = new Outer { Lead = 1, I = new Inner { P = &x, A = 3, B = 4 } };
            result = Check(ref native->I, &x);
            if (result != 0) return 20 + result;

            *nativeNest = new Nest { Head = 100, O = new Outer { Lead = 1, I = new Inner { P = &x, A = 3, B = 4 } } };
            result = Check(ref nativeNest->O.I, &x);
            if (result != 0) return 30 + result;
        }
        finally
        {
            NativeMemory.Free (native);
            NativeMemory.Free (nativeNest);
        }

        return 0;
    }
}

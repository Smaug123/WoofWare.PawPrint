using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

// `p->I` through a pointer into stackalloc'd or natively allocated memory is a byref whose root is
// raw byte storage and whose projections begin with a structural `Field` step. Viewing that field as
// `int` and stepping with `Unsafe.Add`/`Unsafe.Subtract` must reach whatever lies at the resulting
// address -- a later field of `Inner`, a sibling of `Inner` in `Outer` on either side, the next
// `Outer` in the block, or a byte in the middle of a field -- for loads and stores alike.
//
// `SequentialLayout` puts `Inner` at 8 in `Outer` (its `long` aligns it), so relative to `I` in
// `int`s: `Lead` is -2, `Pad` -1, `A` +2, `B` +3, `Tail` +4, `Last` +5, and the next `Outer`'s
// `Lead` +6.

struct Inner
{
    public long P;
    public int A;
    public int B;
}

struct Outer
{
    public int Lead;
    public int Pad;
    public Inner I;
    public int Tail;
    public int Last;
}

struct Nest
{
    public long Head;
    public Outer O;
}

unsafe class Program
{
    static int Check(Outer* p)
    {
        p[0] = new Outer { Lead = 1, Pad = 2, I = new Inner { P = 7, A = 3, B = 4 }, Tail = 5, Last = 6 };
        p[1] = new Outer { Lead = 70, Pad = 80 };

        ref int i0 = ref Unsafe.As<Inner, int>(ref p->I);

        // Offsets landing exactly on a later field of `Inner`.
        if (Unsafe.Add(ref i0, 2) != 3) return 1;
        if (Unsafe.Add(ref i0, 3) != 4) return 2;
        Unsafe.Add(ref i0, 2) = 42;
        Unsafe.Add(ref i0, 3) = 43;
        if (p->I.A != 42) return 3;
        if (p->I.B != 43) return 4;

        // Past the end of `Inner`, into its siblings in `Outer`.
        if (Unsafe.Add(ref i0, 4) != 5) return 5;
        if (Unsafe.Add(ref i0, 5) != 6) return 6;
        Unsafe.Add(ref i0, 4) = 50;
        Unsafe.Add(ref i0, 5) = 60;
        if (p->Tail != 50) return 7;
        if (p->Last != 60) return 8;

        // Backwards, before the start of `Inner`.
        if (Unsafe.Subtract(ref i0, 1) != 2) return 9;
        if (Unsafe.Subtract(ref i0, 2) != 1) return 10;
        Unsafe.Subtract(ref i0, 1) = 20;
        Unsafe.Subtract(ref i0, 2) = 10;
        if (p->Pad != 20) return 11;
        if (p->Lead != 10) return 12;

        // From a later field back to an earlier one.
        ref int b = ref p->I.B;
        if (Unsafe.Subtract(ref b, 1) != 42) return 13;
        Unsafe.Subtract(ref b, 1) = 44;
        if (p->I.A != 44) return 14;

        // Past the end of `Outer`, into the next element of the block.
        if (Unsafe.Add(ref i0, 6) != 70) return 15;
        Unsafe.Add(ref i0, 7) = 81;
        if (p[1].Pad != 81) return 16;
        if (p[1].Lead != 70) return 17;

        // A byte in the middle of `A`, rather than the start of any field.
        ref byte ib = ref Unsafe.As<Inner, byte>(ref p->I);
        if (Unsafe.AddByteOffset(ref ib, 8) != 44) return 18;
        Unsafe.AddByteOffset(ref ib, 9) = 1;
        if (p->I.A != 300) return 19;
        if (Unsafe.AddByteOffset(ref ib, 9) != 1) return 20;

        if (p->I.P != 7) return 21;
        if (p->I.B != 43) return 22;

        return 0;
    }

    static int CheckNested(Nest* n)
    {
        *n = new Nest { Head = 100, O = new Outer { Lead = 1, Pad = 2, I = new Inner { P = 7, A = 3, B = 4 }, Tail = 5, Last = 6 } };

        // Two structural `Field` steps before the reinterpret.
        ref int i0 = ref Unsafe.As<Inner, int>(ref n->O.I);

        if (Unsafe.Add(ref i0, 2) != 3) return 1;
        Unsafe.Add(ref i0, 3) = 47;
        if (n->O.I.B != 47) return 2;
        if (Unsafe.Add(ref i0, 4) != 5) return 3;
        if (Unsafe.Subtract(ref i0, 2) != 1) return 4;
        Unsafe.Subtract(ref i0, 1) = 22;
        if (n->O.Pad != 22) return 5;

        // Before the start of `Outer`, into `Nest.Head`.
        if (Unsafe.Subtract(ref i0, 4) != 100) return 6;
        Unsafe.Subtract(ref i0, 4) = 101;
        if (n->Head != 101) return 7;

        if (n->O.I.P != 7) return 8;

        return 0;
    }

    static int Main(string[] args)
    {
        Outer* stack = stackalloc Outer[2];
        int result = Check(stack);
        if (result != 0) return result;

        Nest* stackNest = stackalloc Nest[1];
        result = CheckNested(stackNest);
        if (result != 0) return 30 + result;

        Outer* native = (Outer*) NativeMemory.AllocZeroed ((nuint) (2 * sizeof (Outer)));
        Nest* nativeNest = (Nest*) NativeMemory.AllocZeroed ((nuint) sizeof (Nest));

        try
        {
            result = Check(native);
            if (result != 0) return 40 + result;

            result = CheckNested(nativeNest);
            if (result != 0) return 70 + result;
        }
        finally
        {
            NativeMemory.Free (native);
            NativeMemory.Free (nativeNest);
        }

        return 0;
    }
}

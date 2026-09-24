using System.Runtime.CompilerServices;

// A byte view taken over a field of a struct holding a pointer, then stepped out of that field:
// past the end of `Inner`, before its start, and from `B` back to `A`. The struct has no byte
// image, so each access has to name the field of the enclosing struct it lands on. The struct
// lives in a local and in a class field.

unsafe struct Inner
{
    public int* P;
    public int A;
    public int B;
}

unsafe struct Outer
{
    public int Lead;
    public int Pad;
    public Inner I;
    public int Tail;
    public int Last;
}

unsafe struct Nest
{
    public long Head;
    public Outer O;
}

class Holder
{
    public Outer O;
    public Nest N;
}

unsafe class Program
{
    static int Check(ref Outer o, int* target)
    {
        o = new Outer { Lead = 1, Pad = 2, I = new Inner { P = target, A = 3, B = 4 }, Tail = 5, Last = 6 };

        ref int i0 = ref Unsafe.As<Inner, int>(ref o.I);

        // Offsets landing exactly on a later field of `Inner`.
        if (Unsafe.Add(ref i0, 2) != 3) return 1;
        if (Unsafe.Add(ref i0, 3) != 4) return 2;
        Unsafe.Add(ref i0, 2) = 42;
        Unsafe.Add(ref i0, 3) = 43;
        if (o.I.A != 42) return 3;
        if (o.I.B != 43) return 4;

        // Past the end of `Inner`, into its siblings in `Outer`.
        if (Unsafe.Add(ref i0, 4) != 5) return 5;
        if (Unsafe.Add(ref i0, 5) != 6) return 6;
        Unsafe.Add(ref i0, 4) = 50;
        Unsafe.Add(ref i0, 5) = 60;
        if (o.Tail != 50) return 7;
        if (o.Last != 60) return 8;

        // Backwards, before the start of `Inner`.
        if (Unsafe.Subtract(ref i0, 1) != 2) return 9;
        if (Unsafe.Subtract(ref i0, 2) != 1) return 10;
        Unsafe.Subtract(ref i0, 1) = 20;
        Unsafe.Subtract(ref i0, 2) = 10;
        if (o.Pad != 20) return 11;
        if (o.Lead != 10) return 12;

        // From a later field back to an earlier one.
        ref int b = ref o.I.B;
        if (Unsafe.Subtract(ref b, 1) != 42) return 13;
        Unsafe.Subtract(ref b, 1) = 44;
        if (o.I.A != 44) return 14;

        if (o.I.P != target) return 15;
        if (*o.I.P != 7) return 16;

        return 0;
    }

    static int CheckNested(ref Nest n, int* target)
    {
        n = new Nest { Head = 100, O = new Outer { Lead = 1, Pad = 2, I = new Inner { P = target, A = 3, B = 4 }, Tail = 5, Last = 6 } };

        // Two structural `Field` steps before the reinterpret.
        ref int i0 = ref Unsafe.As<Inner, int>(ref n.O.I);

        if (Unsafe.Add(ref i0, 2) != 3) return 1;
        Unsafe.Add(ref i0, 3) = 47;
        if (n.O.I.B != 47) return 2;
        if (Unsafe.Add(ref i0, 4) != 5) return 3;
        if (Unsafe.Subtract(ref i0, 2) != 1) return 4;
        Unsafe.Subtract(ref i0, 1) = 22;
        if (n.O.Pad != 22) return 5;
        if (n.Head != 100) return 6;
        if (n.O.I.P != target) return 7;

        return 0;
    }

    static int Main(string[] args)
    {
        int x = 7;

        Outer local = default;
        int result = Check(ref local, &x);
        if (result != 0) return result;

        Nest localNest = default;
        result = CheckNested(ref localNest, &x);
        if (result != 0) return 20 + result;

        Holder holder = new Holder();
        result = Check(ref holder.O, &x);
        if (result != 0) return 40 + result;

        result = CheckNested(ref holder.N, &x);
        if (result != 0) return 60 + result;
        if (holder.O.Tail != 50) return 77;

        return 0;
    }
}

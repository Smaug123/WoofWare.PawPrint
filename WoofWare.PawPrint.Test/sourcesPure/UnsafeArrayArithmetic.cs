using System.Runtime.CompilerServices;

public class TestUnsafeArrayArithmetic
{
    // Unsafe.Add by positive element offset on a non-reinterpreted byref into an int[].
    public static int Test1()
    {
        int[] a = { 10, 20, 30, 40 };
        ref int p = ref a[1];
        ref int q = ref Unsafe.Add(ref p, 2);
        if (q != 40)
            return 1;
        return 0;
    }

    // Unsafe.Add with element offset zero: identity.
    public static int Test2()
    {
        int[] a = { 10, 20, 30 };
        ref int q = ref Unsafe.Add(ref a[1], 0);
        if (q != 20)
            return 2;
        return 0;
    }

    // Unsafe.Add with a negative element offset.
    public static int Test3()
    {
        int[] a = { 10, 20, 30 };
        ref int q = ref Unsafe.Add(ref a[2], -1);
        if (q != 20)
            return 3;
        return 0;
    }

    // Write through a byref obtained via Unsafe.Add.
    public static int Test4()
    {
        int[] a = { 10, 20, 30 };
        ref int q = ref Unsafe.Add(ref a[0], 1);
        q = 222;
        if (a[1] != 222)
            return 4;
        if (a[0] != 10)
            return 5;
        if (a[2] != 30)
            return 6;
        return 0;
    }

    // Unsafe.AreSame: two byrefs into the same element compare equal; different indices unequal.
    public static int Test5()
    {
        int[] a = new int[4];
        if (!Unsafe.AreSame(ref a[0], ref a[0]))
            return 7;
        if (Unsafe.AreSame(ref a[0], ref a[1]))
            return 8;
        return 0;
    }

    // Unsafe.AreSame sees the result of Unsafe.Add as the expected element.
    public static int Test6()
    {
        int[] a = new int[4];
        ref int p = ref a[1];
        ref int q = ref Unsafe.Add(ref a[0], 1);
        if (!Unsafe.AreSame(ref p, ref q))
            return 9;
        if (Unsafe.AreSame(ref p, ref a[2]))
            return 10;
        return 0;
    }

    // Unsafe.AreSame comparing byrefs into two distinct arrays should return false.
    public static int Test7()
    {
        int[] a = new int[2];
        int[] b = new int[2];
        if (Unsafe.AreSame(ref a[0], ref b[0]))
            return 11;
        return 0;
    }

    // Unsafe.ByteOffset within the same array.
    public static int Test8()
    {
        int[] a = new int[4];
        System.IntPtr offset = Unsafe.ByteOffset(ref a[0], ref a[1]);
        if (offset != (System.IntPtr)sizeof(int))
            return 12;
        System.IntPtr zero = Unsafe.ByteOffset(ref a[1], ref a[1]);
        if (zero != System.IntPtr.Zero)
            return 13;
        return 0;
    }

    // Unsafe.As<T,T> is a no-op; AreSame should see the result as aliasing the input.
    public static int Test9()
    {
        int[] a = new int[1];
        ref int p = ref a[0];
        ref int q = ref Unsafe.As<int, int>(ref p);
        if (!Unsafe.AreSame(ref p, ref q))
            return 14;
        return 0;
    }

    // Consecutive Unsafe.As reinterpretations don't move the address: two byrefs that
    // reach the same final type view by different chains should AreSame.
    public static int Test10()
    {
        int[] a = new int[1];
        ref int x = ref a[0];
        ref uint via = ref Unsafe.As<int, uint>(ref x);
        ref short y = ref Unsafe.As<uint, short>(ref via);
        ref short z = ref Unsafe.As<int, short>(ref x);
        if (!Unsafe.AreSame(ref y, ref z))
            return 15;
        return 0;
    }

    // Round-trip Unsafe.As returns to the original natural type view; the
    // resulting byref should still be recognised as the same as the original.
    public static int Test11()
    {
        int[] a = new int[1];
        ref int x = ref a[0];
        ref uint u = ref Unsafe.As<int, uint>(ref x);
        ref int back = ref Unsafe.As<uint, int>(ref u);
        if (!Unsafe.AreSame(ref x, ref back))
            return 16;
        return 0;
    }

    // ByteOffset between two distinct arrays returns some value. The cross-array
    // case must not throw, and ByteOffset must be anti-symmetric: ByteOffset(a, b)
    // is the negation of ByteOffset(b, a), for any two byrefs.
    public static int Test12()
    {
        int[] a = new int[4];
        int[] b = new int[4];
        long forward = (long)Unsafe.ByteOffset(ref a[0], ref b[0]);
        long backward = (long)Unsafe.ByteOffset(ref b[0], ref a[0]);
        if (forward + backward != 0L)
            return 17;
        // ByteOffset from a byref to itself must still be zero.
        System.IntPtr self = Unsafe.ByteOffset(ref b[2], ref b[2]);
        if (self != System.IntPtr.Zero)
            return 18;
        return 0;
    }

    // A negative Unsafe.ByteOffset cast to nuint must not throw. Any same-array
    // order is valid, so ByteOffset(a[0], a[N]) and ByteOffset(a[N], a[0]) are
    // negatives of each other; casting either to nuint should work. This is the
    // shape of the overlap check in SpanHelpers.Memmove.
    public static int Test13()
    {
        int[] a = new int[4];
        System.IntPtr delta = Unsafe.ByteOffset(ref a[3], ref a[0]);
        // The cast below is a Conv_U on a negative native int. Before the fix
        // this threw inside the interpreter.
        nuint unsignedDelta = (nuint)delta;
        if ((long)delta >= 0L)
            return 19;
        // The unsigned value must be very large (no legitimate array length
        // reaches near UInt64.MaxValue), so the overlap check fails as it
        // should for the forward-copy branch of Memmove.
        if (unsignedDelta < (nuint)1000)
            return 20;
        return 0;
    }

    // Arithmetic on a byref obtained via a round-trip Unsafe.As must behave
    // identically to arithmetic on the bare byref. Exercises the interaction
    // between the Unsafe.As canonicalisation and Unsafe.Add/ByteOffset.
    public static int Test14()
    {
        int[] a = { 10, 20, 30, 40 };
        ref int x = ref a[0];
        ref uint u = ref Unsafe.As<int, uint>(ref x);
        ref int back = ref Unsafe.As<uint, int>(ref u);
        ref int q = ref Unsafe.Add(ref back, 2);
        if (q != 30)
            return 21;
        System.IntPtr off = Unsafe.ByteOffset(ref back, ref a[3]);
        if ((long)off != 3L * sizeof(int))
            return 22;
        return 0;
    }

    // A size-preserving Unsafe.As (int -> uint) and arithmetic through the
    // reinterpreted byref. The view type changes but the address step is still
    // over the underlying int storage.
    public static int Test15()
    {
        int[] a = { 100, 200, 300, 400 };
        ref uint u0 = ref Unsafe.As<int, uint>(ref a[0]);
        ref uint u2 = ref Unsafe.Add(ref u0, 2);
        if (u2 != 300u)
            return 23;
        // ByteOffset between two reinterpreted views must match the underlying
        // byte stride (sizeof(int)), not sizeof(uint) coincidentally.
        System.IntPtr off = Unsafe.ByteOffset(ref u0, ref u2);
        if ((long)off != 2L * sizeof(int))
            return 24;
        return 0;
    }

    // Writing through a size-preserving reinterpret (int <-> uint) must land
    // in the underlying storage. Read-only reinterprets already worked; this
    // exercises the write path that previously threw inside applyProjectionsForWrite.
    public static int Test16()
    {
        int[] a = { 10, 20, 30, 40 };
        ref uint u = ref Unsafe.As<int, uint>(ref a[1]);
        u = 0xDEADBEEFu;
        if (a[1] != unchecked((int)0xDEADBEEFu))
            return 25;
        if (a[0] != 10)
            return 26;
        if (a[2] != 30)
            return 27;
        // Round-trip As should also support writes: arithmetic through a
        // reinterpreted byref followed by a store must land in the right slot.
        ref int back = ref Unsafe.As<uint, int>(ref u);
        ref int target = ref Unsafe.Add(ref back, 1);
        target = 999;
        if (a[2] != 999)
            return 28;
        return 0;
    }

    // Unsafe.ByteOffset on two byrefs into the same empty array must return
    // zero rather than throw. Zero-length span helpers call this path after
    // going through MemoryMarshal.GetArrayDataReference on an empty array.
    public static int Test17()
    {
        int[] empty = new int[0];
        ref int r1 = ref System.Runtime.InteropServices.MemoryMarshal.GetArrayDataReference(empty);
        ref int r2 = ref System.Runtime.InteropServices.MemoryMarshal.GetArrayDataReference(empty);
        System.IntPtr off = Unsafe.ByteOffset(ref r1, ref r2);
        if (off != System.IntPtr.Zero)
            return 29;
        return 0;
    }

    public static int Main(string[] argv)
    {
        int r = Test1();
        if (r != 0) return r;
        r = Test2();
        if (r != 0) return r;
        r = Test3();
        if (r != 0) return r;
        r = Test4();
        if (r != 0) return r;
        r = Test5();
        if (r != 0) return r;
        r = Test6();
        if (r != 0) return r;
        r = Test7();
        if (r != 0) return r;
        r = Test8();
        if (r != 0) return r;
        r = Test9();
        if (r != 0) return r;
        r = Test10();
        if (r != 0) return r;
        r = Test11();
        if (r != 0) return r;
        r = Test12();
        if (r != 0) return r;
        r = Test13();
        if (r != 0) return r;
        r = Test14();
        if (r != 0) return r;
        r = Test15();
        if (r != 0) return r;
        r = Test16();
        if (r != 0) return r;
        r = Test17();
        if (r != 0) return r;
        return 0;
    }
}

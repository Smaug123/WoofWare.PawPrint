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
        return 0;
    }
}

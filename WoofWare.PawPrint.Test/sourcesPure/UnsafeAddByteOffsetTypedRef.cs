using System;
using System.Runtime.CompilerServices;

public class TestUnsafeAddByteOffsetTypedRef
{
    // Whole-cell byte offsets on a typed byref over an array of non-byte-addressable
    // elements (here: object references) must yield a typed byref to the next cell,
    // not a byte-view byref that an `ldind.ref` would then fail to dereference.
    public static int Test1()
    {
        string[] a = { "hello", "world", "!" };
        ref string r0 = ref a[0];
        ref string r1 = ref Unsafe.AddByteOffset(ref r0, (IntPtr)IntPtr.Size);
        if (!ReferenceEquals(r1, "world"))
            return 1;
        return 0;
    }

    // Same arithmetic via the UIntPtr overload.
    public static int Test2()
    {
        string[] a = { "alpha", "beta", "gamma" };
        ref string r0 = ref a[0];
        ref string r2 = ref Unsafe.AddByteOffset(ref r0, (nuint)(2 * IntPtr.Size));
        if (!ReferenceEquals(r2, "gamma"))
            return 2;
        return 0;
    }

    // Zero-byte offset on a typed byref must be the identity for ldind.ref.
    public static int Test3()
    {
        string[] a = { "x", "y" };
        ref string r0 = ref a[0];
        ref string same = ref Unsafe.AddByteOffset(ref r0, (IntPtr)0);
        if (!ReferenceEquals(same, "x"))
            return 3;
        return 0;
    }

    // Whole-cell byref equality: AreSame on the typed byref produced by AddByteOffset
    // must agree with a direct `ref a[i]`.
    public static int Test4()
    {
        string[] a = { "p", "q", "r" };
        ref string r0 = ref a[0];
        ref string r1 = ref Unsafe.AddByteOffset(ref r0, (IntPtr)IntPtr.Size);
        if (!Unsafe.AreSame(ref a[1], ref r1))
            return 4;
        return 0;
    }

    // Structural T (here: an array type) must reach the shortcut without requiring
    // the concrete-type registry to carry an array handle — `AllConcreteTypes.lookup`
    // intentionally doesn't store array/pointer/function-pointer handles, so the
    // shortcut must avoid that lookup for whole-cell moves.
    public static int Test5()
    {
        int[][] a = { new[] { 1, 2 }, new[] { 3, 4 }, new[] { 5, 6 } };
        ref int[] r0 = ref a[0];
        ref int[] r1 = ref Unsafe.AddByteOffset(ref r0, (IntPtr)IntPtr.Size);
        if (r1[0] != 3 || r1[1] != 4)
            return 5;
        return 0;
    }

    // Identity case for structural T: zero-byte AddByteOffset on a typed byref to
    // an array element of array type must return the same byref shape.
    public static int Test6()
    {
        int[][] a = { new[] { 7, 8 } };
        ref int[] r0 = ref a[0];
        ref int[] same = ref Unsafe.AddByteOffset(ref r0, (IntPtr)0);
        if (same[0] != 7 || same[1] != 8)
            return 6;
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
        return 0;
    }
}

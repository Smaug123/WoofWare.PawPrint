using System;
using System.Runtime.CompilerServices;

public class NativeIntComparisons
{
    // Force nativeint comparisons by going through IntPtr.
    [MethodImpl(MethodImplOptions.NoInlining)]
    static bool GreaterThan(IntPtr a, IntPtr b)
    {
        // This compiles to cgt on nativeint operands.
        return (nint)a > (nint)b;
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    static bool LessThan(IntPtr a, IntPtr b)
    {
        return (nint)a < (nint)b;
    }

    // Test that cgt gives correct results for nativeint.
    static int TestNativeIntCgt()
    {
        IntPtr big = (IntPtr)10;
        IntPtr small = (IntPtr)3;
        IntPtr same = (IntPtr)10;

        // 10 > 3 should be true
        if (!GreaterThan(big, small)) return 1;

        // 3 > 10 should be false
        if (GreaterThan(small, big)) return 2;

        // 10 > 10 should be false
        if (GreaterThan(big, same)) return 3;

        // 3 < 10 should be true
        if (!LessThan(small, big)) return 4;

        // 10 < 3 should be false
        if (LessThan(big, small)) return 5;

        return 0;
    }

    // Test cgt.un with object references and null.
    static int TestCgtUnNullVsObj()
    {
        object obj = new object();
        object nul = null;

        // null == null should be true
        if (nul != null) return 10;

        // obj != null should be true
        if (obj == null) return 11;

        // (null != obj) should be true -- null is not the same as obj
        // More specifically, for cgt.un:
        // null > obj should be false (null is the zero pointer)
        // obj > null should be true

        // This line compiles to cgt.un: (obj != null) is implemented as cgt.un(obj, ldnull)
        // so obj > null in unsigned sense should be true.
        if (!(obj != null)) return 12;

        return 0;
    }

    public static int Main(string[] argv)
    {
        int result;

        result = TestNativeIntCgt();
        if (result != 0) return 100 + result;

        result = TestCgtUnNullVsObj();
        if (result != 0) return 200 + result;

        return 0;
    }
}

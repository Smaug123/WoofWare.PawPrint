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

    // cgt.un with object references is tested in CgtUn.cs, which uses NoInlining helpers
    // to ensure the compiler materialises the cgt.un result rather than folding it into a branch.

    public static int Main(string[] argv)
    {
        int result;

        result = TestNativeIntCgt();
        if (result != 0) return 100 + result;

        return 0;
    }
}

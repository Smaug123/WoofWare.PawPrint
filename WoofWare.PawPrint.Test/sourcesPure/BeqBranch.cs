using System;
using System.Runtime.CompilerServices;

public class BeqBranch
{
    // NoInlining helpers force the compiler to emit beq/bne branch IL
    // rather than folding the comparison into a ceq + brtrue pattern.

    [MethodImpl(MethodImplOptions.NoInlining)]
    static int CompareInts(int a, int b)
    {
        // if (a == b) compiles to beq or beq.s
        if (a == b) return 1;
        return 0;
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    static int CompareLongs(long a, long b)
    {
        if (a == b) return 1;
        return 0;
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    static int CompareNativeInts(IntPtr a, IntPtr b)
    {
        if (a == b) return 1;
        return 0;
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    static int CompareObjects(object a, object b)
    {
        // Object reference equality: beq on O type
        if (a == b) return 1;
        return 0;
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    static int CompareFloats(float a, float b)
    {
        if (a == b) return 1;
        return 0;
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    static int CompareDoubles(double a, double b)
    {
        if (a == b) return 1;
        return 0;
    }

    static int TestIntEquality()
    {
        if (CompareInts(5, 5) != 1) return 1;
        if (CompareInts(5, 6) != 0) return 2;
        if (CompareInts(-1, -1) != 1) return 3;
        if (CompareInts(-1, 1) != 0) return 4;
        if (CompareInts(0, 0) != 1) return 5;
        if (CompareInts(int.MaxValue, int.MaxValue) != 1) return 6;
        if (CompareInts(int.MinValue, int.MaxValue) != 0) return 7;
        return 0;
    }

    static int TestLongEquality()
    {
        if (CompareLongs(100L, 100L) != 1) return 1;
        if (CompareLongs(100L, 200L) != 0) return 2;
        if (CompareLongs(-1L, -1L) != 1) return 3;
        if (CompareLongs(long.MaxValue, long.MaxValue) != 1) return 4;
        return 0;
    }

    static int TestNativeIntEquality()
    {
        IntPtr a = (IntPtr)42;
        IntPtr b = (IntPtr)42;
        IntPtr c = (IntPtr)99;

        if (CompareNativeInts(a, b) != 1) return 1;
        if (CompareNativeInts(a, c) != 0) return 2;
        if (CompareNativeInts((IntPtr)0, (IntPtr)0) != 1) return 3;
        return 0;
    }

    static int TestObjectEquality()
    {
        object x = new object();
        object y = new object();

        // Same reference
        if (CompareObjects(x, x) != 1) return 1;
        // Different references
        if (CompareObjects(x, y) != 0) return 2;
        // Null == null
        if (CompareObjects(null, null) != 1) return 3;
        // Null != non-null
        if (CompareObjects(null, x) != 0) return 4;
        if (CompareObjects(x, null) != 0) return 5;
        return 0;
    }

    static int TestFloatEquality()
    {
        if (CompareFloats(1.5f, 1.5f) != 1) return 1;
        if (CompareFloats(1.5f, 2.5f) != 0) return 2;
        if (CompareFloats(0.0f, 0.0f) != 1) return 3;
        if (CompareFloats(-1.0f, -1.0f) != 1) return 4;
        if (CompareFloats(-1.0f, 1.0f) != 0) return 5;
        return 0;
    }

    static int TestDoubleEquality()
    {
        if (CompareDoubles(3.14, 3.14) != 1) return 1;
        if (CompareDoubles(3.14, 2.72) != 0) return 2;
        if (CompareDoubles(0.0, 0.0) != 1) return 3;
        return 0;
    }

    public static int Main(string[] argv)
    {
        int result;

        result = TestIntEquality();
        if (result != 0) return 100 + result;

        result = TestLongEquality();
        if (result != 0) return 200 + result;

        result = TestNativeIntEquality();
        if (result != 0) return 300 + result;

        result = TestObjectEquality();
        if (result != 0) return 400 + result;

        result = TestFloatEquality();
        if (result != 0) return 500 + result;

        result = TestDoubleEquality();
        if (result != 0) return 600 + result;

        return 0;
    }
}

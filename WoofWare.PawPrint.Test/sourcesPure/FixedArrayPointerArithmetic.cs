using System;

public unsafe class FixedArrayPointerArithmetic
{
    // After `fixed (T* p = arr)`, the pointer is in native-pointer-world: pointer
    // arithmetic over it must use byte stride (ECMA-335 §III.1.5), not element
    // stride. The C# compiler emits `sizeof(T) * k` explicitly for `p + k` and
    // `p++`. The fix ensures Conv_U/Conv_I on a plain array byref anchors a
    // `ReinterpretAs T` projection so subsequent `add` uses byte arithmetic.

    public static int TestIntArrayWalk()
    {
        int[] arr = new int[] { 10, 20, 30, 40, 50 };
        fixed (int* arrPtr = arr)
        {
            int* p = arrPtr;
            if (*p != 10) return 1;

            p++;
            if (*p != 20) return 2;

            p += 2;
            if (*p != 40) return 3;

            *p = 400;
        }
        if (arr[3] != 400) return 4;
        if (arr[0] != 10) return 5;
        if (arr[2] != 30) return 6;
        return 0;
    }

    public static int TestIntArrayIndex()
    {
        int[] arr = new int[] { 100, 200, 300, 400, 500 };
        fixed (int* arrPtr = arr)
        {
            if (arrPtr[0] != 100) return 10;
            if (arrPtr[1] != 200) return 11;
            if (arrPtr[4] != 500) return 12;
            arrPtr[2] = 333;
            arrPtr[3] = 444;
        }
        if (arr[2] != 333) return 13;
        if (arr[3] != 444) return 14;
        if (arr[0] != 100) return 15;
        return 0;
    }

    public static int TestLongArrayWalk()
    {
        long[] arr = new long[] { 1L, 2L, 3L, 4L };
        fixed (long* arrPtr = arr)
        {
            long* p = arrPtr;
            if (*p != 1L) return 20;
            p += 3;
            if (*p != 4L) return 21;
            *p = 999L;
            p -= 2;
            if (*p != 2L) return 22;
        }
        if (arr[3] != 999L) return 23;
        if (arr[1] != 2L) return 24;
        return 0;
    }

    public static int TestIntPtrArrayWalk()
    {
        IntPtr[] arr = new IntPtr[] { new IntPtr(11), new IntPtr(22), new IntPtr(33) };
        fixed (IntPtr* arrPtr = arr)
        {
            if (arrPtr[0] != new IntPtr(11)) return 30;
            if (arrPtr[2] != new IntPtr(33)) return 31;
            arrPtr[1] = new IntPtr(222);
        }
        if (arr[1] != new IntPtr(222)) return 32;
        return 0;
    }

    // Pointer-to-pointer array: element type is structural (`ConcreteTypeHandle.Pointer Int32`),
    // which is intentionally not registered in `AllConcreteTypes`. The Conv_U/Conv_I anchor
    // must therefore tolerate structural element handles instead of failing the lookup.
    public static int TestIntPointerArrayWalk()
    {
        int a = 7;
        int b = 8;
        int c = 9;
        int*[] arr = new int*[] { &a, &b, &c };
        fixed (int** arrPtr = arr)
        {
            if (*arrPtr[0] != 7) return 40;
            if (*arrPtr[1] != 8) return 41;
            if (*arrPtr[2] != 9) return 42;

            int** p = arrPtr;
            if (*(*p) != 7) return 43;
            p++;
            if (*(*p) != 8) return 44;
            p += 1;
            if (*(*p) != 9) return 45;
        }
        return 0;
    }

    public static int Main(string[] argv)
    {
        int r;
        r = TestIntArrayWalk();
        if (r != 0) return r;
        r = TestIntArrayIndex();
        if (r != 0) return r;
        r = TestLongArrayWalk();
        if (r != 0) return r;
        r = TestIntPtrArrayWalk();
        if (r != 0) return r;
        r = TestIntPointerArrayWalk();
        if (r != 0) return r;
        return 0;
    }
}

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
    // which is intentionally not registered in `AllConcreteTypes`. The Conv_U/Conv_I byte-view
    // anchor must therefore tolerate structural element handles instead of failing the lookup.
    // We only exercise the `ldelema int*; conv.u` shape here — reading or doing pointer
    // arithmetic through the resulting pointer is future work because pointer-array cells
    // carry non-byte-addressable provenance that the byte-view machinery can't slice yet.
    public static int TestIntPointerArrayFixed()
    {
        int a = 7;
        int b = 8;
        int*[] arr = new int*[] { &a, &b };
        fixed (int** arrPtr = arr)
        {
            if (arrPtr == null) return 40;
        }
        return 0;
    }

    // Storing a provenance-bearing native int (here a RuntimeTypeHandle's
    // TypeHandlePtr value) through a `fixed` array pointer must preserve
    // the value's provenance. The Conv_U/Conv_I byte-view anchor must not
    // force these stores through the byte-scatter path, which refuses
    // non-byte-renderable native ints. Reads back through `arr[i]` then
    // recover the typed cell.
    public static int TestIntPtrArrayProvenanceStore()
    {
        IntPtr[] arr = new IntPtr[3];
        IntPtr handle0 = typeof(int).TypeHandle.Value;
        IntPtr handle1 = typeof(long).TypeHandle.Value;
        fixed (IntPtr* p = arr)
        {
            *p = handle0;
            p[1] = handle1;
            // Read back through the same fixed pointer: the read path must
            // surface the typed cell rather than slicing bytes, since the
            // cells now carry non-byte-renderable handle provenance.
            if (*p != handle0) return 50;
            if (p[1] != handle1) return 51;
        }
        if (arr[0] != handle0) return 52;
        if (arr[1] != handle1) return 53;
        return 0;
    }

    // `fixed (object* p = arr) { *p = value; }` over a reference-type array
    // must remain a typed `ArrayElement` store: `stind.ref` over the native
    // pointer cannot byte-flatten an `ObjectRef`. The Conv_U/Conv_I anchor
    // must therefore skip reference-typed element arrays — byte-stride
    // pointer arithmetic over object cells would have no useful semantics
    // anyway, since reference cells aren't byte-addressable.
    public static int TestObjectArrayFixedStore()
    {
        object obj1 = new object();
        object obj2 = new object();
        object[] arr = new object[] { obj1, obj1 };
        fixed (object* p = arr)
        {
            *p = obj2;
        }
        if (!object.ReferenceEquals(arr[0], obj2)) return 60;
        if (!object.ReferenceEquals(arr[1], obj1)) return 61;
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
        r = TestIntPointerArrayFixed();
        if (r != 0) return r;
        r = TestIntPtrArrayProvenanceStore();
        if (r != 0) return r;
        r = TestObjectArrayFixedStore();
        if (r != 0) return r;
        return 0;
    }
}

using System;

public unsafe class FixedArrayPointerArithmetic
{
    // After `fixed (T* p = arr)`, the pointer is in native-pointer-world: pointer
    // arithmetic over it must use byte stride (ECMA-335 §III.1.5), not element
    // stride. The C# compiler emits `sizeof(T) * k` explicitly for `p + k` and
    // `p++`. Conv_U/Conv_I on a plain array byref anchors a `ReinterpretAs T`
    // projection so subsequent `add` uses byte arithmetic.

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

    // After a cell has been populated with a provenance-bearing handle, a
    // subsequent plain numeric store through the same fixed pointer must
    // also succeed. The new value (`IntPtr.Zero`) is byte-addressable, but
    // the *existing* cell is not — so the byte-view write dispatcher must
    // notice the destination's provenance and route through the typed-cell
    // path rather than the byte-scatter path.
    public static int TestIntPtrArrayProvenanceOverwrite()
    {
        IntPtr[] arr = new IntPtr[2];
        IntPtr handle = typeof(int).TypeHandle.Value;
        fixed (IntPtr* p = arr)
        {
            *p = handle;
            *p = IntPtr.Zero;
            p[1] = handle;
            p[1] = new IntPtr(7);
        }
        if (arr[0] != IntPtr.Zero) return 70;
        if (arr[1] != new IntPtr(7)) return 71;
        return 0;
    }

    // Cross-element provenance: once one cell carries non-byte-renderable
    // provenance, a plain numeric store to a *different* cell through the
    // same fixed pointer must still succeed. The byte-scatter path computes
    // its cell stride from element 0 — if that derivation rejected non-byte-
    // addressable cells, the second store would fail before the writer ever
    // touched the (clean) target cell. Stride derivation must therefore not
    // validate unrelated cells.
    //
    // The array is initialised through `Stelem_i` (via the literal form) so
    // each cell is a bare `Numeric NativeInt`; `new IntPtr[3]` would leave
    // cells wrapped in a `ValueType` for the IntPtr struct, which `Ldelem_i`
    // does not currently unwrap and is orthogonal to the stride bug under
    // test.
    public static int TestIntPtrArrayProvenanceCrossElement()
    {
        IntPtr[] arr = new IntPtr[] { IntPtr.Zero, IntPtr.Zero, IntPtr.Zero };
        IntPtr handle = typeof(int).TypeHandle.Value;
        fixed (IntPtr* p = arr)
        {
            *p = handle;
            p[1] = IntPtr.Zero;
            p[2] = new IntPtr(9);
        }
        if (arr[0] != handle) return 80;
        if (arr[1] != IntPtr.Zero) return 81;
        if (arr[2] != new IntPtr(9)) return 82;
        return 0;
    }

    // Whole-cell `stobj` of a struct that carries non-byte-renderable
    // provenance through a byte-view-anchored fixed-array pointer must
    // preserve the value, not try to byte-flatten it. The anchor turns
    // `*p = new S { ... }` into a byte-view write; the dispatcher needs a
    // typed-cell fast path for whole-cell-aligned, same-size writes whose
    // payload cannot survive `CliType.ToBytes` (here a `TypeHandlePtr` in
    // a struct field).
    public struct HandleHolder
    {
        public IntPtr P;
    }

    public static int TestStructWithProvenanceFixedStore()
    {
        HandleHolder[] arr = new HandleHolder[1];
        IntPtr handle = typeof(int).TypeHandle.Value;
        fixed (HandleHolder* p = arr)
        {
            *p = new HandleHolder { P = handle };
        }
        if (arr[0].P != handle) return 90;
        return 0;
    }

    // `fixed (object* p = arr) { *p = value; }` over a reference-type array
    // must remain a typed `ArrayElement` store: `stind.ref` over the native
    // pointer cannot byte-flatten an `ObjectRef`. The Conv_U/Conv_I anchor
    // therefore lands a byte-view on the reference-array byref, but the
    // cell-aligned dispatch in `tryWriteArrayElementPrecise` routes the
    // store through `setArrayValue` and preserves the `ObjectRef` payload.
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

    // Jagged arrays (`object[][]`) carry a structural element handle
    // (`OneDimArrayZero` over the inner element type), which isn't registered
    // in `AllConcreteTypes`. Without anchoring those cells too, the same
    // element-stride bug bites `fixed (object[]* p = jagged)`: the trailing
    // `sizeof object[]; add` for `p[1]` would produce an out-of-bounds cell
    // index. The byte-view anchor uses `System.Object` as the reinterpret
    // target — the cells share `ObjectRef` shape with plain `object[]`, so
    // the cell-aligned read/write short-circuits reuse the same path.
    public static int TestJaggedArrayPointerArithmetic()
    {
        object[] a = new object[] { "a0" };
        object[] b = new object[] { "b0", "b1" };
        object[] c = new object[] { "c0", "c1", "c2" };
        object[][] arr = new object[][] { a, b, c };
        fixed (object[]* p = arr)
        {
            if (!object.ReferenceEquals(p[0], a)) return 120;
            if (!object.ReferenceEquals(p[1], b)) return 121;
            if (!object.ReferenceEquals(p[2], c)) return 122;

            p[0] = c;
            if (!object.ReferenceEquals(p[0], c)) return 123;

            p[2] = null;
            if (p[2] != null) return 124;
        }
        if (!object.ReferenceEquals(arr[0], c)) return 125;
        if (!object.ReferenceEquals(arr[1], b)) return 126;
        if (arr[2] != null) return 127;
        return 0;
    }

    // `fixed (object* p = arr) { p[k]; p[k] = value; }` over a reference-type
    // array exercises both `Ldind_ref`/`Stind_ref` and the byte-stride
    // pointer arithmetic that `p[k]` lowers to (`sizeof object; mul; add`,
    // or `sizeof object; add` for `k = 1`). Without the byte-view anchor on
    // the reference-array byref, the trailing `add` would be element-stride
    // and produce an out-of-bounds cell index, e.g. index 8 for `p[1]` on a
    // length-3 array. The anchor makes the arithmetic byte-stride; cell-
    // aligned reads land back on the right element through
    // `readArrayBytesAs`'s `Rejected` short-circuit, and cell-aligned writes
    // through `tryWriteArrayElementPrecise`.
    public static int TestObjectArrayPointerArithmetic()
    {
        object obj0 = new object();
        object obj1 = "hello";
        object obj2 = new object();
        object[] arr = new object[] { obj0, obj1, obj2 };
        fixed (object* p = arr)
        {
            if (!object.ReferenceEquals(p[0], obj0)) return 100;
            if (!object.ReferenceEquals(p[1], obj1)) return 101;
            if (!object.ReferenceEquals(p[2], obj2)) return 102;

            object replacement = "world";
            p[1] = replacement;
            if (!object.ReferenceEquals(p[1], replacement)) return 103;

            p[2] = null;
            if (p[2] != null) return 104;
        }
        if (!object.ReferenceEquals(arr[0], obj0)) return 105;
        if (arr[1] as string != "world") return 106;
        if (arr[2] != null) return 107;
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
        r = TestIntPtrArrayProvenanceOverwrite();
        if (r != 0) return r;
        r = TestIntPtrArrayProvenanceCrossElement();
        if (r != 0) return r;
        r = TestStructWithProvenanceFixedStore();
        if (r != 0) return r;
        r = TestObjectArrayFixedStore();
        if (r != 0) return r;
        r = TestObjectArrayPointerArithmetic();
        if (r != 0) return r;
        r = TestJaggedArrayPointerArithmetic();
        if (r != 0) return r;
        return 0;
    }
}

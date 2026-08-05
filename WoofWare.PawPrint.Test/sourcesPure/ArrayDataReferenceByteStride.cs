using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

// `MemoryMarshal.GetArrayDataReference` has two `[Intrinsic]` overloads. The generic
// `GetArrayDataReference<T>(T[])` returns a `ref T` and so carries element stride; the
// non-generic `GetArrayDataReference(Array)` returns a `ref byte` and so must carry byte
// stride. `Array.Clear(Array)` is the non-generic one's main caller.
//
// The two must not be conflated: a `ref byte` whose arithmetic advanced by whole elements
// would silently read the wrong slot for every consumer that does its own offsetting.
public unsafe class TestArrayDataReferenceByteStride
{
    private struct Pair
    {
        public int X;
        public int Y;
    }

    // Byte stride is directly observable on an int[]: byte 1 of 0x11223344 is 0x33
    // little-endian, and byte 4 is the first byte of the *next* element.
    private static int TestByteStrideOverInt32Array()
    {
        int[] a = { 0x11223344, 0x55667788 };
        ref byte data = ref MemoryMarshal.GetArrayDataReference((Array)a);

        if (Unsafe.Add(ref data, 0) != 0x44) return 1;
        if (Unsafe.Add(ref data, 1) != 0x33) return 2;
        if (Unsafe.Add(ref data, 2) != 0x22) return 3;
        if (Unsafe.Add(ref data, 3) != 0x11) return 4;
        if (Unsafe.Add(ref data, 4) != 0x88) return 5;
        if (Unsafe.Add(ref data, 7) != 0x55) return 6;

        // The generic overload keeps element stride, so the same offset means something
        // different through it.
        ref int elements = ref MemoryMarshal.GetArrayDataReference(a);
        if (Unsafe.Add(ref elements, 1) != 0x55667788) return 7;

        return 0;
    }

    private static int TestByteStrideOverStructArray()
    {
        Pair[] a = new Pair[2];
        a[0].X = 0x0A0B0C0D;
        a[0].Y = 0x01020304;
        a[1].X = 0x11121314;

        ref byte data = ref MemoryMarshal.GetArrayDataReference((Array)a);

        if (Unsafe.Add(ref data, 0) != 0x0D) return 10;
        if (Unsafe.Add(ref data, 4) != 0x04) return 11;
        if (Unsafe.Add(ref data, 8) != 0x14) return 12;

        return 0;
    }

    // A pointer-element array has no GC pointers, so `Array.Clear` routes it through the
    // byte-count path like any other pointer-free element type. Byte-stride *arithmetic* over
    // its data reference is well defined even though a byte-granular read of a pointer cell is
    // not, so the arithmetic must not be rejected.
    private static int TestPointerElementArray()
    {
        int*[] a = new int*[3];
        a[0] = (int*)3;
        a[1] = (int*)4;
        a[2] = (int*)5;

        Array.Clear(a, 1, 1);
        if (a[0] != (int*)3) return 20;
        if (a[1] != null) return 21;
        if (a[2] != (int*)5) return 22;

        Array.Clear(a);
        if (a[0] != null) return 23;
        if (a[1] != null) return 24;
        if (a[2] != null) return 25;

        // Deliberately arithmetic only: these pointers are never dereferenced, and neither is
        // the byref itself. On the real runtime this is address computation; in PawPrint it
        // must not throw for want of a byte-stride anchor.
        int*[] b = new int*[2];
        ref byte data = ref MemoryMarshal.GetArrayDataReference((Array)b);

        if (!Unsafe.AreSame(ref data, ref MemoryMarshal.GetArrayDataReference((Array)b))) return 26;
        if (Unsafe.AreSame(ref data, ref Unsafe.Add(ref data, 8))) return 27;

        // One element is 8 bytes wide, so the byte-stride step of 8 must land exactly on
        // element 1 -- which is what distinguishes byte stride from element stride here.
        if (!Unsafe.AreSame(ref Unsafe.Add(ref data, 8), ref Unsafe.As<IntPtr, byte>(ref Unsafe.Add(ref Unsafe.As<byte, IntPtr>(ref data), 1))))
        {
            return 28;
        }

        return 0;
    }

    private static int TestEmptyArray()
    {
        // The data reference of an empty array is "where element 0 would have been"; it is
        // legal to form and compare, but never to dereference.
        int[] a = new int[0];
        ref byte data = ref MemoryMarshal.GetArrayDataReference((Array)a);

        if (!Unsafe.AreSame(ref data, ref MemoryMarshal.GetArrayDataReference((Array)a))) return 30;

        Array.Clear(a);

        return 0;
    }

    public static int Main(string[] argv)
    {
        int result;

        result = TestByteStrideOverInt32Array();
        if (result != 0) return 1000 + result;

        result = TestByteStrideOverStructArray();
        if (result != 0) return 2000 + result;

        result = TestPointerElementArray();
        if (result != 0) return 3000 + result;

        result = TestEmptyArray();
        if (result != 0) return 4000 + result;

        return 0;
    }
}

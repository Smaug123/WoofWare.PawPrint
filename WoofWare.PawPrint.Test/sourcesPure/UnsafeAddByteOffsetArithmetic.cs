using System;
using System.Runtime.CompilerServices;

public class TestUnsafeAddByteOffsetArithmetic
{
    // `Unsafe.AddByteOffset(ref T, IntPtr)` over a byte byref advances the
    // cursor by a literal byte count. With an int[] viewed as bytes, +4
    // moves from cell 0 to cell 1.
    public static int Test1()
    {
        int[] a = { 0x11223344, unchecked((int)0xDDCCBBAA) };
        ref byte b = ref Unsafe.As<int, byte>(ref a[0]);
        ref byte b4 = ref Unsafe.AddByteOffset(ref b, (IntPtr)4);
        int v = Unsafe.ReadUnaligned<int>(ref b4);
        if (v != unchecked((int)0xDDCCBBAA))
            return 1;
        return 0;
    }

    // Same arithmetic via the nuint overload (UIntPtr after compilation).
    public static int Test2()
    {
        int[] a = { 0x11223344, unchecked((int)0xDDCCBBAA) };
        ref byte b = ref Unsafe.As<int, byte>(ref a[0]);
        ref byte b4 = ref Unsafe.AddByteOffset(ref b, (nuint)4);
        int v = Unsafe.ReadUnaligned<int>(ref b4);
        if (v != unchecked((int)0xDDCCBBAA))
            return 2;
        return 0;
    }

    // `Unsafe.AddByteOffset<int>(ref int, IntPtr)` with a whole-cell offset must
    // canonicalise so that the result is structurally equivalent to a cell-stride
    // advance — equality via Unsafe.AreSame verifies the byref folds correctly.
    public static int Test3()
    {
        int[] a = new int[4];
        ref int r0 = ref a[0];
        ref int r1 = ref Unsafe.AddByteOffset(ref r0, (IntPtr)sizeof(int));
        if (!Unsafe.AreSame(ref a[1], ref r1))
            return 3;
        return 0;
    }

    // Negative IntPtr offset: starting mid-array and walking back must produce
    // a cursor whose read returns the bytes the forward walk would have produced.
    public static int Test4()
    {
        int[] a = { 0x44332211, unchecked((int)0x88776655) };
        ref byte b = ref Unsafe.As<int, byte>(ref a[0]);
        ref byte b5 = ref Unsafe.AddByteOffset(ref b, (IntPtr)5);
        ref byte b2 = ref Unsafe.AddByteOffset(ref b5, (IntPtr)(-3));
        int v = Unsafe.ReadUnaligned<int>(ref b2);
        if (v != 0x66554433)
            return 4;
        return 0;
    }

    // Composition: AddByteOffset followed by AddByteOffset is structurally
    // equivalent to a single combined AddByteOffset.
    public static int Test5()
    {
        int[] a = { 0x01020304, 0x05060708 };
        ref byte b = ref Unsafe.As<int, byte>(ref a[0]);
        ref byte step1 = ref Unsafe.AddByteOffset(ref b, (IntPtr)3);
        ref byte step2 = ref Unsafe.AddByteOffset(ref step1, (IntPtr)2);
        ref byte direct = ref Unsafe.AddByteOffset(ref b, (IntPtr)5);
        if (!Unsafe.AreSame(ref step2, ref direct))
            return 5;
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
        return 0;
    }
}

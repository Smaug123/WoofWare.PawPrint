using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

public class TestUnsafeByteViewArithmetic
{
    // `Unsafe.As<int, byte>` produces a `ref byte` view over an int[]; bytewise
    // `Unsafe.Add` on that view walks the underlying storage byte by byte.
    // Reading a short at byte offset 4 lands on cell 1 of the array.
    public static int Test1()
    {
        int[] a = { 0x11223344, unchecked((int)0xDDCCBBAA) };
        ref byte b = ref Unsafe.As<int, byte>(ref a[0]);
        ref byte b4 = ref Unsafe.Add(ref b, 4);
        int v = Unsafe.ReadUnaligned<int>(ref b4);
        if (v != unchecked((int)0xDDCCBBAA))
            return 1;
        return 0;
    }

    // ReadUnaligned of a type larger than a single byte at a byte offset that
    // straddles a cell boundary: start 2 bytes into cell 0 (an int[]) and read
    // a 4-byte int, which spans the high half of cell 0 and the low half of
    // cell 1.
    public static int Test2()
    {
        int[] a = { 0x44332211, unchecked((int)0x88776655) };
        ref byte b = ref Unsafe.As<int, byte>(ref a[0]);
        ref byte b2 = ref Unsafe.Add(ref b, 2);
        int v = Unsafe.ReadUnaligned<int>(ref b2);
        // Little-endian: bytes at offset 2..5 are 0x33, 0x44, 0x55, 0x66.
        if (v != 0x66554433)
            return 2;
        return 0;
    }

    // WriteUnaligned at a cell-aligned byte offset: the write must land in the
    // correct cell and leave neighbours untouched.
    public static int Test3()
    {
        int[] a = new int[4];
        ref byte b = ref Unsafe.As<int, byte>(ref a[0]);
        ref byte b8 = ref Unsafe.Add(ref b, 8);
        Unsafe.WriteUnaligned<int>(ref b8, 0x12345678);
        if (a[0] != 0)
            return 3;
        if (a[1] != 0)
            return 4;
        if (a[2] != 0x12345678)
            return 5;
        if (a[3] != 0)
            return 6;
        return 0;
    }

    // WriteUnaligned that straddles a cell boundary updates bytes across two
    // cells without disturbing bytes outside the written region.
    public static int Test4()
    {
        int[] a = { unchecked((int)0xFFFFFFFF), unchecked((int)0xFFFFFFFF), unchecked((int)0xFFFFFFFF) };
        ref byte b = ref Unsafe.As<int, byte>(ref a[0]);
        ref byte b6 = ref Unsafe.Add(ref b, 6);
        // Write a 32-bit value straddling cells 1 and 2 (bytes 6..9).
        Unsafe.WriteUnaligned<int>(ref b6, 0x00000000);
        // Cell 0 bytes 0..3 untouched.
        if (a[0] != unchecked((int)0xFFFFFFFF))
            return 7;
        // Cell 1 bytes 4..7: bytes 4,5 untouched (0xFF), bytes 6,7 zeroed.
        if (a[1] != unchecked((int)0x0000FFFF))
            return 8;
        // Cell 2 bytes 8..11: bytes 8,9 zeroed, bytes 10,11 untouched.
        if (a[2] != unchecked((int)0xFFFF0000))
            return 9;
        return 0;
    }

    // Round-trip byte-level: write a short at an odd byte offset, read it back
    // through the same byte cursor.
    public static int Test5()
    {
        int[] a = new int[4];
        ref byte b = ref Unsafe.As<int, byte>(ref a[0]);
        ref byte b5 = ref Unsafe.Add(ref b, 5);
        Unsafe.WriteUnaligned<short>(ref b5, unchecked((short)0xCAFE));
        short s = Unsafe.ReadUnaligned<short>(ref b5);
        if (s != unchecked((short)0xCAFE))
            return 10;
        return 0;
    }

    // Unsafe.ByteOffset on two byte-view byrefs into the same int[] must
    // return the signed byte delta regardless of the ReinterpretAs chain.
    public static int Test6()
    {
        int[] a = new int[8];
        ref byte b0 = ref Unsafe.As<int, byte>(ref a[0]);
        ref byte b0b = ref Unsafe.Add(ref b0, 3);
        ref byte b1 = ref Unsafe.Add(ref b0, 10);
        System.IntPtr delta = Unsafe.ByteOffset(ref b0b, ref b1);
        if ((long)delta != 7L)
            return 11;
        System.IntPtr back = Unsafe.ByteOffset(ref b1, ref b0b);
        if ((long)back != -7L)
            return 12;
        return 0;
    }

    // Composition of two Unsafe.Adds at the byte level produces the same
    // cursor as a single combined Unsafe.Add.
    public static int Test7()
    {
        int[] a = { 0x01020304, 0x05060708 };
        ref byte b = ref Unsafe.As<int, byte>(ref a[0]);
        ref byte step1 = ref Unsafe.Add(ref b, 3);
        ref byte step2 = ref Unsafe.Add(ref step1, 2);
        ref byte direct = ref Unsafe.Add(ref b, 5);
        if (!Unsafe.AreSame(ref step2, ref direct))
            return 13;
        return 0;
    }

    // Negative byte-level Unsafe.Add: walking back from mid-array must
    // recover the cursor that a forward walk would have produced.
    public static int Test8()
    {
        int[] a = { 0x44332211, unchecked((int)0x88776655) };
        ref byte b = ref Unsafe.As<int, byte>(ref a[0]);
        ref byte b5 = ref Unsafe.Add(ref b, 5);
        ref byte b2 = ref Unsafe.Add(ref b5, -3);
        int v = Unsafe.ReadUnaligned<int>(ref b2);
        // Little-endian bytes at offset 2..5 are 0x33, 0x44, 0x55, 0x66.
        if (v != 0x66554433)
            return 14;
        return 0;
    }

    // Whole-cell byte offset canonicalization: stepping forward by one cell's
    // worth of bytes through the byte view must produce the same cursor as a
    // cell-stride `Unsafe.Add<int>` — i.e. the internal representation must
    // fold whole-cell byte offsets back into the array index.
    public static int Test9()
    {
        int[] a = new int[4];
        ref int rInt = ref a[1];
        ref byte rByte = ref Unsafe.As<int, byte>(ref a[0]);
        ref byte rByteStepped = ref Unsafe.Add(ref rByte, 4);
        ref int reinterpreted = ref Unsafe.As<byte, int>(ref rByteStepped);
        if (!Unsafe.AreSame(ref rInt, ref reinterpreted))
            return 15;
        return 0;
    }

    // Negative byte-level Unsafe.Add crossing a cell boundary backwards must
    // produce a cursor that reads the bytes in the expected straddling layout.
    public static int Test10()
    {
        int[] a = { 0x44332211, unchecked((int)0x88776655), unchecked((int)0xCCBBAA99) };
        ref byte b = ref Unsafe.As<int, byte>(ref a[0]);
        ref byte b9 = ref Unsafe.Add(ref b, 9);
        // Step back 7 bytes: lands at offset 2 (mid cell 0). Read a 4-byte int
        // straddling cells 0 and 1: bytes 0x33 0x44 0x55 0x66.
        ref byte back = ref Unsafe.Add(ref b9, -7);
        int v = Unsafe.ReadUnaligned<int>(ref back);
        if (v != 0x66554433)
            return 16;
        return 0;
    }

    // Reinterpreting a byref that already carries a byte offset must preserve
    // the offset across the ReinterpretAs. A reinterpret-and-back round trip
    // must leave the cursor identical to the original byte offset: if the
    // ByteOffset were lost when applying a new ReinterpretAs, the round-tripped
    // cursor would collapse back to the array origin.
    public static int Test11()
    {
        int[] a = new int[2];
        ref byte b = ref Unsafe.As<int, byte>(ref a[0]);
        ref byte b2 = ref Unsafe.Add(ref b, 2);
        ref short s2 = ref Unsafe.As<byte, short>(ref b2);
        ref byte roundTripped = ref Unsafe.As<short, byte>(ref s2);
        if (!Unsafe.AreSame(ref b2, ref roundTripped))
            return 17;
        // And the round-tripped cursor is not the same as the array origin.
        if (Unsafe.AreSame(ref b, ref roundTripped))
            return 18;
        return 0;
    }

    // Two byrefs that differ only in the trailing reinterpret view still point
    // at the same byte location; address equality must ignore the view type
    // regardless of whether a byte offset rides on top. `ref b2` has a byte
    // offset of 2 under a `ReinterpretAs byte` view; reinterpreting it twice
    // (once via short, once via char) gives two byrefs that should compare
    // equal after each has been reinterpreted back to byte.
    public static int Test12()
    {
        int[] a = { 0x11223344, unchecked((int)0xDDCCBBAA) };
        ref byte b = ref Unsafe.As<int, byte>(ref a[0]);
        ref byte b2 = ref Unsafe.Add(ref b, 2);
        ref byte viaShort = ref Unsafe.As<short, byte>(ref Unsafe.As<byte, short>(ref b2));
        ref byte viaChar = ref Unsafe.As<char, byte>(ref Unsafe.As<byte, char>(ref b2));
        if (!Unsafe.AreSame(ref viaShort, ref viaChar))
            return 19;
        return 0;
    }

    // Empty-array byte-view round trip: byref arithmetic through a `ref byte`
    // view over an `Array.Empty<int>()` origin, stepped by `sizeof(int)` bytes
    // and reinterpreted back to `ref int`, must canonicalize to the same
    // address as `Unsafe.Add(ref origin, 1)`. Without an ElementZero-based
    // stride, empty-array byrefs would have no way to fold the byte offset
    // back into the cell index.
    public static int Test13()
    {
        int[] empty = System.Array.Empty<int>();
        ref int origin = ref MemoryMarshal.GetArrayDataReference(empty);
        ref int viaBytes = ref Unsafe.As<byte, int>(ref Unsafe.AddByteOffset(ref Unsafe.As<int, byte>(ref origin), (IntPtr)4));
        ref int viaAdd = ref Unsafe.Add(ref origin, 1);
        if (!Unsafe.AreSame(ref viaBytes, ref viaAdd))
            return 20;
        return 0;
    }

    // `Unsafe.AddByteOffset` on a plain `ref T` into an array (no intervening
    // reinterpret) must fold a whole-cell byte delta into the cell index.
    // `GetArrayDataReference(a) + 4 bytes` for an int[] lands at element 1.
    public static int Test14()
    {
        int[] a = { 0x11223344, 0x55667788 };
        ref int origin = ref MemoryMarshal.GetArrayDataReference(a);
        ref int second = ref Unsafe.AddByteOffset(ref origin, (IntPtr)4);
        if (second != 0x55667788)
            return 21;
        if (!Unsafe.AreSame(ref second, ref Unsafe.Add(ref origin, 1)))
            return 22;
        return 0;
    }

    // Ordinary `ldind.u1` on a byte-view byref (one produced by `Unsafe.Add`
    // on a `ref byte`) must load the single byte at the cursor. C# emits a
    // plain Ldind for `b1 == 0x22` — it doesn't route through
    // `Unsafe.ReadUnaligned` — so the byref's `[ReinterpretAs byte; ByteOffset n]`
    // shape must be dereferenceable by the generic reader.
    public static int Test15()
    {
        int[] a = { 0x44332211 };
        ref byte b = ref Unsafe.As<int, byte>(ref a[0]);
        ref byte b1 = ref Unsafe.Add(ref b, 1);
        if (b1 != 0x22)
            return 23;
        ref byte b2 = ref Unsafe.Add(ref b, 2);
        if (b2 != 0x33)
            return 24;
        return 0;
    }

    // `Unsafe.AddByteOffset` on a plain `ref T` into an array must accept
    // unaligned byte deltas — callers often reinterpret the result
    // immediately (`Unsafe.As<int, byte>(ref Unsafe.AddByteOffset(ref origin, 1))`)
    // so failing before the reinterpret blocks a valid unsafe pattern.
    public static int Test16()
    {
        int[] a = { 0x44332211 };
        ref int origin = ref MemoryMarshal.GetArrayDataReference(a);
        ref byte asByte = ref Unsafe.As<int, byte>(ref Unsafe.AddByteOffset(ref origin, (IntPtr)1));
        if (asByte != 0x22)
            return 25;
        return 0;
    }

    // Symmetric write path: `stind.i1` through a byte-view byref must scatter
    // a single byte into the cell at the cursor's location without disturbing
    // the rest of the cell.
    public static int Test17()
    {
        int[] a = { 0x44332211 };
        ref byte b = ref Unsafe.As<int, byte>(ref a[0]);
        ref byte b1 = ref Unsafe.Add(ref b, 1);
        b1 = 0xEE;
        if (a[0] != unchecked((int)0x4433EE11))
            return 26;
        return 0;
    }

    // Composition: two `Unsafe.AddByteOffset` calls on a plain `ref T` must
    // accumulate into the same byte cursor. The first call on `ref a[0]`
    // (stride 4) with delta 1 parks at `[ByteOffset 1]`; the second add of
    // 1 advances to `[ByteOffset 2]`. Reinterpreting that as `ref byte`
    // then reads byte 2 of cell 0.
    public static int Test18()
    {
        int[] a = { 0x44332211 };
        ref int p = ref Unsafe.AddByteOffset(ref MemoryMarshal.GetArrayDataReference(a), (IntPtr)1);
        ref int q = ref Unsafe.AddByteOffset(ref p, (IntPtr)1);
        ref byte b = ref Unsafe.As<int, byte>(ref q);
        if (b != 0x33)
            return 27;
        return 0;
    }

    // Zero-offset byte-view write: `b = 0xEE` on a `ref byte` produced by
    // `Unsafe.As<int, byte>(ref a[0])` (no subsequent `Unsafe.Add`) must
    // still splice one byte into the cell's low half, not overwrite the
    // whole int cell.
    public static int Test19()
    {
        int[] a = { 0x44332211 };
        ref byte b = ref Unsafe.As<int, byte>(ref a[0]);
        b = 0xEE;
        if (a[0] != unchecked((int)0x443322EE))
            return 28;
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
        r = Test13();
        if (r != 0) return r;
        r = Test14();
        if (r != 0) return r;
        r = Test15();
        if (r != 0) return r;
        r = Test16();
        if (r != 0) return r;
        r = Test17();
        if (r != 0) return r;
        r = Test18();
        if (r != 0) return r;
        r = Test19();
        if (r != 0) return r;
        return 0;
    }
}

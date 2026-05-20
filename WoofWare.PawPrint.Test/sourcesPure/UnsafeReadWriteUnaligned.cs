using System.Runtime.CompilerServices;

public class TestUnsafeReadWriteUnaligned
{
    // Round-trip a short through `ref byte` into a byte[]: little-endian layout
    // means 0x34 at offset 0, 0x12 at offset 1 reconstructs as 0x1234. Uses
    // `Unsafe.ReadUnaligned<short>` to gather two byte cells into the primitive.
    public static int Test1()
    {
        byte[] b = { 0x34, 0x12, 0, 0 };
        short s = Unsafe.ReadUnaligned<short>(ref b[0]);
        if (s != 0x1234)
            return 1;
        return 0;
    }

    // ReadUnaligned<int> gathers four consecutive byte cells.
    public static int Test2()
    {
        byte[] b = { 0x78, 0x56, 0x34, 0x12, 0xFF };
        int v = Unsafe.ReadUnaligned<int>(ref b[0]);
        if (v != 0x12345678)
            return 2;
        return 0;
    }

    // WriteUnaligned<int> then ReadUnaligned<int>: what we wrote must come
    // back bit-for-bit.
    public static int Test3()
    {
        byte[] b = new byte[8];
        Unsafe.WriteUnaligned<int>(ref b[0], 0x12345678);
        int v = Unsafe.ReadUnaligned<int>(ref b[0]);
        if (v != 0x12345678)
            return 3;
        return 0;
    }

    // WriteUnaligned at a non-zero byte offset must not overwrite earlier
    // bytes. Pre-populate cells 0 and 1 via Unsafe.WriteUnaligned<short>,
    // write a fresh short at offset 2, then re-read both shorts.
    public static int Test4()
    {
        byte[] b = new byte[8];
        Unsafe.WriteUnaligned<short>(ref b[0], unchecked((short)0xBBAA));
        Unsafe.WriteUnaligned<short>(ref b[2], unchecked((short)0xCDEF));
        short prefix = Unsafe.ReadUnaligned<short>(ref b[0]);
        if (prefix != unchecked((short)0xBBAA))
            return 4;
        short mid = Unsafe.ReadUnaligned<short>(ref b[2]);
        if (mid != unchecked((short)0xCDEF))
            return 5;
        return 0;
    }

    // Write / read round-trip for 8-byte types spans eight byte cells.
    public static int Test5()
    {
        byte[] b = new byte[16];
        long v = unchecked((long)0xDEADBEEF_CAFEBABEUL);
        Unsafe.WriteUnaligned<long>(ref b[0], v);
        long r = Unsafe.ReadUnaligned<long>(ref b[0]);
        if (r != v)
            return 6;
        return 0;
    }

    // ReadUnaligned after WriteUnaligned at a non-zero offset sees the
    // new value; bytes adjacent to the written region are undisturbed,
    // as evidenced by a second short read at offset 0 still matching its
    // original payload.
    public static int Test6()
    {
        byte[] b = new byte[8];
        Unsafe.WriteUnaligned<short>(ref b[0], 0x1234);
        Unsafe.WriteUnaligned<int>(ref b[2], 0x77665544);
        short head = Unsafe.ReadUnaligned<short>(ref b[0]);
        if (head != 0x1234)
            return 7;
        int body = Unsafe.ReadUnaligned<int>(ref b[2]);
        if (body != 0x77665544)
            return 8;
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

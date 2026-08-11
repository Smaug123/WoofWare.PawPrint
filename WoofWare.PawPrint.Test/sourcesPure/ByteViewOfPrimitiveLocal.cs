using System;

// Reinterpreting the storage of a primitive through a pointer of a *narrower* type, at
// byte offset zero. A non-zero index (`p[1]`) already worked, because C# emits the offset
// as pointer arithmetic and the byref then carries a trailing byte-view projection; index
// zero emits no arithmetic at all, so the byref names the whole cell and the read has to
// decide for itself that `ldind.u1` means "one byte here" rather than "convert this cell".
//
// Everything asserted here is a fact about both runtimes on the same host. Where the answer
// depends on byte order the expectation is selected by `BitConverter.IsLittleEndian` rather
// than assumed, so the file states what is true rather than what is true on x64/arm64.
unsafe class ByteViewOfPrimitiveLocal
{
    struct WithLong
    {
        public int Before;
        public long L;
    }

    static int TestNarrowReadsOfLong()
    {
        long v = 0x0102030405060708L;
        byte* p = (byte*)&v;

        // The arm under test: offset zero.
        byte expected0 = BitConverter.IsLittleEndian ? (byte)0x08 : (byte)0x01;
        if (p[0] != expected0)
            return 1;

        // The offsets that already worked, so a fix that only moved the problem is caught.
        for (int i = 0; i < 8; i++)
        {
            int shift = BitConverter.IsLittleEndian ? 8 * i : 8 * (7 - i);
            if (p[i] != (byte)(v >> shift))
                return 2;
        }

        // Reassembling every byte must give the original back, whichever order they came in.
        ulong rebuilt = 0;
        for (int i = 0; i < 8; i++)
        {
            int shift = BitConverter.IsLittleEndian ? 8 * i : 8 * (7 - i);
            rebuilt |= (ulong)p[i] << shift;
        }

        if (rebuilt != (ulong)v)
            return 3;

        // Widths between one byte and the whole cell, all at offset zero.
        int expectedInt = BitConverter.IsLittleEndian ? unchecked((int)v) : (int)(v >> 32);
        if (*(int*)&v != expectedInt)
            return 4;

        short expectedShort = BitConverter.IsLittleEndian ? unchecked((short)v) : (short)(v >> 48);
        if (*(short*)&v != expectedShort)
            return 5;

        return 0;
    }

    static int TestNarrowReadOfInt()
    {
        int v = 0x11223344;
        byte* p = (byte*)&v;

        byte expected0 = BitConverter.IsLittleEndian ? (byte)0x44 : (byte)0x11;
        if (p[0] != expected0)
            return 10;

        return 0;
    }

    static int TestNarrowReadOfNativeInt()
    {
        IntPtr v = (IntPtr)0x1122334455667788L;
        byte* p = (byte*)&v;

        byte expected0 = BitConverter.IsLittleEndian ? (byte)0x88 : (byte)0x11;
        if (p[0] != expected0)
            return 20;

        // Reassembling gives the pointer-sized value back.
        ulong rebuilt = 0;
        for (int i = 0; i < IntPtr.Size; i++)
        {
            int shift = BitConverter.IsLittleEndian ? 8 * i : 8 * (IntPtr.Size - 1 - i);
            rebuilt |= (ulong)p[i] << shift;
        }

        if (rebuilt != (ulong)(long)v)
            return 21;

        return 0;
    }

    // `*(float*)&aDouble` is a *reinterpretation*, not the numeric conversion `(float)aDouble`.
    // 2.0 has bit pattern 0x4000_0000_0000_0000, so its low four bytes are all zero and its
    // high four are 0x40000000 — meaning the reinterpreted float is +0 on a little-endian host
    // and 2.0f on a big-endian one. Either way it is never `(float)2.0` on the host that
    // answers zero, which is what makes this distinguish the two behaviours without needing
    // any bit-conversion helper as an oracle.
    static int TestReinterpretDoubleAsFloat()
    {
        double d = 2.0;
        float f = *(float*)&d;

        float expected = BitConverter.IsLittleEndian ? 0.0f : 2.0f;
        if (f != expected)
            return 30;

        return 0;
    }

    // A pointer read at its own width must keep naming the pointer: this is not a narrowing
    // read, so it must not be diverted to a byte view (a pointer has no byte image, so that
    // would fail rather than answer).
    static int TestPointerToPointerDereference()
    {
        long v = 42;
        long* p = &v;
        long** pp = &p;

        if (**pp != 42)
            return 40;

        if (*pp != p)
            return 41;

        return 0;
    }

    static int TestStructFieldAndArrayElement()
    {
        WithLong s;
        s.Before = -1;
        s.L = 0x0102030405060708L;

        byte* pf = (byte*)&s.L;
        byte expected0 = BitConverter.IsLittleEndian ? (byte)0x08 : (byte)0x01;
        if (pf[0] != expected0)
            return 50;

        long[] arr = { 0x0102030405060708L, 0 };
        fixed (long* q = arr)
        {
            byte* pa = (byte*)q;
            if (pa[0] != expected0)
                return 51;
        }

        return 0;
    }

    // The write side already reinterprets at offset zero; pin that the two directions agree,
    // so a change to one cannot silently drift from the other.
    static int TestWriteThenReadRoundTrip()
    {
        long v = 0x0102030405060708L;
        byte* p = (byte*)&v;

        p[0] = 0x99;

        long expected = BitConverter.IsLittleEndian ? 0x0102030405060799L : unchecked((long)0x9902030405060708UL);
        if (v != expected)
            return 60;

        if (p[0] != 0x99)
            return 61;

        return 0;
    }

    static int Main(string[] args)
    {
        int result = TestNarrowReadsOfLong();
        if (result != 0)
            return result;

        result = TestNarrowReadOfInt();
        if (result != 0)
            return result;

        result = TestNarrowReadOfNativeInt();
        if (result != 0)
            return result;

        result = TestReinterpretDoubleAsFloat();
        if (result != 0)
            return result;

        result = TestPointerToPointerDereference();
        if (result != 0)
            return result;

        result = TestStructFieldAndArrayElement();
        if (result != 0)
            return result;

        result = TestWriteThenReadRoundTrip();
        if (result != 0)
            return result;

        return 0;
    }
}

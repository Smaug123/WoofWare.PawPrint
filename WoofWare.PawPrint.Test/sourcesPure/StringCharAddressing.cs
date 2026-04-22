using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

public class TestStringCharAddressing
{
    // `MemoryMarshal.GetReference(s.AsSpan())` exposes a `ref char` to the first
    // character of the string; `Unsafe.Add(ref first, 1)` must walk one char
    // further into the string's internal data, not reinterpret the
    // `_firstChar` field slot.
    public static int Test1()
    {
        string s = "ab";
        ref char first = ref MemoryMarshal.GetReference(s.AsSpan());
        if (first != 'a')
            return 1;
        ref char second = ref Unsafe.Add(ref first, 1);
        if (second != 'b')
            return 2;
        return 0;
    }

    // Stepping further into the string: a 5-char string, read the last char
    // via four Unsafe.Add steps from the first.
    public static int Test2()
    {
        string s = "abcde";
        ref char p = ref MemoryMarshal.GetReference(s.AsSpan());
        for (int i = 0; i < 4; i++)
            p = ref Unsafe.Add(ref p, 1);
        if (p != 'e')
            return 3;
        return 0;
    }

    // Reading at exactly the string's length must yield the null terminator,
    // matching the CLR's null-terminated string layout. Two strings allocated
    // back-to-back must not bleed into each other: unsafe stepping past the
    // terminator of `s1` must NOT observe any character of `s2`.
    public static int Test3()
    {
        string s1 = "ab";
        string s2 = "cd";
        ref char first = ref MemoryMarshal.GetReference(s1.AsSpan());
        ref char terminator = ref Unsafe.Add(ref first, 2);
        if (terminator != '\0')
            return 4;
        // Make sure s2 is still alive and has not been aliased into by the
        // unsafe read above.
        ref char s2First = ref MemoryMarshal.GetReference(s2.AsSpan());
        if (s2First != 'c')
            return 5;
        return 0;
    }

    // Unsafe.AddByteOffset takes a byte delta directly (no premultiplication
    // by sizeof(T) in its IL body), so `add` sees raw bytes. Two bytes on
    // `ref char` must advance one char.
    public static int Test4()
    {
        string s = "ab";
        ref char first = ref MemoryMarshal.GetReference(s.AsSpan());
        ref char second = ref Unsafe.AddByteOffset(ref first, (IntPtr)2);
        if (second != 'b')
            return 6;
        return 0;
    }

    // Unsafe.ByteOffset between two char refs into the same string must return
    // the signed byte delta (char stride = 2). Walking forward by one char
    // should produce a byte offset of 2; the reverse direction should be -2.
    public static int Test5()
    {
        string s = "abcd";
        ref char first = ref MemoryMarshal.GetReference(s.AsSpan());
        ref char third = ref Unsafe.Add(ref first, 2);
        IntPtr delta = Unsafe.ByteOffset(ref first, ref third);
        if ((long)delta != 4L)
            return 7;
        IntPtr back = Unsafe.ByteOffset(ref third, ref first);
        if ((long)back != -4L)
            return 8;
        return 0;
    }

    // Byte-view round trip: reinterpret a char ref to byte, step by
    // `sizeof(char)` bytes, reinterpret back to char — must land on the same
    // char as `Unsafe.Add(ref firstChar, 1)`. Without folding the byte offset
    // back into the char index, `AreSame` would return false.
    public static int Test6()
    {
        string s = "ab";
        ref char firstChar = ref MemoryMarshal.GetReference(s.AsSpan());
        ref char viaBytes = ref Unsafe.As<byte, char>(ref Unsafe.AddByteOffset(ref Unsafe.As<char, byte>(ref firstChar), (IntPtr)2));
        ref char viaAdd = ref Unsafe.Add(ref firstChar, 1);
        if (!Unsafe.AreSame(ref viaBytes, ref viaAdd))
            return 9;
        return 0;
    }

    // Reading a char ref through a `ref byte` reinterpret yields the low byte
    // of the UTF-16 code unit (little-endian). For an ASCII char 'a' (0x0061),
    // the low byte is 0x61.
    public static int Test7()
    {
        string s = "a";
        ref char firstChar = ref MemoryMarshal.GetReference(s.AsSpan());
        ref byte asByte = ref Unsafe.As<char, byte>(ref firstChar);
        if (asByte != 0x61)
            return 10;
        return 0;
    }

    // Wide `ReadUnaligned` through a `ref byte` reinterpret over a string must
    // gather bytes across multiple char cells. `SpanHelpers.Char` and
    // `StringSearchValuesHelper` rely on exactly this pattern
    // (`Unsafe.ReadUnaligned<ulong/uint>` on `Unsafe.As<char, byte>(ref c)`).
    public static int Test8()
    {
        string s = "abcd";
        ref char firstChar = ref MemoryMarshal.GetReference(s.AsSpan());
        ref byte asByte = ref Unsafe.As<char, byte>(ref firstChar);
        uint v = Unsafe.ReadUnaligned<uint>(ref asByte);
        // UTF-16 little-endian bytes of "ab": 0x61 0x00 0x62 0x00.
        if (v != 0x00620061u)
            return 11;
        return 0;
    }

    // `Unsafe.Add<byte>` on a byte view over a string must walk raw bytes, not
    // whole chars. Stepping by 2 bytes must land on char index 1; a subsequent
    // `ReadUnaligned<uint>` then gathers the next four bytes.
    public static int Test9()
    {
        string s = "abcd";
        ref char firstChar = ref MemoryMarshal.GetReference(s.AsSpan());
        ref byte asByte = ref Unsafe.As<char, byte>(ref firstChar);
        ref byte at2 = ref Unsafe.Add(ref asByte, 2);
        uint v = Unsafe.ReadUnaligned<uint>(ref at2);
        // Bytes 2..5 of "abcd" UTF-16 LE: 0x62 0x00 0x63 0x00.
        if (v != 0x00630062u)
            return 12;
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
        return 0;
    }
}

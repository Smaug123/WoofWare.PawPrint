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

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

    public static int Main(string[] argv)
    {
        int r = Test1();
        if (r != 0) return r;
        r = Test2();
        if (r != 0) return r;
        return 0;
    }
}

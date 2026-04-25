using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

public class StringCharByref
{
    public static int TestSecondCharByUnsafeAdd()
    {
        string s = "ab";
        ReadOnlySpan<char> span = s;
        ref char first = ref MemoryMarshal.GetReference(span);
        ref char second = ref Unsafe.Add(ref first, 1);

        if (second != 'b')
            return 1;

        return 0;
    }

    public static int TestStringSpanAsBytesToArray()
    {
        string s = "ab";
        ReadOnlySpan<char> span = s;
        byte[] bytes = MemoryMarshal.AsBytes(span).ToArray();

        if (bytes.Length != 4)
            return 2;
        if (bytes[0] != (byte)'a')
            return 3;
        if (bytes[1] != 0)
            return 4;
        if (bytes[2] != (byte)'b')
            return 5;
        if (bytes[3] != 0)
            return 6;

        return 0;
    }

    public static int Main(string[] argv)
    {
        int result = TestSecondCharByUnsafeAdd();
        if (result != 0)
            return result;

        result = TestStringSpanAsBytesToArray();
        if (result != 0)
            return result;

        return 0;
    }
}

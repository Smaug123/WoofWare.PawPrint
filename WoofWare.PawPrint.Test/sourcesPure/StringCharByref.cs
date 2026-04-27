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

    public static int TestByteViewAddCanonicalisesToNextChar()
    {
        string s = "ab";
        ReadOnlySpan<char> span = s;
        ref char first = ref MemoryMarshal.GetReference(span);
        ref byte firstByte = ref Unsafe.As<char, byte>(ref first);

        ref byte steppedToSecondChar = ref Unsafe.Add(ref firstByte, 2);

        ref char second = ref Unsafe.Add(ref first, 1);
        ref byte directSecondCharByte = ref Unsafe.As<char, byte>(ref second);

        if (!Unsafe.AreSame(ref steppedToSecondChar, ref directSecondCharByte))
            return 7;

        return 0;
    }

    public static int TestDirectStringCharWrite()
    {
        string s = "ab";
        ReadOnlySpan<char> span = s;
        ref char first = ref MemoryMarshal.GetReference(span);

        first = 'z';

        if (s[0] != 'z')
            return 8;

        if (s[1] != 'b')
            return 9;

        return 0;
    }

    public static int TestStringByteViewWrite()
    {
        string s = "ab";
        ReadOnlySpan<char> span = s;
        ref char first = ref MemoryMarshal.GetReference(span);
        ref byte firstByte = ref Unsafe.As<char, byte>(ref first);

        Unsafe.WriteUnaligned(ref firstByte, (ushort)'x');

        if (s[0] != 'x')
            return 10;

        if (s[1] != 'b')
            return 11;

        ref byte secondCharByte = ref Unsafe.Add(ref firstByte, 2);
        Unsafe.WriteUnaligned(ref secondCharByte, (ushort)'y');

        if (s[0] != 'x')
            return 12;

        if (s[1] != 'y')
            return 13;

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

        result = TestByteViewAddCanonicalisesToNextChar();
        if (result != 0)
            return result;

        result = TestDirectStringCharWrite();
        if (result != 0)
            return result;

        result = TestStringByteViewWrite();
        if (result != 0)
            return result;

        return 0;
    }
}

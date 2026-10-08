using System;
using System.Runtime.CompilerServices;

// A span read out of `stackalloc` memory in which only its length was written: its reference
// is undefined. An empty span never uses its reference, so indexing it throws the ordinary
// bounds exception, and clearing it or making a string of it reads nothing through it, whatever
// the stack held. Likewise a comparison reads no character that cannot change its
// answer.

[module: SkipLocalsInit]

unsafe class Program
{
    static int Main(string[] args)
    {
        byte* block = stackalloc byte[32];
        // A span is its reference followed by its length and then padding, which is written
        // too: a value type's padding is stored as a number.
        *(int*)(block + sizeof(void*)) = 0;
        *(int*)(block + sizeof(void*) + 4) = 0;

        Span<int> span = Unsafe.Read<Span<int>>(block);
        if (span.Length != 0) return 1;

        span.Clear();

        try
        {
            _ = span[0];
            return 2;
        }
        catch (IndexOutOfRangeException)
        {
        }

        ReadOnlySpan<int> readOnly = Unsafe.Read<ReadOnlySpan<int>>(block);
        try
        {
            _ = readOnly[0];
            return 3;
        }
        catch (IndexOutOfRangeException)
        {
        }

        ReadOnlySpan<char> noChars = Unsafe.Read<ReadOnlySpan<char>>(block);
        if (noChars.ToString() != "") return 4;

        // Characters nothing wrote, which decide nothing: the lengths differ, or an earlier
        // character already does.
        char* chars = stackalloc char[3];
        chars[0] = 'a';
        ReadOnlySpan<char> partly = new ReadOnlySpan<char>(chars, 3);
        if (partly.Equals("ab".AsSpan(), StringComparison.Ordinal)) return 7;
        if (partly.Equals("bcd".AsSpan(), StringComparison.Ordinal)) return 8;
        if (partly.Equals("ab".AsSpan(), StringComparison.OrdinalIgnoreCase)) return 9;

        return 0;
    }
}

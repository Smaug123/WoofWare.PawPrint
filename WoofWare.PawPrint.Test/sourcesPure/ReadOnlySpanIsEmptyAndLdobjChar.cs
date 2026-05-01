using System;

class Program
{
    static int Main(string[] args)
    {
        ReadOnlySpan<char> chars = "AZ";
        if (chars.IsEmpty)
        {
            return 1;
        }

        if (chars[0] != 'A')
        {
            return 2;
        }

        if (chars[1] != 'Z')
        {
            return 3;
        }

        ReadOnlySpan<int> emptyReadOnly = new ReadOnlySpan<int>(new int[0]);
        if (!emptyReadOnly.IsEmpty)
        {
            return 4;
        }

        Span<int> emptySpan = new Span<int>(new int[0]);
        if (!emptySpan.IsEmpty)
        {
            return 5;
        }

        Span<int> nonEmptySpan = new Span<int>(new int[1]);
        if (nonEmptySpan.IsEmpty)
        {
            return 6;
        }

        return 0;
    }
}

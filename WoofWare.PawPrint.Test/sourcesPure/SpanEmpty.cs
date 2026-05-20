using System;

public class TestSpanEmpty
{
    static int IntSpanEmpty()
    {
        Span<int> s = Span<int>.Empty;
        if (s.Length != 0) return 1;
        if (!s.IsEmpty) return 2;
        return 0;
    }

    static int IntReadOnlySpanEmpty()
    {
        ReadOnlySpan<int> s = ReadOnlySpan<int>.Empty;
        if (s.Length != 0) return 11;
        if (!s.IsEmpty) return 12;
        return 0;
    }

    static int ObjectSpanEmpty()
    {
        Span<object> s = Span<object>.Empty;
        if (s.Length != 0) return 21;
        if (!s.IsEmpty) return 22;
        return 0;
    }

    public static int Main(string[] argv)
    {
        int result = IntSpanEmpty();
        if (result != 0) return result;

        result = IntReadOnlySpanEmpty();
        if (result != 0) return result;

        result = ObjectSpanEmpty();
        if (result != 0) return result;

        return 0;
    }
}

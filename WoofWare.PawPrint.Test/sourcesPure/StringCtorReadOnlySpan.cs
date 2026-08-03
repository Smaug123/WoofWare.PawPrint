using System;

unsafe class StringCtorReadOnlySpan
{
    static int TestPointerBackedSpan()
    {
        char* buf = stackalloc char[3];
        buf[0] = 'a';
        buf[1] = 'b';
        buf[2] = 'c';

        ReadOnlySpan<char> span = new ReadOnlySpan<char>(buf, 3);
        string s = new string(span);
        if (s.Length != 3)
            return 1;
        if (s != "abc")
            return 2;

        return 0;
    }

    static int TestEmptySpanIsCanonicalEmptyString()
    {
        char* buf = stackalloc char[1];
        buf[0] = 'z';

        // A zero-length span allocates nothing: PawPrint collapses empty-string
        // allocations onto the interned "" so reference comparisons agree with .NET.
        ReadOnlySpan<char> span = new ReadOnlySpan<char>(buf, 0);
        string s = new string(span);
        if (s.Length != 0)
            return 10;
        if (!ReferenceEquals(s, ""))
            return 11;

        return 0;
    }

    static int TestEmbeddedNullIsPreserved()
    {
        // Unlike the char* constructor, the span constructor copies exactly Length
        // chars and does not stop at a NUL.
        char* buf = stackalloc char[3];
        buf[0] = 'a';
        buf[1] = '\0';
        buf[2] = 'c';

        ReadOnlySpan<char> span = new ReadOnlySpan<char>(buf, 3);
        string s = new string(span);
        if (s.Length != 3)
            return 20;
        if (s[0] != 'a')
            return 21;
        if (s[1] != '\0')
            return 22;
        if (s[2] != 'c')
            return 23;

        return 0;
    }

    static int Main(string[] args)
    {
        int result = TestPointerBackedSpan();
        if (result != 0)
            return result;

        result = TestEmptySpanIsCanonicalEmptyString();
        if (result != 0)
            return result;

        result = TestEmbeddedNullIsPreserved();
        if (result != 0)
            return result;

        return 0;
    }
}

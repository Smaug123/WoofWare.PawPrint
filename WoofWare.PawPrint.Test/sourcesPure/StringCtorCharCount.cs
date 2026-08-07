using System;

// `new string(char, int)` is the third of CoreCLR's nine string constructors that
// are implemented by redirecting the ECall to the managed static `String.Ctor`.
// Its body has a distinctive shape worth pinning: `count <= 0` throws only for a
// *negative* count and otherwise returns `String.Empty`, and the fill is skipped
// entirely for '\0' (leaving FastAllocateString's zeroed buffer).
class StringCtorCharCount
{
    static int TestRepeat()
    {
        string s = new string('x', 4);
        if (s.Length != 4)
            return 1;
        if (s != "xxxx")
            return 2;

        return 0;
    }

    static int TestSingle()
    {
        if (new string('q', 1) != "q")
            return 10;

        return 0;
    }

    static int TestZeroCountIsCanonicalEmpty()
    {
        string s = new string('x', 0);
        if (s.Length != 0)
            return 20;
        if (!ReferenceEquals(s, ""))
            return 21;

        return 0;
    }

    static int TestNulCharSkipsFill()
    {
        // Ctor(char, int) skips SpanHelpers.Fill when c == '\0', relying on the
        // freshly-allocated string already being zeroed.
        string s = new string('\0', 3);
        if (s.Length != 3)
            return 30;
        if (s[0] != '\0' || s[1] != '\0' || s[2] != '\0')
            return 31;

        return 0;
    }

    // The negative-count case lives in StringCtorArgumentValidation.cs, which is parked
    // on an unrelated `[InlineArray]` gap in the throw helpers' message formatting.

    static int Main(string[] args)
    {
        int result = TestRepeat();
        if (result != 0)
            return result;

        result = TestSingle();
        if (result != 0)
            return result;

        result = TestZeroCountIsCanonicalEmpty();
        if (result != 0)
            return result;

        result = TestNulCharSkipsFill();
        if (result != 0)
            return result;

        return 0;
    }
}

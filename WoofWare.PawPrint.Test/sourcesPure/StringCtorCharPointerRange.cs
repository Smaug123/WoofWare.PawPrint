using System;

// `new string(char*, int, int)`. Distinct from the single-argument char* overload
// covered by StringCtorCharPointer.cs: this one takes an explicit length rather
// than scanning for a NUL, so interior NULs survive, and it has its own argument
// validation (including a null pointer being legal exactly when length == 0).
unsafe class StringCtorCharPointerRange
{
    static int TestRange()
    {
        char[] chars = { 'h', 'e', 'l', 'l', 'o' };
        fixed (char* p = chars)
        {
            if (new string(p, 1, 3) != "ell")
                return 1;
            if (new string(p, 0, 5) != "hello")
                return 2;
        }

        return 0;
    }

    static int TestEmbeddedNullIsPreserved()
    {
        char[] chars = { 'a', '\0', 'b' };
        fixed (char* p = chars)
        {
            string s = new string(p, 0, 3);
            if (s.Length != 3)
                return 10;
            if (s[1] != '\0')
                return 11;
        }

        return 0;
    }

    static int TestZeroLengthIsCanonicalEmpty()
    {
        char[] chars = { 'a', 'b' };
        fixed (char* p = chars)
        {
            string s = new string(p, 1, 0);
            if (!ReferenceEquals(s, ""))
                return 20;
        }

        // A null pointer is checked only *after* the length == 0 early return,
        // so this is legal and yields the canonical empty string.
        if (!ReferenceEquals(new string((char*)null, 0, 0), ""))
            return 21;

        return 0;
    }

    // The null-pointer-with-length and negative-argument cases live in
    // StringCtorArgumentValidation.cs, which is parked on an unrelated `[InlineArray]`
    // gap in the throw helpers' message formatting.

    static int Main(string[] args)
    {
        int result = TestRange();
        if (result != 0)
            return result;

        result = TestEmbeddedNullIsPreserved();
        if (result != 0)
            return result;

        result = TestZeroLengthIsCanonicalEmpty();
        if (result != 0)
            return result;

        return 0;
    }
}

using System;

// `new string(sbyte*)` and `new string(sbyte*, int, int)`. On Unix both route
// through `CreateStringForSByteConstructor` -> `CreateStringFromEncoding(..,
// Encoding.UTF8)`, so this also exercises the UTF-8 decode path from unmanaged
// memory. Kept to ASCII so the expected result is unambiguous under the
// invariant-globalization mode PawPrint runs in.
unsafe class StringCtorSbytePointer
{
    static int TestNulTerminated()
    {
        byte[] bytes = { (byte)'h', (byte)'e', (byte)'l', (byte)'l', (byte)'o', 0 };
        fixed (byte* b = bytes)
        {
            string s = new string((sbyte*)b);
            if (s.Length != 5)
                return 1;
            if (s != "hello")
                return 2;
        }

        return 0;
    }

    static int TestNullPointerIsCanonicalEmpty()
    {
        string s = new string((sbyte*)null);
        if (!ReferenceEquals(s, ""))
            return 10;

        return 0;
    }

    static int TestEmptyIsCanonicalEmpty()
    {
        byte[] bytes = { 0 };
        fixed (byte* b = bytes)
        {
            if (!ReferenceEquals(new string((sbyte*)b), ""))
                return 20;
        }

        return 0;
    }

    static int TestRange()
    {
        byte[] bytes = { (byte)'h', (byte)'e', (byte)'l', (byte)'l', (byte)'o' };
        fixed (byte* b = bytes)
        {
            if (new string((sbyte*)b, 1, 3) != "ell")
                return 30;
            // No NUL needed: the length is explicit.
            if (new string((sbyte*)b, 0, 5) != "hello")
                return 31;
            if (!ReferenceEquals(new string((sbyte*)b, 2, 0), ""))
                return 32;
        }

        return 0;
    }

    static int TestRangeNullPointerZeroLength()
    {
        // A null pointer with length 0 is legal (the null check sits after the
        // length == 0 early return); the throwing counterpart lives in
        // StringCtorArgumentValidation.cs.
        if (!ReferenceEquals(new string((sbyte*)null, 0, 0), ""))
            return 40;

        return 0;
    }

    static int Main(string[] args)
    {
        int result = TestNulTerminated();
        if (result != 0)
            return result;

        result = TestNullPointerIsCanonicalEmpty();
        if (result != 0)
            return result;

        result = TestEmptyIsCanonicalEmpty();
        if (result != 0)
            return result;

        result = TestRange();
        if (result != 0)
            return result;

        result = TestRangeNullPointerZeroLength();
        if (result != 0)
            return result;

        return 0;
    }
}

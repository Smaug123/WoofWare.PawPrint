using System;

unsafe class StringCtorCharPointer
{
    static int TestFixedManagedCharArray()
    {
        char[] chars = { 'h', 'e', 'l', 'l', 'o', '\0' };
        fixed (char* p = chars)
        {
            string s = new string(p);
            if (s.Length != 5)
                return 1;
            if (s != "hello")
                return 2;
        }

        return 0;
    }

    static int TestEmptyTerminatedManagedCharArray()
    {
        char[] chars = { '\0' };
        fixed (char* p = chars)
        {
            string s = new string(p);
            if (s.Length != 0)
                return 10;
            if (s != "")
                return 11;
        }

        return 0;
    }

    static int TestNullPointer()
    {
        // Per CoreCLR's String.Ctor(char*) source, a null char* yields String.Empty.
        string s = new string((char*)null);
        if (s.Length != 0)
            return 20;
        if (s != "")
            return 21;

        return 0;
    }

    static int TestStackallocTerminated()
    {
        char* p = stackalloc char[4];
        p[0] = 'h';
        p[1] = 'i';
        p[2] = '!';
        p[3] = '\0';

        string s = new string(p);
        if (s.Length != 3)
            return 30;
        if (s != "hi!")
            return 31;

        return 0;
    }

    static int TestEmbeddedNullStops()
    {
        // The constructor stops at the first NUL, even if more chars follow.
        char[] chars = { 'a', 'b', '\0', 'c', 'd' };
        fixed (char* p = chars)
        {
            string s = new string(p);
            if (s.Length != 2)
                return 40;
            if (s != "ab")
                return 41;
        }

        return 0;
    }

    static int TestEmptyResultsAreCanonical()
    {
        // CoreCLR returns the canonical empty string (i.e. the same reference as
        // the "" literal) when the input pointer is null or empty. PawPrint must
        // preserve that identity so reference comparisons agree with .NET.
        string fromNull = new string((char*)null);
        char[] terminated = { '\0' };
        string fromEmptyArray;
        fixed (char* p = terminated)
        {
            fromEmptyArray = new string(p);
        }

        if (!ReferenceEquals(fromNull, ""))
            return 50;
        if (!ReferenceEquals(fromEmptyArray, ""))
            return 51;
        if (!ReferenceEquals(fromNull, fromEmptyArray))
            return 52;

        return 0;
    }

    static int Main(string[] args)
    {
        int result = TestFixedManagedCharArray();
        if (result != 0)
            return result;

        result = TestEmptyTerminatedManagedCharArray();
        if (result != 0)
            return result;

        result = TestNullPointer();
        if (result != 0)
            return result;

        result = TestStackallocTerminated();
        if (result != 0)
            return result;

        result = TestEmbeddedNullStops();
        if (result != 0)
            return result;

        result = TestEmptyResultsAreCanonical();
        if (result != 0)
            return result;

        return 0;
    }
}

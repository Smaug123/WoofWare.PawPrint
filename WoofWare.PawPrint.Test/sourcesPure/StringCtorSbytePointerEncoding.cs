using System;
using System.Text;

// `new string(sbyte*, int, int, Encoding)` — the ninth and last of CoreCLR's
// string constructors. A null encoding is documented to fall back to the
// three-argument overload, which is worth pinning because it is the one case
// where one `Ctor` overload delegates to another via a nested `newobj`.
unsafe class StringCtorSbytePointerEncoding
{
    static int TestAscii()
    {
        byte[] bytes = { (byte)'h', (byte)'e', (byte)'l', (byte)'l', (byte)'o' };
        fixed (byte* b = bytes)
        {
            string s = new string((sbyte*)b, 0, 5, Encoding.ASCII);
            if (s != "hello")
                return 1;
        }

        return 0;
    }

    static int TestUtf8Range()
    {
        byte[] bytes = { (byte)'h', (byte)'e', (byte)'l', (byte)'l', (byte)'o' };
        fixed (byte* b = bytes)
        {
            if (new string((sbyte*)b, 1, 3, Encoding.UTF8) != "ell")
                return 10;
        }

        return 0;
    }

    static int TestNullEncodingFallsBackToUtf8Overload()
    {
        byte[] bytes = { (byte)'h', (byte)'i' };
        fixed (byte* b = bytes)
        {
            if (new string((sbyte*)b, 0, 2, null) != "hi")
                return 20;
        }

        return 0;
    }

    static int TestZeroLength()
    {
        byte[] bytes = { (byte)'a' };
        fixed (byte* b = bytes)
        {
            if (new string((sbyte*)b, 0, 0, Encoding.UTF8) != "")
                return 30;
        }

        return 0;
    }

    static int Main(string[] args)
    {
        int result = TestAscii();
        if (result != 0)
            return result;

        result = TestUtf8Range();
        if (result != 0)
            return result;

        result = TestNullEncodingFallsBackToUtf8Overload();
        if (result != 0)
            return result;

        result = TestZeroLength();
        if (result != 0)
            return result;

        return 0;
    }
}

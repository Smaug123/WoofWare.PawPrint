using System.Runtime.CompilerServices;

public struct SkipInitStruct
{
    public int First;
    public long Second;
}

public class TestUnsafeSkipInit
{
    public static int TestIntLocal()
    {
        int before = 17;
        int x;
        int after = 23;

        Unsafe.SkipInit(out x);
        x = 42;

        if (before != 17)
            return 1;
        if (after != 23)
            return 2;
        if (x != 42)
            return 3;
        return 0;
    }

    public static int TestStructLocal()
    {
        int before = 101;
        SkipInitStruct value;
        int after = 202;

        Unsafe.SkipInit(out value);
        value.First = 303;
        value.Second = 4040404040L;

        if (before != 101)
            return 4;
        if (after != 202)
            return 5;
        if (value.First != 303)
            return 6;
        if (value.Second != 4040404040L)
            return 7;
        return 0;
    }

    public static int Main(string[] argv)
    {
        int r = TestIntLocal();
        if (r != 0) return r;
        r = TestStructLocal();
        if (r != 0) return r;
        return 0;
    }
}

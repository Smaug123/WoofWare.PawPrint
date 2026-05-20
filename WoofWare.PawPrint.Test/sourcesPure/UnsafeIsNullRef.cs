using System.Runtime.CompilerServices;

public class TestUnsafeIsNullRef
{
    public static int Main(string[] argv)
    {
        if (!Unsafe.IsNullRef(ref Unsafe.NullRef<int>()))
            return 1;

        int value = 42;

        if (Unsafe.IsNullRef(ref value))
            return 2;

        if (!Unsafe.IsNullRef(ref Unsafe.NullRef<string>()))
            return 3;

        string text = "not null";

        if (Unsafe.IsNullRef(ref text))
            return 4;

        return 0;
    }
}

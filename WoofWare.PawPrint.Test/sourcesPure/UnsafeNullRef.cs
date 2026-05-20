using System.Runtime.CompilerServices;

public class TestUnsafeNullRef
{
    public static unsafe int Main(string[] argv)
    {
        if (!Unsafe.AreSame(ref Unsafe.NullRef<int>(), ref Unsafe.NullRef<int>()))
            return 1;

        int value = 42;

        if (Unsafe.AreSame(ref Unsafe.NullRef<int>(), ref value))
            return 2;

        if (Unsafe.AsPointer(ref Unsafe.NullRef<int>()) != null)
            return 3;

        if (!Unsafe.AreSame(ref Unsafe.NullRef<string>(), ref Unsafe.NullRef<string>()))
            return 4;

        string text = "not null";

        if (Unsafe.AreSame(ref Unsafe.NullRef<string>(), ref text))
            return 5;

        return 0;
    }
}

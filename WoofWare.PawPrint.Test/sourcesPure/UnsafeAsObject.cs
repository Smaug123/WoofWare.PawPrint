using System.Runtime.CompilerServices;

public class TestUnsafeAsObject
{
    private sealed class Box
    {
        public int Value;
    }

    public static int Main(string[] argv)
    {
        object obj = new Box { Value = 17 };
        Box box = Unsafe.As<Box>(obj);

        if (box.Value != 17)
            return 1;

        box.Value = 42;

        if (((Box)obj).Value != 42)
            return 2;

        return 0;
    }
}

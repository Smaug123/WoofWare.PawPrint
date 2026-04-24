using System.Runtime.CompilerServices;

public class TestUnsafePrimitiveByteView
{
    public static int Test1()
    {
        int value = 0x11223344;
        ref byte first = ref Unsafe.As<int, byte>(ref value);

        if (first != 0x44)
            return 1;

        first = 0x99;

        if (value != 0x11223399)
            return 2;

        return 0;
    }

    public static int Test2()
    {
        int value = 0x11223344;
        ref byte first = ref Unsafe.As<int, byte>(ref value);
        ref byte third = ref Unsafe.Add(ref first, 2);

        if (third != 0x22)
            return 3;

        third = 0xAA;

        if (value != 0x11AA3344)
            return 4;

        return 0;
    }

    public static int Test3()
    {
        float value = 1.0f;
        ref int bits = ref Unsafe.As<float, int>(ref value);

        if (bits != 0x3F800000)
            return 5;

        bits = 0x40000000;

        if (value != 2.0f)
            return 6;

        return 0;
    }

    public static int Test4()
    {
        bool value = true;
        ref byte asByte = ref Unsafe.As<bool, byte>(ref value);

        if (asByte != 1)
            return 7;

        asByte = 0;

        if (value)
            return 8;

        return 0;
    }

    public static int Main(string[] argv)
    {
        int r = Test1();
        if (r != 0) return r;
        r = Test2();
        if (r != 0) return r;
        r = Test3();
        if (r != 0) return r;
        r = Test4();
        if (r != 0) return r;
        return 0;
    }
}

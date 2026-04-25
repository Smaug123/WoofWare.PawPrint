using System.Runtime.CompilerServices;

public class TestUnsafePrimitiveByteView
{
    private class Holder
    {
        public int Value;
    }

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

    public static int Test5()
    {
        char value = '\u1234';
        ref byte first = ref Unsafe.As<char, byte>(ref value);
        ref byte second = ref Unsafe.Add(ref first, 1);

        if (first != 0x34)
            return 9;

        if (second != 0x12)
            return 10;

        second = 0xAB;

        if (value != '\uAB34')
            return 11;

        return 0;
    }

    public static int Test6()
    {
        ushort value = 0xBBAA;
        ref byte first = ref Unsafe.As<ushort, byte>(ref value);
        ref byte second = ref Unsafe.Add(ref first, 1);

        if (first != 0xAA)
            return 12;

        if (second != 0xBB)
            return 13;

        first = 0x34;
        second = 0x12;

        if (value != 0x1234)
            return 14;

        return 0;
    }

    public static int Test7()
    {
        Holder holder = new Holder();
        holder.Value = 0x11223344;
        ref byte first = ref Unsafe.As<int, byte>(ref holder.Value);

        if (first != 0x44)
            return 15;

        first = 0x88;

        if (holder.Value != 0x11223388)
            return 16;

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
        r = Test5();
        if (r != 0) return r;
        r = Test6();
        if (r != 0) return r;
        r = Test7();
        if (r != 0) return r;
        return 0;
    }
}

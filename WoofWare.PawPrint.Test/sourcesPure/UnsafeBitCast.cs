using System;
using System.Runtime.CompilerServices;

public class TestUnsafeBitCast
{
    private struct FourBytes
    {
        public byte B0;
        public byte B1;
        public byte B2;
        public byte B3;
    }

    private struct EightBytes
    {
        public byte B0;
        public byte B1;
        public byte B2;
        public byte B3;
        public byte B4;
        public byte B5;
        public byte B6;
        public byte B7;
    }

    // Test 1: identity bitcast is the identity function.
    public static int Test1()
    {
        int original = 0x12345678;
        int copy = Unsafe.BitCast<int, int>(original);
        if (copy != original) return 1;

        int negative = -1;
        if (Unsafe.BitCast<int, int>(negative) != -1) return 2;

        return 0;
    }

    // Test 2: int <-> uint reinterpret preserves the bit pattern.
    public static int Test2()
    {
        uint reinterpreted = Unsafe.BitCast<int, uint>(-1);
        if (reinterpreted != 0xFFFFFFFFu) return 1;

        int back = Unsafe.BitCast<uint, int>(0x80000000u);
        if (back != int.MinValue) return 2;

        return 0;
    }

    // Test 3: int <-> float round-trip preserves the bit pattern for arbitrary inputs.
    public static int Test3()
    {
        // Pick non-trivial bit patterns; round-trip through float and back.
        int[] inputs = { 0, 1, -1, 0x40490FDB, unchecked((int)0x80000001), 0x7F800000, 0x7FC00000 };
        for (int i = 0; i < inputs.Length; i++)
        {
            int x = inputs[i];
            float f = Unsafe.BitCast<int, float>(x);
            int y = Unsafe.BitCast<float, int>(f);
            if (y != x) return 100 + i;
        }

        return 0;
    }

    // Test 4: long <-> double round-trip preserves the bit pattern for arbitrary inputs.
    public static int Test4()
    {
        long[] inputs = { 0L, 1L, -1L, 0x4009_21FB_5444_2D18L, unchecked((long)0x8000_0000_0000_0001L) };
        for (int i = 0; i < inputs.Length; i++)
        {
            long x = inputs[i];
            double d = Unsafe.BitCast<long, double>(x);
            long y = Unsafe.BitCast<double, long>(d);
            if (y != x) return 100 + i;
        }

        return 0;
    }

    // Test 5: char <-> ushort reinterpret preserves the bit pattern.
    public static int Test5()
    {
        ushort u = Unsafe.BitCast<char, ushort>('A');
        if (u != 65) return 1;

        char c = Unsafe.BitCast<ushort, char>(0x1234);
        if (c != 'ሴ') return 2;

        return 0;
    }

    // Test 6: byte <-> sbyte reinterpret preserves the bit pattern.
    public static int Test6()
    {
        sbyte s = Unsafe.BitCast<byte, sbyte>(0xFF);
        if (s != -1) return 1;

        byte b = Unsafe.BitCast<sbyte, byte>(-128);
        if (b != 0x80) return 2;

        return 0;
    }

    // Test 7: struct <-> primitive of identical byte size.
    // Sequential layout with byte fields packs tightly; total size 4 bytes.
    // Assumes little-endian, matching every platform PawPrint runs on.
    public static int Test7()
    {
        FourBytes fb;
        fb.B0 = 0x78;
        fb.B1 = 0x56;
        fb.B2 = 0x34;
        fb.B3 = 0x12;

        int asInt = Unsafe.BitCast<FourBytes, int>(fb);
        if (asInt != 0x12345678) return 1;

        FourBytes back = Unsafe.BitCast<int, FourBytes>(0x0BADF00D);
        if (back.B0 != 0x0D) return 2;
        if (back.B1 != 0xF0) return 3;
        if (back.B2 != 0xAD) return 4;
        if (back.B3 != 0x0B) return 5;

        return 0;
    }

    // Test 8: 8-byte struct <-> long.
    public static int Test8()
    {
        EightBytes eb;
        eb.B0 = 0xEF;
        eb.B1 = 0xCD;
        eb.B2 = 0xAB;
        eb.B3 = 0x89;
        eb.B4 = 0x67;
        eb.B5 = 0x45;
        eb.B6 = 0x23;
        eb.B7 = 0x01;

        long asLong = Unsafe.BitCast<EightBytes, long>(eb);
        if (asLong != 0x0123456789ABCDEFL) return 1;

        EightBytes back = Unsafe.BitCast<long, EightBytes>(0x1122334455667788L);
        if (back.B0 != 0x88) return 2;
        if (back.B1 != 0x77) return 3;
        if (back.B7 != 0x11) return 4;

        return 0;
    }

    // Test 9: mismatched sizes throw NotSupportedException.
    //
    // `Unsafe.BitCast` guards with `if (sizeof(TFrom) != sizeof(TTo) || !typeof(TFrom).IsValueType
    // || !typeof(TTo).IsValueType) ThrowHelper.ThrowNotSupportedException();`, and the JIT
    // deliberately declines to expand in exactly those cases ("Fallback to the software
    // implementation to throw when sizes don't match"), so the managed body runs and throws.
    // `ThrowNotSupportedException` uses the parameterless ctor, so the message is the default.
    public static int Test9()
    {
        try
        {
            long widened = Unsafe.BitCast<int, long>(5);
            return 1;
        }
        catch (NotSupportedException)
        {
        }

        try
        {
            byte narrowed = Unsafe.BitCast<int, byte>(0x12345678);
            return 2;
        }
        catch (NotSupportedException)
        {
        }

        // Struct sizes are compared too, not just primitive ones.
        try
        {
            FourBytes fb = Unsafe.BitCast<EightBytes, FourBytes>(default);
            return 3;
        }
        catch (NotSupportedException)
        {
        }

        // The guard is a single `if` with three clauses, so a *reference* type is rejected
        // too, even though both sides are pointer-sized and the sizes therefore agree:
        //
        //   if (sizeof(TFrom) != sizeof(TTo) || !typeof(TFrom).IsValueType || !typeof(TTo).IsValueType)
        //
        // The JIT declines to expand for reference types for exactly this reason ("Fallback to
        // the software implementation to throw for reference types").
        try
        {
            object o = Unsafe.BitCast<string, object>("x");
            return 4;
        }
        catch (NotSupportedException)
        {
        }

        // ...in either position.
        try
        {
            string s = Unsafe.BitCast<object, string>(new object());
            return 5;
        }
        catch (NotSupportedException)
        {
        }

        // `ThrowHelper.ThrowNotSupportedException` uses the parameterless ctor, so HResult is
        // COR_E_NOTSUPPORTED rather than the generic COR_E_EXCEPTION.
        try
        {
            long widened = Unsafe.BitCast<int, long>(5);
            return 6;
        }
        catch (NotSupportedException e)
        {
            if (e.HResult != unchecked((int) 0x80131515))
            {
                return 7;
            }
        }

        return 0;
    }

    public static int Main(string[] argv)
    {
        var result = Test1();
        if (result != 0) return result;

        result = Test2();
        if (result != 0) return result;

        result = Test3();
        if (result != 0) return result;

        result = Test4();
        if (result != 0) return result;

        result = Test5();
        if (result != 0) return result;

        result = Test6();
        if (result != 0) return result;

        result = Test7();
        if (result != 0) return result;

        result = Test8();
        if (result != 0) return result;

        result = Test9();
        if (result != 0) return 90 + result;

        return 0;
    }
}

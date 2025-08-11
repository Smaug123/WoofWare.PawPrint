using System;
using System.Runtime.CompilerServices;

public class TestUnsafeAs
{
    private struct Int32Wrapper
    {
        public int Value;
    }

    private struct UInt32Wrapper
    {
        public uint Value;
    }

    private struct TwoInt16s
    {
        public short First;
        public short Second;
    }

    private struct FourBytes
    {
        public byte B0;
        public byte B1;
        public byte B2;
        public byte B3;
    }

    private enum TestEnum : int
    {
        Value1 = 0x12345678,
        Value2 = -1
    }

    // Test 1: Int32 -> UInt32 reinterpretation
    public static int Test1()
    {
        int original = -1;
        ref uint reinterpreted = ref Unsafe.As<int, uint>(ref original);

        if (reinterpreted != 0xFFFFFFFF)
            return 1;

        reinterpreted = 0x12345678;
        if (original != 0x12345678)
            return 2;

        original = int.MinValue;
        if (reinterpreted != 0x80000000)
            return 3;

        return 0;
    }

    // Test 2: Struct -> Struct reinterpretation
    public static int Test2()
    {
        Int32Wrapper wrapper = new Int32Wrapper { Value = 0x01020304 };
        ref FourBytes bytes = ref Unsafe.As<Int32Wrapper, FourBytes>(ref wrapper);

        if (BitConverter.IsLittleEndian)
        {
            if (bytes.B0 != 0x04) return 10;
            if (bytes.B1 != 0x03) return 11;
            if (bytes.B2 != 0x02) return 12;
            if (bytes.B3 != 0x01) return 13;
        }
        else
        {
            if (bytes.B0 != 0x01) return 14;
            if (bytes.B1 != 0x02) return 15;
            if (bytes.B2 != 0x03) return 16;
            if (bytes.B3 != 0x04) return 17;
        }

        bytes.B0 = 0xFF;
        int expectedValue = BitConverter.IsLittleEndian ? 0x010203FF : unchecked((int)0xFF020304);
        if (wrapper.Value != expectedValue)
            return 18;

        return 0;
    }

    // Test 3: Int32 -> Two Int16s
    public static int Test3()
    {
        int value = 0x12345678;
        ref TwoInt16s halves = ref Unsafe.As<int, TwoInt16s>(ref value);

        if (BitConverter.IsLittleEndian)
        {
            if (halves.First != unchecked((short)0x5678)) return 20;
            if (halves.Second != 0x1234) return 21;
        }
        else
        {
            if (halves.First != 0x1234) return 22;
            if (halves.Second != unchecked((short)0x5678)) return 23;
        }

        halves.First = -1;
        int expectedValue = BitConverter.IsLittleEndian ? 0x1234FFFF : unchecked((int)0xFFFF5678);
        if (value != expectedValue)
            return 24;

        return 0;
    }

    // Test 4: Array element reinterpretation
    public static int Test4()
    {
        int[] intArray = new int[] { 0x01020304, 0x05060708 };
        ref uint uintRef = ref Unsafe.As<int, uint>(ref intArray[0]);

        if (uintRef != 0x01020304u)
            return 30;

        uintRef = 0xAABBCCDD;
        if (intArray[0] != unchecked((int)0xAABBCCDD))
            return 31;
        if (intArray[1] != 0x05060708)
            return 32;

        return 0;
    }

    // Test 5: Bool -> Byte
    public static int Test5()
    {
        bool trueValue = true;
        bool falseValue = false;

        ref byte trueByte = ref Unsafe.As<bool, byte>(ref trueValue);
        ref byte falseByte = ref Unsafe.As<bool, byte>(ref falseValue);

        if (trueByte != 1)
            return 40;
        if (falseByte != 0)
            return 41;

        // Modify through byte reference
        trueByte = 0;
        if (trueValue != false)
            return 42;

        falseByte = 1;
        if (falseValue != true)
            return 43;

        return 0;
    }

    // Test 6: Char -> UInt16
    public static int Test6()
    {
        char ch = 'A';
        ref ushort asUInt16 = ref Unsafe.As<char, ushort>(ref ch);

        if (asUInt16 != 65)
            return 50;

        asUInt16 = 0x03B1; // Greek lowercase alpha
        if (ch != 'α')
            return 51;

        return 0;
    }

    // Test 7: Float -> Int32
    public static int Test7()
    {
        float floatValue = 1.0f;
        ref int intBits = ref Unsafe.As<float, int>(ref floatValue);

        // IEEE 754: 1.0f = 0x3F800000
        if (intBits != 0x3F800000)
            return 60;

        intBits = 0x40000000; // 2.0f in IEEE 754
        if (floatValue != 2.0f)
            return 61;

        floatValue = -0.0f;
        if (intBits != unchecked((int)0x80000000))
            return 62;

        return 0;
    }

    // Test 8: Double -> Int64
    public static int Test8()
    {
        double doubleValue = 1.0;
        ref long longBits = ref Unsafe.As<double, long>(ref doubleValue);

        // IEEE 754: 1.0 = 0x3FF0000000000000
        if (longBits != 0x3FF0000000000000L)
            return 70;

        longBits = 0x4000000000000000L; // 2.0 in IEEE 754
        if (doubleValue != 2.0)
            return 71;

        return 0;
    }

    // Test 9: Enum -> Underlying type
    public static int Test9()
    {
        TestEnum enumValue = TestEnum.Value1;
        ref int underlying = ref Unsafe.As<TestEnum, int>(ref enumValue);

        if (underlying != 0x12345678)
            return 80;

        underlying = -1;
        if (enumValue != TestEnum.Value2)
            return 81;

        return 0;
    }

    // Test 10: Local variable reinterpretation
    public static int Test10()
    {
        int local = unchecked((int)0xDEADBEEF);
        ref uint localAsUint = ref Unsafe.As<int, uint>(ref local);

        if (localAsUint != 0xDEADBEEF)
            return 90;

        localAsUint = 0xCAFEBABE;
        if (local != unchecked((int)0xCAFEBABE))
            return 91;

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
        if (result != 0) return result;

        result = Test10();
        if (result != 0) return result;

        // All tests passed
        return 0;
    }
}

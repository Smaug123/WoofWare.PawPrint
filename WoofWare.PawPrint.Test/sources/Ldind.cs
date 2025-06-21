using System;

unsafe class LdindTest
{
    static int Main(string[] args)
    {
        var failures = 0;
        // Test Ldind.i1 (signed byte)
        failures += TestLdindI1();

        // Test Ldind.u1 (unsigned byte)
        failures += TestLdindU1();

        // Test Ldind.i2 (signed short)
        failures += TestLdindI2();

        // Test Ldind.u2 (unsigned short)
        failures += TestLdindU2();

        // Test Ldind.i4 (signed int)
        failures += TestLdindI4();

        // Test Ldind.u4 (unsigned int)
        failures += TestLdindU4();

        // Test Ldind.i8 (signed long)
        failures += TestLdindI8();

        // Test Ldind.i8 via u8 (there's no Ldind.u8)
        failures += TestLdindI8ViaU8();

        // Test Ldind.r4 (float)
        failures += TestLdindR4();

        // Test Ldind.r8 (double)
        failures += TestLdindR8();

        // Test truncation behavior
        failures += TestTruncation();

        // Test with managed pointers (ref)
        // failures += TestManagedPointers();

        // Test Ldind.i (native int)
        // failures += TestLdindI();

        return failures;
    }

    static int TestLdindI1()
    {
        sbyte value = -128;
        sbyte* ptr = &value;
        sbyte loaded = *ptr;  // This generates ldind.i1
        if (value != loaded)
        {
            return 1;
        }

        value = 127;
        loaded = *ptr;
        if (value != loaded)
        {
            return 1;
        }

        return 0;
    }

    static int TestLdindU1()
    {
        byte value = 255;
        byte* ptr = &value;
        byte loaded = *ptr;  // This generates ldind.u1
        if (value != loaded)
        {
            return 1;
        }

        value = 0;
        loaded = *ptr;
        if (value != loaded)
        {
            return 1;
        }

        return 0;
    }

    static int TestLdindI2()
    {
        short value = -32768;
        short* ptr = &value;
        short loaded = *ptr;  // This generates ldind.i2
        if (value != loaded)
        {
            return 1;
        }

        value = 32767;
        loaded = *ptr;
        if (value != loaded)
        {
            return 1;
        }

        return 0;
    }

    static int TestLdindU2()
    {
        ushort value = 65535;
        ushort* ptr = &value;
        ushort loaded = *ptr;  // This generates ldind.u2
        if (value != loaded)
        {
            return 1;
        }

        value = 0;
        loaded = *ptr;
        if (value != loaded)
        {
            return 1;
        }

        return 0;
    }

    static int TestLdindI4()
    {
        int value = int.MinValue;
        int* ptr = &value;
        int loaded = *ptr;  // This generates ldind.i4
        if (value != loaded)
        {
            return 1;
        }

        value = int.MaxValue;
        loaded = *ptr;
        if (value != loaded)
        {
            return 1;
        }

        return 0;
    }

    static int TestLdindU4()
    {
        uint value = uint.MaxValue;
        uint* ptr = &value;
        uint loaded = *ptr;  // This generates ldind.u4
        if (value != loaded)
        {
            return 1;
        }

        value = 0;
        loaded = *ptr;
        if (value != loaded)
        {
            return 1;
        }

        return 0;
    }

    static int TestLdindI8()
    {
        long value = long.MinValue;
        long* ptr = &value;
        long loaded = *ptr;  // This generates ldind.i8
        if (value != loaded)
        {
            return 1;
        }

        value = long.MaxValue;
        loaded = *ptr;
        if (value != loaded)
        {
            return 1;
        }

        return 0;
    }

    static int TestLdindI8ViaU8()
    {
        ulong value = ulong.MaxValue;
        ulong* ptr = &value;
        ulong loaded = *ptr;  // This generates ldind.i8 again, because there's no ldind.u8
        if (value != loaded)
        {
            return 1;
        }

        value = 0;
        loaded = *ptr;
        if (value != loaded)
        {
            return 1;
        }

        return 0;
    }

    static int TestLdindR4()
    {
        float value = float.MinValue;
        float* ptr = &value;
        float loaded = *ptr;  // This generates ldind.r4
        if (BitConverter.SingleToInt32Bits(value) != BitConverter.SingleToInt32Bits(loaded))
        {
            return 1;
        }

        value = float.MaxValue;
        loaded = *ptr;
        if (BitConverter.SingleToInt32Bits(value) != BitConverter.SingleToInt32Bits(loaded))
        {
            return 1;
        }

        value = float.NaN;
        loaded = *ptr;
        if (BitConverter.SingleToInt32Bits(value) != BitConverter.SingleToInt32Bits(loaded))
        {
            return 1;
        }

        return 0;
    }

    static int TestLdindR8()
    {
        double value = double.MinValue;
        double* ptr = &value;
        double loaded = *ptr;  // This generates ldind.r8
        if (BitConverter.DoubleToInt64Bits(value) != BitConverter.DoubleToInt64Bits(loaded))
        {
            return 1;
        }

        value = double.MaxValue;
        loaded = *ptr;
        if (BitConverter.DoubleToInt64Bits(value) != BitConverter.DoubleToInt64Bits(loaded))
        {
            return 1;
        }

        value = double.NaN;
        loaded = *ptr;
        if (BitConverter.DoubleToInt64Bits(value) != BitConverter.DoubleToInt64Bits(loaded))
        {
            return 1;
        }

        return 0;
    }

    static int TestLdindI()
    {
        IntPtr value = new IntPtr(42);
        IntPtr* ptr = &value;
        IntPtr loaded = *ptr;  // This generates ldind.i
        if (value != loaded)
        {
            return 1;
        }

        value = IntPtr.Zero;
        loaded = *ptr;
        if (value != loaded)
        {
            return 1;
        }

        return 0;
    }

    static int TestTruncation()
    {
        // Store a larger value and load as smaller type
        int largeValue = 0x1234ABCD;
        void* ptr = &largeValue;

        byte byteValue = *(byte*)ptr;  // Should truncate to 0xCD
        if (byteValue != 0xCD)
        {
            return 1;
        }

       short shortValue = *(short*)ptr;  // Should truncate to 0xABCD
       if (shortValue != -21555)
       {
           return 1;
       }

        // Test sign extension
        sbyte signedByte = *(sbyte*)ptr;  // 0xCD as signed byte is -51
        if (signedByte != -51)
        {
            return 1;
        }

        return 0;
    }

    static int TestManagedPointers()
    {
        int value = 42;
        ref int refValue = ref value;
        var expected42 = TestRefParameter(ref refValue);
        if (expected42 != 42)
        {
            return 1;
        }

        if (refValue != 84)
        {
            return 1;
        }

        // Test with array element
        int[] array = { 10, 20, 30 };
        ref int element = ref array[1];
        if (element != 20)
        {
            return 1;
        }

        // Test with local variable
        int local = 100;
        var expected100 = TestRefLocal(ref local);
        if (expected100 != 100)
        {
            return 1;
        }

        return 0;
    }

    static int TestRefParameter(ref int param)
    {
        var result = 0;
        // This will use ldind instructions when accessing param
        result += param;
        param = 84;
        return result;
    }

    static int TestRefLocal(ref int local)
    {
        // Create a ref local
        ref int refLocal = ref local;
        return refLocal;
    }
}

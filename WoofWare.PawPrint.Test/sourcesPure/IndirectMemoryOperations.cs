using System;

public unsafe class TestIndirectMemoryOperations
{
    // Test Ldind_i/Stind_i: Load/Store native int indirect
    public static int TestIndirectNativeInt()
    {
        IntPtr value = new IntPtr(42);
        IntPtr* ptr = &value;

        // Ldind_i: Load native int through pointer
        IntPtr loaded = *ptr;
        if (loaded != new IntPtr(42)) return 1;

        // Stind_i: Store native int through pointer
        *ptr = new IntPtr(100);
        if (value != new IntPtr(100)) return 2;

        // Array of native ints
        IntPtr[] arr = new IntPtr[] { new IntPtr(1), new IntPtr(2), new IntPtr(3) };
        fixed (IntPtr* arrPtr = arr)
        {
            if (arrPtr[0] != new IntPtr(1)) return 3;
            if (arrPtr[1] != new IntPtr(2)) return 4;
            if (arrPtr[2] != new IntPtr(3)) return 5;

            arrPtr[1] = new IntPtr(20);
            if (arr[1] != new IntPtr(20)) return 6;
        }

        return 0;
    }

    // Test Ldind_i1/Stind_i1: Load/Store int8 indirect
    public static int TestIndirectInt8()
    {
        sbyte value = -50;
        sbyte* ptr = &value;

        // Ldind_i1: Load int8
        sbyte loaded = *ptr;
        if (loaded != -50) return 10;

        // Stind_i1: Store int8
        *ptr = 127;
        if (value != 127) return 11;

        *ptr = -128;
        if (value != -128) return 12;

        // Array access
        sbyte[] arr = new sbyte[] { -1, 0, 1, 127, -128 };
        fixed (sbyte* arrPtr = arr)
        {
            if (arrPtr[0] != -1) return 13;
            if (arrPtr[3] != 127) return 14;
            if (arrPtr[4] != -128) return 15;

            arrPtr[2] = 50;
            if (arr[2] != 50) return 16;
        }

        return 0;
    }

    // Test Ldind_u1: Load uint8 indirect
    public static int TestIndirectUInt8()
    {
        byte value = 200;
        byte* ptr = &value;

        // Ldind_u1: Load uint8
        byte loaded = *ptr;
        if (loaded != 200) return 20;

        // Store and load
        *ptr = 255;
        loaded = *ptr;
        if (loaded != 255) return 21;

        *ptr = 0;
        if (*ptr != 0) return 22;

        return 0;
    }

    // Test Ldind_i2/Stind_i2: Load/Store int16 indirect
    public static int TestIndirectInt16()
    {
        short value = -1000;
        short* ptr = &value;

        // Ldind_i2: Load int16
        short loaded = *ptr;
        if (loaded != -1000) return 30;

        // Stind_i2: Store int16
        *ptr = 32767;
        if (value != 32767) return 31;

        *ptr = -32768;
        if (value != -32768) return 32;

        // Multiple values
        short[] arr = new short[] { 100, -200, 300 };
        fixed (short* arrPtr = arr)
        {
            if (arrPtr[0] != 100) return 33;
            if (arrPtr[1] != -200) return 34;
            if (arrPtr[2] != 300) return 35;

            arrPtr[1] = 500;
            if (arr[1] != 500) return 36;
        }

        return 0;
    }

    // Test Ldind_u2: Load uint16 indirect
    public static int TestIndirectUInt16()
    {
        ushort value = 50000;
        ushort* ptr = &value;

        // Ldind_u2: Load uint16
        ushort loaded = *ptr;
        if (loaded != 50000) return 40;

        *ptr = 65535;
        if (*ptr != 65535) return 41;

        *ptr = 0;
        if (*ptr != 0) return 42;

        return 0;
    }

    // Test Ldind_i4/Stind_i4: Load/Store int32 indirect
    public static int TestIndirectInt32()
    {
        int value = -123456;
        int* ptr = &value;

        // Ldind_i4: Load int32
        int loaded = *ptr;
        if (loaded != -123456) return 50;

        // Stind_i4: Store int32
        *ptr = int.MaxValue;
        if (value != int.MaxValue) return 51;

        *ptr = int.MinValue;
        if (value != int.MinValue) return 52;

        // Pointer arithmetic
        int[] arr = new int[] { 10, 20, 30, 40, 50 };
        fixed (int* arrPtr = arr)
        {
            int* p = arrPtr;
            if (*p != 10) return 53;

            p++;
            if (*p != 20) return 54;

            p += 2;
            if (*p != 40) return 55;

            *p = 400;
            if (arr[3] != 400) return 56;
        }

        return 0;
    }

    // Test Ldind_u4: Load uint32 indirect
    public static int TestIndirectUInt32()
    {
        uint value = 0xDEADBEEF;
        uint* ptr = &value;

        // Ldind_u4: Load uint32
        uint loaded = *ptr;
        if (loaded != 0xDEADBEEF) return 60;

        *ptr = 0xCAFEBABE;
        if (*ptr != 0xCAFEBABE) return 61;

        *ptr = uint.MaxValue;
        if (*ptr != uint.MaxValue) return 62;

        return 0;
    }

    // Test Ldind_i8/Stind_i8: Load/Store int64 indirect
    public static int TestIndirectInt64()
    {
        long value = -123456789012345L;
        long* ptr = &value;

        // Ldind_i8: Load int64
        long loaded = *ptr;
        if (loaded != -123456789012345L) return 70;

        // Stind_i8: Store int64
        *ptr = long.MaxValue;
        if (value != long.MaxValue) return 71;

        *ptr = long.MinValue;
        if (value != long.MinValue) return 72;

        // Array of longs
        long[] arr = new long[] { 1L, 1000000000000L, -1L };
        fixed (long* arrPtr = arr)
        {
            if (arrPtr[0] != 1L) return 73;
            if (arrPtr[1] != 1000000000000L) return 74;
            if (arrPtr[2] != -1L) return 75;

            arrPtr[1] = 999999999999L;
            if (arr[1] != 999999999999L) return 76;
        }

        return 0;
    }

    // Test Ldind_u8: Load uint64 indirect
    public static int TestIndirectUInt64()
    {
        ulong value = 0xDEADBEEFCAFEBABE;
        ulong* ptr = &value;

        // Ldind_u8: Load uint64
        ulong loaded = *ptr;
        if (loaded != 0xDEADBEEFCAFEBABE) return 80;

        *ptr = ulong.MaxValue;
        if (*ptr != ulong.MaxValue) return 81;

        *ptr = 0;
        if (*ptr != 0) return 82;

        return 0;
    }

    // Test Ldind_r4/Stind_r4: Load/Store float32 indirect
    public static int TestIndirectFloat32()
    {
        float value = 3.14159f;
        float* ptr = &value;

        // Ldind_r4: Load float32
        float loaded = *ptr;
        if (Math.Abs(loaded - 3.14159f) > 0.00001f) return 90;

        // Stind_r4: Store float32
        *ptr = -1.23456f;
        if (Math.Abs(value - (-1.23456f)) > 0.00001f) return 91;

        // Special values
        *ptr = float.PositiveInfinity;
        if (!float.IsPositiveInfinity(value)) return 92;

        *ptr = float.NaN;
        if (!float.IsNaN(value)) return 93;

        *ptr = 0.0f;
        if (value != 0.0f) return 94;

        return 0;
    }

    // Test Ldind_r8/Stind_r8: Load/Store float64 indirect
    public static int TestIndirectFloat64()
    {
        double value = 3.141592653589793;
        double* ptr = &value;

        // Ldind_r8: Load float64
        double loaded = *ptr;
        if (Math.Abs(loaded - 3.141592653589793) > 0.000000000001) return 100;

        // Stind_r8: Store float64
        *ptr = -2.718281828459045;
        if (Math.Abs(value - (-2.718281828459045)) > 0.000000000001) return 101;

        // Special values
        *ptr = double.PositiveInfinity;
        if (!double.IsPositiveInfinity(value)) return 102;

        *ptr = double.NaN;
        if (!double.IsNaN(value)) return 103;

        *ptr = double.Epsilon;
        if (value != double.Epsilon) return 104;

        return 0;
    }

    // Test Ldind_ref/Stind_ref: Load/Store object reference indirect
    public static int TestIndirectReference()
    {
        object obj1 = new object();
        object obj2 = "Hello";
        object obj3 = null;

        // Store references in array
        object[] arr = new object[] { obj1, obj2, obj3 };

        fixed (object* arrPtr = arr)
        {
            // Ldind_ref: Load reference
            object loaded = arrPtr[0];
            if (!ReferenceEquals(loaded, obj1)) return 110;

            loaded = arrPtr[1];
            if (!ReferenceEquals(loaded, obj2)) return 111;

            loaded = arrPtr[2];
            if (loaded != null) return 112;

            // Stind_ref: Store reference
            object newObj = new object();
            arrPtr[0] = newObj;
            if (!ReferenceEquals(arr[0], newObj)) return 113;

            arrPtr[2] = "World";
            if (arr[2] as string != "World") return 114;
        }

        return 0;
    }

    // Test mixed indirect operations
    public static int TestMixedIndirect()
    {
        // Structure with different types
        TestStruct s = new TestStruct
        {
            ByteField = 100,
            ShortField = -1000,
            IntField = 123456,
            LongField = 987654321098765L,
            FloatField = 1.5f,
            DoubleField = 2.5
        };

        TestStruct* ptr = &s;

        // Access through pointer
        if (ptr->ByteField != 100) return 120;
        if (ptr->ShortField != -1000) return 121;
        if (ptr->IntField != 123456) return 122;
        if (ptr->LongField != 987654321098765L) return 123;
        if (Math.Abs(ptr->FloatField - 1.5f) > 0.0001f) return 124;
        if (Math.Abs(ptr->DoubleField - 2.5) > 0.0001) return 125;

        // Modify through pointer
        ptr->IntField = 999;
        if (s.IntField != 999) return 126;

        return 0;
    }

    private struct TestStruct
    {
        public byte ByteField;
        public short ShortField;
        public int IntField;
        public long LongField;
        public float FloatField;
        public double DoubleField;
    }

    public static int Main(string[] argv)
    {
        int result;

        result = TestIndirectNativeInt();
        if (result != 0) return 5000 + result;

        result = TestIndirectInt8();
        if (result != 0) return 5100 + result;

        result = TestIndirectUInt8();
        if (result != 0) return 5200 + result;

        result = TestIndirectInt16();
        if (result != 0) return 5300 + result;

        result = TestIndirectUInt16();
        if (result != 0) return 5400 + result;

        result = TestIndirectInt32();
        if (result != 0) return 5500 + result;

        result = TestIndirectUInt32();
        if (result != 0) return 5600 + result;

        result = TestIndirectInt64();
        if (result != 0) return 5700 + result;

        result = TestIndirectUInt64();
        if (result != 0) return 5800 + result;

        result = TestIndirectFloat32();
        if (result != 0) return 5900 + result;

        result = TestIndirectFloat64();
        if (result != 0) return 6000 + result;

        result = TestIndirectReference();
        if (result != 0) return 6100 + result;

        result = TestMixedIndirect();
        if (result != 0) return 6200 + result;

        return 0;
    }
}
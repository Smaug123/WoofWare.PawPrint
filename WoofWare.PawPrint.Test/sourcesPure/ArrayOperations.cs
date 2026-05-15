using System;

public class TestArrayOperations
{
    // Test LdLen: Load array length
    public static int TestArrayLength()
    {
        // Single dimensional arrays
        int[] arr1 = new int[10];
        if (arr1.Length != 10) return 1;
        
        int[] arr2 = new int[0];
        if (arr2.Length != 0) return 2;
        
        int[] arr3 = new int[100];
        if (arr3.Length != 100) return 3;
        
        // Different types
        string[] strArr = new string[5];
        if (strArr.Length != 5) return 4;
        
        object[] objArr = new object[7];
        if (objArr.Length != 7) return 5;
        
        byte[] byteArr = new byte[256];
        if (byteArr.Length != 256) return 6;
        
        // Jagged arrays (length of outer array)
        int[][] jagged = new int[3][];
        if (jagged.Length != 3) return 7;
        
        return 0;
    }
    
    // Test Ldelem_i/Stelem_i: Load/Store native int element
    public static int TestArrayNativeInt()
    {
        IntPtr[] arr = new IntPtr[] { 
            new IntPtr(10), 
            new IntPtr(20), 
            new IntPtr(30) 
        };
        
        // Ldelem_i: Load native int element
        if (arr[0] != new IntPtr(10)) return 10;
        if (arr[1] != new IntPtr(20)) return 11;
        if (arr[2] != new IntPtr(30)) return 12;
        
        // Stelem_i: Store native int element
        arr[1] = new IntPtr(200);
        if (arr[1] != new IntPtr(200)) return 13;
        
        arr[0] = new IntPtr(-1);
        if (arr[0] != new IntPtr(-1)) return 14;
        
        return 0;
    }
    
    // Test Ldelem_i1/Stelem_i1: Load/Store int8 element
    public static int TestArrayInt8()
    {
        sbyte[] arr = new sbyte[] { -128, -1, 0, 1, 127 };
        
        // Ldelem_i1: Load int8 element
        if (arr[0] != -128) return 20;
        if (arr[1] != -1) return 21;
        if (arr[2] != 0) return 22;
        if (arr[3] != 1) return 23;
        if (arr[4] != 127) return 24;
        
        // Stelem_i1: Store int8 element
        arr[2] = 50;
        if (arr[2] != 50) return 25;
        
        arr[0] = 100;
        if (arr[0] != 100) return 26;
        
        return 0;
    }
    
    // Test Ldelem_u1/Stelem_u1: Load/Store uint8 element
    public static int TestArrayUInt8()
    {
        byte[] arr = new byte[] { 0, 1, 128, 255 };
        
        // Ldelem_u1: Load uint8 element
        if (arr[0] != 0) return 30;
        if (arr[1] != 1) return 31;
        if (arr[2] != 128) return 32;
        if (arr[3] != 255) return 33;
        
        // Stelem_u1: Store uint8 element (if implemented)
        arr[1] = 200;
        if (arr[1] != 200) return 34;
        
        arr[3] = 0;
        if (arr[3] != 0) return 35;
        
        return 0;
    }
    
    // Test Ldelem_i2/Stelem_i2: Load/Store int16 element
    public static int TestArrayInt16()
    {
        short[] arr = new short[] { -32768, -100, 0, 100, 32767 };
        
        // Ldelem_i2: Load int16 element
        if (arr[0] != -32768) return 40;
        if (arr[1] != -100) return 41;
        if (arr[2] != 0) return 42;
        if (arr[3] != 100) return 43;
        if (arr[4] != 32767) return 44;
        
        // Stelem_i2: Store int16 element
        arr[2] = 1000;
        if (arr[2] != 1000) return 45;
        
        arr[0] = -1000;
        if (arr[0] != -1000) return 46;
        
        return 0;
    }
    
    // Test Ldelem_u2/Stelem_u2: Load/Store uint16 element
    public static int TestArrayUInt16()
    {
        ushort[] arr = new ushort[] { 0, 100, 32768, 65535 };
        
        // Ldelem_u2: Load uint16 element
        if (arr[0] != 0) return 50;
        if (arr[1] != 100) return 51;
        if (arr[2] != 32768) return 52;
        if (arr[3] != 65535) return 53;
        
        // Stelem_u2: Store uint16 element (if implemented)
        arr[1] = 50000;
        if (arr[1] != 50000) return 54;
        
        arr[3] = 1;
        if (arr[3] != 1) return 55;
        
        return 0;
    }
    
    // Test Ldelem_i4/Stelem_i4: Load/Store int32 element
    public static int TestArrayInt32()
    {
        int[] arr = new int[] { int.MinValue, -1, 0, 1, int.MaxValue };
        
        // Ldelem_i4: Load int32 element
        if (arr[0] != int.MinValue) return 60;
        if (arr[1] != -1) return 61;
        if (arr[2] != 0) return 62;
        if (arr[3] != 1) return 63;
        if (arr[4] != int.MaxValue) return 64;
        
        // Stelem_i4: Store int32 element
        arr[2] = 123456;
        if (arr[2] != 123456) return 65;
        
        arr[0] = -987654;
        if (arr[0] != -987654) return 66;
        
        // Loop access
        for (int i = 0; i < arr.Length; i++)
        {
            arr[i] = i * 10;
        }
        
        for (int i = 0; i < arr.Length; i++)
        {
            if (arr[i] != i * 10) return 67 + i;
        }
        
        return 0;
    }
    
    // Test Ldelem_u4/Stelem_u4: Load/Store uint32 element
    public static int TestArrayUInt32()
    {
        uint[] arr = new uint[] { 0, 1, 0x80000000, 0xFFFFFFFF };
        
        // Ldelem_u4: Load uint32 element
        if (arr[0] != 0) return 80;
        if (arr[1] != 1) return 81;
        if (arr[2] != 0x80000000) return 82;
        if (arr[3] != 0xFFFFFFFF) return 83;
        
        // Stelem_u4: Store uint32 element (if implemented)
        arr[1] = 0xDEADBEEF;
        if (arr[1] != 0xDEADBEEF) return 84;
        
        arr[2] = 0xCAFEBABE;
        if (arr[2] != 0xCAFEBABE) return 85;
        
        return 0;
    }
    
    // Test Ldelem_i8/Stelem_i8: Load/Store int64 element
    public static int TestArrayInt64()
    {
        long[] arr = new long[] { long.MinValue, -1L, 0L, 1L, long.MaxValue };
        
        // Ldelem_i8: Load int64 element
        if (arr[0] != long.MinValue) return 90;
        if (arr[1] != -1L) return 91;
        if (arr[2] != 0L) return 92;
        if (arr[3] != 1L) return 93;
        if (arr[4] != long.MaxValue) return 94;
        
        // Stelem_i8: Store int64 element
        arr[2] = 123456789012345L;
        if (arr[2] != 123456789012345L) return 95;
        
        arr[0] = -987654321098765L;
        if (arr[0] != -987654321098765L) return 96;
        
        return 0;
    }
    
    // Test Ldelem_u8/Stelem_u8: Load/Store uint64 element
    public static int TestArrayUInt64()
    {
        ulong[] arr = new ulong[] { 0UL, 1UL, 0x8000000000000000UL, 0xFFFFFFFFFFFFFFFFUL };
        
        // Ldelem_u8: Load uint64 element
        if (arr[0] != 0UL) return 100;
        if (arr[1] != 1UL) return 101;
        if (arr[2] != 0x8000000000000000UL) return 102;
        if (arr[3] != 0xFFFFFFFFFFFFFFFFUL) return 103;
        
        // Stelem_u8: Store uint64 element (if implemented)
        arr[1] = 0xDEADBEEFCAFEBABEUL;
        if (arr[1] != 0xDEADBEEFCAFEBABEUL) return 104;
        
        return 0;
    }
    
    // Test Ldelem_r4/Stelem_r4: Load/Store float32 element
    public static int TestArrayFloat32()
    {
        float[] arr = new float[] { 0.0f, 1.0f, -1.0f, 3.14159f, float.NaN, float.PositiveInfinity };
        
        // Ldelem_r4: Load float32 element
        if (arr[0] != 0.0f) return 110;
        if (arr[1] != 1.0f) return 111;
        if (arr[2] != -1.0f) return 112;
        if (Math.Abs(arr[3] - 3.14159f) > 0.00001f) return 113;
        if (!float.IsNaN(arr[4])) return 114;
        if (!float.IsPositiveInfinity(arr[5])) return 115;
        
        // Stelem_r4: Store float32 element
        arr[0] = 2.71828f;
        if (Math.Abs(arr[0] - 2.71828f) > 0.00001f) return 116;
        
        arr[4] = float.NegativeInfinity;
        if (!float.IsNegativeInfinity(arr[4])) return 117;
        
        return 0;
    }
    
    // Test Ldelem_r8/Stelem_r8: Load/Store float64 element
    public static int TestArrayFloat64()
    {
        double[] arr = new double[] { 0.0, 1.0, -1.0, Math.PI, double.NaN, double.PositiveInfinity };
        
        // Ldelem_r8: Load float64 element
        if (arr[0] != 0.0) return 120;
        if (arr[1] != 1.0) return 121;
        if (arr[2] != -1.0) return 122;
        if (Math.Abs(arr[3] - Math.PI) > 0.000000000001) return 123;
        if (!double.IsNaN(arr[4])) return 124;
        if (!double.IsPositiveInfinity(arr[5])) return 125;
        
        // Stelem_r8: Store float64 element
        arr[0] = Math.E;
        if (Math.Abs(arr[0] - Math.E) > 0.000000000001) return 126;
        
        arr[4] = double.NegativeInfinity;
        if (!double.IsNegativeInfinity(arr[4])) return 127;
        
        return 0;
    }
    
    // Test Ldelem_ref/Stelem_ref: Load/Store object reference element
    public static int TestArrayReference()
    {
        object obj1 = new object();
        object obj2 = "Hello";
        object obj3 = 42;
        
        object[] arr = new object[] { obj1, obj2, obj3, null };
        
        // Ldelem_ref: Load reference element
        if (!ReferenceEquals(arr[0], obj1)) return 130;
        if (!ReferenceEquals(arr[1], obj2)) return 131;
        if (!ReferenceEquals(arr[2], obj3)) return 132;
        if (arr[3] != null) return 133;
        
        // Stelem_ref: Store reference element
        object newObj = new object();
        arr[0] = newObj;
        if (!ReferenceEquals(arr[0], newObj)) return 134;
        
        arr[3] = "World";
        if (arr[3] as string != "World") return 135;
        
        // Type compatibility
        string[] strArr = new string[3];
        strArr[0] = "Test";
        if (strArr[0] != "Test") return 136;
        
        return 0;
    }
    
    // Test boundary conditions
    public static int TestArrayBoundaries()
    {
        int[] arr = new int[5];
        
        // Fill array
        for (int i = 0; i < arr.Length; i++)
        {
            arr[i] = i * 100;
        }
        
        // Test first element
        if (arr[0] != 0) return 140;
        
        // Test last element
        if (arr[arr.Length - 1] != 400) return 141;
        
        // Test all elements
        for (int i = 0; i < arr.Length; i++)
        {
            if (arr[i] != i * 100) return 142 + i;
        }
        
        return 0;
    }
    
    // Test jagged arrays
    public static int TestJaggedArrays()
    {
        int[][] jagged = new int[3][];
        jagged[0] = new int[] { 1, 2 };
        jagged[1] = new int[] { 3, 4, 5 };
        jagged[2] = new int[] { 6 };
        
        // Test outer array
        if (jagged.Length != 3) return 180;
        
        // Test inner arrays
        if (jagged[0].Length != 2) return 181;
        if (jagged[1].Length != 3) return 182;
        if (jagged[2].Length != 1) return 183;
        
        // Test values
        if (jagged[0][0] != 1) return 184;
        if (jagged[0][1] != 2) return 185;
        if (jagged[1][0] != 3) return 186;
        if (jagged[1][1] != 4) return 187;
        if (jagged[1][2] != 5) return 188;
        if (jagged[2][0] != 6) return 189;
        
        // Modify values
        jagged[1][1] = 40;
        if (jagged[1][1] != 40) return 190;
        
        return 0;
    }
    
    public static int Main(string[] argv)
    {
        int result;
        
        result = TestArrayLength();
        if (result != 0) return 7000 + result;
        
        result = TestArrayNativeInt();
        if (result != 0) return 7100 + result;
        
        result = TestArrayInt8();
        if (result != 0) return 7200 + result;
        
        result = TestArrayUInt8();
        if (result != 0) return 7300 + result;
        
        result = TestArrayInt16();
        if (result != 0) return 7400 + result;
        
        result = TestArrayUInt16();
        if (result != 0) return 7500 + result;
        
        result = TestArrayInt32();
        if (result != 0) return 7600 + result;
        
        result = TestArrayUInt32();
        if (result != 0) return 7700 + result;
        
        result = TestArrayInt64();
        if (result != 0) return 7800 + result;
        
        result = TestArrayUInt64();
        if (result != 0) return 7900 + result;
        
        result = TestArrayFloat32();
        if (result != 0) return 8000 + result;
        
        result = TestArrayFloat64();
        if (result != 0) return 8100 + result;
        
        result = TestArrayReference();
        if (result != 0) return 8200 + result;
        
        result = TestArrayBoundaries();
        if (result != 0) return 8300 + result;

        result = TestJaggedArrays();
        if (result != 0) return 8500 + result;

        return 0;
    }
}
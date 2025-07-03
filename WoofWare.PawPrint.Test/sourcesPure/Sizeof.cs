using System;
using System.Runtime.InteropServices;

public class Program
{
    public struct SmallStruct
    {
        public byte Value;
    }

    public struct MediumStruct
    {
        public int Value1;
        public int Value2;
    }

    public struct LargeStruct
    {
        public long Value1;
        public long Value2;
        public long Value3;
        public long Value4;
    }

    public struct NestedStruct
    {
        public SmallStruct Small;
        public MediumStruct Medium;
        public int Extra;
    }

    [StructLayout(LayoutKind.Explicit)]
    public struct UnionStruct
    {
        [FieldOffset(0)]
        public int AsInt;
        [FieldOffset(0)]
        public float AsFloat;
    }

    public static int Main(string[] args)
    {
        // Test 1: Basic primitive types
        if (sizeof(byte) != 1) return 1;
        if (sizeof(sbyte) != 1) return 2;
        if (sizeof(short) != 2) return 3;
        if (sizeof(ushort) != 2) return 4;
        if (sizeof(int) != 4) return 5;
        if (sizeof(uint) != 4) return 6;
        if (sizeof(long) != 8) return 7;
        if (sizeof(ulong) != 8) return 8;
        if (sizeof(float) != 4) return 9;
        if (sizeof(double) != 8) return 10;
        if (sizeof(char) != 2) return 11;
        if (sizeof(bool) != 1) return 12;

        // Test 2: Struct sizes
        if (sizeof(SmallStruct) != 1) return 13;
        if (sizeof(MediumStruct) != 8) return 14;
        if (sizeof(LargeStruct) != 32) return 15;

        // Test 3: Nested struct size
        // SmallStruct (1) + padding (3) + MediumStruct (8) + int (4) = 16
        if (sizeof(NestedStruct) != 16) return 16;

        // Test 4: Union struct size
        if (sizeof(UnionStruct) != 4) return 17;

        // Test 5: Enum size (underlying type is int)
        if (sizeof(DayOfWeek) != 4) return 18;

        // Test 6: Pointer types
        unsafe
        {
            if (sizeof(IntPtr) != sizeof(void*)) return 19;
            if (sizeof(UIntPtr) != sizeof(void*)) return 20;
        }

        // Test 7: Using sizeof in expressions
        int totalSize = sizeof(int) + sizeof(long) + sizeof(byte);
        if (totalSize != 13) return 21;

        // Test 8: Array element size calculation
        int arrayElementSize = sizeof(MediumStruct);
        int arraySize = arrayElementSize * 3;
        if (arraySize != 24) return 22;

        // Test 9: Conditional using sizeof
        bool is32Bit = sizeof(IntPtr) == 4;
        bool is64Bit = sizeof(IntPtr) == 8;
        if (!is32Bit && !is64Bit) return 23;
        if (is32Bit && is64Bit) return 24;

        // Test 10: Sizeof in switch statement
        int result = 0;
        switch (sizeof(int))
        {
            case 1:
                result = 1;
                break;
            case 2:
                result = 2;
                break;
            case 4:
                result = 4;
                break;
            case 8:
                result = 8;
                break;
            default:
                result = -1;
                break;
        }
        if (result != 4) return 25;

        return 0;
    }
}
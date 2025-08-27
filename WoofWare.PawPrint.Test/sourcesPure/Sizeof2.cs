using System;
using System.Runtime.InteropServices;

unsafe public class Program
{
    // Test for empty struct (should be 1 byte, not 0)
    public struct EmptyStruct
    {
    }

    // Test for char alignment (should align to 2, not 1)
    public struct CharStruct
    {
        public byte B;
        public char C;  // Should be at offset 2, not 1
    }

    // Test for end padding
    public struct NeedsEndPadding
    {
        public int X;
        public byte Y;
        // Should pad to 8 bytes total (multiple of 4)
    }

    // Test Pack=1 (no padding)
    [StructLayout(LayoutKind.Sequential, Pack = 1)]
    public struct PackedStruct
    {
        public byte B;
        public int I;   // At offset 1, not 4
        public byte B2;
        // Total 6 bytes, no padding
    }

    // Test Pack=2
    [StructLayout(LayoutKind.Sequential, Pack = 2)]
    public struct Pack2Struct
    {
        public byte B;
        public int I;   // At offset 2 (2-byte aligned, not 4)
        public byte B2;
        // Should pad to 8 bytes (multiple of 2)
    }

    // Test custom size smaller than natural size
    [StructLayout(LayoutKind.Sequential, Size = 12)]
    public struct CustomSizeSmaller
    {
        public long L1;
        public long L2;
        // Natural size is 16, but Size=12 is ignored (12 < 16)
    }

    // Test custom size larger than natural size
    [StructLayout(LayoutKind.Sequential, Size = 20)]
    public struct CustomSizeLarger
    {
        public long L;
        // Natural size is 8, custom size 20 should win
    }

    // Test custom size not multiple of alignment
    [StructLayout(LayoutKind.Sequential, Size = 15)]
    public struct CustomSizeOdd
    {
        public long L;
        // Size=15 should be honored even though not multiple of 8
    }

    // Test Pack=0 (means default, not 0)
    [StructLayout(LayoutKind.Sequential, Pack = 0)]
    public struct Pack0Struct
    {
        public byte B;
        public int I;  // Should be at offset 4 (default packing)
    }

    // Test both Pack and Size
    [StructLayout(LayoutKind.Sequential, Pack = 1, Size = 10)]
    public struct PackAndSize
    {
        public byte B;
        public int I;
        // Natural packed size is 5, custom size 10 should win
    }

    // Test explicit with custom Size
    [StructLayout(LayoutKind.Explicit, Size = 10)]
    public struct ExplicitWithSize
    {
        [FieldOffset(0)]
        public int I;
        [FieldOffset(2)]
        public short S;
        // Max offset+size is 4, but Size=10 should win
    }

    public struct SmallStruct
    {
        public byte Value;
    }

    public struct MediumStruct
    {
        public int MediumValue1;
        public int MediumValue2;
    }

    public struct LargeStruct
    {
        public long LongValue1;
        public long LongValue2;
        public long LongValue3;
        public long LongValue4;
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

        // Test 6: Empty struct (should be 1, not 0)
        if (sizeof(EmptyStruct) != 1) return 19;

        // Test 7: Char alignment
        // byte (1) + padding (1) + char (2) = 4
        if (sizeof(CharStruct) != 4) return 20;

        // Test 8: End padding
        // int (4) + byte (1) + padding (3) = 8
        if (sizeof(NeedsEndPadding) != 8) return 21;

        // Test 9: Pack=1 removes all padding
        // byte (1) + int (4) + byte (1) = 6
        if (sizeof(PackedStruct) != 6) return 22;

        // Test 10: Pack=2
        // byte (1) + padding (1) + int (4) + byte (1) + padding (1) = 8
        if (sizeof(Pack2Struct) != 8) return 23;

        // Test 11: Custom size smaller than natural (ignored)
        if (sizeof(CustomSizeSmaller) != 16) return 24;

        // Test 12: Custom size larger than natural (honored)
        if (sizeof(CustomSizeLarger) != 20) return 25;

        // Test 13: Custom size not multiple of alignment (honored)
        if (sizeof(CustomSizeOdd) != 15) return 26;

        // Test 14: Pack=0 means default packing
        // byte (1) + padding (3) + int (4) = 8
        if (sizeof(Pack0Struct) != 8) return 27;

        // Test 15: Pack and Size together
        // Natural packed: byte (1) + int (4) = 5, but Size=10
        if (sizeof(PackAndSize) != 10) return 28;

        // Test 16: Explicit with Size
        // Max used is 4, but Size=10
        if (sizeof(ExplicitWithSize) != 10) return 29;

        // Test 17: Pointer types
        unsafe
        {
            if (sizeof(IntPtr) != sizeof(void*)) return 30;
            if (sizeof(UIntPtr) != sizeof(void*)) return 31;
        }

        // Test 18: Using sizeof in expressions
        int totalSize = sizeof(int) + sizeof(long) + sizeof(byte);
        if (totalSize != 13) return 32;

        // Test 19: Array element size calculation
        int arrayElementSize = sizeof(MediumStruct);
        int arraySize = arrayElementSize * 3;
        if (arraySize != 24) return 33;

        // Test 20: Complex nested struct with Pack
        // byte (1) + CharStruct (4) + byte (1) = 6
        if (sizeof(PackedNested) != 6) return 34;

        return 0;
    }

    [StructLayout(LayoutKind.Sequential, Pack = 1)]
    struct PackedNested
    {
        public byte B;
        public CharStruct C;
        public byte B2;
    }
}

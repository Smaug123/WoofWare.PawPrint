using System;
using System.Runtime.InteropServices;

public class StructLayoutTests
{
    // Test structs with various layouts

    [StructLayout(LayoutKind.Sequential)]
    struct SequentialStruct
    {
        public int A;
        public byte B;
        public long C;
    }

    [StructLayout(LayoutKind.Explicit)]
    struct ExplicitUnion
    {
        [FieldOffset(0)] public int AsInt;
        [FieldOffset(0)] public float AsFloat;
        [FieldOffset(0)] public byte Byte0;
        [FieldOffset(1)] public byte Byte1;
        [FieldOffset(2)] public byte Byte2;
        [FieldOffset(3)] public byte Byte3;
    }

    [StructLayout(LayoutKind.Explicit, Size = 16)]
    struct FixedSizeStruct
    {
        [FieldOffset(0)] public long First;
        [FieldOffset(8)] public int Second;
        [FieldOffset(12)] public short Third;
    }

    [StructLayout(LayoutKind.Sequential, Pack = 1)]
    struct PackedStruct
    {
        public byte A;
        public int B;
        public byte C;
    }

    [StructLayout(LayoutKind.Auto)]
    struct AutoLayoutStruct
    {
        public int X;
        public string Y;
        public double Z;
    }

    [StructLayout(LayoutKind.Explicit)]
    struct NestedUnion
    {
        [FieldOffset(0)] public ExplicitUnion Inner;
        [FieldOffset(0)] public long AsLong;
        [FieldOffset(4)] public int UpperInt;
    }

    [StructLayout(LayoutKind.Explicit)]
    struct LargeUnion
    {
        [FieldOffset(0)] public long Long1;
        [FieldOffset(8)] public long Long2;
        [FieldOffset(0)] public double Double1;
        [FieldOffset(8)] public double Double2;
        [FieldOffset(0)] public decimal AsDecimal;
    }

    // Static fields for testing
    static SequentialStruct staticSequential;
    static ExplicitUnion staticUnion;
    static FixedSizeStruct staticFixed;

    // Instance fields for testing
    class FieldContainer
    {
        public SequentialStruct instanceSequential;
        public ExplicitUnion instanceUnion;
        public PackedStruct instancePacked;
        public NestedUnion instanceNested;
    }

    static int TestSequentialLayout()
    {
        var s = new SequentialStruct { A = 42, B = 255, C = long.MaxValue };

        // Test field access
        if (s.A != 42) return 1;
        if (s.B != 255) return 2;
        if (s.C != long.MaxValue) return 3;

        // Test copy semantics
        var s2 = s;
        s2.A = 100;
        if (s.A != 42) return 4; // Should be unchanged (value type)
        if (s2.A != 100) return 5;

        // Test static field storage
        staticSequential = s;
        if (staticSequential.A != 42) return 6;
        if (staticSequential.C != long.MaxValue) return 7;

        return 0;
    }

    static int TestExplicitUnion()
    {
        var u = new ExplicitUnion();

        // Test overlapping int/float
        u.AsInt = 0x3F800000; // IEEE 754 representation of 1.0f
        if (Math.Abs(u.AsFloat - 1.0f) > 0.0001f) return 10;

        // Test byte-level access
        u.AsInt = 0x12345678;
        bool isLittleEndian = BitConverter.IsLittleEndian;
        if (isLittleEndian)
        {
            if (u.Byte0 != 0x78) return 11;
            if (u.Byte1 != 0x56) return 12;
            if (u.Byte2 != 0x34) return 13;
            if (u.Byte3 != 0x12) return 14;
        }
        else
        {
            if (u.Byte0 != 0x12) return 11;
            if (u.Byte1 != 0x34) return 12;
            if (u.Byte2 != 0x56) return 13;
            if (u.Byte3 != 0x78) return 14;
        }

        // Test static field
        staticUnion = u;
        if (staticUnion.AsInt != 0x12345678) return 15;

        return 0;
    }

    static int TestFixedSizeStruct()
    {
        var f = new FixedSizeStruct { First = -1, Second = 42, Third = 1000 };

        if (f.First != -1) return 20;
        if (f.Second != 42) return 21;
        if (f.Third != 1000) return 22;

        // Test size is respected
        int size = Marshal.SizeOf(typeof(FixedSizeStruct));
        if (size != 16) return 23;

        staticFixed = f;
        if (staticFixed.Second != 42) return 24;

        return 0;
    }

    static int TestPackedStruct()
    {
        var p = new PackedStruct { A = 1, B = 0x12345678, C = 2 };

        if (p.A != 1) return 30;
        if (p.B != 0x12345678) return 31;
        if (p.C != 2) return 32;

        // Packed struct should be 6 bytes (1 + 4 + 1)
        int size = Marshal.SizeOf(typeof(PackedStruct));
        if (size != 6) return 33;

        return 0;
    }

    static int TestInstanceFields()
    {
        var container = new FieldContainer();

        container.instanceSequential = new SequentialStruct { A = 111, B = 222, C = 333 };
        if (container.instanceSequential.A != 111) return 40;

        container.instanceUnion = new ExplicitUnion { AsInt = unchecked((int)0xDEADBEEF) };
        if (container.instanceUnion.AsInt != unchecked((int)0xDEADBEEF)) return 41;

        container.instancePacked = new PackedStruct { A = 10, B = 20, C = 30 };
        if (container.instancePacked.B != 20) return 42;

        container.instanceNested = new NestedUnion();
        container.instanceNested.Inner.AsInt = 100;
        if (container.instanceNested.Inner.AsInt != 100) return 43;

        return 0;
    }

    static int TestStructPassing()
    {
        var s = new SequentialStruct { A = 500, B = 50, C = 5005 };
        int result = ProcessSequential(s);
        if (result != 555) return 50; // 500 + 50 + 5 (C % 1000)

        var u = new ExplicitUnion { AsInt = 1000 };
        u = TransformUnion(u);
        if (u.AsInt != 2000) return 51;

        return 0;
    }

    static int ProcessSequential(SequentialStruct s)
    {
        return s.A + s.B + (int)(s.C % 1000);
    }

    static ExplicitUnion TransformUnion(ExplicitUnion u)
    {
        u.AsInt *= 2;
        return u;
    }

    static int TestNestedUnion()
    {
        var n = new NestedUnion();
        n.Inner.AsInt = 0x12345678;

        // Lower 32 bits should match Inner.AsInt
        if ((n.AsLong & 0xFFFFFFFF) != 0x12345678) return 60;

        // Modify upper int
        n.UpperInt = unchecked((int)0xABCDEF00);

        // Check both parts
        if (n.Inner.AsInt != 0x12345678) return 61;
        if (n.UpperInt != unchecked((int)0xABCDEF00)) return 62;

        return 0;
    }

    static int TestLargeUnion()
    {
        var l = new LargeUnion();

        // Test double/long overlap
        l.Double1 = 1.0;
        l.Double2 = 2.0;

        // IEEE 754: 1.0 = 0x3FF0000000000000
        if (l.Long1 != 0x3FF0000000000000) return 70;
        // IEEE 754: 2.0 = 0x4000000000000000
        if (l.Long2 != 0x4000000000000000) return 71;

        // Test decimal overlap (decimal is 128 bits)
        l.AsDecimal = 42m;
        // Just verify it doesn't crash and maintains some structure
        if (l.AsDecimal != 42m) return 72;

        return 0;
    }

    static int TestAutoLayout()
    {
        // Auto layout structs can't use FieldOffset, but we can still test basic functionality
        var a = new AutoLayoutStruct { X = 100, Y = "test", Z = 3.14159 };

        if (a.X != 100) return 80;
        if (a.Y != "test") return 81;
        if (Math.Abs(a.Z - 3.14159) > 0.00001) return 82;

        // Test copy
        var a2 = a;
        a2.X = 200;
        if (a.X != 100) return 83; // Original should be unchanged
        if (a2.X != 200) return 84;

        return 0;
    }

    static int TestStructArray()
    {
        var arr = new ExplicitUnion[3];
        arr[0].AsInt = 10;
        arr[1].AsInt = 20;
        arr[2].AsInt = 30;

        if (arr[0].AsInt != 10) return 90;
        if (arr[1].AsInt != 20) return 91;
        if (arr[2].AsInt != 30) return 92;

        // Modify through float view
        arr[1].AsFloat = 2.5f;
        if (Math.Abs(arr[1].AsFloat - 2.5f) > 0.0001f) return 93;

        return 0;
    }

    static int TestBoxingUnboxing()
    {
        ExplicitUnion u = new ExplicitUnion { AsInt = 999 };
        object boxed = u; // Box
        ExplicitUnion unboxed = (ExplicitUnion)boxed; // Unbox

        if (unboxed.AsInt != 999) return 100;

        // Modify original, boxed should remain unchanged
        u.AsInt = 111;
        ExplicitUnion fromBoxed = (ExplicitUnion)boxed;
        if (fromBoxed.AsInt != 999) return 101; // Should still be 999

        return 0;
    }

    static int TestDefaultValues()
    {
        // Test that default struct initialization zeroes memory
        var s = new SequentialStruct();
        if (s.A != 0) return 110;
        if (s.B != 0) return 111;
        if (s.C != 0) return 112;

        var u = new ExplicitUnion();
        if (u.AsInt != 0) return 113;
        if (u.AsFloat != 0.0f) return 114;

        return 0;
    }

    public static int Main(string[] argv)
    {
        int result = 0;

        result = TestSequentialLayout();
        if (result != 0) return result;

        result = TestExplicitUnion();
        if (result != 0) return result;

        result = TestFixedSizeStruct();
        if (result != 0) return result;

        result = TestPackedStruct();
        if (result != 0) return result;

        result = TestInstanceFields();
        if (result != 0) return result;

        result = TestStructPassing();
        if (result != 0) return result;

        result = TestNestedUnion();
        if (result != 0) return result;

        result = TestLargeUnion();
        if (result != 0) return result;

        result = TestAutoLayout();
        if (result != 0) return result;

        result = TestStructArray();
        if (result != 0) return result;

        result = TestBoxingUnboxing();
        if (result != 0) return result;

        result = TestDefaultValues();
        if (result != 0) return result;

        return 0; // All tests passed
    }
}

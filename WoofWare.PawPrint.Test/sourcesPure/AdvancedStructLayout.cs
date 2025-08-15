using System;
using System.Runtime.InteropServices;
using System.Runtime.CompilerServices;

// Compile with: csc /unsafe StructLayoutTestsAdvanced.cs
public class StructLayoutTestsAdvanced
{
    // Test structs

    [StructLayout(LayoutKind.Sequential)]
    struct PointerTestStruct
    {
        public int A;
        public byte B;
        public short C;
        public int D;
    }

    [StructLayout(LayoutKind.Sequential)]
    unsafe struct FixedBufferStruct
    {
        public int Header;
        public fixed byte Buffer[64];
        public int Footer;
    }

    [StructLayout(LayoutKind.Sequential)]
    unsafe struct NestedFixedStruct
    {
        public fixed int IntArray[4];
        public fixed double DoubleArray[2];
    }

    [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Ansi)]
    struct MarshalStringStruct
    {
        public int Id;
        [MarshalAs(UnmanagedType.ByValTStr, SizeConst = 32)]
        public string Name;
        public double Value;
    }

    [StructLayout(LayoutKind.Sequential)]
    struct MarshalArrayStruct
    {
        public int Count;
        [MarshalAs(UnmanagedType.ByValArray, SizeConst = 8)]
        public int[] Values;
    }

    [StructLayout(LayoutKind.Sequential)]
    struct BlittableStruct
    {
        public int X;
        public double Y;
        public long Z;
    }

    ref struct RefStruct
    {
        public int Value;
        public Span<int> Span;

        public RefStruct(int value)
        {
            Value = value;
            Span = new Span<int>(new int[] { value, value * 2, value * 3 });
        }
    }

    readonly struct ReadOnlyStruct
    {
        public readonly int X;
        public readonly int Y;

        public ReadOnlyStruct(int x, int y)
        {
            X = x;
            Y = y;
        }

        public int Sum => X + Y;
    }

    readonly ref struct ReadOnlyRefStruct
    {
        public readonly int Value;
        public readonly ReadOnlySpan<byte> Data;

        public ReadOnlyRefStruct(int value, ReadOnlySpan<byte> data)
        {
            Value = value;
            Data = data;
        }
    }

    struct Generic<T> where T : struct
    {
        public T Value;
        public int Index;

        public Generic(T value, int index)
        {
            Value = value;
            Index = index;
        }
    }

    struct DoubleGeneric<T, U>
    {
        public T First;
        public U Second;
    }

    interface IIndexable
    {
        int GetIndex();
        void SetIndex(int value);
    }

    struct StructWithInterface : IIndexable
    {
        public int Index;
        public string Data;

        public int GetIndex() => Index;
        public void SetIndex(int value) => Index = value;
    }

    interface IMutable
    {
        void Mutate();
    }

    struct MutableStruct : IMutable
    {
        public int Counter;

        public void Mutate()
        {
            Counter++;
        }
    }

    struct RefReturnStruct
    {
        public int A;
        public int B;
        public int C;

        public ref int GetRef(int index)
        {
            if (index == 0) return ref A;
            if (index == 1) return ref B;
            return ref C;
        }
    }

    static unsafe int TestUnsafePointers()
    {
        var s = new PointerTestStruct { A = 0x12345678, B = 0xAB, C = 0x1234, D = 0xDEADBEEF };

        // Test sizeof
        int size = sizeof(PointerTestStruct);
        if (size == 0) return 1;

        // Test pointer access
        PointerTestStruct* ptr = &s;
        if (ptr->A != 0x12345678) return 2;
        if (ptr->B != 0xAB) return 3;
        if (ptr->C != 0x1234) return 4;
        if (ptr->D != unchecked((int)0xDEADBEEF)) return 5;

        // Test pointer arithmetic and casting
        byte* bytePtr = (byte*)ptr;
        int* intPtr = (int*)bytePtr;
        if (*intPtr != 0x12345678) return 6; // First int field

        // Verify field offsets
        int* dPtr = &(ptr->D);
        int* aPtr = &(ptr->A);
        long ptrDiff = (byte*)dPtr - (byte*)aPtr;
        if (ptrDiff < 8) return 7; // D should be at least 8 bytes from A

        // Test modification through pointer
        ptr->A = 999;
        if (s.A != 999) return 8;

        return 0;
    }

    static unsafe int TestFixedBuffers()
    {
        var f = new FixedBufferStruct();
        f.Header = 0xFEED;
        f.Footer = 0xBEEF;

        // Test fixed buffer access
        for (int i = 0; i < 64; i++)
        {
            f.Buffer[i] = (byte)(i % 256);
        }

        if (f.Header != 0xFEED) return 10;
        if (f.Footer != 0xBEEF) return 11;

        // Verify buffer contents
        for (int i = 0; i < 64; i++)
        {
            if (f.Buffer[i] != (byte)(i % 256)) return 12;
        }

        // Test pointer to fixed buffer
        byte* bufPtr = f.Buffer;
        bufPtr[0] = 255;
        if (f.Buffer[0] != 255) return 13;

        // Test nested fixed arrays
        var n = new NestedFixedStruct();
        n.IntArray[0] = 100;
        n.IntArray[3] = 400;
        n.DoubleArray[0] = 1.5;
        n.DoubleArray[1] = 2.5;

        if (n.IntArray[0] != 100) return 14;
        if (n.IntArray[3] != 400) return 15;
        if (Math.Abs(n.DoubleArray[0] - 1.5) > 0.0001) return 16;
        if (Math.Abs(n.DoubleArray[1] - 2.5) > 0.0001) return 17;

        return 0;
    }

    static unsafe int TestMarshaling()
    {
        // Test string marshaling
        var ms = new MarshalStringStruct
        {
            Id = 42,
            Name = "TestString",
            Value = 3.14159
        };

        if (ms.Id != 42) return 20;
        if (ms.Name != "TestString") return 21;
        if (Math.Abs(ms.Value - 3.14159) > 0.00001) return 22;

        // Test Marshal.SizeOf
        int marshalSize = Marshal.SizeOf(typeof(MarshalStringStruct));
        if (marshalSize == 0) return 23;

        // Test array marshaling
        var ma = new MarshalArrayStruct
        {
            Count = 5,
            Values = new int[] { 1, 2, 3, 4, 5, 6, 7, 8 }
        };

        if (ma.Count != 5) return 24;
        if (ma.Values.Length != 8) return 25;
        if (ma.Values[7] != 8) return 26;

        // Test StructureToPtr and PtrToStructure
        var blittable = new BlittableStruct { X = 100, Y = 200.5, Z = 300 };
        IntPtr ptr = Marshal.AllocHGlobal(Marshal.SizeOf(typeof(BlittableStruct)));
        try
        {
            Marshal.StructureToPtr(blittable, ptr, false);
            var recovered = (BlittableStruct)Marshal.PtrToStructure(ptr, typeof(BlittableStruct));

            if (recovered.X != 100) return 27;
            if (Math.Abs(recovered.Y - 200.5) > 0.00001) return 28;
            if (recovered.Z != 300) return 29;
        }
        finally
        {
            Marshal.FreeHGlobal(ptr);
        }

        return 0;
    }

    static int TestRefStructs()
    {
        // Test ref struct
        var rs = new RefStruct(10);
        if (rs.Value != 10) return 30;
        if (rs.Span.Length != 3) return 31;
        if (rs.Span[0] != 10) return 32;
        if (rs.Span[1] != 20) return 33;
        if (rs.Span[2] != 30) return 34;

        // Modify through span
        rs.Span[0] = 100;
        if (rs.Span[0] != 100) return 35;

        // Test readonly struct
        var ros = new ReadOnlyStruct(5, 7);
        if (ros.X != 5) return 36;
        if (ros.Y != 7) return 37;
        if (ros.Sum != 12) return 38;

        // Verify immutability - create new instance
        var ros2 = new ReadOnlyStruct(10, 20);
        if (ros.X != 5) return 39; // Original should be unchanged

        // Test readonly ref struct
        byte[] data = { 1, 2, 3, 4 };
        var rors = new ReadOnlyRefStruct(42, new ReadOnlySpan<byte>(data));
        if (rors.Value != 42) return 40;
        if (rors.Data.Length != 4) return 41;
        if (rors.Data[3] != 4) return 42;

        return 0;
    }

    static int TestGenerics()
    {
        // Test single generic parameter
        var g1 = new Generic<int>(42, 1);
        if (g1.Value != 42) return 50;
        if (g1.Index != 1) return 51;

        var g2 = new Generic<double>(3.14, 2);
        if (Math.Abs(g2.Value - 3.14) > 0.00001) return 52;
        if (g2.Index != 2) return 53;

        // Test with custom struct
        var inner = new ReadOnlyStruct(10, 20);
        var g3 = new Generic<ReadOnlyStruct>(inner, 3);
        if (g3.Value.X != 10) return 54;
        if (g3.Value.Y != 20) return 55;
        if (g3.Index != 3) return 56;

        // Test double generic
        var dg = new DoubleGeneric<int, string> { First = 100, Second = "test" };
        if (dg.First != 100) return 57;
        if (dg.Second != "test") return 58;

        // Test with different type combinations
        var dg2 = new DoubleGeneric<double, long> { First = 2.718, Second = long.MaxValue };
        if (Math.Abs(dg2.First - 2.718) > 0.00001) return 59;
        if (dg2.Second != long.MaxValue) return 60;

        return 0;
    }

    static int TestByRefReturns()
    {
        var r = new RefReturnStruct { A = 10, B = 20, C = 30 };

        // Test ref return
        ref int refA = ref r.GetRef(0);
        if (refA != 10) return 70;

        // Modify through ref
        refA = 100;
        if (r.A != 100) return 71;

        ref int refB = ref r.GetRef(1);
        refB = 200;
        if (r.B != 200) return 72;

        ref int refC = ref r.GetRef(2);
        refC = 300;
        if (r.C != 300) return 73;

        // Test ref local
        ref int localRef = ref r.A;
        localRef = 1000;
        if (r.A != 1000) return 74;

        // Test that ref points to actual field
        localRef = 2000;
        if (refA != 2000) return 75; // Both should see the change

        return 0;
    }

    static int TestStructInterfaces()
    {
        // Test struct implementing interface
        var s = new StructWithInterface { Index = 42, Data = "test" };
        if (s.GetIndex() != 42) return 80;

        s.SetIndex(100);
        if (s.Index != 100) return 81;

        // Test boxing to interface
        IIndexable boxed = s; // Boxing occurs here
        if (boxed.GetIndex() != 100) return 82;

        // Modify through interface (modifies boxed copy)
        boxed.SetIndex(200);
        if (boxed.GetIndex() != 200) return 83;
        if (s.Index != 100) return 84; // Original should be unchanged

        // Test mutable interface
        var m = new MutableStruct { Counter = 0 };
        m.Mutate();
        if (m.Counter != 1) return 85;

        // Box to interface and mutate
        IMutable boxedMutable = m; // Boxing
        boxedMutable.Mutate();
        if (m.Counter != 1) return 86; // Original unchanged

        // Cast back to see boxed mutation
        var unboxed = (MutableStruct)boxedMutable;
        if (unboxed.Counter != 2) return 87;

        // Direct interface call on boxed struct maintains state
        boxedMutable.Mutate();
        boxedMutable.Mutate();
        var unboxed2 = (MutableStruct)boxedMutable;
        if (unboxed2.Counter != 4) return 88;

        return 0;
    }

    static unsafe int TestCombinedScenarios()
    {
        // Test generic with fixed buffer struct
        var f = new FixedBufferStruct();
        f.Header = 999;
        f.Buffer[0] = 123;
        f.Footer = 111;

        var generic = new Generic<FixedBufferStruct>(f, 42);
        if (generic.Value.Header != 999) return 90;
        if (generic.Value.Buffer[0] != 123) return 91;
        if (generic.Value.Footer != 111) return 92;
        if (generic.Index != 42) return 93;

        // Test marshaling with generic
        var marshalable = new BlittableStruct { X = 10, Y = 20.0, Z = 30 };
        var genericMarshal = new Generic<BlittableStruct>(marshalable, 5);

        if (genericMarshal.Value.X != 10) return 94;
        if (Math.Abs(genericMarshal.Value.Y - 20.0) > 0.00001) return 95;
        if (genericMarshal.Value.Z != 30) return 96;

        return 0;
    }

    public static int Main()
    {
        int result = 0;

        unsafe
        {
            result = TestUnsafePointers();
            if (result != 0) return result;

            result = TestFixedBuffers();
            if (result != 0) return result;
        }

        result = TestMarshaling();
        if (result != 0) return result;

        result = TestRefStructs();
        if (result != 0) return result;

        result = TestGenerics();
        if (result != 0) return result;

        result = TestByRefReturns();
        if (result != 0) return result;

        result = TestStructInterfaces();
        if (result != 0) return result;

        unsafe
        {
            result = TestCombinedScenarios();
            if (result != 0) return result;
        }

        return 0; // All tests passed
    }
}

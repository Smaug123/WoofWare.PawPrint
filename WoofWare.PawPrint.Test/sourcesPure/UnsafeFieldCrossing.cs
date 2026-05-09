using System.Runtime.CompilerServices;

// Mirrors the BCL pattern in Guid.GetHashCode / Guid.EqualsCore: a managed
// pointer to a struct field is advanced by `Unsafe.Add` past the field
// boundary into a sibling field of the same parent struct. Reads and writes
// through the resulting byref must reach the sibling field's storage.
public class TestUnsafeFieldCrossing
{
    private struct TwoInts
    {
        public int A;
        public int B;
    }

    private struct ThreeInts
    {
        public int A;
        public int B;
        public int C;
    }

    // Forward read: `Unsafe.Add(ref s.A, 1)` lands at the start of `s.B`.
    public static int Test1()
    {
        TwoInts s = new TwoInts { A = 0x11111111, B = 0x22222222 };
        ref int rA = ref s.A;
        int viaSibling = Unsafe.Add(ref rA, 1);
        if (viaSibling != 0x22222222)
            return 1;
        return 0;
    }

    // Forward write: assigning through `Unsafe.Add(ref s.A, 1)` updates `s.B`
    // and leaves `s.A` untouched.
    public static int Test2()
    {
        TwoInts s = new TwoInts { A = 0x11111111, B = 0x22222222 };
        ref int rA = ref s.A;
        Unsafe.Add(ref rA, 1) = 0x77777777;
        if (s.A != 0x11111111)
            return 2;
        if (s.B != 0x77777777)
            return 3;
        return 0;
    }

    // Negative offset: `Unsafe.Add(ref s.B, -1)` lands at the start of `s.A`.
    public static int Test3()
    {
        TwoInts s = new TwoInts { A = 0x33333333, B = 0x44444444 };
        ref int rB = ref s.B;
        int viaSibling = Unsafe.Add(ref rB, -1);
        if (viaSibling != 0x33333333)
            return 4;
        return 0;
    }

    // Multi-step forward walk across two field boundaries reads `s.C`.
    public static int Test4()
    {
        ThreeInts s = new ThreeInts { A = 1, B = 2, C = 3 };
        ref int rA = ref s.A;
        int viaC = Unsafe.Add(ref rA, 2);
        if (viaC != 3)
            return 5;
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
        return 0;
    }
}

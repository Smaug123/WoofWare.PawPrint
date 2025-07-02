public class TestShr
{
    public static int Main(string[] args)
    {
        // Test 1: Shift Right with Int32
        int value32 = 20;    // Binary: 10100
        int shift32 = 2;
        int result32 = value32 >> shift32;  // Should be 5 (Binary: 0101)
        if (result32 != 5) return 1;

        // Test 2: Shift Right with Int64
        long value64 = 56L;  // Binary: 111000
        int shift64 = 3;
        long result64 = value64 >> shift64;  // Should be 7 (Binary: 0111)
        if (result64 != 7L) return 2;

        // Test 3: Right shift preserving sign (negative number)
        int negative = -16;
        int negativeResult = negative >> 2;  // Should be -4
        if (negativeResult != -4) return 3;

        // Test 4: Shift by 0
        int noShiftValue = 99;
        int noShiftResult = noShiftValue >> 0;
        if (noShiftResult != 99) return 4;

        // Test 5: Shift with native int
        nint nativeValue = 48;
        int nativeShift = 4;
        nint nativeResult = nativeValue >> nativeShift;  // Should be 3
        if (nativeResult != 3) return 5;

        return 0;
    }
}

public class TestShl
{
    public static int Main(string[] args)
    {
        // Test 1: Shift Left with Int32
        int value32 = 5;     // Binary: 0101
        int shift32 = 2;
        int result32 = value32 << shift32;  // Should be 20 (Binary: 10100)
        if (result32 != 20) return 1;

        // Test 2: Shift Left with Int64
        long value64 = 7L;   // Binary: 0111
        int shift64 = 3;
        long result64 = value64 << shift64;  // Should be 56 (Binary: 111000)
        if (result64 != 56L) return 2;

        // Test 3: Shift by 0
        int noShiftValue = 42;
        int noShiftResult = noShiftValue << 0;
        if (noShiftResult != 42) return 3;

        // Test 4: Shift by 1
        int singleShiftResult = noShiftValue << 1;
        if (singleShiftResult != 84) return 4;

        // Test 5: Shift with native int
        nint nativeValue = 3;
        int nativeShift = 4;
        nint nativeResult = nativeValue << nativeShift;  // Should be 48
        if (nativeResult != 48) return 5;

        return 0;
    }
}

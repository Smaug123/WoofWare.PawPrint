public class TestOr
{
    public static int Main(string[] args)
    {
        // Test 1: Bitwise OR with Int32
        int a32 = 12;        // Binary: 1100
        int b32 = 10;        // Binary: 1010
        int result32 = a32 | b32;  // Should be 14 (Binary: 1110)
        if (result32 != 14) return 1;

        // Test 2: Bitwise OR with Int64
        long a64 = 0x00FF00FFL;
        long b64 = 0xFF00FF00L;
        long result64 = a64 | b64;  // Should be 0xFF00FFFFL
        if (result64 != 0xFF00FFFFL) return 2;

        // Test 3: Mixed bitwise OR (Int32 and native int)
        int aMixed = 15;        // Binary: 1111
        nint bMixed = 240;      // Binary: 11110000
        nint resultMixed = aMixed | bMixed;  // Should be 255 (Binary: 11111111)
        if (resultMixed != 255) return 3;

        // Test 4: OR with itself
        int self = 42;
        int selfResult = self | self;
        if (selfResult != 42) return 4;

        // Test 5: OR with 0
        int withZero = 123;
        int zeroResult = withZero | 0;
        if (zeroResult != 123) return 5;

        // Test 6: Native int OR native int
        nint nativeA = 0x0F;
        nint nativeB = 0xF0;
        nint nativeResult = nativeA | nativeB;  // Should be 0xFF
        if (nativeResult != 0xFF) return 6;

        return 0;
    }
}

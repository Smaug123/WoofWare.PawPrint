public class TestBitwiseOperations
{
    // Test And: Bitwise AND
    public static int TestBitwiseAnd()
    {
        // Basic AND operations
        if ((0xFF & 0x0F) != 0x0F) return 1;
        if ((0x1234 & 0xFF00) != 0x1200) return 2;
        if ((0xFFFF & 0x0000) != 0x0000) return 3;
        if ((0xFFFF & 0xFFFF) != 0xFFFF) return 4;
        
        // Common bit patterns
        if ((0b1010 & 0b1100) != 0b1000) return 5;
        if ((0b1111 & 0b0101) != 0b0101) return 6;
        
        // Identity and zero
        int x = 0x12345678;
        if ((x & 0xFFFFFFFF) != x) return 7;
        if ((x & 0) != 0) return 8;
        
        // Negative numbers (sign bit)
        if ((-1 & 0xFF) != 0xFF) return 9;
        if ((int.MinValue & int.MaxValue) != 0) return 10;
        
        // Masking operations
        int value = 0xABCD;
        int lowByte = value & 0xFF;
        if (lowByte != 0xCD) return 11;
        
        int highByte = (value & 0xFF00) >> 8;
        if (highByte != 0xAB) return 12;
        
        return 0;
    }
    
    // Test Or: Bitwise OR
    public static int TestBitwiseOr()
    {
        // Basic OR operations
        if ((0xF0 | 0x0F) != 0xFF) return 20;
        if ((0x1200 | 0x0034) != 0x1234) return 21;
        if ((0x0000 | 0xFFFF) != 0xFFFF) return 22;
        if ((0x0000 | 0x0000) != 0x0000) return 23;
        
        // Common bit patterns
        if ((0b1010 | 0b0101) != 0b1111) return 24;
        if ((0b1100 | 0b0011) != 0b1111) return 25;
        
        // Identity operations
        int x = 0x12345678;
        if ((x | 0) != x) return 26;
        if ((x | -1) != -1) return 27;
        
        // Setting bits
        int flags = 0;
        flags = flags | 0x01; // Set bit 0
        if (flags != 0x01) return 28;
        
        flags = flags | 0x02; // Set bit 1
        if (flags != 0x03) return 29;
        
        flags = flags | 0x04; // Set bit 2
        if (flags != 0x07) return 30;
        
        return 0;
    }
    
    // Test Xor: Bitwise XOR
    public static int TestBitwiseXor()
    {
        // Basic XOR operations
        if ((0xFF ^ 0xFF) != 0x00) return 40;
        if ((0xFF ^ 0x00) != 0xFF) return 41;
        if ((0xAA ^ 0x55) != 0xFF) return 42;
        if ((0x12 ^ 0x34) != 0x26) return 43;
        
        // XOR properties
        int a = 0x1234;
        if ((a ^ a) != 0) return 44; // Self XOR = 0
        if ((a ^ 0) != a) return 45; // XOR with 0 = identity
        
        // Double XOR (encryption/decryption pattern)
        int data = unchecked((int)0xDEADBEEF);
        int key = 0x12345678;
        int encrypted = data ^ key;
        int decrypted = encrypted ^ key;
        if (decrypted != data) return 46;
        
        // Bit toggling
        int value = 0b1010;
        value = value ^ 0b0011; // Toggle bits 0 and 1
        if (value != 0b1001) return 47;
        
        // Swap without temp variable
        int x = 5, y = 7;
        x = x ^ y;
        y = x ^ y;
        x = x ^ y;
        if (x != 7 || y != 5) return 48;
        
        return 0;
    }
    
    // Test Not: Bitwise NOT (complement)
    public static int TestBitwiseNot()
    {
        // Basic NOT operations
        if (~0 != -1) return 50;
        if (~(-1) != 0) return 51;
        if (~0xFF != unchecked((int)0xFFFFFF00)) return 52;
        
        // Specific bit patterns
        if (~0x55555555 != unchecked((int)0xAAAAAAAA)) return 53;
        if (~0xAAAAAAAA != 0x55555555) return 54;
        
        // Double NOT
        int x = 0x12345678;
        if (~(~x) != x) return 55;
        
        // Sign bit
        if (~int.MaxValue != int.MinValue) return 56;
        if (~int.MinValue != int.MaxValue) return 57;
        
        return 0;
    }
    
    // Test Shl: Shift left
    public static int TestShiftLeft()
    {
        // Basic shift left
        if ((1 << 0) != 1) return 60;
        if ((1 << 1) != 2) return 61;
        if ((1 << 2) != 4) return 62;
        if ((1 << 3) != 8) return 63;
        if ((1 << 4) != 16) return 64;
        
        // Multi-bit values
        if ((0xFF << 8) != 0xFF00) return 65;
        if ((0x1234 << 4) != 0x12340) return 66;
        
        // Shift by zero
        if ((42 << 0) != 42) return 67;
        
        // Large shifts (bits fall off)
        if ((1 << 31) != int.MinValue) return 68;
        if ((1 << 32) != 1) return 69; // Shift count masked to 5 bits
        
        // Negative numbers
        if ((-1 << 1) != -2) return 70;
        if ((-2 << 1) != -4) return 71;
        
        return 0;
    }
    
    // Test Shr: Shift right (arithmetic/signed)
    public static int TestShiftRight()
    {
        // Basic shift right
        if ((8 >> 1) != 4) return 80;
        if ((8 >> 2) != 2) return 81;
        if ((8 >> 3) != 1) return 82;
        if ((8 >> 4) != 0) return 83;
        
        // Multi-bit values
        if ((0xFF00 >> 8) != 0xFF) return 84;
        if ((0x12340 >> 4) != 0x1234) return 85;
        
        // Shift by zero
        if ((42 >> 0) != 42) return 86;
        
        // Negative numbers (sign extension)
        if ((-8 >> 1) != -4) return 87;
        if ((-8 >> 2) != -2) return 88;
        if ((-8 >> 3) != -1) return 89;
        if ((-1 >> 1) != -1) return 90; // Sign bits fill in
        
        // Large value
        if ((int.MinValue >> 1) != unchecked((int)0xC0000000)) return 91;
        
        return 0;
    }
    
    // Test Shr_un: Shift right (logical/unsigned)
    public static int TestShiftRightUnsigned()
    {
        // Basic unsigned shift
        uint u = 8;
        if ((u >> 1) != 4) return 100;
        if ((u >> 2) != 2) return 101;
        if ((u >> 3) != 1) return 102;
        
        // High bit set (no sign extension)
        uint high = 0x80000000;
        if ((high >> 1) != 0x40000000) return 103;
        if ((high >> 31) != 1) return 104;
        
        // Compare with signed shift
        int signedHigh = unchecked((int)0x80000000);
        uint unsignedResult = (uint)(signedHigh) >> 1;  // Logical shift
        if (unsignedResult != 0x40000000) return 105;
        
        // All bits set
        uint allOnes = 0xFFFFFFFF;
        if ((allOnes >> 1) != 0x7FFFFFFF) return 106;
        if ((allOnes >> 16) != 0xFFFF) return 107;
        
        return 0;
    }
    
    // Test combined bitwise operations
    public static int TestBitwiseCombinations()
    {
        // Bit manipulation patterns
        int flags = 0;
        
        // Set bit 2
        flags = flags | (1 << 2);
        if (flags != 4) return 110;
        
        // Set bit 5
        flags = flags | (1 << 5);
        if (flags != 36) return 111;
        
        // Clear bit 2
        flags = flags & ~(1 << 2);
        if (flags != 32) return 112;
        
        // Toggle bit 5
        flags = flags ^ (1 << 5);
        if (flags != 0) return 113;
        
        // Check if bit is set
        int value = 0b10101010;
        bool bit1Set = (value & (1 << 1)) != 0;
        bool bit0Set = (value & (1 << 0)) != 0;
        if (!bit1Set || bit0Set) return 114;
        
        return 0;
    }
    
    // Test bitwise operations with different types
    public static int TestBitwiseWithTypes()
    {
        // Byte operations
        byte b1 = 0xF0;
        byte b2 = 0x0F;
        int bResult = b1 | b2;
        if (bResult != 0xFF) return 120;
        
        // Short operations
        short s1 = unchecked((short)0xFF00);
        short s2 = 0x00FF;
        int sResult = s1 | s2;
        if (sResult != -1) return 121;
        
        // Long operations
        long l1 = unchecked((long)0xFF00FF00FF00FF00UL);
        long l2 = 0x00FF00FF00FF00FFL;
        long lResult = l1 | l2;
        if (lResult != -1L) return 122;
        
        // Unsigned operations
        uint u1 = 0xAAAAAAAA;
        uint u2 = 0x55555555;
        uint uResult = u1 ^ u2;
        if (uResult != 0xFFFFFFFF) return 123;
        
        return 0;
    }
    
    public static int Main(string[] argv)
    {
        int result;
        
        result = TestBitwiseAnd();
        if (result != 0) return 2000 + result;
        
        result = TestBitwiseOr();
        if (result != 0) return 2100 + result;
        
        result = TestBitwiseXor();
        if (result != 0) return 2200 + result;
        
        result = TestBitwiseNot();
        if (result != 0) return 2300 + result;
        
        result = TestShiftLeft();
        if (result != 0) return 2400 + result;
        
        result = TestShiftRight();
        if (result != 0) return 2500 + result;
        
        result = TestShiftRightUnsigned();
        if (result != 0) return 2600 + result;
        
        result = TestBitwiseCombinations();
        if (result != 0) return 2700 + result;
        
        result = TestBitwiseWithTypes();
        if (result != 0) return 2800 + result;
        
        return 0;
    }
}
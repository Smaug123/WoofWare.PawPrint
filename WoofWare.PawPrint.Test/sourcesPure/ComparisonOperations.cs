public class TestComparisonOperations
{
    // Test Ceq: Compare equal
    public static int TestCompareEqual()
    {
        // Integer equality
        if ((5 == 5) != true) return 1;
        if ((5 == 6) != false) return 2;
        if ((int.MaxValue == int.MaxValue) != true) return 3;
        if ((int.MinValue == int.MaxValue) != false) return 4;
        
        // Negative numbers
        if ((-1 == -1) != true) return 5;
        if ((-5 == 5) != false) return 6;
        
        // Long equality
        if ((100L == 100L) != true) return 7;
        if ((100L == 101L) != false) return 8;
        
        // Mixed sizes (after promotion)
        int i = 42;
        long l = 42L;
        if ((l == (long)i) != true) return 9;
        
        // Zero comparisons
        if ((0 == 0) != true) return 10;
        if ((0 == 1) != false) return 11;
        
        return 0;
    }
    
    // Test Cgt: Compare greater than (signed)
    public static int TestCompareGreaterThan()
    {
        // Positive integers
        if ((10 > 5) != true) return 20;
        if ((5 > 10) != false) return 21;
        if ((5 > 5) != false) return 22;
        
        // Negative integers
        if ((-5 > -10) != true) return 23;
        if ((-10 > -5) != false) return 24;
        if ((5 > -5) != true) return 25;
        if ((-5 > 5) != false) return 26;
        
        // Boundary values
        if ((int.MaxValue > int.MinValue) != true) return 27;
        if ((int.MinValue > int.MaxValue) != false) return 28;
        if ((int.MaxValue > (int.MaxValue - 1)) != true) return 29;
        
        // Zero comparisons
        if ((1 > 0) != true) return 30;
        if ((0 > 1) != false) return 31;
        if ((-1 > 0) != false) return 32;
        if ((0 > -1) != true) return 33;
        
        return 0;
    }
    
    // Test Cgt_un: Compare greater than (unsigned)
    public static int TestCompareGreaterThanUnsigned()
    {
        uint a = 10;
        uint b = 5;
        
        // Basic unsigned comparison
        if ((a > b) != true) return 40;
        if ((b > a) != false) return 41;
        if ((a > a) != false) return 42;
        
        // High bit set (would be negative if signed)
        uint high = 0x80000000;
        uint low = 0x7FFFFFFF;
        if ((high > low) != true) return 43;  // Unsigned: high > low
        
        // Maximum values
        uint max = uint.MaxValue;
        uint min = uint.MinValue;
        if ((max > min) != true) return 44;
        if ((min > max) != false) return 45;
        
        // Interpret negative as unsigned
        uint negAsUint = unchecked((uint)-1);
        uint one = 1;
        if ((negAsUint > one) != true) return 46;  // 0xFFFFFFFF > 1
        
        return 0;
    }
    
    // Test Clt: Compare less than (signed)
    public static int TestCompareLessThan()
    {
        // Positive integers
        if ((5 < 10) != true) return 50;
        if ((10 < 5) != false) return 51;
        if ((5 < 5) != false) return 52;
        
        // Negative integers
        if ((-10 < -5) != true) return 53;
        if ((-5 < -10) != false) return 54;
        if ((-5 < 5) != true) return 55;
        if ((5 < -5) != false) return 56;
        
        // Boundary values
        if ((int.MinValue < int.MaxValue) != true) return 57;
        if ((int.MaxValue < int.MinValue) != false) return 58;
        
        // Zero comparisons
        if ((0 < 1) != true) return 59;
        if ((1 < 0) != false) return 60;
        if ((0 < -1) != false) return 61;
        if ((-1 < 0) != true) return 62;
        
        return 0;
    }
    
    // Test Clt_un: Compare less than (unsigned)
    public static int TestCompareLessThanUnsigned()
    {
        uint a = 5;
        uint b = 10;
        
        // Basic unsigned comparison
        if ((a < b) != true) return 70;
        if ((b < a) != false) return 71;
        if ((a < a) != false) return 72;
        
        // High bit set
        uint high = 0x80000000;
        uint low = 0x7FFFFFFF;
        if ((low < high) != true) return 73;  // Unsigned: low < high
        
        // Boundary values
        uint max = uint.MaxValue;
        uint min = uint.MinValue;
        if ((min < max) != true) return 74;
        if ((max < min) != false) return 75;
        
        // Negative as unsigned
        uint one = 1;
        uint negAsUint = unchecked((uint)-1);
        if ((one < negAsUint) != true) return 76;  // 1 < 0xFFFFFFFF
        
        return 0;
    }
    
    // Test comparison combinations
    public static int TestComparisonCombinations()
    {
        int x = 10;
        int y = 20;
        int z = 10;
        
        // Equality chains
        if ((x == z) != true) return 80;
        if ((x == y) != false) return 81;
        
        // Inequality combinations
        if ((x < y && y > x) != true) return 82;
        if ((x < y && x == y) != false) return 83;
        
        // Transitive comparisons
        if (x < y && y < 30)
        {
            if ((x < 30) != true) return 84;
        }
        else
        {
            return 85;
        }
        
        return 0;
    }
    
    // Test comparisons with different types
    public static int TestMixedTypeComparisons()
    {
        // byte comparisons (unsigned by default)
        byte b1 = 200;
        byte b2 = 100;
        if ((b1 > b2) != true) return 90;
        
        // sbyte comparisons (signed)
        sbyte sb1 = -50;
        sbyte sb2 = 50;
        if ((sb1 < sb2) != true) return 91;
        
        // short comparisons
        short s1 = -1000;
        short s2 = 1000;
        if ((s1 < s2) != true) return 92;
        if ((s1 == s2) != false) return 93;
        
        // long comparisons
        long l1 = long.MaxValue;
        long l2 = long.MinValue;
        if ((l1 > l2) != true) return 94;
        
        return 0;
    }
    
    // Test null comparisons
    public static int TestNullComparisons()
    {
        object obj1 = null;
        object obj2 = null;
        object obj3 = new object();
        
        // Null equality
        if ((obj1 == obj2) != true) return 100;
        if ((obj1 == obj3) != false) return 101;
        if ((obj3 == obj1) != false) return 102;
        
        // String null comparisons
        string s1 = null;
        string s2 = null;
        string s3 = "";
        
        if ((s1 == s2) != true) return 103;
        if ((s1 == s3) != false) return 104;
        
        return 0;
    }
    
    public static int Main(string[] argv)
    {
        int result;
        
        result = TestCompareEqual();
        if (result != 0) return 100 + result;
        
        result = TestCompareGreaterThan();
        if (result != 0) return 200 + result;
        
        result = TestCompareGreaterThanUnsigned();
        if (result != 0) return 300 + result;
        
        result = TestCompareLessThan();
        if (result != 0) return 400 + result;
        
        result = TestCompareLessThanUnsigned();
        if (result != 0) return 500 + result;
        
        result = TestComparisonCombinations();
        if (result != 0) return 600 + result;
        
        result = TestMixedTypeComparisons();
        if (result != 0) return 700 + result;
        
        result = TestNullComparisons();
        if (result != 0) return 800 + result;
        
        return 0;
    }
}
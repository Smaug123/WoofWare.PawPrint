public class TestConstantLoading
{
    // Test LdcI4_0 through LdcI4_8: Load int32 constants 0-8
    public static int TestLoadConstants()
    {
        // LdcI4_0
        int zero = 0;
        if (zero != 0) return 1;
        
        // LdcI4_1
        int one = 1;
        if (one != 1) return 2;
        
        // LdcI4_2
        int two = 2;
        if (two != 2) return 3;
        
        // LdcI4_3
        int three = 3;
        if (three != 3) return 4;
        
        // LdcI4_4
        int four = 4;
        if (four != 4) return 5;
        
        // LdcI4_5
        int five = 5;
        if (five != 5) return 6;
        
        // LdcI4_6
        int six = 6;
        if (six != 6) return 7;
        
        // LdcI4_7
        int seven = 7;
        if (seven != 7) return 8;
        
        // LdcI4_8
        int eight = 8;
        if (eight != 8) return 9;
        
        return 0;
    }
    
    // Test LdcI4_m1: Load constant -1
    public static int TestLoadNegativeOne()
    {
        int negOne = -1;
        if (negOne != -1) return 10;
        
        // Test in expressions
        int result = 10 + (-1);
        if (result != 9) return 11;
        
        // Test as bit pattern
        uint unsignedNegOne = unchecked((uint)-1);
        if (unsignedNegOne != 0xFFFFFFFF) return 12;
        
        return 0;
    }
    
    // Test LdNull: Load null reference
    public static int TestLoadNull()
    {
        object obj = null;
        if (obj != null) return 20;
        
        string str = null;
        if (str != null) return 21;
        
        int[] arr = null;
        if (arr != null) return 22;
        
        // Test null comparison
        if (null != null) return 23;
        
        return 0;
    }
    
    // Test constant usage in expressions
    public static int TestConstantsInExpressions()
    {
        // Use multiple constants
        int result = 1 + 2 + 3 + 4 + 5;
        if (result != 15) return 30;
        
        // Constants with operations
        result = 8 * 7 - 6 + 5 - 4 * 3 / 2 + 1 - 0;
        if (result != 50) return 31;
        
        // Negative one in expressions
        result = 10 * -1;
        if (result != -10) return 32;
        
        return 0;
    }
    
    // Test constants as array indices
    public static int TestConstantsAsIndices()
    {
        int[] array = new int[] { 10, 20, 30, 40, 50, 60, 70, 80, 90 };
        
        if (array[0] != 10) return 40;
        if (array[1] != 20) return 41;
        if (array[2] != 30) return 42;
        if (array[3] != 40) return 43;
        if (array[4] != 50) return 44;
        if (array[5] != 60) return 45;
        if (array[6] != 70) return 46;
        if (array[7] != 80) return 47;
        if (array[8] != 90) return 48;
        
        return 0;
    }
    
    // Test constants in conditions
    public static int TestConstantsInConditions()
    {
        // Compare with constants
        int x = 5;
        
        if (x != 5) return 50;
        if (x == 0) return 51;
        if (x == 1) return 52;
        if (x == 2) return 53;
        if (x == 3) return 54;
        if (x == 4) return 55;
        if (x == 6) return 56;
        if (x == 7) return 57;
        if (x == 8) return 58;
        if (x == -1) return 59;
        
        return 0;
    }
    
    // Test null operations
    public static int TestNullOperations()
    {
        // Null assignment and comparison
        object obj1 = null;
        object obj2 = new object();
        
        if (obj1 != null) return 60;
        if (obj2 == null) return 61;
        
        // Null coalescing-like behavior
        object result = obj1 ?? obj2;
        if (result != obj2) return 62;
        
        // Multiple null checks
        string s1 = null;
        string s2 = null;
        if (s1 != s2) return 63;  // Both null should be equal
        
        return 0;
    }
    
    // Test constants with different types
    public static int TestConstantConversions()
    {
        // Constants used in different contexts
        long longVal = 5;  // LdcI4_5 then conv.i8
        if (longVal != 5L) return 70;
        
        short shortVal = 3;  // LdcI4_3 then conv.i2
        if (shortVal != 3) return 71;
        
        byte byteVal = 7;  // LdcI4_7 then conv.u1
        if (byteVal != 7) return 72;
        
        // Negative one conversions
        int negInt = -1;
        uint negUint = unchecked((uint)-1);
        if (negUint != 0xFFFFFFFF) return 73;
        
        return 0;
    }
    
    public static int Main(string[] argv)
    {
        int result;
        
        result = TestLoadConstants();
        if (result != 0) return 100 + result;
        
        result = TestLoadNegativeOne();
        if (result != 0) return 200 + result;
        
        result = TestLoadNull();
        if (result != 0) return 300 + result;
        
        result = TestConstantsInExpressions();
        if (result != 0) return 400 + result;
        
        result = TestConstantsAsIndices();
        if (result != 0) return 500 + result;
        
        result = TestConstantsInConditions();
        if (result != 0) return 600 + result;
        
        result = TestNullOperations();
        if (result != 0) return 700 + result;
        
        result = TestConstantConversions();
        if (result != 0) return 800 + result;
        
        return 0;
    }
}
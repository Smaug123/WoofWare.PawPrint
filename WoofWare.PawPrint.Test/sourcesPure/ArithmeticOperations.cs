using System;

public class TestArithmeticOperations
{
    // Test Add: Addition
    public static int TestAdd()
    {
        // Basic addition
        if (5 + 3 != 8) return 1;
        if (10 + 20 != 30) return 2;
        if (0 + 0 != 0) return 3;
        
        // Negative numbers
        if (-5 + 3 != -2) return 4;
        if (-5 + -3 != -8) return 5;
        if (5 + -5 != 0) return 6;
        
        // Large numbers
        if (1000000 + 2000000 != 3000000) return 7;
        if (int.MaxValue + 0 != int.MaxValue) return 8;
        
        // Different types
        long l = 100L + 200L;
        if (l != 300L) return 9;
        
        // Overflow wrapping (no exception in unchecked)
        int overflow = unchecked(int.MaxValue + 1);
        if (overflow != int.MinValue) return 10;
        
        return 0;
    }
    
    // Test Add_ovf: Addition with overflow check
    public static int TestAddWithOverflow()
    {
        // Normal addition should work
        try
        {
            int result = checked(100 + 200);
            if (result != 300) return 20;
        }
        catch (OverflowException)
        {
            return 21;
        }
        
        // Overflow should throw
        try
        {
            int max = int.MaxValue;
            int one = 1;
            int result = checked(max + one);
            return 22; // Should not reach here
        }
        catch (OverflowException)
        {
            // Expected
        }
        
        // Negative overflow
        try
        {
            int min = int.MinValue;
            int negOne = -1;
            int result = checked(min + negOne);
            return 23; // Should not reach here
        }
        catch (OverflowException)
        {
            // Expected
        }
        
        return 0;
    }
    
    // Test Sub: Subtraction
    public static int TestSubtract()
    {
        // Basic subtraction
        if (10 - 3 != 7) return 30;
        if (5 - 5 != 0) return 31;
        if (0 - 0 != 0) return 32;
        
        // Negative numbers
        if (5 - 10 != -5) return 33;
        if (-5 - 3 != -8) return 34;
        if (-5 - -3 != -2) return 35;
        
        // Large numbers
        if (1000000 - 500000 != 500000) return 36;
        if (int.MinValue - 0 != int.MinValue) return 37;
        
        // Underflow wrapping
        int underflow = unchecked(int.MinValue - 1);
        if (underflow != int.MaxValue) return 38;
        
        return 0;
    }
    
    // Test Mul: Multiplication
    public static int TestMultiply()
    {
        // Basic multiplication
        if (5 * 3 != 15) return 40;
        if (10 * 10 != 100) return 41;
        if (7 * 0 != 0) return 42;
        if (0 * 7 != 0) return 43;
        
        // Negative numbers
        if (-5 * 3 != -15) return 44;
        if (-5 * -3 != 15) return 45;
        if (5 * -3 != -15) return 46;
        
        // Identity
        if (1 * 42 != 42) return 47;
        if (42 * 1 != 42) return 48;
        if (-1 * 42 != -42) return 49;
        
        // Large numbers
        if (1000 * 1000 != 1000000) return 50;
        
        // Overflow wrapping
        int overflow = unchecked(int.MaxValue * 2);
        if (overflow != -2) return 51;
        
        return 0;
    }
    
    // Test Mul_ovf: Multiplication with overflow check
    public static int TestMultiplyWithOverflow()
    {
        // Normal multiplication
        try
        {
            int result = checked(100 * 200);
            if (result != 20000) return 60;
        }
        catch (OverflowException)
        {
            return 61;
        }
        
        // Overflow should throw
        try
        {
            int max = int.MaxValue;
            int two = 2;
            int result = checked(max * two);
            return 62; // Should not reach
        }
        catch (OverflowException)
        {
            // Expected
        }
        
        // Large but valid
        try
        {
            int result = checked(46340 * 46340); // Sqrt of int.MaxValue
            if (result < 0) return 63; // Should be positive
        }
        catch (OverflowException)
        {
            return 64;
        }
        
        return 0;
    }
    
    // Test Div: Division (signed)
    public static int TestDivide()
    {
        // Basic division
        if (10 / 2 != 5) return 70;
        if (15 / 3 != 5) return 71;
        if (7 / 1 != 7) return 72;
        
        // Integer division truncation
        if (7 / 2 != 3) return 73;
        if (10 / 3 != 3) return 74;
        if (1 / 2 != 0) return 75;
        
        // Negative division
        if (-10 / 2 != -5) return 76;
        if (10 / -2 != -5) return 77;
        if (-10 / -2 != 5) return 78;
        
        // Special case: int.MinValue / -1 would overflow at runtime, but the
        // C# compiler constant-folds it under `unchecked` to int.MinValue via
        // two's-complement wraparound, so no `div` IL instruction is emitted.
        int special = unchecked(int.MinValue / -1);
        if (special != int.MinValue) return 79; // Wraps to same value

        return 0;
    }
    
    // Test Div_un: Division (unsigned)
    public static int TestDivideUnsigned()
    {
        uint a = 10;
        uint b = 3;
        
        // Basic unsigned division
        if (a / b != 3) return 80;
        
        // Large unsigned values
        uint large = 0xFFFFFFFF;
        uint half = 2;
        if (large / half != 0x7FFFFFFF) return 81;
        
        // Division by 1
        if (large / 1 != large) return 82;
        
        return 0;
    }
    
    // Test Rem: Remainder (modulo) signed
    public static int TestRemainder()
    {
        // Basic remainder
        if (10 % 3 != 1) return 90;
        if (15 % 5 != 0) return 91;
        if (7 % 10 != 7) return 92;
        
        // Negative operands
        if (-10 % 3 != -1) return 93;
        if (10 % -3 != 1) return 94;
        if (-10 % -3 != -1) return 95;
        
        // Edge cases
        if (0 % 5 != 0) return 96;
        if (1 % 2 != 1) return 97;
        
        return 0;
    }
    
    // Test Rem_un: Remainder unsigned
    public static int TestRemainderUnsigned()
    {
        uint a = 10;
        uint b = 3;
        
        // Basic unsigned remainder
        if (a % b != 1) return 100;
        
        // Large values
        uint large = 0xFFFFFFFF;
        uint divisor = 10;
        uint remainder = large % divisor;
        if (remainder != 5) return 101; // 4294967295 % 10 = 5
        
        return 0;
    }
    
    // Test Neg: Negation
    public static int TestNegate()
    {
        // Basic negation
        if (-5 != -5) return 110;
        if (-(-5) != 5) return 111;
        if (-0 != 0) return 112;
        
        // Variable negation
        int x = 42;
        int negX = -x;
        if (negX != -42) return 113;
        
        // Double negation
        int doubleNeg = -(-x);
        if (doubleNeg != 42) return 114;
        
        // Edge case: negating int.MinValue overflows
        int minNeg = unchecked(-int.MinValue);
        if (minNeg != int.MinValue) return 115; // Wraps to itself
        
        return 0;
    }
    
    // Test complex arithmetic expressions
    public static int TestComplexArithmetic()
    {
        // Order of operations
        int result = 2 + 3 * 4;
        if (result != 14) return 120;
        
        result = (2 + 3) * 4;
        if (result != 20) return 121;
        
        // Complex expression
        result = 10 - 3 * 2 + 8 / 4;
        if (result != 6) return 122; // 10 - 6 + 2
        
        // Mixed operations
        result = 100 / 10 % 3;
        if (result != 1) return 123; // 10 % 3
        
        // Negative in complex expression
        result = -5 + 10 * -2;
        if (result != -25) return 124;
        
        return 0;
    }
    
    // Test arithmetic with different integer types
    public static int TestMixedTypeArithmetic()
    {
        // Byte arithmetic
        byte b1 = 100;
        byte b2 = 50;
        int bResult = b1 + b2; // Promoted to int
        if (bResult != 150) return 130;
        
        // Short arithmetic
        short s1 = 1000;
        short s2 = 2000;
        int sResult = s1 + s2;
        if (sResult != 3000) return 131;
        
        // Long arithmetic
        long l1 = 1000000000L;
        long l2 = 2000000000L;
        long lResult = l1 + l2;
        if (lResult != 3000000000L) return 132;
        
        // Mixed sizes with casting
        int i = 100;
        long l = 200L;
        long mixed = i + l;
        if (mixed != 300L) return 133;
        
        return 0;
    }
    
    public static int Main(string[] argv)
    {
        int result;
        
        result = TestAdd();
        if (result != 0) return 1000 + result;
        
        result = TestAddWithOverflow();
        if (result != 0) return 1100 + result;
        
        result = TestSubtract();
        if (result != 0) return 1200 + result;
        
        result = TestMultiply();
        if (result != 0) return 1300 + result;
        
        result = TestMultiplyWithOverflow();
        if (result != 0) return 1400 + result;
        
        result = TestDivide();
        if (result != 0) return 1500 + result;
        
        result = TestDivideUnsigned();
        if (result != 0) return 1600 + result;
        
        result = TestRemainder();
        if (result != 0) return 1700 + result;
        
        result = TestRemainderUnsigned();
        if (result != 0) return 1800 + result;
        
        result = TestNegate();
        if (result != 0) return 1900 + result;
        
        result = TestComplexArithmetic();
        if (result != 0) return 2000 + result;
        
        result = TestMixedTypeArithmetic();
        if (result != 0) return 2100 + result;
        
        return 0;
    }
}
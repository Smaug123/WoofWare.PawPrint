public class TestStackOperations
{
    // Test LdArg0-3: Load method arguments
    public static int TestLoadArguments(int arg0, int arg1, int arg2, int arg3)
    {
        // LdArg0 loads 'this' for instance methods or first arg for static
        if (arg0 != 10) return 1;
        
        // LdArg1 loads second argument  
        if (arg1 != 20) return 2;
        
        // LdArg2 loads third argument
        if (arg2 != 30) return 3;
        
        // LdArg3 loads fourth argument
        if (arg3 != 40) return 4;
        
        // Test argument reordering
        int sum = arg3 + arg2 + arg1 + arg0;
        if (sum != 100) return 5;
        
        return 0;
    }
    
    // Test Ldloc_0-3 and Stloc_0-3: Load/store local variables
    public static int TestLocalVariables()
    {
        int local0 = 100;
        int local1 = 200;
        int local2 = 300;
        int local3 = 400;
        
        // Test loading locals
        if (local0 != 100) return 10;
        if (local1 != 200) return 11;
        if (local2 != 300) return 12;
        if (local3 != 400) return 13;
        
        // Test storing to locals
        local0 = local1 + local2;  // Stloc_0
        if (local0 != 500) return 14;
        
        local1 = local2 * 2;  // Stloc_1
        if (local1 != 600) return 15;
        
        local2 = local3 - 100;  // Stloc_2
        if (local2 != 300) return 16;
        
        local3 = local0 / 5;  // Stloc_3
        if (local3 != 100) return 17;
        
        return 0;
    }
    
    // Test Pop: Remove top stack value
    public static int TestPop()
    {
        int value = 42;
        
        // Push value on stack then pop it
        PushAndPop(value);
        
        // If we get here, pop worked
        return 0;
    }
    
    private static void PushAndPop(int value)
    {
        // The compiler will generate pop instructions
        // for unused return values
        GetValue();
        GetValue();
    }
    
    private static int GetValue()
    {
        return 123;
    }
    
    // Test Dup: Duplicate top stack value
    public static int TestDup()
    {
        int value = 50;
        
        // Dup is used when same value is needed twice
        int result1 = value * value;  // Compiler may use dup here
        if (result1 != 2500) return 20;
        
        // More complex dup scenario
        int x = 10;
        int result2 = AddTwice(x);
        if (result2 != 20) return 21;
        
        return 0;
    }
    
    private static int AddTwice(int val)
    {
        // Compiler may generate dup to use val twice
        return val + val;
    }
    
    // Test Ret: Return from method
    public static int TestReturn()
    {
        // Test void return
        VoidReturn();
        
        // Test value return
        int result = ValueReturn(5);
        if (result != 5) return 30;
        
        // Test early return
        result = EarlyReturn(true);
        if (result != 1) return 31;
        
        result = EarlyReturn(false);
        if (result != 2) return 32;
        
        return 0;
    }
    
    private static void VoidReturn()
    {
        // Ret with no value
        return;
    }
    
    private static int ValueReturn(int x)
    {
        // Ret with value
        return x;
    }
    
    private static int EarlyReturn(bool condition)
    {
        if (condition)
            return 1;  // Early ret
        
        return 2;  // Normal ret
    }
    
    // Test combinations of stack operations
    public static int TestStackCombinations()
    {
        int a = 10, b = 20, c = 30;
        
        // Complex expression using multiple locals
        int result = (a + b) * c - (b - a);
        if (result != 890) return 40;
        
        // Nested method calls
        result = Compute(a, Compute(b, c));
        if (result != 60) return 41;
        
        return 0;
    }
    
    private static int Compute(int x, int y)
    {
        return x + y;
    }
    
    public static int Main(string[] argv)
    {
        int result;
        
        result = TestLoadArguments(10, 20, 30, 40);
        if (result != 0) return 100 + result;
        
        result = TestLocalVariables();
        if (result != 0) return 200 + result;
        
        result = TestPop();
        if (result != 0) return 300 + result;
        
        result = TestDup();
        if (result != 0) return 400 + result;
        
        result = TestReturn();
        if (result != 0) return 500 + result;
        
        result = TestStackCombinations();
        if (result != 0) return 600 + result;
        
        return 0;
    }
}
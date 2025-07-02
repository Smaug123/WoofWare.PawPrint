using System;
using System.Collections.Generic;

// Test edge cases with generic parameters as mentioned in the diff
public class GenericParameterEdgeCases
{
    // Test method with multiple generic parameters
    public static T2 Convert<T1, T2>(T1 input, Func<T1, T2> converter)
    {
        return converter(input);
    }
    
    // Test nested generic method calls
    public static List<T> WrapInList<T>(T item)
    {
        var list = new List<T>();
        list.Add(item);
        return list;
    }
    
    public static int TestMultipleGenericParameters()
    {
        // Test Convert method with different type combinations
        string result1 = Convert<int, string>(42, x => x.ToString());
        if (result1 != "42") return 1;
        
        int result2 = Convert<string, int>("123", x => int.Parse(x));
        if (result2 != 123) return 2;
        
        return 0;
    }
    
    public static int TestNestedGenericMethodCalls()
    {
        // Test calling generic method from within another generic method
        var intList = WrapInList<int>(42);
        if (intList.Count != 1) return 1;
        if (intList[0] != 42) return 2;
        
        var stringList = WrapInList<string>("test");
        if (stringList.Count != 1) return 3;
        if (stringList[0] != "test") return 4;
        
        return 0;
    }
}

// Test deeply nested generic types
public class DeepNestingTest
{
    public static int TestDeeplyNestedGenerics()
    {
        // Test Dictionary<string, List<Dictionary<int, string>>>
        var complexType = new Dictionary<string, List<Dictionary<int, string>>>();
        
        var innerDict = new Dictionary<int, string>();
        innerDict[1] = "one";
        innerDict[2] = "two";
        
        var listOfDicts = new List<Dictionary<int, string>>();
        listOfDicts.Add(innerDict);
        
        complexType["test"] = listOfDicts;
        
        if (complexType["test"].Count != 1) return 1;
        if (complexType["test"][0][1] != "one") return 2;
        if (complexType["test"][0][2] != "two") return 3;
        
        return 0;
    }
}

// Test generic constraints and inheritance scenarios
public class GenericConstraintTest<T> where T : class
{
    private T value;
    
    public GenericConstraintTest(T val)
    {
        value = val;
    }
    
    public bool IsNull()
    {
        return value == null;
    }
    
    public static int TestGenericConstraints()
    {
        var test = new GenericConstraintTest<string>("hello");
        if (test.IsNull()) return 1;
        
        var nullTest = new GenericConstraintTest<string>(null);
        if (!nullTest.IsNull()) return 2;
        
        return 0;
    }
}

// Test generic field access scenarios mentioned in the diff
public class GenericFieldAccess<T>
{
    public static T DefaultValue = default(T);
    
    public static int TestStaticGenericField()
    {
        // Test that static fields work correctly with generics
        if (GenericFieldAccess<int>.DefaultValue != 0) return 1;
        
        // Test that different instantiations have different static fields
        GenericFieldAccess<int>.DefaultValue = 42;
        if (GenericFieldAccess<int>.DefaultValue != 42) return 2;
        if (GenericFieldAccess<string>.DefaultValue != null) return 3;
        
        return 0;
    }
}

class Program
{
    static int Main(string[] args)
    {
        int result;
        
        result = GenericParameterEdgeCases.TestMultipleGenericParameters();
        if (result != 0) return 100 + result;
        
        result = GenericParameterEdgeCases.TestNestedGenericMethodCalls();
        if (result != 0) return 200 + result;
        
        result = DeepNestingTest.TestDeeplyNestedGenerics();
        if (result != 0) return 300 + result;
        
        result = GenericConstraintTest<string>.TestGenericConstraints();
        if (result != 0) return 400 + result;
        
        result = GenericFieldAccess<int>.TestStaticGenericField();
        if (result != 0) return 500 + result;
        
        return 0; // All tests passed
    }
}
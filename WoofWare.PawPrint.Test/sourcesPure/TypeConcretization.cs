using System;
using System.Collections.Generic;

// Test basic type concretization
public class BasicTypeTest
{
    public static int TestBasicTypes()
    {
        // Test primitive types
        int i = 42;
        string s = "hello";
        bool b = true;
        
        if (i != 42) return 1;
        if (s != "hello") return 2;
        if (!b) return 3;
        
        return 0;
    }
}

// Test generic type instantiation
public class GenericTypeTest<T>
{
    private T value;
    
    public GenericTypeTest(T val)
    {
        value = val;
    }
    
    public T GetValue()
    {
        return value;
    }
    
    public static int TestGenericInstantiation()
    {
        var intTest = new GenericTypeTest<int>(123);
        var stringTest = new GenericTypeTest<string>("test");
        
        if (intTest.GetValue() != 123) return 1;
        if (stringTest.GetValue() != "test") return 2;
        
        return 0;
    }
}

// Test nested generic types
public class NestedGenericTest
{
    public static int TestNestedGenerics()
    {
        var listOfInts = new List<int>();
        listOfInts.Add(1);
        listOfInts.Add(2);
        
        if (listOfInts.Count != 2) return 1;
        if (listOfInts[0] != 1) return 2;
        if (listOfInts[1] != 2) return 3;
        
        var listOfLists = new List<List<int>>();
        listOfLists.Add(listOfInts);
        
        if (listOfLists.Count != 1) return 4;
        if (listOfLists[0].Count != 2) return 5;
        
        return 0;
    }
}

// Test generic methods
public class GenericMethodTest
{
    public static T Identity<T>(T input)
    {
        return input;
    }
    
    public static int TestGenericMethods()
    {
        int intResult = Identity<int>(42);
        string stringResult = Identity<string>("hello");
        
        if (intResult != 42) return 1;
        if (stringResult != "hello") return 2;
        
        return 0;
    }
}

class Program
{
    static int Main(string[] args)
    {
        int result;
        
        result = BasicTypeTest.TestBasicTypes();
        if (result != 0) return 100 + result;
        
        result = GenericTypeTest<int>.TestGenericInstantiation();
        if (result != 0) return 200 + result;
        
        result = NestedGenericTest.TestNestedGenerics();
        if (result != 0) return 300 + result;
        
        result = GenericMethodTest.TestGenericMethods();
        if (result != 0) return 400 + result;
        
        return 0; // All tests passed
    }
}
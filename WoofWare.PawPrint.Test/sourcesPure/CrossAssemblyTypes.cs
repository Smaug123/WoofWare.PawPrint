using System;
using System.Collections.Generic;
using System.Collections;

// Test cross-assembly type resolution using standard library types
public class CrossAssemblyTypeTest
{
    public static int TestSystemTypes()
    {
        // Test various System types to ensure proper assembly resolution
        
        // System.DateTime
        var date = new DateTime(2023, 1, 1);
        if (date.Year != 2023) return 1;
        
        // System.Guid
        var guid = Guid.Empty;
        if (guid != Guid.Empty) return 2;
        
        // System.TimeSpan
        var timeSpan = TimeSpan.FromMinutes(30);
        if (timeSpan.TotalMinutes != 30) return 3;
        
        return 0;
    }
    
    public static int TestCollectionTypes()
    {
        // Test various collection types from different assemblies
        
        // Dictionary<TKey, TValue>
        var dict = new Dictionary<string, int>();
        dict["test"] = 42;
        if (dict["test"] != 42) return 1;
        
        // HashSet<T>
        var hashSet = new HashSet<int>();
        hashSet.Add(1);
        hashSet.Add(2);
        hashSet.Add(1); // duplicate
        if (hashSet.Count != 2) return 2;
        
        // Queue<T>
        var queue = new Queue<string>();
        queue.Enqueue("first");
        queue.Enqueue("second");
        if (queue.Dequeue() != "first") return 3;
        
        return 0;
    }
    
    public static int TestGenericInterfaces()
    {
        // Test generic interfaces across assemblies
        
        var list = new List<int> { 1, 2, 3 };
        
        // IEnumerable<T>
        IEnumerable<int> enumerable = list;
        int count = 0;
        foreach (int item in enumerable)
        {
            count++;
        }
        if (count != 3) return 1;
        
        // ICollection<T>
        ICollection<int> collection = list;
        if (collection.Count != 3) return 2;
        
        // IList<T>
        IList<int> ilist = list;
        if (ilist[0] != 1) return 3;
        
        return 0;
    }
}

// Test Array.Empty<T> which was mentioned in the diff as a specific case
public class ArrayEmptyTest
{
    public static int TestArrayEmpty()
    {
        // Test Array.Empty<T> for different types
        var emptyInts = Array.Empty<int>();
        var emptyStrings = Array.Empty<string>();
        
        if (emptyInts.Length != 0) return 1;
        if (emptyStrings.Length != 0) return 2;
        
        // Verify they are different instances for different types
        // but same instance for same type
        var emptyInts2 = Array.Empty<int>();
        if (!ReferenceEquals(emptyInts, emptyInts2)) return 3;
        
        return 0;
    }
}

class Program
{
    static int Main(string[] args)
    {
        int result;
        
        result = CrossAssemblyTypeTest.TestSystemTypes();
        if (result != 0) return 100 + result;
        
        result = CrossAssemblyTypeTest.TestCollectionTypes();
        if (result != 0) return 200 + result;
        
        result = CrossAssemblyTypeTest.TestGenericInterfaces();
        if (result != 0) return 300 + result;
        
        result = ArrayEmptyTest.TestArrayEmpty();
        if (result != 0) return 400 + result;
        
        return 0; // All tests passed
    }
}
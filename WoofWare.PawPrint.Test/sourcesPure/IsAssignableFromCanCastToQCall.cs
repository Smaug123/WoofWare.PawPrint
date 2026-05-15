using System;

class Animal { }
class Dog : Animal { }
class Cat : Animal { }

public class Program
{
    public static int Main(string[] args)
    {
        // Each of these calls flows through RuntimeType.IsAssignableFrom →
        // RuntimeTypeHandle.CanCastTo → TypeHandle.CanCastTo. The cast cache
        // starts empty, so the first miss on a given (src,dest) pair lands in
        // TypeHandle.CanCastTo_NoCacheLookup (the QCall this test is for).

        // Subclass to base.
        if (!typeof(Animal).IsAssignableFrom(typeof(Dog))) return 1;

        // Downcast is not assignable.
        if (typeof(Dog).IsAssignableFrom(typeof(Animal))) return 2;

        // Sibling reference types are not assignable to each other.
        if (typeof(Dog).IsAssignableFrom(typeof(Cat))) return 3;

        // Reference type to object.
        if (!typeof(object).IsAssignableFrom(typeof(string))) return 4;

        return 0;
    }
}

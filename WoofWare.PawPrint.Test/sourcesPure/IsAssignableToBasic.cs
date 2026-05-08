using System;
using System.Collections.Generic;

class Animal
{
}

class Dog : Animal
{
}

interface IShape
{
}

class Square : IShape
{
}

public class Program
{
    public static int Main(string[] args)
    {
        // Identity
        if (!typeof(string).IsAssignableTo(typeof(string))) return 1;

        // Subclass to base
        if (!typeof(Dog).IsAssignableTo(typeof(Animal))) return 2;

        // Reference type to object
        if (!typeof(string).IsAssignableTo(typeof(object))) return 3;

        // Class to implemented interface
        if (!typeof(Square).IsAssignableTo(typeof(IShape))) return 4;

        // Generic instantiation to constructed interface
        if (!typeof(List<int>).IsAssignableTo(typeof(IEnumerable<int>))) return 5;

        // Unrelated reference types
        if (typeof(string).IsAssignableTo(typeof(Animal))) return 6;

        // Null target
        if (typeof(string).IsAssignableTo(null)) return 7;

        // Symmetric IsAssignableFrom check on the same pair: target.IsAssignableFrom(source)
        if (!typeof(Animal).IsAssignableFrom(typeof(Dog))) return 8;

        // Value type boxes to object
        if (!typeof(int).IsAssignableTo(typeof(object))) return 9;

        // Reference-type array covariance
        if (!typeof(Dog[]).IsAssignableTo(typeof(Animal[]))) return 10;

        // Array reference to object
        if (!typeof(Dog[]).IsAssignableTo(typeof(object))) return 11;

        // Downcast is not assignable
        if (typeof(Animal).IsAssignableTo(typeof(Dog))) return 12;

        return 0;
    }
}

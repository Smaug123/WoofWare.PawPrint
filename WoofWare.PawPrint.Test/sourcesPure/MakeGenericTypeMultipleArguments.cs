using System;
using System.Collections.Generic;

public class Pair<TFirst, TSecond>
{
}

public class Triple<T1, T2, T3>
{
}

public class NeedsStruct<TKey, TValue> where TValue : struct
{
}

public static class Program
{
    // `MakeGenericType` with two or more arguments takes a different route through CoreLib
    // from the one-argument call: `RuntimeTypeHandle.Instantiate(Type[])` copies the handles
    // into an `IntPtr[]`, pins it with `fixed`, and hands the QCall a pointer to element 0.
    // One argument instead takes the address of a single local. Every check here uses at
    // least two arguments so that it is the array-backed buffer being read, and the second and
    // later arguments are what a wrong stride would corrupt.
    //
    // Exit code is the index of the first failing check, so a failure names itself.
    public static int Main()
    {
        Type pair = typeof(Pair<,>).MakeGenericType(typeof(int), typeof(string));
        if (pair != typeof(Pair<int, string>)) return 1;
        if (!ReferenceEquals(pair, typeof(Pair<int, string>))) return 2;
        Type[] pairArgs = pair.GetGenericArguments();
        if (pairArgs.Length != 2) return 3;
        if (pairArgs[0] != typeof(int)) return 4;
        if (pairArgs[1] != typeof(string)) return 5;

        // Swapping the arguments must swap the instantiation: element 1 is read as element 1.
        Type swapped = typeof(Pair<,>).MakeGenericType(typeof(string), typeof(int));
        if (swapped != typeof(Pair<string, int>)) return 6;
        if (swapped == pair) return 7;

        // Three arguments, all distinct, so each element of the buffer is pinned separately.
        Type triple = typeof(Triple<,,>).MakeGenericType(typeof(byte), typeof(object), typeof(List<int>));
        if (triple != typeof(Triple<byte, object, List<int>>)) return 8;
        Type[] tripleArgs = triple.GetGenericArguments();
        if (tripleArgs[0] != typeof(byte)) return 9;
        if (tripleArgs[1] != typeof(object)) return 10;
        if (tripleArgs[2] != typeof(List<int>)) return 11;

        // A CoreLib definition through the same path.
        Type dictionary = typeof(Dictionary<,>).MakeGenericType(typeof(string), typeof(int));
        if (dictionary != typeof(Dictionary<string, int>)) return 12;
        object instance = Activator.CreateInstance(dictionary)!;
        if (instance is not Dictionary<string, int>) return 13;

        // A constraint on the *second* parameter is checked against the second argument.
        if (ConstraintRefused(typeof(NeedsStruct<,>), typeof(string), typeof(string)) is int r1) return 20 + r1;
        Type satisfied = typeof(NeedsStruct<,>).MakeGenericType(typeof(string), typeof(int));
        if (satisfied != typeof(NeedsStruct<string, int>)) return 14;

        return 0;
    }

    // Null when the call throws as CoreCLR does; otherwise a small code saying which check failed.
    static int? ConstraintRefused(Type definition, Type first, Type second)
    {
        try
        {
            definition.MakeGenericType(first, second);
            return 1;
        }
        catch (ArgumentException)
        {
        }

        return null;
    }
}

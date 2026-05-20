using System;

class Box<T> { }

class Pair<T, U> { }

class Program
{
    static int Check(Type t, bool expected, int bit, int acc)
    {
        if (t.IsGenericType != expected)
        {
            return acc | (1 << bit);
        }

        return acc;
    }

    static int Main(string[] args)
    {
        int result = 0;
        result = Check(typeof(int), false, 0, result);
        result = Check(typeof(string), false, 1, result);
        result = Check(typeof(Box<int>), true, 2, result);
        result = Check(typeof(Pair<int, string>), true, 3, result);
        result = Check(typeof(Box<int>[]), false, 4, result);
        result = Check(typeof(int[]), false, 5, result);
        return result;
    }
}

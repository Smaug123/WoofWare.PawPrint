using System;

class OpenBox<T> { }

class OpenPair<T, U> { }

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
        result = Check(typeof(OpenBox<>), true, 0, result);
        result = Check(typeof(OpenPair<,>), true, 1, result);
        return result;
    }
}

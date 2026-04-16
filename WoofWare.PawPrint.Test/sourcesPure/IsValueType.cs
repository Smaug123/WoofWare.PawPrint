using System;

enum MyEnum { A, B }

struct MyStruct { public int X; }

class MyClass { }

delegate void MyDelegate();

class Program
{
    static int Check(System.Type t, bool expected, int bit, int acc)
    {
        if (t.IsValueType != expected)
        {
            return acc | (1 << bit);
        }
        return acc;
    }

    static int Main(string[] args)
    {
        int result = 0;
        result = Check(typeof(int), true, 0, result);
        result = Check(typeof(double), true, 1, result);
        result = Check(typeof(string), false, 2, result);
        result = Check(typeof(MyStruct), true, 3, result);
        result = Check(typeof(MyEnum), true, 4, result);
        result = Check(typeof(MyClass), false, 5, result);
        result = Check(typeof(System.Enum), false, 6, result);
        result = Check(typeof(System.ValueType), false, 7, result);
        result = Check(typeof(System.Object), false, 8, result);
        result = Check(typeof(System.Delegate), false, 9, result);
        result = Check(typeof(System.MulticastDelegate), false, 10, result);
        result = Check(typeof(MyDelegate), false, 11, result);
        return result;
    }
}

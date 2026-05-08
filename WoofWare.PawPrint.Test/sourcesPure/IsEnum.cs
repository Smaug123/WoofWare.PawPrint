using System;

enum MyEnum { A, B }

enum MyByteEnum : byte { X, Y }

struct MyStruct { public int X; }

class MyClass { }

delegate void MyDelegate();

class Program
{
    static int Check(System.Type t, bool expected, int bit, int acc)
    {
        if (t.IsEnum != expected)
        {
            return acc | (1 << bit);
        }
        return acc;
    }

    static int Main(string[] args)
    {
        int result = 0;
        result = Check(typeof(MyEnum), true, 0, result);
        result = Check(typeof(MyByteEnum), true, 1, result);
        result = Check(typeof(int), false, 2, result);
        result = Check(typeof(double), false, 3, result);
        result = Check(typeof(string), false, 4, result);
        result = Check(typeof(MyStruct), false, 5, result);
        result = Check(typeof(MyClass), false, 6, result);
        result = Check(typeof(System.Enum), false, 7, result);
        result = Check(typeof(System.ValueType), false, 8, result);
        result = Check(typeof(System.Object), false, 9, result);
        result = Check(typeof(MyDelegate), false, 10, result);
        result = Check(typeof(int[]), false, 11, result);
        result = Check(typeof(MyEnum[]), false, 12, result);
        return result;
    }
}

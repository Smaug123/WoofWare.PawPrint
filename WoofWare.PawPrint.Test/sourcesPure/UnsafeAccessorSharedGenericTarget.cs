using System;
using System.Runtime.CompilerServices;

// A generic method over a reference type is compiled once, for `System.__Canon`, so a `ref T`
// target position resolves against that shared instantiation rather than the exact class. `__Canon`
// declares no members, so the lookup finds nothing however real the named member is on the actual
// argument -- the accessor is not a way to reach a class's private field generically.
//
// A value-type instantiation is not shared, so the same accessor does reach a struct's field.
// Measured on real .NET 10: the class case reports `'System.__Canon.x'` missing.
public class TestUnsafeAccessorSharedGenericTarget
{
    private class RefType
    {
        private int x;

        public int Peek() => x;
    }

    private struct ValType
    {
        private int x;

        public int Peek() => x;
    }

    [UnsafeAccessor(UnsafeAccessorKind.Field, Name = "x")]
    private static extern ref int Field<T>(ref T t);

    [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "Peek")]
    private static extern int Peek<T>(ref T t);

    private static int Run()
    {
        // Each value-type instantiation is its own type, so the field is reached.
        ValType v = default;
        Field<ValType>(ref v) = 4;
        if (v.Peek() != 4) return 1;

        try
        {
            RefType r = new RefType();
            Field<RefType>(ref r) = 4;
            return 2;
        }
        catch (MissingFieldException e)
        {
            if (!e.Message.Contains("System.__Canon.x")) return 3;
        }

        // The method kind shares the target position, so it shares the answer.
        try
        {
            RefType r = new RefType();
            Peek<RefType>(ref r);
            return 4;
        }
        catch (MissingMethodException e)
        {
            if (!e.Message.Contains("System.__Canon.Peek")) return 5;
        }

        // An array argument is a reference type too.
        try
        {
            int[] a = new int[1];
            Field<int[]>(ref a);
            return 6;
        }
        catch (MissingFieldException) { }

        return 0;
    }

    public static int Main() => Run();
}

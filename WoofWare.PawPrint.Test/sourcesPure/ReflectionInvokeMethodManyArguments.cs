using System;
using System.Reflection;

// `MethodBase.Invoke` on a target taking more than `MethodBaseInvoker.MaxStackAllocArgCount` (4)
// arguments, which routes through `InvokeWithManyArgs` instead of `InvokeDirectByRefWithFewArgs`.
// Up to four arguments are covered by `ReflectionInvokeMethodMultipleArguments.cs` and pass.
//
// The byref buffer here is a `stackalloc IntPtr[3 * argCount]` block offset by `argCount` pointers
// rather than the address of a struct local — a structurally different pointer shape for the QCall
// to stride, which is why it is worth covering separately rather than by raising the argument count
// of the sibling file.
public class Program
{
    private static int SumSix (int a, int b, int c, int d, int e, int f)
    {
        return a + b + c + d + e + f;
    }

    private static MethodInfo Get (string name)
    {
        MethodInfo m = typeof (Program).GetMethod (
            name,
            BindingFlags.Static | BindingFlags.NonPublic);

        if (m == null)
            throw new Exception ("could not find " + name);

        return m;
    }

    public static int Main (string[] args)
    {
        object six = Get ("SumSix").Invoke (null, new object[] { 1, 2, 3, 4, 5, 6 });

        if (!(six is int sixValue) || sixValue != 21)
            return 1;

        return 0;
    }
}

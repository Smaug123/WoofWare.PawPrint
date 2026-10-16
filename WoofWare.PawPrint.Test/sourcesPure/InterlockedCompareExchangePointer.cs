using System;
using System.Reflection;
using System.Threading;

// CoreLib's internal `Interlocked.CompareExchange(int*, int, int)`, which .NET 11's thin lock uses
// on an object's header. Its IL calls itself, which the JIT expands to the same atomic
// compare-exchange as the `ref int` overload's.
//
// It is internal, so it is found by reflection and entered through its function pointer, which
// runs its IL as any call from outside it does; that IL reaches the self-call. .NET 10 has no such
// overload, and exits 0 before the checks.

public static unsafe class Program
{
    public static int Main(string[] args)
    {
        MethodInfo? method = typeof(Interlocked).GetMethod(
            "CompareExchange",
            BindingFlags.NonPublic | BindingFlags.Static,
            new[] { typeof(int*), typeof(int), typeof(int) });

        if (method is null)
        {
            return 0;
        }

        var compareExchange = (delegate*<int*, int, int, int>)method.MethodHandle.GetFunctionPointer();

        int location = 5;

        // The comparand matches, so the value is stored and the old value returned.
        int old = compareExchange(&location, 7, 5);
        if (old != 5)
        {
            return 1;
        }

        if (location != 7)
        {
            return 2;
        }

        // The comparand does not match, so nothing is stored.
        old = compareExchange(&location, 9, 5);
        if (old != 7)
        {
            return 3;
        }

        if (location != 7)
        {
            return 4;
        }

        try
        {
            compareExchange(null, 1, 0);
            return 5;
        }
        catch (NullReferenceException)
        {
        }

        return 0;
    }
}

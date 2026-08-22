using System;
using System.Reflection;
using System.Runtime.InteropServices;

// `MethodInfo.Invoke` on a `[UnmanagedCallersOnly]` method. Reflection reaches the same prologue as
// the delegate route in `UnmanagedCallersOnlyDelegateInvoke.cs`, and dies the same way; this is a
// separate guest because it arrives through a different interpreter path.
//
// Real .NET wraps a *catchable* exception thrown by the target in `TargetInvocationException`, so
// the catch below would be the guest's normal way of seeing a failure. It sees nothing: the
// transition failure is a fatal error, not an exception.

public static class Program
{
    [UnmanagedCallersOnly]
    public static int Doubler (int x)
    {
        return x * 2;
    }

    public static int Main ()
    {
        MethodInfo doubler = typeof (Program).GetMethod ("Doubler");

        try
        {
            return (int) doubler.Invoke (null, new object[] { 21 });
        }
        catch (Exception)
        {
            return 1;
        }
    }
}

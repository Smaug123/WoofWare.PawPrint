using System;
using System.Reflection;
using System.Runtime.InteropServices;

// The same reflection call as `UnmanagedCallersOnlyReflectionInvoke.cs`, but forced down CoreLib's
// *emitted* invoke stub rather than its interpreted one, by setting the switch
// `Switch.System.Reflection.ForceEmitInvoke` before the first invoke caches a strategy
// (`LocalAppContextSwitches.ForceEmitInvoke`, read by `MethodInvokerCommon`).
//
// This is a distinct guest because the emitted stub reaches its target through an ordinary `call`
// in IL it generated, rather than through the interpreter's reflection path — a different arrival
// at the same refusal, and one that a gate placed on the reflection path alone would miss.

public static class Program
{
    [UnmanagedCallersOnly]
    public static int Doubler (int x)
    {
        return x * 2;
    }

    public static int Main ()
    {
        AppContext.SetSwitch ("Switch.System.Reflection.ForceEmitInvoke", true);

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

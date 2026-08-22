using System;
using System.Reflection;
using System.Runtime.InteropServices;

// The same reflection call as `UnmanagedCallersOnlyReflectionInvoke.cs`, but forced down CoreLib's
// *emitted* invoke stub rather than its interpreted one, by setting the switch
// `Switch.System.Reflection.ForceEmitInvoke` before the first invoke caches a strategy
// (`LocalAppContextSwitches.ForceEmitInvoke`, read by `MethodInvokerCommon`).
//
// On the *oracle* this is a distinct arrival: the emitted stub reaches its target through an
// ordinary `call` in IL it generated, rather than through the runtime's interpreted invoke.
//
// Under PawPrint it is not, and that is measured rather than assumed: dynamic code is off by
// default, so CoreLib falls back to the interpreted invoke and this guest takes the same route as
// `UnmanagedCallersOnlyReflectionInvoke.cs` — mutating the reflection call site's convention kills
// both. Turning `RuntimeFeature.IsDynamicCodeSupported` on does send it down the emit path, where
// it stops at an unimplemented `ModuleHandle.ResolveMethod` for a method on a generic type, short
// of reaching the gate at all. So the guest earns its place as an oracle-side route today, and
// becomes a second interpreter-side arrival for free once PawPrint can emit.

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

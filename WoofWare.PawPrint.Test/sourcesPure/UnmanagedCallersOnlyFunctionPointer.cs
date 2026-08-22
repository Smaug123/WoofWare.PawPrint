using System;
using System.Reflection;
using System.Runtime.InteropServices;

// The *legal* half of `[UnmanagedCallersOnly]`, and the control for the refusal that
// `TestUnmanagedCallersOnlyEntry.fs` pins.
//
// A `[UnmanagedCallersOnly]` method may be entered only from native code: CoreCLR compiles it with
// `CORJIT_FLAG_REVERSE_PINVOKE`, whose prologue asserts preemptive GC mode, so a managed entry
// trips `ReversePInvokeBadTransition` (dllimportcallback.cpp) and takes the process down
// uncatchably. Calling it through a `delegate* unmanaged<...>` function pointer is the one route
// that is *not* a managed entry, and it must keep working.
//
// Everything here exits 0 on both runtimes; each numbered check is a route that must NOT be
// refused. Between them they say what the gate must leave alone:
//
//  * checks 1-2: the unmanaged call site itself, which is the legal transition;
//  * checks 3-5: binding a delegate to the method, which real .NET permits (`BindToMethod` has no
//    such check — the only bind-time refusal is `GetDelegateCtor`'s, unreachable from C#). It is
//    only *invoking* the bound delegate that dies, so a fix that refused the bind would be wrong;
//  * checks 6-8: a method carrying a *different* attribute that merely shares the simple name
//    `UnmanagedCallersOnlyAttribute`. A classifier that matched on name without namespace would
//    abort here;
//  * checks 9-10: an ordinary method, so the gate is not simply refusing everything.
//
// Returns 0 on success, or the number of the first check that failed.

namespace NotInterop
{
    // A decoy sharing the simple name but not the namespace. Applying it must change nothing.
    [AttributeUsage (AttributeTargets.Method)]
    public sealed class UnmanagedCallersOnlyAttribute : Attribute
    {
    }
}

public static unsafe class Program
{
    [UnmanagedCallersOnly]
    public static int Doubler (int x)
    {
        return x * 2;
    }

    [NotInterop.UnmanagedCallersOnly]
    public static int Decoy (int x)
    {
        return x + 1;
    }

    public static int Plain (int x)
    {
        return x - 1;
    }

    public static int Main ()
    {
        // 1: the legal transition. `delegate* unmanaged<int, int>` is a call site whose signature
        // header carries `SignatureCallingConvention.Unmanaged` (0x09) rather than `Default`, and
        // that is exactly what distinguishes it from every managed route below.
        delegate* unmanaged<int, int> fp = &Doubler;

        if (fp (21) != 42)
        {
            return 1;
        }

        // 2: and again through a local of `nint` width, so the pointer genuinely round-trips
        // rather than being folded at the call site.
        nint raw = (nint) fp;

        if (((delegate* unmanaged<int, int>) raw) (50) != 100)
        {
            return 2;
        }

        // 3-5: binding succeeds, and reports the method it bound to. Real .NET builds this
        // delegate happily; it is the invocation that would die, and nothing here invokes it.
        MethodInfo doublerMethod = typeof (Program).GetMethod ("Doubler");
        Func<int, int> bound = (Func<int, int>) doublerMethod.CreateDelegate (typeof (Func<int, int>));

        if (bound == null)
        {
            return 3;
        }

        if (bound.Target != null)
        {
            return 4;
        }

        if (bound.Method.Name != "Doubler")
        {
            return 5;
        }

        // 6-8: the decoy is an ordinary method and stays callable by every route.
        if (Decoy (41) != 42)
        {
            return 6;
        }

        Func<int, int> decoyDelegate =
            (Func<int, int>) typeof (Program).GetMethod ("Decoy").CreateDelegate (typeof (Func<int, int>));

        if (decoyDelegate (41) != 42)
        {
            return 7;
        }

        if ((int) typeof (Program).GetMethod ("Decoy").Invoke (null, new object[] { 41 }) != 42)
        {
            return 8;
        }

        // 9-10: an entirely unattributed method, so a gate that refused everything is caught even
        // if the decoy's own attribute were somehow mis-parsed.
        Func<int, int> plainDelegate =
            (Func<int, int>) typeof (Program).GetMethod ("Plain").CreateDelegate (typeof (Func<int, int>));

        if (plainDelegate (43) != 42)
        {
            return 9;
        }

        if ((int) typeof (Program).GetMethod ("Plain").Invoke (null, new object[] { 43 }) != 42)
        {
            return 10;
        }

        return 0;
    }
}

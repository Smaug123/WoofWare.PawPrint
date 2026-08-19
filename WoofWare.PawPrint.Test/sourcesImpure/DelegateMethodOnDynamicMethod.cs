using System;
using System.Reflection;
using System.Reflection.Emit;

// `Delegate.Method` on a delegate bound to a `DynamicMethod`. This does *not* reach the QCall
// `Delegate_FindMethodHandle`: `DynamicMethod.CreateDelegate` calls `d.StoreDynamicMethod(this)`
// straight after binding (DynamicMethod.CoreCLR.cs:60), which fills `_methodBase`, and
// `Delegate.GetMethodImpl` returns that without consulting the runtime.
//
// That short-circuit lives entirely in interpreted CoreLib IL, so nothing in PawPrint enforces it.
// This case is what keeps it honest: the handler's `FunctionPointerTarget.Dynamic` arm refuses
// loudly, so if the caching path is ever perturbed this fails in CI rather than in a guest.
//
// Impure because PawPrint declares dynamic code unsupported by default and the harness registers
// this case with the switch overridden — see `DynamicCodeSupportedOverride.cs` for that contract.
//
// Returns 0 on success, or the number of the first check that failed.

public class Program
{
    public static int Main (string[] argv)
    {
        DynamicMethod dm = new DynamicMethod (
            "Probe",
            typeof (int),
            new Type[] { typeof (int) },
            typeof (Program).Module);
        ILGenerator il = dm.GetILGenerator ();
        il.Emit (OpCodes.Ldarg_0);
        il.Emit (OpCodes.Ret);

        Func<int, int> f = (Func<int, int>) dm.CreateDelegate (typeof (Func<int, int>));

        MethodInfo m = f.Method;
        if (m == null) return 1;
        if (m.Name != "Probe") return 2;

        // The cached `_methodBase` is the `DynamicMethod` itself, not a fresh MethodInfo built
        // from a handle — which is the observable difference between answering from the cache and
        // answering from the runtime.
        if (!ReferenceEquals (m, dm)) return 3;

        return 0;
    }
}

using System;
using System.Reflection;
using System.Runtime.CompilerServices;

// `MethodBase.Invoke` on a target that PawPrint services as a JIT intrinsic rather than by
// interpreting IL. `Unsafe.SizeOf<T>()` is the canonical one: real .NET treats it as an ordinary
// reflectable method and `Invoke` answers 8 for `long`.
//
// Split out of `sourcesPure/ReflectionInvokeMethod.cs` because the interesting path is the *call*:
// `callMethodWithCommitment` services such a method inline rather than pushing a frame, and the
// frame it is asked to complete on behalf of is the native `RuntimeMethodHandle_InvokeMethod` QCall,
// which has no IL and so no program counter to advance.
//
// As in the sibling files, each MethodInfo is invoked exactly once: after the first invocation
// `MethodInvokerCommon.DetermineStrategy_*` switches to a Reflection.Emit delegate and stops
// exercising this QCall.
public class Program
{
    public static int Main (string[] args)
    {
        MethodInfo sizeOf = typeof (Unsafe)
            .GetMethod ("SizeOf", BindingFlags.Static | BindingFlags.Public)
            .MakeGenericMethod (typeof (long));

        object size = sizeOf.Invoke (null, null);

        if (!(size is int sizeValue))
            return 1;

        if (sizeValue != 8)
            return 2;

        return 0;
    }
}

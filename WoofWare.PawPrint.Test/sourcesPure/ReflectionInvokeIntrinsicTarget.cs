using System;
using System.Reflection;
using System.Runtime.CompilerServices;

// `MethodBase.Invoke` on a target that PawPrint services as a JIT intrinsic rather than by
// interpreting IL. `Unsafe.SizeOf<T>()` is the canonical one: real .NET treats it as an ordinary
// reflectable method and `Invoke` answers 8 for `long`.
//
// Split out of `sourcesPure/ReflectionInvokeMethod.cs` because the blocker is in the *call* path
// rather than in the `RuntimeMethodHandle_InvokeMethod` QCall's own bookkeeping.
// `callMethodWithCommitment` services such a method inline: it computes the result and then
// advances the caller's program counter, which is right for a `call` opcode but not for the native
// QCall frame the invocation is running under, since that frame has no IL. The QCall rejects the
// shape up front so the interpreter fails with a message naming the method instead of aborting
// inside `advanceProgramCounter`.
//
// Un-parking this means letting `Intrinsics.call` honour `advanceProgramCounterOfCaller = false`,
// which reaches every intrinsic's completion path.
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

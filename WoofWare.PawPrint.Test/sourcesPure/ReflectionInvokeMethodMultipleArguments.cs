using System;
using System.Reflection;

// `MethodBase.Invoke` on targets taking more than one argument. The single-argument shapes live in
// `sourcesPure/ReflectionInvokeMethod.cs`; this file exists separately because everything here is
// blocked on a write-path gap that has nothing to do with reflection, and parking the two together
// would hide the coverage that does pass.
//
// As in the sibling file, every distinct MethodInfo is invoked exactly once: after the first
// invocation `MethodInvokerCommon.DetermineStrategy_*` switches to a Reflection.Emit delegate and
// stops exercising the `RuntimeMethodHandle_InvokeMethod` QCall at all.
public class Program
{
    // Two arguments, one of each kind: a value-type parameter (whose byref addresses a box payload)
    // and a reference-type one (whose byref addresses an `object?` slot).
    private static int AddLength (int a, string s)
    {
        return a + s.Length;
    }

    // More than `MethodBaseInvoker.MaxStackAllocArgCount` (4) arguments, so the call routes through
    // `InvokeWithManyArgs`, whose byref buffer is a `stackalloc IntPtr[3 * argCount]` block offset
    // by `argCount` pointers rather than the address of a struct local. That is a structurally
    // different pointer shape for the QCall to stride.
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
        object sum = Get ("AddLength").Invoke (null, new object[] { 10, "abc" });

        if (!(sum is int sumValue) || sumValue != 13)
            return 1;

        object six = Get ("SumSix").Invoke (null, new object[] { 1, 2, 3, 4, 5, 6 });

        if (!(six is int sixValue) || sixValue != 21)
            return 2;

        return 0;
    }
}

using System;
using System.Reflection;

// `MethodBase.Invoke` on a target taking more than one argument. The single-argument shapes live in
// `sourcesPure/ReflectionInvokeMethod.cs`; this file exists separately because everything here was
// blocked on a write-path gap that has nothing to do with reflection, and parking the two together
// would have hidden the coverage that did pass.
//
// That gap: `InvokeDirectByRefWithFewArgs` fills a `StackAllocatedByRefs` local — an
// `[InlineArray(4)]` of `ref byte` — with `*(ByReference*)(pByRefFixedStorage + i) = ...`. A
// `ByReference` wraps a managed pointer and so has no byte image, and the buffer is four of them, so
// this is the one shape where a store's width cannot be recovered from the bytes: index 0 arrives as
// a bare byref (`p + 0` is `p`) and used to replace the whole 32-byte local with an 8-byte value,
// and index 1 then found only 8 bytes to write into. Both halves of that are the width rule, and
// `TestNarrowByrefAccess.fs` pins them cell by cell.
//
// As in the sibling file, every distinct MethodInfo is invoked exactly once: after the first
// invocation `MethodInvokerCommon.DetermineStrategy_*` switches to a Reflection.Emit delegate and
// stops exercising the `RuntimeMethodHandle_InvokeMethod` QCall at all.
//
// Targets taking more than four arguments take a structurally different route through
// `InvokeWithManyArgs`, and live in `ReflectionInvokeMethodManyArguments.cs`, which is parked on an
// unrelated missing primitive.
public class Program
{
    // Two arguments, one of each kind: a value-type parameter (whose byref addresses a box payload)
    // and a reference-type one (whose byref addresses an `object?` slot).
    private static int AddLength (int a, string s)
    {
        return a + s.Length;
    }

    // Four arguments: `MethodBaseInvoker.MaxStackAllocArgCount` exactly, so every one of the
    // `StackAllocatedByRefs` inline array's slots is written and read. Three of the four are reached
    // by a byte cursor whose view type is `System.Byte` while the store itself is eight bytes wide,
    // which is the case a store that took its width from the pointer could not serve.
    private static int SumFour (int a, int b, int c, int d)
    {
        return a + b + c + d;
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

        object four = Get ("SumFour").Invoke (null, new object[] { 1, 2, 3, 4 });

        if (!(four is int fourValue) || fourValue != 10)
            return 2;

        return 0;
    }
}

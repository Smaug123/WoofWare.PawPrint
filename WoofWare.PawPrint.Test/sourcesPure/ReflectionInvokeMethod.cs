using System;
using System.Reflection;

// `MethodBase.Invoke` down to its primitive: the `RuntimeMethodHandle_InvokeMethod` QCall
// (reflectioninvocation.cpp:311), reached via `MethodBaseInvoker.InterpretedInvoke_Method`.
//
// Every method here takes at most one argument, and no type reflected over here declares a virtual
// method. Both restrictions are about blockers elsewhere, not about this QCall: see the sibling
// `ReflectionInvokeMethodMultipleArguments.cs` and `ReflectionInvokeVirtualMethod.cs`.
//
// EVERY DISTINCT MethodInfo BELOW IS INVOKED EXACTLY ONCE.
// `MethodInvokerCommon.DetermineStrategy_RefArgs` / `_ObjSpanArgs`
// (MethodInvokerCommon.cs:114-121, :168-181) deliberately take the interpreted
// `RuntimeMethodHandle.InvokeMethod` path only on a given MethodBase's *first* invocation, and
// build a Reflection.Emit delegate for every invocation after that (whenever
// `RuntimeFeature.IsDynamicCodeSupported`, which is true by default; PawPrint seeds it false, but
// the differential oracle runs this program on a stock host, where it is true). A second `Invoke`
// of the same method would therefore stop exercising this QCall at all on the oracle side, and
// start exercising Reflection.Emit. `MethodInfo`s are cached per-method on the
// `RuntimeType`, including `MakeGenericMethod` instantiations, so re-fetching one does not reset it.
public class Program
{
    private static string sideEffect;

    private static void SetSideEffect ()
    {
        sideEffect = "ran";
    }

    // Reference-type parameter, reference-type return: the shape F#'s `sprintf` reaches, where the
    // byref in the argument buffer addresses an `object?` slot rather than a box payload.
    private static string Decorate (string a)
    {
        return "[" + a + "]";
    }

    // A reference-type return that is null. Worth its own case: the re-entry protocol has to tell
    // "the callee returned null" apart from its own bookkeeping, and both are null references, so
    // a handler that confused the two would still pass every non-null case here.
    private static string ReturnsNull (string a)
    {
        return null;
    }

    // Value-type parameter — its byref addresses the *box payload* rather than an object slot —
    // and a value-type return, which the QCall must box before handing it back.
    private static int Doubled (int a)
    {
        return a * 2;
    }

    // Static generic method: `MakeGenericMethod` then `Invoke`, instantiated at a value type. This
    // is precisely the `Specializations<...>.CaptureFinalN<A, B, C>` shape sprintf builds.
    private static T[] Pair<T> (T value)
    {
        return new[]
        {
            value,
            value,
        };
    }

    private static void Throws ()
    {
        throw new InvalidOperationException ("boom");
    }

    // Not `beforefieldinit` (it declares an explicit static constructor), so the cctor runs at the
    // moment of the first call and not before. Nothing else in this file touches the type, so the
    // `Invoke` below is what triggers class initialisation — which the QCall must handle by
    // suspending and being re-entered, rather than by calling into an uninitialised type.
    private static string trace = "";

    private class LazilyInitialised
    {
        static LazilyInitialised ()
        {
            trace += "cctor;";
        }

        // Deliberately reads *Program*'s static rather than one of its own, so the body contains
        // nothing that would trigger this type's initialisation by itself. Only the invocation
        // machinery can run the cctor, which is what makes the ordering below discriminating: a
        // target that touched its own statics would be initialised by the `ldsfld` regardless.
        public static string Probe ()
        {
            return trace;
        }
    }

    private static MethodInfo Get (Type t, string name)
    {
        MethodInfo m = t.GetMethod (
            name,
            BindingFlags.Static | BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic);

        if (m == null)
            throw new Exception ("could not find " + name);

        return m;
    }

    public static int Main (string[] args)
    {
        // Void return: `Invoke` hands back null, and the side effect proves the target really ran
        // rather than the call being elided.
        object voidResult = Get (typeof (Program), "SetSideEffect").Invoke (null, null);

        if (voidResult != null)
            return 1;

        if (sideEffect != "ran")
            return 2;

        // Reference-type argument and return.
        object decorated = Get (typeof (Program), "Decorate").Invoke (null, new object[] { "x" });

        if (!(decorated is string s) || s != "[x]")
            return 3;

        // Reference-type return that is null.
        object nullResult = Get (typeof (Program), "ReturnsNull").Invoke (null, new object[] { "y" });

        if (nullResult != null)
            return 4;

        // Value-type argument, and a value-type return the QCall must box.
        object doubled = Get (typeof (Program), "Doubled").Invoke (null, new object[] { 21 });

        if (!(doubled is int i) || i != 42)
            return 5;

        // Generic static method instantiated at a value type.
        MethodInfo pair = Get (typeof (Program), "Pair").MakeGenericMethod (typeof (int));
        object paired = pair.Invoke (null, new object[] { 7 });

        if (!(paired is int[] arr))
            return 6;

        if (arr.Length != 2 || arr[0] != 7 || arr[1] != 7)
            return 7;

        // Class initialisation triggered by the invocation itself, asserted by *ordering* rather
        // than only by the value read back: a handler that skipped initialisation would still
        // answer 42 if something upstream had already run the cctor, so pin that nothing has.
        MethodInfo probe = Get (typeof (LazilyInitialised), "Probe");

        if (trace != "")
            return 8;

        object probed = probe.Invoke (null, null);

        if (trace != "cctor;")
            return 9;

        // The cctor must have run *before* the body, not merely at some point during the call.
        if (!(probed is string probedValue) || probedValue != "cctor;")
            return 10;

        // A throwing target. `MethodBaseInvoker` wraps in `TargetInvocationException` in *managed*
        // code (MethodBaseInvoker.cs:176), so the QCall must let the original exception propagate
        // unwrapped: if it wrapped as well, `InnerException` here would itself be a
        // TargetInvocationException.
        try
        {
            Get (typeof (Program), "Throws").Invoke (null, null);
            return 11;
        }
        catch (TargetInvocationException ex)
        {
            if (ex.InnerException is TargetInvocationException)
                return 12;

            if (!(ex.InnerException is InvalidOperationException ioe))
                return 13;

            if (ioe.Message != "boom")
                return 14;
        }

        return 0;
    }
}

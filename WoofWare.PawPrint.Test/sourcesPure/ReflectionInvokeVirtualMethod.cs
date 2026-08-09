using System;
using System.Reflection;

// `MethodBase.Invoke` on a *virtual* method, looked up through the base class that declares it: the
// `RuntimeMethodHandle_InvokeMethod` QCall must dispatch virtually, so the derived override runs.
// CoreCLR takes `pMeth->GetSingleCallableAddrOfVirtualizedCode(&gc.target, ownerType)` for a vtable
// method (reflectioninvocation.cpp:417-424) rather than the method's own entry point.
//
// Split out of `sourcesPure/ReflectionInvokeMethod.cs` because it is blocked on something with no
// connection to reflection *invocation*: `RuntimeType.RuntimeTypeCache.PopulateMethods` calls
// `RuntimeMethodHandle.GetSlot` for every virtual method it enumerates (RuntimeType.CoreCLR.cs:685),
// so merely calling `GetMethod` on a type that declares one reaches that unimplemented InternalCall.
// The blocker therefore fires at the *lookup*, before any invocation happens.
//
// As in the sibling files, each MethodInfo is invoked exactly once: after the first invocation
// `MethodInvokerCommon.DetermineStrategy_*` switches to a Reflection.Emit delegate and stops
// exercising this QCall.
public class Program
{
    private class Base
    {
        public virtual string Which ()
        {
            return "base";
        }
    }

    private class Derived : Base
    {
        public override string Which ()
        {
            return "derived";
        }
    }

    public static int Main (string[] args)
    {
        MethodInfo which = typeof (Base).GetMethod (
            "Which",
            BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic);

        if (which == null)
            return 1;

        object result = which.Invoke (new Derived (), null);

        if (!(result is string s))
            return 2;

        // The override, not the base implementation: an interpreter that called the MethodInfo's
        // own body would answer "base" here.
        if (s != "derived")
            return 3;

        return 0;
    }
}

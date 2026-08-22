using System;
using System.Reflection;

// Reaching `Assembly.GetExecutingAssembly()` through `MethodBase.Invoke`, which is what makes the
// stack crawl's reflection-frame skipping observable.
//
// Invoked this way, the frame immediately outside the one declaring the stack-crawl mark is not
// this guest at all: it is CoreLib's invoke machinery (`MethodBaseInvoker`, `RuntimeMethodInfo`,
// an `InvokeStub_` dynamic method). CoreCLR's crawl steps over every one of those — they are
// `SystemDomain::IsReflectionInvocationMethod` — and so reaches this guest, exactly as a direct
// call would. A crawl that did not skip them would answer `System.Private.CoreLib`.
//
// `GetExecutingAssembly` is invoked exactly once: after the first invocation
// `MethodInvokerCommon.DetermineStrategy_*` switches that MethodInfo to a Reflection.Emit
// delegate and the interpreted invoke path is not taken again.
public class Program
{
    public static int Main (string[] args)
    {
        MethodInfo getExecuting = typeof (Assembly).GetMethod (
            "GetExecutingAssembly",
            BindingFlags.Static | BindingFlags.Public,
            null,
            Type.EmptyTypes,
            null);

        if (getExecuting == null)
            return 1;

        object invoked = getExecuting.Invoke (null, null);

        if (!(invoked is Assembly assembly))
            return 2;

        if (!ReferenceEquals (assembly, typeof (Program).Assembly))
            return 3;

        return 0;
    }
}

using System;
using System.Reflection;

// `MethodBase.Invoke` on targets whose signature mentions a pointer. Split out of
// `sourcesPure/ReflectionInvokeMethod.cs` because pointers are the one shape where reflection does
// *not* simply pass the CLR representation through, so both directions need work of their own:
//
//  * a pointer parameter is marked `InvokerArgFlags.IsValueType` by `MethodInvokerCommon.Initialize`,
//    so its entry in the byref buffer addresses the payload of a boxed `IntPtr` rather than an
//    `object?` slot — reachable with a plain `null` argument, which `CheckValue` converts to
//    `IntPtr.Zero`;
//  * a pointer return is wrapped by `InvokeUtil::CreateObjectAfterInvoke` in a
//    `System.Reflection.Pointer`, which carries the pointed-to Type as well as the address, so
//    `Invoke` never returns null for one even when the pointer itself is null. A function-pointer
//    return is boxed as an `IntPtr` instead.
//
// As in the sibling files, each MethodInfo is invoked exactly once: after the first invocation
// `MethodInvokerCommon.DetermineStrategy_*` switches to a Reflection.Emit delegate and stops
// exercising the `RuntimeMethodHandle_InvokeMethod` QCall.
public class Program
{
    private static unsafe int Deref (int* p)
    {
        return p == null ? -1 : *p;
    }

    private static unsafe int* Null ()
    {
        return null;
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
        // A null pointer argument: `CheckValue` turns the `null` into `IntPtr.Zero` and boxes it, so
        // the callee sees a null `int*`.
        object dereferenced = Get ("Deref").Invoke (null, new object[] { null });

        if (!(dereferenced is int derefValue) || derefValue != -1)
            return 1;

        // A null pointer *return* still comes back as a non-null `Pointer` box.
        object returned = Get ("Null").Invoke (null, null);

        if (returned == null)
            return 2;

        if (!(returned is Pointer))
            return 3;

        unsafe
        {
            if (Pointer.Unbox (returned) != null)
                return 4;
        }

        return 0;
    }
}

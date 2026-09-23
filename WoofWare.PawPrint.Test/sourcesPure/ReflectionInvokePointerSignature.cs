using System;
using System.Reflection;

// `MethodBase.Invoke` on targets whose signature mentions a pointer. Split out of
// `sourcesPure/ReflectionInvokeMethod.cs` because pointers are the one shape where reflection does
// *not* simply pass the CLR representation through, so both directions need work of their own:
//
//  * a pointer parameter is marked `InvokerArgFlags.IsValueType` by `MethodInvokerCommon.Initialize`,
//    so its entry in the byref buffer addresses the payload of a boxed `IntPtr` rather than an
//    `object?` slot. `RuntimeType.CheckValue` produces that `IntPtr` from a `null` (as `IntPtr.Zero`),
//    from an `IntPtr` (as a copy), or from a `System.Reflection.Pointer` (by unwrapping it);
//  * a pointer return is wrapped by `InvokeUtil::CreateObjectAfterInvoke` in a
//    `System.Reflection.Pointer`, which carries the pointer *type* as well as the address, so
//    `Invoke` never returns null for one even when the pointer itself is null. A function-pointer
//    return is boxed as an `IntPtr` instead.
//
// The non-null cases address a static field, so the address outlives every frame involved and a
// pointer that went out through `Invoke` and came back must still dereference to the same storage.
//
// As in the sibling files, each MethodInfo is invoked exactly once: after the first invocation
// `MethodInvokerCommon.DetermineStrategy_*` switches to a Reflection.Emit delegate and stops
// exercising the `RuntimeMethodHandle_InvokeMethod` QCall.
public unsafe class Program
{
    private static int s_cell = 17;

    private static int Deref (int* p)
    {
        return p == null ? -1 : *p;
    }

    private static int DerefFromIntPtr (int* p)
    {
        return p == null ? -1 : *p;
    }

    private static int DerefFromPointerBox (int* p)
    {
        return p == null ? -1 : *p;
    }

    private static int DerefVoid (void* p)
    {
        return p == null ? -1 : *(int*)p;
    }

    private static int WriteThrough (int* p, int value)
    {
        *p = value;
        return 0;
    }

    private static int* Null ()
    {
        return null;
    }

    private static int* Identity (int* p)
    {
        return p;
    }

    private static int* AddressOfCell ()
    {
        fixed (int* p = &s_cell)
        {
            return p;
        }
    }

    private static int DerefReturned (int* p)
    {
        return p == null ? -1 : *p;
    }

    private static int DerefLong (long* p)
    {
        return p == null ? -1 : (int)*p;
    }

    private static int Twice (int x)
    {
        return 2 * x;
    }

    private static delegate*<int, int> GetTwice ()
    {
        return &Twice;
    }

    private static int CallThrough (delegate*<int, int> f, int x)
    {
        return f (x);
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

        if (Pointer.Unbox (returned) != null)
            return 4;

        fixed (int* cell = &s_cell)
        {
            // A non-null pointer passed as an `IntPtr`.
            object viaIntPtr = Get ("DerefFromIntPtr").Invoke (null, new object[] { (IntPtr)cell });

            if (!(viaIntPtr is int viaIntPtrValue) || viaIntPtrValue != 17)
                return 5;

            // A non-null pointer passed as a `System.Reflection.Pointer` of the parameter's own type.
            object viaBox = Get ("DerefFromPointerBox").Invoke (
                null,
                new object[] { Pointer.Box (cell, typeof (int*)) });

            if (!(viaBox is int viaBoxValue) || viaBoxValue != 17)
                return 6;

            // A `void*` parameter accepts a `Pointer` of any pointer type.
            object viaVoid = Get ("DerefVoid").Invoke (
                null,
                new object[] { Pointer.Box (cell, typeof (int*)) });

            if (!(viaVoid is int viaVoidValue) || viaVoidValue != 17)
                return 7;

            // The callee writes through the pointer it was handed, and the write lands in the
            // caller's storage.
            Get ("WriteThrough").Invoke (null, new object[] { (IntPtr)cell, 23 });

            if (s_cell != 23)
                return 8;

            // A pointer that goes out through `Invoke` and comes back as a `Pointer` still
            // addresses the same storage.
            object roundTripped = Get ("Identity").Invoke (
                null,
                new object[] { Pointer.Box (cell, typeof (int*)) });

            if (!(roundTripped is Pointer))
                return 9;

            int* back = (int*)Pointer.Unbox (roundTripped);

            if (back != cell || *back != 23)
                return 10;

            // The returned `Pointer` carries the pointer *type*, not the pointee type:
            // `CheckValue` reads it back, accepts it where an `int*` is expected...
            object again = Get ("DerefReturned").Invoke (null, new object[] { roundTripped });

            if (!(again is int againValue) || againValue != 23)
                return 11;

            // ... and refuses it where a `long*` is.
            try
            {
                Get ("DerefLong").Invoke (null, new object[] { roundTripped });
                return 12;
            }
            catch (ArgumentException)
            {
            }
        }

        // A pointer the callee produced itself.
        object produced = Get ("AddressOfCell").Invoke (null, null);

        if (!(produced is Pointer))
            return 13;

        if (*(int*)Pointer.Unbox (produced) != 23)
            return 14;

        // A function-pointer return is boxed as an `IntPtr`, and remains callable.
        object fnptr = Get ("GetTwice").Invoke (null, null);

        if (!(fnptr is IntPtr fnptrValue))
            return 15;

        if (((delegate*<int, int>)fnptrValue) (21) != 42)
            return 16;

        // A function-pointer argument arrives as an `IntPtr`.
        object called = Get ("CallThrough").Invoke (null, new object[] { fnptrValue, 5 });

        if (!(called is int calledValue) || calledValue != 10)
            return 17;

        return 0;
    }
}

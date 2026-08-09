using System;
using System.Reflection;

// `Exception.IsImmutableAgileException` is a private static, so reflection is the only way a
// guest can call it directly — and the only way to reach it with a null argument, which no
// CoreLib caller does (all three pass `this`).
//
// Null is not one of the three preallocated singletons, so the identity comparison in
// `CLRException::IsPreallocatedExceptionObject` (clrex.cpp:433) simply answers false. The
// `ASSERT(pExceptionUNSAFE != NULL)` above it (comutilnative.cpp:53) is debug-only and compiled
// out of the shipping runtime, so it is not a precondition a guest can violate. Verified against
// real .NET: this returns False rather than throwing.
class ImmutableAgileExceptionNullArgument
{
    static int Main(string[] args)
    {
        MethodInfo m = typeof(Exception).GetMethod(
            "IsImmutableAgileException",
            BindingFlags.NonPublic | BindingFlags.Static);

        if (m == null)
        {
            return 1;
        }

        object nullResult = m.Invoke(null, new object[] { null });

        if (!(nullResult is bool nullBool))
        {
            return 2;
        }

        if (nullBool)
        {
            return 3;
        }

        return 0;
    }
}

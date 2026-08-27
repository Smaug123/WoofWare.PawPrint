using System;
using System.Diagnostics;
using System.Reflection;

// A capture taken from inside a method of a *generic* type. Sibling of
// `StackTraceCurrentThreadFrames.cs`, which captures only from non-generic types; this one exists
// because that difference is exactly what decides whether the capture needs a QCall.
//
// CoreCLR's frame fill strips the *method* instantiation from each frame's MethodDesc and leaves
// the class instantiation alone (debugdebugger.cpp:449-452), so a frame on `Holder<int>.Capture`
// arrives at `StackFrameHelper.GetMethodBase` still bound to `int`. That answers false to
// `RuntimeMethodHandle.IsTypicalMethodDefinition`, so `GetTypicalMethodDefinition`
// (RuntimeHandles.cs:1291-1300) falls through to its
// `RuntimeMethodHandle_GetTypicalMethodDefinition` QCall, which loads `Holder<>.Capture` and
// allocates a fresh stub for it. Real .NET therefore reports the frame's declaring type as the
// generic type *definition*, which is what the checks below pin.
//
// PARKED on that QCall. Measured, not predicted; see the parking note in `TestPureCases.fs` for
// what PawPrint currently stops at.
class StackTraceGenericDeclaringFrame
{
    class Holder<T>
    {
        // Non-generic method, generic declaring type: the shape where CoreCLR's strip leaves an
        // instantiation behind. A generic *method* would be stripped and so would not exercise
        // the class-side half.
        internal static StackTrace Capture()
        {
            return new StackTrace();
        }
    }

    static int Main(string[] args)
    {
        StackTrace st = Holder<int>.Capture();

        if (st.FrameCount < 2)
        {
            return 1;
        }

        MethodBase first = st.GetFrame(0).GetMethod();

        if (first == null)
        {
            return 2;
        }

        if (!SameString(first.Name, "Capture"))
        {
            return 3;
        }

        Type declaring = first.DeclaringType;

        if (declaring == null)
        {
            return 4;
        }

        // The whole point: the reported method is the typical definition, so its declaring type is
        // `Holder<T>` and not `Holder<int>`.
        if (!declaring.IsGenericType)
        {
            return 5;
        }

        if (!declaring.IsGenericTypeDefinition)
        {
            return 6;
        }

        MethodBase second = st.GetFrame(1).GetMethod();

        if (second == null || !SameString(second.Name, "Main"))
        {
            return 7;
        }

        return 0;
    }

    // Hand-rolled rather than `string.Equals(a, b, StringComparison.Ordinal)`, which PawPrint
    // services as an unimplemented JIT intrinsic.
    static bool SameString(string a, string b)
    {
        if (a == null || b == null)
        {
            return false;
        }

        if (a.Length != b.Length)
        {
            return false;
        }

        for (int i = 0; i < a.Length; i++)
        {
            if (a[i] != b[i])
            {
                return false;
            }
        }

        return true;
    }
}

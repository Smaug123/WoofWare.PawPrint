using System;
using System.Diagnostics;
using System.Reflection;

// A current-thread capture with real frames in it: `new StackTrace()` from a known call chain.
//
// What this file pins is the one property PawPrint's frame walk leans on. Real .NET inlines
// CoreLib's own capture frames and PawPrint does not, so PawPrint's raw capture is several frames
// deeper -- seven `System.Diagnostics` frames when the walk was written, where real .NET has
// fewer. That is
// harmless only because `CalculateFramesToSkip` (StackTrace.CoreCLR.cs:18-44) skips the leading run
// of frames whose declaring type's namespace is *ordinal-equal* to "System.Diagnostics" and stops at
// the first that is not, so the extra frames are absorbed and the first reported frame is the same
// on both runtimes. Frame *counts* are therefore deliberately not asserted -- they legitimately
// differ -- but the identity and order of the reported frames are, which is what would go red if the
// walk started omitting CoreLib frames itself (making the skip run eat real guest frames) or
// reported them in the wrong order.
//
// Every frame this guest captures is declared on a non-generic type, so every one is already the
// typical method definition and `RuntimeMethodHandle.GetTypicalMethodDefinition` never reaches its
// QCall. `StackTraceGenericDeclaringFrame.cs` is the sibling that does, and is parked on it.
class StackTraceCurrentThreadFrames
{
    static StackTrace Innermost()
    {
        return new StackTrace();
    }

    static StackTrace Middle()
    {
        return Innermost();
    }

    static int Main(string[] args)
    {
        StackTrace st = Middle();

        // Every capture from inside a call chain has at least the three guest frames below it.
        if (st.FrameCount < 3)
        {
            return 1;
        }

        // The leading `System.Diagnostics` run must have been skipped entirely, leaving this
        // guest's own innermost frame first.
        MethodBase first = st.GetFrame(0).GetMethod();

        if (first == null)
        {
            return 2;
        }

        if (!SameString(first.Name, "Innermost"))
        {
            return 3;
        }

        if (first.DeclaringType == null || !SameString(first.DeclaringType.Name, "StackTraceCurrentThreadFrames"))
        {
            return 4;
        }

        // Innermost first, outward from there.
        MethodBase second = st.GetFrame(1).GetMethod();

        if (second == null || !SameString(second.Name, "Middle"))
        {
            return 5;
        }

        MethodBase third = st.GetFrame(2).GetMethod();

        if (third == null || !SameString(third.Name, "Main"))
        {
            return 6;
        }

        // Every *reported* frame is one of this guest's own, so every one has an IL body and a
        // real offset into it. A frame with no IL body reports `OFFSET_UNKNOWN` (-1) instead —
        // PawPrint's capture contains several such frames, the innermost being the P/Invoke stub
        // of the QCall itself — but all of them are inside the `System.Diagnostics` run that
        // `CalculateFramesToSkip` removes, so none survives to here. This check is what would
        // notice if one did.
        for (int i = 0; i < st.FrameCount; i++)
        {
            if (st.GetFrame(i).GetILOffset() < 0)
            {
                return 7;
            }
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

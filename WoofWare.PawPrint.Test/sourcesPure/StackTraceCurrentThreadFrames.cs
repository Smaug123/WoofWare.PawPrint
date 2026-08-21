using System;
using System.Diagnostics;
using System.Reflection;

// A current-thread capture with real frames in it: `new StackTrace()` from a known call chain.
//
// PARKED, blocked at an InternalCall one step past the QCall this file exercises. Measured, not
// predicted: with `StackTrace_GetStackFramesInternal` implemented, this guest stops at
// "Unimplemented native method (InternalCall): System.RuntimeMethodHandle::IsTypicalMethodDefinition
// (System.IRuntimeMethodInfo) -> System.Boolean".
//
// That is unavoidable for any capture with one or more frames: `CaptureStackTrace` builds a
// `StackFrame` for every captured frame before computing any skips (StackTrace.CoreCLR.cs:73-85),
// and the `StackFrame(StackFrameHelper, ...)` constructor calls `GetMethodBase` unconditionally
// (StackFrame.CoreCLR.cs:18), which reaches `RuntimeMethodHandle.GetTypicalMethodDefinition` and so
// that predicate. Un-park when `IsTypicalMethodDefinition` and its
// `RuntimeMethodHandle_GetTypicalMethodDefinition` QCall fallback both land — they belong together,
// because a frame on a method of a *generic* declaring type answers false to the predicate and
// genuinely needs the QCall.
//
// What this file is for, beyond un-parking: it pins the one property PawPrint's frame walk leans
// on. Real .NET inlines CoreLib's own capture frames and PawPrint does not, so PawPrint's raw
// capture is several frames deeper — measured as seven `System.Diagnostics` frames where real .NET
// has fewer. That is harmless only because `CalculateFramesToSkip` (StackTrace.CoreCLR.cs:18-44)
// skips the leading run of frames whose declaring type's namespace is *ordinal-equal* to
// "System.Diagnostics" and stops at the first that is not, so the extra frames are absorbed and
// the first reported frame is the same on both runtimes. Frame *counts* are therefore deliberately
// not asserted — they legitimately differ — but the identity and order of the reported frames are,
// which is what would go red if the walk started omitting CoreLib frames itself (making the skip
// run eat real guest frames) or reported them in the wrong order.
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

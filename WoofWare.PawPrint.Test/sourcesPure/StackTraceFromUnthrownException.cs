using System;
using System.Diagnostics;

// The zero-frame branch of the `StackTrace_GetStackFramesInternal` QCall: an exception that has
// never been thrown has no captured trace, so the QCall reports zero frames and — matching
// CoreCLR, which allocates nothing at all in this case (debugdebugger.cpp:331-334) — leaves every
// array field of the `StackFrameHelper` null. What this file pins is that `StackTrace` then behaves
// as real .NET does on those nulls rather than faulting on one.
//
// Deliberately the *single-argument* `StackTrace(Exception)` overload. It passes
// `fNeedFileInfo: false` (StackTrace.cs:81-86), and the `true` overloads are not reachable under
// PawPrint yet: `InitializeSourceInfo` calls `CreateStackTraceSymbols()` before looking at any
// frame, gated only on that flag, and it is an `[UnsafeAccessor]` constructor, whose dispatch is
// unimplemented. Zero frames does not protect against it —
// `sourcesPure/StackTraceFromExceptionNeedFileInfo.cs` is the parked guest for that shape.
//
// Every assertion here holds on both runtimes, measured. In particular `GetFrames()` answers an
// empty array and never null (StackTrace.cs:171-176), while `GetFrame` answers null; asserting the
// two the other way round would fail on real .NET too.
class StackTraceFromUnthrownException
{
    static int Main(string[] args)
    {
        Exception neverThrown = new Exception("never thrown");

        StackTrace st = new StackTrace(neverThrown);

        if (st.FrameCount != 0)
        {
            return 1;
        }

        StackFrame[] frames = st.GetFrames();

        if (frames == null)
        {
            return 2;
        }

        if (frames.Length != 0)
        {
            return 3;
        }

        // Out of range on an empty trace, which `GetFrame` answers with null rather than throwing.
        if (st.GetFrame(0) != null)
        {
            return 4;
        }

        // A negative index takes the same path, so it must not throw either.
        if (st.GetFrame(-1) != null)
        {
            return 5;
        }

        // Rendering an empty trace must not fault on the null arrays. The exact text is not
        // asserted: PawPrint omits the `in File:line` suffix real .NET emits with debug symbols,
        // and while an empty trace has no frame to carry one, pinning the string here would be
        // pinning a fact this file is not about.
        string rendered = st.ToString();

        if (rendered == null)
        {
            return 6;
        }

        // The same exception must still report no trace of its own, which is the state that made
        // the capture empty in the first place. A non-null `StackTrace` here would mean something
        // had recorded a throw that never happened.
        if (neverThrown.StackTrace != null)
        {
            return 7;
        }

        // Asking twice must give the same answer: the first capture must not have written anything
        // onto the exception that the second one would then find.
        if (new StackTrace(neverThrown).FrameCount != 0)
        {
            return 8;
        }

        return 0;
    }
}

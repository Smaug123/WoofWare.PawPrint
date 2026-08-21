using System;
using System.Diagnostics;

// `new StackTrace(exception, fNeedFileInfo: true)` on an exception with no captured trace, so the
// QCall reports zero frames and there is nothing whose source could be looked up.
//
// PARKED, and the blocker is *not* the frame count. `InitializeSourceInfo` calls
// `CreateStackTraceSymbols()` before the loop that walks frames, gated only on `fNeedFileInfo`
// (StackFrameHelper.cs:95-113), so zero frames does not avoid it. It is an `[UnsafeAccessor]`
// constructor into the `System.Diagnostics.StackTrace` assembly, and PawPrint's `[UnsafeAccessor]`
// dispatch is an unimplemented TODO (AbstractMachine.fs:395-402).
//
// The `try { } catch { }` CoreLib wraps that block in cannot save PawPrint here: it swallows a
// *guest* exception, which is how real .NET copes when `System.Diagnostics.StackTrace.dll` is
// absent, but a host-level refusal is not a guest exception and kills the run. So un-parking needs
// either `[UnsafeAccessor]` dispatch, or — cheaper and enough for this — an unresolvable
// `[UnsafeAccessor]` to raise a guest exception, at which point CoreLib's own catch absorbs it and
// the answers below are reached exactly as on real .NET.
//
// This matters beyond this file: `fNeedFileInfo: true` is what `Exception.StackTrace`'s
// `GetStackTrace()` passes (Exception.cs:232) and what `ExceptionDispatchInfo.SetCurrentStackTrace`
// passes (Exception.cs:247), so this is the blocker standing between those two and working. It is
// the third feature in the chain, after this QCall and the `GetTypicalMethodDefinition` pair.
//
// Verified to exit 0 on real .NET.
class StackTraceFromExceptionNeedFileInfo
{
    static int Main(string[] args)
    {
        Exception neverThrown = new Exception("never thrown");

        StackTrace st = new StackTrace(neverThrown, true);

        if (st.FrameCount != 0)
        {
            return 1;
        }

        if (st.GetFrames().Length != 0)
        {
            return 2;
        }

        if (st.GetFrame(0) != null)
        {
            return 3;
        }

        if (st.ToString() == null)
        {
            return 4;
        }

        return 0;
    }
}

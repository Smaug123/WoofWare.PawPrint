using System;
using System.Diagnostics;

// `new StackTrace(exception, fNeedFileInfo: true)` on an exception with no captured trace, so the
// QCall reports zero frames and there is nothing whose source could be looked up.
//
// Zero frames does not make the file information moot. `InitializeSourceInfo` calls
// `CreateStackTraceSymbols()` before the loop that walks frames, gated only on `fNeedFileInfo`
// (StackFrameHelper.cs:95-113). That is an `[UnsafeAccessor]` constructor whose return type is
// named by `[UnsafeAccessorType]` as `System.Diagnostics.StackTraceSymbols` in the
// `System.Diagnostics.StackTrace` assembly, which ships in the shared framework, so the name
// resolves (through `RuntimeAssembly.InternalLoad`) and a `StackTraceSymbols` is constructed.
//
// `fNeedFileInfo: true` is what `Exception.StackTrace`'s `GetStackTrace()` passes (Exception.cs:232)
// and what `ExceptionDispatchInfo.SetCurrentStackTrace` passes (Exception.cs:247).
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

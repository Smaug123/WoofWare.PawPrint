using System;
using System.Diagnostics;

// `new StackTrace(exception, fNeedFileInfo: true)` on an exception with no captured trace, so the
// QCall reports zero frames and there is nothing whose source could be looked up.
//
// PARKED, and the blocker is *not* the frame count. `InitializeSourceInfo` calls
// `CreateStackTraceSymbols()` before the loop that walks frames, gated only on `fNeedFileInfo`
// (StackFrameHelper.cs:95-113), so zero frames does not avoid it. It is an `[UnsafeAccessor]`
// constructor whose return type is named by `[UnsafeAccessorType]` as
// `System.Diagnostics.StackTraceSymbols` in the `System.Diagnostics.StackTrace` assembly, and
// PawPrint refuses any accessor that names a type that way (`UnsafeAccessorDispatch.resolve`).
//
// Raising that refusal as a guest exception, for the `try { } catch { }` CoreLib wraps the block in
// to absorb, would not match real .NET: that assembly ships in the shared framework, so real .NET
// resolves the name and constructs a `StackTraceSymbols`. Un-parking needs `[UnsafeAccessorType]`
// resolution. CoreCLR resolves the name through the managed `TypeNameResolver.GetTypeHelper`
// (vm/typeparse.cpp), which loads the named assembly with `RuntimeAssembly.InternalLoad`, so that
// in turn needs the `AssemblyNative_InternalLoad` QCall.
//
// This matters beyond this file: `fNeedFileInfo: true` is what `Exception.StackTrace`'s
// `GetStackTrace()` passes (Exception.cs:232) and what `ExceptionDispatchInfo.SetCurrentStackTrace`
// passes (Exception.cs:247), so this is the blocker standing between those two and working.
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

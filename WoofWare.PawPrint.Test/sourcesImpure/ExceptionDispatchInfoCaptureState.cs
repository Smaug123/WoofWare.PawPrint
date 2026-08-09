using System;
using System.Runtime.ExceptionServices;

// Companion to sourcesPure/ExceptionDispatchInfoCapture.cs. That one asserts the cross-runtime
// facts; this one exists so the host can assert PawPrint's own contract for
// `ExceptionNative_GetFrozenStackTrace`, which no exit code can express.
//
// The reason it must exist: every fact the differential test checks is *also* satisfied by a
// handler that writes null and does nothing else, because after a capture PawPrint's unwinder
// re-renders `_stackTraceString` anyway and no managed code ever decodes `_stackTrace`. So the
// differential test cannot tell an honest implementation from a stub. `TestImpureCases`
// inspects the terminal state instead: the captured dispatch state must hold the very token the
// unwind minted, and that token must map to the real frames.
//
// The captured `ExceptionDispatchInfo` is parked in a static so it is unambiguously still live,
// and so there is exactly one of them on the heap for the assertion to find.
public static class ExceptionDispatchInfoCaptureState
{
    public static ExceptionDispatchInfo Captured;

    static void Thrower()
    {
        throw new InvalidOperationException("boom");
    }

    public static int Main(string[] args)
    {
        try
        {
            Thrower();
            return 1;
        }
        catch (InvalidOperationException ex)
        {
            Captured = ExceptionDispatchInfo.Capture(ex);
        }

        return ReferenceEquals(Captured.SourceException, null) ? 2 : 0;
    }
}

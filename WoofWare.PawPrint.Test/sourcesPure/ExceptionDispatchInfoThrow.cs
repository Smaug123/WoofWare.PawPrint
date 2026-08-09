using System;
using System.Runtime.ExceptionServices;

// `ExceptionDispatchInfo.Throw()` — the other half of the capture/rethrow round trip begun in
// `ExceptionDispatchInfoCapture.cs`. It calls `Exception.RestoreDispatchState`, which needs both
// `IsImmutableAgileException` and `PrepareForForeignExceptionRaise`, so this is the file that
// says the round trip works end to end.
//
// Deliberately asserts identity and type, never trace *content*. Real .NET preserves the
// original frames across the rethrow and annotates the boundary with
// "--- End of stack trace from previous location ---"; PawPrint restarts the trace at the
// rethrow, because it does not yet consume the foreign-raise flag. That divergence is recorded
// in docs/divergences.md. Everything below holds on both runtimes.
class ExceptionDispatchInfoThrow
{
    static void Thrower()
    {
        throw new InvalidOperationException("boom");
    }

    static int Main(string[] args)
    {
        Exception original;

        try
        {
            Thrower();
            return 1;
        }
        catch (InvalidOperationException ex)
        {
            original = ex;
        }

        ExceptionDispatchInfo edi = ExceptionDispatchInfo.Capture(original);

        Exception rethrown;

        try
        {
            edi.Throw();
            return 2;
        }
        catch (InvalidOperationException ex)
        {
            rethrown = ex;
        }

        // The rethrow must hand back the very same object, not a copy.
        if (!ReferenceEquals(rethrown, original))
        {
            return 3;
        }

        if (rethrown.Message != "boom")
        {
            return 4;
        }

        // Having been rethrown, it is still a thrown exception: a trace is present, and storing a
        // remote one into it is refused. This is the `_stackTrace`/`_stackTraceString` state that
        // `RestoreDispatchState` leaves behind, observed without depending on frame content.
        if (rethrown.StackTrace == null)
        {
            return 5;
        }

        try
        {
            ExceptionDispatchInfo.SetRemoteStackTrace(rethrown, "SHOULD NOT APPLY");
            return 6;
        }
        catch (InvalidOperationException)
        {
        }

        // A second capture/rethrow of the same object must also work: `Throw()` is not one-shot.
        ExceptionDispatchInfo again = ExceptionDispatchInfo.Capture(rethrown);

        try
        {
            again.Throw();
            return 7;
        }
        catch (InvalidOperationException ex)
        {
            if (!ReferenceEquals(ex, original))
            {
                return 8;
            }
        }

        return 0;
    }
}

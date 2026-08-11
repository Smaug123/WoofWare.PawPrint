using System;
using System.Reflection;
using System.Runtime.ExceptionServices;

// `ForeignRaiseReadsCurrentExceptionTrace.cs` with the rethrow crossing a local `finally` on its
// way out of the method.
//
// The sibling establishes that a flag-consuming rethrow marks the boundary on the exception's
// *current* trace — the one the nested `ExceptionDispatchInfo.Throw()` left behind — rather than
// on the snapshot the enclosing handler was entered with. This file adds the one thing that can
// destroy that trace between the rethrow and the frame that consumes the flag: cleanup in the same
// method, entered while the raise is still in flight. Any write of the in-flight
// `CliException.StackTrace` back onto the exception object at that moment would be writing the
// stale snapshot, since a rethrow that has not yet unwound anything is still carrying it — and the
// consume, which happens later at the first appended frame, would then read the stale list and
// lose the nested boundary. The dispatcher projects at a search's conclusion rather than on the
// way into a clause, so no such write happens; this file is what says so.
//
// Measured on .NET 10 before being written: 2 boundaries, exactly as without the `finally`.
class ForeignRaiseTokenSurvivesLocalCleanup
{
    const string Boundary = "--- End of stack trace from previous location ---";

    static int CountBoundaries(string haystack)
    {
        int count = 0;

        for (int i = 0; i <= haystack.Length - Boundary.Length; i++)
        {
            bool matches = true;

            for (int j = 0; j < Boundary.Length; j++)
            {
                if (haystack[i + j] != Boundary[j])
                {
                    matches = false;
                    break;
                }
            }

            if (matches)
            {
                count++;
                i += Boundary.Length - 1;
            }
        }

        return count;
    }

    static bool finallyRan;

    static void Thrower()
    {
        throw new InvalidOperationException("boom");
    }

    static void NestedThrowThenRethrowThroughFinally(MethodInfo prepare)
    {
        try
        {
            try
            {
                Thrower();
            }
            catch (InvalidOperationException ex)
            {
                // Throw and catch the very same object again, from inside the handler, so that
                // `ex`'s own trace gains frames and a boundary that the enclosing handler's
                // snapshot does not have.
                try
                {
                    ExceptionDispatchInfo.Capture(ex).Throw();
                }
                catch (InvalidOperationException)
                {
                }

                prepare.Invoke(null, null);
                throw;
            }
        }
        finally
        {
            // The raise enters this handler before it unwinds to the caller, which is what makes
            // the write-back ordering observable.
            finallyRan = true;
        }
    }

    static int Main(string[] args)
    {
        MethodInfo prepare = typeof(Exception).GetMethod(
            "PrepareForForeignExceptionRaise",
            BindingFlags.NonPublic | BindingFlags.Static);

        if (prepare == null)
        {
            return 1;
        }

        string trace;

        try
        {
            NestedThrowThenRethrowThroughFinally(prepare);
            return 2;
        }
        catch (InvalidOperationException ex)
        {
            trace = ex.StackTrace;
        }

        if (trace == null)
        {
            return 3;
        }

        // Guards the premise: without the cleanup handler running there is no write-back to race.
        if (!finallyRan)
        {
            return 4;
        }

        // One boundary from the nested `ExceptionDispatchInfo.Throw()`, one from the rethrow that
        // consumed the flag. Reading a clobbered token yields 1.
        if (CountBoundaries(trace) != 2)
        {
            return 5;
        }

        return 0;
    }
}

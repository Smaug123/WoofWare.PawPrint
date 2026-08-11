using System;
using System.Reflection;

// `ForeignRaiseFlagSetInFinally.cs` with a `rethrow` in place of the `throw`.
//
// The `throw` version is served by the throw itself consuming at its seed frame, so nothing is
// left to be confused by the `finally`. A `rethrow` appends no frame at initiation, so its answer
// is decided later — at whichever frame boundary it first crosses. Here there is no flag when the
// rethrow begins and the `finally` sets one, so the rethrown exception gets no boundary and the
// flag is still pending afterwards.
//
// Together with `ForeignRaiseFlagPendingBeforeCleanup.cs` — same shape, flag set one statement
// earlier — this pins *when* the flag is read relative to cleanup. Both are satisfied by reading
// the thread's bit at every append, provided the appends all happen before any cleanup runs, which
// is what the first pass guarantees; an interleaved dispatcher that reached its append only after
// running the `finally` would swap both answers.
//
// Measured on .NET 10 before being written: 0 boundaries, then 1.
class ForeignRaiseFlagSetInFinallyDuringRethrow
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

    // No flag is pending when the `rethrow` runs; the `finally` it unwinds through sets one.
    static void RethrowThenFinallySetsFlag(MethodInfo prepare)
    {
        try
        {
            try
            {
                Thrower();
            }
            catch (InvalidOperationException)
            {
                throw;
            }
        }
        finally
        {
            finallyRan = true;
            prepare.Invoke(null, null);
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

        Exception rethrown;

        try
        {
            RethrowThenFinallySetsFlag(prepare);
            return 2;
        }
        catch (InvalidOperationException ex)
        {
            rethrown = ex;
        }

        // Guards the premise: without the `finally` running there is no flag and nothing to test.
        if (!finallyRan)
        {
            return 3;
        }

        string afterRethrow = rethrown.StackTrace;

        if (afterRethrow == null)
        {
            return 4;
        }

        // The rethrow began with nothing pending, so the flag the `finally` set is not its to take.
        if (CountBoundaries(afterRethrow) != 0)
        {
            return 5;
        }

        // It belongs to the next raise instead.
        string afterThrow;

        try
        {
            throw rethrown;
        }
        catch (InvalidOperationException ex)
        {
            afterThrow = ex.StackTrace;
        }

        if (afterThrow == null)
        {
            return 6;
        }

        if (CountBoundaries(afterThrow) != 1)
        {
            return 7;
        }

        return 0;
    }
}

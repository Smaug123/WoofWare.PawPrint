using System;
using System.Reflection;

// A foreign-raise flag set from a `finally` cannot be consumed by the exception that is unwinding
// through that `finally`.
//
// CoreCLR appends every frame of a raise during pass one, *before* running any cleanup clause
// (`StackTraceInfo::AppendElement` is a pass-one activity). By the time a `finally` runs, the
// raise has no appends left, so the flag it sets survives to be spent by the *next* raise. The
// unwinding exception gets no boundary at all.
//
// This is where PawPrint's single-pass dispatcher — which interleaves handler search with cleanup
// rather than completing a search pass first — has to be told explicitly what CoreCLR gets from
// its pass structure: the resume-after-`finally` path is not eligible to consume a flag. Without
// that, the boundary lands on the unwinding exception (which must have none) and the next raise is
// left with nothing (when it must have one) — both wrong at once, which is what makes this file
// worth its length.
//
// Measured on .NET 10 before being written: 0 boundaries, then 1.
class ForeignRaiseFlagSetInFinally
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

    // Throws, and sets the flag on the way out. The exception still has a caller frame (`Main`)
    // to be appended after the `finally` runs, which is exactly the frame a naive implementation
    // would hang the boundary on.
    static void ThrowThroughFinally(MethodInfo prepare)
    {
        try
        {
            throw new InvalidOperationException("boom");
        }
        finally
        {
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

        Exception unwound;

        try
        {
            ThrowThroughFinally(prepare);
            return 2;
        }
        catch (InvalidOperationException ex)
        {
            unwound = ex;
        }

        string afterUnwind = unwound.StackTrace;

        if (afterUnwind == null)
        {
            return 3;
        }

        // The raise that ran the `finally` had already recorded its frames; nothing of its own is
        // left to mark.
        if (CountBoundaries(afterUnwind) != 0)
        {
            return 4;
        }

        // So the flag is still pending, and the next raise spends it.
        string afterThrow;

        try
        {
            throw unwound;
        }
        catch (InvalidOperationException ex)
        {
            afterThrow = ex.StackTrace;
        }

        if (afterThrow == null)
        {
            return 5;
        }

        if (CountBoundaries(afterThrow) != 1)
        {
            return 6;
        }

        return 0;
    }
}

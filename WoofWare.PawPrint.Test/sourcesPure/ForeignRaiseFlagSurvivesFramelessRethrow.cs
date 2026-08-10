using System;
using System.Reflection;

// The foreign-raise flag is consumed when a *frame is appended*, not when a raise begins.
//
// CoreCLR's read-and-reset lives in `StackTraceInfo::AppendElement` (excep.cpp:3016-3017). A
// `rethrow` caught by another clause in the same method appends no frame at all — nothing has been
// unwound — so `AppendElement` never runs, no boundary is recorded, and the flag is still pending
// for whatever raises next. `ForeignRaiseFlagConsumedByRethrow.cs` covers the other half: a
// rethrow that *does* unwind consumes the flag and marks a boundary.
//
// Together the two files pin the placement. A runtime that consumed the flag at the `rethrow`
// instruction rather than at the append would pass that one and fail this one, in both of its
// assertions at once: it would put a boundary on a trace that must have none, and then have
// nothing left for the throw that must have one.
//
// Measured on .NET 10 before being written: 0 boundaries, then 1.
class ForeignRaiseFlagSurvivesFramelessRethrow
{
    const string Boundary = "--- End of stack trace from previous location ---";

    static int IndexOf(string haystack, string needle)
    {
        for (int i = 0; i <= haystack.Length - needle.Length; i++)
        {
            bool matches = true;

            for (int j = 0; j < needle.Length; j++)
            {
                if (haystack[i + j] != needle[j])
                {
                    matches = false;
                    break;
                }
            }

            if (matches)
            {
                return i;
            }
        }

        return -1;
    }

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

    static void Thrower()
    {
        throw new InvalidOperationException("boom");
    }

    // The rethrow's handler is the outer clause of this same method, so the exception is never
    // unwound out of `FramelessRethrow` and no frame is appended for the rethrow.
    static Exception FramelessRethrow(MethodInfo prepare)
    {
        try
        {
            try
            {
                Thrower();
            }
            catch (InvalidOperationException)
            {
                prepare.Invoke(null, null);
                throw;
            }
        }
        catch (InvalidOperationException caught)
        {
            return caught;
        }

        return null;
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

        Exception rethrown = FramelessRethrow(prepare);

        if (rethrown == null)
        {
            return 2;
        }

        string afterRethrow = rethrown.StackTrace;

        if (afterRethrow == null)
        {
            return 3;
        }

        // No frame was appended, so nothing consumed the flag and nothing was marked.
        if (CountBoundaries(afterRethrow) != 0)
        {
            return 4;
        }

        // The flag is therefore still pending, and the next raise — an ordinary `throw` of this
        // already-thrown object — is what spends it.
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
            return 5;
        }

        if (CountBoundaries(afterThrow) != 1)
        {
            return 6;
        }

        // ...and it spends it the usual way: the frames the exception was already carrying come
        // back with it, above the boundary, rather than the trace restarting at the throw.
        int thrower = IndexOf(afterThrow, "Thrower");

        if (thrower < 0)
        {
            return 7;
        }

        if (IndexOf(afterThrow, Boundary) < thrower)
        {
            return 8;
        }

        return 0;
    }
}

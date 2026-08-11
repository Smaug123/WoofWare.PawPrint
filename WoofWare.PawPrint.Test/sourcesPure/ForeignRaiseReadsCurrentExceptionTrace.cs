using System;
using System.Reflection;
using System.Runtime.ExceptionServices;

// When the flag is consumed, the frames it marks are read off the *exception object*, not off
// whatever list the in-flight raise happens to be carrying.
//
// CoreCLR re-reads `_stackTrace` from the throwable at every `StackTraceInfo::AppendElement`
// (excep.cpp:3080), so the trace it extends is always the current one. That is observable when the
// same exception object is thrown again from inside a handler that caught it: the nested throw
// records new frames and a new boundary onto the object, while the outer handler's own view of the
// trace was fixed when it was entered. A `rethrow` afterwards must continue the object's updated
// trace, keeping the nested boundary and adding one of its own.
//
// An implementation that marked the handler's stale snapshot instead would lose the nested
// boundary and report one where real .NET reports two.
//
// Measured on .NET 10 before being written: 2.
class ForeignRaiseReadsCurrentExceptionTrace
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

    static void Thrower()
    {
        throw new InvalidOperationException("boom");
    }

    static void NestedThrowThenRethrow(MethodInfo prepare)
    {
        try
        {
            Thrower();
        }
        catch (InvalidOperationException ex)
        {
            // Throw and catch the very same object again, from inside the handler. This appends
            // frames and a boundary to `ex`'s own trace; the enclosing handler's view of that
            // trace was taken when it was entered and does not see them.
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
            NestedThrowThenRethrow(prepare);
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

        // One boundary from the nested `ExceptionDispatchInfo.Throw()`, one from the rethrow that
        // consumed the flag. Marking a stale snapshot yields 1.
        if (CountBoundaries(trace) != 2)
        {
            return 4;
        }

        return 0;
    }
}

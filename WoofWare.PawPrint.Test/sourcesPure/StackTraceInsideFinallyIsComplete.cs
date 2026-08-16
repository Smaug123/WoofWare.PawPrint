using System;

class CompleteTraceBoom : Exception
{
}

// The stack trace an exception carries while a `finally` runs during its propagation.
//
// The CLR dispatches in two passes: the first walks frames from the throw point outward,
// appending a trace frame per boundary crossed, and stops at the frame whose `catch` accepts;
// only then does the second pass unwind, running cleanup. So by the time any `finally` body
// runs, the trace is already complete — and complete means "up to and including the frame that
// owns the handler", not "the whole stack".
//
// Measured on .NET 10 before this was written: inside `Cleaner`'s `finally`, the trace is
// `Thrower`, `Cleaner`, `Handler` — three frames. `Caller`, which called `Handler`, is absent,
// as is `Main`.
//
// Both halves are asserted, and both matter. An implementation that interleaves search with
// cleanup enters this `finally` having unwound only as far as `Cleaner` and reports two frames;
// one that records the entire call stack instead is just as wrong, and the `Caller`/`Main`
// checks are what catch it.
class StackTraceInsideFinallyIsComplete
{
    static bool ContainsSubstring(string haystack, string needle)
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
                return true;
            }
        }

        return false;
    }

    static readonly CompleteTraceBoom Boom = new CompleteTraceBoom();

    static string TraceInFinally;
    static int caught;

    static void Thrower()
    {
        throw Boom;
    }

    static void Cleaner()
    {
        try
        {
            Thrower();
        }
        finally
        {
            TraceInFinally = Boom.StackTrace;
        }
    }

    static void Handler()
    {
        try
        {
            Cleaner();
        }
        catch (CompleteTraceBoom)
        {
            caught = 1;
        }
    }

    static void Caller()
    {
        Handler();
    }

    static int Main(string[] args)
    {
        Caller();

        // Guards the premise: the exception really was caught where this test thinks it was.
        if (caught != 1)
        {
            return 1;
        }

        if (TraceInFinally == null)
        {
            return 2;
        }

        if (!ContainsSubstring(TraceInFinally, "StackTraceInsideFinallyIsComplete.Thrower"))
        {
            return 3;
        }

        if (!ContainsSubstring(TraceInFinally, "StackTraceInsideFinallyIsComplete.Cleaner"))
        {
            return 4;
        }

        // The handler-owning frame is in the trace. It is the frame an interleaving
        // implementation misses: the first pass has reached it, but cleanup runs before that
        // is recorded.
        if (!ContainsSubstring(TraceInFinally, "StackTraceInsideFinallyIsComplete.Handler"))
        {
            return 5;
        }

        // The search stopped at `Handler`, so nothing outside it is in the trace.
        if (ContainsSubstring(TraceInFinally, "StackTraceInsideFinallyIsComplete.Caller"))
        {
            return 6;
        }

        if (ContainsSubstring(TraceInFinally, "StackTraceInsideFinallyIsComplete.Main"))
        {
            return 7;
        }

        return 0;
    }
}

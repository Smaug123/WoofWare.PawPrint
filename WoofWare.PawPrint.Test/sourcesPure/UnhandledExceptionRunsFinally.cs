using System;

class UnhandledBoom : Exception
{
}

// An exception that no frame handles still unwinds, and still runs every `finally` on the way
// out — and the trace it carries while they run is complete.
//
// Measured on .NET 10 before this was written. With a `finally` between the throw point and the
// top of the stack and nothing catching, the observed order is: the runtime's `Unhandled
// exception.` banner carrying the *full* trace, then the `finally` body, then abort. A
// `Environment.Exit` from inside that `finally` wins, and the process exits with its code.
//
// So the first pass runs to completion and freezes the trace even when its verdict is "nobody
// handles this", and only then does the second pass unwind. This file pins both halves at once:
// that cleanup runs at all on the unhandled path (PawPrint would otherwise terminate at the
// throw point, and `Environment.Exit` would never be reached, making this an unhandled-exception
// case that the fixture would reject), and that the trace the clause reads names every frame
// including `Main`'s.
//
// Exiting from the `finally` is what makes any of this observable. Left to terminate, both
// runtimes report an unhandled exception and the fixture compares nothing but that fact, so no
// assertion inside the clause could fail the test.
//
// Success is exit *zero*, unusually for this fixture, and that is forced by the oracle rather
// than a stylistic choice: real .NET prints its `Unhandled exception.` banner to stderr before
// running the clause, and `RealRuntime.executeWithRealRuntime` classifies a run as
// `UnhandledException` when the banner is present *and* the exit code is non-zero. A success code
// of 7 would therefore be read as a crash on the real side while PawPrint reported a clean exit,
// and the two would be compared as different kinds of outcome rather than as codes. Exiting 0 on
// success keeps both sides on the `NormalExit` path; every failure code below is non-zero and
// surfaces as a plain code mismatch against the real runtime's 0.
class UnhandledExceptionRunsFinally
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

    static readonly UnhandledBoom Boom = new UnhandledBoom();

    static void Thrower()
    {
        throw Boom;
    }

    static int TraceVerdict()
    {
        string trace = Boom.StackTrace;

        if (trace == null)
        {
            return 2;
        }

        if (!ContainsSubstring(trace, "UnhandledExceptionRunsFinally.Thrower"))
        {
            return 3;
        }

        if (!ContainsSubstring(trace, "UnhandledExceptionRunsFinally.Cleaner"))
        {
            return 4;
        }

        // Nothing catches, so the first pass ran off the end of the stack — and recorded every
        // frame it crossed on the way, `Main` included.
        if (!ContainsSubstring(trace, "UnhandledExceptionRunsFinally.Main"))
        {
            return 5;
        }

        return 0;
    }

    static void Cleaner()
    {
        try
        {
            Thrower();
        }
        finally
        {
            Environment.Exit(TraceVerdict());
        }
    }

    static int Main(string[] args)
    {
        Cleaner();

        // Unreachable: the `finally` above always exits the process.
        return 1;
    }
}

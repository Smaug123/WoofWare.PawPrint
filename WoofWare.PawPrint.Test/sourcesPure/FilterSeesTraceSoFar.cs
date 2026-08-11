using System;

class FilterTraceBoom : Exception
{
}

// What a `when` clause can read off the exception while the first pass is still running.
//
// The CLR appends stack-trace frames as the first pass walks outward, so by the time a filter
// body executes the exception already carries every frame from the throw point up to and
// including the filter's own. Measured on .NET 10 before this was written: three frames,
// `Thrower`, `Middle`, `Outer` — and `Outer` is the frame hosting the `when`.
//
// This is a regression guard rather than a fix. PawPrint projected the partial trace on filter
// entry before it had two passes at all, and the obvious way to restructure it — project only
// once, when the search reaches a verdict — would silently move filter entry to *before* any
// projection, so a `when` clause would read `StackTrace == null` on an exception that has
// genuinely been thrown, and `Exception.HasBeenThrown` (which keys off the same frozen token)
// would answer false.
class FilterSeesTraceSoFar
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

    static readonly FilterTraceBoom Boom = new FilterTraceBoom();

    static string TraceInFilter;
    static int caught;

    static void Thrower()
    {
        throw Boom;
    }

    static void Middle()
    {
        Thrower();
    }

    static bool Filter()
    {
        TraceInFilter = Boom.StackTrace;
        return true;
    }

    static void Outer()
    {
        try
        {
            Middle();
        }
        catch (FilterTraceBoom) when (Filter())
        {
            caught = 1;
        }
    }

    static int Main(string[] args)
    {
        Outer();

        // Guards the premise: the filter ran and accepted.
        if (caught != 1)
        {
            return 1;
        }

        if (TraceInFilter == null)
        {
            return 2;
        }

        if (!ContainsSubstring(TraceInFilter, "FilterSeesTraceSoFar.Thrower"))
        {
            return 3;
        }

        if (!ContainsSubstring(TraceInFilter, "FilterSeesTraceSoFar.Middle"))
        {
            return 4;
        }

        // The frame hosting the filter has already been appended by the time its body runs.
        if (!ContainsSubstring(TraceInFilter, "FilterSeesTraceSoFar.Outer"))
        {
            return 5;
        }

        // The search has got no further than `Outer`, so `Main` is not there yet.
        if (ContainsSubstring(TraceInFilter, "FilterSeesTraceSoFar.Main"))
        {
            return 6;
        }

        return 0;
    }
}

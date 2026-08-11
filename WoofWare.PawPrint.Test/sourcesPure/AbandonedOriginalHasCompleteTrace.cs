using System;

class AbandonedOriginal : Exception
{
}

class AbandonedReplacement : Exception
{
}

// An exception displaced by a `finally` that throws keeps the trace its own dispatch had already
// built.
//
// The first pass for the original runs to a verdict — here, the `catch (AbandonedOriginal)` in
// `Handler` — and freezes the whole trace onto the object *before* the second pass unwinds. The
// `finally` then throws, so the original never reaches that handler; but it has already been
// dispatched as far as `Handler`, and the trace it is left holding says so.
//
// Measured on .NET 10 before this was written: the original carries `Thrower`, `Cleaner`,
// `Handler` — three frames, ending at the frame whose `catch` it was on its way to. PawPrint
// used to freeze the trace at the moment it entered the `finally`, so the original was left with
// two.
//
// This is the one shape where the restructure changes the *content* of a trace rather than only
// when it is written, so it is worth pinning explicitly rather than leaving to the tests that
// cover concluded dispatches.
class AbandonedOriginalHasCompleteTrace
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

    static readonly AbandonedOriginal Original = new AbandonedOriginal();
    static readonly AbandonedReplacement Replacement = new AbandonedReplacement();

    static int caught;

    static void Thrower()
    {
        throw Original;
    }

    static void Cleaner()
    {
        try
        {
            Thrower();
        }
        finally
        {
            throw Replacement;
        }
    }

    static void Handler()
    {
        try
        {
            Cleaner();
        }
        catch (AbandonedOriginal)
        {
            caught = 1;
        }
        catch (AbandonedReplacement)
        {
            caught = 2;
        }
    }

    static int Main(string[] args)
    {
        Handler();

        // Guards the premise: the replacement really did displace the original, so the original
        // never reached the handler its first pass had picked out.
        if (caught != 2)
        {
            return 1;
        }

        string trace = Original.StackTrace;

        if (trace == null)
        {
            return 2;
        }

        if (!ContainsSubstring(trace, "AbandonedOriginalHasCompleteTrace.Thrower"))
        {
            return 3;
        }

        if (!ContainsSubstring(trace, "AbandonedOriginalHasCompleteTrace.Cleaner"))
        {
            return 4;
        }

        // The frame the original was headed for, recorded before the `finally` displaced it.
        if (!ContainsSubstring(trace, "AbandonedOriginalHasCompleteTrace.Handler"))
        {
            return 5;
        }

        if (ContainsSubstring(trace, "AbandonedOriginalHasCompleteTrace.Main"))
        {
            return 6;
        }

        return 0;
    }
}

using System;
using System.Reflection;

// The mirror image of `ForeignRaiseFlagSetInFinally.cs`, and the reason the eligibility to consume
// a foreign-raise flag is a property of the *raise* rather than of the resume site.
//
// There, the flag was set by a `finally` running during an unwind, and the unwinding exception must
// not take it. Here the flag is set *before* the raise begins — a `rethrow` — and that raise then
// passes through a `finally` on its way out of the method. CoreCLR consumes the flag when the
// rethrow appends its caller frame in pass one, which happens before the `finally` body runs at
// all, so the boundary lands on the rethrown exception and nothing is left over.
//
// The two files differ only in *when* the flag is set: both end up resuming propagation from an
// `endfinally`. A rule that decided eligibility by looking at the resume site would therefore have
// to give both the same answer, and would get one of them wrong whichever it chose.
//
// Measured on .NET 10 before being written: 1 boundary, then 0.
class ForeignRaiseFlagPendingBeforeCleanup
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

    static bool finallyRan;

    static void Thrower()
    {
        throw new InvalidOperationException("boom");
    }

    // The flag is set before the `rethrow`; the rethrow then has to run this method's `finally`
    // before it can reach the caller frame it will append.
    static void RethrowThroughFinally(MethodInfo prepare)
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
        finally
        {
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

        Exception rethrown;

        try
        {
            RethrowThroughFinally(prepare);
            return 2;
        }
        catch (InvalidOperationException ex)
        {
            rethrown = ex;
        }

        // Guards the premise: if the `finally` never ran, the raise did not pass through cleanup
        // and the file is testing nothing.
        if (!finallyRan)
        {
            return 3;
        }

        string afterRethrow = rethrown.StackTrace;

        if (afterRethrow == null)
        {
            return 4;
        }

        // The flag predated the raise, so the raise took it — the intervening `finally` changes
        // nothing.
        if (CountBoundaries(afterRethrow) != 1)
        {
            return 5;
        }

        int thrower = IndexOf(afterRethrow, "Thrower");

        if (thrower < 0)
        {
            return 6;
        }

        if (IndexOf(afterRethrow, Boundary) < thrower)
        {
            return 7;
        }

        // And nothing is left pending for the next raise.
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
            return 8;
        }

        if (CountBoundaries(afterThrow) != 0)
        {
            return 9;
        }

        return 0;
    }
}

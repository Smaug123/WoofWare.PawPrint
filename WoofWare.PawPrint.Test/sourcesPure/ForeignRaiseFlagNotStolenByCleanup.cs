using System;
using System.Reflection;

// A flag pending when a raise begins cannot be stolen by that raise's own cleanup.
//
// CoreCLR appends every stack-trace frame in its first pass, so a raise has consumed any pending
// foreign-raise flag before a single `finally` runs; a raise started by guest code inside that
// `finally` finds nothing pending. Measured on .NET 10:
//
//     exception raised in the `finally`       0 boundaries
//     outer exception after the rethrow       1 boundary
//     an ordinary throw afterwards            0 boundaries
//
// The exception thrown in the `finally` is one that has already been thrown once, which is what
// makes a theft visible at all: a flag consumed there would splice that exception's earlier
// frames back on and mark the boundary between them, giving 1 rather than 0. Thrown fresh, it
// would have nothing to mark and the theft would show up only in the outer count, one step
// removed from its cause.
//
// Issue #865: a dispatcher that interleaves search with cleanup — recording at raise
// initiation whether a flag was pending and re-reading the thread's bit at a delayed append —
// leaves a window in which a `finally` can move the bit. A real first pass closes the window
// structurally: there is no point between a raise and its appends at which guest cleanup can
// run.
class ForeignRaiseFlagNotStolenByCleanup
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

    static int innerBoundaries = -1;

    static void Thrower()
    {
        throw new InvalidOperationException("boom");
    }

    static void InnerThrower()
    {
        throw new ArgumentException("inner");
    }

    static void RethrowWithFlagAndThrowingFinally(MethodInfo prepare, Exception alreadyThrown)
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
            // A complete raise of its own, inside the outer raise's cleanup — and deliberately of
            // an exception that has *already* been thrown once. That is what makes a stolen flag
            // visible: a flag consumed here would splice this exception's earlier frames back on
            // and mark the boundary between them, so the count below is 1 rather than 0.
            try
            {
                throw alreadyThrown;
            }
            catch (ArgumentException inner)
            {
                innerBoundaries = inner.StackTrace == null ? -1 : CountBoundaries(inner.StackTrace);
            }
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

        // Thrown and caught once up front, so that it carries frames the `finally` below could
        // splice back on if it managed to consume the outer raise's flag.
        Exception alreadyThrown;

        try
        {
            InnerThrower();
            return 2;
        }
        catch (ArgumentException ex)
        {
            alreadyThrown = ex;
        }

        Exception rethrown;

        try
        {
            RethrowWithFlagAndThrowingFinally(prepare, alreadyThrown);
            return 3;
        }
        catch (InvalidOperationException ex)
        {
            rethrown = ex;
        }

        if (innerBoundaries < 0)
        {
            return 4;
        }

        // The cause, checked first: the raise inside the `finally` must not have taken the flag
        // that the outer raise had already claimed.
        if (innerBoundaries != 0)
        {
            return 5;
        }

        string afterRethrow = rethrown.StackTrace;

        if (afterRethrow == null)
        {
            return 6;
        }

        // The consequence: the outer raise still gets its boundary.
        if (CountBoundaries(afterRethrow) != 1)
        {
            return 7;
        }

        // And nothing is left pending.
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

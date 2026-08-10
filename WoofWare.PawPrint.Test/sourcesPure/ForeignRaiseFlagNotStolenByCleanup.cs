using System;
using System.Reflection;

// A flag pending when a raise begins must be *reserved* for that raise, not merely earmarked.
//
// PawPrint records at raise initiation whether a flag was pending, and re-reads the thread's bit
// when the raise finally appends a frame. Between those two moments the raise can run a `finally`,
// and guest code there can move the bit: throwing and catching an exception inside the `finally`
// starts a raise of its own, which consumes the flag the outer raise had already claimed.
//
// CoreCLR has no such window. It appends every frame in pass one, so the outer raise has taken the
// flag before any cleanup clause runs; the raise inside the `finally` finds nothing pending.
// Measured on .NET 10, and PawPrint's answers beside them:
//
//                                        real .NET   PawPrint
//     exception raised in the `finally`       0          1
//     outer exception after the rethrow       1          0
//     an ordinary throw afterwards            0          0
//
// The exception thrown in the `finally` is one that has already been thrown once, which is what
// makes the theft visible at all: a flag consumed there splices that exception's earlier frames
// back on and marks them. Thrown fresh, it would have nothing to mark and PawPrint would answer 0
// there too — the theft would show up only in the outer count, one step removed from its cause.
//
// Closing this means transferring ownership of the flag at raise initiation and handing it back if
// the raise turns out to append nothing — and "turns out to append nothing" is only answerable once
// dispatch knows a cleanup handler from a real one, which is the `_isFinally` that
// `tryFindAndEnterHandlerAtSearchPC` deliberately ignores today. That is issue #865's two-pass
// structure, so this file is parked on it rather than approximated.
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

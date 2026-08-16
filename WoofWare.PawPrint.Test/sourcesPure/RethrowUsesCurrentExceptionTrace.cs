using System;

// A `rethrow` continues the exception's own trace, not the one the catch handler was entered with.
//
// The two diverge as soon as the same object is thrown again from inside the handler: that raise
// replaces `_stackTrace`, while the enclosing handler still holds the list it was entered with.
// Real .NET's `IL_Rethrow` reaches dispatch without clearing `_stackTrace`, so the rethrow
// accumulates onto the *newer* trace and the nested frame survives; carrying the snapshot instead
// resurrects the pre-nesting frames and drops the nested one.
//
// Measured on .NET 10 before being written: the final trace names `NestedThrower` and does not
// name `Thrower` — the nested `throw ex` reset the trace, and the rethrow continued from there.
//
// The `finally` matters. Entering any handler records the in-flight frame list back onto the
// exception, so for a rethrow that has not yet unwound anything, a snapshot-carrying
// implementation does not merely *read* the stale list, it *writes* it over the good one. Without
// the `finally` the damage is confined to the raise; with it, the exception object itself is left
// holding the older trace.
class RethrowUsesCurrentExceptionTrace
{
    static bool finallyRan;

    static bool Contains(string haystack, string needle)
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

    static void Thrower()
    {
        throw new InvalidOperationException("boom");
    }

    static void NestedThrower(Exception e)
    {
        throw e;
    }

    static void PlainRethrowThroughFinally()
    {
        try
        {
            try
            {
                Thrower();
            }
            catch (InvalidOperationException ex)
            {
                // Throw the very same object again, from inside the handler. This replaces `ex`'s
                // trace; the enclosing handler's snapshot does not see the replacement.
                try
                {
                    NestedThrower(ex);
                }
                catch (InvalidOperationException)
                {
                }

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
        string trace;

        try
        {
            PlainRethrowThroughFinally();
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

        // Guards the premise: without the cleanup handler running there is no write-back at all.
        if (!finallyRan)
        {
            return 4;
        }

        // The nested throw is the most recent thing to have happened to this exception, so its
        // frame is in the trace the rethrow continued.
        if (!Contains(trace, ".NestedThrower("))
        {
            return 5;
        }

        // And the frames from before the nested throw are gone, because that throw reset the
        // trace. Asserted as well as the above so that an implementation which *concatenates* the
        // snapshot and the token — passing the check above for the wrong reason — still fails.
        // Both runtimes render a frame as `at Type.Method(...)`; without the leading dot this
        // would also match `NestedThrower`.
        if (Contains(trace, ".Thrower("))
        {
            return 6;
        }

        return 0;
    }
}

using System;

class Inner : Exception
{
}

class FromFilter : Exception
{
}

class Later : Exception
{
}

// An exception that escapes a `when` filter is discarded: ECMA-335 III.3.34 says it "is
// intercepted and a value of exception_continue_search is returned", so the filter counts as
// false and the search for the original exception carries on. A `catch` that encloses the whole
// try/catch-when construct never sees the escaping exception, even when it names that
// exception's type. Roslyn does lay the filter's IL out inside the enclosing `try`, but the
// filter runs as its own funclet, and anything leaving the funclet dies at its boundary
// (`ExceptionHandling.cs`: `catch when (true) { // Prevent leaking any exception from the filter
// funclet }`).
//
// Each scenario returns 0 on the runtime's behaviour and a distinct code for each wrong outcome;
// `Main` offsets those so a failure names the scenario. Measured on .NET 10 before being written.
class ExceptionEscapingFilterIsNotCaughtByEnclosingCatch
{
    static int finallyRuns;

    // Thrown from a method rather than inline so that the compiler cannot prove the code after
    // each `try` unreachable.
    static void ThrowInner()
    {
        throw new Inner();
    }

    static bool ThrowFromFilter()
    {
        throw new FromFilter();
    }

    // The escaping exception is raised in a callee of the filter, and the enclosing `catch`
    // names its exact type.
    static int EscapeViaCallee()
    {
        try
        {
            try
            {
                ThrowInner();
            }
            catch (Inner) when (ThrowFromFilter())
            {
                return 1;
            }
        }
        catch (FromFilter)
        {
            return 2;
        }
        catch (Inner)
        {
            return 0;
        }

        return 3;
    }

    // The escaping exception is a `throw` expression in the filter body itself, so the raise
    // begins in the frame that owns the filter. The enclosing catch-all comes after a `catch` of
    // the original exception's type, which is the one that must win.
    static int EscapeInBody()
    {
        try
        {
            try
            {
                ThrowInner();
            }
            catch (Inner) when (finallyRuns < 0 ? true : throw new FromFilter())
            {
                return 1;
            }
        }
        catch (Inner)
        {
            return 0;
        }
        catch (Exception)
        {
            return 2;
        }

        return 3;
    }

    // The frame keeps running after the filter has been abandoned: the sibling `catch` handles
    // the original exception, the enclosing `finally` runs once, and a later exception raised in
    // the same frame is dispatched normally rather than being treated as another escape from the
    // long-finished filter.
    static int FrameStaysUsableAfterEscape()
    {
        int outcome = 3;

        try
        {
            try
            {
                ThrowInner();
            }
            catch (Inner) when (ThrowFromFilter())
            {
                outcome = 1;
            }
            catch (Inner)
            {
                outcome = 0;
            }
        }
        catch (FromFilter)
        {
            outcome = 2;
        }
        finally
        {
            finallyRuns++;
        }

        try
        {
            throw new Later();
        }
        catch (Later)
        {
            return outcome;
        }
    }

    // Returns only if the filter accepts; otherwise the original exception leaves the frame.
    static int FilterHost()
    {
        try
        {
            ThrowInner();
        }
        catch (Inner) when (ThrowFromFilter())
        {
            return 1;
        }

        return 3;
    }

    // The enclosing `catch` is in the caller's frame rather than the filter's own: it is not a
    // candidate either, and what reaches the caller is the original exception.
    static int EscapeWithCatchInCaller()
    {
        try
        {
            return FilterHost();
        }
        catch (FromFilter)
        {
            return 2;
        }
        catch (Inner)
        {
            return 0;
        }
    }

    static int Main(string[] args)
    {
        int result = EscapeViaCallee();
        if (result != 0)
        {
            return 10 + result;
        }

        result = EscapeInBody();
        if (result != 0)
        {
            return 20 + result;
        }

        result = FrameStaysUsableAfterEscape();
        if (result != 0)
        {
            return 30 + result;
        }

        if (finallyRuns != 1)
        {
            return 40 + finallyRuns;
        }

        result = EscapeWithCatchInCaller();
        if (result != 0)
        {
            return 50 + result;
        }

        return 0;
    }
}

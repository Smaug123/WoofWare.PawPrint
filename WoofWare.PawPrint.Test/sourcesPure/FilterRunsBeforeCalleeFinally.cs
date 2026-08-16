using System;

class FilterOrderBoom : Exception
{
}

// The order in which an outer frame's `filter` and an inner frame's `finally` run.
//
// The CLR's first pass runs every `filter` from the throw point outward *before* its second pass
// unwinds and runs any `finally`. So a `when` clause in the caller is evaluated while the callee
// frame is still live and its `finally` has not yet executed.
//
// Measured on .NET 10 before this was written: filter, then callee `finally`, then catch body.
// A dispatcher that interleaves the two passes — popping the callee frame and running its
// `finally` before reaching the caller's filter — produces `finally`, filter, catch body.
//
// The ordering is recorded as three ints rather than a string so that the assertion involves no
// string comparison of its own, and so a wrong order is reported as a distinct exit code rather
// than collapsing into a single failure.
class FilterRunsBeforeCalleeFinally
{
    static int step;

    static int filterAt;
    static int finallyAt;
    static int catchAt;

    static void Inner()
    {
        try
        {
            throw new FilterOrderBoom();
        }
        finally
        {
            step = step + 1;
            finallyAt = step;
        }
    }

    static bool Filter()
    {
        step = step + 1;
        filterAt = step;
        return true;
    }

    static void Outer()
    {
        try
        {
            Inner();
        }
        catch (FilterOrderBoom) when (Filter())
        {
            step = step + 1;
            catchAt = step;
        }
    }

    static int Main(string[] args)
    {
        Outer();

        // Guards the premise: all three really ran, exactly once each.
        if (step != 3)
        {
            return 1;
        }

        if (filterAt != 1)
        {
            return 2;
        }

        if (finallyAt != 2)
        {
            return 3;
        }

        if (catchAt != 3)
        {
            return 4;
        }

        return 0;
    }
}

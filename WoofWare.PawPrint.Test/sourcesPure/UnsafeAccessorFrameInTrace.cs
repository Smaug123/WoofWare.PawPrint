using System;
using System.Runtime.CompilerServices;

// An accessor is a real declared method, and it is on the stack while its target runs: an exception
// out of the target unwinds through it. Measured on real .NET 10, the trace of a throwing target
// reads `Thrower.Boom -> Program.Boom -> Program.Access -> ...`, with the accessor between the two.
//
// This is the whole reason the accessor's frame is returned when the target returns rather than
// before. It also pins the ordinary consequences of keeping it: the target's return value still
// reaches the accessor's caller, and a `finally` in between still runs.
public class TestUnsafeAccessorFrameInTrace
{
    private class Target
    {
        private int _value = 5;

        private int Boom() => throw new InvalidOperationException("boom");

        private int Fine(int x) => _value + x;
    }

    [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "Boom")]
    private static extern int Boom(Target t);

    [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "Fine")]
    private static extern int Fine(Target t, int x);

    private static int Access(Target t) => Boom(t);

    private static int Run()
    {
        Target t = new Target();

        // The ordinary path: the value crosses the accessor's frame to its caller.
        if (Fine(t, 2) != 7) return 1;

        int finallyRuns = 0;

        try
        {
            try
            {
                Access(t);
                return 2;
            }
            finally
            {
                finallyRuns++;
            }
        }
        catch (InvalidOperationException e)
        {
            if (e.Message != "boom") return 3;

            string trace = e.StackTrace ?? "";

            // The accessor's own frame is named, between the target and whoever called it.
            int target = trace.IndexOf("Target.Boom", StringComparison.Ordinal);
            int accessor = trace.IndexOf("TestUnsafeAccessorFrameInTrace.Boom", StringComparison.Ordinal);
            int caller = trace.IndexOf("TestUnsafeAccessorFrameInTrace.Access", StringComparison.Ordinal);

            if (target < 0) return 4;
            if (accessor < 0) return 5;
            if (caller < 0) return 6;
            if (!(target < accessor && accessor < caller)) return 7;
        }

        if (finallyRuns != 1) return 8;

        // The accessor is reusable after an exception crossed it.
        if (Fine(t, 3) != 8) return 9;

        return 0;
    }

    public static int Main() => Run();
}

using System;
using System.Threading;

// A lease is taken out for 100ms; the code then waits for a worker for at most
// 50ms and uses the lease afterwards.
//
// The bug is the reasoning, not any one line: "I wait at most 50ms against a
// 100ms lease, so the lease is still live when the wait returns." That is sound
// if and only if a 50ms wait takes 50ms. A wait returns when the scheduler gets
// round to it, which is at the deadline *plus* however long the thread sat
// runnable-but-not-running -- and that overshoot is unbounded in principle and
// routinely milliseconds in practice.
//
// This is a strictly harder target than JoinTimeoutIgnored.cs. There, the bug
// needed a timeout to fire at all, which `EagerDeadlines` reaches by moving the
// clock onto a pending deadline. Here the timeout firing is not enough: the
// guest handles that case correctly, in the sense that it never reads unwritten
// data. What has to go wrong is that *more time passed than was asked for*, so
// it takes a non-zero `maxOvershootTicks`. Measured: 0 of 64 seeds at a zero
// bound, 32 of 64 once the bound exceeds the gap between the wait and the lease.
public static class Entry
{
    private const int Sentinel = 42;

    public static int Main()
    {
        var worker = new Thread(() =>
        {
            for (int i = 0; i < 4000; i++)
            {
                Thread.SpinWait(1);
            }
        });

        worker.Start();

        long leaseExpiry = Environment.TickCount64 + 100;

        // Deliberately half the lease: the author left themselves what looks
        // like a 2x margin.
        worker.Join(50);

        if (Environment.TickCount64 >= leaseExpiry)
        {
            // Used a lease that had already expired: the invariant this guest
            // exists to violate, reported as a sentinel so the host can see it.
            return Sentinel;
        }

        return 0;
    }
}

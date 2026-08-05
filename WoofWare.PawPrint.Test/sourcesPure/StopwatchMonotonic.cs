using System;
using System.Diagnostics;

// End-to-end coverage of SystemNative_GetTimestamp, the PAL entry behind
// Stopwatch.GetTimestamp on Unix. PawPrint derives it from the same deterministic virtual
// clock that backs Environment.TickCount64, so what is assertable here is the shape of a
// monotonic clock -- ordering and non-negativity -- rather than any particular rate.
//
// Every assertion below is straight-line or bounded by a plain counter. Nothing loops on a
// clock predicate: PawPrint's clock advances a millisecond per retired IL instruction, so
// merely reading it twice burns milliseconds, and a clock-conditioned loop can be false on
// its first evaluation. SpinWaitDelayLoop.cs was vacuous under PawPrint for exactly that
// reason before it was restructured -- it ran zero iterations here and 28 on the real
// runtime -- so a test in this area has to be readable as covering the handler *without*
// depending on how fast the clock happens to move.
public static class StopwatchMonotonic
{
    public static int Main (string[] args)
    {
        // Stopwatch.Frequency is not read from the PAL on Unix: Stopwatch.Unix.cs returns
        // the literal 1e9 from GetFrequency(). Asserting it pins the *unit* of the handler
        // end-to-end, which is the one thing about this primitive that would be silently
        // wrong rather than loudly wrong if we picked it incorrectly.
        if (Stopwatch.Frequency != 1_000_000_000L)
        {
            return 1;
        }

        if (!Stopwatch.IsHighResolution)
        {
            return 2;
        }

        long first = Stopwatch.GetTimestamp ();
        long second = Stopwatch.GetTimestamp ();

        // The defining property. Deliberately `<`, not `<=`: real hardware can return the
        // same tick twice, so requiring strict progress would be a genuine race there.
        if (second < first)
        {
            return 3;
        }

        if (Stopwatch.GetElapsedTime (first, second) < TimeSpan.Zero)
        {
            return 4;
        }

        // Bounded by the counter, never by the clock, so the body always runs.
        long previous = second;
        for (int i = 0; i < 32; i++)
        {
            long current = Stopwatch.GetTimestamp ();
            if (current < previous)
            {
                return 5;
            }

            previous = current;
        }

        Stopwatch watch = Stopwatch.StartNew ();
        int accumulator = 0;
        for (int i = 0; i < 64; i++)
        {
            accumulator += i;
        }

        watch.Stop ();

        if (accumulator != 2016)
        {
            return 6;
        }

        // No upper bound and no `> TimeSpan.Zero`: PawPrint bills this loop tens of
        // milliseconds where real hardware bills it nanoseconds, so any assertion sharp
        // enough to see a difference would have to fail on one runtime or the other, and
        // the harness diffs exit codes between the two.
        if (watch.Elapsed < TimeSpan.Zero || watch.ElapsedTicks < 0 || watch.ElapsedMilliseconds < 0)
        {
            return 7;
        }

        if (watch.IsRunning)
        {
            return 8;
        }

        // A stopped Stopwatch holds its reading still: Elapsed stops consulting the clock
        // once IsRunning is false, so two reads agree even though the clock has moved
        // between them (and under PawPrint it certainly has -- reading it once costs
        // milliseconds).
        TimeSpan held = watch.Elapsed;
        if (watch.Elapsed != held)
        {
            return 9;
        }

        return 0;
    }
}

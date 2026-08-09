using System;
using System.Diagnostics;

namespace HelloWorldApp
{
    class Program
    {
        static int Main(string[] args)
        {
            // PawPrint's high-resolution clock is the virtual clock scaled to
            // nanoseconds, so it boots at zero. Both that and the granularity
            // asserted below are part of the replay contract, and neither can
            // be pinned by the sibling pure case `StopwatchElapsed.cs`, which
            // is cross-checked against the real runtime (whose CLOCK_MONOTONIC
            // counts from an unspecified origin at nanosecond resolution).
            //
            // Note what the whole-millisecond assertions below actually pin.
            // The clock itself counts 100 ns ticks and can represent any of
            // them; readings come out as whole milliseconds only because
            // `EmulatedKernel.instructionCostTicks` currently charges exactly
            // one millisecond per retired instruction, so every reachable
            // value is a multiple of 10,000 ticks. These assertions therefore
            // track the *rate*, not the unit, and are expected to change when
            // the rate does. The same caveat applies with more force to the
            // containment check at the end of this file: `TickCount64 * 1e6`
            // lying inside a hi-res interval is a theorem only while readings
            // are millisecond-granular — at a finer rate the millisecond
            // reading floors below `before`.

            // On Unix `Stopwatch.Frequency` is a hard-coded 1e9, which is what
            // fixes our unit as the nanosecond.
            if (Stopwatch.Frequency != 1_000_000_000L) return 1;

            long first = Stopwatch.GetTimestamp();

            if (first % 100L != 0) return 2;

            // The virtual clock advances from zero at a fixed cost per scheduler
            // tick, so by the time the guest reaches here the reading is positive
            // but nowhere near a day's worth of simulated uptime. The generous
            // headroom means this cannot be tripped merely by the interpreter
            // taking more steps than it does today.
            if (first <= 0) return 3;
            if (first >= 86_400_000L * 1_000_000L) return 4;

            // Executing IL advances the clock, so a later reading is strictly
            // later: elapsed-time polling loops in guest code terminate.
            long second = Stopwatch.GetTimestamp();
            if (second <= first) return 5;
            if (second % 100L != 0) return 6;

            // `Environment.TickCount64` (SystemNative_GetLowResolutionTimestamp)
            // and `Stopwatch` (SystemNative_GetTimestamp) are two views of one
            // clock, exactly as they are upstream. Read the low-resolution one
            // between two high-resolution ones and its *millisecond bucket* must
            // overlap the interval they bracket.
            //
            // Not "must fall between them", which is what this used to say. That
            // was a theorem only while the clock was millisecond-granular and the
            // low-resolution view was therefore exact; now it truncates, so the
            // reading generally floors to strictly below `before`. Expanding it
            // to the millisecond it stands for is the honest statement, and it is
            // still enough to catch the failures worth catching: the two entry
            // points reading different clocks, or disagreeing by more than the
            // low-resolution quantum.
            long before = Stopwatch.GetTimestamp();
            long ticks = Environment.TickCount64;
            long after = Stopwatch.GetTimestamp();

            if (ticks * 1_000_000L > after) return 7;
            if (ticks * 1_000_000L + 1_000_000L <= before) return 8;

            return 0;
        }
    }
}

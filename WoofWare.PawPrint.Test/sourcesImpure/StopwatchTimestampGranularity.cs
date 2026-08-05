using System;
using System.Diagnostics;

namespace HelloWorldApp
{
    class Program
    {
        static int Main(string[] args)
        {
            // PawPrint's high-resolution clock is the virtual clock scaled to
            // nanoseconds, so it boots at zero and only ever takes values that
            // are whole milliseconds. Both are part of the replay contract, and
            // neither can be pinned by the sibling pure case
            // `StopwatchElapsed.cs`, which is cross-checked against the real
            // runtime (whose CLOCK_MONOTONIC counts from an unspecified origin
            // at nanosecond resolution).

            // On Unix `Stopwatch.Frequency` is a hard-coded 1e9, which is what
            // fixes our unit as the nanosecond.
            if (Stopwatch.Frequency != 1_000_000_000L) return 1;

            long first = Stopwatch.GetTimestamp();

            if (first % 1_000_000L != 0) return 2;

            // The virtual clock advances 1ms per scheduler tick from zero, so
            // by the time the guest reaches here the reading is positive but
            // nowhere near a day's worth of simulated uptime. The generous
            // headroom means this cannot be tripped merely by the interpreter
            // taking more steps than it does today.
            if (first <= 0) return 3;
            if (first >= 86_400_000L * 1_000_000L) return 4;

            // Executing IL advances the clock, so a later reading is strictly
            // later: elapsed-time polling loops in guest code terminate.
            long second = Stopwatch.GetTimestamp();
            if (second <= first) return 5;
            if (second % 1_000_000L != 0) return 6;

            // `Environment.TickCount64` (SystemNative_GetLowResolutionTimestamp)
            // and `Stopwatch` (SystemNative_GetTimestamp) are two views of one
            // clock, exactly as they are upstream. Read the low-resolution one
            // between two high-resolution ones and it must fall between them.
            long before = Stopwatch.GetTimestamp();
            long ticks = Environment.TickCount64;
            long after = Stopwatch.GetTimestamp();

            if (ticks * 1_000_000L < before) return 7;
            if (ticks * 1_000_000L > after) return 8;

            return 0;
        }
    }
}

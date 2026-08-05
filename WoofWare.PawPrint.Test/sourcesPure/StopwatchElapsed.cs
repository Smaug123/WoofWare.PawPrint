using System;
using System.Diagnostics;

// Exercises `Stopwatch` end to end. On a Unix CoreLib, `Stopwatch.GetTimestamp`
// is `Interop.Sys.GetTimestamp` -> the `SystemNative_GetTimestamp` PAL entry
// point, which is `minipal_hires_ticks()`: CLOCK_MONOTONIC in nanoseconds.
//
// This is a *pure* case, so it is differentially compared against the real
// runtime running in-process. It may therefore only assert facts that hold on
// both: the origin, granularity, and rate of PawPrint's clock are pinned by the
// sibling impure case `StopwatchTimestampGranularity.cs`, not here.
//
// Note that `Stopwatch` scales raw timestamps to `TimeSpan` ticks through a
// `double` (`s_tickFrequency`), so nothing here uses integer arithmetic as an
// oracle for that conversion — the assertions below rely only on the
// conversion being monotone and zero-preserving.
class Program
{
    static int Main(string[] args)
    {
        long first = Stopwatch.GetTimestamp();

        Stopwatch sw = Stopwatch.StartNew();
        long acc = 0;
        for (int i = 0; i < 200; i++)
        {
            acc += i;
        }
        sw.Stop();

        long second = Stopwatch.GetTimestamp();

        // Guard against the loop being optimised into nothing, so the
        // measurement below spans real work on both runtimes.
        if (acc != 19900) return 1;

        // `Stopwatch.GetFrequency()` is a positive constant on every platform
        // (1e9 on Unix, QueryPerformanceFrequency on Windows); code divides by
        // it, so zero or negative would be a live bug.
        if (Stopwatch.Frequency <= 0) return 2;

        // CLOCK_MONOTONIC never runs backwards, and PawPrint's virtual clock is
        // likewise monotonic. This is the property every elapsed-time
        // computation in the BCL rests on.
        if (second < first) return 3;

        if (sw.IsRunning) return 4;
        if (sw.ElapsedTicks < 0) return 5;
        if (sw.Elapsed < TimeSpan.Zero) return 6;

        // A stopped stopwatch has frozen: repeated reads agree.
        long firstRead = sw.ElapsedTicks;
        long secondRead = sw.ElapsedTicks;
        if (firstRead != secondRead) return 7;

        // The stopwatch was started after `first` was read and stopped before
        // `second` was read, so the interval it measured is contained in
        // [first, second]. This ties the object's bookkeeping to the raw PAL
        // readings: a `GetTimestamp` that returned an unrelated clock (or a
        // constant that made the two readings inconsistent with the object's)
        // would break it.
        if (sw.ElapsedTicks > second - first) return 8;

        // The raw-to-`TimeSpan` scaling is monotone, so the containment above
        // survives the conversion.
        if (sw.Elapsed > Stopwatch.GetElapsedTime(first, second)) return 9;

        // ... and a zero-length interval scales to zero.
        if (Stopwatch.GetElapsedTime(first, first) != TimeSpan.Zero) return 10;

        return 0;
    }
}

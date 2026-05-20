using System;

namespace HelloWorldApp
{
    class Program
    {
        static int Main(string[] args)
        {
            // Environment.TickCount64 on Unix calls SystemNative_GetLowResolutionTimestamp.
            // Two properties any reasonable monotonic clock must satisfy:
            //   1. consecutive reads never decrease;
            //   2. a tight polling loop eventually sees the clock advance, so guest
            //      code that waits "until N ms have elapsed" terminates.
            // We assert advance-by-at-least-1ms rather than a larger threshold because
            // real .NET on a fast machine can poll TickCount64 fast enough that a
            // 100k-iteration loop spans only a millisecond or two of wall clock —
            // anything tighter than ">= 1" makes the test JIT-speed-fragile without
            // adding signal. The failure modes we care about (handler missing, clock
            // frozen, clock non-monotonic) all still trip this.
            long start = Environment.TickCount64;
            long previous = start;
            const int maxIterations = 10_000_000;

            for (int iterations = 0; ; iterations++)
            {
                long current = Environment.TickCount64;
                if (current < previous)
                {
                    return 1;
                }
                previous = current;

                if (current - start >= 1)
                {
                    return 0;
                }

                if (iterations >= maxIterations)
                {
                    return 2;
                }
            }
        }
    }
}

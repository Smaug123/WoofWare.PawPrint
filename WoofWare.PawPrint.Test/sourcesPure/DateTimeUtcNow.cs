using System;

namespace HelloWorldApp
{
    class Program
    {
        static int Main(string[] args)
        {
            // DateTime.UtcNow on Unix calls SystemNative_GetSystemTimeAsTicks, which
            // returns 100ns ticks since the Unix epoch; CoreLib adds UnixEpochTicks and
            // stamps DateTimeKind.Utc. The assertions here are invariants that hold on
            // both the real runtime and PawPrint's deterministic virtual clock, because
            // this test is cross-checked against the real runtime's exit code. In
            // particular we must not assert anything about the *absolute* date: PawPrint
            // deliberately boots its wall clock at the Unix epoch, so it reports 1970
            // where the real runtime reports today.
            DateTime start = DateTime.UtcNow;

            if (start.Kind != DateTimeKind.Utc)
            {
                return 1;
            }

            // The PAL contract is "ticks since the Unix epoch", so the value CoreLib
            // constructs is at least UnixEpochTicks. Any clock that satisfies this is
            // representable as a DateTime; a negative or wildly out-of-range tick count
            // would have thrown out of the DateTime ctor before we got here.
            if (start.Year < 1970)
            {
                return 2;
            }

            // Two properties any reasonable wall clock must satisfy in the absence of an
            // NTP step: consecutive reads never go backwards, and a tight polling loop
            // eventually observes the clock advance. As with EnvironmentTickCount64.cs we
            // only demand *some* advance rather than a specific magnitude, so the test is
            // not fragile to how fast the real runtime spins through the loop.
            long previous = start.Ticks;
            const int maxIterations = 10_000_000;

            for (int iterations = 0; ; iterations++)
            {
                long current = DateTime.UtcNow.Ticks;
                if (current < previous)
                {
                    return 3;
                }

                if (current > start.Ticks)
                {
                    return 0;
                }

                previous = current;

                if (iterations >= maxIterations)
                {
                    return 4;
                }
            }
        }
    }
}

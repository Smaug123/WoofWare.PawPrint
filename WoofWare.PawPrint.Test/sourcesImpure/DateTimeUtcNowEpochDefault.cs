using System;

namespace HelloWorldApp
{
    class Program
    {
        static int Main(string[] args)
        {
            // Default KernelConfig: the simulated process boots with its wall
            // clock at the Unix epoch, so DateTime.UtcNow reports 1970. This is
            // part of PawPrint's replay contract, not an accident, so pin it —
            // the pure test cannot, because it is cross-checked against the real
            // runtime, which of course reports today.
            //
            // The virtual clock advances 1ms per scheduler step from zero, so a
            // day of headroom means this cannot be tripped merely by the
            // interpreter taking more steps than it does today.
            DateTime now = DateTime.UtcNow;

            if (now.Kind != DateTimeKind.Utc)
            {
                return 1;
            }

            if (now < DateTime.UnixEpoch)
            {
                return 2;
            }

            if ((now - DateTime.UnixEpoch).Ticks >= TimeSpan.TicksPerDay)
            {
                return 3;
            }

            if (now.Year != 1970 || now.Month != 1 || now.Day != 1)
            {
                return 4;
            }

            return 0;
        }
    }
}

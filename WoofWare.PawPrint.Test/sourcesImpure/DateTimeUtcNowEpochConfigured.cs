using System;

namespace HelloWorldApp
{
    class Program
    {
        static int Main(string[] args)
        {
            // The host sets KernelConfig.WallClockEpochMs = 1_699_920_000_000
            // (2023-11-14T00:00:00Z) for this test, so DateTime.UtcNow must land
            // in [that instant, that instant + 1 day). The whole chain is under
            // test here: KernelConfig -> withWallClockEpochMs -> the kernel field
            // -> systemTimeAsTicks -> SystemNative_GetSystemTimeAsTicks ->
            // CoreLib's UnixEpochTicks offset.
            DateTime configured = new DateTime(2023, 11, 14, 0, 0, 0, DateTimeKind.Utc);
            DateTime now = DateTime.UtcNow;

            if (now.Kind != DateTimeKind.Utc)
            {
                return 1;
            }

            if (now < configured)
            {
                return 2;
            }

            if ((now - configured).Ticks >= TimeSpan.TicksPerDay)
            {
                return 3;
            }

            return 0;
        }
    }
}

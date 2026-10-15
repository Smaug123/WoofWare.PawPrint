using System;
using System.Threading;

// Once Main has returned, `ThreadStore::WaitForOtherThreads` makes the entry thread background,
// marks it `TS_ReportDead` (which `GetSnapshotState` reports as dead) and waits alertably, so a
// worker holding its `Thread` object reads Background | Stopped | WaitSleepJoin. The wait is
// entered last, so the worker waits for Stopped and then gives WaitSleepJoin a bounded number of
// polls to appear. Measured on real .NET 10: exit code 11. Real .NET 11 (11.0.0-rc.1) reports
// Background | Stopped and never WaitSleepJoin, measured: exit code 12.
//
// The worker reports through `Environment.ExitCode`, and Main is `void`, for the reasons given
// in ForegroundThreadExitsAfterMainReturns.cs.
class ThreadStateOfEntryThreadAfterMain
{
    static void Main()
    {
        Thread main = Thread.CurrentThread;

        new Thread(() =>
        {
            ThreadState s = main.ThreadState;
            while ((s & ThreadState.Stopped) == 0)
            {
                Thread.Sleep(1);
                s = main.ThreadState;
            }

            for (int i = 0; i < 1000 && (s & ThreadState.WaitSleepJoin) == 0; i++)
            {
                Thread.Sleep(1);
                s = main.ThreadState;
            }

            if (s == (ThreadState.Background | ThreadState.Stopped | ThreadState.WaitSleepJoin))
                Environment.ExitCode = 11;
            else if (s == (ThreadState.Background | ThreadState.Stopped))
                Environment.ExitCode = 12;
            else
                Environment.ExitCode = 1;
        }).Start();
    }
}

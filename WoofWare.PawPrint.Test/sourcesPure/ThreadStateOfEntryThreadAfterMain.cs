using System;
using System.Threading;

// Once Main has returned, `ThreadStore::WaitForOtherThreads` makes the entry thread background,
// marks it `TS_ReportDead` (which `GetSnapshotState` reports as dead) and waits alertably, so a
// worker holding its `Thread` object reads Background | Stopped | WaitSleepJoin. The wait is
// entered last, so once WaitSleepJoin is visible the whole snapshot is. Measured on real
// .NET 10: exit code 11.
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
            while ((s & ThreadState.WaitSleepJoin) == 0)
            {
                Thread.Sleep(1);
                s = main.ThreadState;
            }

            Environment.ExitCode =
                s == (ThreadState.Background | ThreadState.Stopped | ThreadState.WaitSleepJoin) ? 11 : 1;
        }).Start();
    }
}

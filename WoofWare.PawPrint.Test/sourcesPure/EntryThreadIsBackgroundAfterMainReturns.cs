using System;
using System.Threading;

// `ThreadStore::WaitForOtherThreads`, which the entry thread blocks in once Main has
// returned, begins by making that thread background ("this simplifies our rules for
// counting non-background threads"), and the flip is visible to a worker holding the
// `Thread` object. Measured on real .NET 10: exit code 11.
//
// A runtime that left the entry thread's flag alone would spin here for ever.
//
// The worker reports through `Environment.ExitCode` rather than `Environment.Exit`,
// and Main is `void`, for the reasons given in ForegroundThreadExitsAfterMainReturns.cs:
// an `Exit` on a worker after an `int Main` has returned races the entry thread's own
// exit path.
class EntryThreadIsBackgroundAfterMainReturns
{
    static void Main()
    {
        Thread main = Thread.CurrentThread;
        if (main.IsBackground)
        {
            Environment.ExitCode = 1;
            return;
        }

        new Thread(() =>
        {
            while (!main.IsBackground)
            {
                Thread.Sleep(1);
            }

            Environment.ExitCode = 11;
        }).Start();
    }
}

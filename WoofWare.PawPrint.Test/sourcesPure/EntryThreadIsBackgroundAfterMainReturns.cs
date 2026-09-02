using System;
using System.Threading;

// `ThreadStore::WaitForOtherThreads`, which the entry thread blocks in once Main has
// returned, begins by making that thread background ("this simplifies our rules for
// counting non-background threads"), and the flip is visible to a worker holding the
// `Thread` object. Measured on real .NET 10: prints `main became background`, exit 11.
//
// A runtime that left the entry thread's flag alone would spin here for ever.
class EntryThreadIsBackgroundAfterMainReturns
{
    static int Main()
    {
        Thread main = Thread.CurrentThread;
        if (main.IsBackground) return 1;

        new Thread(() =>
        {
            while (!main.IsBackground)
            {
                Thread.Sleep(1);
            }

            Environment.Exit(11);
        }).Start();

        return 3;
    }
}

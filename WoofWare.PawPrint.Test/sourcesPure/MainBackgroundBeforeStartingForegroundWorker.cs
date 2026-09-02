using System;
using System.Threading;

// CoreCLR's termination event is latched: `CheckForEEShutdown` sets it the moment no
// foreground thread is alive, and nothing resets it. Main making itself background while
// it is the only started thread is such a moment, so the foreground worker started
// afterwards is abandoned when Main returns: `WaitForOtherThreads` finds the event already
// set and the process exits at once. Measured on real .NET 10, ten runs of ten: exit 3;
// the worker's `Environment.Exit(7)` never runs.
//
// A runtime that decided from the thread table alone, when Main returns, would wait for
// the worker and exit 7.
class MainBackgroundBeforeStartingForegroundWorker
{
    static int Main()
    {
        Thread.CurrentThread.IsBackground = true;
        new Thread(() =>
        {
            Thread.Sleep(200);
            Environment.Exit(7);
        }).Start();
        return 3;
    }
}

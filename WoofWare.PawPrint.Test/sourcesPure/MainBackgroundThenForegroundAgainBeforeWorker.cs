using System;
using System.Threading;

// The latched half of `MainBackgroundBeforeStartingForegroundWorker`: the termination
// event set while Main was momentarily the only thread and background is not reset by
// Main becoming foreground again, so the worker started afterwards is still abandoned
// when Main returns. Measured on real .NET 10, ten runs of ten: exit 3.
//
// A runtime that recomputed "any foreground thread alive?" when Main returns, rather than
// remembering that the answer was once no, would wait for the worker and exit 7.
class MainBackgroundThenForegroundAgainBeforeWorker
{
    static int Main()
    {
        Thread.CurrentThread.IsBackground = true;
        Thread.CurrentThread.IsBackground = false;
        new Thread(() =>
        {
            Thread.Sleep(200);
            Environment.Exit(7);
        }).Start();
        return 3;
    }
}

using System.Threading;

// The latched half of `MainBackgroundBeforeStartingForegroundWorker`: the termination
// event set while Main was momentarily the only thread and background is not reset by
// Main becoming foreground again, so the worker started afterwards is still abandoned
// when Main returns. Measured on real .NET 10: exit code 3 (40/40 on a macOS host, 64/64
// in a Linux container with eight instances on one CPU).
//
// A runtime that recomputed "any foreground thread alive?" when Main returns, rather than
// remembering that the answer was once no, would wait for the worker for ever. The worker
// sleeps for ever rather than exiting after a delay for the reason given in the sibling
// guest: a timed exit races the host's own.
class MainBackgroundThenForegroundAgainBeforeWorker
{
    static int Main()
    {
        Thread.CurrentThread.IsBackground = true;
        Thread.CurrentThread.IsBackground = false;
        new Thread(() => Thread.Sleep(Timeout.Infinite)).Start();
        return 3;
    }
}

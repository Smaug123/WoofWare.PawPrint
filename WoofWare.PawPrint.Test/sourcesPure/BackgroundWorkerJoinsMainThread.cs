using System.Threading;

// The entry thread does not die when Main returns; it waits for the other foreground
// threads (CoreCLR's `WaitForOtherThreads`). A `Join` on it therefore never returns:
// with the worker marked background there is nothing to wait for once Main returns, so
// the process ends with Main's exit code while the worker is still parked in the Join.
// Measured on real .NET 10: exit code 4.
//
// The same guest with a *foreground* worker hangs on real .NET (measured: still running
// after five minutes), because the worker waits for the entry thread and the entry
// thread waits for the worker.
class BackgroundWorkerJoinsMainThread
{
    static int Main()
    {
        Thread main = Thread.CurrentThread;
        new Thread(() => main.Join()) { IsBackground = true }.Start();
        return 4;
    }
}

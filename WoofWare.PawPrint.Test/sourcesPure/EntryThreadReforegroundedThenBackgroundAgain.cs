using System.Threading;

// The waiting entry thread's background flag is an ordinary flag a worker may rewrite.
// Cleared, it makes the entry thread count as a foreground thread again and the process
// would never exit (measured on real .NET 10: still running after three seconds); set
// again, the count is back where `WaitForOtherThreads` left it and the process ends with
// Main's exit code once the worker finishes. Measured on real .NET 10: exit 3.
class EntryThreadReforegroundedThenBackgroundAgain
{
    static int Main()
    {
        Thread main = Thread.CurrentThread;
        new Thread(() =>
        {
            Thread.Sleep(200);
            main.IsBackground = false;
            main.IsBackground = true;
        }).Start();

        return 3;
    }
}

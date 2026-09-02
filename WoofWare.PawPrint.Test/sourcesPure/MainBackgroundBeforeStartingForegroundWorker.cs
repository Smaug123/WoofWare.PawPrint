using System.Threading;

// CoreCLR's termination event is latched: `ThreadStore::CheckForEEShutdown` sets it the
// moment no foreground thread is alive, and nothing resets it. Main making itself
// background while it is the only started thread is such a moment, so the foreground
// worker started afterwards is abandoned when Main returns: `WaitForOtherThreads` finds
// the event already set and the process exits at once, worker or no worker. Measured on
// real .NET 10: exit code 3 (40/40 on a macOS host, 64/64 in a Linux container with eight
// instances on one CPU).
//
// The worker never finishes, so a runtime that decided from the thread table alone when
// Main returns would wait for it for ever: a hang on real .NET, a deadlock here. That is
// what makes the exit code the observable. The worker must not do anything timed — an
// `Environment.Exit` after a short sleep, say — because whether that lands before the
// host's own exit is then a race under CPU contention (measured: 1 of 64 contended runs
// took the worker's path).
class MainBackgroundBeforeStartingForegroundWorker
{
    static int Main()
    {
        Thread.CurrentThread.IsBackground = true;
        new Thread(() => Thread.Sleep(Timeout.Infinite)).Start();
        return 3;
    }
}

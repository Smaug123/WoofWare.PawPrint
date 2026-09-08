using System;
using System.Threading;

// The process does not end when Main returns: it ends when the last foreground thread
// does (CoreCLR's `ThreadStore::WaitForOtherThreads`, run after Main from
// `RunMainPost`). So the worker started here still runs after Main has returned, and
// the exit code it latches through `Environment.ExitCode` is what the host reports.
// Measured on real .NET 10: exit code 7.
//
// The worker cannot latch 7 until it has seen `mainDone`, which Main sets last, so a
// runtime that exited as soon as Main returned would report 0 here.
//
// `void Main` is essential. An `int Main` latches its own return value *after* it
// returns, which races the worker's latch. And the worker must set `ExitCode` rather
// than call `Environment.Exit`: an `Exit` on a worker after Main has returned runs the
// runtime shutdown on two threads at once, and which one reaches `_exit` first decides
// the exit code (measured: 10 of 64 runs under CPU contention took the other path).
class ForegroundThreadExitsAfterMainReturns
{
    static volatile bool mainDone;

    static void Main()
    {
        new Thread(() =>
        {
            while (!mainDone) { }
            Environment.ExitCode = 7;
        }).Start();

        mainDone = true;
    }
}

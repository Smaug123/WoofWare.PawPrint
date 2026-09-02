using System;
using System.Threading;

// The process does not end when Main returns: it ends when the last foreground thread
// does (CoreCLR's `ThreadStore::WaitForOtherThreads`, run after Main from
// `RunMainPost`). So the worker started here still runs to completion after Main has
// returned 3, and its `Environment.Exit(7)` is what ends the process. Measured on real
// .NET 10: exit code 7.
//
// A runtime that exited as soon as Main returned would report 3 here, having dropped
// the worker unrun.
class ForegroundThreadExitsAfterMainReturns
{
    static int Main()
    {
        new Thread(() => Environment.Exit(7)).Start();
        return 3;
    }
}

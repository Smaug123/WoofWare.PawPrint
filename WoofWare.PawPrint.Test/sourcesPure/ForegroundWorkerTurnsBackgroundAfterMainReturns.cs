using System.Threading;

// A foreground thread that makes itself background is no longer holding the process
// open. CoreCLR's `Thread::SetBackground` re-checks `OtherThreadsComplete` for exactly
// this reason: once Main has returned, flipping the last foreground thread to
// background ends the process at that moment, even though the thread then parks for
// ever. Measured on real .NET 10: exit code 5.
//
// The worker cannot be observed running after Main on either runtime, so the process
// simply must end with Main's exit code rather than hanging (or, for an interpreter,
// reporting a deadlock) on the parked worker.
class ForegroundWorkerTurnsBackgroundAfterMainReturns
{
    static int Main()
    {
        new Thread(() =>
        {
            Thread.CurrentThread.IsBackground = true;
            Thread.Sleep(Timeout.Infinite);
        }).Start();
        return 5;
    }
}

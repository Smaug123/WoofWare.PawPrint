using System.Threading;

// An idle thread-pool worker parks in `LowLevelLifoSemaphore`, whose wait is the
// `WaitHandle_WaitOnePrioritized` QCall. That calls the PAL wait directly rather than making an
// alertable wait, so the worker does not report WaitSleepJoin: measured on real .NET 10, it
// reads Background alone.
class ThreadStateOfIdlePoolWorker
{
    static int Main(string[] args)
    {
        Thread worker = null;
        ManualResetEvent ran = new ManualResetEvent(false);
        ThreadPool.QueueUserWorkItem(_ =>
        {
            worker = Thread.CurrentThread;
            ran.Set();
        });
        ran.WaitOne();

        // Nothing distinguishes "parked" from "about to park" through `ThreadState`, which is
        // the point: both read Background. The sleep gives the worker time to park.
        Thread.Sleep(50);
        if (worker.ThreadState != ThreadState.Background)
        {
            return 1;
        }

        return 0;
    }
}

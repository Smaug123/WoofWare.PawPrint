using System.Threading;

namespace HelloWorldApp
{
    class Program
    {
        static int Main(string[] args)
        {
            // Thread.Sleep(int) with a positive finite timeout must park the
            // calling thread on `BlockedOnSleep (Some deadline)` until the
            // virtual clock reaches the deadline, then resume past the call.
            //
            // No other thread is Runnable while Main sleeps, so the driver's
            // jump-to-deadline fallback advances `VirtualClockMs` to the
            // deadline and `Scheduler.fireSleepTimeout` flips Main back to
            // Runnable. The IL resumes past the QCall site with no stack
            // rewrite (Sleep returns void).
            //
            // CoreCLR's host-side `Thread.Sleep(50)` really suspends for ~50
            // wall-clock ms, but the only observable effect to the oracle is
            // that control returns past the call. PawPrint and the oracle
            // therefore produce identical output.
            Thread.Sleep(50);

            return 0;
        }
    }
}

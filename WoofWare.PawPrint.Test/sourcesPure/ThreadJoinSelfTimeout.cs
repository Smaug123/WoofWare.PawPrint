using System.Threading;

namespace HelloWorldApp
{
    class Program
    {
        static int Main(string[] args)
        {
            // Thread.CurrentThread.Join(int) with a positive finite timeout
            // must wait for the deadline and return false — not deadlock
            // and not fail loud. This is the self-join case that the
            // deadline machinery resolves naturally: the joiner parks on
            // `BlockedOnJoin (self, Some d)` and `Scheduler.fireJoinTimeout`
            // wakes it when the virtual clock reaches `d`. CoreCLR has the
            // same behaviour (the thread isn't terminated, so the wait
            // expires and returns false).
            //
            // Infinite self-join (`Thread.CurrentThread.Join()` or
            // `Thread.CurrentThread.Join(-1)`) is a different case: it
            // deadlocks under both CoreCLR and PawPrint. PawPrint rejects
            // that at the call site because there is no deadline that can
            // ever resolve it.
            if (Thread.CurrentThread.Join(50))
            {
                // Self is never Terminated while running, so the only way
                // Join returns true would be a wake that mishandled the
                // timeout — which we explicitly do not want.
                return 1;
            }

            return 0;
        }
    }
}

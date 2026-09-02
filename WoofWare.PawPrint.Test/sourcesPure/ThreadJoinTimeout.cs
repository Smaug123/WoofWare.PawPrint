using System.Threading;

namespace HelloWorldApp
{
    class Program
    {
        static int Main(string[] args)
        {
            // Thread.Join(int) with a positive finite timeout against a
            // never-terminating worker must return false after the timeout
            // elapses (rather than blocking forever or returning true).
            //
            // The worker thread parks in an infinite Monitor.Wait on a
            // never-pulsed lock, so it never reaches Terminated. The only
            // way the main thread can observe `false` from Join(50) is if
            // the scheduler woke it from BlockedOnJoin with a fired
            // deadline (`Scheduler.fireJoinTimeout` rewrites the optimistic
            // `Int32 1` to `Int32 0`).
            //
            // PawPrint's virtual clock advances 1 ms per scheduler tick and
            // the driver jumps to the next outstanding deadline when no
            // thread is Runnable, so the 50 ms timeout resolves
            // deterministically.
            //
            // The worker is still parked when Main returns, so it must be
            // a background thread: the process ends only when the last
            // foreground thread does, and this one never finishes.
            object locker = new object();
            Thread worker = new Thread(() =>
            {
                lock (locker)
                {
                    // Nobody pulses, so this parks forever.
                    Monitor.Wait(locker);
                }
            });
            // Background, or neither runtime would ever exit: real .NET
            // would hang in WaitForOtherThreads on the parked worker, and
            // PawPrint reports that shape as a deadlock.
            worker.IsBackground = true;
            worker.Start();

            if (worker.Join(50))
            {
                // The worker never terminates; Join returning true means
                // the timeout was not honoured.
                return 1;
            }

            return 0;
        }
    }
}

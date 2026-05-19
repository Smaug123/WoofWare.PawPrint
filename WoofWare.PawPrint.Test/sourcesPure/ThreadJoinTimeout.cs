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
            // We deliberately leave the worker still parked when Main
            // returns: the driver reports NormalExit as soon as the entry
            // thread terminates, and the worker is BlockedOnSyncBlockWait
            // (not Runnable, so no resources to release before Main exits).
            object locker = new object();
            Thread worker = new Thread(() =>
            {
                lock (locker)
                {
                    // Nobody pulses, so this parks forever.
                    Monitor.Wait(locker);
                }
            });
            // Mark background so the real-runtime oracle
            // (`RealRuntime.executeWithRealRuntime` runs the compiled assembly
            // in the NUnit test process) doesn't hang on a leaked foreground
            // thread once Main returns: a foreground worker stuck in
            // Monitor.Wait would keep the host process alive indefinitely.
            // PawPrint's driver reports NormalExit as soon as the entry
            // thread terminates and does not consult IsBackground, so this
            // does not affect the interpreter side.
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

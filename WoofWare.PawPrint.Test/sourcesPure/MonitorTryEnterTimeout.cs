using System.Threading;

namespace HelloWorldApp
{
    class Program
    {
        static int Main(string[] args)
        {
            // Monitor.TryEnter(obj, ms) with a positive finite timeout against
            // a permanently-held lock must return false after the timeout
            // elapses (rather than blocking forever or returning true).
            //
            // The worker thread acquires `locker`, then parks in an infinite
            // Monitor.Wait on a separate `barrier` lock so the host never
            // wakes it. With `locker` permanently held, Main's `TryEnter`
            // call has no way to acquire and must observe a timeout. The
            // BCL's wrapper for `TryEnter(obj, ms)` routes positive finite
            // timeouts through `Monitor_TryEnter_Slowpath` after the fast
            // path returns `UseSlowPath`; PawPrint's slowpath parks in
            // `BlockedOnSyncBlockAcquire (addr, Some deadline)` and pushes
            // optimistic `Int32 1`. `Scheduler.fireExpiredDeadlines` ->
            // `SyncBlockMonitor.fireAcquireTimeout` rewrites the slot to
            // `Int32 0` ⇒ the slowpath returns false ⇒ TryEnter returns
            // false.
            //
            // PawPrint's virtual clock advances 1 ms per scheduler tick and
            // the driver jumps to the next outstanding deadline when no
            // thread is Runnable, so the 50 ms timeout resolves
            // deterministically.
            object locker = new object();
            object barrier = new object();

            Thread worker = new Thread(() =>
            {
                lock (locker)
                {
                    // Tell Main we hold `locker`, then park forever.
                    lock (barrier)
                    {
                        Monitor.Pulse(barrier);
                    }

                    lock (barrier)
                    {
                        // Nobody pulses, so this parks forever.
                        Monitor.Wait(barrier);
                    }
                }
            });

            // Mark background so the real-runtime oracle doesn't hang on a
            // leaked foreground thread once Main returns; the worker is
            // permanently parked in Monitor.Wait. PawPrint's driver reports
            // NormalExit as soon as the entry thread terminates and does
            // not consult IsBackground.
            worker.IsBackground = true;
            worker.Start();

            // Wait until the worker has acquired `locker` (and is about to
            // park on `barrier`). Pulse-on-empty-wait is a no-op in CoreCLR,
            // so we need to be inside the Wait before the Pulse — use the
            // simpler "spin until we observe the lock is held" pattern.
            lock (barrier)
            {
                Monitor.Wait(barrier);
            }

            // Now `locker` is permanently held by the worker.
            if (Monitor.TryEnter(locker, 50))
            {
                // The worker never releases; TryEnter returning true means
                // the timeout was not honoured.
                Monitor.Exit(locker);
                return 1;
            }

            return 0;
        }
    }
}

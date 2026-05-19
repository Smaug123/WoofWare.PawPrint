using System.Threading;

namespace HelloWorldApp
{
    class Program
    {
        static object locker = new object();
        static object barrier = new object();
        static bool workerHolds = false;

        static void Worker()
        {
            lock (locker)
            {
                // Publish "I hold `locker`" through a flag-guarded barrier
                // pulse so a pulse delivered before Main is parked isn't
                // dropped.
                lock (barrier)
                {
                    workerHolds = true;
                    Monitor.Pulse(barrier);
                }

                // Park forever on `barrier` while still holding `locker`
                // so Main's `TryEnter(locker, 50)` has to time out. The
                // Wait releases only `barrier`; `locker` stays held.
                // Nobody pulses, so this parks indefinitely.
                lock (barrier)
                {
                    Monitor.Wait(barrier);
                }
            }
        }

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
            Thread worker = new Thread(Worker);

            // Mark background so the real-runtime oracle doesn't hang on a
            // leaked foreground thread once Main returns; the worker is
            // permanently parked in Monitor.Wait. PawPrint's driver reports
            // NormalExit as soon as the entry thread terminates and does
            // not consult IsBackground.
            worker.IsBackground = true;

            // Acquire `barrier` *before* starting the worker so the worker
            // cannot pulse before we are parked: if the worker runs first
            // it will block on `lock (barrier)` until our `Monitor.Wait`
            // releases the lock. The shared `workerHolds` flag also guards
            // against spurious wakes and against the pulse running while
            // we are still inside the lock but before the Wait.
            lock (barrier)
            {
                worker.Start();
                while (!workerHolds)
                {
                    Monitor.Wait(barrier);
                }
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

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
                // Fall out of the lock — release `locker`.
            }
        }

        static int Main(string[] args)
        {
            // Monitor.TryEnter(obj, ms) with a large positive finite timeout
            // against a briefly-held lock must return true once the holder
            // releases (rather than waiting out the full timeout).
            //
            // Worker takes `locker`, signals `barrier`, then releases. Main
            // calls TryEnter with a 10s timeout; the worker's Exit transfers
            // ownership directly to Main via the ownership-transfer path on
            // the SyncBlock's AcquireQueue. Main observes `true` from
            // TryEnter without the deadline firing.
            //
            // This exercises the slowpath's success leg: the optimistic
            // `Int32 1` pushed by `TryEnter_Slowpath` survives because the
            // deadline never fires; `Exit_FastPath`'s ownership-transfer
            // dequeues Main and flips it to Runnable with the optimistic 1
            // intact.
            Thread worker = new Thread(Worker);

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

            // Worker now holds `locker` momentarily, then releases.
            // TryEnter with a generous timeout must succeed via ownership
            // transfer.
            if (!Monitor.TryEnter(locker, 10000))
            {
                // Timeout fired before the worker released — that would
                // mean the success path is broken.
                return 1;
            }

            // We hold `locker`; release before returning.
            Monitor.Exit(locker);

            worker.Join();
            return 0;
        }
    }
}

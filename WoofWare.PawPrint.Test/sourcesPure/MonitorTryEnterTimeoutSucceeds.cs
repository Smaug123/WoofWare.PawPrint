using System.Threading;

namespace HelloWorldApp
{
    class Program
    {
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
            object locker = new object();
            object barrier = new object();

            Thread worker = new Thread(() =>
            {
                lock (locker)
                {
                    // Tell Main we hold `locker`.
                    lock (barrier)
                    {
                        Monitor.Pulse(barrier);
                    }
                    // Release `locker` by falling out of the lock.
                }
            });
            worker.Start();

            // Wait until the worker has acquired `locker`.
            lock (barrier)
            {
                Monitor.Wait(barrier);
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

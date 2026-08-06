using System;
using System.Threading;

namespace HelloWorldApp
{
    class Program
    {
        static ManualResetEvent? first;
        static ManualResetEvent? second;

        static void Signaller()
        {
            // Signals the two handles one at a time. The main thread's parked
            // WaitAll must stay parked after the first Set — a wait-all is not
            // satisfied until *every* handle is acquirable — and wake only
            // once the second lands.
            first!.Set();
            second!.Set();
        }

        static int Main(string[] args)
        {
            // The blocking half of WaitHandleWaitAll.cs, and the case that
            // pins the "skip, don't wake" rule: the first Set finds a parked
            // wait-all waiter that it cannot satisfy on its own. Waking it
            // there would let the guest proceed while `second` was still
            // unsignalled.
            using (first = new ManualResetEvent(false))
            using (second = new ManualResetEvent(false))
            {
                Thread t = new Thread(Signaller);
                t.Start();
                bool acquired = WaitHandle.WaitAll(new WaitHandle[] { first, second });
                t.Join();

                if (!acquired)
                {
                    return 1;
                }

                // Both must genuinely be signalled by the time the wait
                // returns — manual-reset events are not consumed by acquiring
                // them, so this reads the state the wait observed.
                if (!first.WaitOne(0) || !second.WaitOne(0))
                {
                    return 2;
                }

                return 0;
            }
        }
    }
}

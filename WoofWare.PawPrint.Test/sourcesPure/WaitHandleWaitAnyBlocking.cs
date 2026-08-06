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
            // Signals the handle at index 1, so the main thread's parked
            // WaitAny must wake reporting *that* index rather than 0.
            second!.Set();
        }

        static int Main(string[] args)
        {
            // The blocking half of WaitHandleWaitAny.cs. Everything there is
            // satisfied inline; here the main thread genuinely parks on both
            // handles and is woken by another thread's Set.
            //
            // This is the case where the returned value cannot be known at
            // park time: the interpreter pushes an optimistic WAIT_OBJECT_0
            // when it parks and the waker has to rewrite that slot once it
            // knows which handle satisfied the wait. Returning index 0 here
            // would mean the rewrite never happened; timing out would mean the
            // wake never reached a thread parked on more than one queue.
            using (first = new ManualResetEvent(false))
            using (second = new ManualResetEvent(false))
            {
                Thread t = new Thread(Signaller);
                t.Start();
                int index = WaitHandle.WaitAny(new WaitHandle[] { first, second });
                t.Join();

                if (index != 1)
                {
                    return 1;
                }

                // The wake must have dequeued the waiter from *both* handles,
                // not just the one that signalled. If a stale entry were left
                // on `first`, closing it would trip the "still has waiters"
                // check as the using-block unwinds.
                return 0;
            }
        }
    }
}

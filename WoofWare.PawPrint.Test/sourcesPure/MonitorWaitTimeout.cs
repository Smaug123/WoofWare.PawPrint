using System.Threading;

namespace HelloWorldApp
{
    class Program
    {
        static object locker = new object();

        static int Main(string[] args)
        {
            // Monitor.Wait(obj, int) with a non-zero finite timeout against
            // an unpulsed lock must return false after the timeout elapses
            // (rather than parking forever or returning true). The caller
            // holds the lock; nobody else ever calls Pulse, so the only way
            // the guest can observe `false` is if the scheduler woke the
            // waiter from the SyncBlock's WaitQueue with a timeout
            // (`SyncBlockMonitor.fireTimeout` rewrites the Wait QCall's
            // optimistic `1` to `0`).
            //
            // PawPrint's virtual clock advances 1 ms per scheduler tick
            // and the driver jumps to the next outstanding deadline when
            // no thread is Runnable, so the 50 ms timeout resolves
            // deterministically.
            lock (locker)
            {
                if (Monitor.Wait(locker, 50))
                {
                    // Nobody pulsed; if Wait returned true, the timeout was
                    // not honoured.
                    return 1;
                }
                return 0;
            }
        }
    }
}

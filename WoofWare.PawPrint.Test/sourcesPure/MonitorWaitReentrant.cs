using System.Threading;

namespace HelloWorldApp
{
    class Program
    {
        static object locker = new object();
        static int produced = 0;

        static void Worker()
        {
            lock (locker)
            {
                produced = 7;
                Monitor.Pulse(locker);
            }
        }

        static int Main(string[] args)
        {
            Thread t = new Thread(Worker);
            lock (locker)
            {
                // Take the lock a second time so we are at reentrancy depth 2
                // when we Wait. Resuming must restore depth 2, otherwise the
                // inner-and-outer lock exits below would throw
                // SynchronizationLockException.
                lock (locker)
                {
                    t.Start();
                    while (produced == 0)
                    {
                        Monitor.Wait(locker);
                    }
                }
            }
            t.Join();
            return produced == 7 ? 0 : 1;
        }
    }
}

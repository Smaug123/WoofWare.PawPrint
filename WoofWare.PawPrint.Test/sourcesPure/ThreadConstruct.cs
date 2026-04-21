using System.Threading;

namespace HelloWorldApp
{
    class Program
    {
        static int Main(string[] args)
        {
            // Read main thread ID before constructing any threads.
            int mainIdBefore = Thread.CurrentThread.ManagedThreadId;
            if (mainIdBefore <= 0) return 1;

            Thread t = new Thread(() => { });
            if (t == null) return 2;

            int id = t.ManagedThreadId;
            if (id <= 0) return 3;

            if (id == mainIdBefore) return 4;

            // The CLR guarantees stability for a given Thread object's managed ID, but it does
            // not guarantee any global ordering relative to the current host thread.
            int idAgain = t.ManagedThreadId;
            if (id != idAgain) return 6;

            // Read main thread ID again after constructing a thread; must be identical.
            int mainIdAfter = Thread.CurrentThread.ManagedThreadId;
            if (mainIdBefore != mainIdAfter) return 5;

            return 0;
        }
    }
}

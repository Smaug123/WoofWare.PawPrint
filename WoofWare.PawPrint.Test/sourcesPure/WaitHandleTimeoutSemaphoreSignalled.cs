using System.Threading;

namespace HelloWorldApp
{
    class Program
    {
        static Semaphore? sem;

        static void Releaser()
        {
            // Releases the main thread's WaitOne(50) so it observes a signal
            // (returning true) rather than the timeout firing (returning false).
            sem!.Release();
        }

        static int Main(string[] args)
        {
            // Companion to WaitHandleTimeoutSemaphore.cs: exercises the
            // *signal* path of WaitOne with a finite timeout. A worker thread
            // releases the semaphore; the main thread is blocked in WaitOne(50)
            // and must wake with the success result (WAIT_OBJECT_0 -> true)
            // rather than waiting out the timeout. If the deadline-fire logic
            // wrongly raced ahead of the signal-wake we'd return 1 here.
            using (sem = new Semaphore(0, 1))
            {
                Thread t = new Thread(Releaser);
                t.Start();
                bool acquired = sem.WaitOne(50);
                t.Join();
                return acquired ? 0 : 1;
            }
        }
    }
}

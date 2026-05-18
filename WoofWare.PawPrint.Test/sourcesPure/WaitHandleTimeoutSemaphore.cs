using System;
using System.Threading;

namespace HelloWorldApp
{
    class Program
    {
        static int Main(string[] args)
        {
            // WaitHandle.WaitOne(int) with a non-zero finite timeout against an
            // unsignalled handle must return false after the timeout elapses
            // (rather than parking forever or returning true). This exercises
            // the timed-wait codepath end-to-end: the semaphore starts with
            // count 0 and nobody releases it, so the only way the guest can
            // observe `false` is if the scheduler woke it from the wait queue
            // with WAIT_TIMEOUT (the BCL converts that into WaitOne returning
            // false).
            //
            // We use Semaphore because it's the simplest WaitHandle subclass
            // that PawPrint supports through the CreateSemaphoreExW QCall and
            // doesn't require additional plumbing (named events PNSE on Unix,
            // mutex ownership has its own state machine). The 50 ms timeout
            // is short enough that real .NET's wall clock walks past it in
            // any test harness; PawPrint's virtual clock advances 1 ms per
            // scheduler tick (and jumps to the next deadline when no thread
            // is Runnable), so the timeout fires after ~50 ticks deterministically.
            using (var sem = new Semaphore(0, 1))
            {
                if (sem.WaitOne(50))
                {
                    // count was zero and nobody released, so this must not
                    // succeed; if it does we've masked the timeout firing.
                    return 1;
                }
                return 0;
            }
        }
    }
}

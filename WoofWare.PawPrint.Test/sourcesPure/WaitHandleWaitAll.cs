using System;
using System.Threading;

namespace HelloWorldApp
{
    class Program
    {
        // `WaitHandle.WaitAll` through the
        // `WaitHandle_WaitMultipleIgnoringSyncContext` QCall. The property
        // under test is atomicity: a wait-all that cannot be satisfied must
        // consume *nothing*, so a partially-applied acquisition would show up
        // as a missing semaphore unit afterwards.
        static int Main(string[] args)
        {
            using (var unsignalled = new ManualResetEvent(false))
            using (var signalled = new ManualResetEvent(true))
            using (var alsoSignalled = new ManualResetEvent(true))
            using (var sem = new Semaphore(1, 1))
            {
                if (!WaitHandle.WaitAll(new WaitHandle[] { signalled, alsoSignalled }))
                {
                    return 1;
                }

                // Not all signalled, finite timeout: must expire, and must not
                // have consumed the semaphore's single unit on the way past.
                if (WaitHandle.WaitAll(new WaitHandle[] { sem, unsignalled }, 50))
                {
                    return 2;
                }

                // If the failed wait-all above had taken the unit, this
                // single-handle wait would time out.
                if (!sem.WaitOne(0))
                {
                    return 3;
                }

                sem.Release();

                // Zero timeout takes the non-blocking path; same atomicity
                // requirement.
                if (WaitHandle.WaitAll(new WaitHandle[] { sem, unsignalled }, 0))
                {
                    return 4;
                }

                if (!sem.WaitOne(0))
                {
                    return 5;
                }

                sem.Release();

                // All acquirable: the semaphore unit *is* consumed this time.
                if (!WaitHandle.WaitAll(new WaitHandle[] { sem, signalled }, 0))
                {
                    return 6;
                }

                if (sem.WaitOne(0))
                {
                    return 7;
                }

                sem.Release();

                return 0;
            }
        }
    }
}

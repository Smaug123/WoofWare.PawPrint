using System.Threading;

// A guest whose single-threaded prefix contains guest yields, and which only then forks.
//
// The point is the yields: `Thread.Yield()` and `Thread.Sleep(0)` reach
// `Scheduler.chargeYieldDebt`, which under PCT used to toss its honour coin unconditionally.
// If it ever draws again while no other thread is Runnable, the RNG stream of a from-scratch
// seeded run diverges from one resumed at the fork point, and the differential test that
// compares the two will say so.
public class ForkAfterSoloYield
{
    private static int shared = 0;

    private static int Main(string[] args)
    {
        // No other thread exists yet, so every one of these is a yield with nobody to yield to.
        for (int i = 0; i < 5; i++)
        {
            Thread.Yield();
        }

        Thread.Sleep(0);

        Thread t = new Thread(() => { shared = 1; });
        t.Start();

        // Racy by construction: 0 or 1 depending on the interleaving, which is what makes the
        // post-fork schedule observable in the exit code rather than only in the trace.
        int seen = shared;
        t.Join();
        return seen;
    }
}

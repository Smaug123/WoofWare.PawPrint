using System.Threading;

// A guest that wedges while still single-threaded: it never reaches a scheduling choice, so it
// never reaches a fork point either, and `Program.runToFirstFork` must report
// `DeadlockedBeforeFork` rather than `NeverForked` or a snapshot.
//
// Like `NeverForked`, that answer is seed-independent — no policy had a choice anywhere, so every
// seed wedges in exactly the same place — which is why a sweep can stop after one run. The
// distinction from `NeverForked` matters because a whole-program `Program.run` turns this into a
// host `failwith`, where the fork API returns it as a value.
public class DeadlockBeforeFork
{
    private static int Main(string[] args)
    {
        // No deadline, so the driver's jump-to-deadline fallback has nothing to jump to and no
        // thread ever becomes Runnable again.
        Thread.Sleep(Timeout.Infinite);
        return 0;
    }
}

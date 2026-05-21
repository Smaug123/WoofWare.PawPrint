using System.Threading;

// Canonical "two threads incrementing a shared counter without
// synchronisation" race. Each thread does the IL pattern
//   ldsfld counter; ldc.i4.1; add; stsfld counter
// (read into a local, add, write back) in a loop. The bad
// interleaving is "T1 reads N, T2 reads N, T1 writes N+1, T2 writes
// N+1" -- a lost update; the final counter is < 2 * Iterations.
//
// The worker iterates ITERATIONS times rather than once because
// PCT's per-IL-op preemption is probabilistic (weight * P_BASE,
// with P_BASE = 0.01 at the time of writing); a one-shot race
// gives ~0.5% per-seed chance of catching the bad window and 64
// seeds aren't enough. Looping turns the search into "find a seed
// where ANY of the N iterations preempts mid-RMW", which catches
// the bug with overwhelming probability under the default sweep.
//
// Main waits for both workers, then returns 1 if `counter`
// disagrees with `2 * ITERATIONS` (some update was lost), and 0
// otherwise. Exit code 1 is the bad outcome the TestConcurrencyBugs
// scenario asserts PCT can reach.

namespace HelloWorldApp
{
    class Program
    {
        static int counter;

        static void Worker()
        {
            int local = counter;
            counter = local + 1;
        }

        static int Main(string[] args)
        {
            counter = 0;
            Thread t1 = new Thread(Worker);
            Thread t2 = new Thread(Worker);
            t1.Start();
            t2.Start();
            t1.Join();
            t2.Join();
            return counter == 2 ? 0 : 1;
        }
    }
}

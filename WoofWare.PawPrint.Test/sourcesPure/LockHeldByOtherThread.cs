// `System.Threading.Lock` identifies its owner by *OS thread id*, so this pins that the ids these
// PAL entry points hand out are actually distinct per thread — not merely that the entry points
// return something.
//
// `Lock.ThreadId.InitializeForCurrentThread` (Lock.NonNativeAot.cs:50) caches the id in a
// `[ThreadStatic]` field, and `IsHeldByCurrentThread` compares the holder against that cache. Two
// separate mistakes both make a worker believe it holds a lock owned by the main thread, and
// neither is visible from a single-threaded test: minting the same OS thread id for every thread,
// or giving the `[ThreadStatic]` cache one process-wide slot so the worker reads the main thread's
// entry.
//
// The worker must take a lock of its *own* first.
// `IsHeldByCurrentThread` reads `ThreadId.Current_NoInitialize`, which returns the raw
// `[ThreadStatic]` without populating it; on a thread that has never entered a lock the cache is 0,
// `IsInitialized` is false, and the id comparison is short-circuited away. Such a worker answers
// False for free, whatever the PAL returns. Entering the uncontended `otherGate` forces
// `InitializeForCurrentThread` to run on the worker and store *its* id, so the subsequent question
// about `gate` reaches the comparison that these entry points feed.
//
// Every assertion is on an ownership predicate, never on the id's numeric value (which the real
// runtime takes from the OS and PawPrint synthesises), nor on scheduling order: the worker runs
// entirely inside the main thread's lock scope, bracketed by Start/Join, and the two locks are
// disjoint so neither thread ever waits on the other.
using System.Threading;

public static class LockHeldByOtherThread
{
    static readonly Lock gate = new Lock ();

    // A second, uncontended lock, used only to force the worker's own ThreadId cache to be
    // populated. Never held by the main thread.
    static readonly Lock otherGate = new Lock ();

    static bool workerRan;
    static bool workerHeldItsOwnLock;
    static bool workerClaimedGate = true;

    static void Worker ()
    {
        // The main thread holds `gate` for the whole of this method, and never `otherGate`.
        using (otherGate.EnterScope ())
        {
            // The worker's own id is now initialized, so the question below is decided by
            // comparing ids rather than short-circuited by an uninitialized cache.
            workerHeldItsOwnLock = otherGate.IsHeldByCurrentThread;
            workerClaimedGate = gate.IsHeldByCurrentThread;
        }

        workerRan = true;
    }

    public static int Main (string[] args)
    {
        using (gate.EnterScope ())
        {
            if (!gate.IsHeldByCurrentThread)
            {
                return 1;
            }

            Thread t = new Thread (Worker);
            t.Start ();
            t.Join ();

            if (!workerRan)
            {
                // The worker never completed; the assertions below would pass vacuously.
                return 2;
            }

            if (!workerHeldItsOwnLock)
            {
                // The worker's id cache was not populated, so the `gate` answer below would be
                // short-circuited rather than decided by an id comparison.
                return 3;
            }

            if (workerClaimedGate)
            {
                return 4;
            }

            // Re-entrancy on the owning thread still works: the owner check is per-thread, and
            // recursion depth is tracked against that same identity.
            using (gate.EnterScope ())
            {
                if (!gate.IsHeldByCurrentThread)
                {
                    return 5;
                }
            }

            if (!gate.IsHeldByCurrentThread)
            {
                return 6;
            }
        }

        if (gate.IsHeldByCurrentThread)
        {
            return 7;
        }

        return 0;
    }
}

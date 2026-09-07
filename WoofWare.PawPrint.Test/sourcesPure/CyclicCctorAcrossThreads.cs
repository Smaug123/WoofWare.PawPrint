using System.Threading;

// Two types whose class constructors each read a static field of the other, triggered from two
// different threads: the main thread touches `A` first and the worker touches `B` first. If the
// scheduler interleaves them so that each thread is inside its own `.cctor` when it reaches the
// other type, each type's initialisation lock is held by a thread that is waiting for the
// other's, and ECMA-335 II.10.5.3.3 step 2.2.1-2.2.2 says the second thread to arrive must not
// block: it proceeds and sees the other type in whatever partially-initialised state it is in.
// CoreCLR implements that in `DeadlockAwareLock::TryBeginEnterLock`, which walks the chain of
// lock holders and refuses to block when the chain leads back to the current thread.
//
// Each `.cctor` runs a counting loop before touching the other type so that a scheduler which
// switches threads at all gets many opportunities to do so while both locks are held; a cctor
// that completed within a single quantum would let the second thread find the type already
// initialised, and the test would pass while exercising nothing. `Value` is written *after* the
// cross-type read so that a thread which proceeds through the cycle really does observe the
// other type's default state (`Value == 0`), and `SawPartial*` records that.
//
// Which thread wins the race is not deterministic on real .NET, so the assertions are only the
// facts that hold whichever way it goes: each `.cctor` runs exactly once, both `Value`s end up
// fully initialised once both threads have finished, and at most one of the two threads can
// have seen the other's partial state (the thread that blocks wakes up to a completed type).
public static class A
{
    public static int Value;
    public static int CctorRuns;
    public static bool SawPartialB;

    static A()
    {
        CctorRuns++;

        int acc = 0;
        for (int i = 0; i < 200; i++)
        {
            acc += i;
        }

        SawPartialB = B.Value == 0;
        Value = acc;
    }
}

public static class B
{
    public static int Value;
    public static int CctorRuns;
    public static bool SawPartialA;

    static B()
    {
        CctorRuns++;

        int acc = 0;
        for (int i = 0; i < 200; i++)
        {
            acc += i;
        }

        SawPartialA = A.Value == 0;
        Value = acc;
    }
}

public static class CyclicCctorAcrossThreads
{
    // 0 + 1 + ... + 199.
    private const int Expected = 19900;

    private static int workerSeen;

    private static void Worker()
    {
        workerSeen = B.Value;
    }

    public static int Main(string[] args)
    {
        Thread t = new Thread(Worker);
        t.Start();

        int mainSeen = A.Value;

        t.Join();

        if (A.Value != Expected)
        {
            return 1;
        }

        if (B.Value != Expected)
        {
            return 2;
        }

        if (A.CctorRuns != 1)
        {
            return 3;
        }

        if (B.CctorRuns != 1)
        {
            return 4;
        }

        // A thread that blocks on the other's initialisation resumes only once that
        // initialisation has completed, so it cannot also have seen a partial state.
        if (A.SawPartialB && B.SawPartialA)
        {
            return 5;
        }

        // Whichever thread triggered a type reads its fully-initialised value: the trigger
        // returns only once the type's own `.cctor` has finished.
        if (mainSeen != Expected)
        {
            return 6;
        }

        if (workerSeen != Expected)
        {
            return 7;
        }

        return 0;
    }
}

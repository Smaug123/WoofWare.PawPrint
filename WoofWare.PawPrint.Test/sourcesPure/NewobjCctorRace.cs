using System.Threading;

// Two threads reach `newobj SlowCctor::.ctor` concurrently, so one of them enters the type's
// `.cctor` and the other finds the type initialisation lock already held by a different thread.
// That is the `WhatWeDid.BlockedOnClassInit` outcome of `ensureTypeInitialised`, which every
// other opcode that can trigger a `.cctor` already forwards to the scheduler.
//
// The cctor is deliberately long-running (a counting loop rather than a constant) so the
// scheduler gets many opportunities to switch threads while the lock is held. A cctor that
// completes within a single scheduler quantum would let both threads through without ever
// contending, and the test would pass while exercising nothing.
//
// ECMA-335 II.10.5.3.2 makes the observable outcome deterministic despite the race: the losing
// thread blocks until initialisation completes, so *both* threads must observe the fully
// initialised `Value`. Reading a partially-initialised (zero) `Value` is not a legal outcome,
// which is what makes the exit code a real assertion rather than a smoke test.
public class SlowCctor
{
    public static int Value;

    static SlowCctor()
    {
        int acc = 0;
        for (int i = 0; i < 200; i++)
        {
            acc += i;
        }

        Value = acc;
    }

    public int Instance;

    public SlowCctor()
    {
        Instance = Value;
    }
}

public static class NewobjCctorRace
{
    // 0 + 1 + ... + 199.
    private const int Expected = 19900;

    private static int workerResult;

    private static void Worker()
    {
        workerResult = new SlowCctor().Instance;
    }

    public static int Main(string[] args)
    {
        Thread t = new Thread(Worker);
        t.Start();

        int mainResult = new SlowCctor().Instance;

        t.Join();

        if (mainResult != Expected)
        {
            return 1;
        }

        if (workerResult != Expected)
        {
            return 2;
        }

        return 0;
    }
}

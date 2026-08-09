using System;
using System.Threading;

// The guest from issue #844, shrunk. Several threads spin on a flag using the BCL's own
// `SpinWait`, while one worker does bounded work and then sets it.
//
// `SpinWait.SpinOnceCore` on a single-processor kernel yields for its first 20 iterations
// (16 `Thread.Yield()` and 4 `Thread.Sleep(0)`, measured) and then calls `Thread.Sleep(1)`
// on every iteration forever. So this exercises the *sleep* half of the backoff, where
// `YieldingSpinnersDoNotStarveWorker.cs` exercises the yield half.
//
// The point of the guest is that a sleeping thread should cost the worker almost nothing.
// It is not a claim about PawPrint alone: on a real single-core machine these spinners are
// asleep essentially all the time, and the worker gets the machine. The fixture measures
// whether that is true here.
internal static class SpinWaitSpinnersDoNotStarveWorker
{
    private static volatile bool ready;
    private static volatile int result;

    private static int Main()
    {
        const int spinners = 6;
        // Deliberately long. `SpinWait` yields for its first twenty iterations and only then
        // starts calling `Thread.Sleep(1)`, so a worker that finishes during that warmup never
        // exercises the sleep path at all — measured, 150 units ended the run after ~4,000
        // ticks with every spinner still in its yield phase, and the fixture's parked-tick
        // assertion was vacuously false rather than meaningfully so.
        const int workUnits = 4000;

        for (int i = 0; i < spinners; i++)
        {
            Thread t = new Thread(() =>
            {
                SpinWait sw = new SpinWait();
                while (!ready)
                {
                    sw.SpinOnce();
                }
            });
            t.IsBackground = true;
            t.Start();
        }

        Thread worker = new Thread(() =>
        {
            int sum = 0;
            for (int j = 0; j < workUnits; j++)
            {
                sum += j;
            }

            result = sum;
            ready = true;
        });

        worker.Start();
        worker.Join();

        // 0 + 1 + ... + 3999.
        return result == 7998000 ? 0 : 1;
    }
}

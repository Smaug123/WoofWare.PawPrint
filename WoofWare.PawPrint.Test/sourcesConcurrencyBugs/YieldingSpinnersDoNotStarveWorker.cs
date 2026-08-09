using System;
using System.Threading;

// Several threads spin on a flag, yielding on every iteration, while one worker does bounded
// real work and then sets the flag. On a real machine the yielders cost the worker almost
// nothing. PawPrint serialises every thread onto one virtual CPU, so the worker cannot have
// the machine to itself — but a scheduler that honours the yields must not let the spinners
// take a proportional share of it either, because a yielding thread has explicitly said it
// has nothing to do.
//
// The assertion that matters is not in this file: the harness pins the total scheduler step
// count for the run (see TestImpureCases). This guest only has to (a) terminate, and (b) do a
// fixed, known amount of real work, so that the step count is a meaningful measure of how much
// of the machine the spinners consumed.
//
// Deliberately `Thread.Yield()` rather than `SpinWait`: SpinWait escalates to `Thread.Sleep(1)`
// after 20 iterations and spends the rest of its life there, and making a sleep cost the
// sleeper any virtual time is a separate piece of work. This guest is scoped to the part the
// yield-debt filter actually fixes.
internal static class YieldingSpinnersDoNotStarveWorker
{
    private static volatile bool ready;
    private static volatile int result;

    private static int Main()
    {
        const int spinners = 6;
        const int workUnits = 150;

        for (int i = 0; i < spinners; i++)
        {
            Thread t = new Thread(() =>
            {
                while (!ready)
                {
                    Thread.Yield();
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

        // 0 + 1 + ... + 149.
        return result == 11175 ? 0 : 1;
    }
}

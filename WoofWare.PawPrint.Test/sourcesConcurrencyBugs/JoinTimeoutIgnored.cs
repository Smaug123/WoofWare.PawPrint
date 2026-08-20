using System.Threading;

// A worker fills a shared buffer; Main waits for it to finish before reading.
//
// The bug is on one line: `Join(int)` returns a bool saying whether the thread
// actually finished, and Main throws it away. The author's reasoning was "200ms
// is far more than this worker ever needs" -- which is true of every run on an
// unloaded machine, and is not a guarantee. When the timeout expires instead,
// Main reads a buffer that is still being written.
//
// No interleaving finds this. Whenever the join succeeds the worker has
// terminated, so the buffer is complete however the two threads were scheduled;
// the only way to the bad state is for time itself to run out, which at the
// default pace of one clock tick per retired instruction would take two million
// instructions the worker does not need. It takes `ClockJitterStrategy` to
// reach it.
public static class Entry
{
    private const int Sentinel = 42;

    private static readonly int[] Buffer = new int[3];

    private static void Produce()
    {
        Buffer[0] = 1;
        Spin();
        Buffer[1] = 2;
        Spin();
        Buffer[2] = 3;
    }

    private static void Spin()
    {
        for (int i = 0; i < 200; i++)
        {
            Thread.SpinWait(1);
        }
    }

    public static int Main()
    {
        var worker = new Thread(Produce);
        worker.Start();

        // THE BUG: the return value says whether the worker finished or the
        // timeout expired, and the two are treated as the same thing.
        worker.Join(200);

        int sum = Buffer[0] + Buffer[1] + Buffer[2];

        if (sum != 6)
        {
            // Read a half-built buffer: the invariant this guest exists to
            // violate, reported as a sentinel so the host can see it.
            return Sentinel;
        }

        return 0;
    }
}

using System;
using System.Threading;

namespace HelloWorldApp
{
    class Program
    {
        static int Main(string[] args)
        {
            // Run with KernelConfig.ProcessorCount = 4.
            //
            // PawPrint places guest threads round-robin over the simulated
            // cores, so the entry thread is on core 0 and successive threads
            // take 1, 2, 3, 0, ... None of this can be pinned by the pure case
            // `ThreadGetCurrentProcessorId.cs`, which is cross-checked against
            // the real runtime: there the value comes from the host's
            // `sched_getcpu` (or, on macOS, from a managed-thread-id fallback
            // that is not bounded by the core count at all).

            if (Environment.ProcessorCount != 4) return 1;

            // The entry thread is the first guest-visible thread created, so it
            // takes rotation slot 0.
            if (Thread.GetCurrentProcessorId() != 0) return 2;

            // Every placement names a processor the guest also counts. This is
            // the invariant BCL shard indexing depends on, and the one that
            // would break if placement and `Environment.ProcessorCount` were
            // ever derived from different counts.
            int[] observed = new int[4];
            for (int i = 0; i < 4; i++)
            {
                observed[i] = -1;
            }

            for (int i = 0; i < 4; i++)
            {
                int slot = i;
                Thread worker = new Thread(() => { observed[slot] = Thread.GetCurrentProcessorId(); });
                worker.Start();
                worker.Join();
            }

            for (int i = 0; i < 4; i++)
            {
                if (observed[i] < 0 || observed[i] >= Environment.ProcessorCount) return 3;
            }

            // Round-robin, not a constant: four threads created after the entry
            // thread take slots 1, 2, 3, 0 in creation order. A regression to
            // "always core 0" would leave every multi-shard BCL path
            // permanently unexercised, and is exactly what this pins.
            if (observed[0] != 1) return 4;
            if (observed[1] != 2) return 5;
            if (observed[2] != 3) return 6;
            if (observed[3] != 0) return 7;

            // A thread that is constructed but never started still consumes a
            // rotation slot, mirroring real .NET's eager `ManagedThreadId`
            // assignment in the `Thread` constructor: the next started
            // thread must skip a core.
            Thread neverStarted = new Thread(() => { });

            int afterSkip = -1;
            Thread afterNeverStarted = new Thread(() => { afterSkip = Thread.GetCurrentProcessorId(); });
            afterNeverStarted.Start();
            afterNeverStarted.Join();

            // Rotation so far: entry thread took 0, the four workers took
            // 1, 2, 3, 4 (i.e. cores 1, 2, 3, 0), `neverStarted` took 5
            // (core 1), so this one takes 6 — core 2. Without the
            // never-started thread consuming its slot it would have been
            // core 1.
            if (afterSkip != 2) return 8;

            // Keep `neverStarted` alive to the end so nothing can argue it was
            // collected before it consumed its slot.
            if (neverStarted == null) return 9;

            return 0;
        }
    }
}

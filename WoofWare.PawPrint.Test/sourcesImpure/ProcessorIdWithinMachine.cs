using System;
using System.Buffers;
using System.Threading;

// Impure because the answer depends on the simulated machine's processor count,
// which only PawPrint's configured kernel fixes: on the real runtime the host's
// core count would decide it, so there is no cross-runtime oracle.
//
// Registered twice, with the machine's count and `DOTNET_PROCESSOR_COUNT` set
// to different values each way round. The guest returns
// `Environment.ProcessorCount * 10 + (the largest processor ID any thread saw)`,
// so the registration states both counts: the knob decides the first, and the
// machine alone decides the second, because a real kernel never reports a
// processor the machine lacks.
namespace HelloWorldApp
{
    class Program
    {
        // Rent two arrays and return both: the thread-local slot keeps one, so
        // the other goes to the per-core partition that `GetCurrentProcessorId`
        // modulo `Environment.ProcessorCount` selects. Renting twice must hand
        // both back.
        static bool PoolRoundTrips()
        {
            ArrayPool<byte> pool = ArrayPool<byte>.Shared;
            byte[] a = pool.Rent(1000);
            byte[] b = pool.Rent(1000);
            if (ReferenceEquals(a, b)) return false;
            pool.Return(a);
            pool.Return(b);
            byte[] c = pool.Rent(1000);
            byte[] d = pool.Rent(1000);
            bool sameSet = (ReferenceEquals(c, a) && ReferenceEquals(d, b)) || (ReferenceEquals(c, b) && ReferenceEquals(d, a));
            pool.Return(c);
            pool.Return(d);
            return sameSet;
        }

        static int Main(string[] args)
        {
            const int workers = 8;
            int[] observed = new int[workers + 1];
            bool[] roundTripped = new bool[workers + 1];

            observed[0] = Thread.GetCurrentProcessorId();
            roundTripped[0] = PoolRoundTrips();

            for (int i = 1; i <= workers; i++)
            {
                int slot = i;
                Thread worker = new Thread(() =>
                {
                    observed[slot] = Thread.GetCurrentProcessorId();
                    roundTripped[slot] = PoolRoundTrips();
                });
                worker.Start();
                worker.Join();
            }

            int max = 0;
            for (int i = 0; i <= workers; i++)
            {
                if (observed[i] < 0) return 1;
                if (!roundTripped[i]) return 2;
                if (observed[i] > max) max = observed[i];
            }

            return Environment.ProcessorCount * 10 + max;
        }
    }
}

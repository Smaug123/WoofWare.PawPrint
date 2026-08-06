// Interlocked.Or / Interlocked.And must be atomic read-modify-writes: a plain
// `x |= bit` from several threads can lose updates, whereas these must not.
// The assertions below are schedule-independent (each thread owns a distinct
// bit, and set-a-bit / clear-a-bit both commute), so this is a fact about any
// conforming runtime, not just PawPrint — hence a sourcesPure differential case.
using System.Threading;

namespace InterlockedAndOrContended
{
    class Program
    {
        const int ThreadCount = 8;

        static int s_orTarget = 0;
        static int s_andTarget = (1 << ThreadCount) - 1;

        // Each worker records what it observed, so a lost update is caught even
        // in the (impossible-under-correct-semantics) case where the final value
        // still happens to be right.
        static bool[] s_sawOwnBitClear = new bool[ThreadCount];
        static bool[] s_sawOwnBitSet = new bool[ThreadCount];

        static void Worker(object state)
        {
            int index = (int) state;
            int bit = 1 << index;

            // Setting our own bit: nobody else touches it, so the value we get
            // back must always have it clear.
            int beforeOr = Interlocked.Or(ref s_orTarget, bit);
            s_sawOwnBitClear[index] = (beforeOr & bit) == 0;

            // Symmetrically, clearing our own bit must always report it set.
            int beforeAnd = Interlocked.And(ref s_andTarget, ~bit);
            s_sawOwnBitSet[index] = (beforeAnd & bit) == bit;
        }

        static int Main(string[] args)
        {
            Thread[] threads = new Thread[ThreadCount];

            for (int i = 0; i < ThreadCount; i++)
            {
                threads[i] = new Thread(Worker);
            }

            for (int i = 0; i < ThreadCount; i++)
            {
                threads[i].Start(i);
            }

            for (int i = 0; i < ThreadCount; i++)
            {
                threads[i].Join();
            }

            if (s_orTarget != (1 << ThreadCount) - 1) return 1;
            if (s_andTarget != 0) return 2;

            for (int i = 0; i < ThreadCount; i++)
            {
                if (!s_sawOwnBitClear[i]) return 3;
                if (!s_sawOwnBitSet[i]) return 4;
            }

            return 0;
        }
    }
}

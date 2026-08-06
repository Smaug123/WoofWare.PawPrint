// `[ThreadStatic] static int t_value = 5;` is legal C# and a well-known gotcha: the
// field initialiser runs once, inside the type initialiser, on whichever thread first
// triggers it. Only *that* thread's slot ends up holding 5; every other thread's slot
// is the ordinary zero. This is a fact about any conforming CLI runtime, so it is a
// differential case.
//
// `Holder` has only a field initialiser (hence `beforefieldinit`), and the main thread
// is the first to touch it, so the main thread runs the initialiser.
using System;
using System.Threading;

namespace ThreadStaticCctorInitialiser
{
    static class Holder
    {
        [ThreadStatic]
        public static int t_value = 5;
    }

    class Program
    {
        static int workerObserved = -1;

        static void Worker()
        {
            workerObserved = Holder.t_value;
        }

        static int Main(string[] args)
        {
            // First touch: runs Holder's initialiser on this thread.
            if (Holder.t_value != 5) return 1;

            Thread t = new Thread(Worker);
            t.Start();
            t.Join();

            // The initialiser has already run, so the worker gets no initialisation at
            // all - just its own zeroed slot.
            if (workerObserved != 0) return 2;

            // The initialising thread's slot is unaffected by the worker.
            if (Holder.t_value != 5) return 3;

            return 0;
        }
    }
}

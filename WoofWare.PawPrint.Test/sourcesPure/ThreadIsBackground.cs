using System.Threading;

namespace HelloWorldApp
{
    class Program
    {
        static int Main(string[] args)
        {
            // The CLR initialises the main thread's IsBackground to false (it's a foreground
            // thread; the process is meant to stay alive while main runs). We exercise the
            // get-only QCall here to cover the read path on a thread that already exists.
            if (Thread.CurrentThread.IsBackground) return 1;

            // A freshly constructed but not-yet-started Thread also defaults to false. Reading
            // IsBackground on a NotStarted thread is the precise window the BCL thread-pool
            // setup writes through, so we cover both directions here without relying on Start.
            Thread t = new Thread(() => { });
            if (t.IsBackground) return 2;

            // Set true, round-trip via the getter — covers the SetIsBackground write path
            // followed by the GetIsBackground read path on the same Thread heap object.
            t.IsBackground = true;
            if (!t.IsBackground) return 3;

            // Set back to false — guards against a "first write sticks" bug where the field
            // would be set-once instead of mutable.
            t.IsBackground = false;
            if (t.IsBackground) return 4;

            // Independence: writing on `t` must not affect Thread.CurrentThread's flag.
            t.IsBackground = true;
            if (Thread.CurrentThread.IsBackground) return 5;

            return 0;
        }
    }
}

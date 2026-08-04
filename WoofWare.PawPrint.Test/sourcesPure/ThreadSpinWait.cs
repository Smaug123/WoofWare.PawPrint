using System.Threading;

namespace HelloWorldApp
{
    class Program
    {
        static int Main(string[] args)
        {
            // Thread.SpinWait(int) is a pure CPU-timing hint: CoreCLR's native
            // implementation (`ThreadNative_SpinWait`) just issues PAUSE/YIELD
            // instructions in a busy loop on the calling thread. It has no
            // managed-visible side effect and never touches the OS scheduler
            // (contrast with Thread.Yield()/Thread.Sleep(0), which do ask the
            // scheduler to consider running someone else).
            //
            // The public `Thread.SpinWait(int)` wrapper routes through two
            // different private methods depending on the iteration count, but
            // both compile down to the very same native entry point
            // (`ThreadNative_SpinWait`) - see `Thread.CoreCLR.cs`:
            //   - iterations < SpinWaitCoopThreshold (1024): `SpinWaitInternal`
            //     (a `[SuppressGCTransition]` fast path)
            //   - iterations >= SpinWaitCoopThreshold: `LongSpinWait` ->
            //     `LongSpinWaitInternal` (an ordinary GC-transitioning P/Invoke)
            // Exercise both so the test proves the same PawPrint handler
            // correctly answers both call shapes.
            int counter = 1;

            Thread.SpinWait(100);
            counter = counter + 1;

            Thread.SpinWait(2000);
            counter = counter + 1;

            Thread.SpinWait(0);
            counter = counter + 1;

            // Negative iteration counts are accepted too (CoreCLR's native side
            // just treats `iterations <= 0` as an immediate no-op rather than
            // throwing), so this must not perturb control flow either.
            Thread.SpinWait(-5);
            counter = counter + 1;

            return counter == 5 ? 0 : 1;
        }
    }
}

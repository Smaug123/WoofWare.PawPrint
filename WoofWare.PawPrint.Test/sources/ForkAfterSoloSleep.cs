using System.Threading;

// A guest whose single-threaded prefix contains a real timed sleep, and which only then forks.
//
// The point is the sleep: with the only thread parked, no thread is Runnable, so the driver's
// jump-to-deadline fallback advances the virtual clock and fires the wake. That path runs
// *inside* the tick preamble, and it is the reason a fork detector has to probe after the
// preamble rather than on the inter-tick state. It is also the path that most obviously must not
// consult the scheduling policy: `advanceUntilRunnableOrQuiescent` deliberately uses
// `hasAnyRunnable` rather than `chooseNext` so a probe cannot advance a stochastic policy's RNG.
public class ForkAfterSoloSleep
{
    private static int shared = 0;

    private static int Main(string[] args)
    {
        Thread.Sleep(5);

        Thread t = new Thread(() => { shared = 1; });
        t.Start();

        int seen = shared;
        t.Join();
        return seen;
    }
}

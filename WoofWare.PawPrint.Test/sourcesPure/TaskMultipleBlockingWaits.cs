using System.Threading.Tasks;

// Pins the path that GC.GetMemoryInfo (issue #698) unblocked.
//
// A *second* blocking wait on thread-pool-scheduled work in the same process reaches .NET's
// thread-pool blocking-adjustment heuristic, which calls the GC::GetMemoryInfo InternalCall.
// Before that InternalCall was implemented, any test performing two such waits died there --
// which is why the sibling Task cases are each written to perform only one. Two bare
// `Task.Run(...).Result` calls in a row are sufficient to reach the heuristic; no Yield or
// ContinueWith is needed.
//
// This case does twelve, mixing the shapes that reach it (plain Task.Run, and a continuation
// resumed through an awaited Task.Yield), so the multiple-wait path stays covered even though
// the other files stick to one wait apiece.
//
// Why only twelve, when there is no longer a ceiling to stay clear of: the budget used to be
// a handful of waits, because the pool's hill-climbing controller reaches the `Math`
// intrinsics -- `[Intrinsic]` + `InternalCall`, with no IL body to fall back on -- as soon as
// it has adjusted its thread count a few times. `Math.Pow` in the gain calculation
// (PortableThreadPool.HillClimbing.cs:301, issue #755, implemented in #763), then `Math.Cos`
// in `GetWaveComponent` (line 448, implemented in #779), then `Math.Sin` nine lines below it
// (line 457), and then `Math.Sqrt` by way of the magnitude the controller takes of those wave
// components -- its own private Complex.Abs is a bare Math.Sqrt
// (PortableThreadPool.HillClimbing.Complex.cs:35), implemented on this branch.
//
// With that last one in, the ceiling is gone: measured on this branch with exactly the loop
// below, 2400 blocking pool waits (1200 iterations) pass, where 240 failed before it. The
// controller no longer reaches any unimplemented primitive, so what bounds this file now is
// only how long the suite is willing to spend -- 2400 waits take about eight minutes under
// the interpreter, and twelve take two seconds. Twelve is well past the two that were needed
// to reach the heuristic in the first place, which is what this case exists to pin.
//
// Every assertion is on a returned value, never on which worker thread ran something, nor on
// timing, nor on ordering between independent tasks -- all of which are guaranteed under both
// the real runtime and PawPrint's simulated pool.
public static class TaskMultipleBlockingWaits
{
    static async Task<int> YieldThenAddAsync (int seed)
    {
        await Task.Yield ();
        return seed + 1;
    }

    public static int Main (string[] args)
    {
        for (int i = 0; i < 6; i++)
        {
            // A plain pool-blocking wait: the shape that reaches the heuristic at all.
            if (Task.Run (() => 41 + i).Result != 41 + i)
            {
                return 1;
            }

            // A wait of a different shape: a pool-scheduled continuation resumed through an
            // awaited Task.Yield rather than a Task.Run body.
            if (YieldThenAddAsync (10 * i).Result != 10 * i + 1)
            {
                return 2;
            }
        }

        return 0;
    }
}

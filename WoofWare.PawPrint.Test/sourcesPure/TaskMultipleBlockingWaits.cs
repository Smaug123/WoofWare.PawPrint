using System.Threading.Tasks;

// Pins the path that GC.GetMemoryInfo (issue #698) unblocked.
//
// A *second* blocking wait on thread-pool-scheduled work in the same process reaches .NET's
// thread-pool blocking-adjustment heuristic, which calls the GC::GetMemoryInfo InternalCall.
// Two bare `Task.Run(...).Result` calls in a row are sufficient to reach the heuristic; no
// Yield or ContinueWith is needed.
//
// This case does twelve, mixing the shapes that reach it (plain Task.Run, and a continuation
// resumed through an awaited Task.Yield), so the multiple-wait path stays covered even though
// the sibling Task cases stick to one wait apiece.
//
// Repeated waits drive the pool's hill-climbing controller into the `Math` intrinsics --
// `[Intrinsic]` + `InternalCall`, with no IL body to fall back on: `Math.Pow` in the gain
// calculation (PortableThreadPool.HillClimbing.cs:301, issues #755, #763), `Math.Cos` in
// `GetWaveComponent` (line 448, #779), `Math.Sin` nine lines below it (line 457), and
// `Math.Sqrt` via the controller's private Complex.Abs
// (PortableThreadPool.HillClimbing.Complex.cs:35). All are implemented: measured with exactly
// the loop below, 2400 blocking pool waits (1200 iterations) pass. What bounds this file is
// suite time -- 2400 waits take about eight minutes under the interpreter, twelve take two
// seconds -- and twelve is well past the two needed to reach the heuristic.
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

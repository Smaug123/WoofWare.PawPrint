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
// This case does two, mixing the shapes that reach it (plain Task.Run, and a continuation
// resumed through an awaited Task.Yield), so the multiple-wait path stays covered even though
// the other files stick to one wait apiece.
//
// Why exactly two: the budget is bounded above, and the bound is sized to the current frontier
// rather than chosen arbitrarily. The wait that exceeds it reaches the pool's hill-climbing
// controller adjusting its thread count, which calls `Math.Pow`
// (PortableThreadPool.HillClimbing.cs:301) -- an unimplemented JIT intrinsic, filed as issue
// #755. Raise this number once #755 lands.
//
// The budget used to be three. It dropped to two when `[ThreadStatic]` fields gained real
// per-thread storage: the pool is built on thread-static per-worker state
// (`ThreadPoolWorkQueue.t_tl`, `PortableThreadPool.t_isWorkerThread`, `Task.t_currentTask`,
// `ThreadInt64PersistentCounter.t_nodes`), so while every worker shared one set of slots
// PawPrint was not executing the controller's real accounting at all. Getting to `Math.Pow` in
// fewer waits means more of the pool now runs, not less -- the frontier moved because the code
// behind it became reachable. The boundary remains sharp and reproducible: 2 waits pass, 3 fail.
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
        // Two plain pool-blocking waits: the minimal shape that reaches the heuristic at all.
        if (Task.Run (() => 41).Result != 41)
        {
            return 1;
        }

        // A second wait, of a different shape: a pool-scheduled continuation resumed through an
        // awaited Task.Yield rather than a Task.Run body.
        if (YieldThenAddAsync (10).Result != 11)
        {
            return 3;
        }

        return 0;
    }
}

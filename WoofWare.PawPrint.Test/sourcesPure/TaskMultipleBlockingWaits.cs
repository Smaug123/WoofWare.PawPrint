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
// This case does three, mixing the shapes that reach it (plain Task.Run, and a continuation
// resumed through an awaited Task.Yield), so the multiple-wait path stays covered even though
// the other files stick to one wait apiece.
//
// Why exactly three: the budget is currently bounded above. Measured on this branch, three
// blocking pool waits pass and the *fourth* reaches a further unimplemented primitive. That
// primitive used to be `SystemNative_GetTimestamp` (issue #726); since that landed in #735 the
// fourth wait instead gets as far as the pool's hill-climbing controller adjusting its thread
// count, which calls `Math.Pow` (PortableThreadPool.HillClimbing.cs:301) -- an unimplemented JIT
// intrinsic, filed as issue #755. The boundary is sharp and reproducible: 3x
// `Task.Run(...).Result` passes; 4x, 6x, 8x, 10x all fail, as does 3x plus an awaited
// `Task.Yield()`. So this number is deliberately sized to the current frontier, not chosen
// arbitrarily; raise it once #755 lands.
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

        if (Task.Run (() => 42).Result != 42)
        {
            return 2;
        }

        // A third wait, of a different shape: a pool-scheduled continuation resumed through an
        // awaited Task.Yield rather than a Task.Run body.
        if (YieldThenAddAsync (10).Result != 11)
        {
            return 3;
        }

        return 0;
    }
}

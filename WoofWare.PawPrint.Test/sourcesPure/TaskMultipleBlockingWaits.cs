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
// Why twelve: the budget is still bounded above, and this is deliberately sized an order of
// magnitude clear of the ceiling rather than sitting on top of it, as previous numbers did.
// That ceiling used to be a handful of waits, because the pool's hill-climbing controller
// reaches the transcendental `Math` intrinsics -- `[Intrinsic]` + `InternalCall`, with no IL
// body to fall back on -- as soon as it has adjusted its thread count a few times: first
// `Math.Pow` in the gain calculation (PortableThreadPool.HillClimbing.cs:301, issue #755,
// implemented in #763) and then `Math.Cos` in `GetWaveComponent` (line 448, implemented on
// this branch).
//
// Measured on this branch, with exactly the loop below: 160 blocking pool waits (80
// iterations) pass and 240 (120 iterations) fail. The failure is `Math.Sin`, nine lines
// further down that same `GetWaveComponent` (line 457) -- so this is still a
// missing-primitive boundary rather than a correctness bug, and implementing it will move the
// name in the failure without moving the number. Note that where exactly the boundary falls
// depends on the shape of the loop body and not only on the number of waits, which is why
// this file no longer tries to sit exactly on it.
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

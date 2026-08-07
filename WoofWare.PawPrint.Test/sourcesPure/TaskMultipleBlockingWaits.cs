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
// That ceiling was two immediately before this branch, because the third wait reached
// `Math.Pow` in the pool's hill-climbing controller (PortableThreadPool.HillClimbing.cs:301)
// -- an unimplemented JIT intrinsic, issue #755. (It had been three until `[ThreadStatic]`
// fields gained real per-thread storage in #777; the pool is built on thread-static per-worker
// state, so that change made more of the controller's real accounting actually run, and
// `Math.Pow` came up sooner.)
//
// With `Math.Pow` implemented the frontier moves a long way out. Measured on this branch after
// rebasing onto #777, with exactly the loop below: 160 blocking pool waits (80 iterations) pass
// and 240 (120 iterations) fail. The failure is again an unimplemented JIT intrinsic in the
// same controller, `Math.Cos` -- so this is still a missing-primitive boundary, not a
// correctness bug. Note that where exactly the boundary falls depends on the shape of the loop
// body and not only on the number of waits, which is why this file no longer tries to sit
// exactly on it.
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

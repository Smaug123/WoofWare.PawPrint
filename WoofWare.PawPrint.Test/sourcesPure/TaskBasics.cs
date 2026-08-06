using System.Threading.Tasks;

// Pins the System.Threading.Tasks baseline described in issue #713: before this file, there was not
// a single Task test case in sourcesPure/sourcesImpure, despite quite a lot already working. Each
// Test* method exercises one independently-verified behaviour and returns 0 on success, matching the
// BeqBranch.cs convention of one file per coherent feature with offset-per-assertion return codes, so
// a failure here still says *which* Task behaviour regressed.
//
// The Task.Yield / ContinueWith / async-void behaviours live in their own single-behaviour files
// (TaskYieldAwait.cs, TaskContinueWith.cs, TaskAsyncVoid.cs) rather than being bundled in here, so
// that a failure names the behaviour that broke rather than just "TaskBasics".
//
// That split originally had a second, harder motivation: a *second* blocking wait on
// pool-scheduled work in the same process reaches .NET's thread-pool blocking-adjustment
// heuristic, which calls GC::GetMemoryInfo -- unimplemented at the time these were written, so
// each test process could only afford one such wait. That is no longer a constraint:
// GC::GetMemoryInfo is implemented (issue #698), and TaskMultipleBlockingWaits.cs pins the
// multiple-wait path directly. The files stay separate for the failure-signal reason above.
//
// Every assertion here only inspects results/values that are guaranteed deterministic under both the
// real runtime and PawPrint's simulated thread pool -- never which worker thread ran something, nor
// timing, nor ordering between independent tasks.
public static class TaskBasics
{
    // Task.FromResult produces an already-completed Task<T>; `.Result` on a completed task never blocks.
    static int TestFromResult()
    {
        Task<int> t = Task.FromResult(41);
        if (t.Result != 41) return 1;
        if (!t.IsCompletedSuccessfully) return 2;
        return 0;
    }

    // TaskCompletionSource<T>.SetResult then reading .Task.Result.
    static int TestTaskCompletionSource()
    {
        var tcs = new TaskCompletionSource<int>();
        tcs.SetResult(41);
        if (tcs.Task.Result != 41) return 1;
        if (!tcs.Task.IsCompletedSuccessfully) return 2;
        return 0;
    }

    // Task.Run dispatches the delegate onto PawPrint's simulated thread pool; `.Result` blocks the
    // calling thread until it completes. This is the only place in this file that genuinely exercises
    // cross-thread scheduling, but the assertion is only on the (deterministic) returned value.
    static int TestTaskRun()
    {
        Task<int> t = Task.Run(() => 41);
        if (t.Result != 41) return 1;
        return 0;
    }

    // async Task<int> with await, then reading .Result on the returned task: exercises the compiler-
    // generated state machine and its awaiter continuation path. The awaited task is already
    // completed (Task.FromResult), so the awaiter takes the synchronous-completion path and never
    // touches the thread pool.
    static async Task<int> ComputeAsync()
    {
        int x = await Task.FromResult(20);
        return x + 21;
    }

    static int TestAsyncAwait()
    {
        if (ComputeAsync().Result != 41) return 1;
        return 0;
    }

    public static int Main(string[] args)
    {
        int result;

        result = TestFromResult();
        if (result != 0) return 100 + result;

        result = TestTaskCompletionSource();
        if (result != 0) return 200 + result;

        result = TestTaskRun();
        if (result != 0) return 300 + result;

        result = TestAsyncAwait();
        if (result != 0) return 400 + result;

        return 0;
    }
}

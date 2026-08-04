using System.Threading;
using System.Threading.Tasks;

// Split out of TaskBasics.cs: `async void` methods run to completion asynchronously with no Task to
// observe. Synchronise via a ManualResetEventSlim (set only after the shared field is written) rather
// than any timing assumption, so this is deterministic under both runtimes. Kept in its own
// file/process for the same reason as TaskYieldAwait.cs -- see TaskBasics.cs's header comment
// (this method's `await Task.Yield()` is itself a blocking-adjacent, pool-scheduled continuation).
public static class TaskAsyncVoid
{
    static int result = 0;

    static async void DoWork(ManualResetEventSlim done)
    {
        await Task.Yield();
        result = 41;
        done.Set();
    }

    public static int Main(string[] args)
    {
        var done = new ManualResetEventSlim(false);
        DoWork(done);
        done.Wait();
        return result == 41 ? 0 : 1;
    }
}

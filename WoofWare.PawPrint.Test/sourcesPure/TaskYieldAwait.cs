using System.Threading.Tasks;

// Split out of TaskBasics.cs: `await Task.Yield()` forces the compiler-generated state machine
// through a real thread-pool-scheduled continuation (rather than the synchronous-completion fast
// path an already-completed awaitable can take). Kept in its own file so a failure names this
// behaviour specifically; see TaskBasics.cs's header comment.
public static class TaskYieldAwait
{
    static async Task<int> ComputeWithYieldAsync()
    {
        await Task.Yield();
        return 41;
    }

    public static int Main(string[] args)
    {
        return ComputeWithYieldAsync().Result == 41 ? 0 : 1;
    }
}

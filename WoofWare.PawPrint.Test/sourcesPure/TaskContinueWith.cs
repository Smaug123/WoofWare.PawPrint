using System.Threading.Tasks;

// Split out of TaskBasics.cs: Task<T>.ContinueWith schedules its continuation onto the default
// (thread-pool) scheduler even though the antecedent is already completed. Kept in its own
// file/process for the same reason as TaskYieldAwait.cs -- see TaskBasics.cs's header comment.
public static class TaskContinueWith
{
    public static int Main(string[] args)
    {
        Task<int> antecedent = Task.FromResult(20);
        Task<int> continuation = antecedent.ContinueWith(prev => prev.Result + 21);
        return continuation.Result == 41 ? 0 : 1;
    }
}

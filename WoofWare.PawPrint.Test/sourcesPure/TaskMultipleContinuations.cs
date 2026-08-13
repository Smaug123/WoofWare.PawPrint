using System.Threading.Tasks;

// Pins Task.RunContinuations' *multi-continuation* branch, which no other Task case reaches.
//
// A Task holds its continuations in one field. While that field holds a single continuation,
// RunContinuations dispatches it through a chain of type tests:
//
//     if (continuationObject is IAsyncStateMachineBox stateMachineBox) { ... }
//     else if (continuationObject is Action action) { ... }
//     else if (continuationObject is TaskContinuation tc) { ... }
//     else { List<object?> list = (List<object?>)continuationObject; ... }
//
// The final branch is an *unconditional* cast, reached only once a second continuation has been
// registered, at which point Task.AddTaskContinuationComplex has replaced the single object with
// a List<object>. So that cast is load-bearing on the preceding isinst tests all answering
// correctly: if any of them wrongly matched, or if the object PawPrint stored were not the one
// CoreLib expects to find there, the guest would die with an unhandled InvalidCastException
// instead. This is the "keep the classifier's contract truthful and load-bearing" case from
// AGENTS.md, and it is the shape that issue #758 suspected of being broken.
//
// The sibling cases cannot reach it: TaskContinueWith.cs and TaskBasics.cs each register exactly
// one continuation per Task, so they only ever exercise the TaskContinuation isinst arm. Getting
// to the List arm needs several continuations attached to a Task that has *not yet completed* --
// attaching to an already-completed Task runs the continuation immediately instead of storing it,
// so in every block below the antecedent is deliberately left incomplete until after the last
// ContinueWith call. That ordering is the whole point of the file, not incidental style.
//
// Both completion sites are covered, because they run RunContinuations on different threads:
// completion from the main thread, and completion from a pool worker -- the latter being where
// issue #758 observed its failure, its stack bottoming out in WorkerThread.WorkerThreadStart.
//
// Every assertion is on a returned value. Nothing here asserts on which thread ran a
// continuation, nor on timing, nor on the order in which independent continuations ran -- all of
// which are free to differ between the real runtime and PawPrint's simulated pool. The values
// hold under both: each continuation reads its antecedent's result, and a Task's result is fixed
// once set.
public static class TaskMultipleContinuations
{
    // Five rather than two, so the List has to grow past its initial capacity.
    static Task<int>[] AttachFive (Task<int> t)
    {
        return new[]
        {
            t.ContinueWith (prev => prev.Result + 1),
            t.ContinueWith (prev => prev.Result + 2),
            t.ContinueWith (prev => prev.Result + 3),
            t.ContinueWith (prev => prev.Result + 4),
            t.ContinueWith (prev => prev.Result + 5),
        };
    }

    // Read in index order, so the sum cannot depend on the order the continuations ran in.
    static int SumInOrder (Task<int>[] tasks)
    {
        int total = 0;

        for (int i = 0; i < tasks.Length; i++)
        {
            total += tasks[i].Result;
        }

        return total;
    }

    public static int Main (string[] args)
    {
        // Completed from this thread: RunContinuations runs on the main thread.
        {
            TaskCompletionSource<int> tcs = new TaskCompletionSource<int> ();
            Task<int>[] continuations = AttachFive (tcs.Task);

            // Only now does the antecedent complete, so all five were stored as a List<object>.
            tcs.SetResult (10);

            // 5 continuations over antecedent 10, adding 1..5: 50 + 15.
            if (SumInOrder (continuations) != 65)
            {
                return 1;
            }
        }

        // Completed from a pool worker: RunContinuations runs on the thread-pool thread, which is
        // the configuration issue #758 reported.
        {
            TaskCompletionSource<int> tcs = new TaskCompletionSource<int> ();
            Task<int>[] continuations = AttachFive (tcs.Task);

            Task.Run (() => tcs.SetResult (20)).Wait ();

            // 5 continuations over antecedent 20, adding 1..5: 100 + 15.
            if (SumInOrder (continuations) != 115)
            {
                return 2;
            }
        }

        // An antecedent that is itself a real pool work item rather than a TaskCompletionSource,
        // held incomplete by a gate so the five continuations all attach before it finishes.
        {
            TaskCompletionSource<int> gate = new TaskCompletionSource<int> ();
            Task<int> antecedent = Task.Run (() => gate.Task.Result + 100);

            Task<int>[] continuations = AttachFive (antecedent);

            // Release the antecedent only once every continuation is registered.
            gate.SetResult (1);

            // 5 continuations over antecedent 101, adding 1..5: 505 + 15.
            if (SumInOrder (continuations) != 520)
            {
                return 3;
            }
        }

        return 0;
    }
}

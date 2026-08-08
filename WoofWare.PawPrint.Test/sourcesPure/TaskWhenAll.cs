using System.Threading.Tasks;

// Pins Task.WhenAll, which issue #712 reported as failing and which nothing covered afterwards.
//
// #712 was not Task-shaped. Unlike WhenAny, which has a dedicated two-argument overload
// (Task.cs:6597, `WhenAny(Task task1, Task task2)`) that TaskWhenAnyTwoResults.cs binds to,
// WhenAll's smallest overload is `WhenAll(params ReadOnlySpan<Task> tasks)` (Task.cs:6027). So a
// two-argument WhenAll call site is a params-collection, and the C# compiler builds the span from a
// local of type `System.Runtime.CompilerServices.InlineArray2<Task>`: `initobj`, then per argument a
// byref to one slot via `<PrivateImplementationDetails>::InlineArrayElementRef` followed by
// `stind.ref`, then `InlineArrayAsReadOnlySpan`. Writing a slot past the first is what used to fail,
// because PawPrint had no N-slot layout for [InlineArray(N)] types and so reached a byte write over
// storage holding object references. Giving those types a real layout (#789, commit 5c3048f) fixed
// it, and this file exists so that a regression in that layout work is caught here rather than
// silently un-breaking WhenAll.
//
// That framing is why the arity of each call below is load-bearing rather than incidental: two
// arguments emits an InlineArray2 and three an InlineArray3, distinct types with distinct layouts.
// (Verified with WoofWare.PawPrint.IlDump against the compiled guest, not assumed.)
//
// Every assertion is a documented .NET guarantee, so it holds identically under the real runtime
// (which is the differential oracle for this file) and under PawPrint's simulated thread pool. In
// particular WhenAll<TResult> guarantees its result array is ordered by the *supplied* tasks, not by
// completion order, so the ordering checks below do not assume anything about scheduling. Nothing
// here asserts timing, thread identity, or the order in which independent tasks finish.
public static class TaskWhenAll
{
    // The exact repro from issue #712: two already-completed non-generic tasks, i.e. the
    // InlineArray2 write with no thread-pool scheduling involved at all.
    static int TestTwoCompletedTasks()
    {
        Task t = Task.WhenAll(Task.CompletedTask, Task.CompletedTask);
        t.Wait();
        if (!t.IsCompletedSuccessfully) return 1;
        return 0;
    }

    // Three arguments, so the call site builds an InlineArray3 rather than an InlineArray2: pins that
    // the layout fix generalises past the width the original repro happened to use.
    static int TestThreeCompletedTasks()
    {
        Task t = Task.WhenAll(Task.CompletedTask, Task.CompletedTask, Task.CompletedTask);
        t.Wait();
        if (!t.IsCompletedSuccessfully) return 1;
        return 0;
    }

    // The generic overload, whose result is an array rather than just a completion signal. This is
    // the case that would still fail if the inline-array slots were laid out but written to the
    // wrong offsets: a WhenAll that completed but mis-ordered its results would pass
    // TestTwoCompletedTasks and fail here.
    static int TestResultsInArgumentOrder()
    {
        Task<int> first = Task.FromResult(1);
        Task<int> second = Task.FromResult(2);
        int[] results = Task.WhenAll(first, second).Result;
        if (results.Length != 2) return 1;
        if (results[0] != 1) return 2;
        if (results[1] != 2) return 3;
        return 0;
    }

    // Tasks that are still incomplete when WhenAll inspects them, so the promise must register a
    // continuation on each rather than observing them already complete.
    //
    // Completion is driven by TaskCompletionSource rather than by Task.Run precisely so that this is
    // guaranteed rather than merely likely: a Task.Run delegate is free to finish before WhenAll ever
    // looks at its argument, on the real runtime as much as under PawPrint, in which case WhenAll
    // takes the already-completed path and the subtest silently degrades into a duplicate of
    // TestResultsInArgumentOrder while still passing. A TCS task completes only when this thread
    // calls SetResult, so the two IsCompleted checks below are facts about WhenAll's semantics and
    // not races: WhenAll cannot complete before *every* input has, so it is incomplete before any
    // SetResult and still incomplete once only one of the two has been completed.
    //
    // Driving completion by hand also lets the inputs be completed in reverse argument order, which
    // is what makes the result-ordering assertions discriminating rather than decorative -- see the
    // comment at the SetResult calls.
    static int TestPendingTasks()
    {
        TaskCompletionSource<int> first = new TaskCompletionSource<int>();
        TaskCompletionSource<int> second = new TaskCompletionSource<int>();

        Task<int[]> all = Task.WhenAll(first.Task, second.Task);
        if (all.IsCompleted) return 1;

        // Deliberately completed in reverse argument order. Completing `first` first would make
        // completion order and argument order coincide, so a WhenAll that collected results in
        // completion order would still produce [1, 2] and the assertions below would be vacuous.
        // This way such a regression produces [2, 1] and is caught.
        second.SetResult(2);
        if (all.IsCompleted) return 2;

        first.SetResult(1);

        int[] results = all.Result;
        if (results.Length != 2) return 3;
        if (results[0] != 1) return 4;
        if (results[1] != 2) return 5;
        return 0;
    }

    // WhenAll composed with thread-pool dispatch. Unlike TestPendingTasks this makes no claim about
    // whether the tasks are still running when WhenAll sees them -- either scheduling is legal, and
    // the assertions hold both ways -- so what it pins is that WhenAll accepts pool-scheduled work
    // and still orders results by argument rather than by completion.
    static int TestPoolScheduledTasks()
    {
        Task<int> first = Task.Run(() => 1);
        Task<int> second = Task.Run(() => 2);
        int[] results = Task.WhenAll(first, second).Result;
        if (results.Length != 2) return 1;
        if (results[0] != 1) return 2;
        if (results[1] != 2) return 3;
        if (!first.IsCompletedSuccessfully) return 4;
        if (!second.IsCompletedSuccessfully) return 5;
        return 0;
    }

    public static int Main(string[] args)
    {
        int result;

        result = TestTwoCompletedTasks();
        if (result != 0) return 100 + result;

        result = TestThreeCompletedTasks();
        if (result != 0) return 200 + result;

        result = TestResultsInArgumentOrder();
        if (result != 0) return 300 + result;

        result = TestPendingTasks();
        if (result != 0) return 400 + result;

        result = TestPoolScheduledTasks();
        if (result != 0) return 500 + result;

        return 0;
    }
}

using System;
using System.Threading.Tasks;

// `Task.ConfigureAwait(bool)` and `Task<TResult>.ConfigureAwait(bool)`. Both are `[Intrinsic]` for
// the same runtime-async-peephole reason as the ValueTask pair covered by the sibling files, but
// their body is a different shape — a bool-to-enum select rather than a struct copy:
//
//   ldarg.0; ldarg.1; brtrue.s L; ldc.i4.0; br.s M; L: ldc.i4.1
//   M: newobj ConfiguredTaskAwaitable[`1]::.ctor(Task[`1], ConfigureAwaitOptions); ret
//
// so `true` maps to ConfigureAwaitOptions.ContinueOnCapturedContext (1) and `false` to None (0).
//
// That mapping is what the faulted cases below pin, and they are the reason this file is not just
// "does the call return". ConfigureAwaitOptions.SuppressThrowing is 2, and awaiting under it
// completes a faulted Task *successfully*; so a select that produced 2 for either input — an easy
// way to get this subtly wrong — would make the non-generic faulted case swallow its exception, and
// would make the generic one throw ArgumentOutOfRangeException instead (Task<T> rejects
// SuppressThrowing). Asserting only on a successful result would miss both.
//
// Nothing here asserts which thread a continuation resumes on, which is all the flag itself
// influences; every assertion is deterministic under PawPrint's scheduler and real .NET alike.
public static class TaskConfigureAwaitBool
{
    // No await: the awaitable and awaiter are inspected directly, so a failure is attributable to
    // ConfigureAwait rather than to the async state machine.
    static int TestDirectGeneric()
    {
        Task<int> t = Task.FromResult(42);

        var awaiter = t.ConfigureAwait(false).GetAwaiter();
        if (!awaiter.IsCompleted) return 1;
        if (awaiter.GetResult() != 42) return 2;

        var awaiterTrue = t.ConfigureAwait(true).GetAwaiter();
        if (!awaiterTrue.IsCompleted) return 3;
        if (awaiterTrue.GetResult() != 42) return 4;

        return 0;
    }

    static int TestDirectNonGeneric()
    {
        Task t = Task.CompletedTask;

        var awaiter = t.ConfigureAwait(false).GetAwaiter();
        if (!awaiter.IsCompleted) return 1;
        awaiter.GetResult();

        var awaiterTrue = t.ConfigureAwait(true).GetAwaiter();
        if (!awaiterTrue.IsCompleted) return 2;
        awaiterTrue.GetResult();

        return 0;
    }

    static async Task<int> AwaitPoolScheduledAsync()
    {
        return await Task.Run(() => 41).ConfigureAwait(false);
    }

    // A pool-scheduled task, so the awaited task must survive into the awaiter rather
    // than the awaitable answering from thin air.
    static int TestAwaitPoolScheduled()
    {
        if (AwaitPoolScheduledAsync().Result != 41) return 1;
        return 0;
    }

    static async Task AwaitFaultedNonGenericAsync()
    {
        await ((Task)Task.FromException(new InvalidOperationException("boom"))).ConfigureAwait(false);
    }

    // Pins that `false` maps to None rather than to SuppressThrowing: under SuppressThrowing this
    // await would complete successfully and the fault would vanish.
    static int TestAwaitFaultedNonGeneric()
    {
        try
        {
            AwaitFaultedNonGenericAsync().Wait();
        }
        catch (AggregateException e) when (e.InnerException is InvalidOperationException inner)
        {
            return inner.Message == "boom" ? 0 : 1;
        }

        return 2;
    }

    static async Task<int> AwaitFaultedGenericAsync()
    {
        return await Task.FromException<int>(new InvalidOperationException("bang")).ConfigureAwait(true);
    }

    // The Task<T> mirror, on the `true` input so that both halves of the select are covered by a
    // faulted case. Task<T> rejects SuppressThrowing outright, so a select producing it would
    // surface here as ArgumentOutOfRangeException rather than as the task's own fault.
    static int TestAwaitFaultedGeneric()
    {
        try
        {
            _ = AwaitFaultedGenericAsync().Result;
        }
        catch (AggregateException e) when (e.InnerException is InvalidOperationException inner)
        {
            return inner.Message == "bang" ? 0 : 1;
        }

        return 2;
    }

    public static int Main(string[] args)
    {
        int result;

        result = TestDirectGeneric();
        if (result != 0) return 100 + result;

        result = TestDirectNonGeneric();
        if (result != 0) return 200 + result;

        result = TestAwaitPoolScheduled();
        if (result != 0) return 300 + result;

        result = TestAwaitFaultedNonGeneric();
        if (result != 0) return 400 + result;

        result = TestAwaitFaultedGeneric();
        if (result != 0) return 500 + result;

        return 0;
    }
}

using System;
using System.Threading.Tasks;

// `Task.ConfigureAwait(ConfigureAwaitOptions)` and `Task<TResult>.ConfigureAwait(ConfigureAwaitOptions)`.
// Same `[Intrinsic]`-is-only-a-peephole-marker story as the `bool` overloads in
// TaskConfigureAwaitBool.cs, but these two bodies do something the others do not: they *validate*,
// against a mask that differs between the two types.
//
//   Task:     options & ~0x7 must be 0  (None | ContinueOnCapturedContext | SuppressThrowing | ForceYielding)
//   Task<T>:  options & ~0x5 must be 0  (SuppressThrowing is rejected — there is a result to return)
//
// so the pair cannot be reviewed as one, and neither can be reviewed as "just constructs an
// awaitable". This file pins the three things that distinguishes them from the `bool` overloads:
//   * SuppressThrowing on a faulted `Task` completes the await *successfully*;
//   * that same option on a `Task<T>` is rejected with ArgumentOutOfRangeException;
//   * ForceYielding makes the awaiter report IsCompleted false even for an already-completed task.
// Each of those can only hold if the options value actually reached the awaiter, so together they
// are what makes this more than a reachability test.
//
// Every assertion is deterministic: ForceYielding is asserted through `IsCompleted` rather than
// through any claim about which thread resumes, and the awaits are on already-completed tasks.
public static class TaskConfigureAwaitOptions
{
    static int TestNonGenericPlainOptions()
    {
        Task t = Task.CompletedTask;

        var none = t.ConfigureAwait(ConfigureAwaitOptions.None).GetAwaiter();
        if (!none.IsCompleted) return 1;
        none.GetResult();

        var captured = t.ConfigureAwait(ConfigureAwaitOptions.ContinueOnCapturedContext).GetAwaiter();
        if (!captured.IsCompleted) return 2;
        captured.GetResult();

        return 0;
    }

    static int TestGenericPlainOptions()
    {
        Task<int> t = Task.FromResult(42);

        var none = t.ConfigureAwait(ConfigureAwaitOptions.None).GetAwaiter();
        if (!none.IsCompleted) return 1;
        if (none.GetResult() != 42) return 2;

        var captured = t.ConfigureAwait(ConfigureAwaitOptions.ContinueOnCapturedContext).GetAwaiter();
        if (!captured.IsCompleted) return 3;
        if (captured.GetResult() != 42) return 4;

        return 0;
    }

    // ForceYielding's whole observable effect on the awaiter is that it never reports completion, so
    // a continuation is always scheduled rather than run inline. An already-completed task makes
    // that unambiguous: without the option both of these would be true.
    static int TestForceYieldingIsNeverCompleted()
    {
        if (Task.CompletedTask.ConfigureAwait(ConfigureAwaitOptions.ForceYielding).GetAwaiter().IsCompleted) return 1;
        if (Task.FromResult(42).ConfigureAwait(ConfigureAwaitOptions.ForceYielding).GetAwaiter().IsCompleted) return 2;
        return 0;
    }

    static async Task<int> AwaitForceYieldingAsync()
    {
        await Task.CompletedTask.ConfigureAwait(ConfigureAwaitOptions.ForceYielding);
        return await Task.FromResult(41).ConfigureAwait(ConfigureAwaitOptions.ForceYielding);
    }

    // ...and the yield still has to land: the continuation must run and produce the result.
    static int TestAwaitForceYielding()
    {
        if (AwaitForceYieldingAsync().Result != 41) return 1;
        return 0;
    }

    static async Task AwaitSuppressThrowingAsync()
    {
        await Task.FromException(new InvalidOperationException("boom")).ConfigureAwait(ConfigureAwaitOptions.SuppressThrowing);
    }

    // The option that most clearly proves the value reached the awaiter: a faulted Task awaited
    // under SuppressThrowing completes successfully instead of rethrowing.
    static int TestSuppressThrowingSwallowsFault()
    {
        try
        {
            AwaitSuppressThrowingAsync().Wait();
        }
        catch (AggregateException)
        {
            return 1;
        }

        return 0;
    }

    // Task<T> rejects SuppressThrowing — there would be no result to hand back — which is the one
    // place the two bodies' masks differ, and so the assertion that stops them being reviewed as
    // one method.
    static int TestGenericRejectsSuppressThrowing()
    {
        try
        {
            _ = Task.FromResult(42).ConfigureAwait(ConfigureAwaitOptions.SuppressThrowing);
        }
        catch (ArgumentOutOfRangeException e)
        {
            return e.ParamName == "options" ? 0 : 1;
        }

        return 2;
    }

    // A bit outside every mask, rejected by both overloads.
    static int TestRejectsUndefinedOption()
    {
        try
        {
            _ = Task.CompletedTask.ConfigureAwait((ConfigureAwaitOptions) 8);
        }
        catch (ArgumentOutOfRangeException e)
        {
            if (e.ParamName != "options") return 1;

            try
            {
                _ = Task.FromResult(42).ConfigureAwait((ConfigureAwaitOptions) 8);
            }
            catch (ArgumentOutOfRangeException inner)
            {
                return inner.ParamName == "options" ? 0 : 2;
            }

            return 3;
        }

        return 4;
    }

    public static int Main(string[] args)
    {
        int result;

        result = TestNonGenericPlainOptions();
        if (result != 0) return 100 + result;

        result = TestGenericPlainOptions();
        if (result != 0) return 200 + result;

        result = TestForceYieldingIsNeverCompleted();
        if (result != 0) return 300 + result;

        result = TestAwaitForceYielding();
        if (result != 0) return 400 + result;

        result = TestSuppressThrowingSwallowsFault();
        if (result != 0) return 500 + result;

        result = TestGenericRejectsSuppressThrowing();
        if (result != 0) return 600 + result;

        result = TestRejectsUndefinedOption();
        if (result != 0) return 700 + result;

        return 0;
    }
}

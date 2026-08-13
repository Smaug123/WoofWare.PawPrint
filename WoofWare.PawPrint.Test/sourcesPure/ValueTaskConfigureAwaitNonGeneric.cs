using System;
using System.Threading.Tasks;
using System.Threading.Tasks.Sources;

// The non-generic `ValueTask.ConfigureAwait(bool)`, sibling of the `ValueTask<T>` overload covered by
// ValueTaskConfigureAwait.cs. Same `[Intrinsic]` story and the same body minus the `_result` field:
//
//   ldarg.0; ldfld _obj; ldarg.0; ldfld _token; ldarg.1
//   newobj ValueTask::.ctor(object, int16, bool)
//   stloc.0; ldloca.s 0; newobj ConfiguredValueTaskAwaitable::.ctor(ValueTask&); ret
//
// A void-returning awaitable cannot be pinned by its result, so each case here is arranged so that
// dropping one of the two carried fields changes whether an *exception* crosses the await:
//   * the faulted-Task case pins `_obj` — lose it and the awaitable becomes a successfully
//     completed default(ValueTask), so nothing is thrown;
//   * the IValueTaskSource case pins `_token` — the source throws for any token but its own.
// Both are exception-shaped rather than timing-shaped, so neither depends on the scheduler.
public static class ValueTaskConfigureAwaitNonGeneric
{
    private sealed class TokenCheckingSource : IValueTaskSource
    {
        public const short Token = 11;

        public void GetResult(short token)
        {
            if (token != Token) throw new InvalidOperationException("wrong token");
        }

        public ValueTaskSourceStatus GetStatus(short token) => ValueTaskSourceStatus.Succeeded;

        public void OnCompleted(Action<object> continuation, object state, short token, ValueTaskSourceOnCompletedFlags flags)
        {
            // Never reached: GetStatus reports the operation is already complete.
        }
    }

    // No await: the awaitable and awaiter are inspected directly, so a failure here is attributable
    // to ConfigureAwait rather than to the async state machine. `default(ValueTask)` is the
    // synchronously-succeeded value.
    static int TestDirect()
    {
        ValueTask vt = default;

        var awaiter = vt.ConfigureAwait(false).GetAwaiter();
        if (!awaiter.IsCompleted) return 1;
        awaiter.GetResult();

        var awaiterTrue = vt.ConfigureAwait(true).GetAwaiter();
        if (!awaiterTrue.IsCompleted) return 2;
        awaiterTrue.GetResult();

        return 0;
    }

    static async Task AwaitCompletedAsync()
    {
        await new ValueTask(Task.CompletedTask).ConfigureAwait(false);
    }

    static int TestAwaitCompleted()
    {
        try
        {
            AwaitCompletedAsync().Wait();
        }
        catch (AggregateException)
        {
            return 1;
        }

        return 0;
    }

    static async Task AwaitFaultedAsync()
    {
        await new ValueTask(Task.FromException(new InvalidOperationException("boom"))).ConfigureAwait(false);
    }

    // Pins `_obj`: the fault can only reach the awaiting frame if the configured copy still refers to
    // the faulted Task.
    static int TestAwaitFaulted()
    {
        try
        {
            AwaitFaultedAsync().Wait();
        }
        catch (AggregateException e) when (e.InnerException is InvalidOperationException inner)
        {
            return inner.Message == "boom" ? 0 : 1;
        }

        return 2;
    }

    static async Task AwaitSourceBackedAsync()
    {
        await new ValueTask(new TokenCheckingSource(), TokenCheckingSource.Token).ConfigureAwait(false);
    }

    // Pins `_token`: the source throws unless it is handed back the token the ValueTask was built
    // with, so a copy that zeroed it would fault here instead of completing. Caught rather than
    // allowed to escape so that the failure is a return code naming this case, not an unhandled
    // exception that says only "the program died".
    static int TestAwaitSourceBacked()
    {
        try
        {
            AwaitSourceBackedAsync().Wait();
        }
        catch (AggregateException)
        {
            return 1;
        }

        return 0;
    }

    public static int Main(string[] args)
    {
        int result;

        result = TestDirect();
        if (result != 0) return 100 + result;

        result = TestAwaitCompleted();
        if (result != 0) return 200 + result;

        result = TestAwaitFaulted();
        if (result != 0) return 300 + result;

        result = TestAwaitSourceBacked();
        if (result != 0) return 400 + result;

        return 0;
    }
}

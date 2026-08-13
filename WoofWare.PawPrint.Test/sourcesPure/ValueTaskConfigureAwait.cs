using System;
using System.Threading.Tasks;
using System.Threading.Tasks.Sources;

// `ValueTask<T>.ConfigureAwait(bool)` (issue #957). The method is `[Intrinsic]`, so before this
// file every guest touching it died at "TODO: implement JIT intrinsic
// System.Threading.Tasks.ValueTask`1.ConfigureAwait(System.Boolean)"; PawPrint now runs the managed
// body, which is what the JIT does too outside the runtime-async await peephole (see the
// safe-intrinsic allowlist entry for the citation).
//
// The body copies `_obj`, `_result`, `_token` and the new flag into a fresh ValueTask<T> and wraps
// it in a ConfiguredValueTaskAwaitable<T>, so each test below is chosen to fail if one of those
// three carried fields is lost rather than merely if the call aborts:
//   * the result-backed cases pin `_result` (losing it yields 0, not 42);
//   * the task-backed case pins `_obj` (losing it yields 0, not 41, and never blocks);
//   * the IValueTaskSource case pins `_token` (the source answers -1 for any other token).
// Nothing here asserts anything about *which* thread a continuation resumes on, which is the only
// thing `continueOnCapturedContext` actually influences; both values are exercised for reachability
// only, and every assertion is deterministic under PawPrint's scheduler and under real .NET alike.
public static class ValueTaskConfigureAwait
{
    // A synchronously-succeeded IValueTaskSource<int> that only answers correctly for the token it
    // was handed to the ValueTask<int> with, so a ConfigureAwait that dropped `_token` is visible.
    private sealed class TokenCheckingSource : IValueTaskSource<int>
    {
        public const short Token = 7;

        public int GetResult(short token) => token == Token ? 33 : -1;

        public ValueTaskSourceStatus GetStatus(short token) => ValueTaskSourceStatus.Succeeded;

        public void OnCompleted(Action<object> continuation, object state, short token, ValueTaskSourceOnCompletedFlags flags)
        {
            // Never reached: GetStatus reports the operation is already complete.
        }
    }

    // No await at all: the awaitable and its awaiter are inspected directly, so a failure here is
    // attributable to ConfigureAwait rather than to the async state machine.
    static int TestDirect()
    {
        ValueTask<int> vt = new ValueTask<int>(42);
        var awaiter = vt.ConfigureAwait(false).GetAwaiter();
        if (!awaiter.IsCompleted) return 1;
        if (awaiter.GetResult() != 42) return 2;

        // continueOnCapturedContext: true is the other half of the flag's domain, and is the value
        // an unconfigured await would have used.
        var awaiterTrue = vt.ConfigureAwait(true).GetAwaiter();
        if (!awaiterTrue.IsCompleted) return 3;
        if (awaiterTrue.GetResult() != 42) return 4;

        return 0;
    }

    static async Task<int> AwaitResultBackedAsync()
    {
        int x = await new ValueTask<int>(20).ConfigureAwait(false);
        return x + 21;
    }

    // The compiler-generated state machine's awaiter is ConfiguredValueTaskAwaiter rather than
    // ValueTaskAwaiter, so this covers the await path in addition to TestDirect's direct one.
    static int TestAwaitResultBacked()
    {
        if (AwaitResultBackedAsync().Result != 41) return 1;
        return 0;
    }

    static async Task<int> AwaitTaskBackedAsync()
    {
        return await new ValueTask<int>(Task.Run(() => 41)).ConfigureAwait(false);
    }

    // Backed by a Task rather than by a result, so `_obj` must survive the copy: a ValueTask<int>
    // that lost it would be an already-completed one carrying default(int).
    static int TestAwaitTaskBacked()
    {
        if (AwaitTaskBackedAsync().Result != 41) return 1;
        return 0;
    }

    static async Task<int> AwaitSourceBackedAsync()
    {
        return await new ValueTask<int>(new TokenCheckingSource(), TokenCheckingSource.Token).ConfigureAwait(false);
    }

    static int TestAwaitSourceBacked()
    {
        if (AwaitSourceBackedAsync().Result != 33) return 1;
        return 0;
    }

    public static int Main(string[] args)
    {
        int result;

        result = TestDirect();
        if (result != 0) return 100 + result;

        result = TestAwaitResultBacked();
        if (result != 0) return 200 + result;

        result = TestAwaitTaskBacked();
        if (result != 0) return 300 + result;

        result = TestAwaitSourceBacked();
        if (result != 0) return 400 + result;

        return 0;
    }
}

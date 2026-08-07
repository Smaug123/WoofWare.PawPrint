// The motivating caller for the enum instantiations of Interlocked.CompareExchange<T>
// and Interlocked.Exchange<T>. CancellationTokenSource holds its cancellation state in a
// private Int32-backed `States` enum and advances it with both: CompareExchange in
// TransitionToCancellationRequested (CancellationTokenSource.cs:711) and Exchange in
// ExecuteCallbackHandlers (CancellationTokenSource.cs:744). The synthetic enum coverage
// lives in InterlockedCompareExchangeEnum.cs and InterlockedExchangeEnum.cs; this asserts
// that the real CoreLib caller works, which the synthetic tests cannot.
using System;
using System.Threading;

class Program
{
    static int Main(string[] args)
    {
        CancellationTokenSource cts = new CancellationTokenSource();
        CancellationToken token = cts.Token;

        if (cts.IsCancellationRequested) return 1;
        if (token.IsCancellationRequested) return 2;
        token.ThrowIfCancellationRequested();

        // Drives the CompareExchange transition out of NotCanceledState and then the
        // Exchange into NotifyingCompleteState.
        cts.Cancel();

        if (!cts.IsCancellationRequested) return 3;
        if (!token.IsCancellationRequested) return 4;

        try
        {
            token.ThrowIfCancellationRequested();
            return 5;
        }
        catch (OperationCanceledException)
        {
        }

        // Cancelling again is a no-op: the state machine is already at its terminal state.
        cts.Cancel();
        if (!cts.IsCancellationRequested) return 6;

        // A source that is never cancelled must stay put.
        CancellationTokenSource other = new CancellationTokenSource();
        if (other.IsCancellationRequested) return 7;

        return 0;
    }
}

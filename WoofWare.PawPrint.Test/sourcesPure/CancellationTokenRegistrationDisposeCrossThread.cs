using System;
using System.Threading;

// `CancellationTokenRegistration.Dispose()`, called from a thread other than the one that
// ran `CancellationTokenSource.Cancel()`, must return.
//
// WARNING: what this catches is a livelock, so a regression makes this file *hang* rather
// than fail. If the suite stops rather than reporting, suspect this one, and read the fast
// signals instead: `NestedFinallyOnLeave.cs` covers the same defect and exits 21 in about a
// second, and `TestFinallyChain.fs` covers the handler-chain rule in milliseconds. Measured
// by reverting the `endfinally` chaining: this wedges, and both of those fail immediately.
//
// It earns its place regardless, as the witness that the real, Release-compiled
// `CancellationTokenSource` works — the sibling file is compiled by the harness, unoptimized,
// and so cannot exhibit the exact lowering that CoreLib does.
//
// The defect this guards against was never in the cancellation machinery; it was the
// multi-region `leave` bug pinned by the sibling `NestedFinallyOnLeave.cs`.
// `ExecuteCallbackHandlers` (CancellationTokenSource.cs) dispatches callbacks in a
// `while (true)` inside an outer `try`, and `break`s out of it when the callback list
// empties. Nothing follows the loop in that `try`, so Roslyn emits one `leave` from inside
// the *inner* `try` (the one whose `finally` calls `ExitLock`) straight past the outer
// handler. Running only the inner `finally` skips the outer one —
//
//     finally { _state = States.NotifyingCompleteState;
//               Interlocked.Exchange(ref registrations.ExecutingCallbackId, 0); }
//
// — leaving `_state` short of `NotifyingCompleteState` and `ExecutingCallbackId` still
// naming the callback that had already finished.
//
// `Dispose` then takes its slow path (`CancellationTokenRegistration.cs`): the node is no
// longer in the list so `Unregister` fails, and `WaitForCallbackIfNecessary` finds
// `IsCancellationRequested && !IsCancellationCompleted && ThreadIDExecutingCallbacks !=
// CurrentManagedThreadId` all three true, so it spins in `WaitForCallbackToComplete(id)`
// awaiting an `ExecutingCallbackId` nothing would ever clear.
//
// The cross-thread part is the point: disposing on the same thread that ran `Cancel()`
// short-circuits on the `ThreadIDExecutingCallbacks` check and returns even with stale
// state, so a single-threaded cancel/dispose guest passed throughout and pins nothing here.
class CancellationTokenRegistrationDisposeCrossThread
{
    static int Main(string[] args)
    {
        var cts = new CancellationTokenSource();
        int fired = 0;

        CancellationTokenRegistration reg = cts.Token.Register(() => Volatile.Write(ref fired, 1));

        var canceller = new Thread(() => cts.Cancel());
        canceller.IsBackground = true;
        canceller.Start();
        canceller.Join();

        // The callback has demonstrably completed: the thread that ran it has been joined.
        // Dispose must observe that and return promptly.
        reg.Dispose();

        return Volatile.Read(ref fired) == 1 ? 0 : 1;
    }
}

using System;
using System.Threading;

// `CancellationTokenRegistration.Dispose()`, called from a thread other than the one that
// ran `CancellationTokenSource.Cancel()`, must return. Under PawPrint today it never does:
// this file does not terminate, it livelocks in `Registrations.WaitForCallbackToComplete`.
//
// WARNING to whoever un-parks this: it hangs rather than failing. The `unimplemented`
// fixture only runs parked files against the *real* runtime, so parking it is safe, but
// moving it out of `unimplemented` before the underlying fix lands will wedge the suite.
//
// Root cause is not in the cancellation machinery at all; it is the multi-region `leave`
// bug pinned by the sibling `NestedFinallyOnLeave.cs`. `ExecuteCallbackHandlers`
// (CancellationTokenSource.cs) dispatches callbacks in a `while (true)` inside an outer
// `try`, and `break`s out of it when the callback list empties. Nothing follows the loop in
// that `try`, so Roslyn emits one `leave` from inside the *inner* `try` (the one whose
// `finally` calls `ExitLock`) straight past the outer handler. PawPrint runs only the inner
// `finally`, so the outer one —
//
//     finally { _state = States.NotifyingCompleteState;
//               Interlocked.Exchange(ref registrations.ExecutingCallbackId, 0); }
//
// — never executes. `_state` therefore never reaches `NotifyingCompleteState` and
// `ExecutingCallbackId` keeps naming the callback that already finished.
//
// `Dispose` then takes its slow path (`CancellationTokenRegistration.cs`): the node is no
// longer in the list so `Unregister` fails, and `WaitForCallbackIfNecessary` sees
// `IsCancellationRequested && !IsCancellationCompleted && ThreadIDExecutingCallbacks !=
// CurrentManagedThreadId`, all three true, so it spins in `WaitForCallbackToComplete(id)`
// waiting for an `ExecutingCallbackId` that nobody will ever clear.
//
// The cross-thread part is load-bearing: disposing on the same thread that ran `Cancel()`
// short-circuits on the `ThreadIDExecutingCallbacks` check and returns even with the stale
// state, which is why single-threaded cancel/dispose guests pass today.
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

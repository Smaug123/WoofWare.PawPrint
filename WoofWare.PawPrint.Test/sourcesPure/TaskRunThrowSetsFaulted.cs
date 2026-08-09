using System;
using System.Threading.Tasks;

// Probed while decomposing issue #713, and parked for a long time: a `Task.Run` delegate that
// threw used to crash PawPrint's simulated thread-pool worker while recording the fault, even
// from a bare `t.IsFaulted` spin-check with no `Wait()`/`Exception` access, because the failure
// was inside Task's own exception-capture path rather than anything user code touches.
//
// Four primitives had to land: an `ldsflda` through a `MemberReference` token (#723, in #740),
// then the `ExceptionNative_GetFrozenStackTrace` QCall (#754), then the
// `IsImmutableAgileException` and `PrepareForForeignExceptionRaise` InternalCalls. The last two
// are what `Exception.RestoreDispatchState` needs, which Task reaches because the thread-pool
// dispatch loop rethrows the captured fault through `ExceptionDispatchInfo.Throw()`.
//
// Asserts the fault's *type*, not its stack trace: PawPrint does not yet preserve the captured
// trace across an EDI rethrow (issue #876, and docs/divergences.md), so `ex.InnerException
// .StackTrace` here reports only the frames from the rethrow onwards. A test asserting trace
// content would diverge; this one does not.
public static class TaskRunThrowSetsFaulted
{
    public static int Main(string[] args)
    {
        Task t = Task.Run(() => { throw new InvalidOperationException("boom"); });

        try
        {
            t.Wait();
            return 1;
        }
        catch (AggregateException ex)
        {
            return ex.InnerException is InvalidOperationException ? 0 : 2;
        }
    }
}

using System;
using System.Threading.Tasks;

// A `Task.Run` delegate that throws exercises Task's own exception-capture path — nothing user
// code touches (issue #713): the thread-pool dispatch loop rethrows the captured fault through
// `ExceptionDispatchInfo.Throw()`, so `Exception.RestoreDispatchState` runs even under a bare
// `t.IsFaulted` spin-check with no `Wait()`/`Exception` access.
//
// Four primitives are required: an `ldsflda` through a `MemberReference` token (#723, in #740),
// the `ExceptionNative_GetFrozenStackTrace` QCall (#754), and the `IsImmutableAgileException`
// and `PrepareForForeignExceptionRaise` InternalCalls, the last two being what
// `Exception.RestoreDispatchState` needs.
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

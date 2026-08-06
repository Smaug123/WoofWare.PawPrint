using System;
using System.Threading.Tasks;

// Probed while decomposing issue #713: when the delegate passed to Task.Run throws, PawPrint's
// simulated thread pool worker crashes trying to record the fault. Even the minimal repro of
// spin-waiting on `t.IsFaulted` (no Wait()/Exception access at all) hits the same failure, so this is
// inside Task's own exception-capture path, not anything downstream that user code touches. See
// TestPureCases.fs's `unimplemented` entry for exactly where this fails.
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

using System.Threading;

namespace HelloWorldApp
{
    class Program
    {
        static int Main(string[] args)
        {
            // Thread.Join(int) with a positive finite timeout against a
            // worker that terminates within the deadline must return true
            // (not false, and not block past the deadline).
            //
            // The worker terminates immediately (its delegate body is
            // empty), so `Scheduler.onThreadTerminated` is the wake. That
            // wake must:
            //  - Flip the joiner back to Runnable.
            //  - Leave the optimistic `Int32 1` pushed at park time intact,
            //    so the BCL's Join(int) returns true.
            //  - Discard the still-outstanding deadline (the new Runnable
            //    status carries no deadline field, so this is structural).
            //
            // If `onThreadTerminated` failed to handle the finite-deadline
            // variant correctly, either the joiner would stay parked
            // (deadlock) or the deadline would still fire on a later tick
            // and rewrite the slot to `Int32 0` (false), which a
            // post-Termination `fireExpiredDeadlines` must not do.
            Thread worker = new Thread(() => { });
            worker.Start();

            if (!worker.Join(10_000))
            {
                // Worker completes in a handful of IL steps; a 10s timeout
                // could never fire first. If Join returned false, the
                // wake-on-terminate path mishandled the finite-deadline
                // case.
                return 1;
            }

            return 0;
        }
    }
}

using System;
using System.Threading;

// The degenerate hog: a thread whose loop body contains no guest-visible operation at all.
// `while (true) { }` is a single unconditional branch to itself, so every instruction the
// thread ever retires is a `br`.
//
// This is the case that motivated deleting the per-opcode context-switch weights. Under that
// scheme the chance of demoting the running thread was scaled by how "interesting" its current
// opcode was to interleave at, and a `br` scored zero -- so this thread's demotion probability
// was identically zero and nothing could ever take the machine away from it once PCT picked it.
// The worker below would then never run again, and the process would spin until it was killed.
//
// Under a flat per-step demotion rate the hog is demoted like anything else, so the worker
// finishes and Main returns. The fixture asserts that within a step budget: a policy that
// cannot preempt this thread does not merely slow the test down, it never terminates.
internal static class NonYieldingSpinnerDoesNotHangTheMachine
{
    private static volatile int result;

    private static int Main()
    {
        Thread spinner = new Thread(() =>
        {
            while (true)
            {
            }
        });
        spinner.IsBackground = true;
        spinner.Start();

        Thread worker = new Thread(() =>
        {
            int sum = 0;
            for (int j = 0; j < 200; j++)
            {
                sum += j;
            }

            result = sum;
        });

        worker.Start();
        worker.Join();

        // 0 + 1 + ... + 199.
        return result == 19900 ? 0 : 1;
    }
}

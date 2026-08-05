using System;
using System.Threading;

// Pins the one cross-runtime-assertable property of Thread.SpinWait: a guest delay loop that
// spins while polling the clock terminates. PawPrint implements the ThreadNative_SpinWait QCall
// as a genuine no-op, so it is worth pinning that this does not strand a guest that is using
// SpinWait to wait for the clock to move.
//
// It terminates for a structural reason rather than an arithmetic one: PawPrint's virtual clock
// advances one millisecond per scheduler tick, and a tick is one retired IL instruction, so the
// loop's own instructions move the clock regardless of what the handler does. See the
// ThreadNative_SpinWait arm in Native/NativeThreading.fs.
//
// What is deliberately NOT asserted here is anything about *how long* SpinWait takes, or how
// many iterations this runs for. PawPrint bills SpinWait(1) and SpinWait(1000000) the same
// handful of ticks, where real hardware puts them orders of magnitude apart; any assertion
// sharp enough to observe that difference would have to fail on one runtime or the other, since
// the harness diffs the exit code against the real runtime. That divergence is documented at the
// handler, not tested here.
public static class SpinWaitDelayLoop
{
    public static int Main (string[] args)
    {
        long start = Environment.TickCount64;
        int iterations = 0;

        // do/while, not while: the loop body must run at least once on BOTH runtimes, and a
        // pre-test loop does not give that. Under PawPrint the clock advances a millisecond per
        // retired IL instruction, so reading `start` and then evaluating the condition already
        // consumes more than this budget -- a `while` here is false on its first evaluation,
        // never calls Thread.SpinWait at all, and the test passes vacuously while appearing to
        // cover the handler. (On a real runtime a scheduling hiccup could do the same, far more
        // rarely.) Testing the post-condition instead makes one spin structural rather than
        // timing-dependent, so no separate `iterations >= 1` assertion is needed -- and asserting
        // it would have been a race on the real runtime anyway.
        do
        {
            Thread.SpinWait (10000);
            iterations++;

            // Backstop so a regression that stopped the clock advancing fails the test rather
            // than hanging it. Sized far above what the real runtime needs: at roughly 300us per
            // SpinWait(10000) a 5ms budget is tens of iterations there, and this bound holds even
            // if that estimate is wrong by three orders of magnitude. Under PawPrint the budget
            // is spent by the first iteration, so this exits after exactly one spin.
            if (iterations > 200000)
            {
                return 1;
            }
        }
        while (Environment.TickCount64 - start < 5);

        return 0;
    }
}

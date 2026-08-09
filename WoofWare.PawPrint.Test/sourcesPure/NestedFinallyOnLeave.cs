// A single `leave` may exit more than one enclosing protected region at once. ECMA-335
// III.3.55 requires it to run *every* `finally` between the leave site and the target,
// innermost first. PawPrint runs only the innermost one and jumps straight to the target,
// so every outer `finally` on the way out is silently skipped.
//
// `UnaryConstIlOp.leave` computes the full list via `Exceptions.findFinallyBlocksToRun`
// (correctly, sorted inner-to-outer) and then matches `finallyOffset :: _`, discarding the
// tail: it pushes one `ExceptionContinuationScope.FinallyHandler` whose continuation is
// `ResumeAfterFinally targetPc`, so after the innermost handler's `endfinally` control goes
// directly to the leave target.
//
// Whether C# produces a multi-region `leave` turns on where the branch target sits, which is
// why this is easy to miss: `OneRegion` below puts a statement after the loop, so the
// loop-exit label is still inside the outer `try` and its `leave` crosses one region — that
// one passes today, and is the control. The other four cross two or more and fail.
//
// It also turns on the optimization level, which matters because `Roslyn.compile` builds
// these guests unoptimized. See `LoopExitTwoRegions` for the measurement: a plain `break`
// gets you two single-region `leave`s under a debug build and only folds into one
// multi-region `leave` when optimized. Every scenario here is therefore spelled so that it
// crosses the regions under the harness's own settings.
//
// This is not a niche shape. `CancellationTokenSource.ExecuteCallbackHandlers` hits it: its
// `break` out of the callback-dispatch loop is the last thing in the outer `try`, so the
// `leave` skips the outer `finally` that sets `_state = NotifyingCompleteState` and clears
// `Registrations.ExecutingCallbackId`. Both stay stale forever, so a later
// `CancellationTokenRegistration.Dispose()` from a thread other than the one that ran
// `Cancel()` spins for ever in `Registrations.WaitForCallbackToComplete`. See the sibling
// `CancellationTokenRegistrationDisposeCrossThread.cs`, which is a guest-level witness of
// exactly that; unlike this file, that one does not terminate under PawPrint today.
//
// Handlers append to a shared ordered trace rather than bumping independent counters,
// because *order* is half the contract and counters cannot see it: an implementation that
// ran every handler but outer-to-inner would satisfy per-handler counts while violating
// ECMA-335. That is the realistic way to get a fix wrong, since `findFinallyBlocksToRun`
// hands back an inner-to-outer list that a chaining fix could easily fold the wrong way
// round. Each mode returns a distinct code so a partial fix is distinguishable from no fix,
// and a wrong-order fix from a missing-handler one.
class NestedFinallyOnLeave
{
    static int[] trace = new int[32];
    static int traceLen = 0;

    static void Reset()
    {
        traceLen = 0;
    }

    // Depth tags are per-scenario and always number the handlers innermost-first, so the
    // expected trace of a correct implementation reads in ascending depth order.
    static void Mark(int depth)
    {
        trace[traceLen] = depth;
        traceLen = traceLen + 1;
    }

    static bool TraceIs(int[] expected)
    {
        if (traceLen != expected.Length) return false;
        for (int i = 0; i < expected.Length; i++)
        {
            if (trace[i] != expected[i]) return false;
        }
        return true;
    }

    // One region crossed: the `leave` target is still inside the outer `try`, because
    // `after = 1` follows the loop. This already passes; it is the control that pins down
    // *which* leaves are broken.
    static int OneRegion()
    {
        Reset();
        int after = 0;
        try
        {
            int i = 0;
            while (true)
            {
                try
                {
                    i++;
                    if (i >= 3) break;
                }
                finally { Mark(1); }
            }
            after = 1;
        }
        finally { Mark(2); }

        if (after != 1) return 11;
        // Two loop-back exits of the inner try, then the `break`, then the outer handler.
        if (!TraceIs(new int[] { 1, 1, 1, 2 })) return 12;
        return 0;
    }

    // Leaving a dispatch loop from inside a nested `try`, straight past the outer handler:
    // the `ExecuteCallbackHandlers` shape, and the one that matters most here.
    //
    // The loop exit is spelled `goto`, not `break`, and that is load-bearing. C#'s `break`
    // does not reliably produce a multi-region `leave`: under the unoptimized compilation
    // `Roslyn.compile` uses (`CSharpCompilationOptions`' default is
    // `OptimizationLevel.Debug`), Roslyn lowers it to `leave.s` to a label that is *still
    // inside* the outer `try`, followed by a second `leave.s` out of it — two instructions
    // crossing one region each, which current PawPrint handles correctly. Measured: with
    // `break` here, the guest exits 32 (the `return` case) rather than failing on this one.
    // Optimized builds fold that pair into a single multi-region `leave`, which is why the
    // real CoreLib's `ExecuteCallbackHandlers` — compiled Release — exhibits it, and why the
    // sibling `CancellationTokenRegistrationDisposeCrossThread.cs`, which runs against that
    // real assembly, is what pins the exact production lowering. `goto` gets the same single
    // multi-region `leave` regardless of optimization, so this stays honest under the
    // harness's own compiler settings.
    static int LoopExitTwoRegions()
    {
        Reset();
        try
        {
            int i = 0;
            while (true)
            {
                try
                {
                    i++;
                    if (i >= 3) goto exited;
                }
                finally { Mark(1); }
            }
        }
        finally { Mark(2); }

    exited:
        if (!TraceIs(new int[] { 1, 1, 1, 2 })) return 21;
        return 0;
    }

    static int ReturnHelper()
    {
        try
        {
            try
            {
                return 7;
            }
            finally { Mark(1); }
        }
        finally { Mark(2); }
    }

    // Two regions crossed by `return`.
    static int ReturnTwoRegions()
    {
        Reset();
        int r = ReturnHelper();
        if (r != 7) return 31;
        if (!TraceIs(new int[] { 1, 2 })) return 32;
        return 0;
    }

    static int TripleHelper()
    {
        try
        {
            try
            {
                try
                {
                    return 9;
                }
                finally { Mark(1); }
            }
            finally { Mark(2); }
        }
        finally { Mark(3); }
    }

    // Three regions crossed: two handlers are dropped, not one — and a fix that chains them
    // in the wrong direction shows up here as the trace {3,2,1} rather than {1,2,3}.
    static int ReturnThreeRegions()
    {
        Reset();
        int r = TripleHelper();
        if (r != 9) return 41;
        if (!TraceIs(new int[] { 1, 2, 3 })) return 42;
        return 0;
    }

    // Two regions crossed by `goto`, i.e. not an artefact of how `break`/`return` are
    // lowered.
    static int GotoTwoRegions()
    {
        Reset();
        try
        {
            try
            {
                goto done;
            }
            finally { Mark(1); }
        }
        finally { Mark(2); }

    done:
        if (!TraceIs(new int[] { 1, 2 })) return 51;
        return 0;
    }

    static int Main(string[] args)
    {
        int r = OneRegion();
        if (r != 0) return r;

        r = LoopExitTwoRegions();
        if (r != 0) return r;

        r = ReturnTwoRegions();
        if (r != 0) return r;

        r = ReturnThreeRegions();
        if (r != 0) return r;

        r = GotoTwoRegions();
        if (r != 0) return r;

        return 0;
    }
}

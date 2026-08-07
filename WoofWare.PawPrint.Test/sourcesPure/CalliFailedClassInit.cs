using System;

// `calli` whose target throws *out of* the call, unwinding past the calling frame.
//
// `calli` cannot consume its function pointer until the call commits, because an intrinsic
// callee may suspend for class initialisation and require the instruction to re-execute
// (see CalliIntrinsicClassInit.cs). Detecting that requires looking at the calling frame
// after `callMethod` returns — but the frame is not guaranteed to still be there. When a
// class initialiser has already failed, the cached failure is dispatched synchronously from
// inside the call, and if the handler lives in an outer frame the dispatch unwinds the
// calling frame away before control comes back. Looking it up unconditionally is then a host
// crash ("Frame ... is not live") in place of an ordinary catchable guest exception.
//
// `Make` exists to put the `calli` one frame below the handler, so the unwind really does
// remove the frame rather than merely returning to it.

// The run counter deliberately lives outside Bad: reading a static of a type whose class
// initialiser has failed throws TypeInitializationException all over again, so a counter on
// Bad itself could not be observed without perturbing what is being measured.
class Counter
{
    public static int CctorRuns;
}

class Bad
{
    static Bad()
    {
        Counter.CctorRuns += 1;
        throw new Exception("boom from cctor");
    }

    public Bad()
    {
    }
}

public static unsafe class Program
{
    static Bad Make()
    {
        delegate*<Bad> p = &Activator.CreateInstance<Bad>;
        return p();
    }

    public static int Main(string[] args)
    {
        int caught = 0;

        // First call: Bad's cctor runs and throws.
        try { Make(); } catch (Exception) { caught += 1; }

        // Second call: the failure is cached, so it is dispatched from within the call
        // without re-running the cctor.
        try { Make(); } catch (Exception) { caught += 2; }

        if (caught != 3) return 10 + caught;

        // A failed class initialiser runs once and stays failed: the second call must have
        // been served from the cached failure rather than by re-running the cctor.
        if (Counter.CctorRuns != 1) return 20 + Counter.CctorRuns;

        return 0;
    }
}

using System;

// `calli` whose target suspends for class initialisation *inside* the call.
//
// `Activator.CreateInstance<T>()` is handled as an intrinsic. When T has a cctor that has
// not run, the intrinsic pushes that cctor frame and deliberately leaves the calling
// instruction's program counter unadvanced, so the call re-executes once the cctor returns.
// For an ordinary `call` that retry is free: the callee comes from a metadata token. For
// `calli` the callee came off the evaluation stack, so the retry only works if the function
// pointer is still there — i.e. `calli` must not consume it until the call has committed.
//
// The suspension is triggered by the generic argument (C), not by the declaring type
// (Activator), so initialising the target's declaring type up front does not avoid it.
//
// `Holder` exists so that C's cctor has an observable side effect that must have run exactly
// once by the time the constructed instance is returned.

class Holder
{
    public static int CctorRuns;
}

class C
{
    public static int Side;

    // Explicit cctor => not BeforeFieldInit, so initialisation is precisely ordered.
    static C()
    {
        Holder.CctorRuns += 1;
        Side = 5;
    }

    public C()
    {
    }
}

public static unsafe class Program
{
    public static int Main(string[] args)
    {
        delegate*<C> p = &Activator.CreateInstance<C>;

        C first = p();
        if (first == null) return 1;
        if (C.Side != 5) return 2;
        if (Holder.CctorRuns != 1) return 3;

        // A second call through the same pointer: the cctor has already run, so this one
        // commits immediately and must not re-run it.
        C second = p();
        if (second == null) return 4;
        if (Holder.CctorRuns != 1) return 5;
        if (ReferenceEquals(first, second)) return 6;

        return 0;
    }
}

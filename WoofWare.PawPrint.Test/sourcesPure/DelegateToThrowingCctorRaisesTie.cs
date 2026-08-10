using System;

// A delegate whose target's `.cctor` throws. Building the delegate must still succeed —
// construction is not a trigger — and each invocation must surface a
// `TypeInitializationException` wrapping the original failure.
//
// This is the only guest-visible route into two of the four outcomes the class-init step
// can produce at a delegate invocation. The first invocation takes the "run it now" path
// and unwinds when the initializer throws; the second takes the cached-failure path, which
// per ECMA-335 rethrows rather than retrying the initializer. That the *same* exception
// instance comes back both times is CLR identity semantics, verified on real .NET 10
// (`ReferenceEquals` is true across the two catches), and it is also what pins the second
// invocation onto the cached path rather than onto a re-run: a re-run would construct a
// fresh exception and would additionally increment the attempt counter below.
//
// Both invocations are inside a `try`, deliberately. An escaping type-initialization
// failure is not the subject here, and letting one escape would test the interpreter's
// unhandled-exception reporting instead of its class-init bookkeeping.

static class AttemptWitness
{
    public static int Attempts;
}

class ThrowingCctor
{
    static ThrowingCctor()
    {
        AttemptWitness.Attempts += 1;
        throw new InvalidOperationException ("boom");
    }

    public static int Value()
    {
        return 42;
    }
}

class Program
{
    static int Main(string[] args)
    {
        // Construction is not a trigger, so this must not throw.
        Func<int> f = ThrowingCctor.Value;

        if (AttemptWitness.Attempts != 0)
        {
            return 1;
        }

        TypeInitializationException first = null;

        try
        {
            f ();
            return 2;
        }
        catch (TypeInitializationException e)
        {
            first = e;
        }

        if (AttemptWitness.Attempts != 1)
        {
            return 3;
        }

        if (!(first.InnerException is InvalidOperationException))
        {
            return 4;
        }

        TypeInitializationException second = null;

        try
        {
            f ();
            return 5;
        }
        catch (TypeInitializationException e)
        {
            second = e;
        }

        // The initializer must not have run a second time.
        if (AttemptWitness.Attempts != 1)
        {
            return 6;
        }

        // ...and the cached instance is handed back, not a fresh one.
        if (!ReferenceEquals (first, second))
        {
            return 7;
        }

        return 0;
    }
}

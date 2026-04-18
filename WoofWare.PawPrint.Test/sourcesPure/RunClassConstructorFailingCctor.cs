using System;
using System.Runtime.CompilerServices;

// Exercises RuntimeHelpers.RunClassConstructor when the .cctor throws.
// The runtime must wrap the failure in TypeInitializationException and
// subsequent calls must rethrow the cached exception (not re-execute the .cctor).
// A side-channel counter proves the .cctor only ran once.

static class BoomCounter
{
    public static int CctorAttempts;
}

class Boom
{
    public static int Value;

    static Boom()
    {
        BoomCounter.CctorAttempts++;
        throw new InvalidOperationException("cctor failed");
    }
}

class Program
{
    static int Main(string[] args)
    {
        // Before any call, counter must be zero.
        if (BoomCounter.CctorAttempts != 0)
            return 1;

        // First call: triggers the failing .cctor, should throw TypeInitializationException.
        bool caught1 = false;
        try
        {
            RuntimeHelpers.RunClassConstructor(typeof(Boom).TypeHandle);
        }
        catch (TypeInitializationException)
        {
            caught1 = true;
        }

        if (!caught1)
            return 2;

        // The .cctor ran exactly once.
        if (BoomCounter.CctorAttempts != 1)
            return 3;

        // Second call: .cctor already failed, should rethrow cached TypeInitializationException
        // without re-executing the .cctor.
        bool caught2 = false;
        try
        {
            RuntimeHelpers.RunClassConstructor(typeof(Boom).TypeHandle);
        }
        catch (TypeInitializationException)
        {
            caught2 = true;
        }

        if (!caught2)
            return 4;

        // Still exactly one .cctor execution — the second throw came from the cache.
        if (BoomCounter.CctorAttempts != 1)
            return 5;

        return 0;
    }
}

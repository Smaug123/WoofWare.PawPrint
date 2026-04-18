using System;
using System.Runtime.CompilerServices;

// Exercises RuntimeHelpers.RunClassConstructor when the .cctor throws.
// The runtime must wrap the failure in TypeInitializationException and
// subsequent calls must rethrow the cached exception.

class Boom
{
    public static int Value;

    static Boom()
    {
        throw new InvalidOperationException("cctor failed");
    }
}

class Program
{
    static int Main(string[] args)
    {
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
            return 1;

        // Second call: .cctor already failed, should rethrow cached TypeInitializationException.
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
            return 2;

        return 0;
    }
}

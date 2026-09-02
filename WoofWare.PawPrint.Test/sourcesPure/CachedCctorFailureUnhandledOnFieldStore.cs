using System;

// A .cctor that throws leaves the type permanently failed: every later access rethrows the
// cached TypeInitializationException. The first access here is caught; the second is not, so
// the cached exception has to unwind out of Main and end the guest as an unhandled exception,
// exactly as a `throw` with no handler would. The second access is a static field store.
class Foo
{
    public static int X;

    static Foo()
    {
        throw new InvalidOperationException("cctor boom");
    }
}

class Program
{
    static int Main(string[] args)
    {
        try
        {
            Foo.X = 1;
        }
        catch (TypeInitializationException)
        {
        }

        Foo.X = 2;
        return 1;
    }
}

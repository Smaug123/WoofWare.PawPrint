using System;

// As CachedCctorFailureUnhandledOnFieldStore.cs, but the uncaught second access is a call to a
// static method, so the cached TypeInitializationException is raised by the callee's
// type-initialisation check on entry rather than by a field opcode.
class Foo
{
    static Foo()
    {
        throw new InvalidOperationException("cctor boom");
    }

    public static int Get()
    {
        return 3;
    }
}

class Program
{
    static int Main(string[] args)
    {
        try
        {
            Foo.Get();
        }
        catch (TypeInitializationException)
        {
        }

        return Foo.Get();
    }
}

using System;
using System.Runtime.CompilerServices;

// As CachedCctorFailureUnhandledOnFieldStore.cs, but the uncaught second access is
// RuntimeHelpers.RunClassConstructor, so the cached TypeInitializationException is raised from
// inside the runtime's own native entry point rather than by an opcode.
class Foo
{
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
            RuntimeHelpers.RunClassConstructor(typeof(Foo).TypeHandle);
        }
        catch (TypeInitializationException)
        {
        }

        RuntimeHelpers.RunClassConstructor(typeof(Foo).TypeHandle);
        return 1;
    }
}

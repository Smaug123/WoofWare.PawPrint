using System;

// A delegate whose target is `Activator.CreateInstance<T>()`, where `T` has a static
// constructor that has not yet run.
//
// `T`'s initialiser runs from inside the delegate's target: PawPrint services
// `Activator.CreateInstance<T>()` as an intrinsic that calls `T`'s constructor, and the
// initialiser runs as that constructor's prologue. Building the delegate must not run it, and
// invoking the delegate must run it exactly once.

static class Witness
{
    public static int Ran;
}

class Foo
{
    static Foo()
    {
        Witness.Ran += 1;
    }

    public Foo()
    {
    }
}

class Program
{
    static int Main(string[] args)
    {
        Func<Foo> f = Activator.CreateInstance<Foo>;

        // Building the delegate is not a use of `Foo`.
        if (Witness.Ran != 0)
        {
            return 1;
        }

        Foo x = f ();

        if (x == null)
        {
            return 2;
        }

        if (Witness.Ran != 1)
        {
            return 3;
        }

        return 0;
    }
}

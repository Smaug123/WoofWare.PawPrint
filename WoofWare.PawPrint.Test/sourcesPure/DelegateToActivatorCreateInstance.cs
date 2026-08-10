using System;

// A delegate whose target is `Activator.CreateInstance<T>()`, where `T` has a static
// constructor that has not yet run.
//
// This is the one target that can ask to suspend from *inside* the call rather than before
// it: PawPrint services `Activator.CreateInstance<T>()` as an intrinsic, and that intrinsic
// runs `T`'s initializer, so it reports `CallCommitment.SuspendedForClassInit` and expects
// its caller to re-execute once the initializer returns. Every call *opcode* can honour
// that — it leaves its program counter unadvanced and runs again. A delegate invocation
// cannot: by the time the target is called, the delegate's synthetic `Invoke` frame has
// already been popped, so there is no frame left to re-enter and nothing to re-execute.
//
// Note the class initialization this needs is not the one this file's fix supplies. That
// one covers the *target's declaring type* — here `System.Activator`, which is initialized
// long before — and runs before the frame is popped, which is precisely why it can suspend
// safely. `T`'s initializer is a second, later one, reached from within the intrinsic.

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

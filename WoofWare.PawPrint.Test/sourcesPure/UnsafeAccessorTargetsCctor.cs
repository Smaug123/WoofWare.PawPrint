using System.Runtime.CompilerServices;

// A `StaticMethod` accessor can name a type's `.cctor`, which is an ordinary static method as far
// as the lookup is concerned. Calling it does *not* replace the class initialiser: entering the
// target arms the declaring type's initialisation the way any static call does, so the body runs
// once for the initialisation and once for the explicit call.
//
// Measured on real .NET 10: one accessor invocation leaves the counter at 2, not 1.
public class TestUnsafeAccessorTargetsCctor
{
    private class Counted
    {
        public static int Runs;

        static Counted()
        {
            Runs++;
        }
    }

    [UnsafeAccessor(UnsafeAccessorKind.StaticMethod, Name = ".cctor")]
    private static extern void RunCctor(Counted c);

    private static int Run()
    {
        RunCctor(null);

        if (Counted.Runs != 2) return 1;

        // The type is initialised now, so a second explicit call adds exactly one more.
        RunCctor(null);

        if (Counted.Runs != 3) return 2;

        return 0;
    }

    public static int Main() => Run();
}

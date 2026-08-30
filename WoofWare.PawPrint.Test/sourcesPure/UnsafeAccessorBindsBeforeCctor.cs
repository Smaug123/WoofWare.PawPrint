using System;
using System.Runtime.CompilerServices;

// CoreCLR resolves an accessor's target as it compiles the stub, which is before the accessor's own
// declaring type is initialised. A binding failure therefore escapes without that type's `.cctor`
// having run: measured on real .NET 10, the counter is still zero inside the catch and only reaches
// one when the field is read afterwards.
public class TestUnsafeAccessorBindsBeforeCctor
{
    private class Target
    {
        private int _f = 3;
    }

    // The counter lives on a *different* type, because reading it is itself a use of whatever
    // declares it: a counter on `Accessors` could never be observed at zero, since the read would
    // initialise `Accessors` before returning.
    private static class Witness
    {
        public static int AccessorsCctorRuns;
    }

    private static class Accessors
    {
        static Accessors()
        {
            Witness.AccessorsCctorRuns++;
        }

        [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "NoSuchMethod")]
        public static extern int Missing(Target t);

        public static void Touch()
        {
        }
    }

    private static int Run()
    {
        int seenInsideCatch = -1;

        try
        {
            Accessors.Missing(new Target());
            return 1;
        }
        catch (MissingMethodException)
        {
            seenInsideCatch = Witness.AccessorsCctorRuns;
        }

        // The binding failure escaped without initialising the accessor's own type.
        if (seenInsideCatch != 0) return 2;

        // ... and an ordinary use of that type still initialises it.
        Accessors.Touch();
        if (Witness.AccessorsCctorRuns != 1) return 3;

        return 0;
    }

    public static int Main() => Run();
}

using System;
using System.Reflection.Emit;
using System.Runtime.CompilerServices;

public class Program
{
    // PawPrint declares that it does not support dynamic code, by seeding
    // `System.Runtime.CompilerServices.RuntimeFeature.IsDynamicCodeSupported=false` into
    // AppContext before any guest code runs. This is the same profile NativeAOT reports, and
    // the BCL is written to route around Reflection.Emit when it is set.
    //
    // Impure because it is a PawPrint-only fact: the differential oracle runs the guest on
    // the host runtime, which does support dynamic code and whose AppContext was seeded
    // before the test process started.
    public static int Main(string[] args)
    {
        if (RuntimeFeature.IsDynamicCodeSupported)
        {
            return 1;
        }

        // `IsDynamicCodeCompiled` is defined as `=> IsDynamicCodeSupported` on this runtime
        // flavour, so it must agree.
        if (RuntimeFeature.IsDynamicCodeCompiled)
        {
            return 2;
        }

        // The switch is also reachable by its documented name through the generic query.
        if (RuntimeFeature.IsSupported("IsDynamicCodeSupported"))
        {
            return 3;
        }

        // And the BCL acts on it: every Reflection.Emit entry point guards on
        // `AssemblyBuilder.EnsureDynamicCodeSupported`, which throws when the switch is off.
        // This is what turns "unimplemented native primitive" into a documented, catchable
        // failure a guest can handle.
        try
        {
            DynamicMethod dm = new DynamicMethod(
                "Add", typeof(int), new[] { typeof(int), typeof(int) }, typeof(Program));
            GC.KeepAlive(dm);
            return 4;
        }
        catch (PlatformNotSupportedException)
        {
            // expected
        }

        return 0;
    }
}

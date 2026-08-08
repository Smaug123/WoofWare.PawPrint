using System;
using System.Reflection;

// `Assembly.GetEntryAssembly()` bottoms out in the `AssemblyNative_GetEntryAssembly` QCall,
// which hands back CoreCLR's root assembly for the AppDomain. Each assertion below is
// sensitive to a different way of getting that wrong, so a handler that returns the wrong
// assembly, or a fresh object each call, fails loudly rather than silently passing.

public class AssemblyGetEntryAssemblyTests
{
    public static int Main(string[] argv)
    {
        Assembly entry = Assembly.GetEntryAssembly();

        // There is always a root assembly for a guest launched from its own image.
        if (entry == null) return 1;

        // The root assembly is the one declaring Main, and RuntimeAssembly objects are
        // cached per assembly identity, so this must be the very same instance that
        // ordinary reflection hands out for a type in that assembly.
        if (!object.ReferenceEquals(entry, typeof(AssemblyGetEntryAssemblyTests).Assembly)) return 2;

        // Repeated calls observe that cache rather than allocating afresh.
        if (!object.ReferenceEquals(entry, Assembly.GetEntryAssembly())) return 3;

        // CoreLib is loaded first and is emphatically not the entry assembly; this catches
        // a handler that answers with whatever assembly happens to be nearest to hand.
        if (object.ReferenceEquals(entry, typeof(object).Assembly)) return 4;

        return 0;
    }
}

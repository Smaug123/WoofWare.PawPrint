using System.Reflection;
using System.Reflection.Emit;
using System.Runtime.Loader;

// Defines one dynamic assembly and nothing else, so that the F# registration can
// check what making it took from the kernel's entropy pool: the runtime stamps
// the new module's version ID with secure random bytes, as `minipal_guid_v4_create`
// makes one.
//
// Returns 0 on success.
public static class Program
{
    public static int Main ()
    {
        // `DefineDynamicAssembly` asks for the caller's load context unless a contextual-reflection
        // scope names one, and PawPrint does not yet answer that question
        // (`AssemblyNative_GetLoadContextForAssembly`).
        using AssemblyLoadContext.ContextualReflectionScope scope = AssemblyLoadContext.Default.EnterContextualReflection ();

        AssemblyBuilder.DefineDynamicAssembly (new AssemblyName ("Stamped"), AssemblyBuilderAccess.Run);
        return 0;
    }
}

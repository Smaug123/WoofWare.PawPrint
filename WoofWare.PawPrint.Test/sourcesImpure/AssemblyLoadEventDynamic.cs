using System;
using System.Reflection;
using System.Reflection.Emit;
using System.Runtime.Loader;

// CoreCLR announces a dynamic assembly through `AppDomain.AssemblyLoad` from inside
// `AppDomain_CreateDynamicAssembly` (`Assembly::CreateDynamic`), so the handler has run, once, by
// the time `DefineDynamicAssembly` returns.
public class Program
{
    private static int s_loads;
    private static Assembly s_announced;

    private static void OnLoad (object sender, AssemblyLoadEventArgs args)
    {
        s_loads++;
        s_announced = args.LoadedAssembly;
    }

    public static int Main (string[] args)
    {
        AppDomain.CurrentDomain.AssemblyLoad += OnLoad;

        // `DefineDynamicAssembly` asks for the caller's load context unless a contextual-reflection
        // scope names one, and PawPrint does not yet answer that question
        // (`AssemblyNative_GetLoadContextForAssembly`).
        using AssemblyLoadContext.ContextualReflectionScope scope = AssemblyLoadContext.Default.EnterContextualReflection ();

        AssemblyBuilder builder =
            AssemblyBuilder.DefineDynamicAssembly (new AssemblyName ("Evented"), AssemblyBuilderAccess.Run);

        if (s_loads != 1)
            return 1;

        if (s_announced == null || s_announced.GetName ().Name != "Evented")
            return 2;

        if (!s_announced.IsDynamic)
            return 3;

        return builder == null ? 4 : 0;
    }
}

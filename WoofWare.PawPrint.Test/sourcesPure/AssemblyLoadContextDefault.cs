using System;
using System.Collections.Generic;
using System.Runtime.Loader;

// `AssemblyLoadContext.Default`: constructing it is what attaches the managed context to the
// runtime's default binder, through `AssemblyNative_InitializeAssemblyLoadContext`.
//
// Returns 0 on success, or the number of the first check that failed.

public static class Program
{
    public static int Main ()
    {
        AssemblyLoadContext context = AssemblyLoadContext.Default;

        if (context == null)
        {
            return 1;
        }

        // 2: a singleton, so the binder was attached once.
        if (!ReferenceEquals (context, AssemblyLoadContext.Default))
        {
            return 2;
        }

        if (context.Name != "Default")
        {
            return 3;
        }

        if (context.IsCollectible)
        {
            return 4;
        }

        // 5: the constructor registers the context, first, in the list of live ones.
        if (context.ToString () != "\"Default\" System.Runtime.Loader.DefaultAssemblyLoadContext #0")
        {
            return 5;
        }

        List<AssemblyLoadContext> all = new List<AssemblyLoadContext> (AssemblyLoadContext.All);

        if (all.Count != 1 || !ReferenceEquals (all[0], context))
        {
            return 6;
        }

        if (AssemblyLoadContext.CurrentContextualReflectionContext != null)
        {
            return 7;
        }

        return 0;
    }
}

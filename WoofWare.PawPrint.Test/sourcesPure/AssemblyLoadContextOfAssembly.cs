using System;
using System.Collections.Generic;
using System.Reflection;
using System.Runtime.Loader;

// `AssemblyLoadContext.GetLoadContext`: which context an assembly was loaded into, which CoreCLR
// answers through `AssemblyNative_GetLoadContextForAssembly`. Every assembly here is in the default
// context.
//
// Returns 0 on success, or the number of the first check that failed.

public static class Program
{
    public static int Main ()
    {
        AssemblyLoadContext defaultContext = AssemblyLoadContext.Default;

        if (!ReferenceEquals (AssemblyLoadContext.GetLoadContext (typeof (object).Assembly), defaultContext))
        {
            return 1;
        }

        if (!ReferenceEquals (AssemblyLoadContext.GetLoadContext (typeof (Program).Assembly), defaultContext))
        {
            return 2;
        }

        // 3: `Assemblies` keeps each loaded assembly whose context is this one, so the default
        // context's are all of them, in load order.
        List<Assembly> inDefault = new List<Assembly> (defaultContext.Assemblies);
        Assembly[] loaded = AppDomain.CurrentDomain.GetAssemblies ();

        if (inDefault.Count != loaded.Length)
        {
            return 3;
        }

        for (int i = 0; i < loaded.Length; i++)
        {
            if (!ReferenceEquals (inDefault[i], loaded[i]))
            {
                return 4;
            }
        }

        return 0;
    }
}

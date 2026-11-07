using System;
using System.Reflection;
using System.Runtime.Loader;

// `AssemblyLoadContext.Default.LoadFromAssemblyName`, which hands `AssemblyNative_InternalLoad`
// the default context itself rather than null. CoreCLR binds through that context's binder, which
// is the default binder, exactly as a plain `Assembly.Load` does.
public class Program
{
    public static int Main (string[] args)
    {
        Assembly loaded = AssemblyLoadContext.Default.LoadFromAssemblyName (new AssemblyName ("System.Security.Claims"));
        if (loaded.GetName ().Name != "System.Security.Claims") return 1;
        if (!ReferenceEquals (loaded, Assembly.Load ("System.Security.Claims"))) return 2;
        return 0;
    }
}

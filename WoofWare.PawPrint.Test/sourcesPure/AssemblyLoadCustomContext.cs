using System;
using System.Reflection;
using System.Runtime.Loader;

// A load into a context other than the default one. This context's `Load` answers null for
// everything, so `LoadFromAssemblyName` falls back to the default context and hands back the
// framework's own System.Security.Claims. PawPrint has one load context and refuses to bind
// into another.
public class Program
{
    sealed class Empty : AssemblyLoadContext
    {
        protected override Assembly Load (AssemblyName name) => null;
    }

    public static int Main (string[] args)
    {
        Assembly claims = new Empty ().LoadFromAssemblyName (new AssemblyName ("System.Security.Claims"));
        if (claims == null)
            return 1;
        if (!ReferenceEquals (claims, Assembly.Load ("System.Security.Claims")))
            return 2;
        return 0;
    }
}

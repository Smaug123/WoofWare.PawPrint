using System;
using System.IO;
using System.Reflection;
using System.Runtime.Loader;

// An `AssemblyLoadContext.Default.Resolving` handler, which CoreCLR calls from the binder
// (`AssemblyLoadContext.ResolveUsingEvent`) when a request in the default context finds nothing,
// before it reports `FileNotFoundException`. This one answers null, so the load still fails, but
// only after the handler has run.
public class Program
{
    public static int Main (string[] args)
    {
        int calls = 0;
        AssemblyLoadContext.Default.Resolving += (context, name) =>
        {
            calls++;
            return null;
        };

        try
        {
            Assembly.Load ("No.Such.Assembly");
            return 1;
        }
        catch (FileNotFoundException)
        {
        }

        return calls == 1 ? 0 : 2 + calls;
    }
}

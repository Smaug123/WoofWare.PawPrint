using System;
using System.IO;
using System.Reflection;

// Handlers added to `AppDomain.AssemblyResolve` and `AppDomain.AssemblyLoad` and removed again
// before any load: the binder then has nobody to call back, and binds and misses as if no handler
// had ever been added.
public class Program
{
    static Assembly Resolve (object sender, ResolveEventArgs e) => typeof (object).Assembly;

    static void Loaded (object sender, AssemblyLoadEventArgs e) => throw new InvalidOperationException ();

    public static int Main (string[] args)
    {
        AppDomain.CurrentDomain.AssemblyResolve += Resolve;
        AppDomain.CurrentDomain.AssemblyLoad += Loaded;
        AppDomain.CurrentDomain.AssemblyResolve -= Resolve;
        AppDomain.CurrentDomain.AssemblyLoad -= Loaded;

        if (!Assembly.Load ("System.Security.Claims").FullName.StartsWith ("System.Security.Claims, Version="))
            return 1;

        try
        {
            Assembly.Load ("No.Such.Assembly");
            return 2;
        }
        catch (FileNotFoundException)
        {
        }

        return 0;
    }
}

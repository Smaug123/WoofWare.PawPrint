using System;
using System.Reflection;

// `AppDomain.AssemblyLoad`, which CoreCLR raises for each assembly as it is loaded, and not again
// for one that is already loaded.
public class Program
{
    public static int Main (string[] args)
    {
        int claimsLoads = 0;

        AppDomain.CurrentDomain.AssemblyLoad += (sender, e) =>
        {
            if (e.LoadedAssembly.GetName ().Name == "System.Security.Claims")
                claimsLoads++;
        };

        Assembly.Load ("System.Security.Claims");
        if (claimsLoads != 1)
            return 1;

        Assembly.Load ("System.Security.Claims");
        if (claimsLoads != 1)
            return 2;

        return 0;
    }
}

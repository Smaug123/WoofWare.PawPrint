using System;
using System.Reflection;

// `BaseAssemblySpec::IsCoreLibSatellite`: `System.Private.CoreLib.resources` requested with
// CoreLib's own full public key is bound by a path of CoreCLR's own rather than by the binder, and
// a satellite that path cannot find is answered with null rather than `FileNotFoundException`,
// even from `Assembly.Load`, which asks to be thrown at.
public class Program
{
    public static int Main (string[] args)
    {
        var name = new AssemblyName { Name = "System.Private.CoreLib.resources" };
        name.SetPublicKey (typeof (object).Assembly.GetName ().GetPublicKey ());

        if (Assembly.Load (name) != null)
            return 1;

        // The key is compared as a blob, whichever way it was handed over: here as a token.
        var asToken = new AssemblyName { Name = "System.Private.CoreLib.resources" };
        asToken.SetPublicKeyToken (typeof (object).Assembly.GetName ().GetPublicKey ());

        if (Assembly.Load (asToken) != null)
            return 2;

        return 0;
    }
}

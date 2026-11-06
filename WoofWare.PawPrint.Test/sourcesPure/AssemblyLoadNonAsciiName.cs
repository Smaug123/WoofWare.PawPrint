using System;
using System.IO;
using System.Reflection;

// A simple name outside ASCII. CoreCLR folds case by rules of its own, so a dotless i answers to an
// already-loaded name spelled with `i`, and converts the name to UTF-8 on the way in, so an unpaired
// surrogate comes back in a failure's `FileName` as U+FFFD.
public class Program
{
    public static int Main (string[] args)
    {
        Assembly claims = Assembly.Load ("System.Security.Claims");
        if (!ReferenceEquals (claims, Assembly.Load ("System.Securıty.Claims")))
            return 1;

        try
        {
            Assembly.Load (new AssemblyName { Name = "No.Such\uD800" });
            return 2;
        }
        catch (FileNotFoundException e)
        {
            if (!e.FileName.StartsWith ("No.Such�,"))
                return 3;
        }

        return 0;
    }
}

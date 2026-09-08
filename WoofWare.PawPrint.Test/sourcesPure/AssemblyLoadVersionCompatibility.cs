using System;
using System.IO;
using System.Reflection;

// `AssemblyBinderCommon::IsCompatibleAssemblyVersion`, row by row, against the framework's
// System.Security.Claims. The shared framework this project pins versions its assemblies 10.0.0.0,
// and every row below is stated against that: a request binds when each component it specifies
// is no higher than the found one, comparing from the major and stopping at the first component
// the request leaves out or the first strict inequality. (A display name cannot carry a
// one-component version, so the major-only rows of the rule are not reachable from here.)
public class Program
{
    static bool Binds (string version)
    {
        try
        {
            Assembly.Load ("System.Security.Claims, Version=" + version);
            return true;
        }
        catch (FileNotFoundException)
        {
            return false;
        }
    }

    public static int Main (string[] args)
    {
        (string, bool)[] rows =
        {
            ("0.0.0.0", true),
            ("9.9.9.9", true),
            ("10.0.0.0", true),
            ("10.0.0.1", false),
            ("10.0.1", false),
            ("10.1", false),
            ("10.0", true),
            ("10.0.0", true),
            ("11.0", false),
        };

        for (int i = 0; i < rows.Length; i++)
        {
            if (Binds (rows[i].Item1) != rows[i].Item2)
                return i + 1;
        }

        return 0;
    }
}

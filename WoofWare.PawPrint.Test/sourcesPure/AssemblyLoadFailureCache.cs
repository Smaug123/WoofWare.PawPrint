using System;
using System.IO;
using System.Reflection;

// The binder remembers its misses for the rest of the process, keyed by simple name (ignoring
// case), version and culture but not by architecture, content type or token. So a miss for a
// request naming an architecture the framework's IL-only assembly does not have poisons the plain
// request for the same name, while a miss at too high a version or in a culture with no satellite
// does not. A request that bound is remembered ahead of that, and survives a later miss for the
// same name. Each sequence below starts from a framework assembly nothing else here has touched.
public class Program
{
    static bool Binds (string name)
    {
        try
        {
            Assembly.Load (name);
            return true;
        }
        catch (FileNotFoundException)
        {
            return false;
        }
    }

    public static int Main (string[] args)
    {
        // An IL-only assembly is `MSIL`, so that architecture binds when nothing has missed yet.
        if (!Binds ("System.Security.Claims, processorArchitecture=MSIL")) return 1;
        if (!Binds ("System.Security.Claims")) return 2;

        // x86 misses, and poisons the plain and MSIL requests -- under another spelling too.
        if (Binds ("system.collections, processorArchitecture=x86")) return 3;
        if (Binds ("System.Collections")) return 4;
        if (Binds ("System.Collections, processorArchitecture=MSIL")) return 5;
        // A version-specific request has its own key.
        if (!Binds ("System.Collections, Version=4.0.0.0")) return 6;
        if (Binds ("System.Collections")) return 7;

        // A version miss leaves the plain request alone.
        if (Binds ("System.Linq, Version=99.0.0.0")) return 8;
        if (!Binds ("System.Linq")) return 9;

        // A plain request that bound survives a later architecture miss.
        if (!Binds ("System.Runtime")) return 10;
        if (Binds ("System.Runtime, processorArchitecture=AMD64")) return 11;
        if (!Binds ("System.Runtime")) return 12;

        // A content-type miss poisons the plain request the same way.
        if (Binds ("System.Memory, ContentType=WindowsRuntime")) return 13;
        if (Binds ("System.Memory")) return 14;

        // `Type.GetType` swallows the miss, and is poisoned by it all the same.
        if (Type.GetType ("System.Text.Json.JsonSerializer, System.Text.Json, processorArchitecture=x86") != null) return 15;
        if (Type.GetType ("System.Text.Json.JsonSerializer, System.Text.Json") != null) return 16;

        return 0;
    }
}

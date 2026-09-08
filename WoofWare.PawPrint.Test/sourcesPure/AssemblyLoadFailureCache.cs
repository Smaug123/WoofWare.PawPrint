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

        // The key keeps the token: a miss under one token leaves the tokenless request and
        // the real token alone, and poisons only requests under that token.
        if (Binds ("System.Runtime.InteropServices, PublicKeyToken=0000000000000000, processorArchitecture=x86")) return 17;
        if (!Binds ("System.Runtime.InteropServices")) return 18;
        if (Binds ("System.Runtime.InteropServices, PublicKeyToken=0000000000000000")) return 19;
        if (!Binds ("System.Runtime.InteropServices, PublicKeyToken=b03f5f7f11d50a3a")) return 20;

        // IA64 and ARM read as MSIL, the way CoreCLR bit-tests the field, and bind.
        if (!Binds ("System.Threading, processorArchitecture=IA64")) return 21;
        if (!Binds ("System.Threading.Tasks, processorArchitecture=ARM")) return 22;

        // A success is remembered under its own spelling; a miss under any spelling.
        if (!Binds ("System.Buffers")) return 23;
        if (Binds ("system.buffers, processorArchitecture=x86")) return 24;
        if (!Binds ("System.Buffers")) return 25;
        if (Binds ("SYSTEM.BUFFERS")) return 26;
        if (Binds ("system.buffers")) return 27;

        // A version whose major is unspecified is no version at all in the key.
        try
        {
            Assembly.Load (new AssemblyName { Name = "System.ComponentModel", Version = new Version (65535, 1, 2, 3), ProcessorArchitecture = ProcessorArchitecture.X86 });
            return 28;
        }
        catch (FileNotFoundException)
        {
        }
        if (Binds ("System.ComponentModel")) return 29;

        // The runtime recorded the entry assembly's own identity, and the identity each
        // reference was bound under, when it loaded them: a miss under exactly that identity
        // does not cost the identity itself.
        string entry = typeof (Program).Assembly.FullName;
        if (Binds (entry + ", processorArchitecture=x86")) return 30;
        if (!Binds (entry)) return 31;
        // Naming the type is what makes this a reference the runtime bound, whichever
        // assemblies this file was compiled against.
        string referenced = typeof (System.Text.RegularExpressions.Regex).Assembly.FullName;
        if (Binds (referenced + ", processorArchitecture=x86")) return 32;
        if (!Binds (referenced)) return 33;

        // An assembly a request read from disk has only the request's spec recorded: the same
        // miss under its identity does cost the identity. System.Security.Claims was read by
        // the plain request at the top.
        string claims = Assembly.Load ("System.Security.Claims").FullName;
        if (Binds (claims + ", processorArchitecture=x86")) return 34;
        if (Binds (claims)) return 35;

        // A bound spec's version is compared no further than the first unspecified component,
        // so `65535.1.2.3` is the unversioned request that bound.
        if (!Binds ("System.Collections.Concurrent")) return 36;
        if (Binds ("System.Collections.Concurrent, processorArchitecture=x86")) return 37;
        try
        {
            Assembly.Load (new AssemblyName { Name = "System.Collections.Concurrent", Version = new Version (65535, 1, 2, 3) });
        }
        catch (FileNotFoundException)
        {
            return 38;
        }

        // `Type.GetType` swallows the miss, and is poisoned by it all the same.
        if (Type.GetType ("System.Text.Json.JsonSerializer, System.Text.Json, processorArchitecture=x86") != null) return 15;
        if (Type.GetType ("System.Text.Json.JsonSerializer, System.Text.Json") != null) return 16;

        return 0;
    }
}

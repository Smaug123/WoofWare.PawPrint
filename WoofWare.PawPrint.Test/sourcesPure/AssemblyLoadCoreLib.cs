using System;
using System.IO;
using System.Reflection;

// `BaseAssemblySpec::IsCoreLib`, consulted by `AppDomain::IsCached`/`FindCachedFile` before any
// ordinary binding rule runs: a spec whose simple name is CoreLib's answers with the system
// assembly whatever else the spec demands. Measured on .NET 10: the version, public key token and
// processor architecture are all ignored (so the peel-off precedes even the architecture-validity
// check that x86 requests otherwise fail), the name comparison ignores case, and the file-name
// spelling `System.Private.CoreLib.dll` is accepted too.
//
// The spec's culture is equally ignored (measured on .NET 10 under ICU: `Culture=fr-FR` still
// answers the system assembly), but no row here can state that: the emulated environment runs
// invariant globalization, where parsing a display name that names a culture throws
// CultureNotFoundException before any binder is consulted -- measured on .NET 10 with
// DOTNET_SYSTEM_GLOBALIZATION_INVARIANT=1, which throws identically.
public class Program
{
    static bool IsSystemAssembly (string name)
    {
        try
        {
            return Assembly.Load (name).Equals (typeof (object).Assembly);
        }
        catch (FileNotFoundException)
        {
            return false;
        }
    }

    static bool IsSystemAssembly (AssemblyName name)
    {
        try
        {
            return Assembly.Load (name).Equals (typeof (object).Assembly);
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
            ("System.Private.CoreLib", true),
            ("System.Private.CoreLib, Version=99.0.0.0", true),
            ("system.private.corelib", true),
            ("SYSTEM.PRIVATE.CORELIB, Version=99.0.0.0", true),
            ("System.Private.CoreLib, PublicKeyToken=1111111111111111", true),
            ("System.Private.CoreLib, Version=1.0.0.0, PublicKeyToken=1111111111111111", true),
            ("System.Private.CoreLib, processorArchitecture=x86", true),
            ("System.Private.CoreLib.dll", true),
            ("System.Private.CoreLib2", false),
            // The peel-off also precedes public-key validation: this blob under any other
            // simple name is SecurityException ("Invalid assembly public key.").
            ("System.Private.CoreLib, PublicKey=0011", true),
        };

        for (int i = 0; i < rows.Length; i++)
        {
            if (IsSystemAssembly (rows[i].Item1) != rows[i].Item2)
                return i + 1;
        }

        // `IsCoreLib` also accepts a comma immediately after the prefix, which is how an
        // `AssemblyName` whose `Name` field holds a whole display string arrives: `Load(string)`
        // parses the display name apart, so only the `AssemblyName` overload can deliver one.
        // The file-name spelling gets no such suffix arm.
        if (!IsSystemAssembly (new AssemblyName { Name = typeof (object).Assembly.FullName }))
            return 20;
        if (!IsSystemAssembly (new AssemblyName { Name = "System.Private.CoreLib,garbage" }))
            return 21;
        if (IsSystemAssembly (new AssemblyName { Name = "System.Private.CoreLib.dll,x" }))
            return 22;
        if (IsSystemAssembly (new AssemblyName { Name = "System.Private.CoreLibX" }))
            return 23;

        return 0;
    }
}

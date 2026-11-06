using System;
using System.Reflection;
using System.Runtime.CompilerServices;

// A module whose `<Module>` type has a method: the static constructor C# emits to call a module
// initializer. CoreCLR then loads `<Module>` as the module's global type
// (`Module::NeedsGlobalMethodTable`), which `RuntimeModule.RuntimeType` reads through
// `ModuleHandle.GetModuleType`, checked here through that internal property. The initializer is a
// method of `Program`, not a global one, so reflection still finds no global methods; the static
// constructor is not a method to `GetMethods`. `ModuleGlobalMembers.cs` is the module without one.
//
// Returns 0 on success, or the number of the first check that failed.

public static class Program
{
    [ModuleInitializer]
    internal static void Initialize ()
    {
    }

    public static int Main ()
    {
        Module module = typeof (Program).Module;

        PropertyInfo globalType = module.GetType ().GetProperty ("RuntimeType", BindingFlags.NonPublic | BindingFlags.Instance);
        if (globalType == null)
        {
            return 1;
        }

        if (!(globalType.GetValue (module) is Type type))
        {
            return 2;
        }

        if (type.Name != "<Module>" || type.Module != module || type.Namespace != null)
        {
            Console.WriteLine ($"{type.Namespace}.{type.Name}");
            return 3;
        }

        if (module.GetMethods ().Length != 0)
        {
            return 4;
        }

        if (module.GetFields (BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Static).Length != 0)
        {
            return 5;
        }

        // Read again, the property gives the type it keeps.
        if (!ReferenceEquals (globalType.GetValue (module), type))
        {
            return 6;
        }

        return 0;
    }
}

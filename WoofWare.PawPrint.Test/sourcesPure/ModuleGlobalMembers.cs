using System;
using System.Reflection;

// A module's global members, which reflection reads through the module's `<Module>` type:
// `RuntimeModule.RuntimeType` asks `ModuleHandle.GetModuleType` for it. CoreCLR loads `<Module>`
// only if it has a method or a field (`Module::NeedsGlobalMethodTable`), and C# gives it neither
// without a module initializer, so here there is no global type. CoreLib reads that as "no
// members"; the type itself is checked through the internal property that holds it.
// `ModuleInitializerGlobalType.cs` is the module that has one.
//
// Returns 0 on success, or the number of the first check that failed.

public static class Program
{
    public static int Main ()
    {
        Module module = typeof (Program).Module;

        if (module.GetMethods ().Length != 0)
        {
            return 1;
        }

        if (module.GetFields ().Length != 0)
        {
            return 2;
        }

        if (module.GetMethod ("Main") != null)
        {
            return 3;
        }

        if (module.GetField ("Anything") != null)
        {
            return 4;
        }

        if (module.GetMethods ().Length != 0)
        {
            return 5;
        }

        PropertyInfo globalType = module.GetType ().GetProperty ("RuntimeType", BindingFlags.NonPublic | BindingFlags.Instance);
        if (globalType == null)
        {
            return 6;
        }

        if (globalType.GetValue (module) != null)
        {
            return 7;
        }

        return 0;
    }
}

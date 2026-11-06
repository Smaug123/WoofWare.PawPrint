using System;
using System.Reflection;
using System.Reflection.Emit;

// `RuntimeModule_GetFullyQualifiedName` on a Reflection.Emit module answers `<In Memory Module>`,
// whatever the module was named. `ModuleBuilder.Name` reaches it through the builder's internal
// `RuntimeModule`, while `ModuleBuilder.FullyQualifiedName` is answered in managed code and does
// not. Measured on .NET 10 to exit 0.
//
// Returns 0 on success, or the number of the first check that failed.

public static class Program
{
    public static int Main()
    {
        AssemblyBuilder assembly = AssemblyBuilder.DefineDynamicAssembly(new AssemblyName("Dyn"), AssemblyBuilderAccess.Run);
        ModuleBuilder module = assembly.DefineDynamicModule("Dyn");

        string name = module.Name;
        if (name != "<In Memory Module>")
        {
            Console.WriteLine($"Name: {name}");
            return 1;
        }

        // CoreCLR's QCall allocates its result afresh on each call.
        if (object.ReferenceEquals(module.Name, module.Name)) return 2;

        return 0;
    }
}

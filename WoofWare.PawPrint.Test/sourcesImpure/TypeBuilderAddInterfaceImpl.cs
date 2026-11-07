using System;
using System.Reflection;
using System.Reflection.Emit;

// `TypeBuilder_AddInterfaceImpl`, through `TypeBuilder.AddInterfaceImplementation`: an interface added
// after `DefineType`, once from another assembly (a TypeRef) and once from the module itself (a TypeDef).
// The emitter finds an interface the type already implements before it adds one, so adding one twice
// is one row; `DefineType` does not, so naming one twice there is two. The test harness reads the rows
// from the dynamic module's scope. Measured on .NET 10 to exit 0.
//
// Returns 0 on success, or the number of the first check that failed.

public interface IMarker
{
}

public static class Program
{
    public static int Main ()
    {
        AssemblyBuilder assembly = AssemblyBuilder.DefineDynamicAssembly (new AssemblyName ("Dyn"), AssemblyBuilderAccess.Run);
        ModuleBuilder module = assembly.DefineDynamicModule ("Dyn");

        TypeBuilder dynamicInterface = module.DefineType (
            "IDynamic",
            TypeAttributes.Public | TypeAttributes.Interface | TypeAttributes.Abstract);

        Type createdInterface = dynamicInterface.CreateType ()!;

        // An interface named twice in `DefineType`.
        TypeBuilder both = module.DefineType (
            "Both",
            TypeAttributes.Public | TypeAttributes.Class,
            typeof (object),
            new[] { typeof (IMarker), typeof (IMarker) });

        // Interfaces added afterwards, one of them twice.
        TypeBuilder added = module.DefineType ("Added", TypeAttributes.Public | TypeAttributes.Class);
        added.AddInterfaceImplementation (typeof (IMarker));
        added.AddInterfaceImplementation (dynamicInterface);
        added.AddInterfaceImplementation (typeof (IMarker));

        // An interface added to a type that `DefineType` gave it already.
        both.AddInterfaceImplementation (typeof (IMarker));

        Type addedType = added.CreateType ()!;
        Type bothType = both.CreateType ()!;

        if (!typeof (IMarker).IsAssignableFrom (addedType) || !createdInterface.IsAssignableFrom (addedType))
        {
            return 1;
        }

        if (Activator.CreateInstance (addedType) is not IMarker)
        {
            return 2;
        }

        if (Activator.CreateInstance (bothType) is not IMarker)
        {
            return 3;
        }

        return 0;
    }
}

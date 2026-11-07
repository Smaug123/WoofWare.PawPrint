using System;
using System.Reflection;
using System.Reflection.Emit;

// `TypeBuilder_SetParentType`, which `TypeBuilder.CreateType` calls for a type with a parent, writing
// the parent's token over the one `DefineType` wrote. These types are a static class, which gets no
// default constructor, and a value type, which gets none either, so with no methods none of them
// reaches `TypeBuilder_SetMethodIL`. Measured on .NET 10 to exit 0.
//
// Returns 0 on success, or the number of the first check that failed.

public static class Program
{
    public static int Main ()
    {
        AssemblyBuilder assembly = AssemblyBuilder.DefineDynamicAssembly (new AssemblyName ("Dyn"), AssemblyBuilderAccess.Run);
        ModuleBuilder module = assembly.DefineDynamicModule ("Dyn");

        TypeAttributes staticClass =
            TypeAttributes.Public | TypeAttributes.Class | TypeAttributes.Abstract | TypeAttributes.Sealed;

        // The parent `DefineType` gave it, written again.
        TypeBuilder plain = module.DefineType ("Plain", staticClass);

        // A parent other than the one `DefineType` gave it.
        TypeBuilder reparented = module.DefineType ("Reparented", staticClass);
        reparented.SetParent (typeof (Exception));

        // A value type, whose parent `DefineType` was given.
        TypeBuilder value = module.DefineType (
            "Value",
            TypeAttributes.Public | TypeAttributes.Sealed | TypeAttributes.SequentialLayout,
            typeof (ValueType));

        if (plain.BaseType != typeof (object) || reparented.BaseType != typeof (Exception))
        {
            return 1;
        }

        Type? plainType = plain.CreateType ();

        if (plainType is null || plainType.BaseType != typeof (object) || plainType.Name != "Plain")
        {
            return 2;
        }

        Type? reparentedType = reparented.CreateType ();

        if (reparentedType is null || reparentedType.BaseType != typeof (Exception))
        {
            return 3;
        }

        if (!reparentedType.IsSubclassOf (typeof (Exception)) || !reparentedType.IsAbstract || !reparentedType.IsSealed)
        {
            return 4;
        }

        Type? valueType = value.CreateType ();

        if (valueType is null || valueType.BaseType != typeof (ValueType) || !valueType.IsValueType)
        {
            return 5;
        }

        return 0;
    }
}

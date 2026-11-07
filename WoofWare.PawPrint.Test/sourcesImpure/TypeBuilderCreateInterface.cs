using System;
using System.Reflection;
using System.Reflection.Emit;

// `TypeBuilder_TermCreateClass`, through `TypeBuilder.CreateType`, for interfaces without methods. An
// interface has no parent, so `CreateType` does not call `TypeBuilder_SetParentType`, and with no
// methods it calls `TypeBuilder_SetMethodIL` for none (it calls it even for an abstract method, with an
// empty body). The type it returns is the created type's `RuntimeType`, which a second `CreateType`
// returns again. A type defined after others are created can be created too. Measured on .NET 10 to
// exit 0.
//
// Returns 0 on success, or the number of the first check that failed.

public static class Program
{
    public static int Main ()
    {
        AssemblyBuilder assembly = AssemblyBuilder.DefineDynamicAssembly (new AssemblyName ("Dyn"), AssemblyBuilderAccess.Run);
        ModuleBuilder module = assembly.DefineDynamicModule ("Dyn");

        TypeBuilder empty = module.DefineType ("N.IEmpty", TypeAttributes.Public | TypeAttributes.Interface | TypeAttributes.Abstract);

        TypeBuilder derived = module.DefineType (
            "IDerived",
            TypeAttributes.Public | TypeAttributes.Interface | TypeAttributes.Abstract,
            null,
            new[] { (Type) empty });

        if (empty.IsCreated ())
        {
            return 1;
        }

        Type? created = empty.CreateType ();

        if (created is null || created is TypeBuilder || !empty.IsCreated ())
        {
            return 2;
        }

        if (!created.IsInterface || created.Name != "IEmpty" || created.Namespace != "N")
        {
            return 3;
        }

        if (created.MetadataToken != 0x02000002)
        {
            return 4;
        }

        // `CreateType` on a created type returns the type it returned the first time.
        if (!ReferenceEquals (empty.CreateType (), created))
        {
            return 5;
        }

        // An interface that extends the one created above.
        Type? second = derived.CreateType ();

        if (second is null || !second.IsInterface || second.Name != "IDerived" || second.MetadataToken != 0x02000003)
        {
            return 6;
        }

        if (ReferenceEquals (second, created))
        {
            return 7;
        }

        // A type defined after others are created, which extends the module's metadata while it holds
        // created types.
        TypeBuilder after = module.DefineType (
            "IAfter",
            TypeAttributes.Public | TypeAttributes.Interface | TypeAttributes.Abstract,
            null,
            new[] { (Type) empty, derived });

        if (after.MetadataToken != 0x02000004)
        {
            return 8;
        }

        Type? third = after.CreateType ();

        if (third is null || !third.IsInterface || third.Name != "IAfter" || third.MetadataToken != 0x02000004)
        {
            return 9;
        }

        return 0;
    }
}

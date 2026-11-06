using System;
using System.Reflection;
using System.Reflection.Emit;

// The TypeDef rows a dynamic module gains from `ModuleBuilder.DefineType`, before any type is
// created: each new type's token is the next TypeDef RID, after `<Module>`'s; a type's interfaces
// get their TypeRefs before its parent does; a parent may be a type of the same module; and a name
// the module's metadata already holds, which the managed check does not know of (`<Module>`), is
// refused with an ArgumentException and adds no row. Measured on .NET 10.
//
// Nothing here loads a type that is not yet created: CoreCLR's loader would raise the TypeResolve
// event for one rather than load it.
//
// Returns 0 on success, or the number of the first check that failed.

public interface IThing
{
    int Value { get; }
}

public static class Program
{
    public static int Main ()
    {
        AssemblyBuilder assembly = AssemblyBuilder.DefineDynamicAssembly (new AssemblyName ("Dyn"), AssemblyBuilderAccess.Run);
        ModuleBuilder module = assembly.DefineDynamicModule ("Dyn");

        // IThing is listed twice: CoreCLR's emitter does not deduplicate a type's interfaces.
        TypeBuilder a = module.DefineType (
            "N.A",
            TypeAttributes.Public | TypeAttributes.Class,
            typeof (object),
            new[] { typeof (IThing), typeof (IDisposable), typeof (IThing) });
        if (a.MetadataToken != 0x02000002)
        {
            Console.WriteLine ($"A: {a.MetadataToken:x8}");
            return 1;
        }

        // The interfaces' TypeRefs are asked for before the parent's.
        if (module.GetTypeMetadataToken (typeof (IThing)) != 0x01000001)
        {
            return 2;
        }

        if (module.GetTypeMetadataToken (typeof (IDisposable)) != 0x01000002)
        {
            return 3;
        }

        if (module.GetTypeMetadataToken (typeof (object)) != 0x01000003)
        {
            return 4;
        }

        // A class with no parent given derives from object, whose TypeRef is found again.
        TypeBuilder b = module.DefineType ("B", TypeAttributes.NotPublic);
        if (b.MetadataToken != 0x02000003)
        {
            return 5;
        }

        // An interface has no parent.
        TypeBuilder i = module.DefineType ("I", TypeAttributes.Public | TypeAttributes.Interface | TypeAttributes.Abstract);
        if (i.MetadataToken != 0x02000004)
        {
            return 6;
        }

        // A parent defined in this module is named by its TypeDef, and adds no TypeRef.
        TypeBuilder c = module.DefineType ("C", TypeAttributes.Public, a, new Type[] { i });
        if (c.MetadataToken != 0x02000005)
        {
            return 7;
        }

        if (module.GetTypeMetadataToken (typeof (string)) != 0x01000004)
        {
            return 8;
        }

        // `<Module>` is not among the module's TypeBuilders, so only the emitter's own duplicate
        // check refuses it.
        try
        {
            module.DefineType ("<Module>");
            return 9;
        }
        catch (ArgumentException e)
        {
            if (e.Message != "Duplicate type name within an assembly.")
            {
                Console.WriteLine (e.Message);
                return 10;
            }
        }

        // The managed check refuses a name a TypeBuilder already has.
        try
        {
            module.DefineType ("N.A");
            return 11;
        }
        catch (ArgumentException)
        {
        }

        // Neither refusal added a row.
        TypeBuilder d = module.DefineType ("D.E.F", TypeAttributes.Public | TypeAttributes.Sealed);
        if (d.MetadataToken != 0x02000006)
        {
            Console.WriteLine ($"D: {d.MetadataToken:x8}");
            return 12;
        }

        return 0;
    }
}

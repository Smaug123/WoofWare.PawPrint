using System;
using System.Reflection;
using System.Reflection.Emit;

// `TypeBuilder_DefineMethod` and `TypeBuilder_SetMethodImpl`, through `MethodBuilder.MetadataToken`,
// which defines a method's row the first time it is read and then sets its implementation flags.
// A dynamic module's emitter checks for no duplicate method, so each definition is a new row, in
// definition order across types. The rows' flags, implementation flags and signatures are not visible
// before `CreateType`; the test harness reads them from the dynamic module's scope. Measured on
// .NET 10 to exit 0.
//
// Returns 0 on success, or the number of the first check that failed.

public static class Program
{
    public static int Main ()
    {
        AssemblyBuilder assembly = AssemblyBuilder.DefineDynamicAssembly (new AssemblyName ("Dyn"), AssemblyBuilderAccess.Run);
        ModuleBuilder module = assembly.DefineDynamicModule ("Dyn");
        TypeBuilder a = module.DefineType ("A", TypeAttributes.Public);
        TypeBuilder b = module.DefineType ("B", TypeAttributes.Public);

        if (a.MetadataToken != 0x02000002 || b.MetadataToken != 0x02000003)
        {
            return 1;
        }

        // Tokens follow the order in which they are first read, across types.
        MethodBuilder b1 = b.DefineMethod ("M", MethodAttributes.Public, typeof (void), Type.EmptyTypes);
        MethodBuilder a1 = a.DefineMethod ("M", MethodAttributes.Public, typeof (void), Type.EmptyTypes);

        if (b1.MetadataToken != 0x06000001)
        {
            return 2;
        }

        if (a1.MetadataToken != 0x06000002)
        {
            return 3;
        }

        // The same type, name and signature again is a new method.
        MethodBuilder a1Again = a.DefineMethod ("M", MethodAttributes.Family, typeof (void), Type.EmptyTypes);
        a1Again.SetImplementationFlags (MethodImplAttributes.NoInlining);

        if (a1Again.MetadataToken != 0x06000003)
        {
            return 4;
        }

        // A signature naming a type of another assembly, and a type of this module.
        MethodBuilder n = a.DefineMethod ("N", MethodAttributes.Public | MethodAttributes.Static, typeof (int), new[] { typeof (Version), b });

        if (n.MetadataToken != 0x06000004)
        {
            return 5;
        }

        // Set after the row is defined: the emitter overwrites the column, keeping its low 16 bits.
        n.SetImplementationFlags ((MethodImplAttributes) 0x10000 | MethodImplAttributes.AggressiveInlining);

        // The emitter marks these two names special.
        MethodBuilder ctor = a.DefineMethod (".ctor", MethodAttributes.Public, typeof (void), new[] { typeof (int) });
        MethodBuilder gap = a.DefineMethod ("_VtblGap1_2", MethodAttributes.Public, typeof (void), Type.EmptyTypes);

        if (ctor.MetadataToken != 0x06000005 || gap.MetadataToken != 0x06000006)
        {
            return 6;
        }

        // The reserved bits and those above 16 are dropped.
        MethodBuilder reserved = a.DefineMethod (
            "Reserved",
            MethodAttributes.Public | MethodAttributes.RTSpecialName | MethodAttributes.HasSecurity | MethodAttributes.RequireSecObject | (MethodAttributes) 0x30000,
            typeof (void),
            Type.EmptyTypes);

        if (reserved.MetadataToken != 0x06000007)
        {
            return 7;
        }

        // A global method belongs to `<Module>`.
        MethodBuilder global = module.DefineGlobalMethod ("G", MethodAttributes.Public | MethodAttributes.Static, typeof (void), Type.EmptyTypes);

        if (global.MetadataToken != 0x06000008)
        {
            return 8;
        }

        // A constructor is a method too.
        ConstructorBuilder constructor = a.DefineConstructor (MethodAttributes.Public, CallingConventions.Standard, new[] { typeof (string) });

        if (constructor.MetadataToken != 0x06000009)
        {
            return 9;
        }

        // Read again, a token is the one handed out.
        if (b1.MetadataToken != 0x06000001 || a1Again.MetadataToken != 0x06000003)
        {
            return 10;
        }

        return 0;
    }
}

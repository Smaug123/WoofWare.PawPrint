using System;
using System.Reflection;
using System.Reflection.Emit;

// `TypeBuilder_DefineMethodImpl`, through `TypeBuilder.DefineMethodOverride`, which adds a MethodImpl
// row naming the type, the body's MethodDef and the declaration's MethodDef or MemberRef. A dynamic
// module's emitter checks for no duplicate MethodImpl, so the same override twice is two rows. The rows
// are not visible before `CreateType`; the test harness reads them from the dynamic module's scope.
// Measured on .NET 10 to exit 0.
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

        // An interface of this module, whose method's token is a MethodDef.
        TypeBuilder i = module.DefineType ("I", TypeAttributes.Public | TypeAttributes.Interface | TypeAttributes.Abstract);
        MethodBuilder iM = i.DefineMethod (
            "M",
            MethodAttributes.Public | MethodAttributes.Abstract | MethodAttributes.Virtual | MethodAttributes.NewSlot
            | MethodAttributes.HideBySig,
            typeof (void),
            Type.EmptyTypes);

        TypeBuilder a = module.DefineType ("A", TypeAttributes.Public | TypeAttributes.Class, typeof (object), new[] { typeof (IThing), i });
        TypeBuilder b = module.DefineType ("B", TypeAttributes.Public | TypeAttributes.Class, typeof (object), new Type[] { i });

        if (i.MetadataToken != 0x02000002 || a.MetadataToken != 0x02000003 || b.MetadataToken != 0x02000004)
        {
            return 1;
        }

        MethodAttributes implementation =
            MethodAttributes.Public | MethodAttributes.Virtual | MethodAttributes.Final | MethodAttributes.HideBySig
            | MethodAttributes.NewSlot;

        MethodBuilder bM = b.DefineMethod ("M", implementation, typeof (void), Type.EmptyTypes);
        MethodBuilder aGet = a.DefineMethod ("get_Value", implementation | MethodAttributes.SpecialName, typeof (int), Type.EmptyTypes);
        MethodBuilder aM = a.DefineMethod ("M", implementation, typeof (void), Type.EmptyTypes);
        MethodBuilder aToString = a.DefineMethod ("ToString", MethodAttributes.Public | MethodAttributes.Virtual | MethodAttributes.HideBySig, typeof (string), Type.EmptyTypes);

        if (iM.MetadataToken != 0x06000001 || bM.MetadataToken != 0x06000002 || aGet.MetadataToken != 0x06000003
            || aM.MetadataToken != 0x06000004 || aToString.MetadataToken != 0x06000005)
        {
            return 2;
        }

        MethodInfo thingValue = typeof (IThing).GetProperty (nameof (IThing.Value))!.GetMethod!;
        MethodInfo objectToString = typeof (object).GetMethod (nameof (object.ToString))!;

        // A declaration of this module: B, defined after A, has its row first.
        b.DefineMethodOverride (bM, iM);

        // A declaration of another assembly, whose token is a MemberRef.
        a.DefineMethodOverride (aGet, thingValue);
        a.DefineMethodOverride (aM, iM);

        // The same override again.
        a.DefineMethodOverride (aGet, thingValue);

        // A class's virtual method, rather than an interface's.
        a.DefineMethodOverride (aToString, objectToString);

        if (module.GetMethodMetadataToken (thingValue) != 0x0A000001)
        {
            return 3;
        }

        if (module.GetMethodMetadataToken (objectToString) != 0x0A000002)
        {
            return 4;
        }

        // The managed caller refuses a body of another type before it asks for any token.
        try
        {
            a.DefineMethodOverride (bM, iM);
            return 5;
        }
        catch (ArgumentException)
        {
        }

        try
        {
            a.DefineMethodOverride (objectToString, objectToString);
            return 6;
        }
        catch (ArgumentException)
        {
        }

        return 0;
    }
}

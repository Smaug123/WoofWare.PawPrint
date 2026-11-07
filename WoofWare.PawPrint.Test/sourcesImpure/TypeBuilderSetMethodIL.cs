using System;
using System.Reflection;
using System.Reflection.Emit;

// `TypeBuilder_SetMethodIL`, which `TypeBuilder.CreateType` calls for every method of the type, an
// abstract one included. CoreCLR writes each body with a fat header, because `CreateType` always passes
// a local signature, even an empty one, and the emitter gives it a `StandAloneSig` row, one for each
// distinct signature. The types are static classes and an interface, which get no default
// constructor. Measured on .NET 10 to exit 0.
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

        MethodAttributes publicStatic = MethodAttributes.Public | MethodAttributes.Static;

        // An interface whose abstract method gets an empty body.
        TypeBuilder i = module.DefineType ("I", TypeAttributes.Public | TypeAttributes.Interface | TypeAttributes.Abstract);

        i.DefineMethod (
            "M",
            MethodAttributes.Public | MethodAttributes.Abstract | MethodAttributes.Virtual | MethodAttributes.NewSlot
            | MethodAttributes.HideBySig,
            typeof (int),
            Type.EmptyTypes);

        Type? created = i.CreateType ();

        if (created is null || created.GetMethod ("M") is null)
        {
            return 1;
        }

        TypeBuilder t = module.DefineType ("T", staticClass);

        // No locals: the empty local signature.
        MethodBuilder fortyTwo = t.DefineMethod ("FortyTwo", publicStatic, typeof (int), Type.EmptyTypes);
        ILGenerator il = fortyTwo.GetILGenerator ();
        il.Emit (OpCodes.Ldc_I4_S, (sbyte) 42);
        il.Emit (OpCodes.Ret);

        // One local, so another signature.
        MethodBuilder seven = t.DefineMethod ("Seven", publicStatic, typeof (int), Type.EmptyTypes);
        il = seven.GetILGenerator ();
        il.DeclareLocal (typeof (int));
        il.Emit (OpCodes.Ldc_I4_7);
        il.Emit (OpCodes.Stloc_0);
        il.Emit (OpCodes.Ldloc_0);
        il.Emit (OpCodes.Ret);

        // The same local signature as `Seven`, and locals not zeroed.
        MethodBuilder sum = t.DefineMethod ("Sum", publicStatic, typeof (int), new[] { typeof (int), typeof (int) });
        sum.InitLocals = false;
        il = sum.GetILGenerator ();
        il.DeclareLocal (typeof (int));
        il.Emit (OpCodes.Ldarg_0);
        il.Emit (OpCodes.Ldarg_1);
        il.Emit (OpCodes.Add);
        il.Emit (OpCodes.Stloc_0);
        il.Emit (OpCodes.Ldloc_0);
        il.Emit (OpCodes.Ret);

        // A call to another method of the module, whose token is a MethodDef.
        MethodBuilder twice = t.DefineMethod ("Twice", publicStatic, typeof (int), Type.EmptyTypes);
        il = twice.GetILGenerator ();
        il.Emit (OpCodes.Call, fortyTwo);
        il.Emit (OpCodes.Call, fortyTwo);
        il.Emit (OpCodes.Add);
        il.Emit (OpCodes.Ret);

        Type? type = t.CreateType ();

        if (type is null)
        {
            return 2;
        }

        if ((int) type.GetMethod ("FortyTwo")!.Invoke (null, null)! != 42)
        {
            return 3;
        }

        if ((int) type.GetMethod ("Seven")!.Invoke (null, null)! != 7)
        {
            return 4;
        }

        if ((int) type.GetMethod ("Sum")!.Invoke (null, new object[] { 3, 4 })! != 7)
        {
            return 5;
        }

        if ((int) type.GetMethod ("Twice")!.Invoke (null, null)! != 84)
        {
            return 6;
        }

        return 0;
    }
}

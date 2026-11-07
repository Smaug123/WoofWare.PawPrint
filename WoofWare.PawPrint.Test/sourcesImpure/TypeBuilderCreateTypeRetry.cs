using System;
using System.Reflection;
using System.Reflection.Emit;

// `TypeBuilder.CreateType` called again after it threw partway, as `TypeBuilder_SetMethodIL` has already
// run for the methods before the one that failed. An abstract method's empty body sets no RVA, so the
// retry gives it again harmlessly. A method with code is released once given its body in a `Run`
// assembly, and the retry refuses to bake it again. (A `RunAndCollect` assembly keeps the body, and its
// retry gives the body a second time; PawPrint refuses a collectible assembly.) Measured on .NET 10 to
// exit 0.
//
// Returns 0 on success, or the number of the first check that failed.

public static class Program
{
    static MethodAttributes PublicStatic => MethodAttributes.Public | MethodAttributes.Static;

    // A static method that branches to `label`, which the caller marks or does not.
    static (MethodBuilder, ILGenerator, Label) Branching (TypeBuilder type, string name)
    {
        MethodBuilder method = type.DefineMethod (name, PublicStatic, typeof (int), Type.EmptyTypes);
        ILGenerator il = method.GetILGenerator ();
        Label label = il.DefineLabel ();
        il.Emit (OpCodes.Br, label);
        return (method, il, label);
    }

    static int Retried (string name, bool withAbstract, bool withFirst)
    {
        AssemblyBuilder assembly = AssemblyBuilder.DefineDynamicAssembly (new AssemblyName (name), AssemblyBuilderAccess.Run);
        ModuleBuilder module = assembly.DefineDynamicModule (name);

        TypeAttributes attributes = withAbstract
            ? TypeAttributes.Public | TypeAttributes.Interface | TypeAttributes.Abstract
            : TypeAttributes.Public | TypeAttributes.Class | TypeAttributes.Abstract | TypeAttributes.Sealed;

        TypeBuilder type = module.DefineType ("T", attributes);

        if (withAbstract)
        {
            type.DefineMethod (
                "M",
                MethodAttributes.Public | MethodAttributes.Abstract | MethodAttributes.Virtual
                | MethodAttributes.NewSlot | MethodAttributes.HideBySig,
                typeof (int),
                Type.EmptyTypes);
        }

        // A method with code, baked before the one that fails.
        if (withFirst)
        {
            MethodBuilder first = type.DefineMethod ("First", PublicStatic, typeof (int), Type.EmptyTypes);
            ILGenerator firstIl = first.GetILGenerator ();
            firstIl.Emit (OpCodes.Ldc_I4_5);
            firstIl.Emit (OpCodes.Ret);
        }

        (MethodBuilder second, ILGenerator il, Label label) = Branching (type, "Second");

        try
        {
            type.CreateType ();
            return 1;
        }
        catch (ArgumentException)
        {
        }

        if (type.IsCreated ())
        {
            return 2;
        }

        il.MarkLabel (label);
        il.Emit (OpCodes.Ldc_I4_6);
        il.Emit (OpCodes.Ret);

        Type? created;

        try
        {
            created = type.CreateType ();
        }
        catch (InvalidOperationException)
        {
            // `Run` releases a baked method's body, and baking it again is refused.
            return withFirst ? 100 : 3;
        }

        if (created is null)
        {
            return 4;
        }

        if ((int) created.GetMethod ("Second")!.Invoke (null, null)! != 6)
        {
            return 5;
        }

        if (withAbstract && created.GetMethod ("M") is null)
        {
            return 6;
        }

        return 0;
    }

    public static int Main ()
    {
        // `First`'s body is released once it is given, so the retry refuses to bake it again.
        if (Retried ("Released", false, true) != 100)
        {
            return 10;
        }

        // Only the abstract method was given its empty body before `Second` failed.
        int withAbstract = Retried ("WithAbstract", true, false);

        if (withAbstract != 0)
        {
            return 20 + withAbstract;
        }

        return 0;
    }
}

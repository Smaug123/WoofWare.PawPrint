using System;
using System.Reflection;
using System.Reflection.Emit;

// `TypeBuilder_SetMethodIL` for bodies with exception handlers: a typed catch, a finally, a filter and a
// fault, each in small clauses, and a catch whose try block is too long for a small clause, so its
// section is fat. Each method is invoked once its type is created. Measured on .NET 10 to exit 0.
//
// Returns 0 on success, or the number of the first check that failed.

public static class Program
{
    static MethodAttributes PublicStatic => MethodAttributes.Public | MethodAttributes.Static;

    static ILGenerator Define (TypeBuilder type, string name) =>
        type.DefineMethod (name, PublicStatic, typeof (int), Type.EmptyTypes).GetILGenerator ();

    public static int Main ()
    {
        AssemblyBuilder assembly = AssemblyBuilder.DefineDynamicAssembly (new AssemblyName ("Dyn"), AssemblyBuilderAccess.Run);
        ModuleBuilder module = assembly.DefineDynamicModule ("Dyn");

        TypeBuilder t = module.DefineType (
            "T",
            TypeAttributes.Public | TypeAttributes.Class | TypeAttributes.Abstract | TypeAttributes.Sealed);

        ConstructorInfo invalidOperation = typeof (InvalidOperationException).GetConstructor (Type.EmptyTypes)!;

        // try { throw new InvalidOperationException (); } catch (InvalidOperationException) { result = 1; }
        ILGenerator il = Define (t, "Caught");
        LocalBuilder result = il.DeclareLocal (typeof (int));
        il.BeginExceptionBlock ();
        il.Emit (OpCodes.Newobj, invalidOperation);
        il.Emit (OpCodes.Throw);
        il.BeginCatchBlock (typeof (InvalidOperationException));
        il.Emit (OpCodes.Pop);
        il.Emit (OpCodes.Ldc_I4_1);
        il.Emit (OpCodes.Stloc, result);
        il.EndExceptionBlock ();
        il.Emit (OpCodes.Ldloc, result);
        il.Emit (OpCodes.Ret);

        // try { result = 1; } finally { result += 10; }
        il = Define (t, "Finally");
        result = il.DeclareLocal (typeof (int));
        il.BeginExceptionBlock ();
        il.Emit (OpCodes.Ldc_I4_1);
        il.Emit (OpCodes.Stloc, result);
        il.BeginFinallyBlock ();
        il.Emit (OpCodes.Ldloc, result);
        il.Emit (OpCodes.Ldc_I4_S, (sbyte) 10);
        il.Emit (OpCodes.Add);
        il.Emit (OpCodes.Stloc, result);
        il.EndExceptionBlock ();
        il.Emit (OpCodes.Ldloc, result);
        il.Emit (OpCodes.Ret);

        // try { throw new InvalidOperationException (); } catch when (true) { result = 3; }
        il = Define (t, "Filtered");
        result = il.DeclareLocal (typeof (int));
        il.BeginExceptionBlock ();
        il.Emit (OpCodes.Newobj, invalidOperation);
        il.Emit (OpCodes.Throw);
        il.BeginExceptFilterBlock ();
        il.Emit (OpCodes.Pop);
        il.Emit (OpCodes.Ldc_I4_1);
        il.BeginCatchBlock (null);
        il.Emit (OpCodes.Pop);
        il.Emit (OpCodes.Ldc_I4_3);
        il.Emit (OpCodes.Stloc, result);
        il.EndExceptionBlock ();
        il.Emit (OpCodes.Ldloc, result);
        il.Emit (OpCodes.Ret);

        // try { try { throw ...; } fault { result = 4; } } catch (InvalidOperationException) { result += 1; }
        il = Define (t, "Faulted");
        result = il.DeclareLocal (typeof (int));
        il.BeginExceptionBlock ();
        il.BeginExceptionBlock ();
        il.Emit (OpCodes.Newobj, invalidOperation);
        il.Emit (OpCodes.Throw);
        il.BeginFaultBlock ();
        il.Emit (OpCodes.Ldc_I4_4);
        il.Emit (OpCodes.Stloc, result);
        il.EndExceptionBlock ();
        il.BeginCatchBlock (typeof (InvalidOperationException));
        il.Emit (OpCodes.Pop);
        il.Emit (OpCodes.Ldloc, result);
        il.Emit (OpCodes.Ldc_I4_1);
        il.Emit (OpCodes.Add);
        il.Emit (OpCodes.Stloc, result);
        il.EndExceptionBlock ();
        il.Emit (OpCodes.Ldloc, result);
        il.Emit (OpCodes.Ret);

        // A try block of more than 255 bytes, which a small clause cannot hold.
        il = Define (t, "Long");
        result = il.DeclareLocal (typeof (int));
        il.BeginExceptionBlock ();

        for (int i = 0; i < 300; i++)
        {
            il.Emit (OpCodes.Nop);
        }

        il.Emit (OpCodes.Newobj, invalidOperation);
        il.Emit (OpCodes.Throw);
        il.BeginCatchBlock (typeof (InvalidOperationException));
        il.Emit (OpCodes.Pop);
        il.Emit (OpCodes.Ldc_I4_5);
        il.Emit (OpCodes.Stloc, result);
        il.EndExceptionBlock ();
        il.Emit (OpCodes.Ldloc, result);
        il.Emit (OpCodes.Ret);

        Type? type = t.CreateType ();

        if (type is null)
        {
            return 1;
        }

        string[] names = { "Caught", "Finally", "Filtered", "Faulted", "Long" };
        int[] expected = { 1, 11, 3, 5, 5 };

        for (int i = 0; i < names.Length; i++)
        {
            if ((int) type.GetMethod (names[i])!.Invoke (null, null)! != expected[i])
            {
                return 10 + i;
            }
        }

        return 0;
    }
}

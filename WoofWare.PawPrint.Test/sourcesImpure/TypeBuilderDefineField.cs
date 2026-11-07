using System;
using System.Reflection;
using System.Reflection.Emit;

// `TypeBuilder_DefineField`, through `TypeBuilder.DefineField`: a static field and an instance field,
// each read and written by emitted IL that names it by its FieldDef token, and the fields of a value
// type. Each method is invoked once: with dynamic code supported, reflection's second invocation of a
// method emits an invoke stub, which is not what this tests. The managed caller clears the reserved
// flags (`FieldAttributes.ReservedMask`), and the emitter marks a field named `value__` special; the test
// harness reads the flags from the dynamic module's scope. Measured on .NET 10 to exit 0.
//
// Returns 0 on success, or the number of the first check that failed.

public static class Program
{
    public static int Main ()
    {
        AssemblyBuilder assembly = AssemblyBuilder.DefineDynamicAssembly (new AssemblyName ("Dyn"), AssemblyBuilderAccess.Run);
        ModuleBuilder module = assembly.DefineDynamicModule ("Dyn");

        // static class Counter { static int count; static int Increment () => ++count; }
        TypeBuilder counter = module.DefineType (
            "Counter",
            TypeAttributes.Public | TypeAttributes.Class | TypeAttributes.Abstract | TypeAttributes.Sealed);

        FieldBuilder count = counter.DefineField ("count", typeof (int), FieldAttributes.Private | FieldAttributes.Static);

        if (count.MetadataToken != 0x04000001)
        {
            return 1;
        }

        MethodBuilder increment = counter.DefineMethod (
            "Increment",
            MethodAttributes.Public | MethodAttributes.Static,
            typeof (int),
            Type.EmptyTypes);

        ILGenerator il = increment.GetILGenerator ();
        il.Emit (OpCodes.Ldsfld, count);
        il.Emit (OpCodes.Ldc_I4_1);
        il.Emit (OpCodes.Add);
        il.Emit (OpCodes.Stsfld, count);
        il.Emit (OpCodes.Ldsfld, count);
        il.Emit (OpCodes.Ret);

        // static int IncrementTwice () { Increment (); return Increment (); }
        MethodBuilder incrementTwice = counter.DefineMethod (
            "IncrementTwice",
            MethodAttributes.Public | MethodAttributes.Static,
            typeof (int),
            Type.EmptyTypes);

        il = incrementTwice.GetILGenerator ();
        il.Emit (OpCodes.Call, increment);
        il.Emit (OpCodes.Pop);
        il.Emit (OpCodes.Call, increment);
        il.Emit (OpCodes.Ret);

        // class Box { long value; public long Swap (long v) { long old = value; value = v; return old; } }
        TypeBuilder box = module.DefineType ("Box", TypeAttributes.Public | TypeAttributes.Class);

        // `HasDefault` is reserved: `RuntimeFieldBuilder` clears it before the emitter sees it.
        FieldBuilder value = box.DefineField ("value", typeof (long), FieldAttributes.Private | FieldAttributes.HasDefault);

        if (value.MetadataToken != 0x04000002)
        {
            return 2;
        }

        MethodBuilder swap = box.DefineMethod (
            "Swap",
            MethodAttributes.Public | MethodAttributes.HideBySig,
            typeof (long),
            new[] { typeof (long) });

        il = swap.GetILGenerator ();
        LocalBuilder old = il.DeclareLocal (typeof (long));
        il.Emit (OpCodes.Ldarg_0);
        il.Emit (OpCodes.Ldfld, value);
        il.Emit (OpCodes.Stloc, old);
        il.Emit (OpCodes.Ldarg_0);
        il.Emit (OpCodes.Ldarg_1);
        il.Emit (OpCodes.Stfld, value);
        il.Emit (OpCodes.Ldloc, old);
        il.Emit (OpCodes.Ret);

        MethodBuilder get = box.DefineMethod ("Get", MethodAttributes.Public | MethodAttributes.HideBySig, typeof (long), Type.EmptyTypes);
        il = get.GetILGenerator ();
        il.Emit (OpCodes.Ldarg_0);
        il.Emit (OpCodes.Ldfld, value);
        il.Emit (OpCodes.Ret);

        // A value type with two fields, one of them named `value__`, which the emitter marks special.
        TypeBuilder pair = module.DefineType (
            "Pair",
            TypeAttributes.Public | TypeAttributes.Sealed | TypeAttributes.SequentialLayout,
            typeof (ValueType));

        pair.DefineField ("value__", typeof (int), FieldAttributes.Public);
        pair.DefineField ("second", typeof (string), FieldAttributes.Public);

        Type counterType = counter.CreateType ()!;

        if ((int) counterType.GetMethod ("IncrementTwice")!.Invoke (null, null)! != 2)
        {
            return 3;
        }

        Type boxType = box.CreateType ()!;
        object instance = Activator.CreateInstance (boxType)!;

        if ((long) boxType.GetMethod ("Swap")!.Invoke (instance, new object[] { 7L })! != 0L || (long) boxType.GetMethod ("Get")!.Invoke (instance, null)! != 7L)
        {
            return 4;
        }

        Type pairType = pair.CreateType ()!;

        if (!pairType.IsValueType || Activator.CreateInstance (pairType) is null)
        {
            return 5;
        }

        return 0;
    }
}

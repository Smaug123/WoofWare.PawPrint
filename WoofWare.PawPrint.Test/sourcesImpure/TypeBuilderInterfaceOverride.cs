using System;
using System.Reflection;
using System.Reflection.Emit;

// Issue #2091's guest: a type emitted with `TypeBuilder`, deriving from `object` and implementing an
// interface through an explicit method override, created, instantiated and called through the
// interface. Measured on .NET 10 to exit 0.
//
// Returns 0 on success, or 1 if the emitted getter returned something other than 42.

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
        TypeBuilder type = module.DefineType ("Thing", TypeAttributes.Public | TypeAttributes.Class, typeof (object), new[] { typeof (IThing) });

        MethodBuilder getter = type.DefineMethod (
            "get_Value",
            MethodAttributes.Public | MethodAttributes.Virtual | MethodAttributes.Final | MethodAttributes.HideBySig
            | MethodAttributes.NewSlot | MethodAttributes.SpecialName,
            typeof (int),
            Type.EmptyTypes);
        ILGenerator il = getter.GetILGenerator ();
        il.Emit (OpCodes.Ldc_I4, 42);
        il.Emit (OpCodes.Ret);
        type.DefineMethodOverride (getter, typeof (IThing).GetProperty (nameof (IThing.Value))!.GetMethod!);

        IThing instance = (IThing)Activator.CreateInstance (type.CreateType ())!;
        Console.WriteLine ($"instance.Value = {instance.Value}");
        return instance.Value == 42 ? 0 : 1;
    }
}

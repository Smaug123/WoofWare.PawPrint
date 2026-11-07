using System;
using System.Collections.Generic;
using System.Reflection;
using System.Reflection.Emit;

// `ModuleBuilder_GetTokenFromTypeSpec`, through `ILGenerator.Emit` of `castclass` and `isinst` naming a
// generic instantiation, whose token is a TypeSpec holding the instantiation's signature. The emitter
// finds a TypeSpec with the same signature before it adds one, so naming `List<int>` twice is one row.
// Each method is invoked once. Measured on .NET 10 to exit 0.
//
// Returns 0 on success, or the number of the first check that failed.

public static class Program
{
    public static int Main ()
    {
        AssemblyBuilder assembly = AssemblyBuilder.DefineDynamicAssembly (new AssemblyName ("Dyn"), AssemblyBuilderAccess.Run);
        ModuleBuilder module = assembly.DefineDynamicModule ("Dyn");

        TypeBuilder t = module.DefineType (
            "T",
            TypeAttributes.Public | TypeAttributes.Class | TypeAttributes.Abstract | TypeAttributes.Sealed);

        // static bool IsListOfInt (object o) => o is List<int>;
        MethodBuilder isList = t.DefineMethod ("IsListOfInt", MethodAttributes.Public | MethodAttributes.Static, typeof (bool), new[] { typeof (object) });
        ILGenerator il = isList.GetILGenerator ();
        il.Emit (OpCodes.Ldarg_0);
        il.Emit (OpCodes.Isinst, typeof (List<int>));
        il.Emit (OpCodes.Ldnull);
        il.Emit (OpCodes.Cgt_Un);
        il.Emit (OpCodes.Ret);

        // static object CastToListOfInt (object o) => (List<int>) o;
        MethodBuilder cast = t.DefineMethod ("CastToListOfInt", MethodAttributes.Public | MethodAttributes.Static, typeof (object), new[] { typeof (object) });
        il = cast.GetILGenerator ();
        il.Emit (OpCodes.Ldarg_0);
        il.Emit (OpCodes.Castclass, typeof (List<int>));
        il.Emit (OpCodes.Ret);

        // static bool IsDictionary (object o) => o is Dictionary<string, int>;
        MethodBuilder isDictionary = t.DefineMethod ("IsDictionary", MethodAttributes.Public | MethodAttributes.Static, typeof (bool), new[] { typeof (object) });
        il = isDictionary.GetILGenerator ();
        il.Emit (OpCodes.Ldarg_0);
        il.Emit (OpCodes.Isinst, typeof (Dictionary<string, int>));
        il.Emit (OpCodes.Ldnull);
        il.Emit (OpCodes.Cgt_Un);
        il.Emit (OpCodes.Ret);

        // `List<int>` was named twice, so it is one TypeSpec, the first.
        if (module.GetTypeMetadataToken (typeof (List<int>)) != 0x1B000001 || module.GetTypeMetadataToken (typeof (Dictionary<string, int>)) != 0x1B000002)
        {
            return 1;
        }

        Type type = t.CreateType ()!;
        List<int> list = new List<int> ();

        if (!(bool) type.GetMethod ("IsListOfInt")!.Invoke (null, new object[] { list })!)
        {
            return 2;
        }

        if (!ReferenceEquals (type.GetMethod ("CastToListOfInt")!.Invoke (null, new object[] { list }), list))
        {
            return 3;
        }

        if ((bool) type.GetMethod ("IsDictionary")!.Invoke (null, new object[] { list })!)
        {
            return 4;
        }

        return 0;
    }
}

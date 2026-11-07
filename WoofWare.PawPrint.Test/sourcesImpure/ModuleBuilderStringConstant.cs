using System;
using System.Reflection;
using System.Reflection.Emit;

// `ModuleBuilder_GetStringConstant`, through `ILGenerator.Emit (OpCodes.Ldstr, ...)` and
// `ModuleBuilder.GetStringMetadataToken`: each string's token is its offset in the module's `#US` heap,
// the same string twice is one entry, and the heap starts with a space, so the first string the guest
// asks for is at offset 5. Emitted methods load the strings, a lone surrogate and a string long enough
// for a four-byte length prefix among them. Each method is invoked once. Measured on .NET 10 to exit 0.
//
// Returns 0 on success, or the number of the first check that failed.

public static class Program
{
    static MethodBuilder Returning (TypeBuilder type, string name, string value)
    {
        MethodBuilder method = type.DefineMethod (name, MethodAttributes.Public | MethodAttributes.Static, typeof (string), Type.EmptyTypes);
        ILGenerator il = method.GetILGenerator ();
        il.Emit (OpCodes.Ldstr, value);
        il.Emit (OpCodes.Ret);
        return method;
    }

    public static int Main ()
    {
        AssemblyBuilder assembly = AssemblyBuilder.DefineDynamicAssembly (new AssemblyName ("Dyn"), AssemblyBuilderAccess.Run);
        ModuleBuilder module = assembly.DefineDynamicModule ("Dyn");

        TypeBuilder t = module.DefineType (
            "T",
            TypeAttributes.Public | TypeAttributes.Class | TypeAttributes.Abstract | TypeAttributes.Sealed);

        string surrogate = "\uD800";
        string longString = new string ('y', 8200);

        Returning (t, "Hello", "hello");
        Returning (t, "HelloAgain", "hello");
        Returning (t, "Empty", "");
        Returning (t, "Surrogate", surrogate);
        Returning (t, "Long", longString);

        // "hello" is 5 bytes after the heap's leading empty entry and space, and one entry.
        if (module.GetStringMetadataToken ("hello") != 0x70000005)
        {
            return 1;
        }

        // A space of its own: the heap's first, whose final byte differs, is not this one.
        if (module.GetStringMetadataToken (" ") == 0x70000001)
        {
            return 2;
        }

        Type type = t.CreateType ()!;
        string hello = (string) type.GetMethod ("Hello")!.Invoke (null, null)!;
        string helloAgain = (string) type.GetMethod ("HelloAgain")!.Invoke (null, null)!;

        if (hello != "hello" || helloAgain != "hello")
        {
            return 3;
        }

        // `ldstr` interns the literal, so both methods load the same object.
        if (!ReferenceEquals (hello, helloAgain))
        {
            return 4;
        }

        if ((string) type.GetMethod ("Empty")!.Invoke (null, null)! != "")
        {
            return 5;
        }

        if ((string) type.GetMethod ("Surrogate")!.Invoke (null, null)! != surrogate)
        {
            return 6;
        }

        if ((string) type.GetMethod ("Long")!.Invoke (null, null)! != longString)
        {
            return 7;
        }

        return 0;
    }
}

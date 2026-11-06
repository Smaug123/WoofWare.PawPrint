using System;
using System.Reflection;
using System.Reflection.Emit;

// `ModuleBuilder_GetMemberRefOfMethodInfo`, through `ModuleBuilder.GetMethodMetadataToken`, for methods
// of other assemblies whose signatures name no type by token. A dynamic module's emitter finds a
// MemberRef with the same parent, name and signature bytes before it adds one, so each method has one
// token, and a method reached through a derived type is its declaring type's. The rows' parents and
// signatures are not visible to a guest; the test harness reads them from the dynamic module's scope.
// Measured on .NET 10 to exit 0.
//
// Returns 0 on success, or the number of the first check that failed.

public interface IThing
{
    int Value { get; }
}

public class Base
{
    public virtual string Name () => "base";
}

public class Derived : Base
{
}

public class Other
{
    public virtual string Name () => "other";
}

public static class Program
{
    public static int Main ()
    {
        AssemblyBuilder assembly = AssemblyBuilder.DefineDynamicAssembly (new AssemblyName ("Dyn"), AssemblyBuilderAccess.Run);
        ModuleBuilder module = assembly.DefineDynamicModule ("Dyn");

        MethodInfo getValue = typeof (IThing).GetProperty (nameof (IThing.Value))!.GetMethod!;

        if (module.GetMethodMetadataToken (getValue) != 0x0A000001)
        {
            return 1;
        }

        // A constructor reaches the same QCall.
        if (module.GetMethodMetadataToken (typeof (object).GetConstructor (Type.EmptyTypes)!) != 0x0A000002)
        {
            return 2;
        }

        if (module.GetMethodMetadataToken (typeof (string).GetMethod (nameof (string.ToUpper), Type.EmptyTypes)!) != 0x0A000003)
        {
            return 3;
        }

        // The same parent and signature under another name is another row...
        if (module.GetMethodMetadataToken (typeof (string).GetMethod (nameof (string.ToLower), Type.EmptyTypes)!) != 0x0A000004)
        {
            return 4;
        }

        // ...as is the same parent and name with another signature.
        MethodInfo concat2 = typeof (string).GetMethod (nameof (string.Concat), new[] { typeof (string), typeof (string) })!;
        MethodInfo concat3 = typeof (string).GetMethod (nameof (string.Concat), new[] { typeof (string), typeof (string), typeof (string) })!;

        if (module.GetMethodMetadataToken (concat2) != 0x0A000005 || module.GetMethodMetadataToken (concat3) != 0x0A000006)
        {
            return 5;
        }

        // A method reached through a derived type is a reference to its declaring type's.
        if (module.GetMethodMetadataToken (typeof (Derived).GetMethod (nameof (Base.Name))!) != 0x0A000007)
        {
            return 6;
        }

        if (module.GetMethodMetadataToken (typeof (Base).GetMethod (nameof (Base.Name))!) != 0x0A000007)
        {
            return 7;
        }

        // The same name and signature on another parent is another row.
        if (module.GetMethodMetadataToken (typeof (Other).GetMethod (nameof (Other.Name))!) != 0x0A000008)
        {
            return 8;
        }

        // A generic method's signature names its type parameter by number, not by token. An
        // instantiation is a reference to the definition.
        MethodInfo empty = typeof (Array).GetMethod (nameof (Array.Empty))!;

        if (module.GetMethodMetadataToken (empty) != 0x0A000009)
        {
            return 9;
        }

        if (module.GetMethodMetadataToken (empty.MakeGenericMethod (typeof (int))) != 0x0A000009)
        {
            return 10;
        }

        // Asked again, a method has the token it was first given.
        if (module.GetMethodMetadataToken (getValue) != 0x0A000001 || module.GetMethodMetadataToken (concat2) != 0x0A000005)
        {
            return 11;
        }

        return 0;
    }
}

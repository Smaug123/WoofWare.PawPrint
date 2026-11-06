using System;
using System.Reflection;
using System.Reflection.Emit;

// `ModuleBuilder_GetMemberRefOfMethodInfo` for methods whose signatures name a type by token: a class
// and a value type. The emitter rewrites each such token to name a TypeRef of the dynamic module
// (`TranslateSigWithScope`) before it looks for or adds the MemberRef. Measured on .NET 10 to exit 0.
//
// Returns 0 on success, or the number of the first check that failed.

public static class Program
{
    public static int Main ()
    {
        AssemblyBuilder assembly = AssemblyBuilder.DefineDynamicAssembly (new AssemblyName ("Dyn"), AssemblyBuilderAccess.Run);
        ModuleBuilder module = assembly.DefineDynamicModule ("Dyn");

        if (module.GetMethodMetadataToken (typeof (Type).GetMethod (nameof (Type.GetType), new[] { typeof (string) })!) != 0x0A000001)
        {
            return 1;
        }

        if (module.GetMethodMetadataToken (typeof (Guid).GetMethod (nameof (Guid.Parse), new[] { typeof (string) })!) != 0x0A000002)
        {
            return 2;
        }

        return 0;
    }
}

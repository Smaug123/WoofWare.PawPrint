using System;
using System.Reflection;
using System.Reflection.Emit;

// `MethodBuilder.SetImplementationFlags` through `TypeBuilder_SetMethodImpl`, setting code types other
// than managed IL on a method that has no body yet, and then IL again: real .NET holds each until
// `CreateType`. Measured on .NET 10 to exit 0.
//
// Returns 0 on success, or the number of the first check that failed.

public static class Program
{
    public static int Main ()
    {
        AssemblyBuilder assembly = AssemblyBuilder.DefineDynamicAssembly (new AssemblyName ("Dyn"), AssemblyBuilderAccess.Run);
        ModuleBuilder module = assembly.DefineDynamicModule ("Dyn");
        TypeBuilder type = module.DefineType ("A", TypeAttributes.Public);
        MethodBuilder method = type.DefineMethod ("M", MethodAttributes.Public, typeof (void), Type.EmptyTypes);

        if (method.MetadataToken != 0x06000001)
        {
            return 1;
        }

        method.SetImplementationFlags (MethodImplAttributes.Native);
        method.SetImplementationFlags (MethodImplAttributes.Unmanaged);
        method.SetImplementationFlags (MethodImplAttributes.IL);

        if (method.GetMethodImplementationFlags () != MethodImplAttributes.IL)
        {
            return 2;
        }

        return 0;
    }
}

using System;
using System.Reflection;
using System.Reflection.Emit;
using System.Runtime.InteropServices;

// `TypeBuilder.DefinePInvokeMethod`, whose first `MetadataToken` defines a P/Invoke row through
// `TypeBuilder_DefineMethod` before `TypeBuilder_SetPInvokeData` gives it its import. Measured on
// .NET 10 to exit 0.
//
// Returns 0 on success, or the number of the first check that failed.

public static class Program
{
    public static int Main ()
    {
        AssemblyBuilder assembly = AssemblyBuilder.DefineDynamicAssembly (new AssemblyName ("Dyn"), AssemblyBuilderAccess.Run);
        ModuleBuilder module = assembly.DefineDynamicModule ("Dyn");
        TypeBuilder natives = module.DefineType ("Natives", TypeAttributes.Public | TypeAttributes.Abstract | TypeAttributes.Sealed);

        MethodBuilder getPid = natives.DefinePInvokeMethod (
            "GetPid",
            "libc",
            "getpid",
            MethodAttributes.Public | MethodAttributes.Static | MethodAttributes.PinvokeImpl,
            CallingConventions.Standard,
            typeof (int),
            Type.EmptyTypes,
            CallingConvention.Cdecl,
            CharSet.Ansi);

        if (getPid.MetadataToken != 0x06000001)
        {
            return 1;
        }

        return 0;
    }
}

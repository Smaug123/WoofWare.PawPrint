using System;
using System.Runtime.CompilerServices;

// CoreCLR's CheckFriendAssemblyName rejects a PublicKeyToken= segment on an InternalsVisibleTo
// name (assemblyspec.cpp), but it only runs that check when an assembly's friend list is first
// consulted by an access check (Assembly::GetFriendAssemblyInfo); nothing looks at it at load.
// Roslyn accepts the segment, so such assemblies exist in the wild and run fine so long as
// nothing ever asks who their friends are. This guest never triggers a cross-assembly access
// check, so it must run to completion.
[assembly: InternalsVisibleTo("SomeFriend, PublicKeyToken=b77a5c561934e089")]

[AttributeUsage(AttributeTargets.Class)]
public sealed class MarkerAttribute : Attribute
{
}

[Marker]
public static class Program
{
    public static int Main()
    {
        // A custom-attribute lookup runs the CA visibility check for the attribute's
        // constructor, which is the entry point that asks about friend assemblies. The
        // attribute type is public and lives in this same assembly, so the friend list is
        // never consulted and the invalid name must go unnoticed.
        object[] attrs = typeof(Program).GetCustomAttributes(typeof(MarkerAttribute), false);
        if (attrs.Length != 1)
        {
            return 1;
        }

        return 0;
    }
}

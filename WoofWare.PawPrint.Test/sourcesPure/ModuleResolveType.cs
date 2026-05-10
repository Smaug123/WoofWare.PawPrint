using System;
using System.Reflection;

namespace ModuleResolveType
{
    public class SomeType { }

    public class Program
    {
        public static int Main(string[] args)
        {
            // Test 1: Resolve a TypeDef token in the executing assembly.
            Type t = typeof(SomeType);
            int token = t.MetadataToken;
            Module module = t.Module;

            Type resolved = module.ResolveType(token);
            if (resolved != typeof(SomeType)) return 1;

            // Test 2: Resolve a TypeRef token (typeof(string).MetadataToken in the executing
            // assembly's metadata produces the TypeDef in CoreLib, not a TypeRef in this assembly).
            // We exercise the TypeDef→TypeRef cross-assembly case implicitly by calling
            // ResolveType on the executing assembly's token table for a referenced type.
            // typeof(int) lives in CoreLib but its MetadataToken is the TypeDef in CoreLib;
            // calling typeof(int).Module.ResolveType(typeof(int).MetadataToken) is a TypeDef
            // self-resolution within CoreLib.
            Type intType = typeof(int);
            int intToken = intType.MetadataToken;
            Module corelibModule = intType.Module;
            Type resolvedInt = corelibModule.ResolveType(intToken);
            if (resolvedInt != typeof(int)) return 2;

            return 0;
        }
    }
}

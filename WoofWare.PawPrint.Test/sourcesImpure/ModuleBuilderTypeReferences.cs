using System;
using System.Collections.Generic;
using System.Reflection;
using System.Reflection.Emit;

// The TypeRef and AssemblyRef rows a dynamic module gains when it is asked for a token naming a type
// of another assembly, through `ModuleBuilder.GetTypeMetadataToken`. Each distinct type gets one
// TypeRef, in the order first asked for, and an enclosing type's TypeRef comes before its nested
// type's; each distinct assembly gets one AssemblyRef, in the same order; and each token resolves
// back to its type. Measured on .NET 10.
//
// Returns 0 on success, or the number of the first check that failed.

public static class Program
{
    public class Inner
    {
        public class Deeper
        {
        }
    }

    static readonly (Type Type, int Token)[] Expected =
    {
        (typeof (object), 0x01000001),
        (typeof (string), 0x01000002),
        // Asked for again: the same row.
        (typeof (object), 0x01000001),
        // `Program` and `Inner` get their rows before `Deeper`, which names `Inner` as its scope.
        (typeof (Inner.Deeper), 0x01000005),
        (typeof (Inner), 0x01000004),
        (typeof (Program), 0x01000003),
        (typeof (Stack<>), 0x01000006),
        (typeof (List<>), 0x01000007),
        (typeof (Dictionary<,>.KeyCollection), 0x01000009),
        (typeof (Uri), 0x0100000a),
        (typeof (int), 0x0100000b),
        (typeof (Environment.SpecialFolder), 0x0100000d),
    };

    public static int Main ()
    {
        AssemblyBuilder assembly = AssemblyBuilder.DefineDynamicAssembly (new AssemblyName ("Dyn"), AssemblyBuilderAccess.Run);
        ModuleBuilder module = assembly.DefineDynamicModule ("Dyn");

        if (assembly.GetReferencedAssemblies ().Length != 0)
        {
            return 1;
        }

        for (int i = 0; i < Expected.Length; i++)
        {
            int token = module.GetTypeMetadataToken (Expected[i].Type);
            if (token != Expected[i].Token)
            {
                Console.WriteLine ($"{Expected[i].Type}: expected {Expected[i].Token:x8}, got {token:x8}");
                return 10 + i;
            }
        }

        // Each row reads back, through the module's image and the AssemblyRef that scopes it, as the
        // type it was made for.
        for (int i = 0; i < Expected.Length; i++)
        {
            if (module.ResolveType (Expected[i].Token) != Expected[i].Type)
            {
                Console.WriteLine ($"{Expected[i].Token:x8} resolved to {module.ResolveType (Expected[i].Token)}, not {Expected[i].Type}");
                return 50 + i;
            }
        }

        string[] expectedReferences =
        {
            typeof (object).Assembly.FullName!,
            typeof (Program).Assembly.FullName!,
            typeof (Stack<>).Assembly.FullName!,
            typeof (Uri).Assembly.FullName!,
        };

        AssemblyName[] references = assembly.GetReferencedAssemblies ();

        if (references.Length != expectedReferences.Length)
        {
            Console.WriteLine ($"expected {expectedReferences.Length} references, got {references.Length}");
            return 2;
        }

        for (int i = 0; i < references.Length; i++)
        {
            if (references[i].FullName != expectedReferences[i])
            {
                Console.WriteLine ($"reference {i}: expected {expectedReferences[i]}, got {references[i].FullName}");
                return 30 + i;
            }

            // The row holds the referenced assembly's public key token, never its full key.
            if (references[i].GetPublicKey () != null || references[i].Flags != AssemblyNameFlags.None)
            {
                return 40 + i;
            }
        }

        return 0;
    }
}

using System;
using System.Reflection;
using System.Reflection.Emit;
using System.Runtime.Loader;

// Dynamic assemblies, as `AppDomain_CreateDynamicAssembly` makes them: the one that anonymously hosts
// every `DynamicMethod` created without an owner, and one a guest defines by name. Each answer was
// measured on .NET 10.
//
// Returns 0 on success, or the number of the first check that failed.

public static class Program
{
    const string AnonymousHost = "Anonymously Hosted DynamicMethods Assembly, Version=0.0.0.0, Culture=neutral, PublicKeyToken=null";

    public static int Main ()
    {
        DynamicMethod answer = new DynamicMethod ("Answer", typeof (int), Type.EmptyTypes);
        ILGenerator il = answer.GetILGenerator ();
        il.Emit (OpCodes.Ldc_I4, 42);
        il.Emit (OpCodes.Ret);

        Module module = answer.Module;
        Assembly host = module.Assembly;

        if (host.FullName != AnonymousHost)
        {
            return 1;
        }

        if (!ReferenceEquals (module, host.ManifestModule))
        {
            return 2;
        }

        // 3: every anonymous dynamic method shares the one host.
        if (!ReferenceEquals (new DynamicMethod ("Other", typeof (void), Type.EmptyTypes).Module, module))
        {
            return 3;
        }

        if (!host.IsDynamic || typeof (object).Assembly.IsDynamic || typeof (Program).Assembly.IsDynamic)
        {
            return 4;
        }

        if (host.Location != "" || host.IsCollectible)
        {
            return 5;
        }

        if (module.ScopeName != "RefEmit_InMemoryManifestModule" || module.MDStreamVersion != 0x20000)
        {
            return 6;
        }

        if (host.GetTypes ().Length != 0)
        {
            return 7;
        }

        // 8: `GetName` asks the manifest module for its PE kind, which a dynamic module does not have.
        AssemblyName hostName = host.GetName ();

        if (hostName.ProcessorArchitecture != ProcessorArchitecture.None)
        {
            return 8;
        }

#pragma warning disable SYSLIB0037
        if (hostName.HashAlgorithm != System.Configuration.Assemblies.AssemblyHashAlgorithm.SHA1)
#pragma warning restore SYSLIB0037
        {
            return 9;
        }

        if (Array.IndexOf (AppDomain.CurrentDomain.GetAssemblies (), host) < 0)
        {
            return 10;
        }

        // 11: and a method it hosts runs.
        if (answer.CreateDelegate<Func<int>> () () != 42)
        {
            return 11;
        }

        // `DefineDynamicAssembly` asks for the caller's load context unless a contextual-reflection
        // scope names one, and PawPrint does not yet answer that question
        // (`AssemblyNative_GetLoadContextForAssembly`).
        using AssemblyLoadContext.ContextualReflectionScope scope = AssemblyLoadContext.Default.EnterContextualReflection ();

        AssemblyName requested = new AssemblyName ("Named, Version=1.2.3.4, Culture=fr-FR");
        requested.Flags = AssemblyNameFlags.Retargetable;
        AssemblyBuilder builder = AssemblyBuilder.DefineDynamicAssembly (requested, AssemblyBuilderAccess.Run);
        Assembly named = builder.ManifestModule.Assembly;

        if (named.FullName != "Named, Version=1.2.3.4, Culture=fr-FR, PublicKeyToken=null, Retargetable=Yes")
        {
            return 12;
        }

        if (!named.IsDynamic || ReferenceEquals (named, builder))
        {
            return 13;
        }

        if ((named.GetName ().Flags & AssemblyNameFlags.Retargetable) == 0)
        {
            return 14;
        }

        // 15: the name is checked where the assembly is made, not by `AssemblyBuilder`.
        try
        {
            AssemblyBuilder.DefineDynamicAssembly (new AssemblyName (), AssemblyBuilderAccess.Run);
            return 15;
        }
        catch (ArgumentException e) when (e.Message == "AssemblyName.Name cannot be null or an empty string.")
        {
        }

        // 16: the metadata emitter reads a version component of 65535 as "unset", leaving it 0.
        AssemblyName sentinelVersion = new AssemblyName ("SentinelVersion") { Version = new Version (65535, 2, 65534, 65535) };
        Assembly withSentinelVersion = AssemblyBuilder.DefineDynamicAssembly (sentinelVersion, AssemblyBuilderAccess.Run).ManifestModule.Assembly;

        if (withSentinelVersion.FullName != "SentinelVersion, Version=0.2.65534.0, Culture=neutral, PublicKeyToken=null")
        {
            return 16;
        }

        // 17: and a hash algorithm of -1 the same way, although 0 means SHA1.
#pragma warning disable SYSLIB0037
        AssemblyName sentinelHash = new AssemblyName ("SentinelHash") { HashAlgorithm = (System.Configuration.Assemblies.AssemblyHashAlgorithm) (-1) };

        if ((int) AssemblyBuilder.DefineDynamicAssembly (sentinelHash, AssemblyBuilderAccess.Run).ManifestModule.Assembly.GetName ().HashAlgorithm != 0)
#pragma warning restore SYSLIB0037
        {
            return 17;
        }

        return 0;
    }
}

using System;
using System.Collections.Generic;
using System.Reflection;

public class CollectibleProbe<T>
{
    public CollectibleProbe() { }

    public int Instance(int x) => x;

    public U Generic<U>(U value) => value;

    // A byref parameter, so `int&` can be reached as a reflected parameter type. Naming it with
    // `MakeByRefType()` instead would need `RuntimeTypeHandle_MakeByRef`, an unimplemented native
    // that has nothing to do with collectibility.
    public void ByrefParameter(ref int value) => value++;
}

public static class Program
{
    // `IsCollectible` on a type, a method and an assembly. Each reads a QCall of its own --
    // `RuntimeTypeHandle_IsCollectible`, `RuntimeMethodHandle_GetIsCollectible` and
    // `AssemblyNative_GetIsCollectible` -- and each answers from the loader allocator behind the
    // thing asked about. PawPrint models only CoreCLR's `GlobalLoaderAllocator`, which is
    // non-collectible, so every answer here is false.
    //
    // Every shape below was measured on real .NET rather than derived from "PawPrint loads nothing
    // collectible": these are the answers a normally-loaded program actually gets. The structural
    // types, the open definition and the type variable are here because they are the shapes that
    // would answer oddly if collectibility were computed from something other than the loader
    // allocator -- from whether the type has a metadata row, say.
    //
    // A collectible answer is not reachable from a pure guest at all. In CoreCLR `m_IsCollectible`
    // is set true only by `AssemblyNative_InitializeAssemblyLoadContext`'s collectible branch, and
    // PawPrint implements no AssemblyLoadContext native, so no guest can construct one. The one
    // `true` a program can see -- `DynamicMethod.IsCollectible` -- comes from `MemberInfo`'s
    // managed virtual default rather than from any QCall, and PawPrint reports dynamic code
    // unsupported, so it is absent here on both counts.
    //
    // Exit code is the index of the first failing check, so a failure names itself.
    public static unsafe int Main()
    {
        // A corelib primitive, and its assembly.
        if (typeof(int).IsCollectible) return 1;
        if (typeof(int).Assembly.IsCollectible) return 2;

        // An ordinary type from the guest's own assembly, which is loaded by a different route
        // from corelib and so is worth asking separately.
        if (typeof(Program).IsCollectible) return 3;
        if (typeof(Program).Assembly.IsCollectible) return 4;

        // A closed instantiation, its definition, and the definition's own type variable. The
        // variable is a TypeDesc in CoreCLR rather than a MethodTable, so it reaches the QCall by
        // a different path from the two above.
        if (typeof(CollectibleProbe<int>).IsCollectible) return 5;
        if (typeof(CollectibleProbe<>).IsCollectible) return 6;
        if (typeof(CollectibleProbe<>).GetGenericArguments()[0].IsCollectible) return 7;

        // Structural types: array, byref and pointer. Each is a TypeDesc with no metadata row of
        // its own, so an implementation keying on "does this have a TypeDef" answers wrongly here.
        if (typeof(int[]).IsCollectible) return 8;

        Type byrefType = typeof(CollectibleProbe<int>).GetMethod("ByrefParameter").GetParameters()[0].ParameterType;
        if (!byrefType.IsByRef) return 9;
        if (byrefType.IsCollectible) return 10;

        if (typeof(int*).IsCollectible) return 11;
        if (typeof(int*[]).IsCollectible) return 12;

        // Methods: an ordinary instance method, a generic method definition, a generic method
        // instantiation, and a constructor. `RuntimeMethodInfo.IsCollectible` consults its
        // reflected type first and short-circuits on *true*, so with the type answering false it
        // is the method QCall that decides each of these.
        Type probe = typeof(CollectibleProbe<int>);
        if (probe.GetMethod("Instance").IsCollectible) return 13;

        MethodInfo generic = probe.GetMethod("Generic");
        if (generic.IsCollectible) return 14;
        if (generic.MakeGenericMethod(typeof(string)).IsCollectible) return 15;
        if (probe.GetConstructors()[0].IsCollectible) return 16;

        // A method on the open definition, which is the shape whose signature decoding needed the
        // definition's own formals as a type context (sourcesPure/
        // ReflectionOpenGenericDefinitionParameterTypes.cs). Its collectibility is a separate
        // question from its signature, and the QCall must answer it without one.
        if (typeof(CollectibleProbe<>).GetMethod("Instance").IsCollectible) return 17;

        // A method of corelib, so the method QCall is asked about both assemblies too.
        if (typeof(object).GetMethod("GetHashCode").IsCollectible) return 18;

        return 0;
    }
}

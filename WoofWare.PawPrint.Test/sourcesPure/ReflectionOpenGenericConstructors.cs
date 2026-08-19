using System;
using System.Reflection;

public sealed class Box<T>
{
    public T Item;

    public Box()
    {
    }

    public Box(T t)
    {
        Item = t;
    }

    public int Plain() => 1;
}

public static class Program
{
    // Enumerating the constructors of an open generic type *definition*. This reaches
    // RuntimeTypeHandle.GetFirstIntroducedMethod / GetNextIntroducedMethod with a declaring type
    // that is the definition rather than an instantiation.
    //
    // GetConstructors is deliberately the query: PopulateConstructors never asks for
    // GetNumVirtuals, because constructors are never virtual, so this isolates the introduced-method
    // walk from the definition's vtable, which
    // sourcesPure/ReflectionOpenGenericDefinitionMethods.cs covers separately.
    //
    // Exit code is the index of the first failing check, so a failure names itself.
    public static int Main()
    {
        ConstructorInfo[] open = typeof(Box<>).GetConstructors();
        ConstructorInfo[] closed = typeof(Box<int>).GetConstructors();

        if (open.Length != 2) return 1;
        if (closed.Length != 2) return 2;

        foreach (ConstructorInfo c in open)
        {
            if (!c.IsPublic) return 3;
            if (c.IsStatic) return 4;
            if (c.MetadataToken == 0) return 5;
        }

        // Metadata order: the enumerator walks MethodDef rows in table order, so the tokens come
        // back ascending. A walk that reversed or re-sorted them fails here and nowhere else --
        // the two constructors are indistinguishable by attributes, and GetParameters() on an
        // open-definition method is not yet supported.
        if (open[0].MetadataToken >= open[1].MetadataToken) return 6;

        // No handle minted for the definition may collide with one minted for the instantiation.
        // This is the check that fails if the open declaring type is ever represented by standing
        // a closed instantiation in for it: the MethodDef tokens are shared (see check 8), so the
        // declaring type is the only thing separating these handles.
        bool sawSharedToken = false;
        foreach (ConstructorInfo o in open)
        {
            foreach (ConstructorInfo c in closed)
            {
                if (o.MetadataToken == c.MetadataToken) sawSharedToken = true;
                if (o.MethodHandle.Equals(c.MethodHandle)) return 7;
            }
        }

        // Vacuity guard for check 7: the definition and the instantiation really do share MethodDef
        // rows, so check 7 cannot pass merely because these are different methods.
        if (!sawSharedToken) return 8;

        // The definition's own two constructors are distinct from each other.
        if (open[0].MethodHandle.Equals(open[1].MethodHandle)) return 9;

        // Asking a second time yields the same handle: the registry dedups on the identity rather
        // than minting a fresh id per query.
        if (!typeof(Box<>).GetConstructors()[0].MethodHandle.Equals(open[0].MethodHandle)) return 10;

        return 0;
    }
}

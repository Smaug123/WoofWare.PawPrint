using System;
using System.Reflection;

public struct SBox<T>
{
    public T Item;

    public SBox(T t)
    {
        Item = t;
    }
}

public static class Program
{
    // The value-type counterpart of sourcesPure/ReflectionOpenGenericConstructors.cs. Parked: the
    // introduced-method walk serves this fine, but PopulateConstructors then calls
    // RuntimeMethodHandle.GetStubIfNeeded, whose decision needs a substitution context for a
    // value-type declaring type that PawPrint cannot yet express for a definition.
    //
    // Not satisfiable the wrong way: an implementation that reported zero constructors, or that
    // answered for SBox<int> instead of the definition, fails check 1 or check 3 respectively.
    public static int Main()
    {
        ConstructorInfo[] open = typeof(SBox<>).GetConstructors();
        if (open.Length != 1) return 1;
        if (!open[0].IsPublic) return 2;

        // The definition's handle must not be the instantiation's.
        ConstructorInfo[] closed = typeof(SBox<int>).GetConstructors();
        if (closed.Length != 1) return 3;
        if (open[0].MethodHandle.Equals(closed[0].MethodHandle)) return 4;

        // Vacuity guard: they share the MethodDef row, so check 4 turns on the declaring type.
        if (open[0].MetadataToken != closed[0].MetadataToken) return 5;

        return 0;
    }
}

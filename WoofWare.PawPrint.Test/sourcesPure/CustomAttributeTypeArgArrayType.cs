using System;

// A System.Type-valued attribute argument naming an *array* of a type of the decorated assembly.
// Roslyn writes it unqualified ("Other[]"), so no assembly binding is involved; CoreLib's
// TypeNameResolver resolves the element and then builds the array through
// RuntimeTypeHandle.MakeSZArray, which is the QCall PawPrint stops at. Plain
// `Type.GetType("Other[]")` takes the same path; CustomAttributeTypeArg.cs covers the names that
// need no such construction.

[AttributeUsage(AttributeTargets.Class)]
public class TypeMarkerAttribute : Attribute
{
    public TypeMarkerAttribute(Type type, int tail)
    {
        Type = type;
        Tail = tail;
    }

    public Type Type { get; }
    public int Tail { get; }
}

public class Other
{
}

[TypeMarker(typeof(Other[]), 9)]
public class Decorated
{
}

public class CustomAttributeTypeArgArrayType
{
    public static int Main(string[] argv)
    {
        var m = (TypeMarkerAttribute)Attribute.GetCustomAttribute(typeof(Decorated), typeof(TypeMarkerAttribute));
        if (m == null) return 1;
        if (m.Type != typeof(Other[])) return 2;
        if (m.Tail != 9) return 3;
        return 0;
    }
}

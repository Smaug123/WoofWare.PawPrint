using System;

// A System.Type-valued attribute argument naming a *nested* type of the decorated assembly. Roslyn
// writes it unqualified ("Outer+Inner"), so no assembly binding is involved; CoreLib's
// TypeNameResolver splits the name and hands the nested part to RuntimeAssembly.GetTypeCore as a
// ReadOnlySpan<string>, and it is that call's marshalling stub which PawPrint cannot yet run.
// Plain `Type.GetType("Outer+Inner")` stops in the same place, so nothing here is specific to
// attributes; CustomAttributeTypeArg.cs covers the top-level names.

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

public class Outer
{
    public class Inner
    {
    }
}

[TypeMarker(typeof(Outer.Inner), 7)]
public class Decorated
{
}

public class CustomAttributeTypeArgNested
{
    public static int Main(string[] argv)
    {
        var m = (TypeMarkerAttribute)Attribute.GetCustomAttribute(typeof(Decorated), typeof(TypeMarkerAttribute));
        if (m == null) return 1;
        if (m.Type != typeof(Outer.Inner)) return 2;
        if (m.Tail != 7) return 3;
        return 0;
    }
}

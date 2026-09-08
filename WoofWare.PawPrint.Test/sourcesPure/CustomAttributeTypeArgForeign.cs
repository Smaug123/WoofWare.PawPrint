using System;
using System.Collections.Generic;

// The complement of CustomAttributeTypeArg.cs: System.Type-valued attribute arguments naming types
// from *other* assemblies. Roslyn writes every such name assembly-qualified -- measured on the 10.0
// compiler as "System.Int32, System.Runtime, Version=10.0.0.0, Culture=neutral,
// PublicKeyToken=b03f5f7f11d50a3a" against the reference pack, and the System.Private.CoreLib
// identity against the implementation assemblies -- and a generic instantiation qualifies its
// type arguments even when they are declared in this assembly. CoreLib's TypeNameResolver binds
// such a name through RuntimeAssembly.InternalLoad, which is the QCall this file stops at.

[AttributeUsage(AttributeTargets.Class, AllowMultiple = true)]
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

public class Local
{
}

[TypeMarker(typeof(int), 1)]
[TypeMarker(typeof(string[]), 2)]
[TypeMarker(typeof(List<Local>), 3)]
public class Decorated
{
}

public class CustomAttributeTypeArgForeign
{
    static TypeMarkerAttribute Marker(int tail)
    {
        foreach (var a in Attribute.GetCustomAttributes(typeof(Decorated), typeof(TypeMarkerAttribute)))
        {
            var m = (TypeMarkerAttribute)a;
            if (m.Tail == tail) return m;
        }
        return null;
    }

    public static int Main(string[] argv)
    {
        var m1 = Marker(1);
        if (m1 == null) return 1;
        if (m1.Type != typeof(int)) return 2;

        var m2 = Marker(2);
        if (m2 == null) return 3;
        if (m2.Type != typeof(string[])) return 4;

        var m3 = Marker(3);
        if (m3 == null) return 5;
        if (m3.Type != typeof(List<Local>)) return 6;

        return 0;
    }
}

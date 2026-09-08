using System;

// ECMA-335 II.23.3 stores a System.Type-valued attribute argument as a SerString holding the
// type's reflection name (TYPE, 0x50), so instantiating the attribute means resolving that name.
// CoreCLR resolves it exactly as Type.GetType would, with the *decorated* assembly as the
// requesting assembly (typeparse.cpp calls back into TypeNameResolver.GetTypeHelper).
//
// Every type named here is declared in this assembly, because that is the one case Roslyn writes
// unqualified: measured on the 10.0 compiler, a type from any other assembly -- `typeof(int)`
// included -- is written assembly-qualified ("System.Int32, System.Runtime, Version=10.0.0.0, ..."
// against the reference pack, "..., System.Private.CoreLib, ..." against the implementation
// assemblies), and resolving that binds the assembly through `AssemblyNative_InternalLoad`.
// `CustomAttributeTypeArgForeign.cs` holds those. Two shapes of *this* assembly's own types take
// further paths through the resolver and are parked on those: `CustomAttributeTypeArgNested.cs`
// (a nested type) and `CustomAttributeTypeArgArrayType.cs` (an array type).
//
// Every attribute here carries a trailing int, so a SerString read of the wrong length
// desynchronises the cursor and corrupts the tail rather than failing silently.

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

[AttributeUsage(AttributeTargets.Class, AllowMultiple = true)]
public class TypeListAttribute : Attribute
{
    public TypeListAttribute(int tag, Type[] types, int tail)
    {
        Tag = tag;
        Types = types;
        Tail = tail;
    }

    public int Tag { get; }
    public Type[] Types { get; }
    public int Tail { get; }
}

public class Other
{
}

public class Third
{
}

// The names the compiler writes into these blobs: "Decorated", "Third", and the null sentinel.
// The array case reads each element's name in turn, and the elements are distinct, so a decoder
// that resolved only the first, or reversed them, would put the wrong type somewhere.
[TypeMarker(typeof(Decorated), 1)]
[TypeMarker(typeof(Third), 2)]
[TypeMarker(null, 3)]
[TypeList(1, new[] { typeof(Other), typeof(Decorated), null, typeof(Third) }, 11)]
[TypeList(2, new Type[0], 12)]
[TypeList(3, null, 13)]
public class Decorated
{
}

public class CustomAttributeTypeArg
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

    static TypeListAttribute List(int tag)
    {
        foreach (var a in Attribute.GetCustomAttributes(typeof(Decorated), typeof(TypeListAttribute)))
        {
            var l = (TypeListAttribute)a;
            if (l.Tag == tag) return l;
        }
        return null;
    }

    public static int Main(string[] argv)
    {
        var m1 = Marker(1);
        if (m1 == null) return 1;
        if (m1.Type != typeof(Decorated)) return 2;

        var m2 = Marker(2);
        if (m2 == null) return 3;
        if (m2.Type != typeof(Third)) return 4;

        var m3 = Marker(3);
        if (m3 == null) return 5;
        if (m3.Type != null) return 6;

        var l1 = List(1);
        if (l1 == null) return 11;
        if (l1.Types == null || l1.Types.Length != 4) return 12;
        if (l1.Types[0] != typeof(Other)) return 13;
        if (l1.Types[1] != typeof(Decorated)) return 14;
        if (l1.Types[2] != null) return 15;
        if (l1.Types[3] != typeof(Third)) return 16;
        if (l1.Tail != 11) return 17;

        var l2 = List(2);
        if (l2 == null) return 18;
        if (l2.Types == null || l2.Types.Length != 0) return 19;
        if (l2.Tail != 12) return 20;

        var l3 = List(3);
        if (l3 == null) return 21;
        if (l3.Types != null) return 22;
        if (l3.Tail != 13) return 23;

        return 0;
    }
}

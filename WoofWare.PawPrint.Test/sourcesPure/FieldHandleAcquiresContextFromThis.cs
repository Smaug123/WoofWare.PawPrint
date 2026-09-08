using System;
using System.Linq.Expressions;
using System.Reflection;

// Every field below is first reached through its *handle*, never by name. That matters: a
// name lookup populates the declaring type's field cache, and `MemberInfoCache.AddField`
// (RuntimeType.CoreCLR.cs:311) returns a cached candidate before it ever asks
// `RuntimeFieldHandle.AcquiresContextFromThis` how to compute `isInherited`. The C# compiler
// lowers a field access inside an expression tree to `ldtoken field` followed by
// `FieldInfo.GetFieldFromHandle`, which is the route `RequestDelegateFactory` takes and the
// route that reaches the FCall with an empty cache.
class Plain
{
    public int Value;
    public static string Tag = "tag";
}

class Generic<T>
{
    public T Value;
}

class Base
{
    public int BaseField;
}

class Derived : Base
{
}

static class Program
{
    static FieldInfo FieldOf(LambdaExpression lambda)
    {
        return (FieldInfo)((MemberExpression)lambda.Body).Member;
    }

    static int Main()
    {
        // --- Instance field, non-generic declaring type: the one-argument GetFieldFromHandle. ---
        Expression<Func<Plain, int>> instance = p => p.Value;
        FieldInfo instanceField = FieldOf(instance);
        if (instanceField.Name != "Value") return 1;
        if (instanceField.DeclaringType != typeof(Plain)) return 2;
        if (instanceField.ReflectedType != typeof(Plain)) return 3;
        if (instanceField.IsStatic) return 4;
        // `isInherited` feeds the cached entry's binding flags, so a DeclaredOnly lookup by name
        // must find it -- and must hand back the very object the handle path minted, because the
        // name lookup merges into the same cache.
        FieldInfo instanceByName = typeof(Plain).GetField("Value", BindingFlags.Public | BindingFlags.Instance | BindingFlags.DeclaredOnly);
        if (!ReferenceEquals(instanceByName, instanceField)) return 5;

        // --- Static field, same route. ---
        Expression<Func<string>> stat = () => Plain.Tag;
        FieldInfo staticField = FieldOf(stat);
        if (staticField.Name != "Tag") return 6;
        if (!staticField.IsStatic) return 7;
        if (staticField.DeclaringType != typeof(Plain)) return 8;
        FieldInfo staticByName = typeof(Plain).GetField("Tag", BindingFlags.Public | BindingFlags.Static | BindingFlags.DeclaredOnly);
        if (!ReferenceEquals(staticByName, staticField)) return 9;

        // --- Closed generic declaring types: the two-argument GetFieldFromHandle(field, type). ---
        // A value-type instantiation, which CoreCLR does not share with any other instantiation.
        Expression<Func<Generic<int>, int>> genericInt = g => g.Value;
        FieldInfo genericIntField = FieldOf(genericInt);
        if (genericIntField.DeclaringType != typeof(Generic<int>)) return 10;
        if (genericIntField.FieldType != typeof(int)) return 11;
        if (!ReferenceEquals(genericIntField, typeof(Generic<int>).GetField("Value", BindingFlags.Public | BindingFlags.Instance | BindingFlags.DeclaredOnly))) return 12;

        // A reference-type instantiation: the case where CoreCLR's FieldDesc is shared and the
        // approximate declaring type is `Generic<__Canon>`. The exact instantiation must still
        // come back, because the two-argument overload supplies it.
        Expression<Func<Generic<string>, string>> genericString = g => g.Value;
        FieldInfo genericStringField = FieldOf(genericString);
        if (genericStringField.DeclaringType != typeof(Generic<string>)) return 13;
        if (genericStringField.FieldType != typeof(string)) return 14;
        if (ReferenceEquals(genericStringField, genericIntField)) return 15;

        // --- An inherited field: the handle names Base.BaseField, the lambda parameter is Derived. ---
        // The compiler still emits the one-argument overload (Base is not generic), so the
        // FieldInfo reflects Base, not Derived.
        Expression<Func<Derived, int>> inherited = d => d.BaseField;
        FieldInfo inheritedField = FieldOf(inherited);
        if (inheritedField.DeclaringType != typeof(Base)) return 16;
        if (inheritedField.ReflectedType != typeof(Base)) return 17;

        // --- Mismatched declaring types are rejected. ---
        // `RuntimeType.GetFieldInfo` (RuntimeType.CoreCLR.cs:1988) asks AcquiresContextFromThis
        // only when the supplied type differs from the handle's own; each of these differs, and
        // none is an instantiation CoreCLR would share with the handle's, so both runtimes throw.
        RuntimeFieldHandle baseHandle = inheritedField.FieldHandle;
        try
        {
            FieldInfo.GetFieldFromHandle(baseHandle, typeof(Derived).TypeHandle);
            return 18;
        }
        catch (ArgumentException)
        {
        }

        RuntimeFieldHandle genericIntHandle = genericIntField.FieldHandle;
        try
        {
            FieldInfo.GetFieldFromHandle(genericIntHandle, typeof(Generic<long>).TypeHandle);
            return 19;
        }
        catch (ArgumentException)
        {
        }

        try
        {
            FieldInfo.GetFieldFromHandle(genericIntHandle, typeof(Generic<>).TypeHandle);
            return 20;
        }
        catch (ArgumentException)
        {
        }

        return 0;
    }
}

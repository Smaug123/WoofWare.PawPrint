using System;
using System.Reflection;

public class ElementDefinition<T>
{
    public int TakesArray(T[] value) => 1;

    public int TakesByref(ref T value) => 2;

    public T[] ReturnsArray() => null!;
}

public static class Program
{
    // The three shapes sourcesPure/ReflectionOpenGenericDefinitionParameterTypes.cs leaves out: an
    // array, a byref, or a pointer *over* an open generic definition's own type variable. Real .NET
    // reflects all of them, with `GetElementType()` reference-equal to the variable itself, which is
    // what checks 3, 6 and 8 pin.
    //
    // PawPrint refuses them. A reflected type is a `RuntimeTypeHandleTarget`, whose cases are a
    // closed runtime type, a definition, a variable of a type or method, an open construction of a
    // definition over targets, and the dynamic-methods class -- there is no array-of, byref-to or
    // pointer-to a target. `ConcreteTypeHandle` does carry those shapes, but only over closed
    // element types, so it cannot hold `T[]` either. Giving the target DU those cases reaches 75
    // match arms across 34 files, so it is its own change rather than part of decoding a signature.
    //
    // A pointer needs `where T : unmanaged` to be spellable in C# at all, so it is absent here; the
    // refusal is the same one, since it is the element type that cannot be named rather than the
    // wrapper.
    //
    // Un-park when a reflected type can name an array, byref or pointer over a type variable.
    //
    // Exit code is the index of the first failing check, so a failure names itself.
    public static int Main()
    {
        Type definition = typeof(ElementDefinition<>);
        Type formal = definition.GetGenericArguments()[0];

        Type arrayParam = definition.GetMethod("TakesArray").GetParameters()[0].ParameterType;
        if (!arrayParam.IsArray) return 1;
        if (arrayParam.GetArrayRank() != 1) return 2;
        if (!ReferenceEquals(arrayParam.GetElementType(), formal)) return 3;

        Type byrefParam = definition.GetMethod("TakesByref").GetParameters()[0].ParameterType;
        if (!byrefParam.IsByRef) return 4;
        if (byrefParam.IsArray) return 5;
        if (!ReferenceEquals(byrefParam.GetElementType(), formal)) return 6;

        // The return path takes the same walk, so it refuses in the same place.
        Type arrayReturn = definition.GetMethod("ReturnsArray").ReturnType;
        if (!arrayReturn.IsArray) return 7;
        if (!ReferenceEquals(arrayReturn.GetElementType(), formal)) return 8;

        // The closed instantiation is the control: substituting makes every one of these an
        // ordinary closed type, so none of the above is about arrays or byrefs as such.
        Type instantiation = typeof(ElementDefinition<int>);
        if (instantiation.GetMethod("TakesArray").GetParameters()[0].ParameterType != typeof(int[])) return 9;
        if (instantiation.GetMethod("TakesByref").GetParameters()[0].ParameterType != typeof(int).MakeByRefType()) return 10;

        return 0;
    }
}

using System;
using System.Reflection;

public class ElementDefinition<T>
{
    public int TakesArray(T[] value) => 1;

    public int TakesByref(ref T value) => 2;

    public T[] ReturnsArray() => null!;

    public int TakesMd(T[,] value) => 3;

    public int TakesRefArray(ref T[] value) => 4;
}

public unsafe class PointerDefinition<T> where T : unmanaged
{
    public int TakesPointer(T* value) => 5;
}

public class Control
{
    public int TakesRefInt(ref int value) => 6;
}

public static class Program
{
    // The three shapes sourcesPure/ReflectionOpenGenericDefinitionParameterTypes.cs leaves out: an
    // array, a byref, or a pointer *over* an open generic definition's own type variable. Real .NET
    // reflects all of them, with `GetElementType()` reference-equal to the variable itself, which is
    // what checks 3, 6 and 8 pin.
    //
    // Under PawPrint each of these is a `RuntimeTypeHandleTarget.Composite`: a shape over a target
    // that is not closed. The element comes back reference-equal to the definition's own formal
    // because the type-handle registry keys `Type` identity on the target, and the constructor
    // `RuntimeTypeHandleTarget.composite` collapses a shape over a *closed* element into the closed
    // shape, so no type ever has two spellings there.
    //
    // A pointer needs `where T : unmanaged` to be spellable in C# at all, so it has a definition
    // of its own below.
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
        // `int&` reflected off a non-generic method is the same closed byref, so the two must be
        // one `Type` object.
        Type refInt = typeof(Control).GetMethod("TakesRefInt").GetParameters()[0].ParameterType;
        if (instantiation.GetMethod("TakesByref").GetParameters()[0].ParameterType != refInt) return 10;

        // A multi-dimensional array keeps its rank, and drops it for the element.
        Type mdParam = definition.GetMethod("TakesMd").GetParameters()[0].ParameterType;
        if (!mdParam.IsArray) return 11;
        if (mdParam.GetArrayRank() != 2) return 12;
        if (!ReferenceEquals(mdParam.GetElementType(), formal)) return 13;

        // A pointer over the variable.
        Type pointerDefinition = typeof(PointerDefinition<>);
        Type pointerFormal = pointerDefinition.GetGenericArguments()[0];
        Type pointerParam = pointerDefinition.GetMethod("TakesPointer").GetParameters()[0].ParameterType;
        if (!pointerParam.IsPointer) return 14;
        if (!ReferenceEquals(pointerParam.GetElementType(), pointerFormal)) return 15;

        // Two shapes deep: a byref whose element is itself an array over the variable, so the
        // inner element type must be reference-equal to the array reflected on `TakesArray`.
        Type refArrayParam = definition.GetMethod("TakesRefArray").GetParameters()[0].ParameterType;
        if (!refArrayParam.IsByRef) return 16;
        if (!ReferenceEquals(refArrayParam.GetElementType(), arrayParam)) return 17;
        if (!ReferenceEquals(refArrayParam.GetElementType().GetElementType(), formal)) return 18;

        // Names: the element's own name under the shape's suffix. FullName is null for any type
        // that contains a generic parameter without being a generic type definition.
        if (arrayParam.Name != "T[]") return 19;
        if (byrefParam.ToString() != "T&") return 20;
        if (mdParam.Name != "T[,]") return 21;
        if (pointerParam.Name != "T*") return 22;
        if (refArrayParam.Name != "T[]&") return 23;
        if (arrayParam.FullName != null) return 24;
        if (!arrayParam.ContainsGenericParameters) return 25;
        if (arrayParam.IsGenericParameter) return 26;
        if (arrayParam.BaseType != typeof(Array)) return 27;
        if (byrefParam.BaseType != null) return 28;

        // Closed controls for the rank: the same FCall, asked about ordinary arrays.
        if (typeof(int[,]).GetArrayRank() != 2) return 29;
        if (typeof(int[]).GetArrayRank() != 1) return 30;
        if (typeof(int[,,]).GetArrayRank() != 3) return 31;

        return 0;
    }
}

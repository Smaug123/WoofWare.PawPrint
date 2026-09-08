using System;
using System.Collections.Generic;
using System.Reflection;

class Sample
{
    public void Arrays(int[] ints, List<int>[] lists, int[][] jagged) { }

    public class Nested<T>
    {
    }

    public ref struct RefStruct
    {
        public int Field;
    }

    public ref struct GenericRefStruct<T>
    {
        public int Field;
    }
}

static class Program
{
    static int Main()
    {
        // `Type.MakeArrayType()` on a RuntimeType is the `RuntimeTypeHandle_MakeSZArray` QCall.
        // (`MakeArrayType(1)` is a different QCall and a different type: a rank-1 `ELEMENT_TYPE_ARRAY`
        // is not `int[]`.)
        Type intArray = typeof(int).MakeArrayType();
        if (!intArray.IsArray) return 1;
        if (!intArray.IsSZArray) return 2;
        if (intArray.GetArrayRank() != 1) return 3;
        if (!intArray.HasElementType) return 4;
        if (intArray.IsByRef || intArray.IsPointer) return 5;
        if (intArray.GetElementType() != typeof(int)) return 6;
        if (intArray.Name != "Int32[]") return 7;
        if (intArray.FullName != "System.Int32[]") return 8;
        if (intArray.IsValueType) return 9;
        if (intArray.BaseType != typeof(Array)) return 10;
        if (intArray.IsGenericType || intArray.ContainsGenericParameters) return 11;

        // One RuntimeType per type handle: the array reached through `typeof`, through reflection
        // on an `int[]` parameter, through `Type.GetType`, and through a second call are all the
        // same object.
        ParameterInfo[] parameters = typeof(Sample).GetMethod("Arrays").GetParameters();
        if (!ReferenceEquals(intArray, typeof(int[]))) return 12;
        if (!ReferenceEquals(intArray, parameters[0].ParameterType)) return 13;
        if (!ReferenceEquals(intArray, typeof(int).MakeArrayType())) return 14;
        if (!ReferenceEquals(intArray, Type.GetType("System.Int32[]"))) return 15;

        Type stringArray = typeof(string).MakeArrayType();
        if (stringArray.FullName != "System.String[]") return 16;
        if (!ReferenceEquals(stringArray, typeof(string[]))) return 17;

        Type objectArray = typeof(object).MakeArrayType();
        if (!ReferenceEquals(objectArray, typeof(object[]))) return 18;

        // Arrays of arrays: a jagged array over an szarray, and an szarray over a rank-2 array.
        Type jagged = typeof(int[]).MakeArrayType();
        if (jagged.Name != "Int32[][]") return 19;
        if (jagged.GetElementType() != typeof(int[])) return 20;
        if (!ReferenceEquals(jagged, parameters[2].ParameterType)) return 21;
        Type overRank2 = typeof(int[,]).MakeArrayType();
        if (overRank2.Name != "Int32[,][]") return 22;
        if (!overRank2.IsSZArray) return 23;
        if (overRank2.GetElementType() != typeof(int[,])) return 24;

        Type listArray = typeof(List<int>).MakeArrayType();
        if (listArray.Name != "List`1[]") return 25;
        if (listArray.GetElementType() != typeof(List<int>)) return 26;
        if (listArray.IsGenericType) return 27;
        if (!ReferenceEquals(listArray, parameters[1].ParameterType)) return 28;

        Type nestedArray = typeof(Sample.Nested<int>).MakeArrayType();
        if (nestedArray.FullName != typeof(Sample.Nested<int>[]).FullName) return 29;
        if (!ReferenceEquals(nestedArray, typeof(Sample.Nested<int>[]))) return 30;

        // A pointer is a TypeDesc, never byref-like, so `Span<int>*` would be a legal element too;
        // C# cannot spell that pointer, but `int*` and `void*` cover the pointer arm.
        unsafe
        {
            Type pointerArray = typeof(int*).MakeArrayType();
            if (pointerArray.FullName != "System.Int32*[]") return 31;
            if (!ReferenceEquals(pointerArray, typeof(int*[]))) return 32;
            Type voidPointerArray = typeof(void*).MakeArrayType();
            if (voidPointerArray.FullName != "System.Void*[]") return 33;
            Type fnptrArray = typeof(delegate*<int, string>).MakeArrayType();
            if (!fnptrArray.IsSZArray) return 34;
            if (!ReferenceEquals(fnptrArray, typeof(delegate*<int, string>[]))) return 35;
        }

        // Elements that are not closed: an open definition and a type variable. The array is a
        // TypeDesc over the variable, and reflection reports the variable itself beneath it -- the
        // same object as the definition's own type parameter. (An open *construction*,
        // `typeof(Nested<>).MakeGenericType(variable)`, is a legal element too, but reaching it
        // needs `RuntimeTypeHandle_Instantiate` to accept a type variable, which PawPrint's does
        // not yet; `TestMakeSZArrayRefusal.fs` covers that element against the host.)
        Type listDefinition = typeof(List<>);
        Type openArray = listDefinition.MakeArrayType();
        if (openArray.Name != "List`1[]") return 36;
        if (!openArray.ContainsGenericParameters) return 37;
        if (!ReferenceEquals(openArray.GetElementType(), listDefinition)) return 38;
        Type variable = listDefinition.GetGenericArguments()[0];
        Type variableArray = variable.MakeArrayType();
        if (variableArray.Name != "T[]") return 39;
        if (!ReferenceEquals(variableArray.GetElementType(), variable)) return 40;
        if (!ReferenceEquals(variableArray, listDefinition.GetMethod("ToArray").ReturnType)) return 41;

        // The type loader refuses three element kinds, in this order: a byref, a byref-like type,
        // and `void`. Each is a TypeLoadException whose message names the type and the assembly of
        // its element, and whose `TypeName` carries the same type string. For a byref element the
        // rendered name is the byref alone -- the type-name builder will not append `[]` after `&`
        // -- where the other two render the array being asked for.
        string coreLib = typeof(int).Assembly.FullName;
        string here = typeof(Sample).Assembly.FullName;

        if (Refused(typeof(int).MakeByRefType(), "System.Int32&", coreLib, "ByRef") is int r1) return 100 + r1;
        if (Refused(typeof(Sample.Nested<int>).MakeByRefType(), "Sample+Nested`1[System.Int32]&", here, "ByRef") is int r2) return 110 + r2;
        if (Refused(typeof(int[,]).MakeByRefType(), "System.Int32[,]&", coreLib, "ByRef") is int r3) return 120 + r3;
        if (Refused(typeof(void).MakeByRefType(), "System.Void&", coreLib, "ByRef") is int r4) return 130 + r4;
        if (Refused(variable.MakeByRefType(), "T&", coreLib, "ByRef") is int r5) return 140 + r5;
        unsafe
        {
            if (Refused(typeof(delegate*<int, string>).MakeByRefType(), "System.String(System.Int32)&", coreLib, "ByRef") is int r6) return 150 + r6;
        }
        // A byref over a byref-like type is refused as a byref: a byref is a TypeDesc, and no
        // TypeDesc is byref-like whatever its element is.
        if (Refused(typeof(Span<int>).MakeByRefType(), "System.Span`1[System.Int32]&", coreLib, "ByRef") is int r7) return 160 + r7;

        if (Refused(typeof(Span<int>), "System.Span`1[System.Int32][]", coreLib, "ByRef-like") is int r8) return 170 + r8;
        if (Refused(typeof(ReadOnlySpan<char>), "System.ReadOnlySpan`1[System.Char][]", coreLib, "ByRef-like") is int r9) return 180 + r9;
        if (Refused(typeof(TypedReference), "System.TypedReference[]", coreLib, "ByRef-like") is int r10) return 190 + r10;
        if (Refused(typeof(Sample.RefStruct), "Sample+RefStruct[]", here, "ByRef-like") is int r11) return 200 + r11;
        if (Refused(typeof(Sample.GenericRefStruct<int>), "Sample+GenericRefStruct`1[System.Int32][]", here, "ByRef-like") is int r12) return 210 + r12;
        // An open definition carries the byref-like flag too.
        if (Refused(typeof(Sample.GenericRefStruct<>), "Sample+GenericRefStruct`1[T][]", here, "ByRef-like") is int r13) return 220 + r13;
        if (Refused(typeof(Span<>), "System.Span`1[T][]", coreLib, "ByRef-like") is int r14) return 230 + r14;

        if (Refused(typeof(void), "System.Void[]", coreLib, "System.Void") is int r15) return 240 + r15;

        return 0;
    }

    // Null when the call behaves as CoreCLR does; otherwise a small code saying which check failed.
    static int? Refused(Type element, string expectedTypeName, string expectedAssembly, string why)
    {
        try
        {
            element.MakeArrayType();
            return 1;
        }
        catch (TypeLoadException e)
        {
            string expected = $"Could not create array type '{expectedTypeName}' from assembly '{expectedAssembly}' because the element type is {why}.";
            if (e.Message != expected)
            {
                Console.Error.WriteLine($"message: {e.Message}");
                return 2;
            }
            if (e.TypeName != expectedTypeName)
            {
                Console.Error.WriteLine($"TypeName: {e.TypeName}");
                return 3;
            }
        }

        return null;
    }
}

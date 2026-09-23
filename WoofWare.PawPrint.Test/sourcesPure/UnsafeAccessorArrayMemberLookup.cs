using System;
using System.Collections.Generic;
using System.Runtime.CompilerServices;

// An array target is legal -- modern CoreCLR gives arrays MethodTables rather than TypeDescs, so
// `ValidateTargetType` lets one through -- but the only members an array declares are its
// constructors and its `Get`/`Set`/`Address` accessors, and the accessors' signatures spell the
// element type as the class type variable `!0`, which no accessor declaration can spell. So every
// lookup not naming `.ctor` finds nothing, and is reported as such rather than as a malformed
// accessor: `Get` on `int[,]` is a *missing method*, not a `BadImageFormatException`.
//
// The target is named as `TypeHandle::GetName` names an array: the element's own name, then the
// rank suffix. That is not the reflection name in two ways, both measured on real .NET 10: a nested
// element type is not qualified by its encloser (`Inner[]`), though a nested *type argument* is
// (`GS`1[TestUnsafeAccessorArrayMemberLookup+Inner][]`); and a function pointer is `FNPTR`.
//
// The constructor, which does bind, is `sourcesPure/UnsafeAccessorArrayConstructor.cs` and
// `sourcesPure/UnsafeAccessorArrayConstructorShapes.cs`.
public unsafe class TestUnsafeAccessorArrayMemberLookup
{
    public class Inner
    {
    }

    public struct GS<T>
    {
    }

    // `Get`, `Set` and `Address` are real on a multi-dimensional array -- the runtime provides them
    // -- and still do not bind, so this is about which members are candidates rather than about
    // the name existing.
    [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "Get")]
    private static extern int RankTwoGet(int[,] a, int i, int j);

    [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "Set")]
    private static extern void RankTwoSet(int[,] a, int i, int j, int value);

    [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "Address")]
    private static extern ref int RankTwoAddress(int[,] a, int i, int j);

    [UnsafeAccessor(UnsafeAccessorKind.StaticMethod, Name = "Get")]
    private static extern int RankTwoStaticGet(int[,] a, int i, int j);

    [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "NoSuch")]
    private static extern int SzArrayMethod(int[] a);

    // Inherited from `System.Array`, and the lookup does not walk to a base class.
    [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "GetLength")]
    private static extern int SzArrayGetLength(int[] a, int dimension);

    // `_numComponents` is the field CoreCLR's own `RawArrayData` names for an array's length, so
    // this is a plausible guess at an array's internals rather than a nonsense name.
    [UnsafeAccessor(UnsafeAccessorKind.Field, Name = "_numComponents")]
    private static extern ref int SzArrayField(int[] a);

    [UnsafeAccessor(UnsafeAccessorKind.StaticField, Name = "NoSuch")]
    private static extern ref int RankTwoStaticField(int[,] a);

    [UnsafeAccessor(UnsafeAccessorKind.StaticMethod, Name = "NoSuch")]
    private static extern int ReferenceArrayStaticMethod(string[] a);

    [UnsafeAccessor(UnsafeAccessorKind.Field, Name = "x")]
    private static extern ref int RankThree(int[,,] a);

    [UnsafeAccessor(UnsafeAccessorKind.Field, Name = "x")]
    private static extern ref int Jagged(int[][] a);

    // C#'s `int[,][]` is an array of rank two whose element is `int[]`, and the name reads
    // element-first.
    [UnsafeAccessor(UnsafeAccessorKind.Field, Name = "x")]
    private static extern ref int JaggedOfRankTwo(int[,][] a);

    [UnsafeAccessor(UnsafeAccessorKind.Field, Name = "x")]
    private static extern ref int NestedElement(Inner[] a);

    [UnsafeAccessor(UnsafeAccessorKind.Field, Name = "x")]
    private static extern ref int GenericElement(Dictionary<string, List<int>>[] a);

    [UnsafeAccessor(UnsafeAccessorKind.Field, Name = "x")]
    private static extern ref int NestedTypeArgument(GS<Inner>[] a);

    [UnsafeAccessor(UnsafeAccessorKind.Field, Name = "x")]
    private static extern ref int NullableElement(int?[] a);

    [UnsafeAccessor(UnsafeAccessorKind.Field, Name = "x")]
    private static extern ref int PointerElement(int*[] a);

    [UnsafeAccessor(UnsafeAccessorKind.Field, Name = "x")]
    private static extern ref int FunctionPointerElement(delegate*<void>[] a);

    // The byref is stripped before the target is examined, so this is `int[]` too.
    [UnsafeAccessor(UnsafeAccessorKind.Field, Name = "x")]
    private static extern ref int ByrefToArray(ref int[] a);

    // A generic accessor whose type argument is a value type is compiled for that exact
    // instantiation, so the array it names is the exact one. A reference-type argument is shared,
    // and the same accessor names `System.__Canon[]` instead.
    [UnsafeAccessor(UnsafeAccessorKind.Field, Name = "x")]
    private static extern ref int OfGenericArgument<T>(T[] a);

    private static int Check<TExpected>(int code, string expectedMessage, int expectedHResult, Action a)
        where TExpected : Exception
    {
        try
        {
            a();
            return code;
        }
        catch (TExpected e)
        {
            if (e.Message != expectedMessage) return code + 100;
            if (e.HResult != expectedHResult) return code + 200;
            return 0;
        }
    }

    private const int MissingMethod = unchecked((int) 0x80131513);
    private const int MissingField = unchecked((int) 0x80131511);

    private static int Run()
    {
        int r;

        r = Check<MissingMethodException>(1, "Method not found: 'System.Int32[,].Get'.", MissingMethod, () => RankTwoGet(new int[2, 2], 0, 0));
        if (r != 0) return r;

        r = Check<MissingMethodException>(2, "Method not found: 'System.Int32[,].Set'.", MissingMethod, () => RankTwoSet(new int[2, 2], 0, 0, 1));
        if (r != 0) return r;

        r = Check<MissingMethodException>(3, "Method not found: 'System.Int32[,].Address'.", MissingMethod, () => RankTwoAddress(new int[2, 2], 0, 0));
        if (r != 0) return r;

        r = Check<MissingMethodException>(4, "Method not found: 'System.Int32[,].Get'.", MissingMethod, () => RankTwoStaticGet(null, 0, 0));
        if (r != 0) return r;

        r = Check<MissingMethodException>(5, "Method not found: 'System.Int32[].NoSuch'.", MissingMethod, () => SzArrayMethod(new int[1]));
        if (r != 0) return r;

        r = Check<MissingMethodException>(6, "Method not found: 'System.Int32[].GetLength'.", MissingMethod, () => SzArrayGetLength(new int[1], 0));
        if (r != 0) return r;

        r = Check<MissingFieldException>(7, "Field not found: 'System.Int32[]._numComponents'.", MissingField, () => SzArrayField(new int[1]));
        if (r != 0) return r;

        r = Check<MissingFieldException>(8, "Field not found: 'System.Int32[,].NoSuch'.", MissingField, () => RankTwoStaticField(null));
        if (r != 0) return r;

        r = Check<MissingMethodException>(9, "Method not found: 'System.String[].NoSuch'.", MissingMethod, () => ReferenceArrayStaticMethod(null));
        if (r != 0) return r;

        r = Check<MissingFieldException>(10, "Field not found: 'System.Int32[,,].x'.", MissingField, () => RankThree(new int[1, 1, 1]));
        if (r != 0) return r;

        r = Check<MissingFieldException>(11, "Field not found: 'System.Int32[][].x'.", MissingField, () => Jagged(new int[1][]));
        if (r != 0) return r;

        r = Check<MissingFieldException>(12, "Field not found: 'System.Int32[][,].x'.", MissingField, () => JaggedOfRankTwo(new int[1, 1][]));
        if (r != 0) return r;

        r = Check<MissingFieldException>(13, "Field not found: 'Inner[].x'.", MissingField, () => NestedElement(new Inner[1]));
        if (r != 0) return r;

        r = Check<MissingFieldException>(
            14,
            "Field not found: 'System.Collections.Generic.Dictionary`2[System.String,System.Collections.Generic.List`1[System.Int32]][].x'.",
            MissingField,
            () => GenericElement(new Dictionary<string, List<int>>[1]));
        if (r != 0) return r;

        r = Check<MissingFieldException>(
            15,
            "Field not found: 'GS`1[TestUnsafeAccessorArrayMemberLookup+Inner][].x'.",
            MissingField,
            () => NestedTypeArgument(new GS<Inner>[1]));
        if (r != 0) return r;

        r = Check<MissingFieldException>(16, "Field not found: 'System.Nullable`1[System.Int32][].x'.", MissingField, () => NullableElement(new int?[1]));
        if (r != 0) return r;

        r = Check<MissingFieldException>(17, "Field not found: 'System.Int32*[].x'.", MissingField, () => PointerElement(new int*[1]));
        if (r != 0) return r;

        r = Check<MissingFieldException>(18, "Field not found: 'FNPTR[].x'.", MissingField, () => FunctionPointerElement(new delegate*<void>[1]));
        if (r != 0) return r;

        r = Check<MissingFieldException>(
            19,
            "Field not found: 'System.Int32[].x'.",
            MissingField,
            () =>
            {
                int[] a = new int[1];
                ByrefToArray(ref a);
            });
        if (r != 0) return r;

        // A null receiver is never read: binding fails first.
        r = Check<MissingFieldException>(20, "Field not found: 'System.Int32[]._numComponents'.", MissingField, () => SzArrayField(null));
        if (r != 0) return r;

        r = Check<MissingFieldException>(21, "Field not found: 'System.Int32[].x'.", MissingField, () => OfGenericArgument<int>(new int[1]));
        if (r != 0) return r;

        r = Check<MissingFieldException>(
            22,
            "Field not found: 'GS`1[System.Int32][].x'.",
            MissingField,
            () => OfGenericArgument<GS<int>>(new GS<int>[1]));
        if (r != 0) return r;

        return 0;
    }

    public static int Main() => Run();
}

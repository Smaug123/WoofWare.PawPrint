using System;
using System.Runtime.CompilerServices;

// Every constructor an array declares binds as an accessor's target, and runs as `newobj` of it
// does. A szarray declares one constructor per level of szarray nesting in its element, plus one,
// and each extra argument allocates one more nested level; a multi-dimensional array declares one
// taking a length per dimension and one taking a lower bound and a length per dimension. Their
// parameters are `int` exactly, and the lookup compares the declaration's parameter count too, so
// any other shape is a missing `.ctor`, named as the array is. Measured on real .NET 10.
//
// The arguments are checked as `AllocateArrayEx` checks them, raising from the accessor: a
// negative length is an `OverflowException`, an over-long one an `OutOfMemoryException`, and a
// lower bound whose last index overflows an `ArgumentOutOfRangeException` with no parameter name.
// A jagged constructor checks a nested length only while allocating its first element, so a zero
// length at one level leaves the levels below it unchecked.
//
// An array whose element is a reference type other than a szarray has no constructors of its own:
// it shares `object`'s array's (`Module::CreateArrayMethodTable`), and the stub names its target by
// method rather than by type, so an accessor returning `string[]` builds an `object[]`. That is
// not what `new string[n]` does.
public class TestUnsafeAccessorArrayConstructorShapes
{
    public struct Pair
    {
        public int A;
        public string B;
    }

    public static class Counter
    {
        public static int Initialised;
    }

    // The accessors below are declared on a type whose initialiser counts, so that the order of
    // binding, initialisation and allocation can be seen.
    public static class Accessors
    {
        static Accessors()
        {
            Counter.Initialised++;
        }

        [UnsafeAccessor(UnsafeAccessorKind.Constructor)]
        public static extern int[] Sz(int n);

        [UnsafeAccessor(UnsafeAccessorKind.Constructor)]
        public static extern int[] SzWithTwo(int n, int m);
    }

    [UnsafeAccessor(UnsafeAccessorKind.Constructor)]
    private static extern int[][] Jagged(int n, int m);

    [UnsafeAccessor(UnsafeAccessorKind.Constructor)]
    private static extern int[][] JaggedOuterOnly(int n);

    [UnsafeAccessor(UnsafeAccessorKind.Constructor)]
    private static extern long[][][] Jagged3(int n, int m, int k);

    // C# reads an array type outside in: this is a szarray whose elements are `int[,]`, which
    // the runtime names `System.Int32[,][]`. Its element is not a szarray, so it is not jagged.
    [UnsafeAccessor(UnsafeAccessorKind.Constructor)]
    private static extern int[][,] SzArrayOfMultiDim(int n, int m);

    // And this is a rank-2 array whose elements are `int[]`, taking the two lengths of any rank-2
    // array.
    [UnsafeAccessor(UnsafeAccessorKind.Constructor)]
    private static extern int[,][] MultiDimOfSzArrays(int n, int m);

    [UnsafeAccessor(UnsafeAccessorKind.Constructor)]
    private static extern int[,] MultiDim(int n, int m);

    [UnsafeAccessor(UnsafeAccessorKind.Constructor)]
    private static extern Pair[,,] MultiDimOfStructs(int a, int b, int c);

    [UnsafeAccessor(UnsafeAccessorKind.Constructor)]
    private static extern string[,] BoundedMultiDim(int lb0, int n, int lb1, int m);

    [UnsafeAccessor(UnsafeAccessorKind.Constructor)]
    private static extern int[,] ThreeForRankTwo(int a, int b, int c);

    [UnsafeAccessor(UnsafeAccessorKind.Constructor)]
    private static extern int[,] UnsignedLength(uint n, int m);

    [UnsafeAccessor(UnsafeAccessorKind.Constructor)]
    private static extern T[] Generic<T>(int n);

    [UnsafeAccessor(UnsafeAccessorKind.Constructor)]
    private static extern string[] OfStrings(int n);

    [UnsafeAccessor(UnsafeAccessorKind.Constructor)]
    private static extern string[,] MultiDimOfStrings(int n, int m);

    [UnsafeAccessor(UnsafeAccessorKind.Constructor)]
    private static extern string[][] JaggedOfStrings(int n, int m);

    [UnsafeAccessor(UnsafeAccessorKind.Constructor)]
    private static extern Pair[] OfStructs(int n);

    public class Holder<T>
    {
        // `T` over a value type is its own canonical form, so this constructs `T[]` exactly.
        [UnsafeAccessor(UnsafeAccessorKind.Constructor)]
        public static extern T[] New(int n);
    }

    private static int Overflows(Func<object> make)
    {
        try
        {
            make();
            return 1;
        }
        catch (OverflowException)
        {
            return 0;
        }
    }

    private static bool MissingCtor(Func<object> make, string arrayName)
    {
        try
        {
            make();
            return false;
        }
        catch (MissingMethodException e)
        {
            return e.Message == "Method not found: '" + arrayName + "..ctor'.";
        }
    }

    private static int Run()
    {
        // Binding fails before the accessor's declaring type is initialised...
        if (!MissingCtor(() => Accessors.SzWithTwo(1, 2), "System.Int32[]")) return 1;
        if (Counter.Initialised != 0) return 2;

        // ... while a length the allocator refuses is refused after it.
        if (Overflows(() => Accessors.Sz(-1)) != 0) return 3;
        if (Counter.Initialised != 1) return 4;

        int[] sz = Accessors.Sz(3);
        if (sz.Length != 3 || sz.GetType() != typeof(int[])) return 5;

        int[][] jagged = Jagged(2, 3);
        if (jagged.Length != 2) return 10;
        if (jagged[0] == null || jagged[0].Length != 3) return 11;
        if (jagged[1] == null || jagged[1].Length != 3) return 12;
        if (ReferenceEquals(jagged[0], jagged[1])) return 13;
        jagged[0][2] = 7;
        if (jagged[1][2] != 0) return 14;

        int[][] outerOnly = JaggedOuterOnly(2);
        if (outerOnly.Length != 2 || outerOnly[0] != null) return 15;

        long[][][] jagged3 = Jagged3(2, 1, 4);
        if (jagged3[1][0].Length != 4 || jagged3[1][0][3] != 0L) return 16;

        // A zero length leaves the nested levels unchecked; a non-zero one checks them.
        if (Jagged(0, -1).Length != 0) return 17;
        if (Jagged3(1, 0, -1)[0].Length != 0) return 18;
        if (Overflows(() => Jagged(1, -1)) != 0) return 19;
        if (Overflows(() => Jagged3(2, 3, -5)) != 0) return 20;

        int[,] rect = MultiDim(2, 3);
        if (rect.GetLength(0) != 2 || rect.GetLength(1) != 3) return 30;
        rect[1, 2] = 9;
        if (rect[1, 2] != 9 || rect[0, 0] != 0) return 31;

        int[,][] ofSzArrays = MultiDimOfSzArrays(2, 1);
        if (ofSzArrays.GetLength(0) != 2 || ofSzArrays.GetLength(1) != 1 || ofSzArrays[1, 0] != null) return 35;

        Pair[,,] structs = MultiDimOfStructs(1, 2, 2);
        if (structs.Length != 4 || structs[0, 1, 1].A != 0 || structs[0, 1, 1].B != null) return 32;

        string[,] bounded = BoundedMultiDim(0, 2, 0, 1);
        if (bounded.GetLength(0) != 2 || bounded.GetLength(1) != 1) return 33;
        if (bounded.GetLowerBound(0) != 0 || bounded[1, 0] != null) return 34;

        if (Overflows(() => MultiDim(3, -1)) != 0) return 40;

        try
        {
            MultiDim(0x7FFFFFC8, 0);
            return 41;
        }
        catch (OutOfMemoryException e)
        {
            if (e.Message != "Array dimensions exceeded supported range.") return 42;
        }

        try
        {
            BoundedMultiDim(int.MaxValue, 2, 0, 1);
            return 43;
        }
        catch (ArgumentOutOfRangeException e)
        {
            if (e.ParamName != null) return 44;
            if (e.Message != "Higher indices will exceed Int32.MaxValue because of large lower bound and/or length.") return 45;
        }

        // The lower-bound check is made dimension by dimension, so it beats a negative length in a
        // later dimension.
        try
        {
            BoundedMultiDim(int.MaxValue, 2, 0, -1);
            return 46;
        }
        catch (ArgumentOutOfRangeException)
        {
        }

        if (!MissingCtor(() => ThreeForRankTwo(1, 2, 3), "System.Int32[,]")) return 50;
        if (!MissingCtor(() => UnsignedLength(1, 2), "System.Int32[,]")) return 51;
        if (!MissingCtor(() => SzArrayOfMultiDim(1, 2), "System.Int32[,][]")) return 52;
        // A generic declaration does not match a non-generic constructor.
        if (!MissingCtor(() => Generic<int>(1), "System.Int32[]")) return 53;

        string[] strings = OfStrings(2);
        if (strings.GetType() != typeof(object[]) || strings.Length != 2 || strings[1] != null) return 60;
        object[] asObjects = strings;
        asObjects[0] = 5;
        if (!(asObjects[0] is int)) return 62;

        if (MultiDimOfStrings(2, 3).GetType() != typeof(object[,])) return 63;

        // A szarray element is not shared, at either level.
        string[][] jaggedStrings = JaggedOfStrings(2, 1);
        if (jaggedStrings.GetType() != typeof(string[][])) return 64;
        if (jaggedStrings[1].GetType() != typeof(string[])) return 65;

        if (OfStructs(1).GetType() != typeof(Pair[])) return 66;

        int[] viaGeneric = Holder<int>.New(2);
        if (viaGeneric.GetType() != typeof(int[]) || viaGeneric.Length != 2) return 61;

        return 0;
    }

    public static int Main() => Run();
}

using System.Runtime.CompilerServices;

// `Unsafe.As<TFrom, TTo>(ref TFrom)` where one of the type arguments is an array type rather than
// a named type. Every struct here holds exactly one reference, at offset 0, so each reinterpret
// names the same cell as the struct's field.

struct Holder
{
    public int[] Array;
}

struct MultiHolder
{
    public int[,] Grid;
}

class Program
{
    static int ReadThroughArrayView()
    {
        Holder holder = new Holder { Array = new[] { 1, 2, 3 } };

        ref int[] asArray = ref Unsafe.As<Holder, int[]>(ref holder);

        if (asArray.Length != 3)
            return 1;

        if (asArray[2] != 3)
            return 2;

        return 0;
    }

    static int WriteThroughArrayView()
    {
        Holder holder = new Holder { Array = new[] { 1, 2, 3 } };

        ref int[] asArray = ref Unsafe.As<Holder, int[]>(ref holder);
        asArray = new[] { 7, 8 };

        if (holder.Array.Length != 2)
            return 11;

        if (holder.Array[1] != 8)
            return 12;

        asArray[0] = 42;

        if (holder.Array[0] != 42)
            return 13;

        return 0;
    }

    static int NullThroughArrayView()
    {
        Holder holder = default;

        ref int[] asArray = ref Unsafe.As<Holder, int[]>(ref holder);

        if (asArray != null)
            return 21;

        return 0;
    }

    static int MultiDimensionalView()
    {
        int[,] grid = new int[2, 3];
        grid[1, 2] = 5;
        MultiHolder holder = new MultiHolder { Grid = grid };

        ref int[,] asGrid = ref Unsafe.As<MultiHolder, int[,]>(ref holder);

        if (asGrid.GetLength(1) != 3)
            return 31;

        if (asGrid[1, 2] != 5)
            return 32;

        return 0;
    }

    static int ArraySourceStructTarget()
    {
        int[] array = new[] { 4, 5, 6 };

        ref Holder asHolder = ref Unsafe.As<int[], Holder>(ref array);

        if (asHolder.Array[1] != 5)
            return 41;

        asHolder.Array = new[] { 9 };

        if (array.Length != 1 || array[0] != 9)
            return 42;

        return 0;
    }

    static int ArrayViewOfJaggedElement()
    {
        int[][] jagged = new[] { new[] { 1 }, new[] { 2, 3 } };

        ref object asObject = ref Unsafe.As<int[], object>(ref jagged[1]);
        ref int[] back = ref Unsafe.As<object, int[]>(ref asObject);

        if (back.Length != 2 || back[1] != 3)
            return 51;

        back = new[] { 10, 20, 30 };

        if (jagged[1].Length != 3 || jagged[1][2] != 30)
            return 52;

        return 0;
    }

    static int Main(string[] args)
    {
        int result;

        result = ReadThroughArrayView();
        if (result != 0)
            return result;

        result = WriteThroughArrayView();
        if (result != 0)
            return result;

        result = NullThroughArrayView();
        if (result != 0)
            return result;

        result = MultiDimensionalView();
        if (result != 0)
            return result;

        result = ArraySourceStructTarget();
        if (result != 0)
            return result;

        result = ArrayViewOfJaggedElement();
        if (result != 0)
            return result;

        return 0;
    }
}

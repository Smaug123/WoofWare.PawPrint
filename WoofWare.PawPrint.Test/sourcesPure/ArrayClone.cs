using System;

public class TestArrayClone
{
    private struct Point
    {
        public int X;
        public int Y;
    }

    private sealed class Box
    {
        public int Value;
    }

    // A clone of a primitive szarray has the same length and contents, but is a
    // distinct object whose elements can be mutated independently.
    private static int TestPrimitiveSzArray()
    {
        int[] original = new int[] { 1, 2, 3, 4, 5 };
        int[] clone = (int[])original.Clone();

        if (ReferenceEquals(clone, original)) return 1;
        if (clone.Length != 5) return 2;

        for (int i = 0; i < original.Length; i++)
        {
            if (clone[i] != original[i]) return 3 + i;
        }

        clone[0] = 100;
        if (original[0] != 1) return 10;
        if (clone[0] != 100) return 11;

        original[4] = 500;
        if (clone[4] != 5) return 12;

        return 0;
    }

    // Cloning is shallow: the element references are copied verbatim, so the
    // referents stay shared between original and clone.
    private static int TestReferenceElementsAreShallow()
    {
        Box shared = new Box { Value = 7 };
        Box[] original = new Box[] { shared, null, new Box { Value = 9 } };
        Box[] clone = (Box[])original.Clone();

        if (ReferenceEquals(clone, original)) return 20;
        if (clone.Length != 3) return 21;
        if (!ReferenceEquals(clone[0], shared)) return 22;
        if (clone[1] != null) return 23;
        if (!ReferenceEquals(clone[2], original[2])) return 24;

        // Mutating the shared referent is visible through both arrays.
        shared.Value = 42;
        if (clone[0].Value != 42) return 25;
        if (original[0].Value != 42) return 26;

        // Overwriting a slot in the clone does not disturb the original.
        clone[0] = null;
        if (!ReferenceEquals(original[0], shared)) return 27;

        return 0;
    }

    // Value-type elements are copied by value, so a mutation through the clone
    // must not be observable through the original.
    private static int TestValueTypeElements()
    {
        Point[] original = new Point[2];
        original[0].X = 1;
        original[0].Y = 2;
        original[1].X = 3;
        original[1].Y = 4;

        Point[] clone = (Point[])original.Clone();

        if (clone.Length != 2) return 30;
        if (clone[0].X != 1 || clone[0].Y != 2) return 31;
        if (clone[1].X != 3 || clone[1].Y != 4) return 32;

        clone[0].X = 99;
        if (original[0].X != 1) return 33;
        if (clone[0].X != 99) return 34;

        return 0;
    }

    private static int TestEmptyArray()
    {
        string[] original = new string[0];
        string[] clone = (string[])original.Clone();

        if (ReferenceEquals(clone, original)) return 40;
        if (clone.Length != 0) return 41;

        return 0;
    }

    // The clone keeps the original's exact runtime type, including when the
    // static type is a supertype (array covariance).
    private static int TestClonePreservesRuntimeType()
    {
        string[] strings = new string[] { "a", "b" };
        Array asArray = strings;
        object cloned = asArray.Clone();

        if (cloned.GetType() != typeof(string[])) return 50;
        if (!(cloned is string[])) return 51;

        string[] typed = (string[])cloned;
        if (typed.Length != 2) return 52;
        if (typed[0] != "a") return 53;
        if (typed[1] != "b") return 54;

        int[] ints = new int[] { 1 };
        if (ints.Clone().GetType() != typeof(int[])) return 55;

        return 0;
    }

    // Cloning a jagged array copies the outer references only.
    private static int TestJaggedArray()
    {
        int[][] original = new int[2][];
        original[0] = new int[] { 1, 2 };
        original[1] = new int[] { 3 };

        int[][] clone = (int[][])original.Clone();

        if (ReferenceEquals(clone, original)) return 70;
        if (clone.Length != 2) return 71;
        if (!ReferenceEquals(clone[0], original[0])) return 72;
        if (!ReferenceEquals(clone[1], original[1])) return 73;

        // The inner arrays are shared, so a write through one is seen by both.
        clone[0][0] = 11;
        if (original[0][0] != 11) return 74;

        return 0;
    }

    // Rank-2 arrays clone their full flat backing store and per-dimension lengths.
    private static int TestMultiDimensionalArray()
    {
        int[,] original = new int[2, 3];
        for (int i = 0; i < 2; i++)
        {
            for (int j = 0; j < 3; j++)
            {
                original[i, j] = (i * 3) + j;
            }
        }

        int[,] clone = (int[,])original.Clone();

        // Deliberately no GetLength/Length assertions here: `Array.GetLength` is itself an
        // unimplemented intrinsic. Indexing is the stronger check anyway, since the row-major
        // offset computation reads the clone's own per-dimension lengths.
        if (ReferenceEquals(clone, original)) return 80;

        for (int i = 0; i < 2; i++)
        {
            for (int j = 0; j < 3; j++)
            {
                if (clone[i, j] != (i * 3) + j) return 84;
            }
        }

        clone[1, 2] = 99;
        if (original[1, 2] != 5) return 85;
        if (clone[1, 2] != 99) return 86;

        return 0;
    }

    public static int Main(string[] argv)
    {
        int result;

        result = TestPrimitiveSzArray();
        if (result != 0) return 1000 + result;

        result = TestReferenceElementsAreShallow();
        if (result != 0) return 2000 + result;

        result = TestValueTypeElements();
        if (result != 0) return 3000 + result;

        result = TestEmptyArray();
        if (result != 0) return 4000 + result;

        result = TestClonePreservesRuntimeType();
        if (result != 0) return 5000 + result;

        result = TestJaggedArray();
        if (result != 0) return 7000 + result;

        result = TestMultiDimensionalArray();
        if (result != 0) return 8000 + result;

        return 0;
    }
}

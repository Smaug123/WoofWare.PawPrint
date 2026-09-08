using System;

// `Array.CreateInstance` and `Array.CreateInstanceFromArrayType` above rank 1. Every
// overload bottoms out in the `Array_CreateInstance` QCall with a `rank`, an `int*` of
// lengths, and an `int*` of lower bounds that is null unless the caller supplied one.
// The lengths reach the QCall in two shapes: the fixed-arity overloads `stackalloc` them,
// and the `int[]` overloads pin the caller's array with `fixed`. The lengths are chosen
// asymmetric so that a stride or ordering mistake on the native side reads as a wrong
// shape rather than passing by accident.
//
// Only zero lower bounds are exercised: an array with a non-zero lower bound is a
// different runtime shape, and this guest is about the rank.
public class TestArrayCreateInstanceMultiDim
{
    private struct Pair
    {
        public int A;
        public long B;
    }

    // stackalloc'd lengths, rank 2, primitive element.
    private static int Int2D()
    {
        Array created = Array.CreateInstance(typeof(int), 2, 3);
        if (created.Rank != 2) return 1;
        if (created.Length != 6) return 2;
        if (created.GetLength(0) != 2) return 3;
        if (created.GetLength(1) != 3) return 4;
        if (created.GetLowerBound(1) != 0) return 5;
        if (created.GetUpperBound(1) != 2) return 6;
        if (created.GetType() != typeof(int[,])) return 7;

        int[,] typed = created as int[,];
        if (typed == null) return 8;

        for (int i = 0; i < 2; i++)
            for (int j = 0; j < 3; j++)
                typed[i, j] = 10 * i + j + 1;

        if (typed[1, 2] != 13) return 9;
        if ((int)created.GetValue(1, 2) != 13) return 10;
        if ((int)created.GetValue(0, 1) != 2) return 11;

        created.SetValue(77, 1, 0);
        if (typed[1, 0] != 77) return 12;

        return 0;
    }

    // stackalloc'd lengths, rank 3, reference element.
    private static int String3D()
    {
        Array created = Array.CreateInstance(typeof(string), 1, 2, 3);
        if (created.Rank != 3) return 1;
        if (created.Length != 6) return 2;
        if (created.GetLength(0) != 1) return 3;
        if (created.GetLength(1) != 2) return 4;
        if (created.GetLength(2) != 3) return 5;
        if (created.GetType() != typeof(string[,,])) return 6;

        string[,,] typed = (string[,,])created;
        if (typed[0, 1, 2] != null) return 7;
        typed[0, 1, 2] = "corner";
        typed[0, 0, 0] = "origin";
        if ((string)created.GetValue(0, 1, 2) != "corner") return 8;
        if ((string)created.GetValue(0, 0, 0) != "origin") return 9;
        if (created.GetValue(0, 1, 1) != null) return 10;

        return 0;
    }

    // Pinned `int[]` of lengths, rank 4, struct element.
    private static int Struct4D()
    {
        Array created = Array.CreateInstance(typeof(Pair), new int[] { 3, 1, 2, 4 });
        if (created.Rank != 4) return 1;
        if (created.Length != 24) return 2;
        if (created.GetLength(0) != 3) return 3;
        if (created.GetLength(1) != 1) return 4;
        if (created.GetLength(2) != 2) return 5;
        if (created.GetLength(3) != 4) return 6;
        if (created.GetType() != typeof(Pair[,,,])) return 7;

        Pair[,,,] typed = (Pair[,,,])created;
        if (typed[2, 0, 1, 3].A != 0 || typed[2, 0, 1, 3].B != 0L) return 8;
        typed[2, 0, 1, 3].A = 5;
        typed[2, 0, 1, 3].B = -6L;
        typed[0, 0, 0, 0].A = 1;
        if (typed[2, 0, 1, 3].A != 5 || typed[2, 0, 1, 3].B != -6L) return 9;
        if (typed[0, 0, 0, 0].A != 1) return 10;
        if (typed[1, 0, 1, 3].A != 0) return 11;

        return 0;
    }

    // Pinned lengths and pinned lower bounds, every lower bound zero, rank 2.
    private static int ZeroLowerBounds2D()
    {
        Array created = Array.CreateInstance(typeof(byte), new int[] { 4, 2 }, new int[] { 0, 0 });
        if (created.Rank != 2) return 1;
        if (created.Length != 8) return 2;
        if (created.GetLength(0) != 4) return 3;
        if (created.GetLength(1) != 2) return 4;
        if (created.GetLowerBound(0) != 0) return 5;
        if (created.GetLowerBound(1) != 0) return 6;
        if (created.GetType() != typeof(byte[,])) return 7;

        byte[,] typed = (byte[,])created;
        typed[3, 1] = 200;
        if ((byte)created.GetValue(3, 1) != 200) return 8;

        return 0;
    }

    // A zero-length dimension empties the array without disturbing the other lengths.
    private static int ZeroDimension()
    {
        Array created = Array.CreateInstance(typeof(int), 2, 0);
        if (created.Rank != 2) return 1;
        if (created.Length != 0) return 2;
        if (created.GetLength(0) != 2) return 3;
        if (created.GetLength(1) != 0) return 4;
        if (created.GetUpperBound(1) != -1) return 5;
        if (!(created is int[,])) return 6;

        Array created3 = Array.CreateInstance(typeof(string), new int[] { 0, 5, 7 });
        if (created3.Length != 0) return 7;
        if (created3.GetLength(1) != 5) return 8;
        if (created3.GetLength(2) != 7) return 9;

        return 0;
    }

    // From the array type rather than the element type: the QCall's fromArrayType branch.
    private static int FromArrayType()
    {
        Array created = Array.CreateInstanceFromArrayType(typeof(int[,]), 2, 5);
        if (created.Rank != 2) return 1;
        if (created.Length != 10) return 2;
        if (created.GetLength(0) != 2) return 3;
        if (created.GetLength(1) != 5) return 4;
        if (created.GetType() != typeof(int[,])) return 5;

        int[,] typed = (int[,])created;
        typed[1, 4] = 42;
        if ((int)created.GetValue(1, 4) != 42) return 6;

        Array withBounds = Array.CreateInstanceFromArrayType(typeof(long[,,]), new int[] { 1, 2, 3 }, new int[] { 0, 0, 0 });
        if (withBounds.Rank != 3) return 7;
        if (withBounds.Length != 6) return 8;
        if (withBounds.GetLength(2) != 3) return 9;
        if (withBounds.GetType() != typeof(long[,,])) return 10;

        long[,,] typedBounds = (long[,,])withBounds;
        typedBounds[0, 1, 2] = long.MinValue;
        if ((long)withBounds.GetValue(0, 1, 2) != long.MinValue) return 11;

        return 0;
    }

    // Rank 1 through the same overloads must still make a szarray, not a rank-1
    // multi-dimensional array.
    private static int Rank1StaysSzArray()
    {
        Array created = Array.CreateInstance(typeof(int), new int[] { 3 });
        if (created.Rank != 1) return 1;
        if (created.GetType() != typeof(int[])) return 2;
        if (!(created is int[])) return 3;

        Array withBounds = Array.CreateInstance(typeof(int), new int[] { 3 }, new int[] { 0 });
        if (withBounds.GetType() != typeof(int[])) return 4;

        // The single-length overload passes the address of its own parameter.
        Array single = Array.CreateInstance(typeof(int), 3);
        if (single.GetType() != typeof(int[])) return 5;
        if (single.Length != 3) return 6;

        return 0;
    }

    public static int Main()
    {
        int r;
        if ((r = Int2D()) != 0) return 100 + r;
        if ((r = String3D()) != 0) return 120 + r;
        if ((r = Struct4D()) != 0) return 140 + r;
        if ((r = ZeroLowerBounds2D()) != 0) return 160 + r;
        if ((r = ZeroDimension()) != 0) return 180 + r;
        if ((r = FromArrayType()) != 0) return 200 + r;
        if ((r = Rank1StaysSzArray()) != 0) return 220 + r;
        return 0;
    }
}

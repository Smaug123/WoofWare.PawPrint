using System;
using System.Collections.Generic;

public class GetElementTypeBasic
{
    public static int Main(string[] argv)
    {
        // 1d szarray: int[] -> int
        Type intArrayElement = typeof(int[]).GetElementType();
        if (!object.ReferenceEquals(intArrayElement, typeof(int))) return 1;

        // Multi-dim array: int[,] -> int (rank is dropped, not preserved)
        Type intMatrixElement = typeof(int[,]).GetElementType();
        if (!object.ReferenceEquals(intMatrixElement, typeof(int))) return 2;

        // Higher-rank array: int[,,] -> int
        Type intCubeElement = typeof(int[,,]).GetElementType();
        if (!object.ReferenceEquals(intCubeElement, typeof(int))) return 3;

        // Jagged array: int[][] -> int[] (element is itself a wrapper; exercises
        // re-allocation through the type-handle registry).
        Type jaggedElement = typeof(int[][]).GetElementType();
        if (!object.ReferenceEquals(jaggedElement, typeof(int[]))) return 4;

        // Reference-type array: string[] -> string
        Type stringArrayElement = typeof(string[]).GetElementType();
        if (!object.ReferenceEquals(stringArrayElement, typeof(string))) return 5;

        // Concrete primitive: int -> null
        if (typeof(int).GetElementType() != null) return 6;

        // Concrete reference type: string -> null
        if (typeof(string).GetElementType() != null) return 7;

        // Closed generic: List<int> -> null
        if (typeof(List<int>).GetElementType() != null) return 8;

        // Open generic type definition: List<> -> null
        if (typeof(List<>).GetElementType() != null) return 9;

        return 0;
    }
}

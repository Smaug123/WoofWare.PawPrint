using System;
using System.Collections.Generic;

public static class Program
{
    // `Type.GetArrayRank` on a RuntimeType is the `RuntimeTypeHandle.GetArrayRank` FCall behind a
    // managed screen that throws ArgumentException for anything that is not an array. The rank is
    // a fact about the array shape alone: a vector answers 1 whatever its element, and a
    // multi-dimensional array answers its declared rank.
    //
    // Exit code is the index of the first failing check, so a failure names itself.
    public static int Main()
    {
        if (typeof(int[]).GetArrayRank() != 1) return 1;
        if (typeof(string[,]).GetArrayRank() != 2) return 2;
        if (typeof(object[,,]).GetArrayRank() != 3) return 3;

        // A generic instantiation as the element makes no difference to the shape.
        if (typeof(List<int>[]).GetArrayRank() != 1) return 4;
        if (typeof(List<string>[,]).GetArrayRank() != 2) return 5;

        // A jagged array is a vector whose element is itself an array: rank 1, not 2.
        if (typeof(int[][]).GetArrayRank() != 1) return 6;
        if (typeof(int[][]).GetElementType()!.GetArrayRank() != 1) return 7;

        // The type of an array instance, reached at runtime rather than through a token.
        if (new int[2, 3].GetType().GetArrayRank() != 2) return 8;
        if (new string[4].GetType().GetArrayRank() != 1) return 9;

        // The managed screen refuses a non-array before the FCall is reached.
        if (NonArrayRefused(typeof(int)) is int r1) return 20 + r1;
        if (NonArrayRefused(typeof(string)) is int r2) return 30 + r2;
        if (NonArrayRefused(typeof(List<int>)) is int r3) return 40 + r3;
        if (NonArrayRefused(typeof(int).MakeByRefType()) is int r4) return 50 + r4;
        if (NonArrayRefused(typeof(int[]).MakeByRefType()) is int r5) return 60 + r5;
        if (NonArrayRefused(typeof(IDisposable)) is int r6) return 70 + r6;
        if (NonArrayRefused(typeof(List<>)) is int r7) return 80 + r7;
        if (NonArrayRefused(typeof(List<>).GetGenericArguments()[0]) is int r8) return 90 + r8;

        return 0;
    }

    // Null when the call throws as CoreCLR does; otherwise a small code saying which check failed.
    static int? NonArrayRefused(Type type)
    {
        try
        {
            type.GetArrayRank();
            return 1;
        }
        catch (ArgumentException e)
        {
            if (e.Message != "Must be an array type.")
            {
                Console.Error.WriteLine($"message: {e.Message}");
                return 2;
            }
            if (e.ParamName != null) return 3;
        }

        return null;
    }
}

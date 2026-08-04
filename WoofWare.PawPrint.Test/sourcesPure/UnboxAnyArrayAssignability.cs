// `unbox.any` with an array-typed token must consult the *whole* array assignability
// rule, not just handle identity: reference-element covariance (string[] -> object[]),
// the SZ-array implicit-generic-interface carve-out (int[] -> IList<int>), and the
// System.Array / non-generic-interface base chain.
//
// Mirrors the coverage of CastClassArrayCovariance / SzArrayImplicitInterfaceCastRef,
// but routed through `unbox.any !!T` instead of `castclass`.

using System;
using System.Collections.Generic;

public class TestUnboxAnyArrayAssignability
{
    private static T Cast<T>(object o)
    {
        return (T) o;
    }

    private static bool CastThrows<T>(object o)
    {
        try
        {
            T _ = Cast<T>(o);
            return false;
        }
        catch (InvalidCastException)
        {
            return true;
        }
    }

    public static int Main(string[] argv)
    {
        object strings = new string[] { "x", "y" };

        // Reference-element covariance: string[] is an object[].
        object[] asObjects = Cast<object[]>(strings);
        if (asObjects == null) return 1;
        if (asObjects.Length != 2) return 2;
        if (!(asObjects[0] is string)) return 3;

        // ... but not the other way round.
        object objects = new object[] { "x" };
        if (!CastThrows<string[]>(objects)) return 4;

        // SZ-array implicit generic interfaces. Only the cast itself is exercised here;
        // dispatching members through them is a separate concern.
        IList<int> asIntList = Cast<IList<int>>(new int[] { 1 });
        if (asIntList == null) return 5;

        IEnumerable<object> asObjectEnumerable = Cast<IEnumerable<object>>(strings);
        if (asObjectEnumerable == null) return 6;

        // The carve-out is SZ-array-only: a rank-2 array implements none of them.
        object grid = new int[1, 1];
        if (!CastThrows<IList<int>>(grid)) return 7;

        // Every array, of any rank, is a System.Array.
        Array gridAsArray = Cast<Array>(grid);
        if (gridAsArray == null) return 8;
        if (gridAsArray.Rank != 2) return 9;

        // Nested arrays follow the same element rule: string[][] is an object[].
        object jagged = new string[1][];
        object[] jaggedAsObjects = Cast<object[]>(jagged);
        if (jaggedAsObjects == null) return 10;

        return 0;
    }
}

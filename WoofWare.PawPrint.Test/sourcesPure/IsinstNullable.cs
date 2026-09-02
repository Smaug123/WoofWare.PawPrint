// ECMA-335 III.4.6: when the type token of `isinst` is `Nullable<T>`, it is interpreted as a boxed
// `T`, because a `Nullable<T>` boxes as a `T` (or as null) and never as itself. So a boxed `T` "is"
// a `Nullable<T>` even though `T` is not structurally assignable to `Nullable<T>`. CoreCLR applies
// the rule in `ObjIsInstanceOfCore` (`Nullable::IsNullableForType`) ahead of the ordinary cast walk,
// and the match is exact: no widening, no enum-to-underlying, no reference type.
//
// Every shape here is `isinst` at the IL level, on the paths Roslyn actually emits: a bare `is`
// test, `is` on a generic method parameter, `is` with a declaration pattern, and `as` (which is
// `isinst` followed by `unbox.any`, so a wrongly-null `isinst` shows up as `HasValue == false`).

using System;

public enum Colour
{
    Red = 0,
    Green = 1,
}

public struct Pair
{
    public int A;
    public int B;
}

public class TestIsinstNullable
{
    private static bool Is<T>(object o)
    {
        return o is T;
    }

    public static int Main(string[] argv)
    {
        object boxedInt = 5;
        object boxedLong = 5L;
        object boxedEnum = Colour.Green;
        object boxedPair = new Pair { A = 1, B = 2 };
        object str = "hello";
        object array = new int[] { 1 };
        object nothing = null;

        // A boxed T is a Nullable<T>.
        if (!(boxedInt is int?)) return 1;
        if (!(boxedEnum is Colour?)) return 2;
        if (!(boxedPair is Pair?)) return 3;

        // Exact match on T: no numeric widening in either direction.
        if (boxedLong is int?) return 4;
        if (boxedInt is long?) return 5;

        // An enum and its underlying type are distinct, both ways round.
        if (boxedEnum is int?) return 6;
        if (boxedInt is Colour?) return 7;

        // An unrelated struct, a reference type, an array and null are never a boxed T.
        if (boxedInt is Pair?) return 8;
        if (str is int?) return 9;
        if (array is int?) return 10;
        if (nothing is int?) return 11;

        // The same rule through `isinst !!T`.
        if (!Is<int?>(boxedInt)) return 12;
        if (!Is<Pair?>(boxedPair)) return 13;
        if (Is<int?>(boxedLong)) return 14;
        if (Is<long?>(boxedInt)) return 15;
        if (Is<int?>(nothing)) return 16;

        // `as`: the `isinst` result feeds `unbox.any Nullable<int>`, which turns a null into a
        // Nullable without a value, so the payload is read back after the test.
        int? asInt = boxedInt as int?;
        if (!asInt.HasValue || asInt.Value != 5) return 17;
        int? asFromLong = boxedLong as int?;
        if (asFromLong.HasValue) return 18;
        Pair? asPair = boxedPair as Pair?;
        if (!asPair.HasValue || asPair.Value.A != 1 || asPair.Value.B != 2) return 19;

        // Boxing a Nullable<T> that has a value yields a boxed T, which matches T? and T alike;
        // boxing one without a value yields null, which matches neither.
        int? withValue = 7;
        object boxedWithValue = withValue;
        if (!(boxedWithValue is int?)) return 20;
        if (!(boxedWithValue is int)) return 21;
        int? withoutValue = null;
        object boxedWithoutValue = withoutValue;
        if (boxedWithoutValue != null) return 22;
        if (boxedWithoutValue is int?) return 23;
        if (boxedWithoutValue is int) return 24;

        return 0;
    }
}

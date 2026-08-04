// ECMA-335 III.4.33 / CoreCLR `Nullable::UnBox`: `unbox.any Nullable<T>` reconstructs the
// Nullable from the two forms `box` can produce for one — null, or a boxed `T`.
//
// The null case is the interesting one: it is the single value-typed `unbox.any` target that
// accepts a null operand, yielding a zeroed Nullable rather than raising NullReferenceException.
//
// `Cast<T>` forces the `unbox.any !!T` token form, which reaches the same code path via a
// generic instantiation rather than a direct type token.

public class TestUnboxAnyNullableInt
{
    private static T Cast<T>(object o)
    {
        return (T) o;
    }

    public static int Main(string[] argv)
    {
        // Boxed T -> Nullable<T> with HasValue = true.
        object boxed = 42;
        int? v = (int?) boxed;
        if (!v.HasValue) return 1;
        if (v.Value != 42) return 2;

        // Null -> Nullable<T> with HasValue = false, and *not* an exception.
        object nothing = null;
        int? w = (int?) nothing;
        if (w.HasValue) return 3;
        if (w.GetValueOrDefault() != 0) return 4;

        // Same two cases through the `unbox.any !!T` token form.
        int? viaGeneric = Cast<int?>(boxed);
        if (!viaGeneric.HasValue) return 5;
        if (viaGeneric.Value != 42) return 6;

        int? nullViaGeneric = Cast<int?>(null);
        if (nullViaGeneric.HasValue) return 7;

        // Default(T) is preserved for a non-zero-defaulting payload too.
        object boxedFalse = false;
        bool? b = (bool?) boxedFalse;
        if (!b.HasValue) return 8;
        if (b.Value) return 9;

        return 0;
    }
}

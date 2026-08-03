// `unbox.any Nullable<T>` matches the boxed operand against `T` by exact equivalence, not by
// assignability or numeric widening (CoreCLR `Nullable::IsNullableForTypeHelper` compares against
// `GetInstantiation()[0]`). Everything that is not a boxed `T` — and not null — raises
// InvalidCastException.

using System;

public enum Colour
{
    Red = 0,
    Green = 1,
}

public class TestUnboxAnyNullableNegative
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
        // No widening: a boxed short/long is not a boxed int.
        if (!CastThrows<int?>((object) 1L)) return 1;
        if (!CastThrows<int?>((object) (short) 1)) return 2;
        if (!CastThrows<long?>((object) 1)) return 3;

        // Unsigned counterpart of the same width is still a different type.
        if (!CastThrows<int?>((object) 1u)) return 4;

        // A reference type is never a boxed T.
        if (!CastThrows<int?>("hello")) return 5;
        if (!CastThrows<int?>(new int[] { 1 })) return 6;

        // An enum and its underlying type are distinct for this purpose, both ways round.
        if (!CastThrows<int?>((object) Colour.Green)) return 7;
        if (!CastThrows<Colour?>((object) 1)) return 8;

        // An unrelated struct.
        if (!CastThrows<int?>((object) 1.0)) return 9;

        // Sanity: the matching case really does succeed, so the above are not all throwing
        // for some unrelated reason.
        int? ok = Cast<int?>((object) 1);
        if (!ok.HasValue) return 10;
        if (ok.Value != 1) return 11;

        Colour? okEnum = Cast<Colour?>((object) Colour.Green);
        if (!okEnum.HasValue) return 12;
        if (okEnum.Value != Colour.Green) return 13;

        return 0;
    }
}

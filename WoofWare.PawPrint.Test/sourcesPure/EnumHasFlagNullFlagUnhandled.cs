// `Enum.HasFlag(null)` raises `ArgumentNullException` — the null check runs before the
// type-equivalence check, so this is a distinct arm from
// `EnumHasFlagMismatchUnhandled.cs`. Nothing catches it, so the guest process dies.
//
// As with the mismatch case, the point is the *unhandled* path: intrinsic dispatch
// must be able to say the exception found no handler.

using System;

public class Program
{
    private enum EnumA
    {
        X = 1,
    }

    private static bool HasFlagViaEnum(Enum value, Enum flag)
    {
        return value.HasFlag(flag);
    }

    public static int Main(string[] args)
    {
        return HasFlagViaEnum(EnumA.X, null) ? 1 : 2;
    }
}

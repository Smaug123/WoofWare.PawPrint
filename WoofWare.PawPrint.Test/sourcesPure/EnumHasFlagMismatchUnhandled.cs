// `Enum.HasFlag` with mismatched enum types raises `ArgumentException`. This is the same
// scenario as `BoxHasFlagTypeMismatch.cs`, but with nothing catching it, so the exception
// escapes `Main` and the guest process dies.
//
// That distinction is the point of the test: PawPrint's intrinsic dispatch had no way to
// report "this exception was unhandled", so it host-`failwith`ed and brought the
// interpreter down instead of reporting a dead guest.

using System;

public class Program
{
    private enum EnumA
    {
        X = 1,
    }

    private enum EnumB
    {
        Y = 1,
    }

    private static bool HasFlagViaEnum(Enum value, Enum flag)
    {
        return value.HasFlag(flag);
    }

    public static int Main(string[] args)
    {
        return HasFlagViaEnum(EnumA.X, EnumB.Y) ? 1 : 2;
    }
}

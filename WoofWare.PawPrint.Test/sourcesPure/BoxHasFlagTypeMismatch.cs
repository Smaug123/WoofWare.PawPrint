using System;

public class Program
{
    private enum EnumA { X = 1 }
    private enum EnumB { Y = 1 }

    private static bool HasFlagViaEnum(Enum value, Enum flag)
    {
        return value.HasFlag(flag);
    }

    public static int Main(string[] args)
    {
        // HasFlag with mismatched enum types must throw ArgumentException.
        // On a correct runtime: throws, we catch, return 0.
        // Bug: our intrinsic silently computes (1 & 1) == 1 → true, returns 1.
        try
        {
            HasFlagViaEnum(EnumA.X, EnumB.Y);
            return 1;
        }
        catch (ArgumentException)
        {
            // expected
        }

        return 0;
    }
}

using System;

// `calli` through a function pointer to a member of `Int128`, a type carrying a type-level
// `[Intrinsic]`. A `calli` enters its target through the target's entry point, so on real .NET it
// runs `Int128.IsNegative`'s IL, which is `(long)value._upper < 0`.
//
// The member is not called directly: PawPrint gates a direct call to an unreviewed member of an
// `[Intrinsic]` type, which is the asymmetry this file exists to pin.

public static unsafe class Program
{
    public static int Main(string[] args)
    {
        delegate*<Int128, bool> isNegative = &Int128.IsNegative;

        if (!isNegative(new Int128(ulong.MaxValue, ulong.MaxValue)))
        {
            return 1;
        }

        if (isNegative(new Int128(0, 1)))
        {
            return 2;
        }

        return 0;
    }
}

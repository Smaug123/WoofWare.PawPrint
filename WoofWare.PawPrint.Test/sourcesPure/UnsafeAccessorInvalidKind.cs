using System;
using System.Runtime.CompilerServices;

// `UnsafeAccessorAttribute`'s constructor takes an enum, and an enum-typed argument may hold any
// `int32`, so `[UnsafeAccessor((UnsafeAccessorKind)99)]` is legal C# and a shape the metadata
// really carries. CoreCLR parses the value, keeps it, and refuses at the switch that consumes it,
// so the refusal reaches the guest as a catchable `BadImageFormatException` on the accessor's first
// invocation -- rather than stopping the assembly from loading, which is what an *absent* attribute
// on an RVA-less method would mean.
//
// Measured on real .NET 10, including the HResult: `COR_E_BADIMAGEFORMAT`.
public class TestUnsafeAccessorInvalidKind
{
    private class Target
    {
        private int _f = 3;
    }

    [UnsafeAccessor((UnsafeAccessorKind) 99, Name = "_f")]
    private static extern ref int NoSuchKind(Target t);

    [UnsafeAccessor((UnsafeAccessorKind) (-1), Name = "_f")]
    private static extern ref int NegativeKind(Target t);

    // The neighbouring valid accessor still binds, so the invalid one is refused on its own account
    // rather than by anything the assembly-wide read decided.
    [UnsafeAccessor(UnsafeAccessorKind.Field, Name = "_f")]
    private static extern ref int Valid(Target t);

    private static int Run()
    {
        Target t = new Target();

        if (Valid(t) != 3) return 1;

        try
        {
            NoSuchKind(t);
            return 2;
        }
        catch (BadImageFormatException e)
        {
            if (e.HResult != unchecked((int) 0x8007000B)) return 3;
        }

        try
        {
            NegativeKind(t);
            return 4;
        }
        catch (BadImageFormatException) { }

        // The refusal recurs rather than being latched.
        try
        {
            NoSuchKind(t);
            return 5;
        }
        catch (BadImageFormatException) { }

        if (Valid(t) != 3) return 6;

        return 0;
    }

    public static int Main() => Run();
}

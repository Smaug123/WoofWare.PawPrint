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

    private class Instance
    {
        // Not static, which CoreCLR refuses before it reads the kind -- and before it resolves any
        // `[UnsafeAccessorType]` name, which is why the name here can be of a type that does not
        // exist: were the name resolved first, this would be a `TypeLoadException`.
        [UnsafeAccessor((UnsafeAccessorKind) 99, Name = "_f")]
        internal extern ref int NotStaticNamingAType([UnsafeAccessorType("NoSuchType")] object t);

        [UnsafeAccessor((UnsafeAccessorKind) 99, Name = "_f")]
        internal extern ref int NotStatic(Target t);
    }

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

        try
        {
            new Instance().NotStatic(t);
            return 7;
        }
        catch (BadImageFormatException) { }

        try
        {
            new Instance().NotStaticNamingAType(t);
            return 8;
        }
        catch (BadImageFormatException) { }

        return 0;
    }

    public static int Main() => Run();
}

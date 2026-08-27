using System;
using System.Runtime.CompilerServices;

// `UnsafeAccessorAttribute.Name` distinguishes "not supplied" from "supplied as null", and only the
// first takes the attribute's documented default of the accessor's own name. CoreCLR keys that off
// the named argument's *presence* (`SERIALIZATION_TYPE_UNDEFINED` in
// `TryParseUnsafeAccessorAttribute`); a supplied value is copied verbatim, and copying a null
// yields the empty string, which no type declares a member called.
//
// Measured on real .NET 10: `Name = null` and `Name = ""` both raise, and only the absent form
// binds the same-named member.
public class TestUnsafeAccessorNameIsExplicitlyNull
{
    private class Target
    {
        private int Same() => 11;

        private int Field;

        private Target()
        {
            Field = 3;
        }
    }

    [UnsafeAccessor(UnsafeAccessorKind.Method)]
    private static extern int Same(Target t);

    [UnsafeAccessor(UnsafeAccessorKind.Method, Name = null)]
    private static extern int SameNullName(Target t);

    [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "")]
    private static extern int SameEmptyName(Target t);

    [UnsafeAccessor(UnsafeAccessorKind.Field, Name = null)]
    private static extern ref int FieldNullName(Target t);

    // A constructor's name is the runtime's to choose, so an *empty* one -- supplied or absent --
    // is accepted where a non-empty one is refused.
    [UnsafeAccessor(UnsafeAccessorKind.Constructor, Name = null)]
    private static extern Target NewNullName();

    [UnsafeAccessor(UnsafeAccessorKind.Constructor, Name = "")]
    private static extern Target NewEmptyName();

    private static int Check<TExpected>(int code, Action a)
        where TExpected : Exception
    {
        try
        {
            a();
            return code;
        }
        catch (TExpected)
        {
            return 0;
        }
    }

    private static int Run()
    {
        Target t = NewNullName();
        if (t == null) return 1;
        if (NewEmptyName() == null) return 2;

        // Absent: the accessor's own name is the target's.
        if (Same(t) != 11) return 3;

        int r;

        r = Check<MissingMethodException>(4, () => SameNullName(t));
        if (r != 0) return r;

        r = Check<MissingMethodException>(5, () => SameEmptyName(t));
        if (r != 0) return r;

        r = Check<MissingFieldException>(6, () => FieldNullName(t));
        if (r != 0) return r;

        return 0;
    }

    public static int Main() => Run();
}

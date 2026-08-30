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

    // The null-named and empty-named accessors are *also* called `Same`, which is the whole point:
    // if an explicit null read as "absent", they would fall back to their own name and bind
    // `Target.Same`. They live in nested classes because one class cannot declare three methods of
    // that name. The same trick gives the field accessor the name of a real field.
    private static class NullName
    {
        [UnsafeAccessor(UnsafeAccessorKind.Method, Name = null)]
        public static extern int Same(Target t);

        [UnsafeAccessor(UnsafeAccessorKind.Field, Name = null)]
        public static extern ref int Field(Target t);
    }

    private static class EmptyName
    {
        [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "")]
        public static extern int Same(Target t);
    }

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

        // Explicitly null: the accessor is called `Same` and `Target.Same` exists, so a reading
        // that treated null as absent would call it and return 11.
        r = Check<MissingMethodException>(4, () => NullName.Same(t));
        if (r != 0) return r;

        r = Check<MissingMethodException>(5, () => EmptyName.Same(t));
        if (r != 0) return r;

        // Likewise the field: `Target.Field` exists and the accessor is called `Field`.
        r = Check<MissingFieldException>(6, () => NullName.Field(t));
        if (r != 0) return r;

        return 0;
    }

    public static int Main() => Run();
}

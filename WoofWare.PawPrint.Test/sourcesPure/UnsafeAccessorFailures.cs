using System;
using System.Runtime.CompilerServices;

// The failures `MethodDesc::TryGenerateUnsafeAccessor` raises, all of which real .NET raises from
// the accessor's *first invocation* -- as it JITs the synthesised stub -- rather than at load, so
// the guest's own `try`/`catch` sees them. CoreLib's own accessors are written expecting exactly
// that: they name members of assemblies that may not be present.
//
// The exception types are asserted; the messages are not, being localisable.
public class TestUnsafeAccessorFailures
{
    private class Target
    {
        private int _field;
        private int Instance(int x) => _field + x;
        private static int Static(int x) => x;
    }

    private struct Value
    {
        private int _field;
        private int Instance(int x) => _field + x;
    }

    [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "NoSuchMethod")]
    private static extern int MissingMethod(Target t, int x);

    [UnsafeAccessor(UnsafeAccessorKind.Field, Name = "_noSuchField")]
    private static extern ref int MissingField(Target t);

    // The field exists, but with a different type: the declaration's `ref` return is compared
    // against the field's own signature, so this is a lookup failure rather than a cast.
    [UnsafeAccessor(UnsafeAccessorKind.Field, Name = "_field")]
    private static extern ref long WrongFieldType(Target t);

    // The method exists, but with a different parameter list.
    [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "Instance")]
    private static extern int WrongSignature(Target t, string x);

    // The method exists and is an instance method; the accessor asks for a static one.
    [UnsafeAccessor(UnsafeAccessorKind.StaticMethod, Name = "Instance")]
    private static extern int WantedStatic(Target t, int x);

    // ... and the other way round.
    [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "Static")]
    private static extern int WantedInstance(Target t, int x);

    // The runtime picks a constructor's name, so the attribute must not supply one.
    [UnsafeAccessor(UnsafeAccessorKind.Constructor, Name = "nope")]
    private static extern Target ConstructorWithName();

    // A field accessor must return a byref, or there would be nothing to write through.
    [UnsafeAccessor(UnsafeAccessorKind.Field, Name = "_field")]
    private static extern int FieldReturnNotByRef(Target t);

    // An instance member of a value type must be reached by ref, not by value.
    [UnsafeAccessor(UnsafeAccessorKind.Field, Name = "_field")]
    private static extern ref int ValueTypeFieldByValue(Value v);

    [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "Instance")]
    private static extern int ValueTypeMethodByValue(Value v, int x);

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

    public static int Main()
    {
        int r;

        r = Check<MissingMethodException>(1, () => MissingMethod(null, 1));
        if (r != 0) return r;

        r = Check<MissingFieldException>(2, () => MissingField(null));
        if (r != 0) return r;

        r = Check<MissingFieldException>(3, () => WrongFieldType(null));
        if (r != 0) return r;

        r = Check<MissingMethodException>(4, () => WrongSignature(null, "x"));
        if (r != 0) return r;

        r = Check<MissingMethodException>(5, () => WantedStatic(null, 1));
        if (r != 0) return r;

        r = Check<MissingMethodException>(6, () => WantedInstance(null, 1));
        if (r != 0) return r;

        r = Check<BadImageFormatException>(7, () => ConstructorWithName());
        if (r != 0) return r;

        r = Check<BadImageFormatException>(8, () => FieldReturnNotByRef(null));
        if (r != 0) return r;

        r = Check<BadImageFormatException>(9, () => ValueTypeFieldByValue(default));
        if (r != 0) return r;

        r = Check<BadImageFormatException>(10, () => ValueTypeMethodByValue(default, 1));
        if (r != 0) return r;

        // The failure recurs on every call rather than being latched as a success.
        r = Check<MissingMethodException>(11, () => MissingMethod(null, 1));
        if (r != 0) return r;

        return 0;
    }
}

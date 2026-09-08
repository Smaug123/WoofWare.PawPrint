using System.Runtime.CompilerServices;

// The five `UnsafeAccessorKind`s against a private target: what CoreCLR's `GenerateAccessor`
// (vm/unsafeaccessors.cpp) synthesises as `newobj`, `callvirt`, `call`, `ldflda` and `ldsflda`
// respectively. Each accessor is exercised for reading and, where it yields a `ref`, for writing
// through it -- a byref that addressed a copy would read back correctly and still lose the write.
//
// `NameDefaulted` carries no `Name` on its attribute, which the attribute documents as "use the
// accessor's own name"; it is the only member here reached that way.
public class TestUnsafeAccessorAllKinds
{
    private class Target
    {
        private int _field;
        private static int _staticField = 11;

        private Target(int seed)
        {
            _field = seed;
        }

        private Target()
        {
            _field = -99;
        }

        private int Instance(int x) => _field + x;

        private static int Static(int x) => _staticField + x;

        private int NameDefaulted(int x) => _field * x;
    }

    [UnsafeAccessor(UnsafeAccessorKind.Constructor)]
    private static extern Target NewTarget(int seed);

    // A constructor accessor taking no arguments at all: the overload set is what makes the
    // signature comparison pick between the two, rather than the name alone.
    [UnsafeAccessor(UnsafeAccessorKind.Constructor)]
    private static extern Target NewDefaultTarget();

    [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "Instance")]
    private static extern int CallInstance(Target t, int x);

    [UnsafeAccessor(UnsafeAccessorKind.StaticMethod, Name = "Static")]
    private static extern int CallStatic(Target t, int x);

    [UnsafeAccessor(UnsafeAccessorKind.Method)]
    private static extern int NameDefaulted(Target t, int x);

    [UnsafeAccessor(UnsafeAccessorKind.Field, Name = "_field")]
    private static extern ref int Field(Target t);

    [UnsafeAccessor(UnsafeAccessorKind.StaticField, Name = "_staticField")]
    private static extern ref int StaticField(Target t);

    private static int Run()
    {
        Target t = NewTarget(7);
        if (t == null) return 1;

        // Constructor: the argument reached the private .ctor.
        if (Field(t) != 7) return 2;

        // Instance method, through the receiver the first argument supplies.
        if (CallInstance(t, 1) != 8) return 3;

        // Static method: the first argument names the type and is not passed on.
        if (CallStatic(t, 1) != 12) return 4;
        if (CallStatic(null, 1) != 12) return 5;

        // The attribute's Name is absent, so the accessor's own name is the target's.
        if (NameDefaulted(t, 3) != 21) return 6;

        // Writing through the instance-field byref is visible to the target's own code.
        Field(t) = 100;
        if (CallInstance(t, 0) != 100) return 7;

        // ... and likewise for the static-field byref.
        if (StaticField(null) != 11) return 8;
        StaticField(null) = 200;
        if (CallStatic(null, 0) != 200) return 9;

        // The zero-argument overload of the same private constructor.
        Target defaulted = NewDefaultTarget();
        if (Field(defaulted) != -99) return 12;

        // A second instance is independent of the first.
        Target other = NewTarget(-4);
        if (Field(other) != -4) return 10;
        if (Field(t) != 100) return 11;

        return 0;
    }

    // `Main` only delegates. An accessor that pushed the wrong number of arguments would leave the
    // extra one on its *caller's* evaluation stack, and the entry frame is never checked for a
    // clean stack on return -- it has nowhere to return to -- so the leak would go unnoticed if the
    // accessors were called from `Main` itself.
    public static int Main() => Run();
}

using System.Runtime.CompilerServices;

// The body CoreCLR synthesises for `UnsafeAccessorKind.StaticField` is an `ldsflda`
// (vm/unsafeaccessors.cpp, `GenerateAccessor`), so reaching a static field through an accessor
// initialises the declaring type exactly as the opcode does -- the accessor is not a way around
// the initialiser. The class-initialisation order is observable here because the `.cctor` records
// that it ran.
public class TestUnsafeAccessorStaticFieldRunsCctor
{
    private static int _cctorRuns;

    private class Lazily
    {
        private static int _value;

        static Lazily()
        {
            _cctorRuns++;
            _value = 42;
        }
    }

    private class AlsoLazily
    {
        private static int _value;

        static AlsoLazily()
        {
            _cctorRuns++;
            _value = 7;
        }
    }

    [UnsafeAccessor(UnsafeAccessorKind.StaticField, Name = "_value")]
    private static extern ref int Value(Lazily l);

    [UnsafeAccessor(UnsafeAccessorKind.StaticField, Name = "_value")]
    private static extern ref int AlsoValue(AlsoLazily l);

    public static int Main()
    {
        // Nothing has touched either type yet.
        if (_cctorRuns != 0) return 1;

        // Reading through the accessor is the first use of `Lazily`, so its initialiser runs
        // and the read sees what the initialiser wrote rather than a zero.
        if (Value(null) != 42) return 2;
        if (_cctorRuns != 1) return 3;

        // A second read does not run it again.
        if (Value(null) != 42) return 4;
        if (_cctorRuns != 1) return 5;

        // A write through the byref is not undone by anything.
        Value(null) = 43;
        if (Value(null) != 43) return 6;
        if (_cctorRuns != 1) return 7;

        // The unrelated type is still untouched until its own accessor is used.
        if (AlsoValue(null) != 7) return 8;
        if (_cctorRuns != 2) return 9;

        return 0;
    }
}

using System;
using System.Runtime.CompilerServices;

// A primitive is a value type with one private field holding its value -- `System.Int32.m_value` --
// and an accessor may name it. Real .NET returns a reference to the underlying value itself, so the
// accessor is an alias for the very `int` it was handed: measured on .NET 10, writing 9 through the
// returned `ref int` is visible in the original local.
//
// A value type with a single field of its own is the near miss beside it, and works: its storage is
// a field map like any other struct's, so the field's address is an ordinary projection.
public class TestUnsafeAccessorPrimitiveBackingField
{
    private struct OneField
    {
        public long Only;
    }

    [UnsafeAccessor(UnsafeAccessorKind.Field, Name = "m_value")]
    private static extern ref int IntValue(ref int value);

    [UnsafeAccessor(UnsafeAccessorKind.Field, Name = "Only")]
    private static extern ref long TheOnlyField(ref OneField f);

    private static int Run()
    {
        // The ordinary single-field struct.
        OneField f = default;
        TheOnlyField(ref f) = 11;
        if (f.Only != 11) return 1;

        int i = 5;

        if (IntValue(ref i) != 5) return 2;

        // The reference aliases the original, so writing through it is visible in the local.
        ref int r = ref IntValue(ref i);
        r = 9;
        if (i != 9) return 3;
        if (r != 9) return 4;

        // ... and a second call sees the write.
        if (IntValue(ref i) != 9) return 5;

        return 0;
    }

    public static int Main() => Run();
}

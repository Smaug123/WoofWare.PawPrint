using System;
using System.Runtime.CompilerServices;

// A primitive is a value type with one private field holding its value -- `System.Int32.m_value` --
// and an accessor may name it. Real .NET returns a reference to the underlying value itself, so the
// accessor is an alias for the very `int` it was handed: measured on .NET 10, writing 9 through the
// returned `ref int` is visible in the original local, and the returned reference is the same
// address as the one passed in.
//
// A value type with a single field of its own is the near miss beside it: its storage is a field
// map like any other struct's, so the field's address is an ordinary projection. `nint`'s `_value`
// is the same near miss from inside CoreLib.
//
// The primitive reaches the accessor from several kinds of storage -- a local, an array element, a
// struct's field, a class's field, a static and stack memory -- and as several primitives, since the
// address is the container's whatever holds it and whatever its width.
public class TestUnsafeAccessorPrimitiveBackingField
{
    private struct OneField
    {
        public long Only;
    }

    private struct HoldsInt
    {
        public long Before;
        public int Value;
    }

    private sealed class ClassHoldsInt
    {
        public int Value;
    }

    private static int s_static;

    [UnsafeAccessor(UnsafeAccessorKind.Field, Name = "m_value")]
    private static extern ref int IntValue(ref int value);

    [UnsafeAccessor(UnsafeAccessorKind.Field, Name = "m_value")]
    private static extern ref bool BoolValue(ref bool value);

    [UnsafeAccessor(UnsafeAccessorKind.Field, Name = "m_value")]
    private static extern ref char CharValue(ref char value);

    [UnsafeAccessor(UnsafeAccessorKind.Field, Name = "m_value")]
    private static extern ref byte ByteValue(ref byte value);

    [UnsafeAccessor(UnsafeAccessorKind.Field, Name = "m_value")]
    private static extern ref long LongValue(ref long value);

    [UnsafeAccessor(UnsafeAccessorKind.Field, Name = "m_value")]
    private static extern ref double DoubleValue(ref double value);

    [UnsafeAccessor(UnsafeAccessorKind.Field, Name = "_value")]
    private static extern ref nint NativeIntValue(ref nint value);

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

        // It is the same address, not merely a reference that happens to read the same value.
        if (!Unsafe.AreSame(ref IntValue(ref i), ref i)) return 6;
        if (!Unsafe.AreSame(ref IntValue(ref IntValue(ref i)), ref i)) return 7;

        // An array element.
        int[] array = { 1, 2, 3 };
        IntValue(ref array[1]) = 20;
        if (array[0] != 1 || array[1] != 20 || array[2] != 3) return 8;
        if (!Unsafe.AreSame(ref IntValue(ref array[2]), ref array[2])) return 9;

        // A field of a struct.
        HoldsInt h = default;
        h.Before = 7;
        IntValue(ref h.Value) = 30;
        if (h.Value != 30 || h.Before != 7) return 10;

        // A field of a class.
        var c = new ClassHoldsInt();
        IntValue(ref c.Value) = 40;
        if (c.Value != 40) return 11;
        if (!Unsafe.AreSame(ref IntValue(ref c.Value), ref c.Value)) return 12;

        // A static.
        IntValue(ref s_static) = 50;
        if (s_static != 50) return 13;

        // Stack memory, whose storage is bytes rather than typed cells.
        Span<int> stack = stackalloc int[2];
        IntValue(ref stack[1]) = 60;
        if (stack[0] != 0 || stack[1] != 60) return 23;
        if (!Unsafe.AreSame(ref IntValue(ref stack[1]), ref stack[1])) return 24;

        // Other primitives, of other widths.
        bool b = false;
        BoolValue(ref b) = true;
        if (!b) return 14;

        char ch = 'a';
        if (CharValue(ref ch) != 'a') return 15;
        CharValue(ref ch) = 'z';
        if (ch != 'z') return 16;

        byte by = 1;
        ByteValue(ref by) = 255;
        if (by != 255) return 17;

        long l = 1L << 40;
        if (LongValue(ref l) != 1L << 40) return 18;
        LongValue(ref l) = -3L;
        if (l != -3L) return 19;

        double d = 1.5;
        DoubleValue(ref d) = 2.25;
        if (d != 2.25) return 20;
        if (!Unsafe.AreSame(ref DoubleValue(ref d), ref d)) return 21;

        nint n = 12;
        NativeIntValue(ref n) = 34;
        if (n != 34) return 22;

        return 0;
    }

    public static int Main() => Run();
}

using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

// Auto layout buckets a field by its *normalised* element type, so a real enum counts as its
// underlying integer and takes a place among the primitives, while an ordinary struct is a value
// class and is placed after every bucket. PawPrint decides which of those a value type is with
// `CliValueType.IsEnumStructural`, a structural test: exactly one instance field, named
// `value__`, at offset 0, of integral type. `value__` is a legal C# identifier, so an ordinary
// struct can match it without being an enum, and is then bucketed as a primitive.
//
// The misclassification predates the layout-kind work — it also drives eval-stack flattening, and
// a GC-containing struct already reached it by the promotion route — but honouring a declared
// `LayoutKind.Auto` gives it a second, reference-free way in, which is what this file pins.
// Closing it means deciding enum-ness nominally (base type is `System.Enum`), which needs
// assembly lookup at a construction site that has none.
public class TestAutoLayoutStructNamedLikeEnum
{
    // Not an enum, whatever its field is called.
    [StructLayout(LayoutKind.Sequential)] private struct Fake { public int value__; }

    [StructLayout(LayoutKind.Auto)] private struct AutoFakeHolder { public byte B; public Fake F; }

    // The control: a real enum, which really is bucketed as its underlying `int` and so really
    // does take the front.
    [StructLayout(LayoutKind.Auto)] private struct AutoEnumHolder { public byte B; public DayOfWeek E; }

    private static byte ByteAt<T>(ref T whole, int index) where T : struct
        => Unsafe.Add(ref Unsafe.As<T, byte>(ref whole), index);

    public static int Main(string[] argv)
    {
        if (Unsafe.SizeOf<AutoFakeHolder>() != 8) return 1;
        if (Unsafe.SizeOf<AutoEnumHolder>() != 8) return 2;

        // A real enum is a primitive to the layout algorithm: it takes the 4-byte bucket at the
        // front and the loose byte follows it. PawPrint agrees about this one.
        AutoEnumHolder e = default;
        e.B = 0x33;
        e.E = (DayOfWeek) 0x44;
        if (ByteAt(ref e, 0) != 0x44) return 3;
        if (ByteAt(ref e, 4) != 0x33) return 4;

        // `Fake` is a value class, so it is placed *after* the byte bucket, not in front of it.
        // PawPrint buckets it as a primitive and answers the other way round.
        AutoFakeHolder h = default;
        h.B = 0x11;
        h.F.value__ = 0x22;
        if (ByteAt(ref h, 0) != 0x11) return 5;
        if (ByteAt(ref h, 4) != 0x22) return 6;

        return 0;
    }
}

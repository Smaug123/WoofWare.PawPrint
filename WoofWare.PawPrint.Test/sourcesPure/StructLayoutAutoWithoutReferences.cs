using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

// CoreCLR reaches `HandleAutoLayout` by two routes (`MethodTableBuilder::PlaceInstanceFields`,
// methodtablebuilder.cpp:8212): the type declares `LayoutKind.Auto`, or the type contains GC
// references and is therefore promoted to auto layout whatever it declared. This exercises the
// first route, which needs the declared kind to survive as far as the layout builder: it lives in
// `TypeAttributes.LayoutMask` and is projected by `TypeLayoutKind.ofTypeAttributes`.
//
// Three rules are visible here, and each has a sequential control beside it:
//   * fields are bucketed by size class, largest first, so the two bytes below become adjacent;
//   * a value class no larger than a pointer is rounded up to the next power of two;
//   * `Pack` and `Size` are both ignored (auto layout never reads the ClassLayout row).
public class TestStructLayoutAutoWithoutReferences
{
    [StructLayout(LayoutKind.Auto)] private struct AutoBucketed { public byte B; public int I; public byte C; }
    [StructLayout(LayoutKind.Auto)] private struct AutoThreeBytes { public byte A; public byte B; public byte C; }
    [StructLayout(LayoutKind.Auto)] private struct AutoThreeShorts { public short A; public short B; public short C; }

    // Past the pointer size with no reference in it, so the size rounds up to
    // `largestAlignmentRequirement` — which is the pointer size because a plain `int` field is not
    // a value class and "non-value-type fields always require pointer alignment"
    // (methodtablebuilder.cpp:8554). Twelve bytes of ints therefore occupy sixteen.
    [StructLayout(LayoutKind.Auto)] private struct AutoThreeInts { public int A; public int B; public int C; }

    // The counterexample to "anything past a pointer rounds up to a pointer": every field here is
    // a value class of alignment 1, so `largestAlignmentRequirement` stays 1 and nine bytes stay
    // nine bytes.
    [StructLayout(LayoutKind.Sequential)] private struct ThreeBytesSeq { public byte A; public byte B; public byte C; }
    [StructLayout(LayoutKind.Auto)] private struct AutoNarrowRun { public ThreeBytesSeq X; public ThreeBytesSeq Y; public ThreeBytesSeq Z; }

    [StructLayout(LayoutKind.Auto, Pack = 1, Size = 64)] private struct AutoPackAndSize { public byte A; public long L; }
    [StructLayout(LayoutKind.Auto, Size = 64)] private struct AutoEmptyWithSize { }
    [StructLayout(LayoutKind.Auto)] private struct AutoNested { public AutoThreeBytes N; public byte B; }

    // The controls: declared sequential, so the declared order and size stand. PawPrint already
    // agreed on all of these before the layout kind was modelled, which is what makes the cases
    // above a LayoutKind gap rather than a bucketing one.
    [StructLayout(LayoutKind.Sequential)] private struct SeqBucketed { public byte B; public int I; public byte C; }
    [StructLayout(LayoutKind.Sequential)] private struct SeqThreeBytes { public byte A; public byte B; public byte C; }
    [StructLayout(LayoutKind.Sequential)] private struct SeqThreeInts { public int A; public int B; public int C; }
    [StructLayout(LayoutKind.Sequential, Pack = 1)] private struct SeqPacked { public byte A; public long L; }
    [StructLayout(LayoutKind.Sequential, Size = 64)] private struct SeqEmptyWithSize { }

    private static int Check<T>(int expected, int code) where T : struct
        => Unsafe.SizeOf<T>() == expected ? 0 : code;

    // Offsets, not just sizes: two layouts of the same total size can still place their fields
    // differently, and it is the placement that a byref into a field observes. Read through a
    // reinterpreted byte view of the whole value rather than with `Unsafe.ByteOffset` on a byref
    // to the field, because that shape is not yet implemented. Every field whose position is
    // asserted here is a byte, so no endianness assumption is needed.
    private static byte ByteAt<T>(ref T whole, int index) where T : struct
        => Unsafe.Add(ref Unsafe.As<T, byte>(ref whole), index);

    public static int Main(string[] argv)
    {
        int r;
        if ((r = Check<SeqBucketed>(12, 1)) != 0) return r;
        if ((r = Check<SeqThreeBytes>(3, 2)) != 0) return r;
        if ((r = Check<SeqThreeInts>(12, 3)) != 0) return r;
        if ((r = Check<SeqPacked>(9, 4)) != 0) return r;
        if ((r = Check<SeqEmptyWithSize>(64, 5)) != 0) return r;

        if ((r = Check<AutoBucketed>(8, 6)) != 0) return r;
        if ((r = Check<AutoThreeBytes>(4, 7)) != 0) return r;
        if ((r = Check<AutoThreeShorts>(8, 8)) != 0) return r;
        if ((r = Check<AutoThreeInts>(16, 9)) != 0) return r;
        if ((r = Check<AutoNarrowRun>(9, 10)) != 0) return r;
        if ((r = Check<AutoPackAndSize>(16, 11)) != 0) return r;
        if ((r = Check<AutoEmptyWithSize>(1, 12)) != 0) return r;
        if ((r = Check<AutoNested>(8, 13)) != 0) return r;

        // The four-byte `I` takes the front, so the two bytes become adjacent behind it.
        AutoBucketed ab = default;
        ab.B = 0x11; ab.C = 0x22;
        if (ByteAt(ref ab, 4) != 0x11) return 14;
        if (ByteAt(ref ab, 5) != 0x22) return 15;

        // The same fields declared sequential keep declared order, so the bytes straddle `I`.
        SeqBucketed sb = default;
        sb.B = 0x11; sb.C = 0x22;
        if (ByteAt(ref sb, 0) != 0x11) return 16;
        if (ByteAt(ref sb, 8) != 0x22) return 17;

        // A value class is placed after every size-class bucket, so the loose byte precedes the
        // nested struct even though it is declared second.
        AutoNested an = default;
        an.B = 0x33; an.N.A = 0x44;
        if (ByteAt(ref an, 0) != 0x33) return 18;
        if (ByteAt(ref an, 4) != 0x44) return 19;

        // `Pack` is ignored, so the long is 8-aligned and takes the pointer-sized bucket first.
        AutoPackAndSize ap = default;
        ap.A = 0x55;
        if (ByteAt(ref ap, 8) != 0x55) return 20;

        // Three 1-aligned value classes pack end to end: nothing here demands wider alignment.
        AutoNarrowRun nr = default;
        nr.X.A = 0x66; nr.Y.A = 0x77; nr.Z.A = 0x88;
        if (ByteAt(ref nr, 0) != 0x66) return 21;
        if (ByteAt(ref nr, 3) != 0x77) return 22;
        if (ByteAt(ref nr, 6) != 0x88) return 23;

        return 0;
    }
}

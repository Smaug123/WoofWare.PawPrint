using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

// CoreCLR lays an `[InlineArray(N)]` type out as its *one* declared field, sizes that single
// element completely -- including the rounding at the end of the layout algorithm -- and only then
// multiplies by N (`MethodTableBuilder::PlaceInstanceFields`, methodtablebuilder.cpp:8612 for the
// auto route, `HandleSequentialLayout` :8663 for the sequential one). So the run is N copies of the
// *rounded* element, and "round once at the end" is a different answer whenever an element's own
// size is not already a multiple of its own alignment.
//
// Two shapes reach that difference, and they are not the same shape:
//
//   * the auto route rounds a 3-byte element up to 4, so three of them are 12 bytes and not 9;
//   * the sequential route rounds a 5-byte element up to 8 -- an element gets a size that is not a
//     multiple of its alignment from a declared `Size` floor, which suppresses the alignment
//     rounding it would otherwise have had (`CalculateSizeWithMetadataSize`, classlayoutinfo.cpp:
//     326-341) -- so three of them are 24 bytes and not 15.
//
// The *alignment* such a type reports is a third fact, and it is not the element's. CoreCLR records
// a custom field alignment only when the element's rounded alignment differs from
// `min(elementSize, sizeof(void*))` (methodtablebuilder.cpp:8598); when it does not,
// `MethodTable::GetFieldAlignmentRequirement` (methodtable.cpp:8853) falls through to
// `min(GetNumInstanceFieldBytes(), sizeof(void*))` -- reading the size *after* the multiplication.
// So `[Auto, InlineArray(3)] struct { int }` is 4-aligned as one element and 8-aligned as a run of
// three, which moves it inside a containing struct. The `Holder*` sizes below are what pin that:
// `sizeof(struct { byte; T; })` is the cheapest single number that reveals `T`'s alignment.
//
// Only the declared-`Auto` route needs `[StructLayout(LayoutKind.Auto)]` spelled out: a C# struct
// with no attribute is `Sequential` in metadata.
public class TestInlineArrayElementSizeRounding
{
    [StructLayout(LayoutKind.Sequential)] private struct S3 { public byte A; public byte B; public byte C; }

    // Five bytes with an alignment of four: the declared `Size` is a floor, and a floor and the
    // alignment rounding are alternatives rather than a sequence.
    [StructLayout(LayoutKind.Sequential, Size = 5)] private struct Q5 { public int I; }

    [StructLayout(LayoutKind.Auto)][InlineArray(3)] private struct AutoNarrow { private S3 _item; }
    [StructLayout(LayoutKind.Sequential)][InlineArray(3)] private struct SeqNarrow { private S3 _item; }
    [StructLayout(LayoutKind.Auto)][InlineArray(3)] private struct AutoInt { private int _item; }
    [StructLayout(LayoutKind.Auto)][InlineArray(3)] private struct AutoFloor { private Q5 _item; }
    [StructLayout(LayoutKind.Sequential)][InlineArray(3)] private struct SeqFloor { private Q5 _item; }

    // `Pack` applies to the single element's layout, so it changes the stride as well as the
    // element's own alignment: five bytes stay five, and three of them are 15.
    [StructLayout(LayoutKind.Sequential, Pack = 1)][InlineArray(3)] private struct PackedFloor { private Q5 _item; }

    // A single-element inline array mints no extra storage slot, so PawPrint's field list is the
    // declared field alone and the ordinary one-element path already gives the right answer. Here
    // to pin that the multiplication does not disturb it.
    [StructLayout(LayoutKind.Auto)][InlineArray(1)] private struct AutoOne { private S3 _item; }

    private struct HolderAutoNarrow { public byte Lead; public AutoNarrow Buf; }
    private struct HolderSeqNarrow { public byte Lead; public SeqNarrow Buf; }
    private struct HolderAutoInt { public byte Lead; public AutoInt Buf; }
    private struct HolderAutoFloor { public byte Lead; public AutoFloor Buf; }
    private struct HolderSeqFloor { public byte Lead; public SeqFloor Buf; }
    private struct HolderPackedFloor { public byte Lead; public PackedFloor Buf; }
    private struct HolderAutoOne { public byte Lead; public AutoOne Buf; }

    private static int Check<T>(int expected, int code) where T : struct
        => Unsafe.SizeOf<T>() == expected ? 0 : code;

    private static int CheckStride<T>(ref T first, ref T second, int expected, int code) where T : struct
        => (int)Unsafe.ByteOffset(ref Unsafe.As<T, byte>(ref first), ref Unsafe.As<T, byte>(ref second)) == expected
            ? 0
            : code;

    // Where the elements sit, which no size check can see. The multiplication decides the *size*
    // and nothing else: CoreCLR keeps one `FieldDesc` at offset 0, and C# lowers `buf[i]` to
    // `InlineArrayElementRef` -> `Unsafe.Add(ref Unsafe.As<TBuffer, TElement>(ref buf), i)`, which
    // strides by `sizeof(TElement)` and never consults the aggregate. So a rounded element leaves
    // slack *after* the last element rather than padding between them: `AutoNarrow` is 12 bytes
    // with its three elements at 0, 3 and 6.
    private static int Strides()
    {
        int r;

        AutoNarrow autoNarrow = default;
        if ((r = CheckStride(ref autoNarrow[0], ref autoNarrow[1], 3, 17)) != 0) return r;
        if ((r = CheckStride(ref autoNarrow[0], ref autoNarrow[2], 6, 18)) != 0) return r;

        SeqNarrow seqNarrow = default;
        if ((r = CheckStride(ref seqNarrow[0], ref seqNarrow[2], 6, 19)) != 0) return r;

        // The sequential route's element rounds 5 -> 8 for sizing, but the elements are still five
        // bytes apart.
        SeqFloor seqFloor = default;
        if ((r = CheckStride(ref seqFloor[0], ref seqFloor[1], 5, 20)) != 0) return r;
        if ((r = CheckStride(ref seqFloor[0], ref seqFloor[2], 10, 21)) != 0) return r;

        AutoFloor autoFloor = default;
        if ((r = CheckStride(ref autoFloor[0], ref autoFloor[2], 10, 22)) != 0) return r;

        AutoInt autoInt = default;
        if ((r = CheckStride(ref autoInt[0], ref autoInt[2], 8, 23)) != 0) return r;

        // Distinct elements really are distinct storage, so a stride that collapsed two of them
        // together would fail here as well as above.
        AutoNarrow written = default;
        written[0].A = 11;
        written[1].A = 22;
        written[2].A = 33;
        if (written[0].A != 11) return 24;
        if (written[1].A != 22) return 25;
        if (written[2].A != 33) return 26;

        return 0;
    }

    public static int Main(string[] argv)
    {
        int r;

        // The elements themselves, so a failure below is attributable to the inline array rather
        // than to the element type's own layout.
        if ((r = Check<S3>(3, 1)) != 0) return r;
        if ((r = Check<Q5>(5, 2)) != 0) return r;

        // Auto route: the element rounds 3 -> 4 before the multiplication.
        if ((r = Check<AutoNarrow>(12, 3)) != 0) return r;
        if ((r = Check<HolderAutoNarrow>(16, 4)) != 0) return r;

        // Sequential route over the same element, which needs no rounding: 3 * 3.
        if ((r = Check<SeqNarrow>(9, 5)) != 0) return r;
        if ((r = Check<HolderSeqNarrow>(10, 6)) != 0) return r;

        // An element that is already its own rounded size. The size is what "lay N slots out
        // together" gives too; the alignment is not, because the run is past the pointer size and
        // the element is not.
        if ((r = Check<AutoInt>(12, 7)) != 0) return r;
        if ((r = Check<HolderAutoInt>(24, 8)) != 0) return r;

        // Auto route over the declared-`Size` element: 5 rounds up to 8, so 24 rather than 15.
        if ((r = Check<AutoFloor>(24, 9)) != 0) return r;
        if ((r = Check<HolderAutoFloor>(32, 10)) != 0) return r;

        // The sequential route rounds the element too: also 24, not the 16 that rounding the whole
        // 15-byte run to the element's 4-byte alignment would give.
        if ((r = Check<SeqFloor>(24, 11)) != 0) return r;
        if ((r = Check<HolderSeqFloor>(28, 12)) != 0) return r;

        // `Pack = 1` drops the element's alignment to 1, so there is nothing to round to and the
        // run really is 15 bytes.
        if ((r = Check<PackedFloor>(15, 13)) != 0) return r;
        if ((r = Check<HolderPackedFloor>(16, 14)) != 0) return r;

        // N = 1: the element's own rounding still applies.
        if ((r = Check<AutoOne>(4, 15)) != 0) return r;
        if ((r = Check<HolderAutoOne>(8, 16)) != 0) return r;

        return Strides();
    }
}

using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

// `Array.Copy` over an array whose elements contain GC pointers takes the write-barrier half of
// `Array.Copy`'s branch (`pMT->ContainsGCPointers`), which lands in
// `Buffer.BulkMoveWithWriteBarrier`. Above 16384 bytes that method does *not* hand the whole move
// to the runtime: `BulkMoveWithWriteBarrierBatch` splits it into 16384-byte chunks, and 16384 is
// not a multiple of a 24-byte element. So the second chunk begins **partway through an element**,
// which is the only way a bulk move ever exposes the interior of a struct.
//
// That matters to PawPrint specifically. It stores values as typed cells rather than flat bytes, so
// `CellAwareMemOps.copy` walks the range taking whole cells; a struct holding object references has
// no byte image at all, so the cell step is the only route and there is no bytewise fallback. Start
// the walk at an element boundary and the whole element moves as one cell, padding and all. Start
// it 16 bytes in and the cursor picks its way through the tail — `N`, and then the four bytes of
// alignment filler behind it, which belong to no field and so name no cell.
//
// Reported against `Regex(..., RegexOptions.NonBacktracking)`, whose `SymbolicRegexBuilder` node
// cache is a `Dictionary` keyed by a tuple of references: resizing it copies an `Entry[]` past
// 16 KB and stops on exactly this padding. Reproduced here without regex so the case tests the
// bulk-move primitive rather than the engine that happened to call it.
public class TestBulkMoveAcrossStructPadding
{
    private sealed class Box
    {
        public int Value;
    }

    // 24 bytes: GC auto layout promotes the two references to offsets 0 and 8, leaving `N` at 16
    // and four bytes of *trailing padding* at [20, 24). Padding at the tail rather than the head is
    // what keeps the case honest — a cursor that lands mid-element only re-synchronises with the
    // element boundary if the filler comes first, and then it would never meet padding again.
    private struct Key
    {
        public Box A;
        public Box B;
        public int N;
    }

    // 24 * 700 = 16800 bytes: over the 16384-byte chunk, and 16384 = 682 * 24 + 16, so the second
    // chunk starts 16 bytes into element 682.
    private const int Count = 700;

    private static Key[] BuildSource()
    {
        Key[] src = new Key[Count];

        for (int i = 0; i < Count; i++)
        {
            src[i].N = i + 1;

            // Populating every element would allocate 1400 objects for an interpreter to chase, and
            // the interesting elements are the ones the chunk boundary falls in and around. The
            // sparse remainder still checks that null reference cells survive the move as null.
            if (i % 89 == 0 || (i >= 680 && i <= 685))
            {
                src[i].A = new Box { Value = i * 3 + 1 };
                src[i].B = new Box { Value = i * 5 + 2 };
            }
        }

        return src;
    }

    private static int CheckCopy(Key[] src, Key[] dst)
    {
        for (int i = 0; i < Count; i++)
        {
            if (dst[i].N != src[i].N) return 1;
            if (!ReferenceEquals(dst[i].A, src[i].A)) return 2;
            if (!ReferenceEquals(dst[i].B, src[i].B)) return 3;
        }

        return 0;
    }

    // The whole array, so the copy crosses the chunk boundary at element 682 offset 16.
    private static int WholeArray()
    {
        Key[] src = BuildSource();
        Key[] dst = new Key[Count];
        Array.Copy(src, dst, Count);
        return CheckCopy(src, dst);
    }

    // A control that stays under the chunk size, so the move is one call and every step starts on
    // an element boundary. It must keep passing: it is what distinguishes "mid-element cursors are
    // broken" from "reference-containing bulk moves are broken".
    private static int UnderChunkSize()
    {
        Key[] src = new Key[8];
        for (int i = 0; i < 8; i++)
        {
            src[i].N = i + 1;
            src[i].A = new Box { Value = i + 100 };
        }

        Key[] dst = new Key[8];
        Array.Copy(src, dst, 8);

        for (int i = 0; i < 8; i++)
        {
            if (dst[i].N != src[i].N) return 1;
            if (!ReferenceEquals(dst[i].A, src[i].A)) return 2;
            if (dst[i].B != null) return 3;
        }

        return 0;
    }

    // Copying into the *middle* of a destination shifts the two arrays relative to each other, so
    // the source and destination cursors sit at different offsets within their own elements for the
    // whole move. Nothing may assume the two sides step in lockstep.
    private static int ShiftedDestination()
    {
        Key[] src = BuildSource();
        Key[] dst = new Key[Count + 3];
        Array.Copy(src, 0, dst, 3, Count);

        for (int i = 0; i < 3; i++)
        {
            if (dst[i].N != 0 || dst[i].A != null || dst[i].B != null) return 1;
        }

        for (int i = 0; i < Count; i++)
        {
            if (dst[i + 3].N != src[i].N) return 2;
            if (!ReferenceEquals(dst[i + 3].A, src[i].A)) return 3;
            if (!ReferenceEquals(dst[i + 3].B, src[i].B)) return 4;
        }

        return 0;
    }

    // Shifting an array up by one within itself. The ranges overlap with the source below the
    // destination, so the move must run backwards or it would clobber bytes it has not read yet —
    // and running backwards means the cursor is a move's *last* byte rather than its first, which
    // the padding step has to mirror.
    //
    // `BulkMoveWithWriteBarrierBatch` reverses its chunk order for this direction too, so the
    // chunk boundary lands mid-element again: 699 * 24 = 16776 bytes leaves a 392-byte remainder,
    // and 392 = 16 * 24 + 8. The backward walk therefore starts on the last byte of an element's
    // trailing filler.
    private static int OverlappingBackwards()
    {
        Key[] a = BuildSource();

        // Snapshot through the disjoint path, which the scenarios above have already pinned. `Key`
        // is a struct, so this copies values rather than aliasing them.
        Key[] expected = new Key[Count];
        Array.Copy(a, expected, Count);

        Array.Copy(a, 0, a, 1, Count - 1);

        if (a[0].N != expected[0].N) return 1;
        if (!ReferenceEquals(a[0].A, expected[0].A)) return 2;
        if (!ReferenceEquals(a[0].B, expected[0].B)) return 3;

        for (int i = 1; i < Count; i++)
        {
            if (a[i].N != expected[i - 1].N) return 4;
            if (!ReferenceEquals(a[i].A, expected[i - 1].A)) return 5;
            if (!ReferenceEquals(a[i].B, expected[i - 1].B)) return 6;
        }

        return 0;
    }

    // A struct with no references at all, so its padding *is* observable: the whole value has a
    // byte image, and a guest can read the filler back through a byte view. `Tag` sits at offset 0
    // and `V` needs 8-byte alignment, so bytes [1, 8) are alignment filler.
    private struct Padded
    {
        public byte Tag;
        public long V;
    }

    // The bytes a bulk move puts into padding must be the *source's*, not whatever the destination
    // happened to hold. Real memory copies filler like any other byte, so a runtime that quietly
    // stepped over it would diverge here — and this is the only scenario in the file that can
    // notice, because a struct holding references has no byte image for a guest to read its filler
    // through.
    //
    // `Unsafe.CopyBlock` rather than `Array.Copy`: it lowers to `cpblk`, which takes a byte count
    // and a byte-view endpoint, and so can start the move partway into an element without needing
    // to exceed a 16 KB chunk first.
    private static int PaddingBytesAreCopied()
    {
        Padded[] src = new Padded[2];
        Padded[] dst = new Padded[2];

        for (int i = 0; i < 2; i++)
        {
            src[i].Tag = (byte)(i + 1);
            src[i].V = 1000 + i;
            dst[i].Tag = 99;
            dst[i].V = -1;
        }

        ref byte s = ref Unsafe.As<Padded, byte>(ref src[0]);
        ref byte d = ref Unsafe.As<Padded, byte>(ref dst[0]);

        // Distinguishable filler on both sides, so "the destination kept its own" and "the source
        // arrived" are different answers rather than both being zero.
        Unsafe.WriteUnaligned<int>(ref Unsafe.Add(ref s, 4), 0x11223344);
        Unsafe.WriteUnaligned<int>(ref Unsafe.Add(ref s, 20), 0x55667788);
        Unsafe.WriteUnaligned<int>(ref Unsafe.Add(ref d, 4), unchecked((int)0xAABBCCDD));
        Unsafe.WriteUnaligned<int>(ref Unsafe.Add(ref d, 20), unchecked((int)0xEEFF0011));

        // From byte 4 — inside the first element's filler — for 20 bytes, so the walk crosses
        // filler, a field, an element boundary, and filler again, and stops at [24) where the
        // second element's `V` begins. Ending on a field boundary keeps every assertion below about
        // a whole field: a range ending mid-`V` would leave it half-copied, which is a real thing
        // `cpblk` does but not what this scenario is measuring.
        Unsafe.CopyBlock(ref Unsafe.Add(ref d, 4), ref Unsafe.Add(ref s, 4), 20);

        if (Unsafe.ReadUnaligned<int>(ref Unsafe.Add(ref d, 4)) != 0x11223344) return 1;
        if (Unsafe.ReadUnaligned<int>(ref Unsafe.Add(ref d, 20)) != 0x55667788) return 2;

        // The fields either side of the copied range are the ones the move was supposed to carry,
        // and byte 0 sits before it and must be untouched.
        if (dst[0].Tag != 99) return 3;
        if (dst[0].V != 1000) return 4;
        if (dst[1].Tag != 2) return 5;
        if (dst[1].V != -1) return 6;

        return 0;
    }

    [StructLayout(LayoutKind.Sequential)]
    private struct Inner
    {
        public byte Tag;
        public long V;
    }

    // `Alias` is laid over [4, 8) — bytes that are alignment filler as far as `Inner` is concerned.
    // So a byte range through them is *not* filler of the outer struct: it is a live field, and it
    // has to be moved as one rather than as part of `A`'s padding.
    [StructLayout(LayoutKind.Explicit, Size = 16)]
    private struct Overlaid
    {
        [FieldOffset(0)]
        public Inner A;

        [FieldOffset(4)]
        public int Alias;
    }

    // A nested struct's filler is only its own where nothing else claims those bytes. Copying
    // across the overlap must carry the sibling, not skip it as though it were padding.
    private static int OverlappingSiblingIsNotPadding()
    {
        Overlaid[] src = new Overlaid[1];
        Overlaid[] dst = new Overlaid[1];

        src[0].A.Tag = 7;
        src[0].Alias = 0x11223344;
        dst[0].A.Tag = 9;
        dst[0].Alias = 0x55667788;

        ref byte s = ref Unsafe.As<Overlaid, byte>(ref src[0]);
        ref byte d = ref Unsafe.As<Overlaid, byte>(ref dst[0]);

        // From byte 1 — inside `Inner`'s filler — through byte 7, so the range starts in genuine
        // padding and then runs straight into `Alias`.
        Unsafe.CopyBlock(ref Unsafe.Add(ref d, 1), ref Unsafe.Add(ref s, 1), 7);

        if (dst[0].Alias != src[0].Alias) return 1;

        // Byte 0 is outside the copied range.
        if (dst[0].A.Tag != 9) return 2;

        return 0;
    }

    public static int Main(string[] argv)
    {
        int result;

        result = OverlappingSiblingIsNotPadding();
        if (result != 0) return 60 + result;

        result = PaddingBytesAreCopied();
        if (result != 0) return 40 + result;

        result = UnderChunkSize();
        if (result != 0) return 10 + result;

        result = WholeArray();
        if (result != 0) return 20 + result;

        result = ShiftedDestination();
        if (result != 0) return 30 + result;

        result = OverlappingBackwards();
        if (result != 0) return 50 + result;

        return 0;
    }
}

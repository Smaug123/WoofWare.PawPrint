using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

// An overlapping copy whose destination byref is `[ReinterpretAs Elem; ByteOffset k*sizeof(Elem);
// Field A]` — the same chain shape as `InlineArrayFieldWriteAtLaterSlot.cs`, but reaching a
// different consumer of the projection walk.
//
// `CellAwareMemOps.shouldCopyBackwards` decides `Memmove` direction by folding both endpoints to a
// flat byte offset in a shared storage. When that fold throws it degrades to a coarse
// storage-identity comparison, and two byrefs into the *same* storage with no offsets to compare
// are undecidable — so it fails loud rather than guessing a direction that could corrupt the
// overlap. A `ByteOffset` followed by a `Field` used to throw in exactly that way, which made this
// program a host failure rather than a copy.
//
// The direction matters here: `dst` starts two ints after `src` and the ranges overlap, so a
// forward loop would smear the first element across the range instead of shifting it. The expected
// contents therefore distinguish "copied backwards" from "copied forwards", and not merely
// "copied".
public class TestInlineArrayOverlappingCopy
{
    private struct Elem
    {
        public int A;
        public int B;
    }

    [InlineArray(4)]
    private struct Buffer
    {
        private Elem _item;
    }

    public static int Main(string[] argv)
    {
        Buffer buf = default;

        // Laid out as the int sequence 0,1,2,3,4,5,6,7.
        for (int i = 0; i < 4; i++)
        {
            buf[i].A = i * 2;
            buf[i].B = i * 2 + 1;
        }

        // src covers ints 0..5, dst covers ints 2..7. They overlap, and dst is the later of the
        // two, so a correct Memmove copies backwards.
        Span<int> src = MemoryMarshal.CreateSpan(ref buf[0].A, 6);
        Span<int> dst = MemoryMarshal.CreateSpan(ref buf[1].A, 6);
        src.CopyTo(dst);

        // Expected int sequence: 0,1,0,1,2,3,4,5.
        int[] expected = { 0, 1, 0, 1, 2, 3, 4, 5 };

        for (int i = 0; i < 4; i++)
        {
            if (buf[i].A != expected[i * 2]) return 10 + i * 2;
            if (buf[i].B != expected[i * 2 + 1]) return 11 + i * 2;
        }

        return 0;
    }
}

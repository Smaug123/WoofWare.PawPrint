using System;
using System.Runtime.CompilerServices;

// The minimal end-to-end case for a *sub-cell* bulk move.
//
// `Span<object>.CopyTo` bottoms out in `Buffer.BulkMoveWithWriteBarrierInternal`, which PawPrint
// serves by walking the byte range and moving whole typed cells. Here the source is one slot of an
// `[InlineArray(8)] struct { object _item; }` local, which PawPrint models as a single indivisible
// 64-byte cell: the 8 bytes being moved are a strict sub-range of it, so a whole-cell move cannot
// serve it. Object references have no byte image, so there is no bytewise fallback either.
//
// Deliberately kept to *one* element rather than the whole buffer, since a whole-buffer copy is a
// whole-cell move on the source side and would pass without any of this. `TestBulkMoveCellAccess`
// sweeps the surrounding space (both directions, overlapping in-place moves, every root a buffer
// can sit in, and byte-addressable controls) against the real runtime.
public class TestInlineArrayBufferMoveToArray
{
    [InlineArray(8)]
    private struct Buffer
    {
        private object _item;
    }

    public static int Main(string[] argv)
    {
        Buffer buffer = default;
        buffer[0] = "first";
        buffer[1] = "second";

        object[] destination = new object[8];

        Span<object> source = buffer;
        source.Slice(1, 1).CopyTo(destination);

        if (!ReferenceEquals(destination[0], "second")) return 1;

        // Everything past the one-element window must be untouched: an over-copying move would
        // have brought "first" along, or landed it one cell over.
        for (int i = 1; i < destination.Length; i++)
        {
            if (destination[i] != null) return 2;
        }

        // The source is unchanged, including the slot that was not read.
        if (!ReferenceEquals(buffer[0], "first")) return 3;
        if (!ReferenceEquals(buffer[1], "second")) return 4;

        return 0;
    }
}

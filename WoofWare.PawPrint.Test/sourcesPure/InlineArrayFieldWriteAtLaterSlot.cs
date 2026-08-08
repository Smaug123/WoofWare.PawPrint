using System;
using System.Runtime.CompilerServices;

// `buffer[k].Tag = ...` for `k > 0`: the same field write as
// `InlineArrayFieldWriteThroughIndex.cs`, one slot along.
//
// The extra slot is what breaks it, and not in the cell resolver. Slot `k` adds a `ByteOffset`
// between the reinterpret and the field, giving `[ReinterpretAs Elem; ByteOffset k*sizeof(Elem);
// Field Tag]`. `walkProjectionByteOffset` refuses a `ByteOffset` followed by a `Field` as a
// construction-site invariant violation — the cursor has no type anchor — and does so while
// computing the byte offset, before any cell naming can be attempted.
//
// This is the same blocker as `ReinterpretReadNestedFieldThroughIndex.cs`, reached from the write
// side instead of the read side. For the `Unsafe.Add(ref elem, k)` idiom the anchor is in fact
// still live — the offset moved by whole elements, so the cursor is `Elem`-typed — but that guard
// is shared well beyond inline arrays, so relaxing it wants its own change.
//
// Not a regression: at the parent commit this program fails at the first statement instead, with
// "refusing byte view over value type containing object references".
public class TestInlineArrayFieldWriteAtLaterSlot
{
    private sealed class Box { public int V; }

    private struct Elem { public byte Tag; public Box Payload; }

    [InlineArray(2)]
    private struct Buffer { private Elem _item; }

    public static int Main(string[] argv)
    {
        Buffer buffer = default;

        // Slot 0 works: no `ByteOffset` intervenes.
        buffer[0].Tag = 7;
        buffer[0].Payload = new Box { V = 70 };
        if (buffer[0].Tag != 7 || buffer[0].Payload.V != 70) return 1;

        // Slot 1 is the gap.
        buffer[1].Tag = 8;
        if (buffer[1].Tag != 8) return 2;

        buffer[1].Payload = new Box { V = 80 };
        if (buffer[1].Payload == null || buffer[1].Payload.V != 80) return 3;

        if (buffer[0].Tag != 7 || buffer[0].Payload.V != 70) return 4;

        return 0;
    }
}

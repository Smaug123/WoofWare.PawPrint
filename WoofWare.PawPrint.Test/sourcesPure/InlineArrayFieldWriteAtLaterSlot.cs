using System;
using System.Runtime.CompilerServices;

// `buffer[k].Tag = ...` for `k > 0`: the same field write as
// `InlineArrayFieldWriteThroughIndex.cs`, one slot along.
//
// The extra slot puts a `ByteOffset` between the reinterpret and the field, giving
// `[ReinterpretAs Elem; ByteOffset k*sizeof(Elem); Field Tag]`. That is a legal chain — the
// cursor is still `Elem`-typed, because a `ByteOffset` is only ever appended to a chain already
// ending in a `ReinterpretAs` — and it folds to `k*sizeof(Elem) + offsetof(Tag)`, exactly what
// `ldflda` on a `ref Elem` sitting `k` elements along means in the real runtime.
//
// The file deliberately covers both field kinds, because they are served by different writers:
// `writeManagedByrefCore` routes on whether the value being stored is byte-renderable, so
// `Tag = 8` (a byte) goes to the bytes-or-typed-cell writer while `Payload = new Box(...)` (a
// reference) goes to the structural writer's trailing-`Field` arm. Both need the chain to fold to
// an offset first, which is what they had in common while this was parked.
//
// Slot 0 is kept as the control: it needs no `ByteOffset` at all.
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

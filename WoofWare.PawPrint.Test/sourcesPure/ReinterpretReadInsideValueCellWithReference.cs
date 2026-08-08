using System;
using System.Runtime.CompilerServices;

// Read side: reading a field that lives *inside* a cell a reinterpreting byref names.
//
// `buffer[0].Tag` is a byte-view read at an offset within slot 0, not at the slot's own extent, so
// serving it means descending: buffer -> slot 0 -> Tag. `Tag` is deliberately not at offset 0 —
// GC auto-layout promotes `Payload`, so `Tag` sits after it — which is what makes this a genuine
// descent rather than a whole-cell read wearing a disguise.
//
// Both leaf kinds are covered here because they take different routes out of the byref layer: a
// primitive leaf comes back through the byte-view read path, and a reference leaf through the
// `ReinterpretAs`+`Field` classifier.
//
// The buffer is filled through `First`, an ordinary field access, so that a failure here is a read
// failure and not a write one; writes are covered by
// `ReinterpretWriteValueCellWithReference.cs`.
public class TestReinterpretReadInsideValueCellWithReference
{
    private sealed class Box { public int V; }

    private struct Elem { public byte Tag; public Box Payload; }

    [InlineArray(2)]
    private struct Buffer
    {
        private Elem _item;

        // Ordinary ldfld/stfld of the single declared field: no ReinterpretAs involved.
        public Elem First
        {
            get => _item;
            set => _item = value;
        }
    }

    public static int Main(string[] argv)
    {
        Buffer buffer = default;
        buffer.First = new Elem { Tag = 5, Payload = new Box { V = 99 } };

        // Primitive leaf, read through the indexer.
        if (buffer[0].Tag != 5) return 1;

        // Reference leaf, read through the indexer.
        Box payload = buffer[0].Payload;
        if (payload == null) return 2;
        if (payload.V != 99) return 3;

        // The untouched slot still reads as default through the same route.
        if (buffer[1].Tag != 0) return 4;
        if (buffer[1].Payload != null) return 5;

        return 0;
    }
}

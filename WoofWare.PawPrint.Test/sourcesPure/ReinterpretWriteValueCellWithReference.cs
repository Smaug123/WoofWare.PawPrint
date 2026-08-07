using System;
using System.Runtime.CompilerServices;

// Write side, in isolation: storing a *struct containing a reference* through a `ReinterpretAs`
// byref.
//
// `buffer[0] = elem` is a byref whose projections are `[ReinterpretAs Elem]` over storage that
// contains an object reference, so it has no byte rendering and the bytewise path cannot serve it
// at all — the only way to serve the write is to name the cell it lands on.
//
// Deliberately kept to element 0 and to a write, so the write-side change alone makes this pass:
//   - reading `buffer[0].Tag` back through the indexer is a byte-view read at an offset *inside*
//     the slot, covered by `ReinterpretReadPrimitiveInsideValueCell.cs`;
//   - reading `buffer[0].Payload` back is the reference-leaf read, covered by
//     `ReinterpretReadReferenceInsideValueCell.cs`.
// The result is therefore observed through `First`, which reads `_item` as an ordinary field
// access rather than through a reinterpret byref.
public class TestReinterpretWriteValueCellWithReference
{
    private sealed class Box { public int V; }

    private struct Elem { public byte Tag; public Box Payload; }

    [InlineArray(2)]
    private struct Buffer
    {
        private Elem _item;

        // Ordinary ldfld of the single declared field: no ReinterpretAs involved.
        public Elem First => _item;
    }

    public static int Main(string[] argv)
    {
        Buffer buffer = default;

        buffer[0] = new Elem { Tag = 7, Payload = new Box { V = 42 } };

        Elem read = buffer.First;
        if (read.Tag != 7) return 1;
        if (read.Payload == null) return 2;
        if (read.Payload.V != 42) return 3;

        return 0;
    }
}

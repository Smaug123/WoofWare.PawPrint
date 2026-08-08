using System;
using System.Runtime.CompilerServices;

// Writing a *field* of a slot through an inline-array index: `buffer[0].Tag = ...`, as opposed to
// replacing the whole slot with `buffer[0] = ...`.
//
// `InlineArrayValueTypeElementWithReference.cs` covers the read direction of this shape. The write
// lowers to `[ReinterpretAs Elem; Field Tag]`, one projection longer than the whole-slot write,
// and the slot is byte-unaddressable, so naming the cell is again the only route. Both directions
// of the byref layer need it: a byte-renderable value (`Tag`) arrives at the bytes-or-typed-cell
// writer, and a reference (`Payload`) at the structural one.
//
// Only slot 0 is exercised here. `InlineArrayFieldWriteAtLaterSlot.cs` is the same access at a
// later slot, which is parked behind a different blocker.
public class TestInlineArrayFieldWriteThroughIndex
{
    private sealed class Box { public int V; }

    private struct Elem { public byte Tag; public Box Payload; }

    [InlineArray(2)]
    private struct Buffer { private Elem _item; }

    public static int Main(string[] argv)
    {
        Buffer buffer = default;
        buffer[1] = new Elem { Tag = 9, Payload = new Box { V = 90 } };

        // The primitive field, which auto-layout put at a non-zero offset within the slot.
        buffer[0].Tag = 7;
        if (buffer[0].Tag != 7) return 1;

        // The reference field, which auto-layout promoted to offset 0.
        buffer[0].Payload = new Box { V = 70 };
        if (buffer[0].Payload == null || buffer[0].Payload.V != 70) return 2;

        // Neither field write may disturb the other, nor the neighbouring slot.
        if (buffer[0].Tag != 7) return 3;
        if (buffer[1].Tag != 9 || buffer[1].Payload == null || buffer[1].Payload.V != 90) return 4;

        // Writing null through the same route, and re-writing the primitive.
        buffer[0].Payload = null;
        if (buffer[0].Payload != null) return 5;
        if (buffer[0].Tag != 7) return 6;

        buffer[0].Tag = 11;
        if (buffer[0].Tag != 11) return 7;
        if (buffer[1].Tag != 9 || buffer[1].Payload == null || buffer[1].Payload.V != 90) return 8;

        return 0;
    }
}

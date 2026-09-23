using System;
using System.Runtime.CompilerServices;

// Reading a field of a *nested* struct directly through an inline-array index: `buf[k].I.P`.
//
// This is one step deeper than `ReinterpretReadInsideValueCellWithReference.cs`, which reads
// `buf[k].Field`. `buf[k].I` is `[ReinterpretAs Elem; ByteOffset k*sizeof(Elem); Field I]`, and
// reading `.P` off that is an `ldfld` through a byref whose `ReinterpretAs` is followed by a
// `Field` rather than being the last projection. The read has to be served from where that whole
// chain lands and the type it lands on (`Inner`), not from the reinterpret target (`Elem`).
//
// The element holds a reference, so the storage has no byte image and both leaves are served by
// naming the storage cell the chain picks out. The reference-free counterpart, where the same
// chain is served bytewise, is `ReinterpretNestedFieldThroughIndexReferenceFree.cs`; writes are
// `ReinterpretWriteNestedFieldThroughIndex.cs`.
public class TestReinterpretReadNestedFieldThroughIndex
{
    private sealed class Box { public int V; }

    private struct Inner { public byte Q; public Box P; }

    private struct Elem { public Inner I; public byte Tag; }

    [InlineArray(2)]
    private struct Buffer
    {
        private Elem _item;
    }

    public static int Main(string[] argv)
    {
        Buffer buffer = default;

        buffer[0] = new Elem { I = new Inner { Q = 3, P = new Box { V = 30 } }, Tag = 1 };
        buffer[1] = new Elem { I = new Inner { Q = 4, P = new Box { V = 40 } }, Tag = 2 };

        // Whole-element reads work: these name the slot cell and copy it out.
        if (buffer[0].Tag != 1) return 1;
        if (buffer[1].Tag != 2) return 2;

        // The nested field read through the index is the gap.
        if (buffer[0].I.Q != 3) return 3;
        if (buffer[0].I.P.V != 30) return 4;
        if (buffer[1].I.Q != 4) return 5;
        if (buffer[1].I.P.V != 40) return 6;

        return 0;
    }
}

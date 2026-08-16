using System;
using System.Runtime.CompilerServices;

// Reading a field of a *nested* struct directly through an inline-array index: `buf[k].I.P`.
//
// This is one step deeper than `ReinterpretReadInsideValueCellWithReference.cs`, which reads
// `buf[k].Field`. The extra step is what breaks it, and not in the cell resolver:
// `CliType.CellPathsExactlyCovering` descends to any depth, and `TestCliTypeCellPaths` covers
// depth 3 directly.
//
// The blocker is routing, in `readManagedByrefField`. `buf[k].I` is
// `[ReinterpretAs Elem; ByteOffset k*sizeof(Elem); Field I]`, and reading `.P` off that appends a
// second `Field`. That function's reinterpret-aware arms only fire when `ReinterpretAs` is the last
// projection (or last-but-a-`ByteOffset`), so a trailing `Field` falls through to
// `readProjectedValue`, which cannot navigate across a `ReinterpretAs` and fails with
// "read through `ReinterpretAs` from value ...; needs a bytewise implementation".
//
// `walkProjectionByteOffset` folds the `ByteOffset`-then-`Field` shape, so the peeled chain
// `[ByteOffset k*sizeof(Elem); Field I; Field P]` resolves to a byte offset fine. Only the routing
// blocks this, and it is a change to the read dispatcher rather than to the projection walk.
//
// Un-park when `readManagedByrefField` learns to route a chain that *contains* a `ReinterpretAs`
// but does not end at one to the byte-view reader.
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

using System;
using System.Runtime.CompilerServices;

// Reading a field of a *nested* struct directly through an inline-array index: `buf[k].I.P`.
//
// This is one step deeper than `ReinterpretReadInsideValueCellWithReference.cs`, which reads
// `buf[k].Field`. The extra step is what breaks it, and not in the cell resolver:
// `CliType.CellPathsExactlyCovering` descends to any depth, and `TestCliTypeCellPaths` covers
// depth 3 directly.
//
// The blocker is the projection chain. `buf[k].I` is
// `[ReinterpretAs Elem; ByteOffset k*sizeof(Elem); Field I]`, and reading `.P` off that reaches
// `readManagedByrefField`, whose reinterpret-aware arms only fire when `ReinterpretAs` is the last
// projection (or last-but-a-`ByteOffset`). With a trailing `Field` it falls through to
// `readProjectedValue`, which cannot navigate across a `ReinterpretAs` and fails with
// "read through `ReinterpretAs` from value ...; needs a bytewise implementation".
//
// Routing that shape to the byte-view reader instead is not enough on its own: the peeled chain is
// then `[ByteOffset k*sizeof(Elem); Field I; Field P]`, and `walkProjectionByteOffset` deliberately
// refuses a `ByteOffset` followed by a `Field` as a construction-site invariant violation, on the
// grounds that the cursor has no type anchor. For the `Unsafe.Add(ref elem, k)` idiom the anchor is
// in fact still live — the offset moved by whole elements, so the cursor is `Elem`-typed — but
// relaxing that guard changes a shared invariant used well beyond inline arrays, so it wants its
// own change rather than riding along with the cell resolver.
//
// Not a regression: on the parent commit this program does not even reach the nested read, failing
// earlier at `buffer[0] = ...` with "write through `ReinterpretAs` over byte-unaddressable
// storage". This change moves it forward to the nested read, which is the remaining gap.
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

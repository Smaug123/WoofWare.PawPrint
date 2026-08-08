using System;
using System.Runtime.CompilerServices;

// An `[InlineArray(N)]` whose element is a *struct that contains a reference*, rather than a bare
// reference or a byte-addressable primitive.
//
// Such storage has no byte image at all, so every bytewise route into it fails and the only way to
// serve an access is to name the storage cell its byte range picks out
// (`CliType.CellPathsExactlyCovering`). All three accesses below take different routes to that:
// `buffer[k] = ...` names the slot on the write side, `buffer[k].Tag` descends into the slot
// through the byte-view read path, and `buffer[k].Payload` through the `ReinterpretAs`+`Field`
// classifier.
//
// The reference is declared second but laid out first — auto layout promotes references — so `Tag`
// sits at a non-zero offset inside each slot. That is what makes reading it a genuine descent
// rather than a whole-cell read: a resolver that only looked at top-level cells, or that ignored
// the offset within the slot, would fail here rather than quietly returning the wrong byte.
//
// `TestReinterpretCellAccess` sweeps this shape and its variants against the real runtime;
// this file is the minimal end-to-end case.
public class TestInlineArrayValueTypeElementWithReference
{
    private sealed class Box { public int V; }

    private struct Elem { public byte Tag; public Box Payload; }

    [InlineArray(2)]
    private struct Buffer { private Elem _item; }

    public static int Main(string[] argv)
    {
        Buffer buffer = default;

        buffer[0] = new Elem { Tag = 1, Payload = new Box { V = 10 } };
        buffer[1] = new Elem { Tag = 2, Payload = new Box { V = 20 } };

        if (buffer[0].Tag != 1 || buffer[0].Payload.V != 10) return 1;
        if (buffer[1].Tag != 2 || buffer[1].Payload.V != 20) return 2;

        return 0;
    }
}

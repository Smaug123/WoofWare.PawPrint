using System;
using System.Runtime.CompilerServices;

// The same reference-containing `[InlineArray(N)]` as
// `InlineArrayValueTypeElementWithReference.cs`, but stored in a *managed array element* rather
// than a local.
//
// The byref root is what differs: `buffers[k][j]` roots at `ByrefRoot.ArrayElement` with an empty
// structural prefix, so the read dispatches through the specialised array byte-reader rather than
// through the generic prefix arm. Cell naming has to be reachable from both, or the walk is total
// only for locals.
public class TestReinterpretCellInArrayElementStorage
{
    private sealed class Box { public int V; }

    private struct Elem { public byte Tag; public Box Payload; }

    [InlineArray(2)]
    private struct Buffer { private Elem _item; }

    public static int Main(string[] argv)
    {
        Buffer[] buffers = new Buffer[2];

        buffers[0][0] = new Elem { Tag = 1, Payload = new Box { V = 10 } };
        buffers[0][1] = new Elem { Tag = 2, Payload = new Box { V = 20 } };
        buffers[1][0] = new Elem { Tag = 3, Payload = new Box { V = 30 } };
        buffers[1][1] = new Elem { Tag = 4, Payload = new Box { V = 40 } };

        if (buffers[0][0].Tag != 1 || buffers[0][0].Payload.V != 10) return 1;
        if (buffers[0][1].Tag != 2 || buffers[0][1].Payload.V != 20) return 2;
        if (buffers[1][0].Tag != 3 || buffers[1][0].Payload.V != 30) return 3;
        if (buffers[1][1].Tag != 4 || buffers[1][1].Payload.V != 40) return 4;

        // A whole-slot copy out of array-element storage.
        Elem copied = buffers[1][0];
        if (copied.Tag != 3 || copied.Payload.V != 30) return 5;

        return 0;
    }
}

using System;
using System.Runtime.CompilerServices;

// `ReinterpretWriteNestedFieldThroughIndex.cs` reaches `buffer[k].M.I` with the buffer in a local.
// The root decides which writer an access reaches, so each root is its own case: with the buffer
// in an array element or a class field, a write whose value has no byte image lands first on the
// byte writer's precise-cell helpers, which serve only a top-level cell at an exact offset, and
// must be handed to the structural writer's cell naming instead.
//
// Both roots are covered one field past the reinterpret (`buffer[1].M`, a reference-holding
// struct) and three fields past it (`buffer[1].M.I.P`, a reference), at slot 1 so that a
// `ByteOffset` intervenes. Reads and a byte-leaf write ride along, since they share the chain.
public class TestReinterpretNestedFieldThroughIndexHeapRoots
{
    private sealed class Box { public int V; }

    private struct Inner { public byte Q; public Box P; }

    private struct Mid { public byte Pad; public Inner I; }

    private struct Elem { public Mid M; public byte Tag; }

    [InlineArray(2)]
    private struct Buffer
    {
        private Elem _item;
    }

    private sealed class Holder { public Buffer Buf; }

    private static Buffer Make()
    {
        Buffer b = default;
        b[0] = new Elem { M = new Mid { Pad = 1, I = new Inner { Q = 3, P = new Box { V = 30 } } }, Tag = 5 };
        b[1] = new Elem { M = new Mid { Pad = 2, I = new Inner { Q = 4, P = new Box { V = 40 } } }, Tag = 6 };
        return b;
    }

    public static int Main(string[] argv)
    {
        // Rooted at a managed array element.
        Buffer[] arr = new Buffer[2];
        arr[1] = Make();

        if (arr[1][1].M.I.Q != 4 || arr[1][1].M.I.P.V != 40) return 1;

        arr[1][1].M.I.Q = 9;
        arr[1][1].M.I.P = new Box { V = 70 };
        if (arr[1][1].M.I.Q != 9 || arr[1][1].M.I.P.V != 70) return 2;
        if (arr[1][1].M.Pad != 2 || arr[1][1].Tag != 6) return 3;
        if (arr[1][0].M.I.P.V != 30 || arr[0][1].M.I.P != null) return 4;

        arr[1][1].M = new Mid { Pad = 3, I = new Inner { Q = 5, P = new Box { V = 50 } } };
        if (arr[1][1].M.Pad != 3 || arr[1][1].M.I.Q != 5 || arr[1][1].M.I.P.V != 50) return 5;
        if (arr[1][1].Tag != 6 || arr[1][0].M.I.P.V != 30) return 6;

        // Rooted at a class field.
        Holder h = new Holder();
        h.Buf = Make();

        if (h.Buf[1].M.I.Q != 4 || h.Buf[1].M.I.P.V != 40) return 7;

        h.Buf[1].M.I.Q = 9;
        h.Buf[1].M.I.P = new Box { V = 70 };
        if (h.Buf[1].M.I.Q != 9 || h.Buf[1].M.I.P.V != 70) return 8;
        if (h.Buf[1].M.Pad != 2 || h.Buf[1].Tag != 6) return 9;
        if (h.Buf[0].M.I.P.V != 30) return 10;

        h.Buf[1].M = new Mid { Pad = 3, I = new Inner { Q = 5, P = new Box { V = 50 } } };
        if (h.Buf[1].M.Pad != 3 || h.Buf[1].M.I.Q != 5 || h.Buf[1].M.I.P.V != 50) return 11;
        if (h.Buf[1].Tag != 6 || h.Buf[0].M.I.P.V != 30) return 12;

        return 0;
    }
}

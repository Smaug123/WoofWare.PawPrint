using System;
using System.Runtime.CompilerServices;

// `InlineArrayFieldWriteThroughIndex.cs` writes a slot's field with the buffer in a local. The
// root decides which writer the access reaches, so each root is its own case: an array element and
// a class field both bypass the arm that serves the local, landing instead on the specialised
// array/heap byte writers (for a byte-renderable value) or on the bytes-or-typed-cell writer via
// the `NotTransparent` dispatch (for a reference).
public class TestInlineArrayFieldWriteInNonLocalRoots
{
    private sealed class Box { public int V; }

    private struct Elem { public byte Tag; public Box Payload; }

    [InlineArray(2)]
    private struct Buffer { private Elem _item; }

    private sealed class Holder { public Buffer Buf; }

    public static int Main(string[] argv)
    {
        // Rooted at a managed array element.
        Buffer[] buffers = new Buffer[2];
        buffers[1][1] = new Elem { Tag = 9, Payload = new Box { V = 90 } };

        buffers[1][0].Tag = 7;
        if (buffers[1][0].Tag != 7) return 1;

        buffers[1][0].Payload = new Box { V = 70 };
        if (buffers[1][0].Payload == null || buffers[1][0].Payload.V != 70) return 2;

        if (buffers[1][0].Tag != 7) return 3;
        if (buffers[1][1].Tag != 9 || buffers[1][1].Payload.V != 90) return 4;
        if (buffers[0][0].Tag != 0 || buffers[0][0].Payload != null) return 5;

        // Rooted at a class field.
        Holder holder = new Holder();
        holder.Buf[1] = new Elem { Tag = 4, Payload = new Box { V = 40 } };

        holder.Buf[0].Tag = 3;
        if (holder.Buf[0].Tag != 3) return 6;

        holder.Buf[0].Payload = new Box { V = 30 };
        if (holder.Buf[0].Payload == null || holder.Buf[0].Payload.V != 30) return 7;

        if (holder.Buf[0].Tag != 3) return 8;
        if (holder.Buf[1].Tag != 4 || holder.Buf[1].Payload.V != 40) return 9;

        return 0;
    }
}
